-- | Simplifying wrapper around the Cardano core libraries
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module UTxO.Simplified (
    -- * Simplified API to the core block generation API
    Simplified(..)
  , Simpl(..)
    -- * Genesis data
  , genesisBlock0
  , generatedActors
  , Actors(..)
  , Rich(..)
  , Poor(..)
    -- * Translation context
  , Translate
  , runTranslate
  , lift
  , liftPure
  , liftMaybe
  ) where

import Universum hiding (lift)
import Control.Exception (throw)
import Control.Monad.Except (MonadError)
import Data.Default (def)
import System.IO.Error (userError)
import Formatting (bprint, build, (%))
import Serokell.Util (listJson, pairF)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Buildable

import Pos.Block.Logic
import Pos.Client.Txp
import Pos.Core hiding (genesisData, generatedSecrets)
import Pos.Crypto
import Pos.Ssc
import Pos.Update
import qualified Pos.Context
import qualified Pos.Core

{-------------------------------------------------------------------------------
  Testing infrastructure from cardano-sl-core

  The genesis block comes from defaultTestConf, which in turn uses
  configuration.yaml. It is specified by a 'GenesisSpec'.
-------------------------------------------------------------------------------}

import Test.Pos.Util (
    withDefConfiguration
  , withDefUpdateConfiguration
  )

{-------------------------------------------------------------------------------
  Simplified API for block construction
-------------------------------------------------------------------------------}

class Simplified a where
  data Simpl a :: *

  fromSimpl :: Simpl a -> Translate a

instance Simplified (SecretKey, TxIn) where
  data Simpl (SecretKey, TxIn) = Input {
      inpOwner :: SecretKey
    , inpTrans :: Simpl TxAux
    , inpIndex :: Word32
    }

  fromSimpl :: Simpl (SecretKey, TxIn) -> Translate (SecretKey, TxIn)
  fromSimpl Input{..} = do
      inpTrans' <- (hash . taTx) <$> fromSimpl inpTrans
      return (
          inpOwner
        , TxInUtxo {
              txInHash  = inpTrans'
            , txInIndex = inpIndex
            }
        )

instance Simplified TxOutAux where
  data Simpl TxOutAux = Output {
      outAddr :: Address
    , outVal  :: Word64
    }

  fromSimpl :: Simpl TxOutAux -> Translate TxOutAux
  fromSimpl Output{..} = return TxOutAux {
        toaOut = TxOut {
            txOutAddress = outAddr
          , txOutValue   = mkCoin outVal
          }
      }

instance Simplified TxAux where
  data Simpl TxAux = Transaction {
      trIns  :: [Simpl (SecretKey, TxIn)]
    , trOuts :: [Simpl TxOutAux]
    }

  fromSimpl :: Simpl TxAux -> Translate TxAux
  fromSimpl Transaction{..} = do
      trIns'  <- mapM fromSimpl trIns
      trOuts' <- mapM fromSimpl trOuts
      liftPure $ makeMPubKeyTx
                   FakeSigner
                   (NE.fromList trIns')
                   (NE.fromList trOuts')

-- | Simplified block
--
-- NOTES:
--
-- * We don't include any delegation stuff
-- * We don't test the shared seed computation
-- * We stay within a single epoch for now
-- * We use the genesis block from the test configuration
--   (which has implications for which slot leaders etc we have)
--
-- TODO: Figure out what the test dev is, precisely. It says that it uses
-- HD addresses, but in which way? (cf. 'mkGenesisBlock', 'genesisLeaders',
-- 'genesisBlock0').
instance Simplified MainBlock where
  data Simpl MainBlock = Block {
      blockPrev  :: Maybe (Simpl MainBlock)
    , blockSId   :: SlotId
    , blockKey   :: SecretKey
    , blockTrans :: [Simpl TxAux]
    }

  fromSimpl :: Simpl MainBlock -> Translate MainBlock
  fromSimpl Block{..} = do
        blockTrans' <- mapM fromSimpl blockTrans

        -- empty delegation payload
        dlgPayload <- lift $ mkDlgPayload []

        -- empty update payload
        let updPayload = def

        -- previous block header
        -- if none specified, use genesis block
        prev <-
          case blockPrev of
            Just block -> (Right . view gbHeader) <$> fromSimpl block
            Nothing    -> (Left  . view gbHeader) <$> genesisBlock0

        lift $ createMainBlockPure
          blockSizeLimit
          prev
          Nothing -- Delegation info
          blockSId
          blockKey
          (RawPayload
              blockTrans'
              (defaultSscPayload (siSlot blockSId))
              dlgPayload
              updPayload
            )
    where
      blockSizeLimit = 1 * 1024 * 1024 -- 1 MB

{-------------------------------------------------------------------------------
  Genesis data

  See also:

  * 'generateGenesisData'
-------------------------------------------------------------------------------}

genesisBlock0 :: Translate GenesisBlock
genesisBlock0 = liftPure Pos.Context.genesisBlock0

generatedActors :: Translate Actors
generatedActors = do
     secrets <- liftMaybe "Generated secrets unavailable" Pos.Core.generatedSecrets
     return Actors {
         actorsRich = map mkRich (richKeys secrets)
       , actorsPoor = map mkPoor (poorKeys secrets)
       }
  where
    richKeys :: GeneratedSecrets -> [SecretKey]
    richKeys = map rsPrimaryKey . gsRichSecrets -- this just ignores Vss keys

    poorKeys :: GeneratedSecrets -> [EncryptedSecretKey]
    poorKeys = gsPoorSecrets

    -- TODO: This mapping from the secret keys to the corresponding addresses
    -- is already present in generateGenesisData , but it is not returned.
    -- I see no choice currently but to recompute it. This is unfortunate
    -- because it means that when 'generateGenesisData' changes, we'll be
    -- out of sync here. Also, we're assuming here that 'tboUseHDAddresses'
    -- is true ('useHDAddresses' must be set to true in the config yaml file).

    mkRich :: SecretKey -> Rich
    mkRich sk = Rich {
          richKey  = sk
        , richAddr = makePubKeyAddressBoot (toPublic sk)
        }

    mkPoor :: EncryptedSecretKey -> Poor
    mkPoor sk = Poor {
          poorKey   = sk
        , poorAddrs = [ case deriveFirstHDAddress
                               (IsBootstrapEraAddr True)
                               emptyPassphrase
                               sk of
                          Nothing          -> error "impossible"
                          Just (addr, key) -> (key, addr)
                      ]
        }


-- | A rich actor has a key and a "simple" (non-HD) address
data Rich = Rich {
      richKey  :: SecretKey
    , richAddr :: Address
    }
  deriving (Show)

-- | A poor actor gets a HD wallet, so it has a SecretKey per address
-- (current generation just creates a single address though)
--
-- NOTE: `encToSecret :: EncryptedSecretKey -> SecretKey`
data Poor = Poor {
      poorKey   :: EncryptedSecretKey
    , poorAddrs :: [(EncryptedSecretKey, Address)]
    }
  deriving (Show)

data Actors = Actors {
      actorsRich :: [Rich]
    , actorsPoor :: [Poor]
    }
  deriving (Show)

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Buildable Rich where
  build Rich{..} = bprint ( "Rich {"
                          % "key: " % build
                          % ", addr: " % build
                          % "}"
                          )
                          richKey
                          richAddr

instance Buildable Poor where
  build Poor{..} = bprint ( "Poor {"
                          % "key: " % build
                          % ", addrs: " % listJson
                          % "}"
                          )
                          (encToSecret poorKey)
                          (map (bprint pairF . first encToSecret) poorAddrs)

instance Buildable Actors where
  build Actors{..} = bprint ( "Actors {"
                            % "rich: " % listJson
                            % " poor: " % listJson
                            % "}"
                            )
                            actorsRich
                            actorsPoor

{-------------------------------------------------------------------------------
  Translation context
-------------------------------------------------------------------------------}

data Translate a = Translate {
      unTranslate :: (HasConfiguration, HasUpdateConfiguration) => Either Text a
    }

instance Functor Translate where
  fmap = liftM

instance Applicative Translate where
  pure  = return
  (<*>) = ap

instance Monad Translate where
  return a = Translate $ Right a
  x >>= f  = Translate $ case unTranslate x of
                           Left err -> Left err
                           Right a  -> unTranslate (f a)

lift :: (forall m. (HasConfiguration, HasUpdateConfiguration, MonadError Text m) => m a)
     -> Translate a
lift act = Translate act

liftPure :: ((HasConfiguration, HasUpdateConfiguration) => a) -> Translate a
liftPure a = Translate (Right a)

liftMaybe :: Text -> ((HasConfiguration, HasUpdateConfiguration) => Maybe a) -> Translate a
liftMaybe err ma = Translate $ case ma of
                                 Just a  -> Right a
                                 Nothing -> Left err

runTranslate :: Translate a -> a
runTranslate (Translate act) =
   withDefConfiguration $
   withDefUpdateConfiguration $
     case act of
       Left  e -> throw (userError (show e))
       Right a -> a

{-------------------------------------------------------------------------------
  BELOW THIS LINE IS JUST EXPERIMENTATION
-------------------------------------------------------------------------------}


{-------------------------------------------------------------------------------
  GenesisData

encToSecret :: EncryptedSecretKey -> SecretKey
encToSecret (EncryptedSecretKey sk _) = SecretKey sk

-- | Generate a public key using an encrypted secret key and passphrase
encToPublic :: EncryptedSecretKey -> PublicKey
encToPublic = toPublic . encToSecret

-- | Valuable secrets which can unlock genesis data.
data GeneratedSecrets = GeneratedSecrets
    { gsDlgIssuersSecrets :: ![SecretKey]
    -- ^ Secret keys which issued heavyweight delegation certificates
    -- in genesis data. If genesis heavyweight delegation isn't used,
    -- this list is empty.
    , gsRichSecrets       :: ![RichSecrets]
    -- ^ All secrets of rich nodes.
    , gsPoorSecrets       :: ![EncryptedSecretKey]
    -- ^ Keys for HD addresses of poor nodes.
    , gsFakeAvvmSeeds     :: ![ByteString]
    -- ^ Fake avvm seeds.
    }

It makes sense that this wouldn't have any private keys, since normally the nodes
wouldn't have those. But presumably they do get created someplace during testing?


generateGenesisData
    :: (HasCryptoConfiguration, HasGenesisBlockVersionData, HasProtocolConstants)
    => GenesisInitializer
    -> GenesisAvvmBalances
    -> GeneratedGenesisData

    , tboUseHDAddresses :: !Bool
    -- ^ Whether generate plain addresses or with hd payload.


data GenesisData = GenesisData
    { gdBootStakeholders :: !GenesisWStakeholders
    , gdHeavyDelegation  :: !GenesisDelegation
    , gdStartTime        :: !Timestamp
    , gdVssCerts         :: !GenesisVssCertificatesMap
    , gdNonAvvmBalances  :: !GenesisNonAvvmBalances
    , gdBlockVersionData :: !BlockVersionData
    , gdProtocolConsts   :: !ProtocolConstants
    , gdAvvmDistr        :: !GenesisAvvmBalances
    , gdFtsSeed          :: !SharedSeed
    } deriving (Show, Eq)

newtype GenesisWStakeholders = GenesisWStakeholders
    { getGenesisWStakeholders :: Map StakeholderId Word16
    } deriving (Show, Eq, Monoid)

newtype GenesisDelegation = UnsafeGenesisDelegation
    { unGenesisDelegation :: HashMap StakeholderId ProxySKHeavy
    } deriving (Show, Eq, Container, NontrivialContainer)

-- | Stakeholder identifier (stakeholders are identified by their public keys)
type StakeholderId = AddressHash PublicKey



-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------
  Let's make sure we have access to the types we need

  TODO: Look at Pos.Txp.GenesisUtxo
-------------------------------------------------------------------------------}

-- someBlunds :: [Blund] -- == (Block, Undo)
-- someBlunds = zip someBlocks someUndos
--
-- someBlocks :: [Core.Block]  -- Either GenesisBlock MainBlock
-- someBlocks = map Left someGenesisBlocks ++ map Right someMainBlocks
--
-- someUndos :: [Undo]
-- someUndos = []
--
-- someGenesisBlocks :: [GenesisBlock]
-- someGenesisBlocks = []
--
-- someMainBlocks :: [MainBlock] -- GenericBlock MainBlockchain
-- someMainBlocks = []
--
-- someGenericBlocks :: [GenericBlock a]
-- someGenericBlocks = []

{-------------------------------------------------------------------------------
  Address stuff..

-- | Hash wrapper with phantom type for more type-safety.
-- Made abstract in order to support different algorithms in
-- different situations
newtype AbstractHash algo a = AbstractHash (Digest algo)
    deriving (Show, Eq, Ord, ByteArray.ByteArrayAccess, Generic, NFData)

type AddressHash = AbstractHash Blake2b_224

newtype Address' = Address'
    { unAddress' :: (AddrType, AddrSpendingData, Attributes AddrAttributes)
    } deriving (Eq, Show, Generic, Typeable)

\-- | 'Address' is where you can send coins.
data Address = Address
    { addrRoot       :: !(AddressHash Address')
    -- ^ Root of imaginary pseudo Merkle tree stored in this address.
    , addrAttributes :: !(Attributes AddrAttributes)
    -- ^ Attributes associated with this address.
    , addrType       :: !AddrType
    -- ^ The type of this address. Should correspond to
    -- 'AddrSpendingData', but it can't be checked statically, because
    -- spending data is hashed.
    } deriving (Eq, Ord, Generic, Typeable, Show)

-- | Type of an address. It corresponds to constructors of
-- 'AddrSpendingData'. It's separated, because 'Address' doesn't store
-- 'AddrSpendingData', but we want to know its type.
data AddrType
    = ATPubKey
    | ATScript
    | ATRedeem
    | ATUnknown !Word8
    deriving (Eq, Ord, Generic, Typeable, Show)

-- | Data which is bound to an address and must be revealed in order
-- to spend coins belonging to this address.
data AddrSpendingData
    = PubKeyASD !PublicKey
    -- ^ Funds can be spent by revealing a 'PublicKey' and providing a
    -- valid signature.
    | ScriptASD !Script
    -- ^ Funds can be spent by revealing a 'Script' and providing a
    -- redeemer 'Script'.
    | RedeemASD !RedeemPublicKey
    -- ^ Funds can be spent by revealing a 'RedeemScript' and providing a
    -- valid signature.
    | UnknownASD !Word8 !ByteString
    -- ^ Unknown type of spending data. It consists of a tag and
    -- arbitrary 'ByteString'. It allows us to introduce a new type of
    -- spending data via softfork.
    deriving (Eq, Generic, Typeable, Show)

-- | Convenient wrapper for the datatype to represent it (in binary
-- format) as k-v map.
data Attributes h = Attributes
    { -- | Data, containing known keys (deserialized)
      attrData   :: h
      -- | Remaining, unparsed fields.
    , attrRemain :: UnparsedFields
    } deriving (Eq, Ord, Generic, Typeable)

-- | Additional information stored along with address. It's intended
-- to be put into 'Attributes' data type to make it extensible with
-- softfork.
data AddrAttributes = AddrAttributes
    { aaPkDerivationPath  :: !(Maybe HDAddressPayload)
    , aaStakeDistribution :: !AddrStakeDistribution
    } deriving (Eq, Ord, Show, Generic, Typeable)

-- | Stake distribution associated with an address.
data AddrStakeDistribution
    = BootstrapEraDistr
    -- ^ Stake distribution for bootstrap era.
    | SingleKeyDistr !StakeholderId
    -- ^ Stake distribution stating that all stake should go to the given stakeholder.
    | UnsafeMultiKeyDistr !(Map StakeholderId CoinPortion)
    -- ^ Stake distribution which gives stake to multiple
    -- stakeholders. 'CoinPortion' is a portion of an output (output
    -- has a value, portion of this value is stake). The constructor
    -- is unsafe because there are some predicates which must hold:
    --
    -- • the sum of portions must be @maxBound@ (basically 1);
    -- • all portions must be positive;
    -- • there must be at least 2 items, because if there is only one item,
    -- 'SingleKeyDistr' can be used instead (which is smaller).
    deriving (Eq, Ord, Show, Generic, Typeable)

-- | HDAddressPayload consists of
--
-- * serialiazed and encrypted using HDPassphrase derivation path from the
-- root key to given descendant key (using ChaChaPoly1305 algorithm)
--
-- * cryptographic tag
--
-- For more information see 'packHDAddressAttr' and 'encryptChaChaPoly'.
data HDAddressPayload
    = HDAddressPayload
    { getHDAddressPayload :: !ByteString
    } deriving (Eq, Ord, Show, Generic)

-- | Serialize tree path and encrypt it using HDPassphrase via ChaChaPoly1305.
packHDAddressAttr :: HDPassphrase -> [Word32] -> HDAddressPayload
packHDAddressAttr (HDPassphrase passphrase) path =  ... ecrnyptChaChaPoly ..

-- | Passphrase is a hash of root public key.
data HDPassphrase = HDPassphrase !ByteString
    deriving Show

deriveHDPassphrase :: PublicKey -> HDPassphrase

-- | Derive child's secret key from parent's secret key using user's passphrase.
deriveHDSecretKey
    :: (Bi PassPhrase, Bi EncryptedPass)
    => ShouldCheckPassphrase
    -> PassPhrase
    -> EncryptedSecretKey
    -> Word32
    -> Maybe EncryptedSecretKey

-------------------------------------------------------------------------------}

--mkAddress :: Address
--mkAddress = undefined
