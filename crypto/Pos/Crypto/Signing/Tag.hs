module Pos.Crypto.Signing.Tag
       ( signTag
       , module Pos.Crypto.Signing.Types.Tag
       ) where

import           Universum

import           Data.ByteString.Builder (Builder, byteString)

import qualified Pos.Binary.Class as Bi
import           Pos.Crypto.Configuration (HasCryptoConfiguration, ProtocolMagic (..),
                                           protocolMagic)
import           Pos.Crypto.Signing.Types.Tag

-- | Get magic bytes corresponding to a 'SignTag'. Guaranteed to be different
-- (and begin with a different byte) for different tags.
signTag :: HasCryptoConfiguration => SignTag -> Builder
signTag = \case
    SignForTestingOnly -> byteString "\x00"
    SignTx             -> byteString "\x01" <> network
    SignRedeemTx       -> byteString "\x02" <> network
    SignVssCert        -> byteString "\x03" <> network
    SignUSProposal     -> byteString "\x04" <> network
    SignCommitment     -> byteString "\x05" <> network
    SignUSVote         -> byteString "\x06" <> network
    SignMainBlock      -> byteString "\x07" <> network
    SignMainBlockLight -> byteString "\x08" <> network
    SignMainBlockHeavy -> byteString "\x09" <> network
    SignProxySK        -> byteString "\x0a" <> network
  where
    network = Bi.serializeBuilder (getProtocolMagic protocolMagic)
