{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE ViewPatterns      #-}
module Preface.Codec
  ( Base64
  , Hex
  , Encoded(..), pattern EncodedHex, pattern EncodedBase64
  , encodedText, encodedHex
  , mkEncodedHex
  , mkEncodedBase64
  , genRandomBase64
  , genRandomBaseHex
  , toBase64Text
  , fromBase64Text
  , encodeBase64
  , fromHex
  , toHex
  ) where

import           Crypto.Random.Entropy
import           Data.Aeson
import           Data.Bifunctor
import qualified Data.ByteString.Base16       as B16
import qualified Data.ByteString.Base64       as B64
import qualified Data.ByteString.Char8        as B
import qualified Data.ByteString         as BS
import           Data.String
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import           Data.Text.ToText

data Base64
data Hex

data Encoded code = ValidEncoded B.ByteString
                  | InvalidEncoded T.Text -- ^ For legacy reasons.
  deriving (Eq, Ord)

instance Show (Encoded Hex) where
  show (ValidEncoded t) = B.unpack (B16.encode t)
  show (InvalidEncoded t) = T.unpack t

instance Show (Encoded Base64) where
  show (ValidEncoded t) = B.unpack (B64.encode t)
  show (InvalidEncoded t) = T.unpack t

instance Read (Encoded Hex) where
  readsPrec n = fmap (first fromString) . readsPrec n

instance ToJSON (Encoded Hex) where
  toJSON (ValidEncoded t) = String (TE.decodeUtf8 (B16.encode t))
  toJSON (InvalidEncoded t) = String t

instance FromJSON (Encoded Hex) where
  parseJSON (String t) = case B16.decode (TE.encodeUtf8 t) of
                         (r, u) | B.null u -> pure (ValidEncoded r)
                         _ -> pure (InvalidEncoded t)
  parseJSON v          = fail $ "not a valid hexadecimal encoded string: "  ++ show v

instance IsString (Encoded Hex) where
  fromString t = case B16.decode (fromString t) of
                 (r, u) | B.null u -> ValidEncoded r
                 _ -> InvalidEncoded (fromString t)

instance ToText (Encoded Hex) where
  toText (ValidEncoded t) = TE.decodeUtf8 (B16.encode t)
  toText (InvalidEncoded t) = t

instance ToText (Encoded Base64) where
  toText (ValidEncoded t) = TE.decodeUtf8 (B64.encode t)
  toText (InvalidEncoded t) = t

encodedText :: ToText a => a -> T.Text
encodedText = toText

encodedHex :: Encoded Hex -> T.Text
encodedHex = encodedText

encodedBase64 :: Encoded Base64 -> T.Text
encodedBase64 = encodedText

mkEncodedHex :: T.Text -> Encoded Hex
mkEncodedHex t = case B16.decode (TE.encodeUtf8 t) of
  (r, u) | B.null u -> ValidEncoded r
  _ -> InvalidEncoded t

mkEncodedBase64 :: T.Text -> Encoded Base64
mkEncodedBase64 t = case B64.decode (TE.encodeUtf8 t) of
  Right r -> ValidEncoded r
  Left _ -> InvalidEncoded t

pattern EncodedHex :: T.Text -> Encoded Hex
pattern EncodedHex t <- (encodedHex -> t) where
  EncodedHex t = mkEncodedHex t

pattern EncodedBase64 :: T.Text -> Encoded Base64
pattern EncodedBase64 t <- (encodedBase64 -> t) where
  EncodedBase64 t = mkEncodedBase64 t

-- | Generates a cryptographically secure random string of given length
genRandomBase64 :: Int -> IO (Encoded Base64)
genRandomBase64 len = ValidEncoded <$> getEntropy len

-- | Generates a cryptographically secure random string of given length producing only hexadecimal characters
genRandomBaseHex :: Int -> IO (Encoded Hex)
genRandomBaseHex len = ValidEncoded <$> getEntropy len

toBase64Text :: BS.ByteString -> Encoded Base64
toBase64Text = ValidEncoded

fromBase64Text :: Encoded Base64 -> BS.ByteString
fromBase64Text (InvalidEncoded t) = B64.decodeLenient (TE.encodeUtf8 t)
fromBase64Text (ValidEncoded t) = t

encodeBase64 :: BS.ByteString -> BS.ByteString
encodeBase64 = B64.encode

fromHex :: Encoded Hex -> BS.ByteString
fromHex (ValidEncoded t) = t
fromHex (InvalidEncoded _) = error "fromHex: should never happen"

toHex :: BS.ByteString -> Encoded Hex
toHex = ValidEncoded
