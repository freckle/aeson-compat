{-# LANGUAGE CPP #-}

-- | Follows the latest @aeson-2.*@ version of "Data.Aeson.Key"
module Data.Aeson.V2.Key
  ( -- * @2.0.0.0@
    Key
  , fromString
  , toString
  , toText
  , fromText
  , coercionToText

    -- * @2.0.2.0@
  , toShortText
  , fromShortText
  ) where

import Prelude

#if MIN_VERSION_aeson(2, 0, 0)
import Data.Aeson.Key
#endif

import Data.Text (Text, pack, unpack)
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as ST
import Data.Type.Coercion (Coercion (..))

#if !MIN_VERSION_aeson(2, 0, 0)
type Key = Text

fromString :: String -> Key
fromString = pack

toString :: Key -> String
toString = unpack

toText :: Key -> Text
toText = id

fromText :: Text -> Key
fromText = id

coercionToText :: Maybe (Coercion Key Text)
coercionToText = Just Coercion
#endif

#if !MIN_VERSION_aeson(2, 0, 2)
toShortText :: Key -> ShortText
toShortText = ST.fromText . toText

fromShortText :: ShortText -> Key
fromShortText = fromText . ST.toText
#endif
