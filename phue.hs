{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP
import Data.Aeson
import Control.Applicative
import Data.ByteString.Lazy (pack)
import Data.ByteString.Internal
import Data.Map.Strict (Map, fromList, (!), adjust)

data Light = Light 
  { state :: LightState
  } deriving Show

instance FromJSON Light where
   parseJSON (Object v) = Light <$>
                          v .: "state"
   parseJSON _          = error "Can't decode into Light - not Object."

data LightState = LightState 
  { on        :: Bool
  , bri       :: Int
  , hue       :: Int
  , sat       :: Int
  , xy        :: [Double]
  , alert     :: String
  , effect    :: String
  } deriving Show

instance FromJSON LightState where
   parseJSON (Object v) = LightState <$>
                          v .: "on" <*>
                          v .: "bri" <*>
                          v .: "hue" <*>
                          v .: "sat" <*>
                          v .: "xy" <*>
                          v .: "alert" <*>
                          v .: "effect" 
   -- A non-Object value is of the wrong type, so fail.
   parseJSON _          = error "Can't decode into LightState - not Object."

cs = Data.ByteString.Lazy.pack . map c2w 

lights     = simpleHTTP (getRequest $ apiBaseUrl ++ "lights") >>= getResponseBody >>= return .  cs
apiBaseUrl = "http://192.168.2.13/api/1167c7272c58f39f20e271172bbe1f8f/"
currStatus = lights >>= return . decode :: IO (Maybe (Map String Light))
{-
This is how you would get the status for light #1:
  m = do 
    x <- currStatus
    return $ fmap (! "1") x 
-}
