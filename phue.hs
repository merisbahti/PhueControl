{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP
import Data.Aeson
import Control.Applicative
import Data.ByteString.Lazy (pack)
import Data.ByteString.Internal

data Light = Light 
  { state :: LightState
  } deriving Show

instance FromJSON Light where
   parseJSON (Object v) = Light <$>
                          v .: "state"
   parseJSON _          = error "Can't decode into Light - not Object."


exJsonLightState' :: String
exJsonLightState' = "{\"on\":true,\"bri\":174,\"hue\":0,\"sat\":0,\"xy\":[0.4753,0.4676],\"ct\":241,\"alert\":\"none\",\"effect\":\"none\",\"colormode\":\"xy\",\"reachable\":false}"
exJsonLightState = exJsonLightState'
cs = Data.ByteString.Lazy.pack . map c2w 

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

lights :: IO Data.ByteString.Lazy.Internal.ByteString
lights = simpleHTTP (getRequest $ apiBaseUrl ++ "lights") >>= getResponseBody >>= return .  cs

apiBaseUrl = "http://192.168.2.13/api/1167c7272c58f39f20e271172bbe1f8f/"
