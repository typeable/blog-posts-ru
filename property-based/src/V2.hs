{-# LANGUAGE DeriveGeneric #-}
module V2 where

import           Control.Arrow   (first, (***))
import           Data.Char       (isDigit)
import           Data.List       (intercalate)
import           Data.Maybe
import           Generic.Random
import           GHC.Generics
import           Test.QuickCheck
import           Text.Read       (reads)

data Json
  = Object [(String, Json)]
  | Array [Json]
  | String String
  | Number Double
  deriving (Show, Eq, Generic)

instance Arbitrary Json where
  arbitrary = genericArbitraryRec uniform `withBaseCase` return (Array [])

serialize :: Json -> String
serialize (Object props) =
  "{" ++ intercalate "," (map toKeyValue props) ++ "}"
  where
    toKeyValue (key, value) = serializeString key ++ ":" ++ serialize value
serialize (Array entries) =
  "[" ++ intercalate "," (map serialize entries) ++ "]"
serialize (String str) = show str
serialize (Number n) = show n

serializeString :: String -> String
serializeString = show

parse :: String -> Maybe Json
parse input = case decode input of
  Just (json, "") -> Just json
  _               -> Nothing

decode :: String -> Maybe (Json, String)
decode ('{' : rest)                = first Object <$> decodeProps rest
decode ('[' : rest)                = first Array <$> decodeArray rest
decode ('"' : rest)                = first String <$> decodeString rest
decode (c : rest)
  | isDigit c || c == '-' = first Number <$> decodeNumber (c:rest)
decode _ = Nothing

decodeProps :: String -> Maybe ([(String, Json)], String)
decodeProps ('}' : rest) = Just ([], rest)
decodeProps (',' : rest) = decodeProps rest
decodeProps ('"' : input) = do
  (key, ':' : input') <- decodeString input
  (value, input'') <- decode input'
  (restProps, input''') <- decodeProps input''
  return ((key, value) : restProps, input''')

decodeArray :: String -> Maybe ([Json], String)
decodeArray (']' : rest) = Just ([], rest)
decodeArray (',' : rest) = decodeArray rest
decodeArray input = do
  (entry, rest) <- decode input
  first (entry :) <$> decodeArray rest

decodeString :: String -> Maybe (String, String)
decodeString = listToMaybe . reads . ('"':)

decodeNumber :: String -> Maybe (Double, String)
decodeNumber = listToMaybe . reads

-- unused
example1 = Object
  [ ("k1", Array [Number 1.0, Number 2.0, Object [], Object [("k4", String "b")]])
  , ("k2", String "a")
  ]

-- unused
example2 = Object
  [ ("k1", String "a")
  ]

prop_serialize_parse :: Json -> Property
prop_serialize_parse json = parse (serialize json) === Just json
