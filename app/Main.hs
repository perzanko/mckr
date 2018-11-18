module Main where

import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy.Char8
import Data.Map (Map, toList)
import GHC.Generics
import Lib

requestPayload :: String
requestPayload =
  "{\"email\":\"EMAIL\",\"name\":\"FIRST_NAME\",\"surname\":\"LAST_NAME\",\"phone\":\"PHONE_NUMBER\"}"

limit :: Int
limit = 30

main :: IO ()
main =
  let parsedRequestPayload = decodeRequestPayload requestPayload
      count = convertLimitToList limit
   in print $ Prelude.map (getDataPerIndex parsedRequestPayload) count

convertLimitToList :: Int -> [Int]
convertLimitToList list =
        case () of
          _
            | limit < 100 -> [0 .. limit - 1]
            | otherwise -> [0 .. 99]

getDataPerIndex :: [(String, Collection)] -> Int -> [(String, String)]
getDataPerIndex list index =
  Prelude.map (\a -> (fst a, getData (snd a) index)) list

decodeRequestPayload :: String -> [(String, Collection)]
decodeRequestPayload value = parsedList
  where
    packedValue = pack value
    decoded = decode packedValue :: Maybe (Map String String)
    parsedList =
      case decoded of
        Nothing -> error "JSON is invalid."
        Just a -> (parseRequest . toList) a
