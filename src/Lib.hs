module Lib
  ( getData
  , parseRequest
  , Collection(Email, FirstName, LastName, PhoneNumber, City, Country)
  ) where

import Store

data Collection
  = Email
  | FirstName
  | LastName
  | PhoneNumber
  | City
  | Country
  | Empty
  deriving (Show)

parseRequest :: [(String, String)] -> [(String, Collection)]
parseRequest =
  map
    (\x ->
       case snd x of
         "EMAIL" -> (fst x, Email)
         "FIRST_NAME" -> (fst x, FirstName)
         "LAST_NAME" -> (fst x, LastName)
         "PHONE_NUMBER" -> (fst x, PhoneNumber)
         "CITY" -> (fst x, City)
         "COUNTRY" -> (fst x, Country)
         _ -> (fst x, Empty))

getData :: Collection -> Int -> String
getData collection =
  case collection of
    Empty -> const ""
    _ -> (!!) list
      where list = getCollection collection

getCollection :: Collection -> [String]
getCollection x =
  case x of
    Email -> emails
    FirstName -> firstNames
    LastName -> lastNames
    PhoneNumber -> phoneNumbers
    City -> cities
    Country -> countries
