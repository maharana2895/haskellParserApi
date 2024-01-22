{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

import Data.Aeson
import qualified Data.Aeson as DA
import Data.Text (Text, pack, unpack)
import Data.List.Split (splitOn)
import Data.Time.Format
import Data.Time.Calendar
import Text.Read (readMaybe)
import Data.Maybe
import GHC.Generics(Generic)
import Control.Monad.IO.Class(liftIO)

import Network.Wai (Application, responseLBS, requestMethod, pathInfo,rawPathInfo, requestBody, strictRequestBody, Response, queryString, Request, getRequestBodyChunk)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types.Status ( status200, status404, status400 )
import qualified Data.Text as T
import qualified Data.ByteString as B

-- Data type to represent your expected request body structure
data MyRequestBody = MyRequestBody
    { input :: Text
    } deriving (Generic, FromJSON, Show, ToJSON)

-- getting input param from body
getFullRequestBody :: Request -> IO B.ByteString
getFullRequestBody request = do
  chunk <- getRequestBodyChunk request
  if chunk == B.empty
    then pure chunk
  else do
    nextChunk <- getFullRequestBody request
    pure $ chunk <> nextChunk


data DataType = DDate Text | DNumber Double | DString Text | DBool Bool deriving (Generic, Eq, Show, ToJSON)

data Value1 = VArrayData [DataType] | VSingleData DataType deriving (Generic, Eq, Show, ToJSON)

data KeyValuePair = KeyValuePair
  { key :: Text
  , value :: Value1
  } deriving (Generic, Eq, Show, ToJSON)

-- Parse a single key-value pair
parseKeyValuePair :: String -> Maybe KeyValuePair
parseKeyValuePair "" = Nothing
parseKeyValuePair str = do
  let (typePart, rest) = splitAt 2 str
      valueTypeChar = typePart !! 0
      valueDataType = typePart !! 1
      keyValuePair = splitOn "|" rest
  case keyValuePair of
    [key, valuePart] -> do
      parsedValue <- parseValue valuePart valueTypeChar valueDataType
      return $ KeyValuePair (pack key) parsedValue
    _ -> Nothing

getBool :: String -> Bool
getBool val = do
 let boolVal = head val
 if elem boolVal ['y','Y','T','t'] then True else False

-- Parse the value part of a key-value pair
parseValue :: String -> Char -> Char -> Maybe Value1
parseValue valuePart valueTypeChar valueDataType= case (valueTypeChar, valueDataType) of
  ('0', '0') -> do
    date <- parseDate valuePart
    return (VSingleData $ DDate $ pack date)
  ('0', '1') -> do
    num <- readMaybe valuePart :: Maybe Double
    return (VSingleData $ DNumber num)
  ('0','2') -> return (VSingleData $ DString $ pack valuePart)
  ('0','3') -> do
    return (VSingleData $ DBool (getBool valuePart))
  ('1', _) -> do
    let arrayValues = splitOn "," valuePart
    case valueDataType of
     '0' -> do
       let dateArr = map (pack) (catMaybes (map parseDate arrayValues))
       return (VArrayData $ map DDate dateArr)

     '1' -> do
       let numArr = map readMaybe arrayValues :: [Maybe Double]
       return (VArrayData $ (map DNumber (catMaybes numArr)))
     '2' -> return (VArrayData $ map DString (map pack arrayValues))
     '3' -> do
       let boolArr = map getBool arrayValues
       return (VArrayData $ map DBool boolArr)
  _ -> Nothing

-- Parse the date in the format YYYY-MM-DD
parseDate :: String -> Maybe String
parseDate datePart = do
  let [yearPart, monthPart, dayPart] = splitOn "-" datePart
  year <- readMaybe yearPart
  month <- readMaybe monthPart
  day <- readMaybe dayPart
  let parsedDate = fromGregorian (toInteger year) month day
  return $ formatTime defaultTimeLocale "%F" parsedDate

getparsedVal :: String -> Value
getparsedVal input = do
  let keyValues = splitOn "#" input
  let parsedKeyValues = map parseKeyValuePair keyValues
  toJSON (catMaybes parsedKeyValues)

checkBoolArrType :: [[Char]] -> Bool
checkBoolArrType [] = False
checkBoolArrType (x:xs) = if notElem (head x) ['y','Y','T','t'] then True else checkBoolArrType xs

validateKeyValue :: [[Char]] -> (Bool, String)
validateKeyValue [] = (True, "")
validateKeyValue (str:xs) = do
 let (typePart, rest) = splitAt 2 str
     valueTypeChar = typePart !! 0
     valueDataType = typePart !! 1
     keyValuePair = splitOn "|" rest
     valuePart = if length keyValuePair > 1 then keyValuePair !! 1 else "r"
     checkBoolType =
      case (valueTypeChar, valueDataType) of
       ('0', '3') -> if notElem (head valuePart) ['y','Y','T','t'] then True else False
       ('1', '3') -> do
        let arrayValues = splitOn "," valuePart
        checkBoolArrType arrayValues
       (_, _) -> False
 if length str < 3 then
   (False, "Wrong input")
  else if (notElem valueTypeChar ['0','1']) then
   (False, "isArray param is wrong in input")
  else if (notElem valueDataType ['0','1','2','3']) then
   (False, "type is wrong in input")
  else if length keyValuePair == 0 then
   (False, "wrong key value input")
  else if checkBoolType then
   (False, "wrong bool type in input")
  else
   validateKeyValue xs


validateInput :: String -> (Bool, String)
validateInput str = do
 let keyValues = drop 1 (splitOn "#" str)
 if length keyValues == 0 then
  (False, "Delimeter # should be in input")
 else
  validateKeyValue keyValues


-- Your API endpoint logic
apiHandler :: T.Text -> Response
apiHandler input =
 if successFullvalidated then
  responseLBS
     status200
     headers
     body
 else
  responseLBS
     status400
     headers
     (encode $ object ["result" .= str])
  where
   (successFullvalidated, str) = validateInput (unpack input)
   status = status200
   headers = [("Content-Type", "application/json")]
   body = encode $ object ["result" .= getparsedVal (unpack input)]

-- Application logic
app :: Application
app req respond = do
    body <- liftIO $ getFullRequestBody req
    let path =rawPathInfo req
    let method = requestMethod req
    if method == "GET" && path == "/api/parse" then do
     let response = case (DA.decodeStrict body) :: Maybe MyRequestBody of
                             Nothing -> responseLBS status400 [("Content-Type", "application/json")] (encode $ object [("Reason", "Bad Request")])
                             Just aa -> apiHandler (input aa)
     respond response
    else do
     let response = responseLBS status400 [] "URL not supported"
     respond response

-- Main function to run the server
main :: IO ()
main = do
    putStrLn "Starting server on http://localhost:8080"
    run 8080 app
