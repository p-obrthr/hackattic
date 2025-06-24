import System.Environment (lookupEnv)
import System.Exit (die)
import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as B8
import qualified Data.CaseInsensitive as CI
import Data.List (isPrefixOf)
import Data.Word (Word8, Word32)
import Data.Bits (shiftL, (.|.))
import Data.Binary.Get (runGet, getFloatle, getDoublele, getDoublebe)
import Numeric (readHex)
import Data.Binary.IEEE754 (wordToFloat)

main :: IO ()
main = do
  token <- lookupEnv "HACKATTIC_TOKEN" >>= maybe (die "HACKATTIC_TOKEN not found") return

  let requestUrl = "https://hackattic.com/challenges/help_me_unpack/problem?access_token=" ++ token

  getRequest <- parseRequest requestUrl 
  response <- httpLBS getRequest

  let base64 = extractBase64 $ L8.unpack $ getResponseBody response

  putStrLn $ "\nBASE64:\n" ++ base64 ++ "\n"
  
  let allBytes = convertAllBytes base64
  let int = bytesToInt $ hexBytesToDec $ take 4 allBytes 

  let restBytes1 = drop 4 allBytes
  
  let uint = bytesToUInt $ hexBytesToDec $ take 4 restBytes1
  let restBytes2 = drop 4 restBytes1

  let short = bytesToShort $ hexBytesToDec $ take 2 restBytes2 
  let restBytes3 = drop 2 restBytes2

  -- 2 bytes padding
  let restBytes4 = drop 2 restBytes3

  let float = hexStringsToFloat $ take 4 restBytes4
  let restBytes5 = drop 4 restBytes4

  let double = bytesToDouble $ hexBytesToDec $ take 8 restBytes5
  let restBytes6 = drop 8 restBytes5
  
  let doubleBE = bytesToDoubleBE $ hexBytesToDec $ take 8 restBytes6

  let solution =
        "{\"int\": " ++ show int ++
        ", \"uint\": " ++ show uint ++
        ", \"short\": " ++ show short ++
        ", \"float\": " ++ show float ++
        ", \"double\": " ++ show double ++
        ", \"big_endian_double\": " ++ show doubleBE ++ "}"

  putStrLn $ "SOLUTION:\n" ++ solution ++ "\n"

  postRequest <-
    parseRequest $
      "POST https://hackattic.com/challenges/help_me_unpack/solve?access_token="
        ++ token
        ++ "&playground=1" --after successful solution

  let jsonBody = L8.pack solution
  let req = setRequestMethod (B8.pack "POST")
            $ setRequestBodyLBS jsonBody
            $ setRequestHeader (CI.mk $ B8.pack "Content-Type") [B8.pack "application/json"]
            $ postRequest
  
  resp <- httpLBS req

  putStrLn "RESPONSE:"
  L8.putStrLn $ getResponseBody resp

extractBase64 :: String -> String
extractBase64 [] = error ""
extractBase64 str@(x:xs)
  | prefix `isPrefixOf` str = takeWhile (/= '"') (drop (length prefix) str)
  | otherwise = extractBase64 xs
  where
    prefix = "\"bytes\": \""

convertAllBytes :: String -> [String]
convertAllBytes [] = [] 
convertAllBytes xs = concat $ map base64ToHexBytes $ splitBy 4 xs 

base64ToHexBytes :: String -> [String]
base64ToHexBytes [] = []
base64ToHexBytes xs = map binToHex $ splitBy 8 $ concat $ map extendTo6Bit $ map decToBin $ map charTo6Bit xs 

decToBin :: Int -> String 
decToBin x = extract x "" 
  where
    extract 0 acc = reverse acc
    extract x acc = extract (x `div` 2) (acc ++ show (x `mod` 2))

extendTo6Bit :: String -> String
extendTo6Bit x
  | length x < 6 = replicate (6 - length x) '0' ++ x
  | otherwise = x

splitBy :: Int -> String -> [String]
splitBy val xs = split xs []
  where
    split [] acc = reverse acc
    split ys acc = split rest (chunk : acc)
      where
        (chunk, rest) = splitAt val ys

charTo6Bit :: Char -> Int
charTo6Bit c
  | c >= 'A' && c <= 'Z' = fromEnum c - fromEnum 'A'
  | c >= 'a' && c <= 'z' = fromEnum c - fromEnum 'a' + 26
  | c >= '0' && c <= '9' = fromEnum c - fromEnum '0' + 52
  | c == '+' = 62
  | c == '/' = 63
  | otherwise = 0

binToHex :: String -> String
binToHex [] = ""
binToHex binary = decToHex $ binToDec binary

binToDec :: String -> Int
binToDec bin = parse bin 0
  where
    parse [] acc = acc
    parse (x:xs) acc = parse xs $ acc * 2 + bit x

    bit '1' = 1
    bit '0' = 0
 
decToHex :: Int -> String
decToHex n
  | n < 16 = [hexDigit n]
  | otherwise = decToHex (n `div` 16) ++ [hexDigit (n `mod` 16)]

hexDigit :: Int -> Char
hexDigit n
  | n < 10 = toEnum $ n + fromEnum '0'
  | otherwise = toEnum $ n - 10 + fromEnum 'a'

hexCharToInt :: Char -> Int
hexCharToInt c
  | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
  | c >= 'A' && c <= 'F' = fromEnum c - fromEnum 'A' + 10
  | c >= 'a' && c <= 'f' = fromEnum c - fromEnum 'a' + 10
  | otherwise = error "invalid hex char" 

hexByteToDec :: String -> Int
hexByteToDec xs = convert xs 0
  where
    convert [] acc     = acc
    convert (x:xs) acc = convert xs (acc * 16 + hexCharToInt x)

hexBytesToDec :: [String] -> [Int]
hexBytesToDec [] = []
hexBytesToDec xs = map hexByteToDec xs 

hexStringToWord8 :: String -> Word8
hexStringToWord8 str = fromIntegral (fst $ head $ readHex str)

hexListToBytes :: [String] -> [Word8]
hexListToBytes = map hexStringToWord8

bytesToWord32 :: [Word8] -> Word32
bytesToWord32 [b0,b1,b2,b3] =
  (fromIntegral b0) .|.
  (fromIntegral b1 `shiftL` 8)  .|.
  (fromIntegral b2 `shiftL` 16) .|.
  (fromIntegral b3 `shiftL` 24)

hexStringsToFloat :: [String] -> Float
hexStringsToFloat hexs = wordToFloat w32
  where
    bytes = hexListToBytes hexs
    w32 = bytesToWord32 bytes

intsToByteString :: [Int] -> L.ByteString
intsToByteString = L.pack . map fromIntegral

bytesToInt :: [Int] -> Int
bytesToInt [b0, b1, b2, b3]
  | b3 >= 128 = b0 + b1 * 256 + b2 * 256^2 + b3 * 256^3 - 2^32
  | otherwise = b0 + b1 * 256 + b2 * 256^2 + b3 * 256^3

bytesToUInt :: [Int] -> Int
bytesToUInt [b0, b1, b2, b3] =
  b0 + b1 * 256 + b2 * 256^2 + b3 * 256^3

bytesToShort :: [Int] -> Int
bytesToShort [b0, b1] 
  | b1 >= 128 = b0 + b1 * 256 - 2^16
  | otherwise = b0 + b1 * 256

bytesToDouble :: [Int] -> Double
bytesToDouble bytes
  | length bytes == 8 = runGet getDoublele (intsToByteString bytes)
  | otherwise = error "input must be 8 bytes"

bytesToDoubleBE :: [Int] -> Double
bytesToDoubleBE bytes
  | length bytes == 8 = runGet getDoublebe (intsToByteString bytes)
  | otherwise = error "input must be 8 bytes"
