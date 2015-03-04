import Data.Char

version = "0.2"

--------------------------------------------------------
--- Main function
--------------------------------------------------------

main :: IO()
main = do
    putStrLn intro
    putStrLn " :: Please insert PIN code"
    putStrLn " :: PIN can't be 0"
    pin <- getLine
    -- Checking pin
    if (read pin :: Int) == 0 then
        error " :: PIN can't be 0"
        else
            putStr ""
    putStrLn " :: Please insert KEY code, with values between square brakets separated by commas"
    putStrLn " :: DO NOT insert 0 values in KEY"
    key <- getLine
    -- Checking key
    if 0 `elem` (read key :: [Int]) then
        error " :: KEY cannot contain 0!"
        else if null (read key :: [Int]) then
            error " :: KEY cannot be empty!"
            else
                putStr ""
    putStrLn " :: Please insert the message"
    message <- getLine
    if length key > length message then
        putStrLn " :: Warning: useless components in KEY:"
        putStrLn " ::   number of values in KEY is greater than number of chars in message"
        else
            putStr ""
    putStrLn " :: Choose what you want to do (enc/dec)"
    mode <- getLine
    case mode of
        "enc" -> putStrLn (encode (read pin :: Int) (read key :: [Int]) message)
        "dec" -> putStrLn (decode (read pin :: Int) (read key :: [Int]) message)
        _     -> putStrLn " :: Wrong mode. Please choose enc to encrypt or dec to decrypt"


--------------------------------------------------------
--- Encoding functions
--------------------------------------------------------

encode :: Int -> [Int] -> String -> String
encode pin key message =
    tail $
    replacements $
    reverse $
    concat $
    addSeparators $
    intToTextList $
    applyKey key $
    applyPin pin $
    extractAsciiVals message

extractAsciiVals :: String -> [Int]
extractAsciiVals = map ord

applyPin :: Int -> [Int] -> [Int]
applyPin pin list = [e * (pin ^ 2) | e <- list]

applyKey :: [Int] -> [Int] -> [Int]
applyKey key list = [uncurry (*) e | e <- zippedList]
    where
        zippedList = zip list ((concat . repeat) key)

intToTextList :: [Int] -> [String]
intToTextList = map show

addSeparators :: [String] -> [String]
addSeparators = map (++ "_")

replacements :: String -> String
replacements =
    replace '0' 'o' .
    replace '1' 'a' .
    replace '2' '+' .
    replace '3' '-' .
    replace '5' '$'

--------------------------------------------------------
--- Decocoding functions
--------------------------------------------------------

decode :: Int -> [Int] -> String -> String
decode pin key message =
    convertToChars $
    deapplyPin pin $
    deapplyKey key $
    textToIntList $
    splitStringIntoWords $
    reverseReplacements $
    reverse message

reverseReplacements :: String -> String
reverseReplacements =
    replace '_' ' ' .
    replace 'o' '0' .
    replace 'a' '1' .
    replace '+' '2' .
    replace '-' '3' .
    replace '$' '5'

splitStringIntoWords :: String -> [String]
splitStringIntoWords = words

textToIntList :: [String] -> [Int]
textToIntList list = [read e :: Int | e <- list]

deapplyKey :: [Int] -> [Int] -> [Int]
deapplyKey key list = [uncurry quot e | e <- zippedList]
    where
        zippedList = zip list ((concat . repeat) key)

deapplyPin :: Int -> [Int] -> [Int]
deapplyPin pin list = [quot e (pin ^ 2) | e <- list]

convertToChars :: [Int] -> String
convertToChars = map chr

--------------------------------------------------------
--- Utilities functions
--------------------------------------------------------

--takeAlternate :: [a] -> [a]
--takeAlternate list = [ list !! n | n <- [0..((length list) - 1 )], n `mod` 2 == 0 ]

intro :: String
intro = "-----------------------------------------------\n\
    \Haskell Message Encrypter by Gianluca Mondini\n\
    \-----------------------------------------------\n\
    \Version " ++ version ++ " - Different version may cause misunderstandings!\n"

test pin key message = message == decode pin key (encode pin key message)

replace :: (Eq a) => a -> a -> [a] -> [a]
replace _ _ []  = []
replace old new (x:xs)
    | x == old = new : replace old new xs
    | otherwise = x : replace old new xs
