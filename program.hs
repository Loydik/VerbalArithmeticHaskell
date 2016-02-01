import Data.Maybe

getFirstLine input = head (lines input)

expand [] = 0
expand [x] = x
expand (x:xs) = x* 10^(length xs) + expand xs

digits = [0..9]  -- all the digits

answer isOk = head [ list | list <- perm digits, isOk list]


wordToNumbers letters numbers word = map (\x -> fromJust $ lookup x (zip letters numbers)) word

isOk :: String -> String -> String -> String -> [Integer] -> Bool
isOk letters word1 word2 word3 digits =
    head number1 /= 0 &&
    head number2 /= 0 &&
    head number3 /= 0 &&
    expand number1 + expand number2 == expand number3
  where
    mapping = zip letters digits
    toNumber = map (\x -> fromJust $ lookup x mapping)
    number1 = toNumber word1
    number2 = toNumber word2
    number3 = toNumber word3  

parseString string = removeDuplicates (stripChars "+= " string)

stripChars :: String -> String -> String
stripChars = filter . flip notElem

removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (\y -> not(x == y)) xs)

--unique permutation
perm :: [t] -> [[t]]
perm  [] = [[]]
perm (x:xs) = [(y:zs) | (y,ys) <- del (x:xs), zs <- perm ys]

--create all possible views
del :: [t] -> [(t,[t])]
del [] = []
del (x:xs) = ((x,xs) : [ (y,(x:ys)) | (y,ys) <- del xs ])

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'


main :: IO()
main = do
       contents <- readFile "expression.txt"
       let firstLine = getFirstLine contents
       let uniqueLetters = parseString firstLine
       let firstWord = stripChars " " (head (wordsWhen (=='+') firstLine))
       let remainder = concat (tail (wordsWhen (=='+') firstLine))
       let secondWord = stripChars " " (head (wordsWhen (=='=') remainder))
       let result = stripChars " " (last (wordsWhen (=='=') remainder))
       
       print (take (length uniqueLetters) (answer (isOk uniqueLetters firstWord secondWord result)))
