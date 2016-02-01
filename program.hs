--FINAL VERSION

--gets first line from the multiline string
getFirstLine :: String -> String
getFirstLine input = head (lines input)

--removes duplicate chars from a string
getUniqueChars :: String -> String
getUniqueChars string = removeDuplicates (stripChars "+= " string)

--removes specified chars from a string
stripChars :: String -> String -> String
stripChars = filter . flip notElem

--removes duplicates from a string
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (\y -> x /= y) xs)


--splits string on the occurence of a specified char
splitString     :: (Char -> Bool) -> String -> [String]
splitString p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : splitString p s''
                            where (w, s'') = break p s'
                            

--"expands" a list of numbers, according to the formula - a + b*10 + c*100 etc.
expand :: Num a => [a] -> a
expand [] = 0
expand [x] = x
expand (x:xs) = x* 10 ^ length xs + expand xs

--gets the answer to the cryptarithm
getAnswer :: ([Integer] -> Bool) -> [Integer]
getAnswer isOk = head [ list | list <- perm [0..9], isOk list]


--checks whether list of numbers fits as a solution
isOk :: (Eq a, Eq b, Num b) => [a] -> [a] -> [a] -> [a] -> [b] -> Bool
isOk letters var1 var2 result numbers = expand (wordToDigits letters numbers var1) + expand (wordToDigits letters numbers var2) == expand (wordToDigits letters numbers result) 
                                        && head (wordToDigits letters numbers var1) /= 0 
                                        && head (wordToDigits letters numbers var2) /= 0 
                                        && head (wordToDigits letters numbers result) /= 0

--transforms a list of chars into numbers
wordToDigits :: (Eq a, Num b) => [a] -> [b] -> [a] -> [b]
wordToDigits letters numbers word = map (\x -> extractFromJust $ lookup x (zip letters numbers)) word


-- permutation
perm :: [t] -> [[t]]
perm  [] = [[]]
perm (x:xs) = [(y:zs) | (y,ys) <- views (x:xs), zs <- perm ys]

--create all possible views (x_j, [x_0,...,x_{j-1},x_{j+1},...x_{n-1}]) of an n-length list [x_0,...,x_{n-1}]
views :: [t] -> [(t,[t])]
views [] = []
views (x:xs) = ((x,xs) : [ (y,(x:ys)) | (y,ys) <- views xs ])

-- extracts the element out of a 'Just' using pattern matching
extractFromJust :: Maybe a -> a
extractFromJust (Just x) = x


main :: IO()
main = do
       contents <- readFile "expression.txt"
       let firstLine = getFirstLine contents
       let uniqueLetters = getUniqueChars firstLine
       let firstWord = stripChars " " (head (splitString (=='+') firstLine))
       let remainder = concat (tail (splitString (=='+') firstLine))
       let secondWord = stripChars " " (head (splitString (=='=') remainder))
       let result = stripChars " " (last (splitString (=='=') remainder))
       
       print (take (length uniqueLetters) (getAnswer (isOk uniqueLetters firstWord secondWord result)))
