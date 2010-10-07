module Main where

import List

data Operator = Add | Minus | Multiply | Divide
    deriving (Show, Eq, Ord)
data Token a = Op Operator | N a
    deriving (Show, Eq, Ord)

calc :: (Fractional a) => [Token a] -> Maybe a
calc xs = stackCalc xs []

stackCalc :: (Fractional a) => [Token a] -> [a] -> Maybe a
stackCalc [] [n] = Just n
stackCalc (N n:xs) ns = stackCalc xs $ n:ns
stackCalc (Op Add:xs) (a:b:ns) = stackCalc xs $ (a+b):ns
stackCalc (Op Minus:xs) (a:b:ns) = stackCalc xs $ (a-b):ns
stackCalc (Op Multiply:xs) (a:b:ns) = stackCalc xs $ (a*b):ns
stackCalc (Op Divide:xs) (a:b:ns) | b == 0 = Nothing
                                  | otherwise = stackCalc xs $ (a/b):ns
stackCalc _ _ = undefined

permutationsGen :: [a] -> [[a]]
permutationsGen nums = map (map (nums !!)) $ permutations $ length nums where
    permutations :: Int -> [[Int]]
    permutations n | n == 0 = [[]]
                   | otherwise = concat $ map (insertN n) $ permutations (n-1)
    insertN :: Int -> [Int] -> [[Int]]
    insertN n ns = map (\i -> take i ns ++ [n-1] ++ drop i ns) [0..(n-1)]

stackGen :: [a] -> [[Token a]]
stackGen nums = map reverse $ concat $ map (\ns -> stackGenReverse ns 0 []) $ permutationsGen nums

stackGenReverse :: [a] -> Int -> [Token a] -> [[Token a]]
stackGenReverse [] 1 xs = [xs]
stackGenReverse [] n xs = insertOperator [] n xs
stackGenReverse nums n xs = insertNumber nums n xs ++ insertOperator nums n xs

insertOperator :: [a] -> Int -> [Token a] -> [[Token a]]
insertOperator nums n xs | n < 2 = []
                         | otherwise = concat $
    [ stackGenReverse nums (n-1) (Op Add:xs)
    , stackGenReverse nums (n-1) (Op Minus:xs)
    , stackGenReverse nums (n-1) (Op Multiply:xs)
    , stackGenReverse nums (n-1) (Op Divide:xs)
    ]

insertNumber :: [a] -> Int -> [Token a] -> [[Token a]]
insertNumber (num:nums) n xs = stackGenReverse nums (n+1) (N num:xs)
insertNumber _ _ _ = undefined

prettyPrint :: (RealFrac a) => [Token a] -> String
prettyPrint tokens = prettyPrintStack tokens []

prettyPrintStack :: (RealFrac a) => [Token a] -> [String] -> String
prettyPrintStack [] [str] = str
prettyPrintStack (N a:tokens) strs = prettyPrintStack tokens $ (show (round a :: Int)):strs
prettyPrintStack (Op Add:tokens) (a:b:strs) = prettyPrintStack tokens $ (a ++ "+" ++ b):strs
prettyPrintStack (Op Minus:tokens) (a:b:strs) = prettyPrintStack tokens $ (a ++ "-" ++ flipSign b):strs
prettyPrintStack (Op Multiply:tokens) (a:b:strs) = prettyPrintStack tokens $ (multiplyParenthesis a ++ "*" ++ multiplyParenthesis b):strs
prettyPrintStack (Op Divide:tokens) (a:b:strs) = prettyPrintStack tokens $ (multiplyParenthesis a ++ "/" ++ divideParenthesis b):strs
prettyPrintStack _ _ = undefined

flipSign :: String -> String
flipSign xs = snd $ foldr flipSignChar (0, "") xs where
    flipSignChar :: Char -> (Int, String) -> (Int, String)
    flipSignChar x (n, ys) | x == '+' && n == 0 = (n, '-':ys)
                           | x == '-' && n == 0 = (n, '+':ys)
                           | x == '(' = (n+1, '(':ys)
                           | x == ')' = (n-1, ')':ys)
                           | otherwise = (n, x:ys)

multiplyParenthesis :: String -> String
multiplyParenthesis xs = if searchPlusOrMinus 0 xs
    then "(" ++ xs ++ ")"
    else xs

divideParenthesis :: String -> String
divideParenthesis xs = if searchPlusOrMinus 0 xs
    then "(" ++ xs ++ ")"
    else snd $ foldr flipMultiplyChar (0, "") xs where
        flipMultiplyChar :: Char -> (Int, String) -> (Int, String)
        flipMultiplyChar x (n, ys) | x == '*' && n == 0 = (n, '/':ys)
                                   | x == '/' && n == 0 = (n, '*':ys)
                                   | x == '(' = (n+1, '(':ys)
                                   | x == ')' = (n-1, ')':ys)
                                   | otherwise = (n, x:ys)

searchPlusOrMinus :: Int -> String -> Bool
searchPlusOrMinus 0 [] = False
searchPlusOrMinus n (c:cs) | c == '(' = searchPlusOrMinus (n+1) cs
                           | c == ')' = searchPlusOrMinus (n-1) cs
                           | n == 0 && (c == '+' || c == '-') = True
                           | otherwise = searchPlusOrMinus n cs
searchPlusOrMinus n cs = error $ show n ++ " " ++ cs

uniqueAfterSort :: (Eq a) => [a] -> [a]
uniqueAfterSort xs = foldr addToList [] xs where
    addToList x [] = [x]
    addToList x ys@(y:_) | x == y = ys
                         | otherwise = x:ys

main :: IO ()
main = do
    nums <- return ([8, 8, 3, 3] :: [Rational])
    tokensList <- return $ filter (\tokens -> Just 24 == calc tokens) $ stackGen nums
    expressions <- return $ uniqueAfterSort . sort $ map prettyPrint tokensList
    mapM_ putStrLn expressions
    print $ (length tokensList, length expressions)


