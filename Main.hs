module Main where

import Data.List
import Control.Monad

--
-- Define Expressions
--
data Expr a = Number a
            | Add (Expr a) (Expr a)
            | Minus (Expr a) (Expr a)
            | Multiply (Expr a) (Expr a)
            | Divide (Expr a) (Expr a)
            | Abs (Expr a)
            | Signum (Expr a)
            | Recip (Expr a)
    deriving (Eq, Ord)

instance (RealFrac a) => Num (Expr a) where
    a + b = Add a b
    a - b = Minus a b
    a * b = Multiply a b
    abs a = Abs a
    signum a = Signum a
    fromInteger x = Number $ fromInteger x

instance (RealFrac a) => Fractional (Expr a) where
    a / b = Divide a b
    recip a = Recip a
    fromRational x = Number $ fromRational x

instance (Show a, RealFrac a) => Show (Expr a) where
    showsPrec p (Number x) = showsPrec p $ (round x :: Integer)
    showsPrec p (Add a b) = showParen (p>6) $
        showsPrec 6 a . ('+':) . showsPrec 6 b
    showsPrec p (Minus a b) = showParen (p>6) $
        showsPrec 6 a . ('-':) . showsPrec 7 b
    showsPrec p (Multiply a b) = showParen (p>7) $
        showsPrec 7 a . ('*':) . showsPrec 7 b
    showsPrec p (Divide a b) = showParen (p>7) $
        showsPrec 7 a . ('/':) . showsPrec 8 b
    showsPrec _ (Abs a) = ( "abs" ++ ) . showParen True (showsPrec 0 a)
    showsPrec _ (Signum a) = ( "signum" ++ ) . showParen True (showsPrec 0 a)
    showsPrec _ (Recip a) = ( "recip" ++ ) . showParen True (showsPrec 0 a)

--
-- Evaluate Expressions
--
eval :: (RealFrac a) => Expr a -> Maybe a
eval (Number x) = return x
eval (Add a b) = do
    x <- eval a
    y <- eval b
    return $ x + y
eval (Minus a b) = do
    x <- eval a
    y <- eval b
    return $ x - y
eval (Multiply a b) = do
    x <- eval a
    y <- eval b
    return $ x * y
eval (Divide a b) = do
    x <- eval a
    y <- eval b
    case y of
        0 -> fail "Divide by zero."
        _ -> return $ x / y
eval (Abs a) = liftM abs $ eval a
eval (Signum a) = liftM signum $ eval a
eval (Recip a) = liftM recip $ eval a

--
-- Build Expressions
--
buildExpr :: (RealFrac a) => [a] -> [Expr a]
buildExpr [] = []
buildExpr [x] = [Number x]
buildExpr xs = concat $ iterate ( concat . map contract ) [es] !! ( len - 1 ) where
    len = length es
    es = map Number xs

contract :: (RealFrac a) => [Expr a] -> [[Expr a]]
contract xs = concat . map combine $ pickTwoFromList xs where
    combine ( x , y , rs ) = map (:rs) $ cons x y

cons :: (RealFrac a) => Expr a -> Expr a -> [Expr a]
cons a b = [ a + b , a - b , a * b , a / b ]

pickTwoFromList :: [a] -> [ ( a , a , [a] ) ]
pickTwoFromList xs = concat $ map pickAgain $ pickOneFromList xs where
    pickAgain ( y , ys ) = map combine $ pickOneFromList ys where
        combine ( z , zs ) = ( y , z , zs )

pickOneFromList :: [a] -> [ ( a , [a] ) ]
pickOneFromList xs = pickGen id xs where
    pickGen f [y] = [ ( y , f [] ) ]
    pickGen f (y:ys) = ( y , f ys ) : pickGen ( f . (y:) ) ys
    pickGen _ _ = error "pickGen"

--
-- Output Results
--
expr24 :: (RealFrac a) => [a] -> [Expr a]
expr24 nums = filter (\expr -> Just 24 == eval expr) $ buildExpr nums

expressions24 :: (RealFrac a) => [a] -> [String]
expressions24 nums = uniqueAfterSort . sort $ map show $ expr24 nums

oneExpression24 :: (RealFrac a) => [a] -> Maybe String
oneExpression24 nums = case expressions24 nums of
    x:_ -> Just x
    [] -> Nothing

uniqueAfterSort :: (Eq a) => [a] -> [a]
uniqueAfterSort xs = foldr addToList [] xs where
    addToList x [] = [x]
    addToList x ys@(y:_) | x == y = ys
                         | otherwise = x:ys

print24 :: (RealFrac a) => [a] -> String
print24 nums = prettyPrintNums nums ++ ": " ++
    case oneExpression24 nums of
        Nothing -> "Nothing"
        Just str -> str

prints24 :: (RealFrac a) => [a] -> String
prints24 nums = prettyPrintNums nums ++ ": [" ++
    (show $ length expr) ++ "]" ++
    (concat $ map (\s -> "\n    " ++ s) expr) where
    expr = expressions24 nums

prettyPrintNums :: (RealFrac a) => [a] -> String
prettyPrintNums nums = drop 1 $ concat $ map (\x -> "," ++ show (round x :: Int)) nums

numsGen :: (RealFrac a, Enum a) => Int -> a -> a -> [[a]]
numsGen 1 minNum maxNum = map (\x -> [x]) [minNum..maxNum]
numsGen n minNum maxNum | n > 1 = do
    item <- numsGen (n-1) minNum maxNum
    map (\x -> x:item) [minNum..(head item)]
numsGen _ _ _ = undefined

main :: IO ()
main = do
    let result = map prints24 $ (numsGen 4 1 9 :: [[Rational]])
    mapM_ putStrLn result
