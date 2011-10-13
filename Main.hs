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
-- Simpliify Expressions
--
simplify :: (RealFrac a) => Expr a -> Expr a
simplify a = simpl a

simpl :: (RealFrac a) => Expr a -> Expr a
simpl (Add a (Add b c)) = simpl $ a + b + c
simpl (Add a (Minus b c)) = simpl $ a + b - c
simpl (Minus a (Add b c)) = simpl $ a - b - c
simpl (Minus a (Number 0)) = simpl a + 0
simpl (Multiply a (Multiply b c)) = simpl $ a * b * c
simpl (Multiply a (Divide b c)) = simpl $ a * b / c
simpl (Divide a (Divide b c)) = simpl $ a * c / b
simpl (Divide a (Multiply b c)) = simpl $ a / b / c
simpl (Divide a (Number 1)) = simpl a * 1
simpl (Divide (Number 0) a) = 0 * simpl a
simpl (Add a b) = simpl a + simpl b
simpl (Minus a b) = case tryNegate b of
    Just b' -> simpl $ a + b'
    Nothing -> simpl a - simpl b
simpl (Multiply a b) = simpl a * simpl b
simpl (Divide a b) = simpl a / simpl b
simpl a = a

tryNegate :: (RealFrac a) => Expr a -> Maybe (Expr a)
tryNegate (Minus a b) = return $ Minus b a
tryNegate (Multiply a b) = case tryNegate a of
    Just a' -> return $ Multiply a' b
    Nothing -> do
        b' <- tryNegate b
        return $ Multiply a b'
tryNegate (Divide a b) = case tryNegate a of
    Just a' -> return $ Divide a' b
    Nothing -> do
        b' <- tryNegate b
        return $ Divide a b'
tryNegate (Number 0) = return $ Number 0
tryNegate _ = Nothing

--
-- List Expressions
--
data ExprList a = Element a
                | AddList [(ExprList a)] [(ExprList a)]
                | MultiplyList [(ExprList a)] [(ExprList a)]
    deriving (Show, Eq)

instance (RealFrac a) => Ord (ExprList a) where
    Element x <= Element y = x >= y
    Element _ <= _ = False
    _ <= Element _ = True
    MultiplyList xs xs' <= MultiplyList ys ys' = (xs, xs') <= (ys, ys')
    MultiplyList _ _ <= _ = False
    _ <= MultiplyList _ _ = True
    AddList xs xs' <= AddList ys ys' = (xs, xs') <= (ys, ys')

instance (RealFrac a) => Num (ExprList a) where
    AddList xs xs' + AddList ys ys' = AddList (xs ++ ys) (xs' ++ ys')
    AddList xs xs' + a = AddList (a:xs) xs'
    a + AddList xs xs' = AddList (a:xs) xs'
    a + b = AddList [a, b] []
    AddList xs xs' - AddList ys ys' = AddList (xs ++ ys') (xs' ++ ys)
    AddList xs xs' - a = AddList xs (a:xs')
    a - AddList xs xs' = AddList (a:xs') xs
    a - b = AddList [a] [b]
    MultiplyList xs xs' * MultiplyList ys ys' = MultiplyList (xs ++ ys) (xs' ++ ys')
    MultiplyList xs xs' * a = MultiplyList (a:xs) xs'
    a * MultiplyList xs xs' = MultiplyList (a:xs) xs'
    a * b = MultiplyList [a, b] []
    abs = error "abs"
    signum = error "signum"
    fromInteger = error "fromInteger"

instance (RealFrac a) => Fractional (ExprList a) where
    MultiplyList xs xs' / MultiplyList ys ys' = MultiplyList (xs ++ ys') (xs' ++ ys)
    MultiplyList xs xs' / a = MultiplyList xs (a:xs')
    a / MultiplyList xs xs' = MultiplyList (a:xs') xs
    a / b = MultiplyList [a] [b]
    fromRational = error "fromRational"

exprListFromExpr :: (RealFrac a) => Expr a -> ExprList a
exprListFromExpr (Number x) = Element x
exprListFromExpr (Add a b) = exprListFromExpr a + exprListFromExpr b
exprListFromExpr (Minus a b) = exprListFromExpr a - exprListFromExpr b
exprListFromExpr (Multiply a b) = exprListFromExpr a * exprListFromExpr b
exprListFromExpr (Divide a b) = exprListFromExpr a / exprListFromExpr b
exprListFromExpr _ = error  "exprListFromExpr"

sortExprList :: (RealFrac a) => ExprList a -> ExprList a
sortExprList (Element x) = Element x
sortExprList (AddList xs xs') = AddList (sort . map sortExprList $ xs)
                                        (sort . map sortExprList $ xs')
sortExprList (MultiplyList xs xs') = MultiplyList (sort . map sortExprList $ xs)
                                                  (sort . map sortExprList $ xs')

exprFromExprList :: (RealFrac a) => ExprList a -> Expr a
exprFromExprList (Element x) = Number x
exprFromExprList (AddList [] []) = error "exprFromExprList"
exprFromExprList (AddList xs (x':xs') ) = Minus
    ( exprFromExprList $ AddList xs xs' ) ( exprFromExprList x' )
exprFromExprList (AddList [x] [] ) = exprFromExprList x
exprFromExprList (AddList (x:xs) [] ) = Add
    ( exprFromExprList $ AddList xs [] ) ( exprFromExprList x )
exprFromExprList (MultiplyList [] []) = error "exprFromExprList"
exprFromExprList (MultiplyList xs (x':xs') ) = Divide
    ( exprFromExprList $ MultiplyList xs xs' ) ( exprFromExprList x' )
exprFromExprList (MultiplyList [x] [] ) = exprFromExprList x
exprFromExprList (MultiplyList (x:xs) [] ) = Multiply
    ( exprFromExprList $ MultiplyList xs [] ) ( exprFromExprList x )

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
expr24 nums = map simplify $ filter (\expr -> Just 24 == eval expr) $ buildExpr nums

expressions24 :: (RealFrac a) => [a] -> [String]
expressions24 nums = uniqueAfterSort . sort $ map (show . exprFromExprList . sortExprList . exprListFromExpr) $ expr24 nums

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
    let result = map prints24 $ (numsGen 4 0 9 :: [[Rational]])
    mapM_ putStrLn result
