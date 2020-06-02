--1

{-tribonacci :: Int -> Int
tribonacci n | n <= 2
             | otherwise = tribonacci(n-1) + tribonacci(n-2) + tribonacci(n-3) -}

--2
{-esMultiploDe3 :: Int -> Bool
esMultiploDe3 3 = True
esMultiploDe3 2 = False
esMultiploDe3 1 = False
esMultiploDe3 0 = False
esMultiploDe3 n | n > 0 && EsMultiploDe3(n-3)
                | otherwise = False-}

--3
diabolico :: Int -> Bool
diabolico 6 = True
diabolico n | n `mod` 10 == 6 = diabolico (n `div` 10)
            | otherwise = False

--4
diabolicoTodo :: Int -> Bool
diabolicoTodo 6 = True
diabolicoTodo n | n `mod` 10 == 0 = diabolico (n `div` 10)
                | otherwise = False

--5
--resta :: Int -> Int -> Int--