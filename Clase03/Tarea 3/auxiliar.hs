module Auxiliar
where

    {-’1’-}
    absoluto :: Int -> Int
    absoluto n | n < 0 = n*(-1)
            | otherwise = n
    {-’2’-}
    maximoAbsoluto :: Int -> Int -> Int
    maximoAbsoluto x y | (absoluto x) > (absoluto y) = absoluto x
                    | otherwise = absoluto y
    {-’3’-}
    maximo3 :: Int -> Int -> Int -> Int
    maximo3 x y z | ((x >= y) && (x >= z)) = x
                | ((y >= x) && (y >= z)) = y
                | otherwise = z
    {-’4’-}
    {-’
    algunoEs0 :: Float -> Float -> Bool
    algunoEs0 x y | x == 0 || y == 0 = True
                | otherwise = False
    ’-}

    algunoEs0PM :: Float -> Float -> Bool
    algunoEs0PM 0 _ = True
    algunoEs0PM _ 0 = True
    algunoEs0PM _ _ = False          

    algunoEs0 :: Float -> Float -> Bool
    algunoEs0 x y = x==0 || y==0
    {-’5’-}
    {-’
    ambosSon0 :: Float -> Float -> Bool
    ambosSon0 x y | x == 0 && y == 0 = True
                | otherwise = False
    ’-}

    ambosSon0PM :: Float -> Float -> Bool
    ambosSon0PM 0 0 = True
    ambosSon0PM _ _ = False

    ambosSon0 :: Float -> Float -> Bool
    ambosSon0 x y = x==0 && y==0
    {-’6’-}
    esMultiploDe :: Int -> Int -> Bool
    esMultiploDe x y = mod x y == 0
    {-’7’-}
    digitoUnidades :: Int -> Int
    digitoUnidades n | n <= 0 = undefined
                    | otherwise = mod n 10
    {-’8’-}
    digitoDecenas :: Int -> Int
    digitoDecenas n | n <= 10 = undefined
                    | otherwise = div (mod n 100) 10