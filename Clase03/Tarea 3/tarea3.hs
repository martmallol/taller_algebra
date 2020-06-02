module Tarea3
where 
    import Auxiliar
    --1
    tribonacci :: Int -> Int
    tribonacci n | n <= 2
                 | otherwise = tribonacci(n-1) + tribonacci(n-2) + tribonacci(n-3) 

    --2
    esMultiploDe3 :: Int -> Bool
    esMultiploDe3 n | n == 0 = True
                    | n < 3 = False
                    | otherwise = esMultiploDe3 (n-3)
    --3
    diabolico :: Int -> Bool
    diabolico 6 = True
    diabolico n | n `mod` 10 == 6 = diabolico (n `div` 10)
                | otherwise = False

    --4
    digitosIguales :: Int -> Bool
    digitosIguales n | n < 10 = True
                     | (digitoUnidades n) == (digitoDecenas n) = digitosIguales (n `div` 10)
                     | n < 0 = False

    --5
    --a
    resta :: Int -> Int -> Int
    resta n 0 = n
    resta n m | m > n = 0
              | resta (pred n) (pred m)
    
    --b (con recursion)
    menor :: Int -> Int -> Bool
    menor 0 m = True
    menor n m | n == m = False
              | menor (pred n) (pred m)
    
    --b (sin recursion)
    menor2 :: Int -> Int -> Bool
    menor2 n m | n == m = False
               | resta n m == 0 = True

    --c (sin recursion)
    mayor :: Int -> Int -> Bool
    mayor n m | n == m = False
              | resta n m == 0 = False
              | otherwise = True

    --d
    iguales :: Int -> Int -> Bool
    iguales 0 0 = True
    iguales 0 m = False
    iguales n 0 = False
    iguales n m | iguales (pred n) (pred m)
                | otherwise = False
    
    --6 (en mas de una oracion)
    esPotenciaDe :: Int -> Int -> Bool
    esPotenciaDe n 0 = undefined
    esPotenciaDe 1 m = True
    esPotenciaDe n m | mod n m > 0 = False
                     | esPotenciaDe (div n m) m

    -- esPotenciaDe 8 2 : si, porque 8 = 2.2.2
    -- pongo n m asi se puede escribir, 8 'esPotenciaDe' 2