--1
{-n va bajando de valor hasta que, al ser menor que i, da 0 y termina 
la recursion (i^(i-1) = 0 y no se contabiliza)-}
g1 :: Int -> Int -> Int
g1 i n | n < i = 0
       | otherwise = g1 i (n-1) + i^n

--2
{-Creo una funcion aux que me permita sumar todas las columnas de la 
recursion g3 original-}
aux :: Int -> Int -> Int
aux _ 0 = 0
aux n q = q^n + aux n (q-1)

{-g2 me tiene que sumar todos los nros de 1 a n con potencia no menor 
a ellos. Por ej: g2 3: 1^1 + 1^2 + 1^3 + 2^2 + 2^3 + 3^3-}
g2:: Int -> Int
g2 0 = 0
g2 n = g2 (n-1) + aux n n

--3
{-Adjunto funcion auxiliar esPar que me permite saber si n es par o no-}
esPar :: Int -> Bool
esPar 0 = True
esPar 1 = False
esPar n = esPar (n-2)
{-g3 tiene que sumar 2^n tal que n y sus terminos anteriores son pares.
Por ejemplo, g3 8: 2^2 + 2^4 + 2^6 + 2^8
Otro ejemplo, g3 7: 2^2 + 2^4 + 2^6-}
g3 :: Int -> Int
g3 0 = 0
g3 n | esPar n == True = g3 (n-2) + 2^n
     | otherwise = g3 (n-1)

--4
{-g4 tiene que devolver la suma de todos los numeros naturales menores o 
iguales a n que tengan todos los digitos iguales
Por ejemplo, g4 97: 11 + 22 + 33 + 44 + ... + 88
Otro ejemplo, g4 100: 22: 11 + 22 -}

digitoUnidades :: Int -> Int
digitoUnidades n | n <= 0 = undefined
                 | otherwise = mod n 10

digitoDecenas :: Int -> Int
digitoDecenas n | n <= 10 = undefined
                | otherwise = div (mod n 100) 10

digitosiguales2 :: Int -> Bool
digitosiguales2 n = n < 10 || ( (digitoUnidades n == digitoDecenas n && digitosiguales2 (div n 10))

g4 :: Int -> Int
g4 0 = 0
g4 n | digitosIguales2 n == True = n + g4 (n-1)
     | otherwise = g4 (n-1)

