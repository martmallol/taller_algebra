{-TODAS LAS F AUXILIARES QUE ME OCUPAN ESPACIO Y NO LAS ESCRIBI YO LAS IMPORTO
DE OTRO ARCHIVO QUE TENGO QUE CREAR LLAMADO CLASEO5-}

{-Ej del video de la clase para tenerlo como ejemplo-}
menorFactDesde :: Int -> Int
menorFactDesde m = menorFactDesdeDesde 1 m

menorFactDesdeDesde :: Int -> Int -> Int
menorFactDesdeDesde i m | (fact i) >= m = fact i
                        | otherwise     = menorFactDesdeDesde (i + 1) m

{-7-}
{-Aca tengo que crear una f que dado un nro m, me entregue un k! que sea menor o igual a m-}
mayorFactHasta :: Int -> Int
mayorFactHasta m = mayorFactHastaDesde 1 m
{-Aca creo como f general a esta f que itera de factorial en factorial hasta que 
encuentra a un fact que sea mayor/igual a m. En ese caso, si es mayor a m, me devuelve
el fact actual - 1. Y si es igual a m, me devuelve el fact actual.-}
mayorFactHastaDesde :: Int -> Int -> Int
mayorFactHastaDesde i m | (fact i) > m  = fact (i - 1)
                        | (fact i) == m = fact i
                        | otherwise     = mayorFactHastaDesde (i + 1) m

{-8-}
{-Aca creo la f esFact n , que me dice si el n ingresado es el resultado de un factorial
X ej, esFact 1 = True, xq 0! = 1 y 1! = 1.
      esFact 6 = True, xq 3! = 6.-}
esFact :: Int -> Bool
esFact n = esFactDesde 0 n
{-Aca creo la f aux que toma dos parametros, i m, e itera por la funcion hasta encontrar
que (fact i) == n, si (fact i) > n entonces es falso, y si (fact i) < n, entonces sigue 
iterando. Esta f busca decirme si n es el resultado de un factorial o no.-}
esFactDesde :: Int -> Int -> Bool
esFactDesde i n | (fact i) == n = True
                | (fact i) > n = False
                | otherwise = esFactDesde (i+1) n

{-9-}
{-Creo una f que me dice si mi parametro n es un numero fibonacci-}
esFibonacci :: Int -> Bool
esFibonacci n = nroFibonacci 0 n

{-Tengo que crear una f aux que itere por los indices (i=0) de la sumatoria fibonacci
(osea que utilizo fibo en su interior) chequeando si el resultado de ese indice es 
igual a mi parametro n. Si es menor sigue iterando y si es mayor lo tomo como falso.-}
nroFibonacci :: Int -> Int -> Bool
nroFibonacci i n | (fibo i) == n = True
                 | (fibo i) > n = False
                 | otherwise = nroFibonacci (i + 1) n

{-La f fibo me da como parametro un n. Este n es el indice de la sumatoria fibonacci. 
Me devuelve el resultado de ese indice.-}
fibo i | i <= 1 = 1 
       | otherwise = fibo (i - 1) + fibo (i - 2)

{-10-}
{-Tengo que crear la f que dado un n decide si n = a la suma de los m primeros nros
primos.-}
esSumaInicialDePrimos :: Int -> Bool 
esSumaInicialDePrimos n = sumaPrimos 1 n  

{-Esta f es la sumatoria de primos. Si el indice m = 1, me da el primer primo.
Si m = 2 me da (elPrimerPrimo + elSegundoPrimo), y asi hasta el m que yo quiera.-}
sumaPrimosHasta :: Int -> Int
sumaPrimosHasta 0 = 0
sumaPrimosHasta m | nEsimoPrimo m + sumaPrimosHasta (m - 1) 

{-sumaPrimos es mi f aux gral. Me dice si mi nro n es, o no, una suma de nros primos.
Por eso dentro de esta f uso a sumaPrimosHasta (mi sumatoria de primos ya declarada).-}
sumaPrimos :: Int -> Int -> Bool
sumaPrimos m n | (sumaPrimosHasta m) == n = True 
               | (sumaPrimosHasta m) < n = sumaPrimosHasta (m + 1) n 
               | otherwise = False

---Aca pongo las funciones auxiliares escritas por el profe
esPrimo :: Int -> Bool
esPrimo 1 = False
esPrimo n = n == menorDivisor n

menorDivisor :: Int -> Int
menorDivisor n = menorDivisorDesde n 2

menorDivisorDesde :: Int -> Int -> Int
menorDivisorDesde n k | n `mod` k == 0 = k
                      | otherwise      = menorDivisorDesde n (k + 1)

nEsimoPrimo :: Int -> Int
nEsimoPrimo 1 = 2
nEsimoPrimo n = minimoPrimoDesde (1 + nEsimoPrimo (n - 1))

minimoPrimoDesde :: Int -> Int
minimoPrimoDesde n | esPrimo n = n
                   | otherwise = minimoprimoDesde (n + 1)

{-11-}
{-Dado n > 1, y m > n, esta f me devuelve algun z entre n y m tal que:
sumaDivisores(z) = max{sumaDivisores(i) | n <= i <= m}
Osea, me devuelve el nro que tiene la suma de sus divisores mas grande entre n y m -}
tomaValorMax :: Int -> Int -> Int
tomaValorMax n m | n > m = undefined
                 | sumaDivisoresMaxEntre n (n+1) m

{-En esta f gral se itera desde n hasta k (z vendria a ser un contador que parte desde n+1), buscando
la suma de divisores mayor entre n y k inclusives-}
sumaDivisoresMaxEntre :: Int -> Int -> Int -> Int
sumaDivisoresMaxEntre n z k | n > z = undefined
                            | n == k = n
                            | z > (k+1) = n  
                            | (sumaDivisoresMax n z == z) || (z <= k) = sumaDivisoresMaxEntre z (z+1) k 
                            | (sumaDivisoresMax n z == n) || (z <= k) = sumaDivisoresMaxEntre n (z+1) k 

{-Creo la f que me dice cual suma de divisores es mayor entre dos variables-}
sumaDivisoresMax :: Int -> Int -> Int
sumaDivisoresMax n k | (sumaDivisores n) >= (sumaDivisores k) = n
                     | otherwise = k

---Aux del profesor
sumaDivisores :: Int -> Int
sumaDivisores n = sumaDivisoresHasta n n

sumaDivisoresHasta :: Int -> Int -> Int
sumaDivisoresHasta n k | k == 1 = |
                       | n `mod` k == 0 = k + sumaDivisoresHasta n (k - 1)
                       | otherwise      = sumaDivisoresHasta n (k - 1)

maximo :: Int -> Int -> Int
maximo x y | x >= y = x
           | otherwise = y

{-12-}
{-Dado n > 1, y m > n, esta f me devuelve algun z entre n y m tal que:
sumaDivisores(z) = min{sumaDivisores(i) | n <= i <= m}
Osea, me devuelve el nro que tiene la suma de sus divisores mas chica entre n y m -}
tomaValorMin :: Int -> Int -> Int
tomaValorMin n m | n > m = undefined
                 | sumaDivisoresMinEntre n (m-1) m

{-En esta f gral se itera desde n hasta k (z vendria a ser un contador que parte desde n+1), buscando
la suma de divisores menor entre n y k inclusives-}
sumaDivisoresMinEntre :: Int -> Int -> Int -> Int
sumaDivisoresMinEntre n z k | k < z = undefined
                            | k == n = k
                            | z < (n-1) = k  
                            | (sumaDivisoresMin k z == z) || (z >= n) = sumaDivisoresMinEntre n (z-1) z 
                            | (sumaDivisoresMin k z == k) || (z >= n) = sumaDivisoresMinEntre n (z-1) k 

{-Creo la f que me dice cual suma de divisores es menor entre dos variables-}
sumaDivisoresMin :: Int -> Int -> Int
sumaDivisoresMin n k | (sumaDivisores n) >= (sumaDivisores k) = k
                     | otherwise = n

{-13-}
{-Dado un nro natural esta f me tiene que decir si este nro es la suma de dos primos-}

{-
--- Esta f esta hecha por mi
esSumaDeDosPrimos :: Int -> Bool
esSumaDeDosPrimos n | sumaDePrimosDesde n 1

{--Esta f tiene como nro fijo a n. Quiero saber si n es suma de primos. Entonces arranco restandole
el nro primo nro k. Si Al restar n - kNroPrimo no me da un nro primo, itero k++.
Si kNroPrimo > n, para la iteracion dandome a entender que n no es una suma de nros primos.
Pero, si el resultado de n - kNroPrimo es un nro primo, entonces la f me da True.-}
sumaDePrimosDesde :: Int -> Int -> Bool
sumaDePrimosDesde n k | n < 5 = False
                      | (n - nEsimoPrimo k) <= 0 = False 
                      | esPrimo (n - nEsimoPrimo k) == True = True
                      | otherwise = sumaDePrimosDesde n (k+1)
-}

---Esta f esta hecha por alguien en el foro

esSumaDeDosPrimos :: Int -> Bool
esSumaDeDosPrimos n = esSumaDosPrimosAux n n

esSumaDosPrimosAux :: Int -> Int -> Bool
esSumaDosPrimosAux i n   | i < 1 = False
                         | esPrimo i && esPrimo (n-i) = True
                         | otherwise = esSumaDosPrimosAux (i-1) n
{-14-}
{-Tengo que escribir una f que a partir de un n, pruebe que todos los nros pares hasta 
ese n son suma de dos primos-}
goldbach :: Int -> Bool
goldbach n | n == 4         = True
           | n < 4          = False
           | n `mod` 2 /= 0 = goldbach (n-1)
           | (esSumaDeDosPrimos n && goldbach (n-2)) = True  

{-15-}
{-Dado n, devuelve la cantidad de primos gemelos (a,b) y b <= n-}
primosGem :: Int -> Int
primosGem n = primosGemHasta 3 5 n 

{-Creo la f que me dice si un par de nros a b son primos gemelos-}
sonPrimosGemelos :: Int -> Int -> Bool
sonPrimosGemelos a b | (esPrimo a && esPrimo b) && b == a + 2
                     | otherwise = False

{-Calcula la cantidad de primos gemelos hasta n. -}
primosGemHasta :: Int -> Int -> Int -> Int
primosGemHasta a b n | b > n = 0
                     | sonPrimosGemelos a b && b <= n = 1 + primosGemHasta (a+1) (b+1) n
                     | sonPrimosGemelos a b == False = primosGemHasta (a+1) (b+1) n

{-16-}
{-Dado n, me devuelve el prox  par de primos gemelos (a,b) tal que a > n-}
proxPrimosGem :: Int -> (Int, Int)
proxPrimosGem n | maxPrimosGemDesde ((n+1), n+3) n+1

{-Esta f verifica que el par (a,b) sean primos gemelos y como minimo, a>= n. Entonces
me devuelve (a,b). Si ninguna de esas dos condiciones se cumplen, itera sumando 1 a cada 
miembro del par hasta encontrar el primer grupo de primos gem tal que a > n. -}
maxPrimosGemDesde :: (Int, Int) -> Int -> (Int, Int)
maxPrimosGemDesde (a,b) n | sonPrimosGemelos a b && a >= n = (a,b)
                          | otherwise                      = maxPrimosGemDesde (a+1,b+1) n 

{-17-}
collatz :: Int -> Int
collatz 1 = 1
collatz n | n `mod` 2 == 0 = collatz (n/2)
          | otherwise = collatz (3*n+1)
--a
largoSecuencia :: Int -> Int
largoSecuencia 0 = undefined
largoSecuencia n = reduccionesCollatz n 

reduccionesCollatz :: Int -> Int
reduccionesCollatz 1 = 1
reduccionesCollatz n | n `mod` 2 == 0 = 1 + reduccionesCollatz (n/2)
                     | otherwise = 1 + reduccionesCollatz (3*n+1)
--b