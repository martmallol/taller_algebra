module Tarea9
where
import Tarea6
type Set a = [a]
--1
{-Dados n >= 0 y b > 1, retorne su representacion por listas en base b.-}
digitos :: Int -> Int -> [Int]
digitos n b = reverso (digitosAux n b)

digitosAux :: Int -> Int -> [Int]
digitosAux 0 b  = []
digitosAux n b  = n `mod` b : (digitosAux (div n b) b)



--2
{-Dada la representacion por listas de n >= 0 en base b y la base b > 1, retorne n-}
numero :: [Int] -> Int -> Int
numero [] b = 0
numero (n:ns) b = ( n * b^( length (n:ns) - 1) ) + numero ns b

--3
{-Dado un valor n != 0 retorna el conjunto de sus divisores positivos-}
--ARREGLAR DIVISORES, QUE QUEDE PROLIJO
divisores :: Int -> [Int]
divisores n = divisoresAux n n

divisoresAux :: Int -> Int -> [Int]
divisoresAux n 1 = [1] 
divisoresAux n d | n `mod` d == 0 = d : (divisoresAux n (d-1))
                 | otherwise = divisoresAux n (d-1)


{-nPrimosDivisoresDesde :: Int -> Int -> Set Int
nPrimosDivisoresDesde n 1 = [1]
nPrimosDivisoresDesde n p | nEsimoPrimo p > n = [1]  
                          | n `mod` nEsimoPrimo p == 0 = p : (nPrimosDivisoresDesde (dividido n d) (nEsimoPrimo (p+1)))
                          | otherwise = nPrimosDivisoresDesde n (p+1)
                where d = ((nEsimoPrimo p)^(quePotenciaLoDivide n (nEsimoPrimo p)))-}




--4
{-(a : b) es el maximo comun divisor (mcd) de a y b-}
mcdDef :: Int -> Int -> Int
mcdDef a b = maximo ( interseccion (divisores a) (divisores b) )

interseccion :: [Int] -> [Int] -> [Int]
interseccion a b = reverso (interseccionAux a b [])  

interseccionAux :: [Int] -> [Int] -> [Int] -> [Int]
interseccionAux _ [] cs                                      = cs
interseccionAux xs (y:ys) cs | cuantasVecesAparece y xs == 1 = (interseccionAux xs ys cs) ++ (y : cs)
                             | otherwise                     = interseccionAux xs ys cs


--5
{-Medir el tiempo que tarda mcdDef para un par de valores en 10^10 <= a, b <= 2.10^10-}
-- + de 10 minutos
--6
{-Dados a,b en Z, b != 0, calcule (a : b) usando el algoritmo de Euclides.-}
mcd :: Int -> Int -> Int
mcd a 0 = a
mcd a b | b > a = mcd b a 
        | otherwise = mcd b (a `mod` b) 

--7
{-Medir el tiempo de esta funcion y compararlo con mcdDef-}
--0.01 segund0s
--8
{-dados a >= 0 y b >= 0 calcule el minimo d >= 0 que sea multiplo tanto de a como de b.
Cuanto vale mcm 0 0 ?-}
mcm :: Int -> Int -> Int
mcm a b = minimo ( interseccion (divisores a) (divisores b) )

--9
{-Dados a y b, utilice el algoritmo de Euclides extendido para obtener una 
tripla ((a : b),s,t) tal que sa + tb = (a : b)-}
{-emcdst :: Int -> Int -> Int -> Int -> (Int, Int, Int)
emcdst a b s t | a < 0 || b < 0 = emcd (abs a) (abs b) s t
               | a < b = emcd b a s t
               | otherwise = emcd b (a `mod` b) s2 -}

--NO SE POR QUE EL WHERE NO FUNCIONA
{-
emcd :: Int -> Int -> (Int , Int , Int)
emcd _ 0 = (a,1,0)
emcd a b | (a < 0 || b < 0) = emcd (abs a) (abs b)
           | a < b          = emcd b a
           | otherwise      = (d, t, s - t * k)
where (d,s,t)             = emcd b (mod a b)
      (k, r) = (div a b, mod a b)-}

mcdExt :: Int -> Int -> (Int , Int , Int)
mcdExt a 0 = (abs a, signum a, 0)
mcdExt a b = (d, t, s - t * k)
       where (k, r) = (div a b, mod a b)
             (d, s, t) = mcdExt b r

emcd :: Int -> Int -> (Int , Int , Int)
emcd a b = mcdExt a b

--10
{-{-Definir una funcion que dados a != 0 y b != 0 encuentre el par s,t en Z tal que
sa + tb = (a : b) donde s >= 0 sea lo minimo posible. 
Repasar la teorica para este ejercicio.-}-}

---Aca pongo las funciones auxiliares escritas por el profe por si acaso
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
                   | otherwise = minimoPrimoDesde (n + 1)

quePotenciaLoDivide :: Int -> Int -> Int
quePotenciaLoDivide n p | n `mod` p == 0  = 1 + quePotenciaLoDivide (n `div` p) p
                        | otherwise = 0

{--- Division de numeros naturales_0 : a `divNat ` d = a `div ` d
divNat :: Int -> Int -> Int
divNat a d | a < d = 0
           | otherwise = (a-d) `divNat ` d + 1

-- Resto de numeros naturales_0 : a `modNat ` d = a `mod ` d
modNat :: Int -> Int -> Int
modNat a d = a - d*(a `divNat ` d)

-- Division de numeros enteros : n `dividido ` m = n `div ` m
dividido :: Int -> Int -> Int
dividido a d = sgq * absq --obs 1
   where absq = abs (a-r) `divNat ` ( abs d) --obs 2
         sgq = ( signum a) * ( signum d) --obs 3
           r = a `modulo ` d-}