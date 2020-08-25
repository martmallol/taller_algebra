module Tarea10
where
import Clase10

--1
{-Dado un sistema general, decide si cada una de sus ecuaciones, 
vista independientemente de las otras, tiene solucion.-}
cadaEcTieneSoluc :: [(Int, Int, Int)] -> Bool
cadaEcTieneSoluc [] = True
cadaEcTieneSoluc (e:es) = ( (solucionEc e) /= undefined ) && ( (cadaEcTieneSoluc es) /= undefined )

--2
{-Dado un sistema simplicado, decide si tiene solucion.-}
tieneSolucionSimplif :: [(Int, Int)] -> Bool
tieneSolucionSimplif sist = solucSistemaSimplif /= undefined

{-f auxiliar que me da la solucion a un sistema simplificado.-}
solucSistemaSimplif :: [(Int, Int)] -> (Int, Int)
solucSistemaSimplif sist = solucSistemaModCoprimos ( sistemaEquivSinPrimosMalos sist )

--3
{-Dado un sistema general, decide si tiene solucion.-}
tieneSolucion :: [(Int, Int, Int)] -> Bool
tieneSolucion sist = ( (solucSistema sist) /= undefined ) 

--4
{-Dados dos numeros coprimos r y m con 1 <= r < m, encuentra 
un numero primo en la clase de congruencia X  r (mod m).-}
dirichlet :: Int -> Int -> Int
drichlet r m | r > m           = drichlet m r
             | not (sonCoprimos r m) = undefined
             | otherwise = solucPrimoClaseCongruenciaDesde (nEsimoPrimo 1) m r

solucPrimoClaseCongruenciaDesde :: Int -> Int -> Int -> Int
solucPrimoClaseCongruenciaDesde (nEsimoPrimo 100) m r = undefined
solucPrimoClaseCongruenciaDesde p m r | (nEsimoPrimo p) `mod` m == r = p
                                      | otherwise = solucPrimoClaseCongruenciaDesde (nEsimoPrimo (p+1)) m r


--solucEcSimplif = solucionEc (1, r, m)

-- Funciones auxiliares del tp
sumaDivisoresEnComunDesde :: Int -> Int -> Int -> Int
sumaDivisoresEnComunDesde n m k | (k > n) || (k > m) = 0 
                                | ((n `mod` k) == 0) && ((m `mod` k) == 0) = 1 + sumaDivisoresEnComunDesde n m (k+1)
                                | otherwise = sumaDivisoresEnComunDesde n m (k+1)

sonCoprimos :: Int -> Int -> Bool
sonCoprimos n m | sumaDivisoresEnComunDesde n m 1 == 1 = True
                | otherwise = False
