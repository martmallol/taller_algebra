{-TP del Taller de Programacion de Algebra. Tanto las funciones auxiliares como las pedidas en el enunciado del TP estan 
explicadas mediante comentarios.
Hecho por Martin Federico Mallol.-}

--1
{-Aux: Esta funcion me dice cuantos divisores en comun desde "k" tienen "n" y "m". Dado un nro "k" inicial (Natural), la f 
itera preguntando si ese mismo "k" es divisor tanto de "n" como de "m". Si lo es, se le suma 1 al resultado de la f. 
Si no lo es, no se suma nada. La f va iterando de "k" en "k" (se le suma 1 en cada caso), hasta que "k" sea mayor 
al menor entre "n" y "m".-}
sumaDivisoresEnComunDesde :: Integer -> Integer -> Integer -> Integer
sumaDivisoresEnComunDesde n m k | (k > n) || (k > m) = 0 
                                | ((n `mod` k) == 0) && ((m `mod` k) == 0) = 1 + sumaDivisoresEnComunDesde n m (k+1)
                                | otherwise = sumaDivisoresEnComunDesde n m (k+1)

{-Pto 1: Para que "n" y "m" sean coprimos, solo tienen que tener un divisor en comun, el 1. Por eso, al usar
sumaDivisoresEnComundDesde (con k=1), "n" y "m" solo tienen que tener 1 solo divisor. Por lo tanto, esta f
me tiene que dar como resultado 1. Si asi sucede, sonCoprimos es Verdadera. Si no, es Falsa.-}
sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos n m | sumaDivisoresEnComunDesde n m 1 == 1 = True
                | otherwise = False

--2
{-Funcion auxiliar ya utilizada en una clase/tarea previa.-}
menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n k | n `mod` k == 0 = k
                      | otherwise = menorDivisorDesde n (k + 1)

{-Funcion auxiliar ya utilizada en una clase/tarea previa.-}
menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorDesde n 2

{-Funcion auxiliar ya utilizada en una clase/tarea previa.-}
esPrimo :: Integer -> Bool
esPrimo 1 = False
esPrimo n = n == menorDivisor n

{-Pto 2: Esta f me dice si un nro natural compuesto "n" (n > 1 y no-primo), es 2PseudoPrimo. Para que esto suceda
(2^(n-1) - 1) `mod` n == 0 -}
es2Pseudoprimo :: Integer -> Bool
es2Pseudoprimo 1 = False
es2Pseudoprimo n | esPrimo n = False
                 | (2^(n-1) - 1) `mod` n == 0 = True
                 | otherwise = False

--3
{-Aux: Esta f me dice si un nro natural compuesto "n" (n > 1 y no-primo), es 3PseudoPrimo. Para que esto suceda
(3^(n-1) - 1) `mod` n == 0-}
es3Pseudoprimo :: Integer -> Bool
es3Pseudoprimo 1 = False
es3Pseudoprimo n | esPrimo n = False
                 | (3^(n-1) - 1) `mod` n == 0 = True
                 | otherwise = False

{-Aux: Suma la cantidad de 3PseudoPrimos desde "i" hasta "m" inclusive. Si i es 3PseudoPrimo, suma 1 al resultado total y pregunta
al siguiente "i", osea i+1. Y la funcion sigue preguntando hasta que "i" sea mayor a "m". En ese caso, no se suma nada. -}
suma3PseudoprimosDesde :: Integer -> Integer -> Integer
suma3PseudoprimosDesde i m | i > m = 0
                           | es3Pseudoprimo i = 1 + suma3PseudoprimosDesde (i+1) m
                           | otherwise = suma3PseudoprimosDesde (i+1) m

{-Pto 3: La cantidad de 3-pseudo primos desde 1 hasta "m" inclusive. Para averiguarlo uso la funcion creada anteriormente,
tomando i = 1 (la funcion evalua desde 1 hasta "m").-}
cantidad3Pseudoprimos :: Integer -> Integer
cantidad3Pseudoprimos m = suma3PseudoprimosDesde 1 m

--4
{-Aux: Pregunta si "k" es 2PseudoPrimo Y 3PseudoPrimo.-}
es2y3Pseudoprimo :: Integer -> Bool
es2y3Pseudoprimo k | es2Pseudoprimo k && es3Pseudoprimo k == True = True
                   | otherwise = False

{-Aux: Quiere saber el minimo 2y3Pseudoprimo desde "k". Si "k" lo es, entonces la f = k. Si "k" no lo es, la f
evalua en k + 1 constantemente hasta encontrar el primer 2y3Pseudoprimo mayor a mi "k" inicial. -}
minimo2y3PseudoprimoDesde :: Integer -> Integer
minimo2y3PseudoprimoDesde k | es2y3Pseudoprimo k = k
                            | otherwise = minimo2y3PseudoprimoDesde (k + 1)

{-Pto 4: Esta funcion me dice el k-esimo 2y3PseudoPrimo. Si k = 1, entonces el primer 2y3PseudoPrimo es 1105 
(como se mostraba en el PDF del TP). Por lo tanto, si k > 1, la f busca al minimo 2y3PseudoPrimo recursivamente 
preguntando por kesimo2y3PseudoPrimo (k-1) y cada vez que lo hace suma 1 al resultado parcial. Y asi, hasta 
llegar hasta minimo2y3PseudoprimoDesde (1 + kesimo2y3Pseudoprimo 1 ), sabiendo que kesimo2y3Pseudoprimo 1 = 1105-}
kesimo2y3Pseudoprimo :: Integer -> Integer
kesimo2y3Pseudoprimo 1 = 1105 
kesimo2y3Pseudoprimo k = minimo2y3PseudoprimoDesde (1 + kesimo2y3Pseudoprimo (k - 1))

--5
{-Aux: Esta f me dice si un nro natural compuesto "n" (n > 1 y no-primo) es aPseudoPrimo respecto a "a".-}
esaPseudoprimo :: Integer -> Integer -> Bool
esaPseudoprimo _ 1 = False
esaPseudoprimo a n | esPrimo n = False
                   | (a^(n-1) - 1) `mod` n == 0 = True
                   | otherwise = False

{-Me dice si n es a-pseudoprimo para todo "a" entre 1 y "a" que sea coprimo con "n". Si "a" y "n" son coprimos Y "n" es a-pseudoprimo con "a", 
entonces la f sigue preguntando hasta llegar a a = 1 (en el caso de que efectivamente siempre que sonprimos, "n" es a-pseudoprimo con "a"). 
Si "a" y "n" son coprimos pero "n" no es apseudoprimo con "a", entonces el resultado de la f automaticamente es Falso. Si "a" y "n" no son 
coprimos, la f sigue preguntando recursivamente.-}
aPseudoprimoyCoPrimoHasta :: Integer -> Integer -> Bool
aPseudoprimoyCoPrimoHasta 1 n = sonCoprimos 1 n && esaPseudoprimo 1 n
aPseudoprimoyCoPrimoHasta a n | sonCoprimos a n && esaPseudoprimo a n = aPseudoprimoyCoPrimoHasta (a-1) n 
                              | (sonCoprimos a n == True && esaPseudoprimo a n /= True) = False
                              | otherwise = aPseudoprimoyCoPrimoHasta (a-1) n

{-Pto 5: Como los nros de Carmichael son los "n" que son a-pseudoprimos para todo 
"a" entre 1 y (n - 1) que sea coprimo con n. Entonces esCarmichael es igual a la f anterior evaluando a "a" como "(n - 1)".-}
esCarmichael :: Integer -> Bool
esCarmichael n = aPseudoprimoyCoPrimoHasta (n-1) n