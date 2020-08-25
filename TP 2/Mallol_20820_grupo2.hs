import Data.Char (ord, chr)

{-Segundo TP del Taller de Programacion de Algebra. Tanto las funciones auxiliares como las pedidas en
el enunciado del TP estan explicadas mediante comentarios.
Hecho por Martin Federico Mallol.-}

--1
{-Enunciado:
Dados dos numeros primos p y q, genera un par que contiene una clave publica y una clave privada en
el formato ((n, d), (n, e)).-}


{-Pto 1: Esta funcion toma dos primos ("p" y "q") y primero verifica que su producto ("n")
sea mayor a 127. 
Si lo es, genera la clave publica (n,d) y la clave privada (n,e). Representando
a "e" y a "d" como el resultado de llamar a las funciones auxiliares "eleccionDeEDesde" y 
"elijoDDesde" respectivamente (con "m" == (p-1)*(q-1)).
Si no lo es, y "p" es menor a "q", vuelve a llamar a la funcion con p = el primo que le sigue
Y viceversa.-}
generarClaves :: Integer -> Integer -> ((Integer, Integer), (Integer, Integer))
generarClaves p q | n <= 127 && p >= q = generarClaves p (minimoPrimoDesde (q+1))
                  | n <= 127 && p < q  = generarClaves (minimoPrimoDesde (p+1)) q
                  | otherwise          = ( (n, d) , (n, e) )
                               where n = p*q
                                     m = (p-1)*(q-1)
                                     e = eleccionDeEDesde 2 m
                                     d = elijoDDesde 2 e m
{- Senti que la mejor opcion fue declarar varias variables con el "where" asi podia tener
dos lineas prolijas de codigo para la funcion. Pero nose que tan limpio o eficiente es tener 
4 variables para esta f. Me quedo esa duda. Tal vez hubiese sido mejor usar mas fs auxiliares.-}

{-Dado un "e" y un "m" (tal que m == (p-1)*(q-1) ) elige un "e" posible entre 2 y "m" partiendo desde
el numero asignado a la variable "e". Como para la funcion generarClaves quiero utilizar el 
menor "e" posible (nro coprimo con "m"), llamaré a esta funcion "eleccionDeEDesde 2 m".-}
eleccionDeEDesde :: Integer -> Integer -> Integer 
eleccionDeEDesde e m | e < 2 = eleccionDeEDesde (e+1) m
                     | e > (m-2) = eleccionDeEDesde (e-1) m
                     | mcd e m == 1 = e
                     | otherwise = eleccionDeEDesde (e+1) m

{-Esta f busca un "d" tal que d*e "congruente" 1 (m). Puse como condicion que "d" no sea igual a "e", 
porque supongo que no tendria mucho sentido tener una clave publica y privada iguales.-}
elijoDDesde :: Integer -> Integer -> Integer -> Integer
elijoDDesde d e m | (d*e) `mod` m == 1 && d /= e = d
                  | otherwise                  = elijoDDesde (d+1) e m

----------------------------------------------------------------------------------------------------
------FUNCIONES AUXILIARES UTILIZADAS EN CLASES Y TAREAS ANTERIORES QUE ME SIRVIERON PARA ESTE PUNTO
mcd :: Integer -> Integer -> Integer
mcd a 0 = a
mcd a b | b > a = mcd b a 
        | otherwise = mcd b (a `mod` b)

esPrimo :: Integer -> Bool
esPrimo 1 = False
esPrimo n = n == menorDivisor n

menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorDesde n 2

menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n k | n `mod` k == 0 = k
                      | otherwise      = menorDivisorDesde n (k + 1)

nEsimoPrimo :: Integer -> Integer
nEsimoPrimo 1 = 2
nEsimoPrimo n = minimoPrimoDesde (1 + nEsimoPrimo (n - 1))

minimoPrimoDesde :: Integer -> Integer
minimoPrimoDesde n | esPrimo n = n
                   | otherwise = minimoPrimoDesde (n + 1)
------FIN DE LAS FUNCIONES AUXILIARES
----------------------------------------------------------------------------------------------------

--2
{-Enunciado:
Dada una clave publica y un mensaje, lo reemplaza por la lista de enteros que lo encripta.-}

{-Pto 2: En esta funcion toma un string y primero le asigna su numero entero segun "ord" y luego calcula,
el resto de este numero elevado a la d, al ser dividido por n. Y asi recursivamente recorre cada 
char de la lista convirtiendolo por aquella cuenta, hasta quedarme sin letras. Cuando eso sucede,
la funcion le asigna el valor "[]" Y por lo tanto todos los enteros antes calculados, se adjuntan
de manera recursiva a la lista vacia.-}
encriptar :: (Integer, Integer) -> String -> [Integer]
encriptar (_,_) ""      = []
encriptar (n,d) (s:str) = resto : encriptar (n,d) str
            where resto = ((fromIntegral (ord s))^d) `mod` n

--3
{-Enunciado:
Dada una clave privada y una lista de enteros que encripta un mensaje, lo desencripta.-}

{-Pto 3: La f agarra la lista encriptada y mediante la funcion "listaDeNumerosDeBaA" transforma dicha lista,
utilizando el proceso matematico que se explica en dicha funcion. Luego, con cada elemento de la lista 
original tranformado de "b" a "a", procede a convertir cada numero en caracter, y asi desencriptar el
mensaje.-}
desencriptar :: (Integer, Integer) -> [Integer] -> String
desencriptar (_,_) []     = []
desencriptar (n,e) (x:xs) = desencripto : desencriptar (n,e) xs
        where desencripto = chr (fromInteger y)
              (y:ys)      = listaDeNumerosDeBaA (n,e) (x:xs)

{-Dada su clave privada y la lista encriptada recibida, Bob reemplaza los "nros b" de la lista recibida
por "a". Donde a == resto de (b^e)/n -}
listaDeNumerosDeBaA :: (Integer, Integer) -> [Integer] -> [Integer]
listaDeNumerosDeBaA (_,_) []     = []
listaDeNumerosDeBaA (n,e) (x:xs) = resto : listaDeNumerosDeBaA (n,e) xs 
                     where resto = (x^e) `mod` n 

--4 (OPCIONAL)
{-Enunciado: Romper el codigo asociado a la clave publica (100337; 60953), desencriptar la pregunta.-}

{-Pto 4: Dada una clave publica y su mensaje encriptado, esta funcion lo desencripta descubriendo cual es su clave
privada sin tenerla de antemano. Utiliza las funciones escritas abajo.-}
desencriptarRompiendo :: (Integer, Integer) -> [Integer] -> String
desencriptarRompiendo (n,d) (ms:msg)      = desencriptar (n, elijoEMedianteD 1 d m) (ms:msg)
                         where (x:xs:xss) = romperNClavePublica (n,d) 1
                               m          = (x-1)*(xs-1)

{-Itera de primo en primo hasta encontrar uno que divida a n. Cuando lo hace, llamo devuelta a la f
pero dividiendo a n por ese primo encontrado, que es el resultado del segundo primo faltante. El caso
base recursivo es cuando n vale 1. Y asi me termina dando una lista que contiene a los "p" y "q" originales-}
romperNClavePublica :: (Integer, Integer) -> Integer -> [Integer]
romperNClavePublica (1,d) _                    = [] 
romperNClavePublica (n,d) po | n `mod` p == 0  = p : romperNClavePublica (div n p,d) (queNEsimoPrimoEsDesde 1 (div n p))
                             | p > n           = romperNClavePublica (n,d) 1
                             | otherwise       = romperNClavePublica (n,d) (po+1)
                                       where p = nEsimoPrimo po

{-Funcion auxiliar que me dice que nesimo primo es "p" desde un nro "k" inicial-}
queNEsimoPrimoEsDesde :: Integer -> Integer -> Integer
queNEsimoPrimoEsDesde k p | k == p            = 0 
                          | not (esPrimo p)   = undefined
                          | esPrimo k || k==1 = 1 + queNEsimoPrimoEsDesde (k+1) p
                          | otherwise         = queNEsimoPrimoEsDesde (k+1) p
                   

{-Dado un "d" y "m" ya dados, y una "e" como incognita, busca esa "e" tal que d*e "congruente" 1 (m). -}
elijoEMedianteD :: Integer -> Integer -> Integer -> Integer
elijoEMedianteD e d m | (d*e) `mod` m == 1 = e
                      | otherwise          = elijoEMedianteD (e+1) d m


{-Pregunta encriptada:
[33706,38913,58255,99961,77756,23881,220,77756,1606,38913,77756,78982,18800,91658,91658,58255,77756,96593,58255,
438,22839,28700,18800,1606,58255,48389]
-}

{-Pregunta descencriptada (Clave Publica: (100337,60953) | Clave Privada: (100337,1001)): 
"Cual es tu pizza favorita?"
-}

{-Respuesta encriptada : 
[78387,58255,77756,78982,18800,91658,91658,58255,77756,85626,23881,77756,22329,58255,50740,22839,96986,77756,
61099,77756,50740,22839,28700,28700,22839,96986,23881,220]
-}