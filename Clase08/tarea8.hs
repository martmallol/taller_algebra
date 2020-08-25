module Tarea8
where
    import Clase8
    import Tarea6
    -------------------------------------------------------------------------
    -----------------------FUNCIONES ANTERIORES------------------------------
    perteneceC :: Set Int -> Set (Set Int) -> Bool
    perteneceC xs []       = False
    perteneceC xs (ys:yss) = iguales xs ys || perteneceC xs yss
    
    pertenece :: Int -> [Int] -> Bool
    pertenece _ [] = False
    pertenece x (y:ys) | x == y    = True
                       | otherwise = pertenece x ys
    incluido :: Set Int -> Set Int -> Bool
    incluido [] c     = True
    incluido (x:xs) c = pertenece x c && incluido xs c

    iguales :: Set Int -> Set Int -> Bool
    iguales c1 c2 = incluido c1 c2 && incluido c2 c1
    ------------------------------------------------------------------------
    ------------------------------------------------------------------------
    
    --1
    listade1hastaN :: Int -> Set Int
    listade1hastaN 1 = [1]
    listade1hastaN n = ordenar (agregar n (listade1hastaN (n-1)))

    bolitasEnCajas :: Int -> Int -> Set [Int]
    bolitasEnCajas n k = variaciones (listade1hastaN k) n
    
    --2
    --Reconoce si el 1 (la primera caja) corresponde a alguna posición i de una lista.
    recorrerLista :: Set Int -> Bool
    recorrerLista [] = False
    recorrerLista (x:xs) | x == 1 = True
                         | otherwise = recorrerLista xs
    

    --Recorre todas las listas de un conjunto y descarta aquellas en que el 1 no está.
    recorrerCadaLista :: Set [Int] -> Set [Int]
    recorrerCadaLista [] = []
    recorrerCadaLista (xs:xss) | recorrerLista xs = xs : recorrerCadaLista xss
                               | otherwise = recorrerCadaLista xss
    

    bolitasEnCajas' :: Int -> Int -> Set [Int]
    bolitasEnCajas' n k = recorrerCadaLista (variaciones [1..k] n)

    --3
    {-n: la cantidad de nros disponibles desde 1
      k: la cantidad de nros en un conjunto
      Ej: listasOrdenadas 5 2
          [[1,2],[1,3],[1,4],[1,5],[2,3],[2,4],[2,5],[3,4],[3,5],[4,5]]
          listasOrdenadas 3 4 
          [[1,2,3], [1,2,4], [1,3,4], [2,3,4]]-}   
    listasOrdenadas :: Int -> Int -> Set(Set Int)
    listasOrdenadas n k = eliminarCjtosRepetidos (permutacionesLongitud n k)

    {-Calcula las permutaciones de [1..n] pero cada subconjunto tiene longitud k.-}
    permutacionesLongitud :: Int -> Int -> Set [Int] 
    permutacionesLongitud n k = ordenarC ( moldearLongitud (permutaciones (listade1hastaN n)) k )

    {-Ordena los cjtos [] de una lista de [[],[],[]] de forma creciente-}
    ordenarC :: Set [Int] -> Set [Int]
    ordenarC [] = []
    ordenarC (xs:xss) = (ordenar xs) : (ordenarC xss) 
    
    {-Dado [[] [] []] elimina los [] que se repitan en la lista  -}
    eliminarCjtosRepetidos :: Set [Int] -> Set [Int]
    eliminarCjtosRepetidos [] = []
    eliminarCjtosRepetidos (xs:xss)  | cuantasVecesApareceC xs (xs:xss) == 1 =  xs: eliminarCjtosRepetidos xss
                                     | otherwise = eliminarCjtosRepetidos xss

    {-Me dice cuantas veces aparece un conjunto [] en una lista [[] [] []]-}
    cuantasVecesApareceC :: [Int] -> Set [Int] -> Int
    cuantasVecesApareceC cs [] = 0
    cuantasVecesApareceC cs (xs:xss) | iguales xs cs = 1 + cuantasVecesApareceC cs xss
                                     | otherwise     = cuantasVecesApareceC cs xss

    
    {- Me devuelve el conjunto [[xs],[xs']..[xs^n]] con cada subconjunto de longitud k-}
    moldearLongitud :: Set [Int] -> Int -> Set [Int]
    moldearLongitud [] k = []
    moldearLongitud (xs:xss) k | length xs > k = moldearLongitud ((quitarUltimo (ultimo xs) xs):xss) k 
                               | otherwise = xs : (moldearLongitud xss k)

    --4
    {-sucesionesAB 3 2
    ["aaabb","aabba","aabab","abbaa","ababa","abaab","bbaaa","babaa","baaba","baaab"]-}   
    sucesionesAB :: Int -> Int -> Set String
    sucesionesAB 0 0 = [""]
    sucesionesAB x y | x >= y = insertarEnCadaPosDeTodasLasListasS (sucesionesAB (x-1) y) 'a'
                     | otherwise = insertarEnCadaPosDeTodasLasListasS (sucesionesAB x (y-1)) 'b'
    
    insertarEnCadaPosDeTodasLasListasS :: Set String -> Char -> Set String 
    insertarEnCadaPosDeTodasLasListasS [] c       = []
    insertarEnCadaPosDeTodasLasListasS (xs:xss) c = (insertarEnCadaPosS xs c (length xs + 1)) `union`
                                                (insertarEnCadaPosDeTodasLasListasS xss c)

    {-Inserta una letra c en cada posicion de un string "aaa" ---> ["aaab", "aaba", "abaa", "baaa"]-}
    insertarEnCadaPosS :: String -> Char -> Int -> Set String
    insertarEnCadaPosS xs c 1 = agregar (insertarEnS xs c 1) vacio
    insertarEnCadaPosS xs c i = agregar (insertarEnS xs c i) (insertarEnCadaPosS xs c (i-1)) 
    
    {-Insterta una letra n en un string "aaaa" dada una posicion i-}
    insertarEnS :: String -> Char -> Int -> String
    insertarEnS xs n i | i == 1    = n:xs 
                       | otherwise = (head xs) : (insertarEnS (tail xs) n (i-1))

    --5
    sucesiones3 :: Int -> Int -> Int -> Set String
    sucesiones3 0 0 0 = [""]
    sucesiones3 x y z | x >= y && x >= z = insertarEnCadaPosDeTodasLasListasS (sucesiones3 (x-1) y z) 'a'
                      | y >= z           = insertarEnCadaPosDeTodasLasListasS (sucesiones3 x (y-1) z) 'b'
                      | otherwise        = insertarEnCadaPosDeTodasLasListasS (sucesiones3 x y (z-1)) 'c' 

    --6
    {-Dados un conjunto de enteros y un entero k, genera todos los subconjuntos de k elementos del 
    conjunto pasado por parametro.
    Ej: subjconjuntos [1,2,3] 2
    [[1, 2], [2, 3], [1, 3]]-}
    subconjuntos :: Set Int -> Int -> Set (Set Int)
    subconjuntos (x:xs) k = eliminarCjtosRepetidos (ordenarC ( moldearLongitud (permutaciones (x:xs)) k ) )
