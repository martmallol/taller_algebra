module Tarea6
where
    {-HACER TODAS CON PATTERNMACHING TAMBIEN-}
    --1
    productoria :: [Int] -> Int
    productoria l | l == []   = 1
                  | otherwise = head l * productoria (tail l)

    --2
    {-Dado un nuumero N y una lista xs, suma N a cada elemento de xs.-}
    sumarN :: Int -> [Int] -> [Int]
    sumarN n xs | xs == []  = []
                | otherwise = (head xs + n) : sumarN n (tail xs) 

    --3
    {-Dada una lista no vacia xs, suma el primer elemento a cada elemento de xs. 
    Ejemplo sumarElPrimero [1,2,3] --> [2,3,4]-}
    sumarElPrimero :: [Int] -> [Int]
    sumarElPrimero xs | xs == []  = []
                      | otherwise = ((head xs)*2 ) : sumarN (head xs) (tail xs)

    --4
    {-Dada una lista no vacia xs, suma el ultimo elemento a cada elemento de xs. 
    Ejemplo sumarElUltimo [1,2,3] --> [4,5,6]-}
    sumarElUltimo :: [Int] -> [Int]
    sumarElUltimo xs | xs == []  = []
                    | otherwise = ((head xs) + ultimo xs ) : sumarN (ultimo xs) (tail xs)

    {-Auxiliar-}
    ultimo :: [Int] -> Int
    ultimo xs | xs == []        = 0
              | (tail xs) == [] = (head xs)
            -- (tail (tail xs)) == [] = head (tail xs) (al final no hacia falta poner esto)
              | otherwise       = ultimo (tail xs)

    --5
    {-Devuelve una lista con los elementos pares de la lista original. 
    Ejemplo pares [1,2,3,5,8] --> [2,8]-}
    pares :: [Int] -> [Int]
    pares xs | xs == []               = []
            | (head xs) `mod` 2 == 0 = (head xs) : (pares (tail xs)) 
            | otherwise              = pares (tail xs)

    --6
    {-Elimina la primera aparicion del elemento en la lista (de haberla).-}
    quitar :: Int -> [Int] -> [Int]
    quitar n xs | xs == []       = []
                | (head xs) == n = tail xs  
                | otherwise      = (head xs) : (quitar n (tail xs)) 
    --7
    {-Elimina todas las apariciones del elemento en la lista (de haberla).-}
    quitarTodas :: Int -> [Int] -> [Int]
    quitarTodas n xs | xs == []       = []
                    | (head xs) /= n = (head xs) : (quitarTodas n (tail xs)) 
                    | otherwise      = quitarTodas n (tail xs)
                    
    --8
    {-Indica si una lista tiene elementos repetidos.-}
    hayRepetidos :: [Int] -> Bool
    hayRepetidos xs | xs == []                                = False
                    | ((tail xs) == quitarTodas (head xs) xs) = hayRepetidos (tail xs) 
                    | otherwise                               = True

    --9
    {-Deja en la lista la primera aparicion de cada elemento, eliminando las 
    repeticiones adicionales.-}
    eliminarRepetidosAlFinal :: [Int] -> [Int]
    eliminarRepetidosAlFinal xs | hayRepetidos xs == False               = xs
                                | (cuantasVecesAparece (head xs) xs) > 1 = eliminarRepetidosAlFinal (dejarPrimera (head xs) xs)
                                | otherwise                              = head xs : eliminarRepetidosAlFinal (dejarPrimera (head (tail xs)) (tail xs))
                                
    {-Elimina las repeticiones adicionales de un elemento-}
    dejarPrimera :: Int -> [Int] -> [Int]
    dejarPrimera n xs | xs == []                = xs
                      | head xs == n            = head xs : quitarTodas n (tail xs)
                      | otherwise               = head xs : dejarPrimera n (tail xs) 
                  
    cuantasVecesAparece :: Int -> [Int] -> Int
    cuantasVecesAparece n xs | xs == []     = 0
                            | n == head xs = 1 + cuantasVecesAparece n (tail xs)
                            | otherwise    = cuantasVecesAparece n (tail xs)

    --10
    {-Deja en la lista la ultima aparicion de cada elemento, eliminando las
    repeticiones adicionales.-}
    eliminarRepetidosAlInicio :: [Int] -> [Int]
    eliminarRepetidosAlInicio xs | hayRepetidos xs == False = xs
                                | (cuantasVecesAparece (head xs) xs) > 1 = eliminarRepetidosAlInicio (dejarUltima (head xs) xs)
                                | otherwise                              = head xs : eliminarRepetidosAlInicio (dejarUltima (head (tail xs)) (tail xs))

    dejarUltima :: Int -> [Int] -> [Int]
    dejarUltima n xs | xs == []                                           = xs
                    | (head xs == n) && ((cuantasVecesAparece n xs) > 1) = dejarUltima n (tail xs)
                    | otherwise                                          = head xs : dejarUltima n (tail xs)

    --11
    {-Calcula el maximo elemento de una lista no vacia.-}
    maximo :: [Int] -> Int
    maximo xs | xs == []                 = 0
              | (tail xs) == []          = head xs
              | head xs > head (tail xs) = maximo (head xs : (tail (tail xs)))
              | otherwise                = maximo (tail xs)

    --12
    {-Ordena los elementos de forma creciente.-}
    ordenar :: [Int] -> [Int]
    ordenar xs | xs == [] = []
               | otherwise = (minimo xs) : ordenar (quitar (minimo xs) xs)

    minimo :: [Int] -> Int
    minimo xs | xs == []                 = 0
              | (tail xs) == []          = head xs
              | head xs < head (tail xs) = minimo (head xs : (tail (tail xs)))
              | otherwise                = minimo (tail xs)

    --13
    {-Dada una lista invierte su orden.-}
    reverso :: [Int] -> [Int]
    reverso xs | xs == [] = []
               | otherwise = ultimo xs : reverso (quitarUltimo (ultimo xs) xs) 

    quitarUltimo :: Int -> [Int] -> [Int]
    quitarUltimo n xs | xs == []                          = []
                      | cuantasVecesAparece n xs == 1     = quitarTodas n xs
                      | (head xs == n) && (tail xs == []) = quitarUltimo n (tail xs)
                      | otherwise                         = (head xs) : (quitarUltimo n (tail xs))

    --14
    {-Devuelve la concatenacion de la primera lista con la segunda. 
    Ejemplo: concatenar [1,2,3] [4,5,6] --> [1,2,3,4,5,6],
            concatenar [] [4,5,6]      --> [4,5,6]. 
    Esta operacion esta en el prelude y se escribe como (++).-}
    concatenar :: [Int] -> [Int] -> [Int]
    concatenar xs ys | xs == [] = ys
                    | otherwise =  concatenar (quitarUltimo (ultimo xs) xs)  (head (reverso xs) : ys)

    --15
    {-Devuelve una lista de tuplas, cada tupla contiene elementos de ambas listas que
    ocurren en la misma posicion. En caso que tengan distintas longitudes, la 
    longitud de la lista resultado es igual a la longitud de la lista mas chica
    pasada por parametro. 
    Ejemplo: zipi [1,2,3] ['a','b','c'] --> [(1,'a'), (2,'b'), (3,'c')], 
            zipi [1,2,3] ['a','b']     --> [(1,'a'), (2,'b')].-}
    zipi :: [a] -> [b] -> [(a,b)]
    zipi _ []  = []
    zipi [] _  = []
    zipi xs ys = (head xs, head ys) : (zipi (tail xs) (tail ys))

    {-La hice al pedo lmao xdddDDDDdd-}
    longitud :: [Int] -> Int
    longitud [] = 0
    longitud xs = 1 + longitud (tail xs)