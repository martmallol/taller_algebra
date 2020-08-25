module Clase8
where 
    --------------------------------------------------------------------------------
    fact :: Int -> Int
    fact 0 = 1
    fact n = n * fact (n-1)

    type Set a = [a]

    vacio :: Set a
    vacio = []

    agregar :: Eq a => a -> Set a -> Set a
    agregar n c | n `elem` c = c
                | otherwise = n:c

    union :: Eq a => Set a -> Set a -> Set a
    union [] ys     = ys
    union (x:xs) ys = union xs (agregar x ys)

    --------------------------------------------------------------------------------
    ---------------------------- Funciones Clase 8 ---------------------------------
    --------------------------------------------------------------------------------
    --Ejercicio 1
    combinatorio :: Int -> Int -> Int
    combinatorio n k = (fact n) `div` ((fact k) * (fact (n-k)))

    combinatorio' :: Int -> Int -> Int
    combinatorio' n 0             = 1
    combinatorio' n k | n == k    = 1
                    | otherwise = (combinatorio' (n-1) k) + (combinatorio' (n-1) (k-1))

    --Ejercicio 2
    -- variaciones [4, 7] 0 = [[]]
    -- variaciones [4, 7] 1 = [[4], [7]]
    -- variaciones [4, 7] 2 = [[4, 4], [4, 7], [7, 4], [7, 7]]
    variaciones :: Set Int -> Int -> Set [Int]
    variaciones c 0 = [[]]
    variaciones c k = agregarElementosAListas c (variaciones c (k-1) )

    -- agregarElementosAListas [4, 7] [[4], [7]] = [[4, 4], [4, 7], [7, 4] [7, 7]]
    -- agregarElementosAListas [4, 7] [[4], [7]] = [[4, 4], [4, 7]] U [[7, 4], [7, 7]]
    agregarElementosAListas :: Set Int -> Set [Int] -> Set [Int]
    agregarElementosAListas [] _     = []
    agregarElementosAListas (x:xs) c = (agregarElementoAdelante x c) `union` (agregarElementosAListas xs c)


    -- agreagarElementoAdelante 4 [[4]:[7]] = agregar (4:[4]) (agregarElementoAdelante 4 [[7]])
    agregarElementoAdelante :: Int -> Set [Int] -> Set [Int]
    agregarElementoAdelante x [] = []
    agregarElementoAdelante x (y:ys) = agregar (x:y) (agregarElementoAdelante x ys)

    --Ejercicio 3
    insertarEn :: [Int] -> Int -> Int -> [Int]
    insertarEn xs n i | i == 1    = n:xs 
                    | otherwise = (head xs) : (insertarEn (tail xs) n (i-1))

    --Ejercicio 4
    {-Dado un conjunto de enteros, genere todas las posibles permutaciones de los numeros
    del conjunto pasado por parametro.
    Ej permutaciones [1,2,3]
    [[1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2], [3, 2, 1]]-}
    permutaciones :: Set Int -> Set [Int]
    permutaciones []     = [[]]
    permutaciones (c:cs) = insertarEnCadaPosDeTodasLasListas (permutaciones cs) c


    insertarEnCadaPosDeTodasLasListas :: Set [Int] -> Int -> Set [Int] 
    insertarEnCadaPosDeTodasLasListas [] c       = []
    insertarEnCadaPosDeTodasLasListas (xs:xss) c = (insertarEnCadaPos xs c (length xs + 1)) `union`
                                                (insertarEnCadaPosDeTodasLasListas xss c)

    insertarEnCadaPos :: [Int] -> Int -> Int -> Set [Int]
    insertarEnCadaPos xs c 1 = agregar (insertarEn xs c 1) vacio
    insertarEnCadaPos xs c i = agregar (insertarEn xs c i) (insertarEnCadaPos xs c (i-1))
    --[1, 2] 3 1 = [3, 1, 2]
    --[1, 2] 3 2 = [1, 3, 2]
    --[1, 2] 3 3 = [1, 2, 3]