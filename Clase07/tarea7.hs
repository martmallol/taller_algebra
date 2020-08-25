module Tarea7
where 
    import Tarea6
    type Set a = [a]
    --FUNCIONES HECHAS EN CLASE
    vacio :: [Int]
    vacio = []

    pertenece :: Int -> [Int] -> Bool
    pertenece _ [] = False
    pertenece x (y:ys) | x == y    = True
                       | otherwise = pertenece x ys

    agregar :: Int -> [Int] -> [Int]
    agregar x c | pertenece x c = c
                | otherwise     = x:c

    incluido :: Set Int -> Set Int -> Bool
    incluido [] c     = True
    incluido (x:xs) c = pertenece x c && incluido xs c

    iguales :: Set Int -> Set Int -> Bool
    iguales c1 c2 = incluido c1 c2 && incluido c2 c1 
    
    --FUNCIONES HECHAS DE TAREA
    --1
    {-Dado dos conjuntos, devuelve la union entre ellos-}
    union :: Set Int -> Set Int -> Set Int
    union xs []                        = ordenar xs
    union xs (y:ys) | y `pertenece` xs = union xs ys
                    | otherwise        = union (y:xs) ys

    --2
    {-Dado dos conjuntos, devuelve la intereseccion entre ellos.-}
    interseccion :: Set Int -> Set Int -> Set Int
    interseccion a b = reverso (interseccionAux a b [])  

    interseccionAux :: Set Int -> Set Int -> Set Int -> Set Int
    interseccionAux _ [] cs                                      = cs
    interseccionAux xs (y:ys) cs | cuantasVecesAparece y xs == 1 = (interseccionAux xs ys cs) ++ (y : cs)
                                 | otherwise                     = interseccionAux xs ys cs

    --3
    {-Dado los conjuntos A y B, devuelve A - B.-}
    diferencia :: Set Int -> Set Int -> Set Int
    diferencia a b = diferenciaAux a b []

    diferenciaAux :: Set Int -> Set Int -> Set Int -> Set Int
    diferenciaAux [] _ cs                                 = cs
    diferenciaAux (x:xs) (y:ys) cs | x `pertenece` (y:ys) = diferenciaAux xs (y:ys) cs 
                                   | otherwise            = reverso ((diferenciaAux xs (y:ys) cs) ++ (agregar x cs)) 
                            

    --4
    {-Dado los conjuntos A y B, devuelve la diferencia simetrica, es decir, A4B-}
    diferenciaSimetrica :: Set Int -> Set Int -> Set Int
    diferenciaSimetrica a b     = ordenar( (diferencia a b) `union` (diferencia b a) )
    --Tambien se puede escribir = diferencia (a `union` b) (a `interseccion` b)

    --5
    {-Genera el conjunto de partes de un conjunto dado.-}
    partes :: Set Int -> Set (Set Int)
    partes []     = [[]]
    partes (x:xs) = unionC (partes xs) (agregarATodos x (partes xs))

    agregarATodos :: Int -> Set (Set Int) -> Set (Set Int)
    agregarATodos x []     = []
    agregarATodos x (c:cs) = agregarC (agregar x c) (agregarATodos x cs)

    agregarC :: Set Int -> Set (Set Int) -> Set (Set Int)
    agregarC xs xss | xs `perteneceC` xss = xss
                    | otherwise           = xs:xss

    perteneceC :: Set Int -> Set (Set Int) -> Bool
    perteneceC xs []       = False
    perteneceC xs (ys:yss) = iguales xs ys || perteneceC xs yss


    unionC :: Set (Set Int) -> Set (Set Int) -> Set (Set Int)
    unionC xs []                        = xs
    unionC xs (y:ys) | y `perteneceC` xs = unionC xs ys
                     | otherwise        = unionC (y:xs) ys

    --6
    {-Genera los subconjuntos del conjunto-}
    partesN :: Int -> Set (Set Int)
    partesN n = partes (conjuntoHasta n)

    conjuntoHasta :: Int -> Set Int
    conjuntoHasta 0 = []
    conjuntoHasta n = reverso (n:(conjuntoHasta (n-1)))
    
    {---7
    {-Genera todos los pares posibles (como pares de dos elementos) tomando el primer
    elemento del primer conjunto y el segundo elemento del segundo conjunto.-}
    {-productoCartesiano :: Set Int -> Set Int -> Set (Int, Int)
    productoCartesiano a b = prodCartesianoAux a b

    prodCartesianoAux :: Set Int -> Set Int -> Set (Int, Int)
    prodCartesianoAux _ []  = []
    prodCartesianoAux [] _  = [] 
    prodCartesianoAux (x:xs) (y:ys) = ((x, y) : prodCartesianoAux (x:xs) ys) `unionAux` (prodCartesianoAux x (y:ys))

    agregarAux :: Set Int -> Set (Int, Int) -> Set (Int, Int)
    agregarAux xs xss | xs `perteneceAux` xss = xss
                      | otherwise             = xs:xss-}
    
    incluidoAux :: Set (Int, Int) -> (Int, Int) -> Bool
    incluidoAux [] c     = True
    incluidoAux (x:xs) c = pertenece x c && incluido xs c

    igualesAux :: (Int, Int) -> (Int, Int) -> Bool
    igualesAux c1 c2 = incluido c1 c2 && incluido c2 c1

    perteneceAux :: (Int, Int) -> Set (Int, Int) -> Bool
    perteneceAux xs []       = False
    perteneceAux xs (ys:yss) = iguales xs ys || perteneceAux xs yss


    {-unionAux :: Set (Int, Int) -> Set (Int, Int) -> Set (Int, Int)
    unionAux xs []                        = xs
    unionAux xs (y:ys) | y `perteneceAux` xs = unionAux xs ys
                       | otherwise        = unionAux (y:xs) ys-}
    --TENGO QUE CREAR LA FUNC UNIONC-}
    