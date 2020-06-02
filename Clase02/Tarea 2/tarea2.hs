{-1-}

{-2-}
prodIntPM :: (Int, Int) -> (Int, Int) -> (Int, Int)
prodIntPM (vx, vy) (ux, uy) = (vx*ux, vy*uy)

prodInt :: (Int, Int) -> (Int, Int) -> (Int, Int)
prodInt v u = ((fst v) * (fst u) , (snd v) * (snd u))
{-3-}
todoMenor :: (Int, Int) -> (Int, Int) -> (Bool, Bool)
todoMenor v u = ((fst v) < (fst u) , (snd v) < (snd u))
{-4-}
distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos p1 p2 = sqrt(((fst p2) - (fst p1))^2 + ((snd p2) - (snd p1))^2)

{-ARREGLAR
distanciaPuntosResume :: (Eq, Eq) -> (Eq, Eq) -> Eq
distanciaPuntosResume p1 p2 = sqrt(pTermino^2 + sTermino^2)
                              where pTermino = ((fst p2) - (fst p1))
                              where sTermino = ((snd p2) - (snd p1))-}
{-5-}
sumaTerna :: (Int, Int, Int) -> Int
sumaTerna (x, y, z) = x + y + z

{-6-}
posicPrimerPar :: (Int, Int, Int) -> Int
posicPrimerPar (x, y, z) | mod x 2 == 0 = 1
                         | mod y 2 == 0 = 2
                         | mod z 2 == 0 = 3
                         | otherwise = 4

{-ARREGLAR
posicionPrimerPar :: (Int, Int, Int) -> Int
posicionPrimerPar (x, y, z) | x esPar = Primero
                            | y esPar = Segundo
                            | z esPar = Tercero
                            | otherwise = 4
                            where esPar = 'mod' 2 == 0-}
{-7-}
-- Falta agregar la opcion par floats. Deberia preguntar
crearPar :: a -> b -> (a, b)
crearPar x y = (x, y)
{-8-}
invertir :: (a, b) -> (b, a)
invertir (x, y) = (y, x)