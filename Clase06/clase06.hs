-- +------------------------------------------------+
-- |             Taller de Álgebra I                |
-- +------------------------------------------------+
-- | - Clase Número 6                               |
-- |                                                |
-- | - Tema: Listas                                 |
-- |                                                |
-- |   * Tipo y valores                             |
-- |                                                |
-- |   * Operaciones sobre listas                   |
-- |                                                |
-- |   * Recursión sobre listas                     |
-- |                                                |
-- |   * Pattern Maching sobre listas               |
-- |                                                |
-- +------------------------------------------------+



sumatoria :: [Int] -> Int
sumatoria l | l == [] = 0
            | otherwise = head l + sumatoria (tail l)

longitud :: [Int] -> Int
longitud l | l == [] = 0
           | otherwise = 1 + longitud (tail l)

pertenece :: Int -> [Int] -> Bool
pertenece x l | l == [] = False
              | otherwise = (x == head l) || pertenece x (tail l)


primerMultiplode45345 :: [Int] -> Int
primerMultiplode45345 l | mod (head l) 45345 == 0 = (head l)
                        | otherwise = primerMultiplode45345 (tail l)



sumatoriaPM :: [Int] -> Int
sumatoriaPM [] = 0
sumatoriaPM (x:xs) = x + sumatoriaPM xs