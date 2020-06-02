module Naturales
where
    import Prelude(Int,Bool(True,False),succ,pred,(||),(&&),not,otherwise)
    
    suma :: Int -> Int -> Int
    --suma m n = suma (succ (pred m)) n = suma (pred m) (succ n)
    suma 0 n = n
    suma m n = suma (pred m) (succ n)
    
    mult :: Int -> Int -> Int
    -- m * n = (1 + (m-1)) * n = n + (m-1)*n
    mult 0 n = 0
    mult m n = suma n (mult (pred m) n)
    
