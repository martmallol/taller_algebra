type Complejo = (Float,Float)

--CLASE: PARTE 1

--1
re :: Complejo -> Float
re (a,_) = a

--2
im :: Complejo -> Float
im (_,b) = b

--3
conjugado :: Complejo -> Complejo
conjugado (a,b) = (a,-b)


--4
suma :: Complejo -> Complejo -> Complejo
suma (a,b) (c,d) = (a+c,b+d)

--5
producto :: Complejo -> Complejo -> Complejo
producto (a,b) (c,d) = ( a*c-b*d , a*d+b*c )

--6
inverso :: Complejo -> Complejo
inverso (a,b) = ( a/(a**2 + b**2) , (-b)/(a**2 + b**2) )
--para hacer potencias de float, usar **

--7 (TAREA) X
cociente :: Complejo -> Complejo -> Complejo
cociente (a,b) (c,d) = ( (a*c+b*d)/(c**2+d**2) , (b*c-a*d)/(c**2+d**2) )

--8 (TAREA) 
-- ME FALTA DEFINIR POTENCIA Z 0
potencia :: Complejo -> Integer -> Complejo
potencia (a,b) 0 = (1,1)
potencia (a,b) 1 = (a,b)
potencia (a,b) n = producto (a,b) (potencia (a,b) (n-1))

--9 (TAREA)
{-Dada una funcion cuadratica ax^2 + bx + c con a,b,c en R, a != 0, definir la funcion que
tome los coeficientes a, b y c y devuelve las dos raices. En caso de haber una sola,
devolverla dos veces.-}
solucionesCuadratica :: Float -> Float -> Float -> (Complejo, Complejo)
solucionesCuadratica a b c = ( ( ecConMas , 0 ) , ( ecConMenos , 0 ) )
                  where ecConMas   = (-b + sqrt(b**2 - 4*a*c)) / (2*a)
                        ecConMenos = (-b - sqrt(b**2 - 4*a*c)) / (2*a)
------------------------------------------------------------------

--CLASE: PARTE 2

--10
modulo :: Complejo -> Float
modulo (a,b) = sqrt(a**2 + b**2)

--11
argumento :: Complejo -> Float 
argumento (a,b) |cuadrante (a,b) == 1 = atan (b/a)
                |cuadrante (a,b) == 2 = pi + (atan (b/a))
                |cuadrante (a,b) == 3 = pi + (atan (b/a))
                |otherwise = 2*pi + (atan (b/a))

cuadrante :: Complejo -> Int
cuadrante (a,b) |a>=0 && b>=0 = 1
                |a<=0 && b>=0 = 2
                |a<0 && b<=0 = 3
                |a>=0 && b<=0 = 4

--12
pasarACartesianas :: Float -> Float -> Complejo
pasarACartesianas r tita = (r*(cos tita),r*(sin tita))

--13
raizCuadrada :: Complejo -> (Complejo,Complejo)
raizCuadrada z = (pasarACartesianas r tita ,pasarACartesianas r (tita+pi))
                  where r = sqrt (modulo z)
                        tita = (argumento z)/2

--14 (TAREA)
{-Dado un polinomio az^2 + bz + c, con a,b,c en C, a != 0, implementar la funcion que
devuelve sus dos raices.-}
solucionesCuadraticaCoefComplejos :: Complejo -> Complejo -> Complejo -> Complejo -> (
Complejo,Complejo)
solucionesCuadraticaCoefComplejos a b c z = ( (realDosRaices, imaginarioRaizUno), (realDosRaices, imaginarioRaizDos) )
                              where polinomioEj       = polinomioComplejo a b c z 
                                    realDosRaices     = raizCuadrada ( ( (modulo polinomioEj) + (re polinomioEj) ) / 2 )
                                    imaginarioRaizUno = raizCuadrada ( ( (modulo polinomioEj) - (re polinomioEj) ) / 2 )
                                    imaginarioRaizDos = (-1) * imaginarioRaizUno

polinomioComplejo :: Complejo -> Complejo -> Complejo -> Complejo -> Complejo
polinomioComplejo a b c z = suma ( suma ( producto a (potencia z 2) ) (producto b z) ) c

------------------------------------------------------------------

--CLASE: PARTE 3

--15
raicesNEsimas :: Integer -> [Complejo]
raicesNEsimas n = raicesNEsimasDesde 0 n 

raicesNEsimasDesde :: Integer -> Integer -> [Complejo]
raicesNEsimasDesde k n | k>= n = []
                       |otherwise =(kesimaRaiz):(raicesNEsimasDesde (k+1) n)
                       where kesimaRaiz =(cos (2*(fromInteger k)*pi/(fromInteger n)) , sin ((2*(fromInteger k)*pi)/(fromInteger n)))

--16 (TAREA)
{-Dados k y n enteros con 0 <= k < n, devuelve la lista con las potencias 0, 1,..., n-1 
de la raiz n-esima asociada a k siguiendo la formula de arriba.-}
potenciasRaizNEsima :: Integer -> Integer -> [Complejo]
potenciasRaizNesima k 0 = (potencia (raizNEsimaAsociadaCon k) n) : [] 
potenciasRaizNEsima k n | k < n     = undefined     
                        | otherwise = (potencia (raizNEsimaAsociadaCon k) n) : (potenciasRaizNEsima k (n-1))

raizNEsimaAsociadaCon :: Integer -> Complejo
raizNEsimaAsociadaCon k = ultimoComplejo (raicesNEsimas k)

ultimoComplejo :: [Complejo] -> Complejo
ultimoComplejo zs | zs == []        =  (0, 0)
                  | (tail zs) == [] = (head zs)
                  | otherwise       = ultimo (tail zs)