data Var = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z deriving (Show, Eq, Ord)

data Formula = Atom Var
               |Neg Formula
               |Formula :&: Formula
               |Formula :|: Formula
               |Formula :=>: Formula
               |Formula :<=>: Formula deriving (Show, Eq, Ord)

infixl 9 :&:
infixl 9 :|:
infixl 7 :=>:
infixl 8 :<=>:

------------------- EJERCICIO 1 --------------------
variables :: Formula -> [Var]
variables (Atom a) = [a]
variables (Neg f1)  = conjunto (variables f1)
variables (f1 :&: f2) = conjunto (variables f1 ++ variables f2)
variables (f1 :|: f2) = conjunto (variables f1 ++ variables f2)
variables (f1 :=>: f2) = conjunto (variables f1 ++ variables f2)
variables (f1 :<=>: f2) = conjunto (variables f1 ++ variables f2)

------Funciones auxiliares para el ejercicio 1-------
estaContenido :: [Var] -> Var -> Bool
estaContenido [] a = False
estaContenido (x:xs) a = 
  if a == x
  then True
  else estaContenido xs a

conjunto :: [Var] -> [Var] 
conjunto [] = []
conjunto (x:xs) =
  if estaContenido xs x
  then conjunto xs
  else x:conjunto xs
----------------------------------------------------

-------------------- EJERCICIO 2 --------------------
negacion :: Formula -> Formula
negacion (Atom a) = Neg (Atom a)
negacion (Neg f1) = f1
negacion (f1 :&: f2) = negacion f1 :|: negacion f2
negacion (f1 :|: f2) = negacion f1 :&: negacion f2
negacion (f1 :=>: f2) = f1 :&: negacion f2
negacion (f1 :<=>: f2) = (f1 :&: negacion f2) :|: (f2 :&: negacion f1)
-----------------------------------------------------

-------------------- EJERCICIO 3 --------------------
equivalencia :: Formula -> Formula
equivalencia (Atom a) = (Atom a)
equivalencia (Neg (Atom a)) = negacion (Atom a)
equivalencia (f1 :|: f2) = equivalencia f1 :|: equivalencia f2
equivalencia (f1 :&: f2) = equivalencia f1 :&: equivalencia f2
equivalencia (f1 :=>: f2) = equivalencia (negacion (f1 :&: negacion f2))
equivalencia (f1 :<=>: f2) = equivalencia (negacion (f1 :&: negacion f2) :|: (f2 :&: negacion f1))
-----------------------------------------------------

-------------------- EJERCICIO 4 --------------------
---Evalúa una formula booleana bajop una lista de tuplas que son los valores que se asiganan a las variables (true, false)
interpretacion :: Formula -> [(Var, Bool)] -> Bool
interpretacion (Atom a) asignación = valorDeVar a asignación
interpretacion (Neg f) asignación = not (interpretacion f asignación)
interpretacion (f1 :&: f2) asignación = (interpretacion f1 asignación) && (interpretacion f2 asignación)
interpretacion (f1 :|: f2) asignación = (interpretacion f1 asignación) || (interpretacion f2 asignación)
interpretacion (f1 :=>: f2) asignación = not (interpretacion f1 asignación) || (interpretacion f2 asignación)
interpretacion (f1 :<=>: f2) asignación = (interpretacion f1 asignación) == (interpretacion f2 asignación)

-- Función auxiliar --
-- busca el valorboolenao de una variable específica, devuelve error en caso de no encontrarle, devuelve val si a == x y si no sigue buscando
valorDeVar :: Var -> [(Var, Bool)] -> Bool
valorDeVar a [] = error "No todas las variables están definidas"
valorDeVar a ((x, val):xs)
  | a == x    = val
  | otherwise = valorDeVar a xs
-----------------------------------------------------

-------------------- EJERCICIO 5 --------------------
--- Definición de las combinaciones para una fórmula booleana
combinaciones :: Formula -> [[(Var, Bool)]]
combinaciones f1 = diferentesCombis (variables f1)

------Funciones auxiliares para el ejercicio 5-------
diferentesCombis :: [Var] -> [[(Var, Bool)]]
diferentesCombis [] = [[]] --Si no hay más combis devuelve la lista vacía
diferentesCombis (var:vars) = fusionarValores var (diferentesCombis vars) --Crea todas las combinaciones posibles para las variables y luego el valor para la variable actual

--- Tomz todas las combinaciones generadas y luego las fusiana con la variable actual
fusionarValores :: Var -> [[(Var, Bool)]] -> [[(Var, Bool)]]
fusionarValores _ [] = []
fusionarValores var (x:xs) =
  ((var, False):x) : ((var, True):x) : fusionarValores var xs

--convierte las dos listas en tuplas (Var, bool) como lo que hicmos en el lab
asignarValores :: [Var] -> [Bool] -> [(Var, Bool)]
asignarValores [] [] = []  
asignarValores (v:vs) (b:bs) = (v, b) : asignarValores vs bs
-----------------------------------------------------

 --------------------EJERCICIO 6 -------------------------
tablaDeVerdad :: Formula -> [([(Var, Bool)], Bool)]a
tablaDeVerdad f1 = crearTab (combinaciones f1) f1

-- Función auxiliar ---
crearTab :: [[(Var, Bool)]] -> Formula -> [([(Var, Bool)], Bool)]
crearTab [] _ = [] --No hay mas combis, no devueñve nada
crearTab (x:xs) f1 = (x, interpretacion f1 x) : crearTab xs f1 --Evalúa y guarda el resultado
-----------------------------------------------------

