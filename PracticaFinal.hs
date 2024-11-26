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
interpretacion :: Formula -> [(Var, Bool)] -> Bool
interpretacion (Atom a) asignación = valorDeVar a asignación
interpretacion (Neg f) asignación = not (interpretacion f asignación)
interpretacion (f1 :&: f2) asignación = (interpretacion f1 asignación) && (interpretacion f2 asignación)
interpretacion (f1 :|: f2) asignación = (interpretacion f1 asignación) || (interpretacion f2 asignación)
interpretacion (f1 :=>: f2) asignación = not (interpretacion f1 asignación) || (interpretacion f2 asignación)
interpretacion (f1 :<=>: f2) asignación = (interpretacion f1 asignación) == (interpretacion f2 asignación)

-- Función auxiliar --
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
diferentesCombis [] = [[]] 
diferentesCombis (var:vars) = fusionarValores var (diferentesCombis vars)

fusionarValores :: Var -> [[(Var, Bool)]] -> [[(Var, Bool)]]
fusionarValores _ [] = []
fusionarValores var (x:xs) =
  ((var, False):x) : ((var, True):x) : fusionarValores var xs
-----------------------------------------------------

 --------------------EJERCICIO 6 -------------------------
tablaDeVerdad :: Formula -> [([(Var, Bool)], Bool)]
tablaDeVerdad f1 = crearTab (combinaciones f1) f1

-- Función auxiliar ---
crearTab :: [[(Var, Bool)]] -> Formula -> [([(Var, Bool)], Bool)]
crearTab [] _ = []
crearTab (x:xs) f1 = 
  (x, interpretacion f1 x) : crearTab xs f1
-----------------------------------------------------

