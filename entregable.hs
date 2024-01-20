-- Mario Calvarro Marines
type Variable = [Char]
type Estado = [(Variable, Integer)]

-- Tipo de datos para las expresiones que pueden ser simplemente enteros,
-- variables o suma/resta/producto/división de expresiones, 
data Expr = N Integer | V Variable | Mas Expr Expr | Menos Expr Expr | Por Expr Expr | Entre Expr Expr deriving Show
-- Para evaluar una expresión necesitamos un estado, es decir, el valor de las
-- varibles que encontramos en la expresión
eval::Expr -> Estado -> Integer
eval (N n) _ = n -- Evaluación de un simple entero
eval (V v) est = snd $ head $ dropWhile (\(x, _) -> x /= v) est -- Para evaluar una variable necesitamos saber su estado
                                                                -- Si no se encuentra su estado -> excepción
-- La evaluación de las operaciones se hace recursivamente, con el operador
-- correspondiente
eval (Mas e1 e2) est = eval e1 est + eval e2 est 
eval (Menos e1 e2) est = eval e1 est - eval e2 est 
eval (Por e1 e2) est = eval e1 est * eval e2 est 
eval (Entre e1 e2) est = eval e1 est `div` eval e2 est 

data BoolExpr = B Bool | Igual Expr Expr | Menor Expr Expr | Mayor Expr Expr | MenorIgual Expr Expr | MayorIgual Expr Expr |
                Neg BoolExpr | Conj BoolExpr BoolExpr | Disy BoolExpr BoolExpr
                deriving Show
evalBool::BoolExpr -> Estado -> Bool
evalBool (B b) _ = b -- Evaluación de un simple booleano
                 -- No se consideran variables de tipo booleano (Primer punto
                 -- enunciado)

-- Evaluación de comparaciones de expresiones de enteros
evalBool (Igual e1 e2) est = eval e1 est == eval e2 est 
evalBool (Menor e1 e2) est = eval e1 est < eval e2 est 
evalBool (Mayor e1 e2) est = eval e1 est > eval e2 est 
evalBool (MenorIgual e1 e2) est = eval e1 est <= eval e2 est 
evalBool (MayorIgual e1 e2) est = eval e1 est >= eval e2 est 

-- Evaluación de operaciones entre expresiones booleanas
evalBool (Neg e1) est = not $ evalBool e1 est
evalBool (Conj e1 e2) est = evalBool e1 est && evalBool e2 est 
evalBool (Disy e1 e2) est = evalBool e1 est || evalBool e2 est 


data Instruccion = Asignacion Variable Expr | Condicional BoolExpr Programa Programa | Bucle BoolExpr Programa;
type Programa = [Instruccion]

-- Función auxiliar para modificar el estado del programa
ejecutaAux::Programa -> Estado -> Estado
ejecutaAux p est = foldl (flip ejecutaInstruccion) est p 

ejecutaInstruccion::Instruccion -> Estado -> Estado
ejecutaInstruccion (Asignacion v1 exp) est = (v1, eval exp est) : est
ejecutaInstruccion (Condicional b p1 p2) est = if evalBool b est then ejecutaAux p1 est else ejecutaAux p2 est
ejecutaInstruccion (Bucle b p) est =
    if evalBool b est then ejecutaInstruccion (Bucle b p) $ ejecutaAux p est else est

-- Función que devuelve el resultado de la variable "R"
ejecuta::Programa -> Estado -> Integer
ejecuta p e = snd $ head $ dropWhile (\(x, _) -> x /= "R") $ ejecutaAux p e
