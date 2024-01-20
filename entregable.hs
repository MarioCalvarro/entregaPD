---------------------------------
-- Mario Calvarro Marines. UCM --
---------------------------------
import System.IO

-------------
-- Parte 1 --
-------------

--- Código para las expresiones ---
type Variable = [Char]
type Estado = [(Variable, Integer)]

-- Tipo de datos para las expresiones que pueden ser simplemente enteros,
-- variables o suma/resta/producto/división de expresiones, 
data Expr = I Integer | V Variable | Expr :+ Expr | Expr :- Expr | Expr :* Expr | Expr :/ Expr deriving (Show, Read)
-- Para evaluar una expresión necesitamos un estado, es decir, el valor de las
-- varibles que encontramos en la expresión
eval::Expr -> Estado -> Integer
eval (I n) _ = n -- Evaluación de un simple entero
eval (V v) est = snd $ head $ dropWhile (\(x, _) -> x /= v) est -- Para evaluar una variable necesitamos saber su estado
                                                                -- Si no se encuentra su estado -> excepción
-- La evaluación de las operaciones se hace recursivamente, con el operador
-- correspondiente
eval ((:+) e1 e2) est = eval e1 est + eval e2 est
eval ((:-) e1 e2) est = eval e1 est - eval e2 est
eval ((:*) e1 e2) est = eval e1 est * eval e2 est
eval ((:/) e1 e2) est = eval e1 est `div` eval e2 est

data BoolExpr = B Bool | Expr :== Expr | Expr :< Expr | Expr :> Expr | Expr :<= Expr | Expr :>= Expr |
                Neg BoolExpr | BoolExpr :&& BoolExpr | BoolExpr :|| BoolExpr
                deriving (Show, Read)
evalBool::BoolExpr -> Estado -> Bool
evalBool (B b) _ = b -- Evaluación de un simple booleano
                 -- No se consideran variables de tipo booleano (Primer punto
                 -- enunciado)

-- Evaluación de comparaciones de expresiones de enteros
evalBool ((:==) e1 e2) est = eval e1 est == eval e2 est
evalBool ((:<) e1 e2) est = eval e1 est < eval e2 est
evalBool ((:>) e1 e2) est = eval e1 est > eval e2 est
evalBool ((:<=) e1 e2) est = eval e1 est <= eval e2 est
evalBool ((:>=) e1 e2) est = eval e1 est >= eval e2 est

-- Evaluación de operaciones entre expresiones booleanas
evalBool (Neg e1) est = not $ evalBool e1 est
evalBool ((:&&) e1 e2) est = evalBool e1 est && evalBool e2 est
evalBool ((:||) e1 e2) est = evalBool e1 est || evalBool e2 est


--- Código para el programa en sí ---
infixl 1 :=     -- Reducimos la prioridad de la asignación a casi el mínimo para separar la variable de la expresión adecuadamente
data Instruccion = Variable := Expr | Cond BoolExpr Programa Programa | While BoolExpr Programa deriving Show
type Programa = [Instruccion]

-- Función auxiliar para modificar el estado del programa
ejecutaAux::Programa -> Estado -> Estado
ejecutaAux p est = foldl (flip ejecutaInstruccion) est p

ejecutaInstruccion::Instruccion -> Estado -> Estado
--ejecutaInstruccion ((:=) v1 (I n)) est = (v1, n) : est
ejecutaInstruccion ((:=) v1 (I n)) [] = [(v1, n)] -- La variable es nueva → la añadimos al estado
-- Comprobamos paso a paso si la variable es nueva. Si es nueva cambiamos su
-- valor. En caso contrario seguimos comprobando recursivamente
ejecutaInstruccion ((:=) v1 (I n)) (x:xs) = if v1 == fst x then (v1, n):xs else x:ejecutaInstruccion ((:=) v1 (I n)) xs
ejecutaInstruccion ((:=) v1 exp) est = ejecutaInstruccion ((:=) v1 (I (eval exp est))) est -- Si la expresión no es directamente un entero, la evaluamos primero
ejecutaInstruccion (Cond b p1 p2) est =
    if evalBool b est then ejecutaAux p1 est else ejecutaAux p2 est
    -- Si se cumple la condicion, el nuevo estado viene del dado al ejecutar p1.
    -- En caso contrario, de ejecutar p2

ejecutaInstruccion (While b p) est =
    if evalBool b est then ejecutaInstruccion (While b p) $ ejecutaAux p est else est
    -- Si se cumple la condición, ejecutamos el programa y llamamos de nuevo al
    -- bucle con el nuevo estado. En caso contrario, el estado se mantiene igual

-- Función que devuelve el resultado de la variable "R"
ejecuta::Programa -> Estado -> Integer
ejecuta p e = snd $ head $ dropWhile (\(x, _) -> x /= "R") $ ejecutaAux p e

-- Programa de factorial
factorial = [
    "Y" := V "X",
    "R" := I 1,
    While (I 0 :< V "Y")[
        "R" := V "R" :* V "Y",
        "Y" := V "Y" :- I 1
    ]]
s0 = [("X", 3)]


-------------
-- Parte 2 --
-------------
leerEstado::String -> Estado
leerEstado = read

-- Para leer una asignación:
-- · La primera palabra es el nombre de la variable
-- · Las dos primeras palabras son el nombre y el signo de la variable
-- · El resto lo leemos como una expresión
leerAsig::String -> Instruccion
leerAsig s = head (words s) := read (concat $ drop 2 $ words s)

readProgAux::Handle -> Programa -> Programa
readProgAux h _ = []

leerWhile::String -> Handle -> Instruccion
leerWhile line h = While cond prog
    where cond = read $ concat $ drop 1 $ words line --La primera palabra de la linea es "While"
          prog = readProgAux h []

leerCond::String -> Handle -> Instruccion
leerCond line h = Cond cond prog1 prog2
    where cond = read $ concat $ drop 1 $ words line --La primera palabra de la linea es "While"
          prog1 = readProgAux h []
          prog2 = readProgAux h []

-- Suposiciones del programa:
-- 1) En cada línea tenemos una instrucción
-- 2) Las instrucciones condicionales y de bucle tienen el siguiente formato
--      · Una primera línea con el identificado (Cond / While) y, entre
--        paréntesis, la expresión booleana
--      · La siguiente línea, directamente, la primera instrucción del programa que
--        ejecutan si se cumple la condición, seguido del resto de instrucciones (una
--        por línea)
--      · La primera línea empezará con un corchete "[" y la última, con uno que
--        cierra "]".
--      · En el caso del cond, la misma sintaxis que para el programa 1
leerProgramaAux::Handle -> Programa -> IO Programa
leerProgramaAux h p = do
    line <- hGetLine h
    let fw = head $ words line
    if null line
        then return $ reverse p
    else do
        p' <- case fw of
            "While" -> return (leerWhile line h : p)
            "Cond"  -> return (leerCond line h : p)
            _       -> return (leerAsig line : p) -- Le damos la vuelta a todo al final
        leerProgramaAux h p'


leerPrograma::Handle -> IO Programa
leerPrograma h = leerProgramaAux h []

main = do
    print "Introduzca el nombre del fichero del programa"
    fileName <- getLine
    handle <- openFile fileName ReadMode
    print "Introduzca el estado inicial (El estado es una lista de parejas en la que el primer elemento es una cadena de caracteres que representa el nombre de una variable y el segundo un entero que indica el valor de dicha variable)"
    cadenaEstadoInicial <- getLine
    let estadoInicial = leerEstado cadenaEstadoInicial
    programa <- leerPrograma handle

    hClose handle
