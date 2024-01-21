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
data Instruccion = Variable := Expr | Cond BoolExpr Programa Programa | While BoolExpr Programa deriving (Show, Read)
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
-- Lectura por fichero --
leerEstado::String -> Estado
leerEstado = read

leerExpr::String -> Expr
leerExpr = read

leerBoolExpr::String -> BoolExpr
leerBoolExpr = read

-- Para leer una asignación:
-- · La primera palabra es el nombre de la variable
-- · Las dos primeras palabras son el nombre y el signo de la variable
-- · El resto lo leemos como una expresión
-- leerAsig::String -> Instruccion
-- leerAsig s = head (words s) := leerExpr (concat $ drop 2 $ words s)

leerAsig::String -> Instruccion
leerAsig = read

leerWhile::String -> Handle -> IO Instruccion
leerWhile line h = do
    let cond = leerBoolExpr $ unwords $ tail $ words line --Quitar la palabra "While"
    _ <- hGetLine h     -- Saltamos la línea que abre corchete
    prog <- leerProgramaAux h []
    return $ While cond prog

leerCond::String -> Handle -> IO Instruccion
leerCond line h = do
    let cond = leerBoolExpr $ unwords $ tail $ words line --Quitar la palabra "Cond"
    _ <- hGetLine h     -- Saltamos la línea que abre corchete
    prog1 <- leerProgramaAux h []
    _ <- hGetLine h     -- Saltamos la línea que abre corchete
    prog2 <- leerProgramaAux h []
    return $ Cond cond prog1 prog2

-- Suposiciones del programa:
-- 1) En cada línea tenemos una instrucción
-- 2) Las instrucciones condicionales y de bucle tienen el siguiente formato
--      · Una primera línea con el identificado (Cond / While) y, SIN
--        paréntesis, la expresión booleana
--      · La siguiente línea será un corchete que abre "[" y la última, uno que
--        cierra "]"
--      · Tras esto, la primera instrucción del programa que se
--        ejecuta si se cumple la condición, seguido del resto de instrucciones (una
--        por línea)
--      · En el caso del Cond, la misma sintaxis que para el programa 1 para el 2
--  (Adjunto aparece un fichero con el programa factorial como ejemplo)
leerProgramaAux::Handle -> Programa -> IO Programa
leerProgramaAux h p = do
    eof <- hIsEOF h
    if eof then
        return $ reverse p
    else do
        line <- hGetLine h
        let fw = head $ words line
        if fw == "]" then
            return $ reverse p         -- Corchete que cierra -> Acaba la lectura del programa
        else do
            p' <- case fw of
                "While" -> do
                    while <- leerWhile line h
                    return (while : p)

                "Cond"  -> do
                    cond <- leerCond line h
                    return (cond : p)

                _       -> return (leerAsig line : p) -- Le damos la vuelta a todo al final

            leerProgramaAux h p'


leerPrograma::Handle -> IO Programa
leerPrograma h = leerProgramaAux h []


-- Ejecucción paso a paso --
-- TODO: Hacer paso a paso correcto de WHILE y COND
ejecutaDebug::Programa -> Estado -> IO ()
ejecutaDebug [] s = do
    let res = snd $ head $ dropWhile (\(x, y) -> x /= "R") s
    print $ "Estado " ++ show s

ejecutaDebug ((Cond b p1 p2):xs) s = do
    print $ "Linea: " ++ show (Cond b p1 p2)
    print $ "Estado " ++ show s
    _ <- getLine
    if evalBool b s then do --La condición es verdadera -> Primer programa
        ejecutaDebug p1 s
        let ns = ejecutaInstruccion (Cond b p1 p2) s
        _ <- getLine
        ejecutaDebug xs ns
    else do     --La condición es falsa -> Segundo programa
        ejecutaDebug p2 s
        let ns = ejecutaInstruccion (Cond b p1 p2) s
        _ <- getLine
        ejecutaDebug xs ns

ejecutaDebug ((While b p):xs) s = do
    print $ "Linea: " ++ show (While b p)
    print $ "Estado: " ++ show s
    _ <- getLine
    if evalBool b s then do
        -- Tenemos que hacerlo así porque ejecutaDebug no devuelve nada (TODO)
        ejecutaDebug p s
        ejecutaDebug [While b p] $ ejecutaAux p s
        let ns = ejecutaInstruccion (While b p) s
        _ <- getLine
        ejecutaDebug xs ns
    else do     --No se ejecuta el While
        _ <- getLine
        ejecutaDebug xs s

--Asignaciones
ejecutaDebug (x:xs) s = do
    print $ "Linea: " ++ show x
    print $ "Estado: " ++ show s
    let ns = ejecutaInstruccion x s
    _ <- getLine
    ejecutaDebug xs ns


main = do
    print "Introduzca el nombre del fichero del programa"
    fileName <- getLine
    handle <- openFile "factorial.inv" ReadMode
    programa <- leerPrograma handle

    print "Introduzca el estado inicial (El estado es una lista de parejas en la que el primer elemento es una cadena de caracteres que representa el nombre de una variable y el segundo un entero que indica el valor de dicha variable)"
    cadenaEstadoInicial <- getLine
    let estadoInicial = leerEstado cadenaEstadoInicial
    let estadoInicial = [("X", 3)]

    print "Desea ejecutar el modo paso a paso: (S)i / (N)o"
    modoDebug <- getLine
    if modoDebug == "S" then
        ejecutaDebug programa estadoInicial
        --print $ "Fin del programa: " 
    else
        print $ ejecuta programa estadoInicial

    hClose handle
