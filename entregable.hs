---------------------------------
-- Mario Calvarro Marines. UCM --
---------------------------------
import System.IO

-------------
-- Parte 1 --
-------------
--- Código para las expresiones ---
type Variable = [Char]
type Estado = [(Variable, Integer)]     -- Las variables son siempre de tipo entero

-- Tipo de datos para las expresiones que pueden ser simplemente enteros,
-- variables o suma/resta/producto/división de expresiones
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

-- Tipo de datos para las expresiones booleanas. Pueden ser directamente un
-- booleano, una comparación de expresiones o una operación de expresiones bool
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

-- Evaluación de operaciones entre expresiones booleanas (recursividad)
evalBool (Neg e1) est = not $ evalBool e1 est
evalBool ((:&&) e1 e2) est = evalBool e1 est && evalBool e2 est
evalBool ((:||) e1 e2) est = evalBool e1 est || evalBool e2 est


--- Código para el programa en sí ---
infixl 1 :=     -- Reducimos la prioridad de la asignación a casi el mínimo para separar la variable de la expresión adecuadamente
-- Tipo de datos para las instrucciones. Pueden ser asignaciones, condicionales
-- o bucles while
data Instruccion = Variable := Expr | Cond BoolExpr Programa Programa | While BoolExpr Programa deriving (Show, Read)
-- Un programa es una lista de instrucciones
type Programa = [Instruccion]

-- Función auxiliar para modificar el estado del programa.
-- Ejecuta de izquierda a derecha las instrucciones del programa aplicando el
-- nuevo estado en cada paso
ejecutaAux::Programa -> Estado -> Estado
ejecutaAux p est = foldl (flip ejecutaInstruccion) est p

-- Función para ejecutar, en base a un estado, una instrucción. Devuelve el nuevo
-- estado
ejecutaInstruccion::Instruccion -> Estado -> Estado
ejecutaInstruccion ((:=) v1 (I n)) [] = [(v1, n)] -- La variable es nueva → la añadimos al estado

-- Comprobamos paso a paso si la variable es nueva. Si es nueva cambiamos su
-- valor. En caso contrario, seguimos comprobando recursivamente
ejecutaInstruccion ((:=) v1 (I n)) (x:xs) = if v1 == fst x then (v1, n):xs 
                                            else x:ejecutaInstruccion ((:=) v1 (I n)) xs

-- Si la expresión no es directamente un entero, la evaluamos primero
ejecutaInstruccion ((:=) v1 exp) est = ejecutaInstruccion ((:=) v1 (I (eval exp est))) est 

-- Si se cumple la condicion, el nuevo estado viene del dado al ejecutar p1.
-- En caso contrario, de ejecutar p2
ejecutaInstruccion (Cond b p1 p2) est =
    if evalBool b est then ejecutaAux p1 est else ejecutaAux p2 est

-- Si se cumple la condición, ejecutamos el programa y llamamos de nuevo al
-- bucle con el nuevo estado. En caso contrario, el estado se mantiene igual
ejecutaInstruccion (While b p) est =
    if evalBool b est then ejecutaInstruccion (While b p) $ ejecutaAux p est else est

-- Función que devuelve el resultado de la variable "R"
ejecuta::Programa -> Estado -> Integer
ejecuta p e = snd $ head $ dropWhile (\(x, _) -> x /= "R") $ ejecutaAux p e

-- Programa de factorial
factorial::Programa
factorial = [
    "Y" := V "X",
    "R" := I 1,
    While (I 0 :< V "Y")[
        "R" := V "R" :* V "Y",
        "Y" := V "Y" :- I 1
    ]]
s0::Estado
s0 = [("X", 3)]


-------------
-- Parte 2 --
-------------
-- A la hora de escribir con print no he puesto tildes puesto que aparecían con
-- caracteres incorrectos
-- Lectura por fichero --
leerEstado::String -> Estado
leerEstado = read

leerExpr::String -> Expr
leerExpr = read

leerBoolExpr::String -> BoolExpr
leerBoolExpr = read

leerAsig::String -> Instruccion
leerAsig = read

-- Leer una instrucción while recursivamente
leerWhile::String -> Handle -> IO Instruccion
leerWhile line h = do
    let cond = leerBoolExpr $ unwords $ tail $ words line --Quitar la palabra "While"
    _ <- hGetLine h     -- Saltamos la línea que abre corchete
    prog <- leerProgramaAux h [] -- Leemos recursivamente el subprograma del while
    return $ While cond prog -- Devolvemos la instrucción dentro de la mónada

-- Leer una instrucción condicional
leerCond::String -> Handle -> IO Instruccion
leerCond line h = do
    let cond = leerBoolExpr $ unwords $ tail $ words line --Quitar la palabra "Cond"
    _ <- hGetLine h     -- Saltamos la línea que abre corchete
    prog1 <- leerProgramaAux h [] -- Leemos recursivamente el primer programa
    _ <- hGetLine h     -- Saltamos la línea que abre corchete
    prog2 <- leerProgramaAux h [] -- Leemos el segundo programa
    return $ Cond cond prog1 prog2 -- Devolvemos la instrucción dentro de la mónada

-- Suposiciones del fichero con el programa:
-- 1) En cada línea tenemos una instrucción (excepto los while/cond que ocupan
--    múltiples)
-- 2) Las instrucciones condicionales y de bucle tienen el siguiente formato
--      · Una primera línea con el identificador (Cond / While) y, SIN
--        paréntesis, la expresión booleana
--      · La siguiente línea será un corchete que abre "[" y la última, uno que
--        cierra "]"
--      · Dentro de los corchetes se encontrará el subprograma que se ejecuta si
--        se cumple la condición
--      · En el caso del Cond, la misma sintaxis que para el programa 1, para el 2
--  (Adjunto aparece un fichero con el programa factorial como ejemplo)
leerProgramaAux::Handle -> Programa -> IO Programa
leerProgramaAux h p = do
    eof <- hIsEOF h
    if eof then
        return $ reverse p          -- Fin de fichero -> Acaba la lectura
    else do
        line <- hGetLine h
        let fw = head $ words line
        if fw == "]" then
            return $ reverse p         -- Corchete que cierra -> Acaba la lectura del subprograma
        else do
            p' <- case fw of
                "While" -> do
                    while <- leerWhile line h
                    return (while : p)

                "Cond"  -> do
                    cond <- leerCond line h
                    return (cond : p)

                -- Si no empieza con While / Cond, suponemos que es una
                -- asignación. Si no es así, tendremos una excepción
                _       -> return (leerAsig line : p) -- Le damos la vuelta a todo al final

            leerProgramaAux h p'


leerPrograma::Handle -> IO Programa
leerPrograma h = leerProgramaAux h []



-- Ejecucción paso a paso --
ejecutaDebug::Programa -> Estado -> IO Estado
ejecutaDebug [] s = do return s

-- Condicional
ejecutaDebug ((Cond b p1 p2):xs) s = do
    print $ "Instruccion: " ++ show (Cond b p1 p2)
    print $ "Estado " ++ show s
    _ <- getLine
    if evalBool b s then do --La condición es verdadera -> Primer programa
        ns <- ejecutaDebug p1 s
        ejecutaDebug xs ns
    else do     --La condición es falsa -> Segundo programa
        ns <- ejecutaDebug p2 s
        ejecutaDebug xs ns

-- While bucle
ejecutaDebug ((While b p):xs) s = do
    print $ "Instruccion: " ++ show (While b p)
    print $ "Estado: " ++ show s
    _ <- getLine
    if evalBool b s then do  -- La condición es verdadera -> Se ejecuta el subprograma
        ns <- ejecutaDebug p s
        ns <- ejecutaDebug [While b p] ns  -- Se ejecuta el while de nuevo con el nuevo estado
        ejecutaDebug xs ns
    else do     --No se ejecuta el While
        ejecutaDebug xs s

-- Asignaciones
ejecutaDebug (x:xs) s = do
    print $ "Instruccion: " ++ show x
    print $ "Estado: " ++ show s
    _ <- getLine
    let ns = ejecutaInstruccion x s
    ejecutaDebug xs ns


main = do
    print "Introduzca el nombre del fichero del programa"
    fileName <- getLine
    handle <- openFile fileName ReadMode
    programa <- leerPrograma handle

    print "Introduzca el estado inicial (El estado es una lista de parejas en la que el primer elemento es una cadena de caracteres que representa el nombre de una variable y el segundo, un entero que indica el valor de dicha variable)"
    cadenaEstadoInicial <- getLine
    let estadoInicial = leerEstado cadenaEstadoInicial

    print "Desea ejecutar el modo paso a paso: (S)i / No"
    modoDebug <- getLine
    if modoDebug == "S" || modoDebug == "s" then do
        s <- ejecutaDebug programa estadoInicial
        let res = snd $ head $ dropWhile (\(x, y) -> x /= "R") s
        print $ "El resultado final es " ++ show res
    else do
        --print $ ejecuta programa estadoInicial
        print $ "El resultado final es " ++ show (ejecuta programa estadoInicial)

    hClose handle
