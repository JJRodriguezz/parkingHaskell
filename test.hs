import Data.Time.Clock
import Data.List (find)
import System.IO
import System.Directory (doesFileExist)
import Control.Exception (catch, IOException)
import Control.DeepSeq (deepseq)


-- Definición del tipo de datos para representar la información de un vehículo
data Vehiculo = Vehiculo {
    placa :: String,
    entrada :: UTCTime,
    salida :: Maybe UTCTime
} deriving (Show, Read)

-- Función para registrar la entrada de un vehículo al parqueadero
registrarEntrada :: String -> UTCTime -> [Vehiculo] -> [Vehiculo]
registrarEntrada placaVehiculo tiempo parqueadero =
    Vehiculo placaVehiculo tiempo Nothing : parqueadero

-- Función para registrar la salida de un vehículo del parqueadero
registrarSalida :: String -> UTCTime -> [Vehiculo] -> [Vehiculo]
registrarSalida placaVehiculo tiempo parqueadero =
    map (\v -> if placaVehiculo == placa v then v { salida = Just tiempo } else v) parqueadero

-- Función para buscar un vehículo por su placa en el parqueadero
buscarVehiculo :: String -> [Vehiculo] -> Maybe Vehiculo
buscarVehiculo placaVehiculo parqueadero =
    find (\v -> placaVehiculo == placa v && salida v == Nothing) parqueadero

-- Función para calcular el tiempo que un vehículo permaneció en el parqueadero
tiempoEnParqueadero :: UTCTime -> Vehiculo -> String
tiempoEnParqueadero tiempoActual vehiculo =
    case salida vehiculo of
        Just tiempoSalida -> formatearTiempo (diffUTCTime tiempoSalida (entrada vehiculo))
        Nothing           -> "El vehículo aún se encuentra en el parqueadero. Tiempo en parqueadero: " ++ formatearTiempo (diffUTCTime tiempoActual (entrada vehiculo))

-- Función para guardar la información de los vehículos en un archivo de texto
guardarParqueadero :: [Vehiculo] -> IO () -- .
guardarParqueadero parqueadero = do
    let contenido = unlines (map show parqueadero)
    contenido `deepseq` withFile "parqueadero.txt" WriteMode (\handle -> do
        hPutStr handle contenido
        putStrLn "Parqueadero guardado en el archivo parqueadero.txt."
        ) `catch` handleGuardarExcepcion
    where
        handleGuardarExcepcion :: IOException -> IO ()
        handleGuardarExcepcion e = putStrLn $ "Error al guardar el parqueadero: " ++ show e

-- Función para cargar la información de los vehículos desde un archivo de texto
cargarParqueadero :: IO [Vehiculo] -- .
cargarParqueadero = do
    existeArchivo <- doesFileExist "parqueadero.txt"
    if existeArchivo
        then do
            contenido <- readFile "parqueadero.txt"
            let lineas = lines contenido
            return (map leerVehiculo lineas)
        else do
            putStrLn "El archivo parqueadero.txt no existe. Se creará uno nuevo."
            return []
    where
        leerVehiculo linea = case reads linea of
                                [(vehiculo, "")] -> vehiculo
                                _ -> error $ "Error al leer el vehículo: " ++ linea

-- Función para mostrar la información de un vehículo como cadena de texto
mostrarVehiculo :: Vehiculo -> String
mostrarVehiculo vehiculo =
    let entradaStr = "Entrada: " ++ show (entrada vehiculo)
        salidaStr = case salida vehiculo of
                        Just t -> "Salida: " ++ show t
                        Nothing -> "En parqueadero"
    in placa vehiculo ++ "," ++ entradaStr ++ "," ++ salidaStr

-- Función auxiliar para formatear el tiempo
formatearTiempo :: NominalDiffTime -> String -- .
formatearTiempo tiempo =
    let segundosEnUnDia = 24 * 60 * 60
        segundosEnUnaHora = 60 * 60
        segundosEnUnMinuto = 60
        segundos = round tiempo
        dias = segundos `div` segundosEnUnDia
        horas = (segundos `mod` segundosEnUnDia) `div` segundosEnUnaHora
        minutos = (segundos `mod` segundosEnUnaHora) `div` segundosEnUnMinuto
        restantesSegundos = segundos `mod` segundosEnUnMinuto
    in unwords [show dias ++ " días", show horas ++ " horas", show minutos ++ " minutos", show restantesSegundos ++ " segundos"]

-- Función para listar todos los vehículos en el parqueadero
listarVehiculos :: [Vehiculo] -> IO () -- .
listarVehiculos parqueadero = do
    putStrLn "Vehículos en el parqueadero:"
    mapM_ (putStrLn . mostrarVehiculo) parqueadero

-- Función principal del programa
main :: IO ()
main = do
    parqueadero <- cargarParqueadero
    putStrLn "¡Bienvenido al Sistema de Gestión de Parqueadero!"
    cicloPrincipal parqueadero

-- Función para el ciclo principal del programa
cicloPrincipal :: [Vehiculo] -> IO ()
cicloPrincipal parqueadero = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar entrada de vehículo"
    putStrLn "2. Registrar salida de vehículo"
    putStrLn "3. Buscar vehículo por placa"
    putStrLn "4. Listar vehículos en el parqueadero"
    putStrLn "5. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese la placa del vehículo:"
            placaVehiculo <- getLine
            tiempoActual <- getCurrentTime
            let parqueaderoActualizado = registrarEntrada placaVehiculo tiempoActual parqueadero
            putStrLn $ "Vehículo con placa " ++ placaVehiculo ++ " ingresado al parqueadero."
            guardarParqueadero parqueaderoActualizado
            cicloPrincipal parqueaderoActualizado

        "2" -> do
            putStrLn "Ingrese la placa del vehículo a salir:"
            placaVehiculo <- getLine
            tiempoActual <- getCurrentTime
            let parqueaderoActualizado = registrarSalida placaVehiculo tiempoActual parqueadero
            putStrLn $ "Vehículo con placa " ++ placaVehiculo ++ " salido del parqueadero."
            guardarParqueadero parqueaderoActualizado
            cicloPrincipal parqueaderoActualizado

        "3" -> do
            putStrLn "Ingrese la placa del vehículo a buscar:"
            placaVehiculo <- getLine
            tiempoActual <- getCurrentTime
            case buscarVehiculo placaVehiculo parqueadero of
                Just vehiculo -> do
                    let tiempoTotal = tiempoEnParqueadero tiempoActual vehiculo
                    putStrLn $ "El vehículo con placa " ++ placaVehiculo ++ " se encuentra en el parqueadero."
                    putStrLn $ "Tiempo en parqueadero: " ++ tiempoTotal
                Nothing -> putStrLn "Vehículo no encontrado en el parqueadero."
            cicloPrincipal parqueadero

        "4" -> do
            listarVehiculos parqueadero
            cicloPrincipal parqueadero

        "5" -> putStrLn "¡Hasta luego!"

        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal parqueadero 