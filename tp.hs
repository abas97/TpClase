import Text.Show.Functions()

main :: IO()
main = return()

data Jugador = Jugador {  nombre :: String ,
cantidadDeDinero :: Int ,
tacticaDeJuego :: String,
propiedadesCompradas :: [Propiedad],
accionsARealizar :: [Acciones] } deriving (Show)

type Propiedad = (String,Int)

type Acciones = Jugador -> Jugador 

carolina :: Jugador
carolina = Jugador "Carolina" 500  "accionista" [] [pasaPorElBanco , pagarAccionistas] 
 
manuel :: Jugador
manuel =  Jugador "Manuel" 500  "oferente singular" [] [ pasaPorElBanco, enojarse]

pasaPorElBanco :: Acciones
pasaPorElBanco jugador = jugador { cantidadDeDinero = cantidadDeDinero jugador + 40 , tacticaDeJuego="Comprador compulsivo"}

enojarse :: Acciones
enojarse jugador = jugador{
    cantidadDeDinero = cantidadDeDinero jugador + 50,
    accionsARealizar  = accionsARealizar jugador ++ [gritar]
}

gritar::Acciones
gritar jugador = jugador{ nombre = "AHHHH"++ nombre jugador}

ganarSubasta :: Jugador -> Bool
ganarSubasta jugador = (tacticaDeJuego jugador) == "oferente singular" || (tacticaDeJuego jugador)== "accionista"

subastar :: Propiedad->Acciones
subastar propiedad jugador |ganarSubasta jugador = jugador{ cantidadDeDinero =cantidadDeDinero jugador-snd propiedad, propiedadesCompradas = propiedadesCompradas jugador ++ [propiedad] }
                           |otherwise = jugador


cobrarAlquileres :: Acciones
cobrarAlquileres jugador = jugador { cantidadDeDinero = cantidadDeDinero jugador + (sum.(map esBarata).propiedadesCompradas) jugador }

esBarata :: Propiedad -> Int
esBarata propiedad 
    | snd propiedad < 150 = 10
    | otherwise = 20

pagarAccionistas :: Acciones 
pagarAccionistas jugador
    | tacticaDeJuego jugador == "Accionista" = jugador { cantidadDeDinero = cantidadDeDinero jugador + 200 }
    | otherwise = jugador { cantidadDeDinero = cantidadDeDinero jugador - 100 }