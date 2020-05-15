import Text.Show.Functions()

main :: IO()
main = return()

data Jugador = Jugador {  nombre :: String ,
cantidadDeDinero :: Int ,
tacticaDeJuego :: String,
propiedadesCompradas :: [Propiedad],
accionsARealizar :: [Accion]
 } deriving (Show)

type Propiedad = (String,Int)

type Accion = Jugador -> Jugador 

carolina :: Jugador
carolina = Jugador "Carolina" 500  "accionista" [] [pasaPorElBanco , pagarAccionistas] 
 
prueba :: Int->Int->Int
prueba valor1 valor2= valor1 + valor2

manuel :: Jugador
manuel =  Jugador "Manuel" 500  "oferente singular" [] [ pasaPorElBanco, enojarse]

modificarDinero :: Int->Accion
modificarDinero cantidad jugador = jugador { cantidadDeDinero= cantidadDeDinero jugador + cantidad } 

pasaPorElBanco :: Accion
pasaPorElBanco jugador = modificarDinero 40 jugador { tacticaDeJuego="Comprador compulsivo"}

enojarse :: Accion
enojarse jugador = modificarDinero 50 jugador{accionsARealizar  = accionsARealizar jugador ++ [gritar]}

gritar::Accion
gritar jugador = jugador{ nombre = "AHHHH"++ nombre jugador}

puedeGanarSubasta :: Jugador -> Bool
puedeGanarSubasta jugador = (tacticaDeJuego jugador) == "oferente singular" || (tacticaDeJuego jugador)== "accionista"




subastar :: Propiedad->Accion
subastar propiedad jugador |puedeGanarSubasta jugador = modificarDinero (-precioPropiedad propiedad) jugador{ propiedadesCompradas = propiedadesCompradas jugador ++ [propiedad] }
                           |otherwise = jugador


precioPropiedad :: Propiedad->Int
precioPropiedad (_,precio) = precio

cobrarAlquileres :: Accion
cobrarAlquileres jugador = modificarDinero ((sum.(map alquilerPropiedad).propiedadesCompradas) jugador) jugador

alquilerPropiedad :: Propiedad -> Int
alquilerPropiedad propiedad 
    | precioPropiedad propiedad < 150 = 10
    | otherwise = 20

pagarAccionistas :: Accion
pagarAccionistas jugador
    | tacticaDeJuego jugador == "Accionista" = modificarDinero 200 jugador 
    | otherwise = modificarDinero (-100) jugador 

hacerBerrinchepor ::Propiedad->Accion
hacerBerrinchepor propiedad jugador | precioPropiedad propiedad > cantidadDeDinero jugador = hacerBerrinchepor propiedad ((gritar.(modificarDinero 10)) jugador)
                                    | otherwise = jugador{ propiedadesCompradas = propiedadesCompradas jugador ++ [propiedad] }

ultimaRonda :: Jugador->Accion
ultimaRonda jugador = foldl1 (.) (accionsARealizar jugador) 

juegoFinal :: Jugador->Jugador->Jugador
juegoFinal jugador1 jugador2 | dineroFinal jugador1 > dineroFinal jugador2= jugador1
                             | otherwise =jugador2

dineroFinal :: Jugador->Int
dineroFinal jugador= (cantidadDeDinero.jugarUltimaRonda) jugador

jugarUltimaRonda :: Accion
jugarUltimaRonda jugador = (ultimaRonda jugador) jugador