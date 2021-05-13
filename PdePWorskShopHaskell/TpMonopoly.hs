module Lib where
import Text.Show.Functions ()


type Propiedades = (String,Int)
type Dinero = Int
type Accion = Persona -> Persona
type Ganador = String
-- UN TYPE ALIAS GANADADOR???

data Persona = UnJugador{
    nombre                      :: String,
    cantidadDinero              :: Int, --empeizan con 500
    tacticaDeJuego              :: String,
    propiedadesCompradas        ::  [Propiedades], --una tupla con Nombre y precio
    accionesDuranteElJuego      :: [Accion]
}deriving (Show)


carolina :: Persona
carolina  =  (UnJugador "Carolina" 500 "Accionista" [("Ny",10),("Cali",160)] [pasarPorElBanco,pagarAAccionistas])
manuel :: Persona
manuel = (UnJugador "Manuel" 500 "Oferente singular" [] [pasarPorElBanco,enojarse])


-- pasarPorElBanco: 
modificarDinero :: Dinero -> Persona -> Persona
modificarDinero dinero unjugador =  unjugador{cantidadDinero =  cantidadDinero unjugador + dinero}

pasarPorElBanco :: Persona -> Persona
pasarPorElBanco unjugador = (modificarDinero 40) unjugador{ tacticaDeJuego = "Comprador compulsivo"}


--enojarse:
enojarse:: Persona -> Persona
enojarse unjugador =  (modificarDinero 50) unjugador{accionesDuranteElJuego = accionesDuranteElJuego unjugador ++ [gritar]}


--gritar:
gritar:: Persona -> Persona
gritar unjugador = unjugador{nombre = "AHHHH" ++ nombre unjugador} 


--subastar:
tieneTacticaOferenteSingular:: Persona -> Bool
tieneTacticaOferenteSingular unjugador = tacticaDeJuego  unjugador == "Oferente singular"

tieneTacticaAccionista:: Persona -> Bool
tieneTacticaAccionista unjugador = tacticaDeJuego unjugador == "Accionista"

tieneDinero::  (String,Int) -> Persona -> Bool
tieneDinero propiedad unjugador = (cantidadDinero unjugador) - (snd propiedad) >= 0

comprarPropiedad :: (String,Int) -> Persona -> Persona
comprarPropiedad propiedad unjugador =  ( modificarDinero (- snd propiedad)) unjugador{propiedadesCompradas = [propiedad] ++ propiedadesCompradas unjugador}
 
subastar:: (String,Int) -> Persona -> Persona
subastar propiedad unjugador 
 | (tieneTacticaOferenteSingular unjugador || tieneTacticaAccionista unjugador) && (tieneDinero  propiedad unjugador ) = comprarPropiedad propiedad unjugador
 | otherwise = unjugador


--cobrarAlquileres:
propiedadesACobrar:: (Int -> Bool) -> Persona -> Int
propiedadesACobrar funcion unjugador =  length(filter funcion (map snd (propiedadesCompradas unjugador)))

cobrarAlquileres:: Persona -> Persona
cobrarAlquileres unjugador =  modificarDinero ((propiedadesACobrar (150<=) unjugador)*20 + (propiedadesACobrar (150>) unjugador)*10) unjugador --unjugador{cantidadDinero = cantidadDinero unjugador + (propiedadesACobrar (150<=) unjugador)*20 + (propiedadesACobrar (150>) unjugador)*10 } 


--pagarAAccionistas: 
pagarAAccionistas:: Persona -> Persona
pagarAAccionistas unjugador
 |tieneTacticaAccionista unjugador = (modificarDinero 200 unjugador)-- unjugador{cantidadDinero = cantidadDinero unjugador +200}
 |otherwise =  (modificarDinero (-100) unjugador) --unjugador{cantidadDinero = cantidadDinero unjugador - 100}


--hacerBerrinchePor: 
hacerBerrinchePor :: (String,Int) -> Persona -> Persona 
hacerBerrinchePor propiedad unjugador 
 | tieneDinero propiedad unjugador = comprarPropiedad propiedad unjugador
 | otherwise = (hacerBerrinchePor propiedad).gritar.(modificarDinero 10) $ unjugador



--juegoFinal
ultimaRonda:: Persona -> Accion
ultimaRonda unjugador =  foldr1 (.) (accionesDuranteElJuego unjugador) 

juegoFinal :: Persona -> Persona -> Ganador 
juegoFinal jugador1 jugador2 
 | (cantidadDinero ((ultimaRonda jugador1) jugador1)) > (cantidadDinero ((ultimaRonda jugador2) jugador2)) = (nombre jugador1)
 | otherwise = nombre jugador2


{-

Se pide:
Modelar a Carolina y Manuel.
Modelar las acciones.
Modelar las propiedades.
Hacer una función juegoFinal la cual toma dos participantes y devuelve al ganador.


-}