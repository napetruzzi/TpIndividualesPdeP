module Lib where
import Text.Show.Functions ()


type Propiedades = (String,Int)
type Dinero = Int
type Accion = Persona -> Persona
type Ganador = String


data Persona = UnJugador{
    nombre                      :: String,
    cantidadDinero              :: Int,
    tacticaDeJuego              :: String,
    propiedadesCompradas        ::  [Propiedades],
    accionesDuranteElJuego      :: [Accion]
}deriving (Show)


--Ejemplos de Propiedades:
nuevaYork :: Propiedades
nuevaYork = ("Ny", 350)
california :: Propiedades
california = ("Cali",250)
texas :: Propiedades
texas = ("Tx",100)


carolina :: Persona
carolina  =  (UnJugador "Carolina" 500 "Accionista" [] [pasarPorElBanco,pagarAAccionistas])
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
cobrarAlquileres unjugador =  modificarDinero ((propiedadesACobrar (150<=) unjugador)*20 + (propiedadesACobrar (150>) unjugador)*10) unjugador  


--pagarAAccionistas: 
pagarAAccionistas:: Persona -> Persona
pagarAAccionistas unjugador
 |tieneTacticaAccionista unjugador = (modificarDinero 200 unjugador)
 |otherwise =  (modificarDinero (-100) unjugador) 


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

