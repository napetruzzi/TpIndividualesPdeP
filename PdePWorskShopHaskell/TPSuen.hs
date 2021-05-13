module Lib where
import Text.Show.Functions ()

data Persona = Unapersona{
    edad         :: Int,
    sueños       :: [Sueño],
    nombre       :: String,
    felicidonios :: Int,
    habilidades  :: [String]
}deriving (Show)

type Sueño = Persona -> Persona


gradoOcoeficiente :: Persona->Int-> Int-> Int-> (Int->Int->Int) -> Int
gradoOcoeficiente  unapersona protagonista edadOfelicidonios sueñosOedad funcion
  | felicidonios unapersona > 100 = protagonista * edadOfelicidonios
  | felicidonios unapersona > 50 = protagonista * sueñosOedad
  | otherwise                   = funcion protagonista 2


coeficienteDeSatisfaccion:: Persona -> Int
coeficienteDeSatisfaccion unapersona = gradoOcoeficiente unapersona (felicidonios unapersona) (edad unapersona) (length(sueños unapersona)) (div)

gradoDeAmbicionDeUnaPersona:: Persona -> Int
gradoDeAmbicionDeUnaPersona unapersona = gradoOcoeficiente unapersona (length(sueños unapersona)) (felicidonios unapersona) (edad unapersona) (*)

nombreLargo :: Persona -> Bool
nombreLargo unapersona = (>10).length.nombre $ unapersona

personaSuertuda :: Persona -> Bool
personaSuertuda = even.(*3).coeficienteDeSatisfaccion

nombreLindo :: Persona -> Bool
nombreLindo unapersona = (== 'a').last.nombre $ unapersona

recibirseDeUnaCarrera :: String -> Sueño
recibirseDeUnaCarrera nombreCarrera unapersona  = unapersona{felicidonios = length nombreCarrera * 1000 + felicidonios unapersona , habilidades = [nombreCarrera] ++ habilidades unapersona}

viajarA :: [String] -> Sueño
viajarA listaDeCiudades unapersona = unapersona{edad= edad unapersona + 1 ,felicidonios= felicidonios unapersona + 100*length listaDeCiudades }

enamorarseDe :: Persona -> Sueño
enamorarseDe unapersona otrapersona = unapersona{felicidonios = felicidonios otrapersona + felicidonios unapersona}

queTodoSigaIgual :: Sueño
queTodoSigaIgual  = id

comboPerfecto::  Sueño
comboPerfecto  unapersona =  (recibirseDeUnaCarrera "Medicina" ) (viajarA ["Berazategui","Paris"]  (unapersona {felicidonios = felicidonios unapersona + 100}) )


juan :: Persona
juan = (Unapersona 26 [viajarA ["uzbekistan","Argelia"]] "Juan" 50   ["Tocar la guitarra con los pies"])

maria :: Persona
maria = (Unapersona 23 [viajarA ["Barcelona", "Sevilla"], recibirseDeUnaCarrera "Bioquimica"] "Maria" 1000 [])

