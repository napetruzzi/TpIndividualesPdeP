
module Lib where


pesoPino :: Int -> Int
alturaRest :: Int -> Int
alturaBase :: Int -> Int
esPesoUtil :: Int -> Bool
--sirvePino :: Int -> ??-> Bool
--sirvePino :: Int -> a -> Bool

alturaBase alturaM = min alturaM 3
alturaRest alturaM = max alturaM (alturaBase alturaM) - alturaBase alturaM
pesoPino alturaM = (alturaBase alturaM)*300 + (alturaRest alturaM)*200
esPesoUtil pesoKg = pesoKg >= 400 &&  pesoKg<= 1000

--esPar numero = rem numero 2 == 0
--sirvePino alturaPino = esPesoUtil.pesoPino (alturaPino)
--funcionMisteriosa2 = length.lar
--mitad numero = numero / 2
--longitudPar = even.length 

productoDeLujo :: String -> Bool
productoDeLujo nombreProd = 'x' `elem`  nombreProd  || 'z' `elem`  nombreProd 

productoCodiciado :: String -> Bool
productoCodiciado nombreProd = (length nombreProd) > 10

productoDeElite :: String -> Bool
productoDeElite nombreProd = (productoDeLujo  nombreProd && productoCodiciado  nombreProd)

productoCorriente :: String -> Bool
productoCorriente nombreProd = head nombreProd == 'A' || head nombreProd == 'E' || head nombreProd == 'I' || head nombreProd == 'O' || head nombreProd == 'U' || head nombreProd == 'a' || head nombreProd == 'e' || head nombreProd == 'i' || head nombreProd == 'o' || head nombreProd == 'u'

--sacar las últimas letras hasta que la cantidad de letras en el nombre quede igual a 10 (ó menor a 10 en productos con nombres cortos)

descodiciarProducto :: String -> String
droppear :: String -> String
droppear nom = drop 2 nom

descodiciarProducto nombreProd = reverse.droppear.reverse $ nombreProd

--esMultiploDe  :: (Integral a) => a -> a -> Bool
--esMultiploDe num1 num2 = num2 div num1 || num2 div num1

--esMultiploDe:: Integral a => a -> a -> Bool
--esMultiploDe num1 num2 =  ((==0).div num1 $ num2) ||((==0).div num2 $ num1) || num1 == num2

sumaEsPar :: Int -> Int -> Bool
sumaEsPar x = even.(+) x 
--sumaEsPar x y= even.(+) x y

--sumarNumeroAlTriple :: Int -> Int -> Int
----sumarNumeroAlTriple num2= (+num2).(*3)
--sumarNumeroAlTriple num2= ((+).(*3)) num2

esMultiploDe :: Int -> Int -> Bool
esMultiploDe num1 = (==0).mod num1

--esBisiesto :: Int -> Bool 
--esBisiesto = esMultiploDe  400 

--mejor:: (Int -> Int ) -> (Int -> Int) -> Int
--mejor funcion1 funcion2 num = max (funcion1 num funcion2 num)

--aplicarPar :: (Int -> Int) -> Int -> Int -> Int
--aplicarPar funcion val1 val2 = (funcion val1 , funcion val2)

--cuantoPagaCadaUno :: (Fractional a)=> a -> a -> a 

--cuantoPagaCadaUno precioPizza = (/)((techo.(/8).(*3))* precioPizza)
--cuantoPagaCadaUno precioPizza cantComensales = ((*precioPizza).techo ((3*cantComensales)/8)) / cantComensales

--darVuelta :: (c->b) -> a -> a -> b
--darVuelta funcion argumento1 argumento2 = funcion argumento2 argumento1 

cuadrado:: Int -> Int
cuadrado num = num*num

triple :: Int -> Int
triple num = num*3

--mejor:: (Int -> Int)-> (Int->Int) -> Int -> Int
--mejor cuadrado triple num = max (cuadrado num $ triple num)

cosa:: String -> Bool
cosa = (>10).length 

--esNotaBochazo val1 = val1<4
--aprobo (val1,val2) = not (esNotaBochazo val1) && not (esNotaBochazo val2)

--obtenerEdad (_, edad) = edad
--esMayorDeEdad  = (21<).obtenerEdad 
textos :: [(a,b)] -> [b]
textos lista = map snd lista

recortarUnTuit :: (String,String) -> (String,String)
recortarUnTuit (user, tuit ) = (user,take 15 tuit)

recortar:: [(String,String)] -> [(String,String)]
recortar listaTuits = map recortarUnTuit listaTuits

--obtenerTuit (_,tuit) = length tuit

--tuitCorto  = 10 > obtenerTuit 

--cantidadTuitsCortos = sum.filter tuitCorto


--Sesumir  listaTuit = textos.recortar $ listaTuit

--obtenerPorciones (_,cantidad) = cantidad

--cuantasMuzzas listaDeTuplas = (sum.map obtenerPorciones $ listaDeTuplas)/8

--obtenerPorcionesMuzza (tipoPizza,cantidad) 
 --                                   | tipoPizza == "muzza" = cantidad
  --                                  | otherwise = 0


--cuantasMuzzas listaDeTuplas = ceiling((sum.map obtenerPorcionesMuzza $ listaDeTuplas)/8)


--cabeza :: [a] ->a
--cabeza (cabeza : _) = cabeza

--contiene elemento lista = elem elemento lista

{-
cuandoHizoMasLlamadas llamada 
  | length (fst llamada) > length (snd llamada) = "Normal" 
  | otherwise = "Reducido"

cuandoHizoLaLlamadaMasLarga llamada 
  | maximum (fst llamada) > maximum (snd llamada) = "Normal" 
  | otherwise = "Reducido"

cuandoHizoMasLlamadasBreves llamada 
  | length (filter (<2) (fst llamada)) > length (filter (<2) (snd llamada)) = "Normal" 
  | otherwise = "Reducido"

cuandoHabloMas llamada 
  | sum (fst llamada) > sum (snd llamada) = "Normal" 
  | otherwise = "Reducido"


-- cuandoHabloMas llamada 
--  |sum (fst llamada) > sum (snd llamada) = "normal" 
--  |otherwise = "reducido"

-}

f1:: Show a => a -> String
f1 x = "¡" ++ show x ++ "!"
