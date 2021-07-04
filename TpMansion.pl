




%Resultado teniendo en cuenta que las personas SI se pueden odiar a si mismas

vivenEnMansionDreadbury(tiaAgatha).
vivenEnMansionDreadbury(mayordomo).
vivenEnMansionDreadbury(charles).

odiaA(tiaAgatha,charles).
odiaA(tiaAgatha,tiaAgatha).
odiaA(mayordomo,charles).
odiaA(mayordomo,tiaAgatha).
odiaA(charles,mayordomo).



esMasRicoQueAgatha(Alguien):-
    not(odiaA(mayordomo,Alguien)),
    vivenEnMansionDreadbury(Alguien),
    Alguien \= tiaAgatha.


    
mataATiaAgatha(Asesino):-
    odiaA(Asesino,tiaAgatha),
    not(esMasRicoQueAgatha(Asesino)),
    vivenEnMansionDreadbury(Asesino).

% 1b) Mostrar la consulta utilizada y la respuesta obtenida.
% mataATiaAgatha(Quien). 
% Devuelve: tiaAgatha.



% Parte 2

odiaAMillHouse(UnaPersona):-
odiaA(UnaPersona,milhouse).

/* 
Para "Si existe alguien que odie a milhouse."
Usamos odiaAMillHouse(_).
Retorna: false.

Para "A quién odia charles."
Usamos odiaA(charles,AQuien).
Retrona: AQuien = mayordomo ;
        

- El nombre de quien odia a tía Ágatha.
Usamos: odiaA(Quien,tiaAgatha).
Retorna:
        Quien = tiaAgatha ;
        Quien = mayordomo.


Para "Todos los odiadores y sus odiados."
Usamos: odiaA(Odiador,Odiado).
Retorna: Odiador = tiaAgatha,
    Odiado = charles ;
    Odiador = Odiado, Odiado = tiaAgatha ;
    Odiador = mayordomo,
    Odiado = charles ;
    Odiador = mayordomo,
    Odiado = tiaAgatha ;
    Odiador = charles,
    Odiado = mayordomo.

Para "Si es cierto que el mayordomo odia a alguien."      

Usamos: odiaA(mayordomo,_).
Retorna: true.
*/


