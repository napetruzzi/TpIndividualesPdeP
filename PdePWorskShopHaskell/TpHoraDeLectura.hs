
module Lib where


type Titulo = String
type Autor = String
type CantidadPaginas = Int
type Libro = (Titulo, Autor, CantidadPaginas)

elVisitante :: Libro
elVisitante = ("El Visitante","Stephen King",592)

shingekiNoKyojin1 :: Libro
shingekiNoKyojin1 = ("Shingeki no Kyojin 1","Hajime Isayama",40)

shingekiNoKyojin3 :: Libro
shingekiNoKyojin3 = ("Shingeki no Kyojin 3","Hajime Isayama",40)

shingekiNoKyojin127 :: Libro
shingekiNoKyojin127 = ("Shingeki no Kyojin 127","Hajime Isayama",40)

fundacion :: Libro
fundacion = ("Fundación","Isaac Asimov",230)

sandman5 ::Libro
sandman5 = ("Sandman 5","Neil Gaiman",35)

sandman10 ::Libro
sandman10 = ("Sandman 10","Neil Gaiman",35) 

sandman12 ::Libro
sandman12 = ("Sandman 12","Neil Gaiman",35)  

eragonEragon ::Libro
eragonEragon = ("Eragon: Eragon","Christopher Paolini",544)

eragonEldest ::Libro
eragonEldest  = ("Eragon: Eldest ","Christopher Paolini",704)

eragonBrisignr ::Libro
eragonBrisignr  = ("Eragon: Brisignr ","Christopher Paolini",700)

eragonLegado ::Libro
eragonLegado  = ("Eragon: Legado ","Christopher Paolini",811)


type Biblioteca = [Libro]

biblioteca :: Biblioteca
biblioteca = [elVisitante,shingekiNoKyojin1,shingekiNoKyojin3,shingekiNoKyojin127,fundacion,sandman5,sandman10,sandman12,eragonEragon,eragonEldest,eragonBrisignr,eragonLegado]



promedioDePaginas :: Biblioteca -> Int

promedioDePaginas unaBiblioteca =  div (cantPaginasTot unaBiblioteca) (length unaBiblioteca)

cantPaginasTot :: Biblioteca -> Int
cantPaginasTot unaBiblioteca =  sum.map cantidadDePaginasTot $ unaBiblioteca

cantidadDePaginasTot:: Libro -> Int
cantidadDePaginasTot (_,_,unasPaginas) = unasPaginas

lecturaObligatoria:: Biblioteca -> Bool
lecturaObligatoria unaBiblioteca = "Stephen King" `elem` (cadenaAutoresObligatorios unaBiblioteca) || "Christopher Paolini" `elem` (cadenaAutoresObligatorios unaBiblioteca) || "Isaac Asimov" `elem` (cadenaAutoresObligatorios unaBiblioteca) 

cadenaAutoresObligatorios :: Biblioteca -> String
cadenaAutoresObligatorios unaBiblioteca = concat.map obtenerAutorObligatorio $ unaBiblioteca

obtenerAutorObligatorio:: Libro -> String
obtenerAutorObligatorio (_,autorObligatorio,_) = autorObligatorio



bibliotecaLigera::Biblioteca -> Bool
bibliotecaLigera unaBiblioteca = 