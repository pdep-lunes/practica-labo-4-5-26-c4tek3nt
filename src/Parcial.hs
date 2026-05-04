module Parcial where
import Text.Show.Functions()

data Perrito = unPerro {
    raza :: String,
    jueguetesFavs :: [String],
    tiempoEnGuarderiaEnMinutos :: Int,
    energia :: Int
} deriving Show

type Guarderia = ([Actividad], TiempoEnMinutos)
type Actividad = Perrito -> Perrito
type TiempoEnMinutos = Int

--Modelos
zara :: Perrito
zara = unPerro "Dalmata" ["Pelota", "Mantita"] 90 80

guarderiaPdePerritos :: Guarderia
guarderiaPdePerritos = ([jugar, ladrar 18, regalar "Pelota", diaDeSpa, diaDeCampo], 30 + 20 + 0 + 120 + 720)

--Funciones
jugar :: Actividad
jugar (unPerro raza juguetes tiempo energia) = unPerro raza juguetes tiempo (max 0 (energia - 10))

ladrar :: Number -> Actividad
ladrar cantLadridos (unPerro raza juguetes tiempo energia) = unPerro raza juguetes tiempo (energia + (cantLadridos/2))

regalar :: String -> Actividad
regalar nuevoJuguete (unPerro raza juguetes tiempo energia) = unPerro raza (nuevoJuguete : juguetes) tiempo energia

diaDeSpa :: Actividad 
diaDeSpa (unPerro raza juguetes tiempo energia) 
    | tiempo >= 50 || razaExtravagante raza = regalar "peineDeGoma" (unPerro raza juguetes tiempo 100)

razaExtravagante :: String -> Bool
razaExtravagante raza = raza == "Dalmata" || raza == "Pomerania"

diaDeCampo :: Actividad
diaDeCampo  (unPerro raza juguetes tiempo energia) = unPerro raza (perderPrimerJuguete juguetes) tiempo energia

perderPrimerJuguete :: [String] -> [String]
perderPrimerJuguete [] = []
perderPrimerJuguete (_ : resto) = resto
