module Parcial where
import Text.Show.Functions()

data Perrito = Perrito {
    raza :: String,
    jueguetesFavs :: [String],
    tiempoEnGuarderiaEnMinutos :: Int,
    energia :: Int
} deriving Show

type Guarderia = ([Actividad], TiempoEnMinutos)
type Actividad = Perritos -> Perritos
type TiempoEnMinutos = Int

--Modelos
zara :: Perrito
zara = Perrito "Dalmata" ["Pelota", "Mantita"] 90 80

guarderiaPdePerritos :: Guarderia
guarderiaPdePerritos = ([jugar, ladrar 18, regalar "Pelota", diaDeSpa, diaDeCampo], 30 + 20 + 0 + 120 + 720)

--Funciones
jugar :: Actividad
jugar (unPerro, raza, juguetes, tiempo, energia) = unPerro raza juguetes tiempo (max 0(energia - 10))

ladrar :: Int -> Actividad
ladrar cantLadridos (unPerro, raza, juguetes, tiempo, energia) = unPerro raza juguetes tiempo (energia + (cantLadridos/2))

regalar :: String -> Actividad
regalar nuevoJuguete (unPerro, raza, juguetes, tiempo, energia) = nuevoJuguete : juguetes

diaDeSpa :: Actividad 
diaDeSpa (unPerro, raza, juguetes, tiempo, energia) 
    | tiempo >= 50 || razaExtravagante raza = energia == 100 && regalar "peineDeGoma"
    | otherwise unPerro raza juguetes tiempo energia

razaExtravagante :: String -> Bool
razaExtravagante raza = raza == "Dalmata" || raza == "Pomerania"

diaDeCampo :: Actividad
diaDeCampo  (unPerro, raza, juguetes, tiempo, energia) = unPerro raza (perderprimerJuguete juguetes) tiempo energia

perderPrimerJuguete :: [String] -> [String]
perderPrimerJuguete [] = []
perderPrimerJuguete (_ : resto) = resto