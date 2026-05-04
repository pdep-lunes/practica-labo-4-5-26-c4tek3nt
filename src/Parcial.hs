module Parcial where
import Text.Show.Functions()

data Perrito = UnPerro {
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
zara = UnPerro "Dalmata" ["Pelota", "Mantita"] 90 80

guarderiaPdePerritos :: Guarderia
guarderiaPdePerritos = ([jugar, ladrar 18, regalar "Pelota", diaDeSpa, diaDeCampo], 30 + 20 + 0 + 120 + 720)

--Funciones
jugar :: Actividad
jugar perro = perro { energia = max 0 (energia perro - 10) }

ladrar :: Int -> Actividad
ladrar cantLadridos perro = perro { energia = energia + (cantLadridos `div` 2) }

regalar :: String -> Actividad
regalar nuevoJuguete perro = perro { juguetesFavs = (nuevoJuguete : juguetesFavs)}

diaDeSpa :: Actividad 
diaDeSpa (UnPerro raza juguetes tiempo energia) 
    | tiempo >= 50 || razaExtravagante raza = regalar "peineDeGoma" (UnPerro raza juguetes tiempo 100)
    | otherwise = UnPerro raza juguetes tiempo energia

razaExtravagante :: String -> Bool
razaExtravagante raza = raza == "Dalmata" || raza == "Pomerania"

diaDeCampo :: Actividad
diaDeCampo  (UnPerro raza juguetes tiempo energia) = UnPerro raza (perderPrimerJuguete juguetes) tiempo energia

perderPrimerJuguete :: [String] -> [String]
perderPrimerJuguete [] = []
perderPrimerJuguete (_ : resto) = resto

puedeEstarEnGuarderia :: Perrito -> Guarderia -> Bool
puedeEstarEnGuarderia (UnPerro _ _ tiempo _) (_, tiempoRutina) = tiempo > tiempoRutina

perrosResponsables :: Perrito -> Bool
perrosResponsables perro = length (jueguetesFavs (diaDeCampo perro)) > 3
