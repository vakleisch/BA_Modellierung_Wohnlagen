library("here")
library("foreign")
library("haven")
library("sf")

# Rohaten (infas 360) einlesen

data_raw <- read.csv(here("rohdaten/raw/I360_WOLAGE_DATA02042024.csv"), sep = ";")

# Räumliche Daten einlesen
new_mu_fl <- st_read(here("rohdaten/Datenabgabe_TU_LMU_2024_02_01/Miet_Flaeche_2023.shp"))
new_zent_b <- st_read(here("rohdaten/Datenabgabe_TU_LMU_2024_02_01/Zentraler_Bereich_2023.shp"))
new_miet_l <- st_read(here("rohdaten/Datenabgabe_TU_LMU_2024_02_01/Miet_Linie_2023.shp"))


# Umbenennen
wohnlagen_muc <- new_mu_fl
polygon_zentraler_bereich <- new_zent_b
wohnlage_grenzen <- new_miet_l
