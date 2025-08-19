library(here)
source(here("daten", "data_read.R")) # Rohdatensätze 
library(dplyr)
library(sf)


# 1) Infas 360 Datensatz bearbeiten
data_raw[data_raw == -99] <- NA

data <- data_raw

# Kategoriale Variablen mit factor() umwandeln
data_colnames_categorial <- data_raw %>% 
  dplyr::select(plz5_kba_dichte_priv, ot_nachfrage_kl, sb_bdichte_kl, 
                sb_block_typ, casa_kompl, casa_hoehe_kl, casa_gebpretyp, 
                casa_garten_kl, casa_bj_kl, CASA_CSD_KL, casa_wfl_kl, casa_exkl,
                casa_soz_sch, casa_alter_kl, casa_poi_8, casa_poi_2, casa_poi_1,
                casa_ngew, casa_n, CASA_CSD_ID, casa_wolage,
                casa_wasser, casa_str_typ, casa_ortslage, casa_basistyp
  )

data_colnames_binary <- data %>% 
  dplyr::select(casa_garage, casa_poi_9, casa_poi_7, casa_poi_6, casa_poi_5, 
                casa_poi_4, casa_poi_3
  )

data_colnames_categorial <- colnames(data_colnames_categorial)
data_colnames_binary <- colnames(data_colnames_binary)


# Umwandlung
data[data_colnames_categorial] <- lapply(data[data_colnames_categorial], factor)
data[data_colnames_binary] <- lapply(data[data_colnames_binary], factor)

# Splaten einheitlich benennen
data<- data %>% 
  rename(casa_csd_kl = CASA_CSD_KL, 
         casa_csd_id = CASA_CSD_ID)


# 2) Räumlicher Datensatz von der Stadt München

# EBENE zu Wohnlage umbennen und Kategorien definieren
mapping <- data.frame(
  EBENE = 1:6,
  Wohnlage = c(
    "durchschnittliche Lage (außerhalb)",
    "gute Lage (außerhalb)",
    "beste Lage (außerhalb)",
    "zentrale durchschnittliche Lage",
    "zentrale gute Lage",
    "zentrale beste Lage"
  )
)

wohnlagen_muc2 <- wohnlagen_muc %>%
  left_join(mapping, by = "EBENE")

# Wohnlage anzeigen
head((wohnlagen_muc2[, c("geometry", "EBENE", "Wohnlage")]))
str(wohnlagen_muc2)

# Zu Typ Faktor konvertieren
wohnlagen_muc2$Wohnlage <- factor(wohnlagen_muc2$Wohnlage, levels = c(
  "durchschnittliche Lage (außerhalb)",
  "gute Lage (außerhalb)",
  "beste Lage (außerhalb)",
  "zentrale durchschnittliche Lage",
  "zentrale gute Lage",
  "zentrale beste Lage"
))

# Flächen berechnen
wohnlagen_muc2 <- wohnlagen_muc2 %>%
  mutate(flaeche_m2 = st_area(geometry))

# Summiere Fläche je Wohnlage + berechne prozentualen Anteil
wohnlage_flaeche <- wohnlagen_muc2 %>%
  group_by(Wohnlage) %>%
  summarise(gesamtflaeche = sum(flaeche_m2)) %>%
  mutate(anteil = as.numeric(gesamtflaeche) / sum(as.numeric(gesamtflaeche)) * 100)

# Für zentrale Lagen
wohnlage_flaeche_zentral <- wohnlage_flaeche %>%
  filter(Wohnlage %in% c("zentrale durchschnittliche Lage",
                         "zentrale gute Lage",
                         "zentrale beste Lage"))
wohnlage_flaeche_zentral <- wohnlage_flaeche_zentral %>%
  mutate(fläche_zentral = st_area(geometry))

wohnlage_flaeche_zentral <- wohnlage_flaeche_zentral %>%
  group_by(Wohnlage) %>%
  summarise(gesamtflaeche = sum(gesamtflaeche)) %>%
  mutate(anteil = as.numeric(gesamtflaeche) / sum(as.numeric(gesamtflaeche)) * 100)

# Für Lagen ausserhalb
wohnlage_flaeche_ausserhalb <- wohnlage_flaeche %>%
  filter(Wohnlage %in% c("durchschnittliche Lage (außerhalb)",
                         "gute Lage (außerhalb)",
                         "beste Lage (außerhalb)"))

wohnlage_flaeche_ausserhalb <- wohnlage_flaeche_ausserhalb %>%
  mutate(fläche_ausserhalb = st_area(geometry))
wohnlage_flaeche_ausserhalb <- wohnlage_flaeche_ausserhalb %>%
  group_by(Wohnlage) %>%
  summarise(gesamtflaeche = sum(gesamtflaeche)) %>%
  mutate(anteil = as.numeric(gesamtflaeche) / sum(as.numeric(gesamtflaeche)) * 100)


# 3) Beide Datensätze kombinieren

# Erstelle sf-Objekt aus data
data_sf <- st_as_sf(
  data,
  coords = c("ip_wgs84long", "ip_wgs84lat"),
  crs = 4326  # WGS84
)

# Gleiche crs sicherstellen
data_sf <- st_transform(data_sf, st_crs(wohnlagen_muc2))

# wohnlage zuordnen
data_with_wohnlage <- st_join(data_sf, wohnlagen_muc2[, c("Wohnlage")], join = st_within)

# Wohnungen ohne Wohnlage 
data_without_wohnlage <- data_with_wohnlage %>%
  filter(is.na(Wohnlage))

# Wohnungen mit Wohnlage
data_with_classified_wohnlage <- data_with_wohnlage %>%
  filter(!is.na(Wohnlage))

# Bei sf Datensatz auch NAs entfernen 
data_with_wohnlage <- data_with_wohnlage %>%
  filter(!is.na(Wohnlage))

data_final <- data_with_wohnlage


sum(is.na(data_final$Wohnlage)) # 25 237 Wohnungen können nicht klassifiziert werden

# NA in Wohnlage entfernen
data_final <- data_final %>%
  filter(!is.na(Wohnlage))

# NAs im ganzen Datensatz zählen
na_counts <- sapply(data_final, function(x) sum(is.na(x)))
na_counts

# Variablen mit vielen NAs entfernen; werden in Analyse gar nicht berücksichtigt
# Für die Wohnlage unrelevante Variablen entfernen
# Variablen umbennen
data_final_short <- data_final %>%
  filter(ort == "M\xfcnchen") %>%
  select(-c(casa_ngew, casa_poi_1, casa_poi_2, casa_poi_3, casa_poi_4, casa_poi_5, 
            casa_poi_6, casa_poi_7, casa_poi_8, casa_poi_9, casa_garage,
            hnr, hnrz, 
            stn,  # Straßenname, unpassend für Modell
            ags2, ags5, ags8,
            ags11, # ags11 = Ortsteil
            ags13, # ags13 = Gemeinde/ Postleitzahl
            ags16, # ags16 = Plz scharfe Ortsteile 
            ags20, # ags20 = Siedlungsblock 
            ags22, # ags22 = Straßenblockseite
            ort,
            casa_csd_id,
            sb_ausl_euro27_anz, sb_ausl_welt_anz, 
            sb_ew_ausl_ant, sb_ew_ausl_anz)) %>% 
  rename(building_id = ags24, 
         adress = ags27, 
         ortsteil = ot1,
         anzahl_einwohner = casa_ew,
         distanz_mittelzentrum = casa_mz_dist,
         distanz_oberzentrum = casa_oz_dist,
         distanz_unterzentrum = casa_uz_dist,
         distanz_bahnhof = casa_dist_bhf,
         distanz_bushaltestelle = casa_dist_bush,
         distanz_ubahn = casa_dist_ustrab,
         distanz_opnv = casa_dist_opnv, 
         pkw_dichte = plz5_kba_dichte_priv, 
         nahversorgungs_index = casa_nvi,
         ortslage_klasse = casa_ortslage,
         straßentyp = casa_str_typ,
         nähe_wasser_klasse = casa_wasser,
         gebäudenutzung = casa_n,
         opnv_index = casa_opnv_idx,
         alterstruktur_klasse = casa_alter_kl, 
         soziale_schicht = casa_soz_sch,
         exklusivität_klasse = casa_exkl,
         wohnfläche_klasse = casa_wfl_kl,
         hauspreis_index = casa_wh_preis_index,
         shopping_distrikt_klasse = casa_csd_kl,
         garten_klasse = casa_garten_kl,
         gebäudetyp = casa_gebpretyp,
         grundfläche = casa_grundfl,
         gebäudehöhe_klasse = casa_hoehe_kl,
         gestaltung_haus_klasse = casa_kompl,
         baublocktyp = sb_block_typ,
         baudichte = sb_bdichte_kl,
         anzahl_einwohner_siedlungsblock = sb_ew_anz,
         kaufspiegel = sb_kauf,
         mietspiegel = sb_miet,
         kaufkraft_haushalt = ot_kk_hh,
         fläche_ortsteil = ot_flaeche,
         arbeitslosenquote = ot_alo_q,
         nachfrage_klasse = ot_nachfrage_kl) 


# Variablen des kombinierten Datensatzes überarbeiten

# straßentyp
# straßentyp als Faktor mit Labels definieren
straßentyp_chr <- as.character(data_final_short$straßentyp)
data_final_short$straßentyp <- factor(
  ifelse(is.na(straßentyp_chr), NA, straßentyp_chr),
  levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"),
  labels = c(
    "Bundesstraße",
    "Landstraße",
    "Kreisstraße",
    "Hauptverkehrsstraße",
    "Sammelstraße",
    "Anliegerstraße / Wohnstraße",
    "verkehrsberuhigter Bereich",
    "Fußgängerzone",
    "Fußweg"
  )
)

# Straßentyp zusammenfassen
data_final_short <- data_final_short %>%
  mutate(
    straßentyp_gruppe = case_when(
      straßentyp %in% c("Bundesstraße", "Kreisstraße", "Hauptverkehrsstraße") ~ "Hauptstraße",
      straßentyp == "Sammelstraße" ~ "Sammelstraße",
      straßentyp %in% c("Anliegerstraße / Wohnstraße", "verkehrsberuhigter Bereich") ~ "Wohnstraße",
      straßentyp %in% c("Fußgängerzone", "Fußweg") ~ "Fußgängerbereich",
      TRUE ~ NA_character_
    ),
    straßentyp_gruppe = factor(
      straßentyp_gruppe,
      levels = c("Hauptstraße", "Sammelstraße", "Wohnstraße", "Fußgängerbereich")
    )
  )


# Verteilung der neuen Straßentypen 
table(data_final_short$straßentyp_gruppe, useNA = "always") 

# Weitere kategorielle Variablen zu Faktoren umwandeln
data_final_short <- data_final_short %>% 
  select(-c(casa_bj_kl)) %>% 
  mutate(plz = factor(plz),
         ortsteil = factor(ortsteil),
         building_id = factor(building_id),
         adress = factor(adress))

# Nur private Gebäudenutzung relevant: Datensatz filtern
data_final_short <- data_final_short %>%
  filter(gebäudenutzung == "1") 


# data_final_short filtern - nach zentralen Lagen und Lagen außerhalb
data_final_short_zentral <- data_final_short %>%
  filter(Wohnlage %in% c("zentrale durchschnittliche Lage", 
                         "zentrale gute Lage", 
                         "zentrale beste Lage"))
data_final_short_ausserhalb <- data_final_short %>%
  filter(Wohnlage %in% c("durchschnittliche Lage (außerhalb)", 
                         "gute Lage (außerhalb)", 
                         "beste Lage (außerhalb)"))


# model_data: finaler Datensatz für die Modellierung erstellen

# Daten für die Modelle
# Zielvariable Wohnlage GAM-konform encoden
model_data <- data_final_short
model_data$Wohnlage_numerisch <- as.numeric(model_data$Wohnlage) - 1

# Nur relevante Variaben drinnen lassen 
model_data_f <- model_data %>% 
  select(distanz_bahnhof,
         distanz_mittelzentrum,
         opnv_index,
         distanz_unterzentrum,
         hauspreis_index,
         straßentyp_gruppe,
         distanz_ubahn,
         distanz_bushaltestelle,
         nahversorgungs_index,
         Wohnlage_numerisch, 
         Wohnlage, 
         geometry)
model_data_complete <- na.omit(model_data_f) # Zeilen mit NA entfernen

# Mit straßentyp (zur Visualisierung)
model_data_mit_stra <- model_data %>% 
  select(distanz_bahnhof,
         distanz_mittelzentrum,
         opnv_index,
         distanz_unterzentrum,
         hauspreis_index,
         straßentyp_gruppe,
         straßentyp,
         distanz_ubahn,
         distanz_bushaltestelle,
         nahversorgungs_index,
         Wohnlage_numerisch, 
         Wohnlage, 
         geometry)
model_data_mit_stra_complete <- na.omit(model_data_mit_stra) # Zeilen mit NA entfernen

# Nach zentral und nicht-zentral unterscheiden
# zentral (filter for Wohnlage = 3, 4, 5)
model_data_zentral <- model_data_complete %>%
  filter(Wohnlage_numerisch %in% c(3, 4, 5)) %>%
  mutate(Wohnlage_numerisch = Wohnlage_numerisch - 3) # Encoding anpassen
model_data_zentral_complete <- na.omit(model_data_zentral) # Zeilen mit NA entfernen

# außerhalb (filter for Wohnlage = 0, 1, 2)
model_data_ausserhalb <- model_data_complete %>%
  filter(Wohnlage_numerisch %in% c(0, 1, 2))
model_data_ausserhalb_complete <- na.omit(model_data_ausserhalb) # Zeilen mit NA entfernen

# zentral /nicht zentral ursprünglicher Straßentyp (zur Visualisierung)
model_data_mit_stra_zentral <- model_data_mit_stra %>%
  filter(Wohnlage_numerisch %in% c(3, 4, 5)) %>%
  mutate(Wohnlage_numerisch = Wohnlage_numerisch - 3) # Encoding anpassen
model_data_mit_stra_zentral_complete <- na.omit(model_data_mit_stra_zentral) # Zeilen mit NA entfernen

# Gleiches für außerhalb (zur Visualisierung)
model_data_ausserhalb_mit_stra <- model_data_mit_stra %>%
  filter(Wohnlage_numerisch %in% c(0, 1, 2))
model_data_ausserhalb_mit_stra_complete <- na.omit(model_data_ausserhalb_mit_stra) # Zeilen mit NA entfernen
