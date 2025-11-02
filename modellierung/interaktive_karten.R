library(rgl)
library(here)
library(ggplot2)
library(sf)
library(patchwork)
library(dplyr)
library(leaflet)
library(osmdata)
library(htmlwidgets)
library(tmaptools)
source(here("modellierung", "model_evaluation.R"))
source(here("daten", "data_edit.R"))

# Modelle einlesen
model_gam_zentral <- readRDS("modelle/gam_model_zentral.rds")
model_gam_ausserhalb <- readRDS("modelle/gam_model_ausserhalb.rds")

model_gam_zentral_ohne_preis <- readRDS("modelle/gam_model_zentral_ohne_preis.rds")
model_gam_ausserhalb_ohne_preis <- readRDS("modelle/gam_model_ausserhalb_ohne_preis.rds")

model_gam_zentral_ohne <- readRDS("modelle/gam_model_zentral_ohne.rds")
model_gam_ausserhalb_ohne <- readRDS("modelle/gam_model_ausserhalb_ohne.rds")

model_gam_zentral_ohne_preis_bahn <- readRDS("modelle/gam_model_zentral_ohne_preis_bahn.rds")
model_gam_ausserhalb_ohne_preis_bahn <- readRDS("modelle/gam_model_ausserhalb_ohne_preis_bahn.rds")

# Daten laden
load("daten/model_data_zentral_complete.RData")
load("daten/model_data_ausserhalb_complete.RData")
load("daten/model_data_complete.RData")

# Farbpalette für Wohnlagen
wohnlage_farben <- c(
  "durchschnittliche Lage (außerhalb)" = "#e8f5a4", # ursprünglich: #FFFFCC
  "gute Lage (außerhalb)" = "#afe391", # ursprünglich: #7FCDBB
  "beste Lage (außerhalb)" = "#7FCDBB",
  "zentrale durchschnittliche Lage" = "#41B6C4",
  "zentrale gute Lage" = "#1f5a82",
  "zentrale beste Lage" = "#271352"
)



# Lagen der Stadt
wohnlagen_muc_zentral <- wohnlagen_muc2 %>%
  filter(Wohnlage %in% c("zentrale durchschnittliche Lage",
                         "zentrale gute Lage",
                         "zentrale beste Lage"))
wohnlagen_muc_ausserhalb <- wohnlagen_muc2 %>%
  filter(Wohnlage %in% c("durchschnittliche Lage (außerhalb)",
                         "gute Lage (außerhalb)",
                         "beste Lage (außerhalb)"))

# Wohnlagegrenzen für zentral und nicht zentral
# Finde alle Linien, die innerhalb zentraler Wohnlagen liegen
linien_in_zentral <- st_intersects(wohnlage_grenzen, wohnlagen_muc_zentral, sparse = FALSE)

# Nur die Linien, die je passen
wohnlage_grenzen_ausserhalb <- wohnlage_grenzen[!apply(linien_in_zentral, 1, any), ]
wohnlage_grenzen_zentral <- wohnlage_grenzen[apply(linien_in_zentral, 1, any), ]


# Daten erstellen
fehler_model_gam_zentral <- missclassification_data_zentral(model_gam_zentral, 
                                                            data = model_data_zentral_complete,
                                                            predict_fun = predict_labels_discr)
fehler_model_gam_ausserhalb <- missclassification_data_ausserhalb(model_gam_ausserhalb, 
                                                                  data = model_data_ausserhalb_complete,
                                                                  predict_fun = predict_labels_discr)
fehler_model_gam_zentral_ohne_preis <- missclassification_data_zentral(model_gam_zentral_ohne_preis, 
                                                                       data = model_data_zentral_complete,
                                                                       predict_fun = predict_labels_discr)
fehler_model_gam_ausserhalb_ohne_preis <- missclassification_data_ausserhalb(model_gam_ausserhalb_ohne_preis, 
                                                                             data = model_data_ausserhalb_complete,
                                                                             predict_fun = predict_labels_discr)
fehler_model_gam_zentral_ohne <- missclassification_data_zentral(model_gam_zentral_ohne, 
                                                                       data = model_data_zentral_complete,
                                                                       predict_fun = predict_labels_discr)
fehler_model_gam_ausserhalb_ohne <- missclassification_data_ausserhalb(model_gam_ausserhalb_ohne, 
                                                                             data = model_data_ausserhalb_complete,
                                                                             predict_fun = predict_labels_discr)
fehler_model_gam_zentral_ohne_preis_bahn <- missclassification_data_zentral(model_gam_zentral_ohne_preis_bahn, 
                                                                       data = model_data_zentral_complete,
                                                                       predict_fun = predict_labels_discr)
fehler_model_gam_ausserhalb_ohne_preis_bahn <- missclassification_data_ausserhalb(model_gam_ausserhalb_ohne_preis_bahn, 
                                                                             data = model_data_ausserhalb_complete,
                                                                             predict_fun = predict_labels_discr)


korrekt_model_gam_zentral <- korrekte_vorhersagen_zentral(model_gam_zentral, 
                                                          data = model_data_zentral_complete,
                                                          predict_fun = predict_labels_discr)
korrekt_model_gam_ausserhalb <- korrekte_vorhersagen_ausserhalb(model_gam_ausserhalb, 
                                                                data = model_data_ausserhalb_complete,
                                                                predict_fun = predict_labels_discr)
korrekt_model_gam_zentral_ohne_preis <- korrekte_vorhersagen_zentral(model_gam_zentral_ohne_preis, 
                                                                     data = model_data_zentral_complete,
                                                                     predict_fun = predict_labels_discr)
korrekt_model_gam_ausserhalb_ohne_preis <- korrekte_vorhersagen_ausserhalb(model_gam_ausserhalb_ohne_preis, 
                                                                           data = model_data_ausserhalb_complete,
                                                                           predict_fun = predict_labels_discr)
korrekt_model_gam_zentral_ohne <- korrekte_vorhersagen_zentral(model_gam_zentral_ohne, 
                                                                     data = model_data_zentral_complete,
                                                                     predict_fun = predict_labels_discr)
korrekt_model_gam_ausserhalb_ohne <- korrekte_vorhersagen_ausserhalb(model_gam_ausserhalb_ohne, 
                                                                           data = model_data_ausserhalb_complete,
                                                                           predict_fun = predict_labels_discr)
korrekt_model_gam_zentral_ohne_preis_bahn <- korrekte_vorhersagen_zentral(model_gam_zentral_ohne_preis_bahn, 
                                                                     data = model_data_zentral_complete,
                                                                     predict_fun = predict_labels_discr)
korrekt_model_gam_ausserhalb_ohne_preis_bahn <- korrekte_vorhersagen_ausserhalb(model_gam_ausserhalb_ohne_preis_bahn, 
                                                                           data = model_data_ausserhalb_complete,
                                                                           predict_fun = predict_labels_discr)


# mit gleicher priori
fehler_model_gam_zentral_prior <- missclassification_data_zentral(model_gam_zentral, 
                                                                  data = model_data_zentral_complete,
                                                                  predict_fun = predict_labels_equal_priors)
fehler_model_gam_ausserhalb_prior <- missclassification_data_ausserhalb(model_gam_ausserhalb, 
                                                                        data = model_data_ausserhalb_complete,
                                                                        predict_fun = predict_labels_equal_priors)
korrekt_model_gam_zentral_prior <- korrekte_vorhersagen_zentral(model_gam_zentral, 
                                                                data = model_data_zentral_complete,
                                                                predict_fun = predict_labels_equal_priors)
korrekt_model_gam_ausserhalb_prior <- korrekte_vorhersagen_ausserhalb(model_gam_ausserhalb, 
                                                                      data = model_data_ausserhalb_complete,
                                                                      predict_fun = predict_labels_equal_priors)


# in sf Objekte umwandeln
fehler_model_gam_zentral <- st_as_sf(fehler_model_gam_zentral)
fehler_model_gam_zentral <- fehler_model_gam_zentral %>%
  filter(Wohnlage %in% c("zentrale durchschnittliche Lage",
                         "zentrale gute Lage",
                         "zentrale beste Lage"))
fehler_model_gam_zentral_prior <- st_as_sf(fehler_model_gam_zentral_prior)
fehler_model_gam_zentral_prior <- fehler_model_gam_zentral_prior %>%
  filter(Wohnlage %in% c("zentrale durchschnittliche Lage",
                         "zentrale gute Lage",
                         "zentrale beste Lage"))
fehler_model_gam_ausserhalb <- st_as_sf(fehler_model_gam_ausserhalb)
fehler_model_gam_ausserhalb <- fehler_model_gam_ausserhalb %>%
  filter(Wohnlage %in% c("durchschnittliche Lage (außerhalb)",
                         "gute Lage (außerhalb)",
                         "beste Lage (außerhalb)"))
fehler_model_gam_ausserhalb_prior <- st_as_sf(fehler_model_gam_ausserhalb_prior)
fehler_model_gam_ausserhalb_prior <- fehler_model_gam_ausserhalb_prior %>%
  filter(Wohnlage %in% c("durchschnittliche Lage (außerhalb)",
                         "gute Lage (außerhalb)",
                         "beste Lage (außerhalb)"))
korrekt_model_gam_zentral <- st_as_sf(korrekt_model_gam_zentral)
korrekt_model_gam_zentral <- korrekt_model_gam_zentral %>%
  filter(Wohnlage %in% c("zentrale durchschnittliche Lage",
                         "zentrale gute Lage",
                         "zentrale beste Lage"))
korrekt_model_gam_ausserhalb <- st_as_sf(korrekt_model_gam_ausserhalb)
korrekt_model_gam_ausserhalb <- korrekt_model_gam_ausserhalb %>%
  filter(Wohnlage %in% c("durchschnittliche Lage (außerhalb)",
                         "gute Lage (außerhalb)",
                         "beste Lage (außerhalb)"))
korrekt_model_gam_zentral_prior <- st_as_sf(korrekt_model_gam_zentral_prior)
korrekt_model_gam_zentral_prior <- korrekt_model_gam_zentral_prior %>%
  filter(Wohnlage %in% c("zentrale durchschnittliche Lage",
                         "zentrale gute Lage",
                         "zentrale beste Lage"))
korrekt_model_gam_ausserhalb_prior <- st_as_sf(korrekt_model_gam_ausserhalb_prior)
korrekt_model_gam_ausserhalb_prior <- korrekt_model_gam_ausserhalb_prior %>%
  filter(Wohnlage %in% c("durchschnittliche Lage (außerhalb)",
                         "gute Lage (außerhalb)",
                         "beste Lage (außerhalb)"))
# Ohne hauspreis_index-----------------------------------------------------------
fehler_model_gam_zentral_ohne_preis <- st_as_sf(fehler_model_gam_zentral_ohne_preis)
fehler_model_gam_zentral_ohne_preis <- fehler_model_gam_zentral_ohne_preis %>%
  filter(Wohnlage %in% c("zentrale durchschnittliche Lage",
                         "zentrale gute Lage",
                         "zentrale beste Lage"))
fehler_model_gam_ausserhalb_ohne_preis <- st_as_sf(fehler_model_gam_ausserhalb_ohne_preis)
fehler_model_gam_ausserhalb_ohne_preis <- fehler_model_gam_ausserhalb_ohne_preis %>%
  filter(Wohnlage %in% c("durchschnittliche Lage (außerhalb)",
                         "gute Lage (außerhalb)",
                         "beste Lage (außerhalb)"))
korrekt_model_gam_zentral_ohne_preis <- st_as_sf(korrekt_model_gam_zentral_ohne_preis)
korrekt_model_gam_zentral_ohne_preis <- korrekt_model_gam_zentral_ohne_preis %>%
  filter(Wohnlage %in% c("zentrale durchschnittliche Lage",
                         "zentrale gute Lage",
                         "zentrale beste Lage"))
korrekt_model_gam_ausserhalb_ohne_preis <- st_as_sf(korrekt_model_gam_ausserhalb_ohne_preis)
korrekt_model_gam_ausserhalb_ohne_preis <- korrekt_model_gam_ausserhalb_ohne_preis %>%
  filter(Wohnlage %in% c("durchschnittliche Lage (außerhalb)",
                         "gute Lage (außerhalb)",
                         "beste Lage (außerhalb)"))

#  Ohne distanz_bahnhof-----------------------------------------------------------
fehler_model_gam_zentral_ohne <- st_as_sf(fehler_model_gam_zentral_ohne)
fehler_model_gam_zentral_ohne <- fehler_model_gam_zentral_ohne %>%
  filter(Wohnlage %in% c("zentrale durchschnittliche Lage",
                         "zentrale gute Lage",
                         "zentrale beste Lage"))
fehler_model_gam_ausserhalb_ohne <- st_as_sf(fehler_model_gam_ausserhalb_ohne)
fehler_model_gam_ausserhalb_ohne <- fehler_model_gam_ausserhalb_ohne %>%
  filter(Wohnlage %in% c("durchschnittliche Lage (außerhalb)",
                         "gute Lage (außerhalb)",
                         "beste Lage (außerhalb)"))
korrekt_model_gam_zentral_ohne <- st_as_sf(korrekt_model_gam_zentral_ohne)
korrekt_model_gam_zentral_ohne <- korrekt_model_gam_zentral_ohne %>%
  filter(Wohnlage %in% c("zentrale durchschnittliche Lage",
                         "zentrale gute Lage",
                         "zentrale beste Lage"))
korrekt_model_gam_ausserhalb_ohne <- st_as_sf(korrekt_model_gam_ausserhalb_ohne)
korrekt_model_gam_ausserhalb_ohne <- korrekt_model_gam_ausserhalb_ohne %>%
  filter(Wohnlage %in% c("durchschnittliche Lage (außerhalb)",
                         "gute Lage (außerhalb)",
                         "beste Lage (außerhalb)"))

#  Ohne hauspreis_index & distanz_bahnhof---------------------------------------
fehler_model_gam_zentral_ohne_preis_bahn <- st_as_sf(fehler_model_gam_zentral_ohne_preis_bahn)
fehler_model_gam_zentral_ohne_preis_bahn <- fehler_model_gam_zentral_ohne_preis_bahn %>%
  filter(Wohnlage %in% c("zentrale durchschnittliche Lage",
                         "zentrale gute Lage",
                         "zentrale beste Lage"))
fehler_model_gam_ausserhalb_ohne_preis_bahn <- st_as_sf(fehler_model_gam_ausserhalb_ohne_preis_bahn)
fehler_model_gam_ausserhalb_ohne_preis_bahn <- fehler_model_gam_ausserhalb_ohne_preis_bahn %>%
  filter(Wohnlage %in% c("durchschnittliche Lage (außerhalb)",
                         "gute Lage (außerhalb)",
                         "beste Lage (außerhalb)"))
korrekt_model_gam_zentral_ohne_preis_bahn <- st_as_sf(korrekt_model_gam_zentral_ohne_preis_bahn)
korrekt_model_gam_zentral_ohne_preis_bahn <- korrekt_model_gam_zentral_ohne_preis_bahn %>%
  filter(Wohnlage %in% c("zentrale durchschnittliche Lage",
                         "zentrale gute Lage",
                         "zentrale beste Lage"))
korrekt_model_gam_ausserhalb_ohne_preis_bahn <- st_as_sf(korrekt_model_gam_ausserhalb_ohne_preis_bahn)
korrekt_model_gam_ausserhalb_ohne_preis_bahn <- korrekt_model_gam_ausserhalb_ohne_preis_bahn %>%
  filter(Wohnlage %in% c("durchschnittliche Lage (außerhalb)",
                         "gute Lage (außerhalb)",
                         "beste Lage (außerhalb)"))


model_data_complete_sf <- st_as_sf(model_data_complete)
model_data_zentral_complete_sf <- st_as_sf(model_data_zentral_complete)

# Hilfsfunktion zum Bereinigen von sf-Objekten
prepare_sf_object <- function(sf_obj) {
  sf_obj %>%
    st_zm(drop = TRUE, what = "ZM") %>%     # Z- und M-Dimensionen entfernen
    st_make_valid()                         # ungültige Geometrien reparieren
}

# Wende es auf alle sf-Objekte an
wohnlagen_muc2 <- prepare_sf_object(wohnlagen_muc2)
wohnlagen_muc_zentral <- prepare_sf_object(wohnlagen_muc_zentral)
wohnlagen_muc_ausserhalb <- prepare_sf_object(wohnlagen_muc_ausserhalb)

model_data_complete <- prepare_sf_object(model_data_complete)
model_data_complete_sf <- prepare_sf_object(model_data_complete_sf)
model_data_zentral_complete <- prepare_sf_object(model_data_zentral_complete)
model_data_zentral_complete_sf <- prepare_sf_object(model_data_zentral_complete_sf)
model_data_ausserhalb_complete <- prepare_sf_object(model_data_ausserhalb_complete)

fehler_model_gam_zentral <- prepare_sf_object(fehler_model_gam_zentral)
fehler_model_gam_ausserhalb <- prepare_sf_object(fehler_model_gam_ausserhalb)
fehler_model_gam_zentral_prior <- prepare_sf_object(fehler_model_gam_zentral_prior)
fehler_model_gam_ausserhalb_prior <- prepare_sf_object(fehler_model_gam_ausserhalb_prior)
fehler_model_gam_zentral_ohne_preis <- prepare_sf_object(fehler_model_gam_zentral_ohne_preis)
fehler_model_gam_ausserhalb_ohne_preis <- prepare_sf_object(fehler_model_gam_ausserhalb_ohne_preis)
fehler_model_gam_zentral_ohne <- prepare_sf_object(fehler_model_gam_zentral_ohne)
fehler_model_gam_ausserhalb_ohne <- prepare_sf_object(fehler_model_gam_ausserhalb_ohne)
fehler_model_gam_zentral_ohne_preis_bahn <- prepare_sf_object(fehler_model_gam_zentral_ohne_preis_bahn)
fehler_model_gam_ausserhalb_ohne_preis_bahn <- prepare_sf_object(fehler_model_gam_ausserhalb_ohne_preis_bahn)


korrekt_model_gam_zentral <- prepare_sf_object(korrekt_model_gam_zentral)
korrekt_model_gam_ausserhalb <- prepare_sf_object(korrekt_model_gam_ausserhalb)
korrekt_model_gam_zentral_prior <- prepare_sf_object(korrekt_model_gam_zentral_prior)
korrekt_model_gam_ausserhalb_prior <- prepare_sf_object(korrekt_model_gam_ausserhalb_prior)
korrekt_model_gam_zentral_ohne_preis <- prepare_sf_object(korrekt_model_gam_zentral_ohne_preis)
korrekt_model_gam_ausserhalb_ohne_preis <- prepare_sf_object(korrekt_model_gam_ausserhalb_ohne_preis)
korrekt_model_gam_zentral_ohne <- prepare_sf_object(korrekt_model_gam_zentral_ohne)
korrekt_model_gam_ausserhalb_ohne <- prepare_sf_object(korrekt_model_gam_ausserhalb_ohne)
korrekt_model_gam_zentral_ohne_preis_bahn <- prepare_sf_object(korrekt_model_gam_zentral_ohne_preis_bahn)
korrekt_model_gam_ausserhalb_ohne_preis_bahn <- prepare_sf_object(korrekt_model_gam_ausserhalb_ohne_preis_bahn)



wohnlage_grenzen <- prepare_sf_object(wohnlage_grenzen)
wohnlage_grenzen_zentral <- prepare_sf_object(wohnlage_grenzen_zentral)
wohnlage_grenzen_ausserhalb <- prepare_sf_object(wohnlage_grenzen_ausserhalb)


# Interaktive Karten 

wohnlage_farben <- c(
  "durchschnittliche Lage (außerhalb)" = "#e8f5a4", # ursprünglich: #FFFFCC
  "gute Lage (außerhalb)" = "#afe391", # ursprünglich: #7FCDBB
  "beste Lage (außerhalb)" = "#7FCDBB",
  "zentrale durchschnittliche Lage" = "#41B6C4",
  "zentrale gute Lage" = "#1f5a82",
  "zentrale beste Lage" = "#271352"
)


# Datenvorverarbeitung
# Beide fehler datensätze kombinieren:
# Gleiche Levels für beide Spalten vor dem Zusammenfügen
levels_kombiniert <- c(
  "durchschnittliche Lage (außerhalb)",
  "gute Lage (außerhalb)",
  "beste Lage (außerhalb)",
  "zentrale durchschnittliche Lage",
  "zentrale gute Lage",
  "zentrale beste Lage"
)

# Setze beide Faktoren auf denselben Level-Satz
# normal
fehler_model_gam_zentral$Wohnlage_vorhersage <- factor(
  fehler_model_gam_zentral$Wohnlage_vorhersage,
  levels = levels_kombiniert)
fehler_model_gam_zentral$Wohnlage_wahr <- factor(
  fehler_model_gam_zentral$Wohnlage_wahr,
  levels = levels_kombiniert)
fehler_model_gam_ausserhalb$Wohnlage_vorhersage <- factor(
  fehler_model_gam_ausserhalb$Wohnlage_vorhersage,
  levels = levels_kombiniert)
fehler_model_gam_ausserhalb$Wohnlage_wahr <- factor(
  fehler_model_gam_ausserhalb$Wohnlage_wahr,
  levels = levels_kombiniert)

fehler_model_gam_zentral_ohne_preis$Wohnlage_vorhersage <- factor(
  fehler_model_gam_zentral_ohne_preis$Wohnlage_vorhersage,
  levels = levels_kombiniert)
fehler_model_gam_zentral_ohne_preis$Wohnlage_wahr <- factor(
  fehler_model_gam_zentral_ohne_preis$Wohnlage_wahr,
  levels = levels_kombiniert)
fehler_model_gam_ausserhalb_ohne_preis$Wohnlage_vorhersage <- factor(
  fehler_model_gam_ausserhalb_ohne_preis$Wohnlage_vorhersage,
  levels = levels_kombiniert)
fehler_model_gam_ausserhalb_ohne_preis$Wohnlage_wahr <- factor(
  fehler_model_gam_ausserhalb_ohne_preis$Wohnlage_wahr,
  levels = levels_kombiniert)

fehler_model_gam_zentral_ohne$Wohnlage_vorhersage <- factor(
  fehler_model_gam_zentral_ohne$Wohnlage_vorhersage,
  levels = levels_kombiniert)
fehler_model_gam_zentral_ohne$Wohnlage_wahr <- factor(
  fehler_model_gam_zentral_ohne$Wohnlage_wahr,
  levels = levels_kombiniert)
fehler_model_gam_ausserhalb_ohne$Wohnlage_vorhersage <- factor(
  fehler_model_gam_ausserhalb_ohne$Wohnlage_vorhersage,
  levels = levels_kombiniert)
fehler_model_gam_ausserhalb_ohne$Wohnlage_wahr <- factor(
  fehler_model_gam_ausserhalb_ohne$Wohnlage_wahr,
  levels = levels_kombiniert)

fehler_model_gam_zentral_ohne_preis_bahn$Wohnlage_vorhersage <- factor(
  fehler_model_gam_zentral_ohne_preis_bahn$Wohnlage_vorhersage,
  levels = levels_kombiniert)
fehler_model_gam_zentral_ohne_preis_bahn$Wohnlage_wahr <- factor(
  fehler_model_gam_zentral_ohne_preis_bahn$Wohnlage_wahr,
  levels = levels_kombiniert)
fehler_model_gam_ausserhalb_ohne_preis_bahn$Wohnlage_vorhersage <- factor(
  fehler_model_gam_ausserhalb_ohne_preis_bahn$Wohnlage_vorhersage,
  levels = levels_kombiniert)
fehler_model_gam_ausserhalb_ohne_preis_bahn$Wohnlage_wahr <- factor(
  fehler_model_gam_ausserhalb_ohne_preis_bahn$Wohnlage_wahr,
  levels = levels_kombiniert)

korrekt_model_gam_zentral$Wohnlage_vorhersage <- factor(
  korrekt_model_gam_zentral$Wohnlage_vorhersage,
  levels = levels_kombiniert)
korrekt_model_gam_ausserhalb$Wohnlage_vorhersage <- factor(
  korrekt_model_gam_ausserhalb$Wohnlage_vorhersage,
  levels = levels_kombiniert)
korrekt_model_gam_zentral$Wohnlage_wahr <- factor(
  korrekt_model_gam_zentral$Wohnlage_wahr,
  levels = levels_kombiniert)
korrekt_model_gam_ausserhalb$Wohnlage_wahr <- factor(
  korrekt_model_gam_ausserhalb$Wohnlage_wahr,
  levels = levels_kombiniert)

korrekt_model_gam_zentral_ohne_preis$Wohnlage_vorhersage <- factor(
  korrekt_model_gam_zentral_ohne_preis$Wohnlage_vorhersage,
  levels = levels_kombiniert)
korrekt_model_gam_ausserhalb_ohne_preis$Wohnlage_vorhersage <- factor(
  korrekt_model_gam_ausserhalb_ohne_preis$Wohnlage_vorhersage,
  levels = levels_kombiniert)
korrekt_model_gam_zentral_ohne_preis$Wohnlage_wahr <- factor(
  korrekt_model_gam_zentral_ohne_preis$Wohnlage_wahr,
  levels = levels_kombiniert)
korrekt_model_gam_ausserhalb_ohne_preis$Wohnlage_wahr <- factor(
  korrekt_model_gam_ausserhalb_ohne_preis$Wohnlage_wahr,
  levels = levels_kombiniert)

korrekt_model_gam_zentral_ohne$Wohnlage_vorhersage <- factor(
  korrekt_model_gam_zentral_ohne$Wohnlage_vorhersage,
  levels = levels_kombiniert)
korrekt_model_gam_ausserhalb_ohne$Wohnlage_vorhersage <- factor(
  korrekt_model_gam_ausserhalb_ohne$Wohnlage_vorhersage,
  levels = levels_kombiniert)
korrekt_model_gam_zentral_ohne$Wohnlage_wahr <- factor(
  korrekt_model_gam_zentral_ohne$Wohnlage_wahr,
  levels = levels_kombiniert)
korrekt_model_gam_ausserhalb_ohne$Wohnlage_wahr <- factor(
  korrekt_model_gam_ausserhalb_ohne$Wohnlage_wahr,
  levels = levels_kombiniert)

korrekt_model_gam_zentral_ohne_preis_bahn$Wohnlage_vorhersage <- factor(
  korrekt_model_gam_zentral_ohne_preis_bahn$Wohnlage_vorhersage,
  levels = levels_kombiniert)
korrekt_model_gam_ausserhalb_ohne_preis_bahn$Wohnlage_vorhersage <- factor(
  korrekt_model_gam_ausserhalb_ohne_preis_bahn$Wohnlage_vorhersage,
  levels = levels_kombiniert)
korrekt_model_gam_zentral_ohne_preis_bahn$Wohnlage_wahr <- factor(
  korrekt_model_gam_zentral_ohne_preis_bahn$Wohnlage_wahr,
  levels = levels_kombiniert)
korrekt_model_gam_ausserhalb_ohne_preis_bahn$Wohnlage_wahr <- factor(
  korrekt_model_gam_ausserhalb_ohne_preis_bahn$Wohnlage_wahr,
  levels = levels_kombiniert)


# mit prior
fehler_model_gam_zentral_prior$Wohnlage_vorhersage <- factor(
  fehler_model_gam_zentral_prior$Wohnlage_vorhersage,
  levels = levels_kombiniert)
fehler_model_gam_ausserhalb_prior$Wohnlage_vorhersage <- factor(
  fehler_model_gam_ausserhalb_prior$Wohnlage_vorhersage,
  levels = levels_kombiniert)
fehler_model_gam_zentral_prior$Wohnlage_wahr <- factor(
  fehler_model_gam_zentral_prior$Wohnlage_wahr,
  levels = levels_kombiniert)
fehler_model_gam_ausserhalb_prior$Wohnlage_wahr <- factor(
  fehler_model_gam_ausserhalb_prior$Wohnlage_wahr,
  levels = levels_kombiniert)

korrekt_model_gam_zentral_prior$Wohnlage_vorhersage <- factor(
  korrekt_model_gam_zentral_prior$Wohnlage_vorhersage,
  levels = levels_kombiniert)
korrekt_model_gam_ausserhalb_prior$Wohnlage_vorhersage <- factor(
  korrekt_model_gam_ausserhalb_prior$Wohnlage_vorhersage,
  levels = levels_kombiniert
)
korrekt_model_gam_zentral_prior$Wohnlage_wahr <- factor(
  korrekt_model_gam_zentral_prior$Wohnlage_wahr,
  levels = levels_kombiniert)
korrekt_model_gam_ausserhalb_prior$Wohnlage_wahr <- factor(
  korrekt_model_gam_ausserhalb_prior$Wohnlage_wahr,
  levels = levels_kombiniert
)


# Jetzt kombinieren 
fehler_model_gam_kombiniert <- rbind(fehler_model_gam_zentral, fehler_model_gam_ausserhalb)
korrekt_model_gam_kombiniert <- rbind(korrekt_model_gam_zentral, korrekt_model_gam_ausserhalb)
fehler_model_gam_kombiniert_ohne_preis <- rbind(fehler_model_gam_zentral_ohne_preis, fehler_model_gam_ausserhalb_ohne_preis)
korrekt_model_gam_kombiniert_ohne_preis <- rbind(korrekt_model_gam_zentral_ohne_preis, korrekt_model_gam_ausserhalb_ohne_preis)
fehler_model_gam_kombiniert_ohne <- rbind(fehler_model_gam_zentral_ohne, fehler_model_gam_ausserhalb_ohne)
korrekt_model_gam_kombiniert_ohne <- rbind(korrekt_model_gam_zentral_ohne, korrekt_model_gam_ausserhalb_ohne)
korrekt_model_gam_kombiniert_ohne_preis_bahn <- rbind(korrekt_model_gam_zentral_ohne_preis_bahn, korrekt_model_gam_ausserhalb_ohne_preis_bahn)
fehler_model_gam_kombiniert_ohne_preis_bahn <- rbind(fehler_model_gam_zentral_ohne_preis_bahn, fehler_model_gam_ausserhalb_ohne_preis_bahn)

# Jetzt kombinieren prior
fehler_model_gam_prior_kombiniert <- rbind(fehler_model_gam_zentral_prior,
                                           fehler_model_gam_ausserhalb_prior)
korrekt_model_gam_prior_kombiniert <- rbind(korrekt_model_gam_zentral_prior,
                                            korrekt_model_gam_ausserhalb_prior)

# WGS84 sicherstellen
wohnlagen_muc_wgs <- wohnlagen_muc2 %>%
  st_transform(4326) %>%
  mutate(color = case_when(
    Wohnlage == "durchschnittliche Lage (außerhalb)" ~ "#e8f5a4",
    Wohnlage == "gute Lage (außerhalb)" ~ "#afe391",
    Wohnlage == "beste Lage (außerhalb)" ~ "#7FCDBB",
    Wohnlage == "zentrale durchschnittliche Lage" ~ "#41B6C4",
    Wohnlage == "zentrale gute Lage" ~ "#1f5a82",
    Wohnlage == "zentrale beste Lage" ~ "#271352"
  ))

model_data_complete_wgs <- st_transform(model_data_complete, crs = 4326)

fehler_model_gam_ausserhalb_wgs <- st_transform(fehler_model_gam_ausserhalb, crs = 4326)
fehler_model_gam_zentral_wgs    <- st_transform(fehler_model_gam_zentral,    crs = 4326)
fehler_model_gam_ausserhalb_ohne_preis_wgs <- st_transform(fehler_model_gam_ausserhalb_ohne_preis, crs = 4326)
fehler_model_gam_zentral_ohne_preis_wgs    <- st_transform(fehler_model_gam_zentral_ohne_preis,    crs = 4326)
fehler_model_gam_ausserhalb_ohne_wgs <- st_transform(fehler_model_gam_ausserhalb_ohne, crs = 4326)
fehler_model_gam_zentral_ohne_wgs    <- st_transform(fehler_model_gam_zentral_ohne,    crs = 4326)
fehler_model_gam_ausserhalb_ohne_preis_bahn_wgs <- st_transform(fehler_model_gam_ausserhalb_ohne_preis_bahn, crs = 4326)
fehler_model_gam_zentral_ohne_preis_bahn_wgs    <- st_transform(fehler_model_gam_zentral_ohne_preis_bahn,    crs = 4326)


fehler_model_gam_kombiniert_wgs <- st_transform(fehler_model_gam_kombiniert, crs = 4326)
fehler_model_gam_prior_kombiniert_wgs <- st_transform(fehler_model_gam_prior_kombiniert, crs = 4326)

korrekt_model_gam_kombiniert_wgs <- st_transform(korrekt_model_gam_kombiniert, crs = 4326)
korrekt_model_gam_prior_kombiniert_wgs <- st_transform(korrekt_model_gam_prior_kombiniert, crs = 4326)

fehler_model_gam_kombiniert_ohne_preis_wgs <- st_transform(fehler_model_gam_kombiniert_ohne_preis, crs = 4326)
korrekt_model_gam_kombiniert_ohne_preis_wgs <- st_transform(korrekt_model_gam_kombiniert_ohne_preis, crs = 4326)

fehler_model_gam_kombiniert_ohne_wgs <- st_transform(fehler_model_gam_kombiniert_ohne, crs = 4326)
korrekt_model_gam_kombiniert_ohne_wgs <- st_transform(korrekt_model_gam_kombiniert_ohne, crs = 4326)

fehler_model_gam_kombiniert_ohne_preis_bahn_wgs <- st_transform(fehler_model_gam_kombiniert_ohne_preis_bahn, crs = 4326)
korrekt_model_gam_kombiniert_ohne_preis_bahn_wgs <- st_transform(korrekt_model_gam_kombiniert_ohne_preis_bahn, crs = 4326)

# Farbzuordung der Punkte
fehler_model_gam_kombiniert_wgs <- fehler_model_gam_kombiniert_wgs %>% 
  mutate(color = case_when(Wohnlage_vorhersage == "durchschnittliche Lage (außerhalb)" ~ "#e8f5a4",
                           Wohnlage_vorhersage == "gute Lage (außerhalb)" ~ "#afe391",
                           Wohnlage_vorhersage == "beste Lage (außerhalb)" ~ "#7FCDBB",
                           Wohnlage_vorhersage == "zentrale durchschnittliche Lage" ~ "#41B6C4",
                           Wohnlage_vorhersage == "zentrale gute Lage" ~ "#1f5a82",
                           Wohnlage_vorhersage == "zentrale beste Lage" ~ "#271352"))
fehler_model_gam_prior_kombiniert_wgs <- fehler_model_gam_prior_kombiniert_wgs %>%
  mutate(color = case_when(Wohnlage_vorhersage == "durchschnittliche Lage (außerhalb)" ~ "#e8f5a4",
                           Wohnlage_vorhersage == "gute Lage (außerhalb)" ~ "#afe391",
                           Wohnlage_vorhersage == "beste Lage (außerhalb)" ~ "#7FCDBB",
                           Wohnlage_vorhersage == "zentrale durchschnittliche Lage" ~ "#41B6C4",
                           Wohnlage_vorhersage == "zentrale gute Lage" ~ "#1f5a82",
                           Wohnlage_vorhersage == "zentrale beste Lage" ~ "#271352"))
fehler_model_gam_kombiniert_ohne_preis_wgs <- fehler_model_gam_kombiniert_ohne_preis_wgs %>%
  mutate(color = case_when(Wohnlage_vorhersage == "durchschnittliche Lage (außerhalb)" ~ "#e8f5a4",
                           Wohnlage_vorhersage == "gute Lage (außerhalb)" ~ "#afe391",
                           Wohnlage_vorhersage == "beste Lage (außerhalb)" ~ "#7FCDBB",
                           Wohnlage_vorhersage == "zentrale durchschnittliche Lage" ~ "#41B6C4",
                           Wohnlage_vorhersage == "zentrale gute Lage" ~ "#1f5a82",
                           Wohnlage_vorhersage == "zentrale beste Lage" ~ "#271352"))
fehler_model_gam_kombiniert_ohne_wgs <- fehler_model_gam_kombiniert_ohne_wgs %>%
  mutate(color = case_when(Wohnlage_vorhersage == "durchschnittliche Lage (außerhalb)" ~ "#e8f5a4",
                           Wohnlage_vorhersage == "gute Lage (außerhalb)" ~ "#afe391",
                           Wohnlage_vorhersage == "beste Lage (außerhalb)" ~ "#7FCDBB",
                           Wohnlage_vorhersage == "zentrale durchschnittliche Lage" ~ "#41B6C4",
                           Wohnlage_vorhersage == "zentrale gute Lage" ~ "#1f5a82",
                           Wohnlage_vorhersage == "zentrale beste Lage" ~ "#271352"))
fehler_model_gam_kombiniert_ohne_preis_bahn_wgs <- fehler_model_gam_kombiniert_ohne_preis_bahn_wgs %>%
  mutate(color = case_when(Wohnlage_vorhersage == "durchschnittliche Lage (außerhalb)" ~ "#e8f5a4",
                           Wohnlage_vorhersage == "gute Lage (außerhalb)" ~ "#afe391",
                           Wohnlage_vorhersage == "beste Lage (außerhalb)" ~ "#7FCDBB",
                           Wohnlage_vorhersage == "zentrale durchschnittliche Lage" ~ "#41B6C4",
                           Wohnlage_vorhersage == "zentrale gute Lage" ~ "#1f5a82",
                           Wohnlage_vorhersage == "zentrale beste Lage" ~ "#271352"))

  model_data_complete_wgs <- model_data_complete_wgs %>%
  mutate(color = case_when(Wohnlage == "durchschnittliche Lage (außerhalb)" ~ "#e8f5a4",
                           Wohnlage == "gute Lage (außerhalb)" ~ "#afe391",
                           Wohnlage == "beste Lage (außerhalb)" ~ "#7FCDBB",
                           Wohnlage == "zentrale durchschnittliche Lage" ~ "#41B6C4",
                           Wohnlage == "zentrale gute Lage" ~ "#1f5a82",
                           Wohnlage == "zentrale beste Lage" ~ "#271352"))
  
korrekt_model_gam_kombiniert_wgs <- korrekt_model_gam_kombiniert_wgs %>%
  mutate(color = case_when(Wohnlage_vorhersage == "durchschnittliche Lage (außerhalb)" ~ "#e8f5a4",
                           Wohnlage_vorhersage == "gute Lage (außerhalb)" ~ "#afe391",
                           Wohnlage_vorhersage == "beste Lage (außerhalb)" ~ "#7FCDBB",
                           Wohnlage_vorhersage == "zentrale durchschnittliche Lage" ~ "#41B6C4",
                           Wohnlage_vorhersage == "zentrale gute Lage" ~ "#1f5a82",
                           Wohnlage_vorhersage == "zentrale beste Lage" ~ "#271352"))
korrekt_model_gam_prior_kombiniert_wgs <- korrekt_model_gam_prior_kombiniert_wgs %>%
  mutate(color = case_when(Wohnlage_vorhersage == "durchschnittliche Lage (außerhalb)" ~ "#e8f5a4",
                           Wohnlage_vorhersage == "gute Lage (außerhalb)" ~ "#afe391",
                           Wohnlage_vorhersage == "beste Lage (außerhalb)" ~ "#7FCDBB",
                           Wohnlage_vorhersage == "zentrale durchschnittliche Lage" ~ "#41B6C4",
                           Wohnlage_vorhersage == "zentrale gute Lage" ~ "#1f5a82",
                           Wohnlage_vorhersage == "zentrale beste Lage" ~ "#271352"))
korrekt_model_gam_kombiniert_ohne_preis_wgs <- korrekt_model_gam_kombiniert_ohne_preis_wgs %>%
  mutate(color = case_when(Wohnlage_vorhersage == "durchschnittliche Lage (außerhalb)" ~ "#e8f5a4",
                           Wohnlage_vorhersage == "gute Lage (außerhalb)" ~ "#afe391",
                           Wohnlage_vorhersage == "beste Lage (außerhalb)" ~ "#7FCDBB",
                           Wohnlage_vorhersage == "zentrale durchschnittliche Lage" ~ "#41B6C4",
                           Wohnlage_vorhersage == "zentrale gute Lage" ~ "#1f5a82",
                           Wohnlage_vorhersage == "zentrale beste Lage" ~ "#271352"))
korrekt_model_gam_kombiniert_ohne_wgs <- korrekt_model_gam_kombiniert_ohne_wgs %>%
  mutate(color = case_when(Wohnlage_vorhersage == "durchschnittliche Lage (außerhalb)" ~ "#e8f5a4",
                           Wohnlage_vorhersage == "gute Lage (außerhalb)" ~ "#afe391",
                           Wohnlage_vorhersage == "beste Lage (außerhalb)" ~ "#7FCDBB",
                           Wohnlage_vorhersage == "zentrale durchschnittliche Lage" ~ "#41B6C4",
                           Wohnlage_vorhersage == "zentrale gute Lage" ~ "#1f5a82",
                           Wohnlage_vorhersage == "zentrale beste Lage" ~ "#271352"))
korrekt_model_gam_kombiniert_ohne_preis_bahn_wgs <- korrekt_model_gam_kombiniert_ohne_preis_bahn_wgs %>%
  mutate(color = case_when(Wohnlage_vorhersage == "durchschnittliche Lage (außerhalb)" ~ "#e8f5a4",
                           Wohnlage_vorhersage == "gute Lage (außerhalb)" ~ "#afe391",
                           Wohnlage_vorhersage == "beste Lage (außerhalb)" ~ "#7FCDBB",
                           Wohnlage_vorhersage == "zentrale durchschnittliche Lage" ~ "#41B6C4",
                           Wohnlage_vorhersage == "zentrale gute Lage" ~ "#1f5a82",
                           Wohnlage_vorhersage == "zentrale beste Lage" ~ "#271352"))


# Wohnlage_grenzen
wohnlage_grenzen_wgs <- st_transform(wohnlage_grenzen, crs = 4326)



# Interaktive Karten erstellen
# Hintergrunddaten: Straßen aus OpenStreetMap

# Interaktive Karte mit den Korrekten und Falschen
interaktive_karte_model2 <- leaflet() %>%
  setView(lng = 11.5761, lat = 48.1371, zoom = 11) %>%
  addProviderTiles("CartoDB.Positron") %>%
  # Wohnlagen hinzufügen – jetzt die transformierte Version!
  addPolygons(data = wohnlagen_muc_wgs,
              fillColor = ~wohnlagen_muc_wgs$color,
              fillOpacity = 0.6,
              color = "black",
              weight = 0.5,
              label = ~Wohnlage) %>%
  addCircleMarkers(
    data = fehler_model_gam_kombiniert_wgs,
    fillColor = fehler_model_gam_kombiniert_wgs$color,
    fillOpacity = 1,
    color = "red",
    stroke = TRUE,
    weight = 1,
    radius = 4,
    label = ~Wohnlage_vorhersage,
    group = "Fehler"
  ) %>%
  addCircleMarkers(
    data = korrekt_model_gam_kombiniert_wgs,
    fillColor = korrekt_model_gam_kombiniert_wgs$color,
    fillOpacity = 1,
    color = "black",
    stroke = TRUE,
    weight = 1,
    radius = 4,
    label = ~Wohnlage,
    group = "Korrekt"
  ) %>%
  # Optional: Grenzen (transformiert)
  addPolylines(data = wohnlage_grenzen_wgs, color = "black", weight = 0.5)%>% 
  addLegend(
    position = "bottomright",
    colors = wohnlage_farben,
    labels = names(wohnlage_farben),
    title = "Wohnlage",
    opacity = 1
  ) %>%
  addLayersControl(overlayGroups = c("Fehler", "Korrekt"),
                   options = layersControlOptions(collapsed = FALSE))
# anschauen
interaktive_karte_model2

# Speichern als html
# saveWidget(interaktive_karte_model2, file = "interaktive_karten/interaktive_karte_model2.html", selfcontained = TRUE)
# browseURL("interaktive_karten/interaktive_karte_model2.html")



# Interaktive Karte mit den Korrekten und Falschen (bereinigt)
interaktive_karte_model2_bereinigt <- leaflet() %>%
  setView(lng = 11.5761, lat = 48.1371, zoom = 11) %>%
  addProviderTiles("CartoDB.Positron") %>%
  # Wohnlagen hinzufügen – jetzt die transformierte Version!
  addPolygons(data = wohnlagen_muc_wgs,
              fillColor = ~wohnlagen_muc_wgs$color,
              fillOpacity = 0.6,
              color = "black",
              weight = 0.5,
              label = ~Wohnlage) %>%
  addCircleMarkers(
    data = fehler_model_gam_prior_kombiniert_wgs,
    fillColor = fehler_model_gam_prior_kombiniert_wgs$color,
    fillOpacity = 1,
    color = "red",
    stroke = TRUE,
    weight = 1,
    radius = 4,
    label = ~Wohnlage_vorhersage,
    group = "Fehler"
  ) %>%
  addCircleMarkers(
    data = korrekt_model_gam_prior_kombiniert_wgs,
    fillColor = korrekt_model_gam_prior_kombiniert_wgs$color,
    fillOpacity = 1,
    color = "black",
    stroke = TRUE,
    weight = 1,
    radius = 4,
    label = ~Wohnlage,
    group = "Korrekt"
  ) %>%
  # Optional: Grenzen (transformiert)
  addPolylines(data = wohnlage_grenzen_wgs, color = "black", weight = 0.5)%>% 
  addLegend(
    position = "bottomright",
    colors = wohnlage_farben,
    labels = names(wohnlage_farben),
    title = "Wohnlage",
    opacity = 1
  ) %>%
  addLayersControl(overlayGroups = c("Fehler", "Korrekt"),
                   options = layersControlOptions(collapsed = FALSE))
# anschauen
interaktive_karte_model2_bereinigt

# Speichern als html
# saveWidget(interaktive_karte_model2_bereinigt, file = "interaktive_karten/interaktive_karte_model2_bereinigt.html", selfcontained = TRUE)
# browseURL("interaktive_karten/interaktive_karte_model2_bereinigt.html")


# Interaktive Karte alle Wohnungen
interaktive_karte_complete <- leaflet() %>%
  setView(lng = 11.5761, lat = 48.1371, zoom = 11) %>%
  addProviderTiles("CartoDB.Positron") %>%
  # Wohnlagen hinzufügen – jetzt die transformierte Version!
  addPolygons(data = wohnlagen_muc_wgs,
              fillColor = ~wohnlagen_muc_wgs$color,
              fillOpacity = 0.6,
              color = "black",
              weight = 0.5,
              label = ~Wohnlage) %>%
  addCircleMarkers(
    data = model_data_complete_wgs,
    fillColor = model_data_complete_wgs$color,
    fillOpacity = 1,
    color = "black",
    stroke = TRUE,
    weight = 1,
    radius = 4,
    label = ~Wohnlage,
    group = "Wohnlage"
  ) %>%
  addPolylines(data = wohnlage_grenzen_wgs, color = "black", weight = 0.5)%>% 
  addLegend(
    position = "bottomright",
    colors = wohnlage_farben,
    labels = names(wohnlage_farben),
    title = "Wohnlage",
    opacity = 1
  ) %>%
  addLayersControl(overlayGroups = c("Straßen"),
                   options = layersControlOptions(collapsed = FALSE))
# anschauen
interaktive_karte_complete


# Speichern als html
# saveWidget(interaktive_karte_complete, file = "interaktive_karten/interaktive_karte_complete.html", selfcontained = TRUE)
# browseURL("interaktive_karten/interaktive_karte_complete.html")





# Experiment
# Interaktive Karte mit den Korrekten und Falschen
interaktive_karte_model_e <- leaflet() %>%
  setView(lng = 11.5761, lat = 48.1371, zoom = 11) %>%
  addProviderTiles("CartoDB.Positron") %>%
  # Wohnlagen hinzufügen – jetzt die transformierte Version!
  addPolygons(data = wohnlagen_muc_wgs,
              fillColor = ~wohnlagen_muc_wgs$color,
              fillOpacity = 0.6,
              color = "black",
              weight = 0.5,
              label = ~Wohnlage) %>%
  addCircleMarkers(
    data = fehler_model_gam_kombiniert_wgs,
    fillColor = fehler_model_gam_kombiniert_wgs$color,
    fillOpacity = 1,
    color = "red",
    stroke = TRUE,
    weight = 1,
    radius = 4,
    label = ~Wohnlage_vorhersage,
    popup = ~paste0(
      "<b>Distanz Bahnhof:</b> ", distanz_bahnhof, "<br>",
      "<b>Distanz U-Bahn:</b> ", distanz_ubahn, "<br>",
      "<b>Distanz Bus:</b> ", distanz_bushaltestelle, "<br>",
      "<b>Distanz Unterzentrum:</b> ", distanz_unterzentrum, "<br>",
      "<b>Distanz Mittelzentrum:</b> ", distanz_mittelzentrum, "<br>",
      "<b>Nahversorgungsindex:</b> ", nahversorgungs_index, "<br>",
      "<b>Hauspreisindex:</b> ", hauspreis_index, "<br>", 
      "<b> ÖPNV-Index:</b> ", opnv_index, "<br>",
      "<b>Straßentyp:</b> ", straßentyp_gruppe, "<br>"
    ),
    group = "Fehler"
  ) %>%
  addCircleMarkers(
    data = korrekt_model_gam_kombiniert_wgs,
    fillColor = korrekt_model_gam_kombiniert_wgs$color,
    fillOpacity = 1,
    color = "black",
    stroke = TRUE,
    weight = 1,
    radius = 4,
    label = ~Wohnlage,
    popup = ~paste0(
      "<b>Distanz Bahnhof:</b> ", distanz_bahnhof, "<br>",
      "<b>Distanz U-Bahn:</b> ", distanz_ubahn, "<br>",
      "<b>Distanz Bus:</b> ", distanz_bushaltestelle, "<br>",
      "<b>Distanz Unterzentrum:</b> ", distanz_unterzentrum, "<br>",
      "<b>Distanz Mittelzentrum:</b> ", distanz_mittelzentrum, "<br>",
      "<b>Nahversorgungsindex:</b> ", nahversorgungs_index, "<br>",
      "<b>Hauspreisindex:</b> ", hauspreis_index, "<br>", 
      "<b> ÖPNV-Index:</b> ", opnv_index, "<br>",
      "<b>Straßentyp:</b> ", straßentyp_gruppe, "<br>"
    ),
    group = "Korrekt"
  ) %>%
  # Optional: Grenzen (transformiert)
  addPolylines(data = wohnlage_grenzen_wgs, color = "black", weight = 0.5)%>% 
  addLegend(
    position = "bottomright",
    colors = wohnlage_farben,
    labels = names(wohnlage_farben),
    title = "Wohnlage",
    opacity = 1
  ) %>%
  addLayersControl(overlayGroups = c("Fehler", "Korrekt"),
                   options = layersControlOptions(collapsed = FALSE))
# anschauen
interaktive_karte_model_e

saveWidget(interaktive_karte_model_e, file = "interaktive_karten/interaktive_karte_model_e.html", selfcontained = TRUE)
browseURL("interaktive_karten/interaktive_karte_model_e.html")






# Ohne Huaspreisindex

# test------------
fehler_model_gam_kombiniert_ohne_preis_wgs <- fehler_model_gam_kombiniert_ohne_preis_wgs %>%
  mutate(
    lon = st_coordinates(geometry)[,1],
    lat = st_coordinates(geometry)[,2]
  )


# Interaktive Karte mit den Korrekten und Falschen
interaktive_karte_model_reduziert <- leaflet() %>%
  setView(lng = 11.5761, lat = 48.1371, zoom = 11) %>%
  addProviderTiles("CartoDB.Positron") %>%
  # Wohnlagen hinzufügen – jetzt die transformierte Version!
  addPolygons(data = wohnlagen_muc_wgs,
              fillColor = ~wohnlagen_muc_wgs$color,
              fillOpacity = 0.6,
              color = "black",
              weight = 0.5,
              label = ~Wohnlage) %>%
  addCircleMarkers(
    data = fehler_model_gam_kombiniert_ohne_preis_wgs,
    fillColor = fehler_model_gam_kombiniert_ohne_preis_wgs$color,
    fillOpacity = 1,
    color = "red",
    stroke = TRUE,
    weight = 1,
    radius = 4,
    label = ~Wohnlage_vorhersage,
    popup = ~paste0(
      "<b>Distanz Bahnhof:</b> ", distanz_bahnhof, "<br>",
      "<b>Distanz U-Bahn:</b> ", distanz_ubahn, "<br>",
      "<b>Distanz Bus:</b> ", distanz_bushaltestelle, "<br>",
      "<b>Distanz Unterzentrum:</b> ", distanz_unterzentrum, "<br>",
      "<b>Distanz Mittelzentrum:</b> ", distanz_mittelzentrum, "<br>",
      "<b>Hauspreisindex:</b> ", hauspreis_index, "<br>", 
      "<b>Nahversorgungsindex:</b> ", nahversorgungs_index, "<br>",
      "<b> ÖPNV-Index:</b> ", opnv_index, "<br>",
      "<b>Straßentyp:</b> ", straßentyp_gruppe, "<br>",
      "<b>Koordinaten:</b> ", round(st_coordinates(geometry)[,1], 5), ", ",
      round(st_coordinates(geometry)[,2], 5), "<br>"
    ),
    group = "Fehler"
  ) %>%
  addCircleMarkers(
    data = korrekt_model_gam_kombiniert_ohne_preis_wgs,
    fillColor = korrekt_model_gam_kombiniert_ohne_preis_wgs$color,
    fillOpacity = 1,
    color = "black",
    stroke = TRUE,
    weight = 1,
    radius = 4,
    label = ~Wohnlage,
    popup = ~paste0(
      "<b>Distanz Bahnhof:</b> ", distanz_bahnhof, "<br>",
      "<b>Distanz U-Bahn:</b> ", distanz_ubahn, "<br>",
      "<b>Distanz Bus:</b> ", distanz_bushaltestelle, "<br>",
      "<b>Distanz Unterzentrum:</b> ", distanz_unterzentrum, "<br>",
      "<b>Hauspreisindex:</b> ", hauspreis_index, "<br>", 
      "<b>Distanz Mittelzentrum:</b> ", distanz_mittelzentrum, "<br>",
      "<b>Nahversorgungsindex:</b> ", nahversorgungs_index, "<br>",
      "<b> ÖPNV-Index:</b> ", opnv_index, "<br>",
      "<b>Straßentyp:</b> ", straßentyp_gruppe, "<br>",
      "<b>Koordinaten:</b> ", round(st_coordinates(geometry)[,1], 5), ", ",
      round(st_coordinates(geometry)[,2], 5), "<br>"
    ),
    group = "Korrekt"
  ) %>%
  # Optional: Grenzen (transformiert)
  addPolylines(data = wohnlage_grenzen_wgs, color = "black", weight = 0.5)%>% 
  addLegend(
    position = "bottomright",
    colors = wohnlage_farben,
    labels = names(wohnlage_farben),
    title = "Wohnlage",
    opacity = 1
  ) %>%
  addLayersControl(overlayGroups = c("Fehler", "Korrekt"),
                   options = layersControlOptions(collapsed = FALSE))
# anschauen
interaktive_karte_model_reduziert

saveWidget(interaktive_karte_model_reduziert, file = "interaktive_karten/interaktive_karte_model_reduziert.html", selfcontained = TRUE)
browseURL("interaktive_karten/interaktive_karte_model_reduziert.html")



# Ohne Hauspreisindex und ohne distanz_bahnhof
# Interaktive Karte mit den Korrekten und Falschen
interaktive_karte_model_ohne_preis_bahn <- leaflet() %>%
  setView(lng = 11.5761, lat = 48.1371, zoom = 11) %>%
  addProviderTiles("CartoDB.Positron") %>%
  # Wohnlagen hinzufügen – jetzt die transformierte Version!
  addPolygons(data = wohnlagen_muc_wgs,
              fillColor = ~wohnlagen_muc_wgs$color,
              fillOpacity = 0.6,
              color = "black",
              weight = 0.5,
              label = ~Wohnlage) %>%
  addCircleMarkers(
    data = fehler_model_gam_kombiniert_ohne_preis_bahn_wgs,
    fillColor = fehler_model_gam_kombiniert_ohne_preis_bahn_wgs$color,
    fillOpacity = 1,
    color = "red",
    stroke = TRUE,
    weight = 1,
    radius = 4,
    label = ~Wohnlage_vorhersage,
    popup = ~paste0(
      "<b>Distanz U-Bahn:</b> ", distanz_ubahn, "<br>",
      "<b>Distanz Bus:</b> ", distanz_bushaltestelle, "<br>",
      "<b>Distanz Unterzentrum:</b> ", distanz_unterzentrum, "<br>",
      "<b>Distanz Mittelzentrum:</b> ", distanz_mittelzentrum, "<br>",
      "<b>Hauspreisindex:</b> ", hauspreis_index, "<br>", 
      "<b>Nahversorgungsindex:</b> ", nahversorgungs_index, "<br>",
      "<b> ÖPNV-Index:</b> ", opnv_index, "<br>",
      "<b>Straßentyp:</b> ", straßentyp_gruppe, "<br>"
    ),
    group = "Fehler"
  ) %>%
  addCircleMarkers(
    data = korrekt_model_gam_kombiniert_ohne_preis_bahn_wgs,
    fillColor = korrekt_model_gam_kombiniert_ohne_preis_bahn_wgs$color,
    fillOpacity = 1,
    color = "black",
    stroke = TRUE,
    weight = 1,
    radius = 4,
    label = ~Wohnlage,
    popup = ~paste0(
      "<b>Distanz U-Bahn:</b> ", distanz_ubahn, "<br>",
      "<b>Distanz Bus:</b> ", distanz_bushaltestelle, "<br>",
      "<b>Distanz Unterzentrum:</b> ", distanz_unterzentrum, "<br>",
      "<b>Hauspreisindex:</b> ", hauspreis_index, "<br>", 
      "<b>Distanz Mittelzentrum:</b> ", distanz_mittelzentrum, "<br>",
      "<b>Nahversorgungsindex:</b> ", nahversorgungs_index, "<br>",
      "<b> ÖPNV-Index:</b> ", opnv_index, "<br>",
      "<b>Straßentyp:</b> ", straßentyp_gruppe, "<br>"
    ),
    group = "Korrekt"
  ) %>%
  # Optional: Grenzen (transformiert)
  addPolylines(data = wohnlage_grenzen_wgs, color = "black", weight = 0.5)%>% 
  addLegend(
    position = "bottomright",
    colors = wohnlage_farben,
    labels = names(wohnlage_farben),
    title = "Wohnlage",
    opacity = 1
  ) %>%
  addLayersControl(overlayGroups = c("Fehler", "Korrekt"),
                   options = layersControlOptions(collapsed = FALSE))
# anschauen
interaktive_karte_model_ohne_preis_bahn

saveWidget(interaktive_karte_model_ohne_preis_bahn, 
           file = "interaktive_karten/interaktive_karte_model_ohne_preis_bahn.html",
           selfcontained = TRUE)
browseURL("interaktive_karten/interaktive_karte_model_ohne_preis_bahn.html")


# Ohne distanz_bahnhof
# Interaktive Karte mit den Korrekten und Falschen
interaktive_karte_model_ohne <- leaflet() %>%
  setView(lng = 11.5761, lat = 48.1371, zoom = 11) %>%
  addProviderTiles("CartoDB.Positron") %>%
  # Wohnlagen hinzufügen – jetzt die transformierte Version!
  addPolygons(data = wohnlagen_muc_wgs,
              fillColor = ~wohnlagen_muc_wgs$color,
              fillOpacity = 0.6,
              color = "black",
              weight = 0.5,
              label = ~Wohnlage) %>%
  addCircleMarkers(
    data = fehler_model_gam_kombiniert_ohne_wgs,
    fillColor = fehler_model_gam_kombiniert_ohne_wgs$color,
    fillOpacity = 1,
    color = "red",
    stroke = TRUE,
    weight = 1,
    radius = 4,
    label = ~Wohnlage_vorhersage,
    popup = ~paste0(
      "<b>Distanz U-Bahn:</b> ", distanz_ubahn, "<br>",
      "<b>Distanz Bus:</b> ", distanz_bushaltestelle, "<br>",
      "<b>Distanz Unterzentrum:</b> ", distanz_unterzentrum, "<br>",
      "<b>Distanz Mittelzentrum:</b> ", distanz_mittelzentrum, "<br>",
      "<b>Hauspreisindex:</b> ", hauspreis_index, "<br>", 
      "<b>Nahversorgungsindex:</b> ", nahversorgungs_index, "<br>",
      "<b> ÖPNV-Index:</b> ", opnv_index, "<br>",
      "<b>Straßentyp:</b> ", straßentyp_gruppe, "<br>"
    ),
    group = "Fehler"
  ) %>%
  addCircleMarkers(
    data = korrekt_model_gam_kombiniert_ohne_wgs,
    fillColor = korrekt_model_gam_kombiniert_ohne_wgs$color,
    fillOpacity = 1,
    color = "black",
    stroke = TRUE,
    weight = 1,
    radius = 4,
    label = ~Wohnlage,
    popup = ~paste0(
      "<b>Distanz U-Bahn:</b> ", distanz_ubahn, "<br>",
      "<b>Distanz Bus:</b> ", distanz_bushaltestelle, "<br>",
      "<b>Distanz Unterzentrum:</b> ", distanz_unterzentrum, "<br>",
      "<b>Hauspreisindex:</b> ", hauspreis_index, "<br>", 
      "<b>Distanz Mittelzentrum:</b> ", distanz_mittelzentrum, "<br>",
      "<b>Nahversorgungsindex:</b> ", nahversorgungs_index, "<br>",
      "<b> ÖPNV-Index:</b> ", opnv_index, "<br>",
      "<b>Straßentyp:</b> ", straßentyp_gruppe, "<br>"
    ),
    group = "Korrekt"
  ) %>%
  # Optional: Grenzen (transformiert)
  addPolylines(data = wohnlage_grenzen_wgs, color = "black", weight = 0.5)%>% 
  addLegend(
    position = "bottomright",
    colors = wohnlage_farben,
    labels = names(wohnlage_farben),
    title = "Wohnlage",
    opacity = 1
  ) %>%
  addLayersControl(overlayGroups = c("Fehler", "Korrekt"),
                   options = layersControlOptions(collapsed = FALSE))
# anschauen
interaktive_karte_model_ohne

saveWidget(interaktive_karte_model_ohne, 
           file = "interaktive_karten/interaktive_karte_model_ohne.html",
           selfcontained = TRUE)
browseURL("interaktive_karten/interaktive_karte_model_ohne.html")



# Karte mit Variablen

# Karte mit Variablen (TEST)


pal <- colorNumeric(
  palette = "magma",  # alternativ: "viridis", "plasma", "inferno", "magma", "Spectral"
  domain = model_data_complete_wgs$nahversorgungs_index
)


interaktive_karte_nahversorgung <- leaflet(model_data_complete_wgs) %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(lng = 11.5761, lat = 48.1371, zoom = 11) %>%
  addPolygons(data = wohnlagen_muc_wgs,
              fillColor = ~wohnlagen_muc_wgs$color,
              fillOpacity = 0.6,
              color = "black",
              weight = 0.5,
              group = "Wahre Wohnlage") %>%
  addPolylines(data = wohnlage_grenzen_wgs, color = "black", weight = 0.5)%>% 
  addLegend(
    position = "bottomright",
    colors = wohnlage_farben,
    labels = names(wohnlage_farben),
    title = "Wohnlage",
    opacity = 1,
    group = "Wahre Wohnlage"
  ) %>%
  addCircleMarkers(
    fillColor = ~pal(nahversorgungs_index),
    fillOpacity = 1,
    color = "black",
    stroke = TRUE,
    weight = 1,
    radius = 4,
    label = ~paste("Nahversorgungsindex:", round(nahversorgungs_index, 2))
  ) %>%
  addLegend(
    "bottomright",
    pal = pal,
    values = ~nahversorgungs_index,
    title = "Nahversorgungsindex",
    opacity = 1
  )%>%
  addLayersControl(overlayGroups = c("Wahre Wohnlage"),
                   options = layersControlOptions(collapsed = FALSE))

interaktive_karte_nahversorgung



# Speichern als html
# saveWidget(interaktive_karte_complete, file = "interaktive_karten/interaktive_karte_complete.html", selfcontained = TRUE)
# browseURL("interaktive_karten/interaktive_karte_complete.html")

# Karte: Nahversorgung

pal <- colorNumeric(
  palette = "magma",  # alternativ: "viridis", "plasma", "inferno", "magma", "Spectral"
  domain = model_data_complete_wgs$nahversorgungs_index
)


interaktive_karte_nahversorgung <- leaflet(model_data_complete_wgs) %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(lng = 11.5761, lat = 48.1371, zoom = 11) %>%
  addPolylines(data = wohnlage_grenzen_wgs, color = "black", weight = 0.5)%>% 
  addCircleMarkers(
    fillColor = ~pal(nahversorgungs_index),
    fillOpacity = 1,
    color = "black",
    stroke = TRUE,
    weight = 1,
    radius = 4,
    label = ~paste("Nahversorgungsindex:", round(nahversorgungs_index, 2)),
    group = "Punkte"
  ) %>%
  addLegend(
    "bottomright",
    pal = pal,
    values = ~nahversorgungs_index,
    title = "Nahversorgungsindex",
    opacity = 1
  ) %>%
  addLayersControl(overlayGroups = c("Punkte"),
                      options = layersControlOptions(collapsed = FALSE))


# Speichern als html
 saveWidget(interaktive_karte_nahversorgung,
file = "interaktive_karten/variablen/interaktive_karte_nahversorgung.html",
selfcontained = TRUE)
 browseURL("interaktive_karten/variablen/interaktive_karte_nahversorgung.html")


 # Karte: distanz_mittelzentrum
 
 pal <- colorNumeric(
   palette = "Spectral",  # alternativ: "viridis", "plasma", "inferno", "magma", "Spectral"
   domain = model_data_complete_wgs$distanz_mittelzentrum
 )
 
 
 interaktive_karte_distanz_mittelzentrum <- leaflet(model_data_complete_wgs) %>%
   addProviderTiles("CartoDB.Positron") %>%
   setView(lng = 11.5761, lat = 48.1371, zoom = 11) %>%
   addPolylines(data = wohnlage_grenzen_wgs, color = "black", weight = 0.5)%>% 
   addCircleMarkers(
     fillColor = ~pal(distanz_mittelzentrum),
     fillOpacity = 1,
     color = "black",
     stroke = TRUE,
     weight = 1,
     radius = 4,
     label = ~paste("Distanz zum Mittelzentrum:", round(distanz_mittelzentrum, 2)),
     group = "Punkte"
   ) %>%
   addLegend(
     "bottomright",
     pal = pal,
     values = ~distanz_mittelzentrum,
     title = "Distanz zum Mittelzentrum (in m)",
     opacity = 1
   )%>%
   addLayersControl(overlayGroups = c("Punkte"),
                    options = layersControlOptions(collapsed = FALSE))
 
 
 # Speichern als html
 saveWidget(interaktive_karte_distanz_mittelzentrum,
            file = "interaktive_karten/variablen/interaktive_karte_distanz_mittelzentrum.html",
            selfcontained = TRUE)
 browseURL("interaktive_karten/variablen/interaktive_karte_distanz_mittelzentrum.html")
 
 
 # Karte: distanz_bahnhof
 
 pal <- colorNumeric(
   palette = "Spectral",  # alternativ: "viridis", "plasma", "inferno", "magma", "Spectral"
   domain = model_data_complete_wgs$distanz_bahnhof
 )
 
 
 interaktive_karte_distanz_bahnhof <- leaflet(model_data_complete_wgs) %>%
   addProviderTiles("CartoDB.Positron") %>%
   setView(lng = 11.5761, lat = 48.1371, zoom = 11) %>%
   addPolylines(data = wohnlage_grenzen_wgs, color = "black", weight = 0.5)%>% 
   addCircleMarkers(
     fillColor = ~pal(distanz_bahnhof),
     fillOpacity = 1,
     color = "black",
     stroke = TRUE,
     weight = 1,
     radius = 4,
     label = ~paste("Distanz zum Bahnhof:", round(distanz_bahnhof, 2)),
     group = "Punkte"
   ) %>%
   addLegend(
     "bottomright",
     pal = pal,
     values = ~distanz_bahnhof,
     title = "Distanz zum Bahnhof (in m)",
     opacity = 1
   )%>%
   addLayersControl(overlayGroups = c("Punkte"),
                    options = layersControlOptions(collapsed = FALSE))
 
 
 # Speichern als html
 saveWidget(interaktive_karte_distanz_bahnhof,
            file = "interaktive_karten/variablen/interaktive_karte_distanz_bahnhof.html",
            selfcontained = TRUE)
 browseURL("interaktive_karten/variablen/interaktive_karte_distanz_bahnhof.html")
 
 # Karte: distanz_unterzentrum
 
 pal <- colorNumeric(
   palette = "Spectral",  # alternativ: "viridis", "plasma", "inferno", "magma", "Spectral"
   domain = model_data_complete_wgs$distanz_unterzentrum
 )
 
 
 interaktive_karte_distanz_unterzentrum <- leaflet(model_data_complete_wgs) %>%
   addProviderTiles("CartoDB.Positron") %>%
   setView(lng = 11.5761, lat = 48.1371, zoom = 11) %>%
   addPolylines(data = wohnlage_grenzen_wgs, color = "black", weight = 0.5)%>% 
   addCircleMarkers(
     fillColor = ~pal(distanz_unterzentrum),
     fillOpacity = 1,
     color = "black",
     stroke = TRUE,
     weight = 1,
     radius = 4,
     label = ~paste("Distanz zum Unterzentrum:", round(distanz_unterzentrum, 2)),
     group = "Punkte"
   ) %>%
   addLegend(
     "bottomright",
     pal = pal,
     values = ~distanz_unterzentrum,
     title = "Distanz zum Unterzentrum (in m)",
     opacity = 1
   )%>%
   addLayersControl(overlayGroups = c("Punkte"),
                    options = layersControlOptions(collapsed = FALSE))
 
 
 # Speichern als html
 saveWidget(interaktive_karte_distanz_unterzentrum,
            file = "interaktive_karten/variablen/interaktive_karte_distanz_unterzentrum.html",
            selfcontained = TRUE)
 browseURL("interaktive_karten/variablen/interaktive_karte_distanz_unterzentrum.html")
 
 
 # Karte: distanz_bushaltestelle
 
 pal <- colorNumeric(
   palette = "Spectral",  # alternativ: "viridis", "plasma", "inferno", "magma", "Spectral"
   domain = model_data_complete_wgs$distanz_bushaltestelle
 )
 
 
 interaktive_karte_distanz_bushaltestelle <- leaflet(model_data_complete_wgs) %>%
   addProviderTiles("CartoDB.Positron") %>%
   setView(lng = 11.5761, lat = 48.1371, zoom = 11) %>%
   addPolylines(data = wohnlage_grenzen_wgs, color = "black", weight = 0.5)%>% 
   addCircleMarkers(
     fillColor = ~pal(distanz_bushaltestelle),
     fillOpacity = 1,
     color = "black",
     stroke = TRUE,
     weight = 1,
     radius = 4,
     label = ~paste("Distanz zur Bushaltestelle:", round(distanz_bushaltestelle, 2)),
     group = "Punkte"
   ) %>%
   addLegend(
     "bottomright",
     pal = pal,
     values = ~distanz_bushaltestelle,
     title = "Distanz zur Bushaltestelle (in m)",
     opacity = 1
   )%>%
   addLayersControl(overlayGroups = c("Punkte"),
                    options = layersControlOptions(collapsed = FALSE))
 
 
 # Speichern als html
 saveWidget(interaktive_karte_distanz_bushaltestelle,
            file = "interaktive_karten/variablen/interaktive_karte_distanz_bushaltestelle.html",
            selfcontained = TRUE)
 browseURL("interaktive_karten/variablen/interaktive_karte_distanz_bushaltestelle.html")
 
 
 # Karte: distanz_ubahn
 
 pal <- colorNumeric(
   palette = "Spectral",  # alternativ: "viridis", "plasma", "inferno", "magma", "Spectral"
   domain = model_data_complete_wgs$distanz_ubahn
 )
 
 
 interaktive_karte_distanz_ubahn <- leaflet(model_data_complete_wgs) %>%
   addProviderTiles("CartoDB.Positron") %>%
   setView(lng = 11.5761, lat = 48.1371, zoom = 11) %>%
   addPolylines(data = wohnlage_grenzen_wgs, color = "black", weight = 0.5)%>% 
   addCircleMarkers(
     fillColor = ~pal(distanz_ubahn),
     fillOpacity = 1,
     color = "black",
     stroke = TRUE,
     weight = 1,
     radius = 4,
     label = ~paste("Distanz zur U-Bahn:", round(distanz_ubahn, 2)),
     group = "Punkte"
   ) %>%
   addLegend(
     "bottomright",
     pal = pal,
     values = ~distanz_ubahn,
     title = "Distanz zur U-Bahn (in m)",
     opacity = 1
   )%>%
   addLayersControl(overlayGroups = c("Punkte"),
                    options = layersControlOptions(collapsed = FALSE))
 
 
 # Speichern als html
 saveWidget(interaktive_karte_distanz_ubahn,
            file = "interaktive_karten/variablen/interaktive_karte_distanz_ubahn.html",
            selfcontained = TRUE)
 browseURL("interaktive_karten/variablen/interaktive_karte_distanz_ubahn.html")
 
 # Karte: OPNV-Index
 
 pal <- colorNumeric(
   palette = "magma",  # alternativ: "viridis", "plasma", "inferno", "magma", "Spectral"
   domain = model_data_complete_wgs$opnv_index
 )
 
 
 interaktive_karte_opnv_index <- leaflet(model_data_complete_wgs) %>%
   addProviderTiles("CartoDB.Positron") %>%
   setView(lng = 11.5761, lat = 48.1371, zoom = 11) %>%
   addPolylines(data = wohnlage_grenzen_wgs, color = "black", weight = 0.5)%>% 
   addCircleMarkers(
     fillColor = ~pal(opnv_index),
     fillOpacity = 1,
     color = "black",
     stroke = TRUE,
     weight = 1,
     radius = 4,
     label = ~paste("OPNV-Index:", round(opnv_index, 2)),
     group = "Punkte"
   ) %>%
   addLegend(
     "bottomright",
     pal = pal,
     values = ~opnv_index,
     title = "OPNV-Index",
     opacity = 1
   ) %>%
   addLayersControl(overlayGroups = c("Punkte"),
                    options = layersControlOptions(collapsed = FALSE))
 
 
 # Speichern als html
 saveWidget(interaktive_karte_opnv_index,
            file = "interaktive_karten/variablen/interaktive_karte_opnv_index.html",
            selfcontained = TRUE)
 browseURL("interaktive_karten/variablen/interaktive_karte_opnv_index.html")
 