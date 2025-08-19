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
model_gam_zentral <- readRDS("modell_outputs/gam_model_zentral.rds")
model_gam_ausserhalb <- readRDS("modell_outputs/gam_model_ausserhalb.rds")

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
korrekt_model_gam_zentral <- korrekte_vorhersagen_zentral(model_gam_zentral, 
                                                           data = model_data_zentral_complete,
                                                           predict_fun = predict_labels_discr)
korrekt_model_gam_ausserhalb <- korrekte_vorhersagen_ausserhalb(model_gam_ausserhalb, 
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

korrekt_model_gam_zentral <- prepare_sf_object(korrekt_model_gam_zentral)
korrekt_model_gam_ausserhalb <- prepare_sf_object(korrekt_model_gam_ausserhalb)
korrekt_model_gam_zentral_prior <- prepare_sf_object(korrekt_model_gam_zentral_prior)
korrekt_model_gam_ausserhalb_prior <- prepare_sf_object(korrekt_model_gam_ausserhalb_prior)

wohnlage_grenzen <- prepare_sf_object(wohnlage_grenzen)
wohnlage_grenzen_zentral <- prepare_sf_object(wohnlage_grenzen_zentral)
wohnlage_grenzen_ausserhalb <- prepare_sf_object(wohnlage_grenzen_ausserhalb)







# Plots 
# Alle Wohnungen visualisieren--------------------------------------------------
karte_Wohnungen <- ggplot() +
  # Hintergrundflächen mit tatsächlicher Wohnlage
  geom_sf(data = wohnlagen_muc2, aes(fill = Wohnlage), color = "white", size = 0.1) +
  geom_sf(data = wohnlage_grenzen, color = "black", size = 0.05) + # das evt. weg
  geom_sf(data = model_data_complete_sf, aes(fill = Wohnlage),color = "black",        # schwarzer Rand
          shape = 21,             # Punkt mit Rand & Füllung
          size = 1.2,
          alpha = 0.8,
          stroke = 0.3) +
  scale_fill_manual(values = wohnlage_farben) +
  labs(
    color = "Wohnlage"
  ) +
  guides(
    fill = guide_legend(override.aes = list(color = NA))  # verhindert schwarzen Rand in Legende
  ) +
  theme_minimal()+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.title = element_blank()
  )
karte_Wohnungen


# Alle zentralen Wohnungen visualisieren
karte_Wohnungen_zentral <- ggplot() +
  # Hintergrundflächen mit tatsächlicher Wohnlage
  geom_sf(data = wohnlagen_muc_zentral, aes(fill = Wohnlage), color = "white", size = 0.1) +
  geom_sf(data = wohnlage_grenzen_zentral, color = "black", size = 0.05) + # das evt. weg
  geom_sf(data = model_data_zentral_complete_sf, aes(fill = Wohnlage),color = "lightgrey",        # schwarzer Rand
          shape = 21,             # Punkt mit Rand & Füllung
          size = 1.2,
          alpha = 0.8,
          stroke = 0.4) +
  scale_fill_manual(values = wohnlage_farben) +
  labs(
    color = "Wohnlage"
  ) +
  guides(
    fill = guide_legend(override.aes = list(color = NA))  # verhindert schwarzen Rand in Legende
  ) +
  theme_minimal()+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.title = element_blank()
  )
karte_Wohnungen_zentral


# Alle Wohnungen mit wahrer Wohnlage: In mehrere Teile teilen

# Bounding Box-Koordinaten aus Daten
# Gesamtbereich
st_bbox(model_data_complete_sf)
xmin <- 677330
xmax <- 702011
ymin <- 5326808
ymax <- 5346692

# Mittelwerte berechnen
xmid <- (xmin + xmax) / 2  # 689670.5
ymid <- (ymin + ymax) / 2  # 5336750

# Definiere die vier Bounding Boxes
bbox_nw <- coord_sf(xlim = c(xmin, xmid), ylim = c(ymid, ymax))
bbox_ne <- coord_sf(xlim = c(xmid, xmax), ylim = c(ymid, ymax))
bbox_sw <- coord_sf(xlim = c(xmin, xmid), ylim = c(ymin, ymid))
bbox_se <- coord_sf(xlim = c(xmid, xmax), ylim = c(ymin, ymid))

# Plotfunktion 
karte_teil <- function(coord) {
  ggplot() +
    geom_sf(data = wohnlagen_muc2, aes(fill = Wohnlage), color = "white", size = 0.1) +
    geom_sf(data = wohnlage_grenzen, color = "black", size = 0.05) + # optional
    geom_sf(data = model_data_complete_sf, aes(fill = Wohnlage), color = "black",
            shape = 21, size = 1.0, alpha = 0.8, stroke = 0.2) +
    scale_fill_manual(values = wohnlage_farben) +
    labs(color = "Wohnlage") +
    guides(fill = guide_legend(override.aes = list(color = NA))) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.title = element_blank()
    ) +
    coord
}

# Erzeuge die 4 Teilkarten
karte_nw <- karte_teil(bbox_nw)
karte_ne <- karte_teil(bbox_ne)
karte_sw <- karte_teil(bbox_sw)
karte_se <- karte_teil(bbox_se)

# ansehen
karte_nw
karte_ne
karte_sw
karte_se




# Fehlklassifikationskarten

# Funktion zum teilen (für Lagen außerhalb)
karte_teil_fehl <- function(coord, point_data) {
  ggplot() +
    geom_sf(data = wohnlagen_muc_ausserhalb, aes(fill = Wohnlage), color = "white", size = 0.1) +
    geom_sf(data = wohnlage_grenzen_ausserhalb, color = "black", size = 0.05) + # optional
    geom_sf(data = point_data, aes(fill = Wohnlage_vorhersage), color = "black",
            shape = 21, size = 1.0, alpha = 0.8, stroke = 0.2) +
    scale_fill_manual(values = wohnlage_farben) +
    labs(color = "Wohnlage") +
    guides(fill = guide_legend(override.aes = list(color = NA))) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.title = element_blank()
    ) +
    coord
}

# Funktion für die zentralen Karten (ohne teilen)
karte_zentral_fehl <- function(point_data) {
  ggplot() +
    # Hintergrundflächen mit tatsächlicher Wohnlage
    geom_sf(data = wohnlagen_muc_zentral, aes(fill = Wohnlage), color = "white", size = 0.1) +
    # Fehlklassifikationen als Punkte mit Füllung nach Vorhersage, schwarzer Rand
    geom_sf(data = wohnlage_grenzen_zentral, color = "black", size = 0.05)+
    geom_sf(
      data = point_data,
      aes(fill = Wohnlage_vorhersage),
      color = "lightgrey",        # schwarzer Rand
      shape = 21,             # Punkt mit Rand & Füllung
      size = 1.5,
      alpha = 0.95,
      stroke = 0.3)+            # Randdicke +
    # Farbskalen – beide mit derselben Palette
    scale_fill_manual(values = wohnlage_farben) +
    # Nur eine Legende für die Flächen
    guides(
      fill = guide_legend(override.aes = list(color = NA)))+  # verhindert schwarzen Rand in Legende + 
    labs(fill = "Wohnlage") +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.title = element_blank())
}



# Modell zentral
karte_model_gam_zentral <- karte_zentral_fehl(fehler_model_gam_zentral)
karte_model_gam_zentral
ggsave("plots/karten/karte_model_gam_zentral.png",
       plot = karte_model_gam_zentral,
       width = 8, height = 6, dpi = 300)

# mit prior
karte_model_gam_zentral_prior <- karte_zentral_fehl(fehler_model_gam_zentral_prior)
karte_model_gam_zentral_prior
ggsave("plots/karten/karte_model_gam_zentral_prior.png",
       plot = karte_model_gam_zentral_prior,
       width = 8, height = 6, dpi = 300)


# Modell außerhalb
karte_nw_fehl2 <- karte_teil_fehl(point_data = fehler_model_gam_ausserhalb, bbox_nw)
karte_ne_fehl2 <- karte_teil_fehl(point_data = fehler_model_gam_ausserhalb, bbox_ne)
karte_sw_fehl2 <- karte_teil_fehl(point_data = fehler_model_gam_ausserhalb, bbox_sw)
karte_se_fehl2 <- karte_teil_fehl(point_data = fehler_model_gam_ausserhalb, bbox_se)

karte_nw_fehl2
karte_ne_fehl2 
karte_sw_fehl2 
karte_se_fehl2

# mit prior
karte_nw_fehl2_prior <- karte_teil_fehl(point_data = fehler_model_gam_ausserhalb_prior, bbox_nw)
karte_ne_fehl2_prior <- karte_teil_fehl(point_data = fehler_model_gam_ausserhalb_prior, bbox_ne)
karte_sw_fehl2_prior <- karte_teil_fehl(point_data = fehler_model_gam_ausserhalb_prior, bbox_sw)
karte_se_fehl2_prior <- karte_teil_fehl(point_data = fehler_model_gam_ausserhalb_prior, bbox_se)

karte_nw_fehl2_prior
karte_ne_fehl2_prior 
karte_sw_fehl2_prior 
karte_se_fehl2_prior














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

fehler_model_gam_kombiniert_wgs <- st_transform(fehler_model_gam_kombiniert, crs = 4326)
fehler_model_gam_prior_kombiniert_wgs <- st_transform(fehler_model_gam_prior_kombiniert, crs = 4326)

korrekt_model_gam_kombiniert_wgs <- st_transform(korrekt_model_gam_kombiniert, crs = 4326)
korrekt_model_gam_prior_kombiniert_wgs <- st_transform(korrekt_model_gam_prior_kombiniert, crs = 4326)


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
