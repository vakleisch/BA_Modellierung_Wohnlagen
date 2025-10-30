library(here)
source(here("modellierung", "model_evaluation.R"))
source(here("modellierung", "plot_funktionen.R"))
library(yardstick)
library(dplyr)
library(caret)
library(gratia)
library(ggplot2)
library(corrplot)
library(car)
library(mgcv)
library(reshape2)
library(mgcViz)
library(confintr)
library(rcompanion)
library(rgl)
library(sf)
options(scipen = 999) # Verhindert wissenschaftliche Notation in ggplot2



# Modelle laden
model_gam_zentral <- readRDS("modelle/gam_model_zentral.rds") 
model_gam_ausserhalb <- readRDS("modelle/gam_model_ausserhalb.rds")
model_gam_zentral_ohne_preis <- readRDS("modelle/gam_model_zentral_ohne_preis.rds")
model_gam_ausserhalb_ohne_preis <- readRDS("modelle/gam_model_ausserhalb_ohne_preis.rds")
model_gam_zentral_ohne <- readRDS("modelle/gam_model_zentral_ohne.rds")
model_gam_ausserhalb_ohne <- readRDS("modelle/gam_model_ausserhalb_ohne.rds")
model_gam_ausserhalb_ohne_preis_bahn <- readRDS("modelle/gam_model_ausserhalb_ohne_preis_bahn.rds")
model_gam_zentral_ohne_preis_bahn <- readRDS("modelle/gam_model_zentral_ohne_preis_bahn.rds")

# Daten laden
load("daten/model_data_zentral_complete.RData")
load("daten/model_data_ausserhalb_complete.RData")
load("daten/model_data_complete.RData")

# Modelloutput ansehen
summary(model_gam_zentral)
summary(model_gam_ausserhalb)
summary(model_gam_zentral_ohne)

# Modellgüte evaluieren
evaluate_confusion_matrix(model_gam_zentral, 
                          test_data = model_data_zentral_complete,
                          y_col = "Wohnlage_numerisch")
evaluate_confusion_matrix(model_gam_ausserhalb, 
                          test_data = model_data_ausserhalb_complete,
                          y_col = "Wohnlage_numerisch")
evaluate_confusion_matrix(model_gam_zentral_ohne, 
                          test_data = model_data_zentral_complete,
                          y_col = "Wohnlage_numerisch")
evaluate_confusion_matrix(model_gam_ausserhalb_ohne_preis, 
                          test_data = model_data_ausserhalb_complete,
                          y_col = "Wohnlage_numerisch")
evaluate_confusion_matrix(model_gam_zentral_ohne_preis, 
                          test_data = model_data_zentral_complete,
                          y_col = "Wohnlage_numerisch")


# Modellgüte evaluieren: Mit bereinigeter Vorhersage
evaluate_confusion_matrix_equal_priors(model_gam_zentral, 
                                       test_data = model_data_zentral_complete,
                                       y_col = "Wohnlage_numerisch")
evaluate_confusion_matrix_equal_priors(model_gam_ausserhalb, 
                                       test_data = model_data_ausserhalb_complete,
                                       y_col = "Wohnlage_numerisch")
evaluate_confusion_matrix_equal_priors(model_gam_zentral_ohne, 
                                       test_data = model_data_zentral_complete,
                                       y_col = "Wohnlage_numerisch")

# Modellgüte der neuen Modelle
# ohne huaspreisindex
evaluate_confusion_matrix(model_gam_ausserhalb_ohne_preis, 
                          test_data = model_data_ausserhalb_complete,
                          y_col = "Wohnlage_numerisch")
evaluate_confusion_matrix(model_gam_zentral_ohne_preis, 
                          test_data = model_data_zentral_complete,
                          y_col = "Wohnlage_numerisch")

# ohne bahnhof
evaluate_confusion_matrix(model_gam_ausserhalb_ohne, 
                          test_data = model_data_ausserhalb_complete,
                          y_col = "Wohnlage_numerisch")
evaluate_confusion_matrix(model_gam_zentral_ohne, 
                          test_data = model_data_zentral_complete,
                          y_col = "Wohnlage_numerisch")

# ohne hauspreis und bahnhof
evaluate_confusion_matrix(model_gam_ausserhalb_ohne_preis_bahn, 
                          test_data = model_data_ausserhalb_complete,
                          y_col = "Wohnlage_numerisch")
evaluate_confusion_matrix(model_gam_zentral_ohne_preis_bahn, 
                          test_data = model_data_zentral_complete,
                          y_col = "Wohnlage_numerisch")


# neue modelle mit prior anpassung
# ohne huaspreisindex
evaluate_confusion_matrix_equal_priors(model_gam_ausserhalb_ohne_preis, 
                          test_data = model_data_ausserhalb_complete,
                          y_col = "Wohnlage_numerisch")
evaluate_confusion_matrix_equal_priors(model_gam_zentral_ohne_preis, 
                          test_data = model_data_zentral_complete,
                          y_col = "Wohnlage_numerisch")

# ohne bahnhof
evaluate_confusion_matrix_equal_priors(model_gam_ausserhalb_ohne, 
                          test_data = model_data_ausserhalb_complete,
                          y_col = "Wohnlage_numerisch")
evaluate_confusion_matrix_equal_priors(model_gam_zentral_ohne, 
                          test_data = model_data_zentral_complete,
                          y_col = "Wohnlage_numerisch")

# ohne hauspreis und bahnhof
evaluate_confusion_matrix_equal_priors(model_gam_ausserhalb_ohne_preis_bahn, 
                          test_data = model_data_ausserhalb_complete,
                          y_col = "Wohnlage_numerisch")
evaluate_confusion_matrix_equal_priors(model_gam_zentral_ohne_preis_bahn, 
                          test_data = model_data_zentral_complete,
                          y_col = "Wohnlage_numerisch")


# Modelle auf falsche Pukte anwenden
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
probs_zentral <- predict(model_gam_zentral,
                 newdata = fehler_model_gam_zentral, type = "response")
probs_ausserhalb <- predict(model_gam_ausserhalb,
                     newdata = fehler_model_gam_ausserhalb, type = "response")
probs_zentral_ohne <- predict(model_gam_zentral_ohne,
                 newdata = fehler_model_gam_zentral_ohne, type = "response")
probs_ausserhalb_ohne <- predict(model_gam_ausserhalb_ohne,
                     newdata = fehler_model_gam_ausserhalb_ohne, type = "response")
probs_zentral_ohne_preis <- predict(model_gam_zentral_ohne_preis,
                          newdata = fehler_model_gam_zentral_ohne_preis, type = "response")
probs_ausserhalb_ohne_preis <- predict(model_gam_ausserhalb_ohne_preis,
                              newdata = fehler_model_gam_ausserhalb_ohne_preis, type = "response")
probs_zentral_ohne_preis_bahn <- predict(model_gam_zentral_ohne_preis_bahn,
                                   newdata = fehler_model_gam_zentral_ohne_preis_bahn, type = "response")
probs_ausserhalb_ohne_preis_bahn <- predict(model_gam_ausserhalb_ohne_preis_bahn,
                                         newdata = fehler_model_gam_ausserhalb_ohne_preis_bahn, type = "response")


# Wie viele Wert werden mit großer confidence falsch zugeordnet?
anteil_ueber_grenze <- function(df, grenze = 0.7) {
  # Für jede Zeile prüfen, ob ein Wert größer als die Grenze ist
  any_gt <- apply(df, 1, function(x) any(x > grenze))
  
  # Anteil berechnen
  anteil <- mean(any_gt)
  
  # Ergebnis zurückgeben
  return(anteil)
}
anteil_ueber_grenze(probs_zentral, grenze = 0.7)
anteil_ueber_grenze(probs_ausserhalb, grenze = 0.7)
anteil_ueber_grenze(probs_zentral_ohne, grenze = 0.7)
anteil_ueber_grenze(probs_ausserhalb_ohne, grenze = 0.7)
anteil_ueber_grenze(probs_zentral_ohne_preis, grenze = 0.7)
anteil_ueber_grenze(probs_ausserhalb_ohne_preis, grenze = 0.7)
anteil_ueber_grenze(probs_zentral_ohne_preis_bahn, grenze = 0.7)
anteil_ueber_grenze(probs_ausserhalb_ohne_preis_bahn, grenze = 0.7)

# Boxplots der wahrscheinlichkeiten
colnames(probs_zentral) <- c("Durchschnittlich", "Gut", "Beste")
colnames(probs_ausserhalb) <- c("Durchschnittlich", "Gut", "Beste")
colnames(probs_zentral_ohne) <- c("Durchschnittlich", "Gut", "Beste")
colnames(probs_ausserhalb_ohne) <- c("Durchschnittlich", "Gut", "Beste")
colnames(probs_zentral_ohne_preis) <- c("Durchschnittlich", "Gut", "Beste")
colnames(probs_ausserhalb_ohne_preis) <- c("Durchschnittlich", "Gut", "Beste")
colnames(probs_zentral_ohne_preis_bahn) <- c("Durchschnittlich", "Gut", "Beste")
colnames(probs_ausserhalb_ohne_preis_bahn) <- c("Durchschnittlich", "Gut", "Beste")

# In data.frame umwandeln und ins long-Format bringen
df <- as.data.frame(probs_zentral)
df_long <- melt(df, variable.name = "Spalte", value.name = "Wert")

# Boxplot pro Spalte
ggplot(df_long, aes(x = Spalte, y = Wert)) +
  geom_boxplot(color = "black") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Boxplot der Klassenwahrscheinlichkeiten (zentral)",
    x = "Zentrale Wohnlagekategorie",
    y = "Klassenwahrscheinlichkeit"
  )

df <- as.data.frame(probs_ausserhalb)
df_long <- melt(df, variable.name = "Spalte", value.name = "Wert")

# Boxplot pro Spalte
ggplot(df_long, aes(x = Spalte, y = Wert)) +
  geom_boxplot(color = "black", alpha = 0.1) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Boxplot der Klassenwahrscheinlichkeiten (außerhalb)",
    x = "Wohnlagekategorie außerhalb",
    y = "Klassenwahrscheinlichkeit"
  )

# Boxplots der wahrscheinlichkeiten ohne bahnhof
df <- as.data.frame(probs_zentral_ohne)
df_long <- melt(df, variable.name = "Spalte", value.name = "Wert")

# Boxplot pro Spalte
ggplot(df_long, aes(x = Spalte, y = Wert)) +
  geom_boxplot(color = "black") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Boxplot der Klassenwahrscheinlichkeiten (zentral)",
    x = "Zentrale Wohnlagekategorie",
    y = "Klassenwahrscheinlichkeit"
  )

df <- as.data.frame(probs_ausserhalb_ohne)
df_long <- melt(df, variable.name = "Spalte", value.name = "Wert")

# Boxplot pro Spalte
ggplot(df_long, aes(x = Spalte, y = Wert)) +
  geom_boxplot(color = "black", alpha = 0.1) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Boxplot der Klassenwahrscheinlichkeiten (außerhalb)",
    x = "Wohnlagekategorie außerhalb",
    y = "Klassenwahrscheinlichkeit"
  )

# Boxplots der wahrscheinlichkeiten ohne Hauspreisindex
df <- as.data.frame(probs_zentral_ohne_preis)
df_long <- melt(df, variable.name = "Spalte", value.name = "Wert")

# Boxplot pro Spalte
ggplot(df_long, aes(x = Spalte, y = Wert)) +
  geom_boxplot(color = "black") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Boxplot der Klassenwahrscheinlichkeiten (zentral)",
    x = "Zentrale Wohnlagekategorie",
    y = "Klassenwahrscheinlichkeit"
  )

df <- as.data.frame(probs_ausserhalb_ohne_preis)
df_long <- melt(df, variable.name = "Spalte", value.name = "Wert")

# Boxplot pro Spalte
ggplot(df_long, aes(x = Spalte, y = Wert)) +
  geom_boxplot(color = "black", alpha = 0.1) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Boxplot der Klassenwahrscheinlichkeiten (außerhalb)",
    x = "Wohnlagekategorie außerhalb",
    y = "Klassenwahrscheinlichkeit"
  )

# jetzt mit angepasster priori
predict1 <- function(model,
                                        newdata,
                                        number_categories = 3,
                                        y_col = "Wohnlage_numerisch") {
  # Matrix mit P(y = k | x) vom Modell
  probs <- predict(model, newdata = newdata, type = "response")
  
  # Levels sichern
  all_levels <- as.character(0:(number_categories - 1))
  colnames(probs) <- all_levels
  
  # Modellklassenverteilung (durchschnittliche Wahrscheinlichkeit je Klasse)
  prior_model <- colMeans(probs)
  
  # Gleichverteilung als neue Prior (z.B. 1/3 für jede Klasse bei 3 Klassen)
  prior_uniform <- rep(1 / number_categories, number_categories)
  
  # Wahrscheinlichkeiten anpassen: adjusted = probs / prior_model * prior_uniform
  adjusted <- sweep(probs, 2, prior_model, "/")
  adjusted <- sweep(adjusted, 2, prior_uniform, "*")
  adjusted_probs <- adjusted / rowSums(adjusted)
  
  # Noch einmal sicherstellen, dass colnames stimmen
  colnames(adjusted_probs) <- c("Durchschnittlich", "Gut", "Beste")
  return(adjusted_probs)
}

probs_zentral <- predict1(model_gam_zentral,
                         newdata = fehler_model_gam_zentral)
probs_ausserhalb <- predict1(model_gam_ausserhalb,
                            newdata = fehler_model_gam_ausserhalb)
probs_zentral_ohne <- predict1(model_gam_zentral_ohne,
                              newdata = fehler_model_gam_zentral_ohne)
probs_ausserhalb_ohne <- predict1(model_gam_ausserhalb_ohne,
                                 newdata = fehler_model_gam_ausserhalb_ohne)
probs_zentral_ohne_preis <- predict1(model_gam_zentral_ohne_preis,
                                    newdata = fehler_model_gam_zentral_ohne_preis)
probs_ausserhalb_ohne_preis <- predict1(model_gam_ausserhalb_ohne_preis,
                                       newdata = fehler_model_gam_ausserhalb_ohne_preis)
probs_zentral_ohne_preis_bahn <- predict1(model_gam_zentral_ohne_preis_bahn,
                                         newdata = fehler_model_gam_zentral_ohne_preis_bahn)
probs_ausserhalb_ohne_preis_bahn <- predict1(model_gam_ausserhalb_ohne_preis_bahn,
                                            newdata = fehler_model_gam_ausserhalb_ohne_preis_bahn)

df <- as.data.frame(probs_zentral)
df_long <- melt(df, variable.name = "Spalte", value.name = "Wert")

# Boxplot pro Spalte
ggplot(df_long, aes(x = Spalte, y = Wert)) +
  geom_boxplot(color = "black") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Boxplot der Klassenwahrscheinlichkeiten (zentral)",
    x = "Zentrale Wohnlagekategorie",
    y = "Klassenwahrscheinlichkeit"
  )

df <- as.data.frame(probs_ausserhalb)
df_long <- melt(df, variable.name = "Spalte", value.name = "Wert")

# Boxplot pro Spalte
ggplot(df_long, aes(x = Spalte, y = Wert)) +
  geom_boxplot(color = "black", alpha = 0.1) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Boxplot der Klassenwahrscheinlichkeiten (außerhalb)",
    x = "Wohnlagekategorie außerhalb",
    y = "Klassenwahrscheinlichkeit"
  )

# Boxplots der wahrscheinlichkeiten ohne bahnhof
df <- as.data.frame(probs_zentral_ohne)
df_long <- melt(df, variable.name = "Spalte", value.name = "Wert")

# Boxplot pro Spalte
ggplot(df_long, aes(x = Spalte, y = Wert)) +
  geom_boxplot(color = "black") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Boxplot der Klassenwahrscheinlichkeiten (zentral)",
    x = "Zentrale Wohnlagekategorie",
    y = "Klassenwahrscheinlichkeit"
  )

df <- as.data.frame(probs_ausserhalb_ohne)
df_long <- melt(df, variable.name = "Spalte", value.name = "Wert")

# Boxplot pro Spalte
ggplot(df_long, aes(x = Spalte, y = Wert)) +
  geom_boxplot(color = "black", alpha = 0.1) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Boxplot der Klassenwahrscheinlichkeiten (außerhalb)",
    x = "Wohnlagekategorie außerhalb",
    y = "Klassenwahrscheinlichkeit"
  )

# Boxplots der wahrscheinlichkeiten ohne Hauspreisindex
df <- as.data.frame(probs_zentral_ohne_preis)
df_long <- melt(df, variable.name = "Spalte", value.name = "Wert")

# Boxplot pro Spalte
ggplot(df_long, aes(x = Spalte, y = Wert)) +
  geom_boxplot(color = "black") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Boxplot der Klassenwahrscheinlichkeiten (zentral)",
    x = "Zentrale Wohnlagekategorie",
    y = "Klassenwahrscheinlichkeit"
  )

df <- as.data.frame(probs_ausserhalb_ohne_preis)
df_long <- melt(df, variable.name = "Spalte", value.name = "Wert")

# Boxplot pro Spalte
ggplot(df_long, aes(x = Spalte, y = Wert)) +
  geom_boxplot(color = "black", alpha = 0.1) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Boxplot der Klassenwahrscheinlichkeiten (außerhalb)",
    x = "Wohnlagekategorie außerhalb",
    y = "Klassenwahrscheinlichkeit"
  )






# Auffääoge punkte testen
library(sf)
library(dplyr)

# 1) die 5 auffälligen Koordinaten (Lon/Lat WGS84)
coords_wgs <- data.frame(
  lon = c(11.46421, 11.46424, 11.46429, 11.46428, 11.46434),
  lat = c(48.14176, 48.14171, 48.14156, 48.14138, 48.14118)
)

# 2) als sf-Punkte mit WGS84 anlegen
coords_sf_wgs <- st_as_sf(coords_wgs, coords = c("lon", "lat"), crs = 4326)

# 3) prüfe das CRS deines Datensatzes (soll ETRS89 / UTM zone 32N sein)
st_crs(model_data_ausserhalb_complete)

# 4) in das CRS des Datensatzes transformieren
target_crs <- st_crs(model_data_ausserhalb_complete)
coords_sf_proj <- st_transform(coords_sf_wgs, crs = target_crs)

# Optional: zeige die projizierten Koordinaten (x,y in Meter)
coords_xy <- st_coordinates(coords_sf_proj)
coords_xy_df <- cbind(coords_sf_proj, as.data.frame(coords_xy))
print(coords_xy_df)

# 5) Für jeden transformierten Punkt das nächstliegende Feature im Datensatz finden
# st_nearest_feature(x, y) gibt für jedes x den Index des nächsten y zurück
nearest_idx <- st_nearest_feature(coords_sf_proj, model_data_ausserhalb_complete)

# 6) Distanz zum jeweils gefundenen nächsten Punkt bestimmen (in Meter)
dists <- st_distance(coords_sf_proj, model_data_ausserhalb_complete[nearest_idx, ], by_element = TRUE)
print(dists)  # Kontrolle, wie groß die Abstände sind

# 7) Filter: nur akzeptieren, wenn Abstand < z.B. 2 Meter (anpassen falls nötig)
tolerance_m <- units::set_units(2, "m")  # 2 Meter Toleranz
valid_mask <- as.numeric(dists) <= as.numeric(tolerance_m)

# 8) die Indices der gültigen Treffer
valid_indices <- nearest_idx[valid_mask]

# 9) neuen Datensatz erstellen mit genau diesen Punkten
auffaellige_punkte <- model_data_ausserhalb_complete %>%
  slice(unique(valid_indices))

# 10) Ergebnis prüfen
print(auffaellige_punkte)
# falls du die Koordinaten als Spalten sehen willst:
auffaellige_punkte <- auffaellige_punkte %>%
  mutate(x = st_coordinates(geometry)[,1],
         y = st_coordinates(geometry)[,2])
print(st_drop_geometry(auffaellige_punkte))  # ohne geometry anzeigen


# Klassenwahrscheinlichkeiten der Punkte
pred_auffällige_punkte <- predict(model_gam_ausserhalb_ohne_preis,
                                  newdata = auffaellige_punkte, type = "response")
# 1️⃣ deine sf-Objekte (auffaellige_punkte) von UTM auf WGS84 umrechnen
auffaellige_punkte_wgs <- st_transform(auffaellige_punkte, crs = 4326)

# 2️⃣ Längen- und Breitengrad auslesen
coords_wgs <- st_coordinates(auffaellige_punkte_wgs)

# 3️⃣ mit Predictions zusammenführen
pred_auffällige_punkte_df <- cbind(
  st_drop_geometry(auffaellige_punkte_wgs),
  lon = coords_wgs[, 1],
  lat = coords_wgs[, 2],
  round(pred_auffällige_punkte, 4)
)

# 4️⃣ prüfen
print(pred_auffällige_punkte_df)
# Stelle sicher, dass dein Dataframe existiert
# (pred_auffällige_punkte_df enthält jetzt lon/lat + predictions)

pred_auffällige_punkte_df <- pred_auffällige_punkte_df %>%
  rename(
    prob_durchschnittliche_Lage = 1,
    prob_gute_Lage = 2,
    prob_beste_Lage = 3
  )

save(pred_auffällige_punkte_df, file = "pred_auffaellige_punkte_df.RData")
load("pred_auffaellige_punkte_df.RData")

# jetzt mit angepasster priori
probs <- pred_auffällige_punkte 


# Levels sichern
all_levels <- as.character(0:2)
colnames(probs) <- all_levels

# Modellklassenverteilung (durchschnittliche Wahrscheinlichkeit je Klasse)
prior_model <- colMeans(probs)

# Gleichverteilung als neue Prior (z.B. 1/3 für jede Klasse bei 3 Klassen)
prior_uniform <- rep(1 / 3, 3)

# Wahrscheinlichkeiten anpassen: adjusted = probs / prior_model * prior_uniform
adjusted <- sweep(probs, 2, prior_model, "/")
adjusted <- sweep(adjusted, 2, prior_uniform, "*")
adjusted_probs <- adjusted / rowSums(adjusted)

# Noch einmal sicherstellen, dass colnames stimmen
colnames(adjusted_probs) <- all_levels

# Vorhergesagte Klassen (als Strings)
predicted_classes <- apply(adjusted_probs, 1, function(x) all_levels[which.max(x)])






# Log Likelihood der Modelle vergeleichen: näher an 0 ist besser
# Freiheitsgrade: Komplexität des Modells
logLik.gam(model_gam_zentral)
logLik.gam(model_gam_ausserhalb)
logLik.gam(model_gam_zentral_ohne)

# AIC = -2 * Log-Likelihood + 2 * df, näher an 0 ist besser
AIC(model_gam_zentral, 
    model_gam_ausserhalb,
    model_gam_zentral_ohne)



# Korrelation (zentral) mit distanz_bahnhof
variablen <- c(
  "distanz_bahnhof", "distanz_ubahn", "distanz_bushaltestelle",
  "distanz_mittelzentrum", "distanz_unterzentrum", "opnv_index",
  "nahversorgungs_index", "hauspreis_index")

# Wähle nur numerische Prädiktoren aus Datensatz
df_korr <- model_data_zentral_complete %>%
  select(all_of(variablen)) %>% 
  st_drop_geometry() # Geometrien droppen

# Korrelationsmatrix berechnen
korr_matrix <- cor(df_korr, use = "complete.obs")

# Heatmap-Plot
corrplot(korr_matrix, method = "color", type = "upper",
         tl.col = "black", tl.cex = 0.8,
         addCoef.col = "black", 
         col = colorRampPalette(c("blue", "white", "red"))(200),
         number.cex = 0.7)


# Korrelation (zentral) ohne distanz_bahnhof
variablen <- c(
  "distanz_ubahn", "distanz_bushaltestelle",
  "distanz_mittelzentrum", "distanz_unterzentrum", "opnv_index",
  "nahversorgungs_index", "hauspreis_index")

# Wähle nur numerische Prädiktoren aus Datensatz
df_korr <- model_data_zentral_complete %>%
  select(all_of(variablen)) %>% 
  st_drop_geometry() # geometrien droppen

# Korrelationsmatrix berechnen
korr_matrix <- cor(df_korr, use = "complete.obs")

# Heatmap-Plot
corrplot(korr_matrix, method = "color", type = "upper",
         tl.col = "black", tl.cex = 0.8,
         addCoef.col = "black", # Korrelationen anzeigen
         col = colorRampPalette(c("blue", "white", "red"))(200),
         number.cex = 0.7)



#  Korrelation (außerhalb)
variablen <- c(
  "distanz_bahnhof", "distanz_ubahn", "distanz_bushaltestelle",
  "distanz_mittelzentrum", "distanz_unterzentrum", "opnv_index",
  "nahversorgungs_index", "hauspreis_index")

# Wähle nur numerische Prädiktoren aus Datensatz
df_korr <- model_data_ausserhalb_complete %>%
  select(all_of(variablen)) %>%
  st_drop_geometry() # geometrien droppen

# Korrelationsmatrix berechnen
korr_matrix <- cor(df_korr, use = "complete.obs")

# Heatmap-Plot
corrplot(korr_matrix, method = "color", type = "upper",
         tl.col = "black", tl.cex = 0.8,
         addCoef.col = "black", # Korrelationen anzeigen
         col = colorRampPalette(c("blue", "white", "red"))(200),
         number.cex = 0.7)



# Berechne die Konkurvität der Modelle zur genaueren Überprüfung
# 'full = FALSE' gibt eine übersichtlichere Zusammenfassung
con_results <- concurvity(model_gam_zentral, full = FALSE)
con_results_aus <- concurvity(model_gam_ausserhalb, full = FALSE)

# Ergebnisse
con_results
con_results_aus


# Partielle Effekte:
# Wie verändert sich Vorhersage bei Änderung eines Prädiktors,
# wenn alle anderen konstant gehalten werden?
# y-Achse: log odds gegenüber referenzkategorie (durchschnittliche Lage)
visualize_part_effects(model_gam_zentral, "part_eff", subfolder_name = "part_effects_zent")
visualize_part_effects(model_gam_ausserhalb, "part_eff", subfolder_name = "part_effects_aus")
visualize_part_effects(model_gam_zentral_ohne, "part_eff", subfolder_name = "part_effects_zentral_ohne")
visualize_part_effects(model_gam_ausserhalb_ohne_preis, "part_eff",
                       subfolder_name = "part_effects_ausserhalb_ohne_preis")
visualize_part_effects(model_gam_zentral_ohne_preis, "part_eff",
                       subfolder_name = "part_effects_zentral_ohne_preis")

# Effekte der kategorialen Variable straßentyp (Odds-Ratios)
# Zentral
 visualize_odds_ratios(model_gam_zentral, file_name = "Oddsratio_plots/OR_model_gam_zentral.png")
 visualize_odds_ratios(model_gam_zentral_ohne, file_name = "Oddsratio_plots/OR_model_gam_zentral_ohne.png")

# Außerhalb
visualize_odds_ratios(model_gam_ausserhalb, file_name = "Oddsratio_plots/OR_model_gam_ausserhalb.png")
vis


# Odds-Ratios Plot mit anderen Dimesnsionen
visualize_odds_ratios(model_gam_zentral, file_name = "Oddsratio_plots/OR_model_gam_zentral_klein.png",
                      width = 6, height = 6, fontsize = 18)
visualize_odds_ratios(model_gam_zentral_ohne, file_name = "Oddsratio_plots/OR_model_gam_zentral_ohne.png",
                      width = 6, height = 6, fontsize = 18)


# Bei einigen partiellen Effektplots die Skala anpassen
# distanz_unterzentrum
p <- draw(model_gam_zentral, select = "s(distanz_unterzentrum)", partial_match = TRUE) + 
  theme_minimal() +
  labs(
    title = NULL,               # Entfernt den Haupttitel
    x = "Distanz zum Unterzentrum",  # Setzt die neue X-Achsen-Beschriftung
    y = "Partieller Effekt"
  ) +
  scale_y_continuous(limits = c(-12, 25)) + # Setzt die Y-Achsen-Grenzen
  geom_hline(yintercept = 0, color = "red", linetype = 2) + # fügt eine vertikale rot gestrichelte Linie bei x=0 hinzu
  theme(
    axis.title.x = element_text(size = 15), # Setzt die Schriftgröße der X-Achsen-Beschriftung
    axis.title.y = element_text(size = 15),  # Setzt die Schriftgröße der Y-Achsen-Beschriftung
    axis.text = element_text(size = 14)
  )

ggsave("plots/part_effects_zentral_ohne/vergleich/part_eff_s(distanz_unterzentrum).png", 
       plot = p, width = 7, height = 6)

# ohne
p <- draw(model_gam_zentral_ohne, select = "s(distanz_unterzentrum)", partial_match = TRUE) + 
  theme_minimal() +
  labs(
    title = NULL,               # Entfernt den Haupttitel
    x = "Distanz zum Unterzentrum",  # Setzt die neue X-Achsen-Beschriftung
    y = "Partieller Effekt"
  ) +
  scale_y_continuous(limits = c(-12, 25)) + # Setzt die Y-Achsen-Grenzen
  geom_hline(yintercept = 0, color = "red", linetype = 2) + # fügt eine vertikale rot gestrichelte Linie bei x=0 hinzu
  theme(
    axis.title.x = element_text(size = 15), # Setzt die Schriftgröße der X-Achsen-Beschriftung
    axis.title.y = element_text(size = 15),  # Setzt die Schriftgröße der Y-Achsen-Beschriftung
    axis.text = element_text(size = 14)
  )

ggsave("plots/part_effects_zentral_ohne/part_eff_s(distanz_unterzentrum).png", 
       plot = p, width = 7, height = 6)

p <- draw(model_gam_zentral, select = "s.1(distanz_unterzentrum)", partial_match = TRUE) + 
  theme_minimal() +
  labs(
    title = NULL,               # Entfernt den Haupttitel
    x = "Distanz zum Unterzentrum",  # Setzt die neue X-Achsen-Beschriftung
    y = "Partieller Effekt"
  ) +
  scale_y_continuous(limits = c(-7.5, 30)) + # Setzt die Y-Achsen-Grenzen
  geom_hline(yintercept = 0, color = "red", linetype = 2) + # fügt eine vertikale rot gestrichelte Linie bei x=0 hinzu
  theme(
    axis.title.x = element_text(size = 15), # Setzt die Schriftgröße der X-Achsen-Beschriftung
    axis.title.y = element_text(size = 15),  # Setzt die Schriftgröße der Y-Achsen-Beschriftung
    axis.text = element_text(size = 14)
  )

ggsave("plots/part_effects_zentral_ohne/vergleich/part_eff_s.1(distanz_unterzentrum).png", 
       plot = p, width = 7, height = 6)

# ohne
p <- draw(model_gam_zentral_ohne, select = "s.1(distanz_unterzentrum)", partial_match = TRUE) + 
  theme_minimal() +
  labs(
    title = NULL,               # Entfernt den Haupttitel
    x = "Distanz zum Unterzentrum",  # Setzt die neue X-Achsen-Beschriftung
    y = "Partieller Effekt"
  ) +
  scale_y_continuous(limits = c(-7.5, 30)) + # Setzt die Y-Achsen-Grenzen
  geom_hline(yintercept = 0, color = "red", linetype = 2) + # fügt eine vertikale rot gestrichelte Linie bei x=0 hinzu
  theme(
    axis.title.x = element_text(size = 15), # Setzt die Schriftgröße der X-Achsen-Beschriftung
    axis.title.y = element_text(size = 15),  # Setzt die Schriftgröße der Y-Achsen-Beschriftung
    axis.text = element_text(size = 14)
  )

ggsave("plots/part_effects_zentral_ohne/part_eff_s.1(distanz_unterzentrum).png", 
       plot = p, width = 7, height = 6)


# opnv_index
p <- draw(model_gam_zentral, select = "s(opnv_index)", partial_match = TRUE) + 
  theme_minimal() +
  labs(
    title = NULL,               # Entfernt den Haupttitel
    x = "ÖPNV-Index",  # Setzt die neue X-Achsen-Beschriftung
    y = "Partieller Effekt"
  ) +
  scale_y_continuous(limits = c(-8, 5)) + # Setzt die Y-Achsen-Grenzen
  geom_hline(yintercept = 0, color = "red", linetype = 2) + # fügt eine vertikale rot gestrichelte Linie bei x=0 hinzu
  theme(
    axis.title.x = element_text(size = 15), # Setzt die Schriftgröße der X-Achsen-Beschriftung
    axis.title.y = element_text(size = 15),  # Setzt die Schriftgröße der Y-Achsen-Beschriftung
    axis.text = element_text(size = 14)
  )

ggsave("plots/part_effects_zentral_ohne/vergleich/part_eff_s(opnv_index).png", 
       plot = p, width = 7, height = 6)

# ohne
p <- draw(model_gam_zentral_ohne, select = "s(opnv_index)", partial_match = TRUE) + 
  theme_minimal() +
  labs(
    title = NULL,               # Entfernt den Haupttitel
    x = "ÖPNV-Index",  # Setzt die neue X-Achsen-Beschriftung
    y = "Partieller Effekt"
  ) +
  scale_y_continuous(limits = c(-8, 5)) + # Setzt die Y-Achsen-Grenzen
  geom_hline(yintercept = 0, color = "red", linetype = 2) + # fügt eine vertikale rot gestrichelte Linie bei x=0 hinzu
  theme(
    axis.title.x = element_text(size = 15), # Setzt die Schriftgröße der X-Achsen-Beschriftung
    axis.title.y = element_text(size = 15),  # Setzt die Schriftgröße der Y-Achsen-Beschriftung
    axis.text = element_text(size = 14)
  )

ggsave("plots/part_effects_zentral_ohne/part_eff_s(opnv_index).png", 
       plot = p, width = 7, height = 6)

p <- draw(model_gam_zentral, select = "s.1(opnv_index)", partial_match = TRUE) + 
  theme_minimal() +
  labs(
    title = NULL,               # Entfernt den Haupttitel
    x = "ÖPNV-Index",  # Setzt die neue X-Achsen-Beschriftung
    y = "Partieller Effekt"
  ) +
  scale_y_continuous(limits = c(-8.5, 11.5)) + # Setzt die Y-Achsen-Grenzen
  geom_hline(yintercept = 0, color = "red", linetype = 2) + # fügt eine vertikale rot gestrichelte Linie bei x=0 hinzu
  theme(
    axis.title.x = element_text(size = 15), # Setzt die Schriftgröße der X-Achsen-Beschriftung
    axis.title.y = element_text(size = 15),  # Setzt die Schriftgröße der Y-Achsen-Beschriftung
    axis.text = element_text(size = 14)
  )

ggsave("plots/part_effects_zentral_ohne/vergleich/part_eff_s.1(opnv_index).png", 
       plot = p, width = 7, height = 6)

# ohne
p <- draw(model_gam_zentral_ohne, select = "s.1(opnv_index)", partial_match = TRUE) + 
  theme_minimal() +
  labs(
    title = NULL,               # Entfernt den Haupttitel
    x = "ÖPNV-Index",  # Setzt die neue X-Achsen-Beschriftung
    y = "Partieller Effekt"
  ) +
  scale_y_continuous(limits = c(-8.5, 11.5)) + # Setzt die Y-Achsen-Grenzen
  geom_hline(yintercept = 0, color = "red", linetype = 2) + # fügt eine vertikale rot gestrichelte Linie bei x=0 hinzu
  theme(
    axis.title.x = element_text(size = 15), # Setzt die Schriftgröße der X-Achsen-Beschriftung
    axis.title.y = element_text(size = 15),  # Setzt die Schriftgröße der Y-Achsen-Beschriftung
    axis.text = element_text(size = 14)
  )

ggsave("plots/part_effects_zentral_ohne/part_eff_s.1(opnv_index).png", 
       plot = p, width = 7, height = 6)




# Nahversorgungsindex
p <- draw(model_gam_zentral, select = "s(nahversorgungs_index)", partial_match = TRUE) + 
  theme_minimal() +
  labs(
    title = NULL,               # Entfernt den Haupttitel
    x = "Nahversorgungsindex",  # Setzt die neue X-Achsen-Beschriftung
    y = "Partieller Effekt"
  ) +
  scale_y_continuous(limits = c(-15, 10)) + # Setzt die Y-Achsen-Grenzen
  geom_hline(yintercept = 0, color = "red", linetype = 2) + # fügt eine vertikale rot gestrichelte Linie bei x=0 hinzu
  theme(
    axis.title.x = element_text(size = 15), # Setzt die Schriftgröße der X-Achsen-Beschriftung
    axis.title.y = element_text(size = 15),  # Setzt die Schriftgröße der Y-Achsen-Beschriftung
    axis.text = element_text(size = 14)
  )

ggsave("plots/part_effects_zentral_ohne/vergleich/part_eff_s(nahversorgungs_index).png", 
       plot = p, width = 7, height = 6)

# ohne
p <- draw(model_gam_zentral_ohne, select = "s(nahversorgungs_index)", partial_match = TRUE) + 
  theme_minimal() +
  labs(
    title = NULL,               # Entfernt den Haupttitel
    x = "Nahversorgungsindex",  # Setzt die neue X-Achsen-Beschriftung
    y = "Partieller Effekt"
  ) +
  scale_y_continuous(limits = c(-15, 10)) + # Setzt die Y-Achsen-Grenzen
  geom_hline(yintercept = 0, color = "red", linetype = 2) + # fügt eine vertikale rot gestrichelte Linie bei x=0 hinzu
  theme(
    axis.title.x = element_text(size = 15), # Setzt die Schriftgröße der X-Achsen-Beschriftung
    axis.title.y = element_text(size = 15),  # Setzt die Schriftgröße der Y-Achsen-Beschriftung
    axis.text = element_text(size = 14)
  )

ggsave("plots/part_effects_zentral_ohne/part_eff_s(nahversorgungs_index).png", 
       plot = p, width = 7, height = 6)

p <- draw(model_gam_zentral, select = "s.1(nahversorgungs_index)", partial_match = TRUE) + 
  theme_minimal() +
  labs(
    title = NULL,               # Entfernt den Haupttitel
    x = "Nahversorgungsindex",  # Setzt die neue X-Achsen-Beschriftung
    y = "Partieller Effekt"
  ) +
  scale_y_continuous(limits = c(-15, 10)) + # Setzt die Y-Achsen-Grenzen
  geom_hline(yintercept = 0, color = "red", linetype = 2) + # fügt eine vertikale rot gestrichelte Linie bei x=0 hinzu
  theme(
    axis.title.x = element_text(size = 15), # Setzt die Schriftgröße der X-Achsen-Beschriftung
    axis.title.y = element_text(size = 15),  # Setzt die Schriftgröße der Y-Achsen-Beschriftung
    axis.text = element_text(size = 14)
  )

ggsave("plots/part_effects_zentral_ohne/vergleich/part_eff_s.1(nahversorgungs_index).png", 
       plot = p, width = 7, height = 6)

# ohne
p <- draw(model_gam_zentral_ohne, select = "s.1(nahversorgungs_index)", partial_match = TRUE) + 
  theme_minimal() +
  labs(
    title = NULL,               # Entfernt den Haupttitel
    x = "Nahversorgungsindex",  # Setzt die neue X-Achsen-Beschriftung
    y = "Partieller Effekt"
  ) +
  scale_y_continuous(limits = c(-15, 10)) + # Setzt die Y-Achsen-Grenzen
  geom_hline(yintercept = 0, color = "red", linetype = 2) + # fügt eine vertikale rot gestrichelte Linie bei x=0 hinzu
  theme(
    axis.title.x = element_text(size = 15), # Setzt die Schriftgröße der X-Achsen-Beschriftung
    axis.title.y = element_text(size = 15),  # Setzt die Schriftgröße der Y-Achsen-Beschriftung
    axis.text = element_text(size = 14)
  )

ggsave("plots/part_effects_zentral_ohne/part_eff_s.1(nahversorgungs_index).png", 
       plot = p, width = 7, height = 6)




# Kapitel 5
# distanz_bahnhof 
p <- draw(model_gam_zentral, select = "s(distanz_bahnhof)", partial_match = TRUE) + 
  theme_minimal() +
  labs(
    title = NULL,               # Entfernt den Haupttitel
    x = "Distanz zum Bahnhof",  # Setzt die neue X-Achsen-Beschriftung
    y = "Partieller Effekt"
  ) +
  scale_y_continuous(limits = c(-25, 83)) + # Setzt die Y-Achsen-Grenzen
  geom_hline(yintercept = 0, color = "red", linetype = 2) + # fügt eine vertikale rot gestrichelte Linie bei x=0 hinzu
  theme(
    axis.title.x = element_text(size = 15), # Setzt die Schriftgröße der X-Achsen-Beschriftung
    axis.title.y = element_text(size = 15),  # Setzt die Schriftgröße der Y-Achsen-Beschriftung
    axis.text = element_text(size = 14)
  )
ggsave("plots/part_effects_zent/part_eff_s(distanz_bahnhof).png", 
       plot = p, width = 7, height = 6)

p <- draw(model_gam_zentral, select = "s.1(distanz_bahnhof)", partial_match = TRUE) + 
  theme_minimal() +
  labs(
    title = NULL,               # Entfernt den Haupttitel
    x = "Distanz zum Bahnhof",  # Setzt die neue X-Achsen-Beschriftung
    y = "Partieller Effekt"
  ) +
  scale_y_continuous(limits = c(-25, 83)) + # Setzt die Y-Achsen-Grenzen
  geom_hline(yintercept = 0, color = "red", linetype = 2) + # fügt eine vertikale rot gestrichelte Linie bei x=0 hinzu
  theme(
    axis.title.x = element_text(size = 15), # Setzt die Schriftgröße der X-Achsen-Beschriftung
    axis.title.y = element_text(size = 15),  # Setzt die Schriftgröße der Y-Achsen-Beschriftung
    axis.text = element_text(size = 14)
  )
ggsave("plots/part_effects_zent/part_eff_s.1(distanz_bahnhof).png", 
       plot = p, width = 7, height = 6)

# distnaz_ubahn
p <- draw(model_gam_zentral, select = "s(distanz_ubahn)", partial_match = TRUE) + 
  theme_minimal() +
  labs(
    title = NULL,               # Entfernt den Haupttitel
    x = "Distanz zur U-Bahn",  # Setzt die neue X-Achsen-Beschriftung
    y = "Partieller Effekt"
  ) +
  scale_y_continuous(limits = c(-15, 20)) + # Setzt die Y-Achsen-Grenzen
  geom_hline(yintercept = 0, color = "red", linetype = 2) + # fügt eine vertikale rot gestrichelte Linie bei x=0 hinzu
  theme(
    axis.title.x = element_text(size = 15), # Setzt die Schriftgröße der X-Achsen-Beschriftung
    axis.title.y = element_text(size = 15),  # Setzt die Schriftgröße der Y-Achsen-Beschriftung
    axis.text = element_text(size = 14)
  )
ggsave("plots/part_effects_zent/part_eff_s(distanz_ubahn).png", 
       plot = p, width = 7, height = 6)

p <- draw(model_gam_zentral, select = "s.1(distanz_ubahn)", partial_match = TRUE) +
  theme_minimal() +
  labs(
    title = NULL,               # Entfernt den Haupttitel
    x = "Distanz zur U-Bahn",  # Setzt die neue X-Achsen-Beschriftung
    y = "Partieller Effekt"
  ) +
  scale_y_continuous(limits = c(-15, 20)) + # Setzt die Y-Achsen-Grenzen
  geom_hline(yintercept = 0, color = "red", linetype = 2) + # fügt eine vertikale rot gestrichelte Linie bei x=0 hinzu
  theme(
    axis.title.x = element_text(size = 15), # Setzt die Schriftgröße der X-Achsen-Beschriftung
    axis.title.y = element_text(size = 15),  # Setzt die Schriftgröße der Y-Achsen-Beschriftung
    axis.text = element_text(size = 14)
  )
ggsave("plots/part_effects_zent/part_eff_s.1(distanz_ubahn).png", 
       plot = p, width = 7, height = 6)

# distanz_bushaltestelle
p <- draw(model_gam_zentral, select = "s(distanz_bushaltestelle)", partial_match = TRUE) + 
  theme_minimal() +
  labs(
    title = NULL,               # Entfernt den Haupttitel
    x = "Distanz zur Bushaltestelle",  # Setzt die neue X-Achsen-Beschriftung
    y = "Partieller Effekt"
  ) +
  scale_y_continuous(limits = c(-15, 10)) + # Setzt die Y-Achsen-Grenzen
  geom_hline(yintercept = 0, color = "red", linetype = 2) + # fügt eine vertikale rot gestrichelte Linie bei x=0 hinzu
  theme(
    axis.title.x = element_text(size = 15), # Setzt die Schriftgröße der X-Achsen-Beschriftung
    axis.title.y = element_text(size = 15),  # Setzt die Schriftgröße der Y-Achsen-Beschriftung
    axis.text = element_text(size = 14)
  )
ggsave("plots/part_effects_zent/part_eff_s(distanz_bushaltestelle).png", 
       plot = p, width = 7, height = 6)
p <- draw(model_gam_zentral, select = "s.1(distanz_bushaltestelle)", partial_match = TRUE) +
  theme_minimal() +
  labs(
    title = NULL,               # Entfernt den Haupttitel
    x = "Distanz zur Bushaltestelle",  # Setzt die neue X-Achsen-Beschriftung
    y = "Partieller Effekt"
  ) +
  scale_y_continuous(limits = c(-15, 10)) + # Setzt die Y-Achsen-Grenzen
  geom_hline(yintercept = 0, color = "red", linetype = 2) + # fügt eine vertikale rot gestrichelte Linie bei x=0 hinzu
  theme(
    axis.title.x = element_text(size = 15), # Setzt die Schriftgröße der X-Achsen-Beschriftung
    axis.title.y = element_text(size = 15),  # Setzt die Schriftgröße der Y-Achsen-Beschriftung
    axis.text = element_text(size = 14)
  )
ggsave("plots/part_effects_zent/part_eff_s.1(distanz_bushaltestelle).png", 
       plot = p, width = 7, height = 6)

#distanz_unterzentrum
p <- draw(model_gam_zentral, select = "s(distanz_unterzentrum)", partial_match = TRUE) + 
  theme_minimal() +
  labs(
    title = NULL,               # Entfernt den Haupttitel
    x = "Distanz zum Unterzentrum",  # Setzt die neue X-Achsen-Beschriftung
    y = "Partieller Effekt"
  ) +
  scale_y_continuous(limits = c(-15, 15)) + # Setzt die Y-Achsen-Grenzen
  geom_hline(yintercept = 0, color = "red", linetype = 2) + # fügt eine vertikale rot gestrichelte Linie bei x=0 hinzu
  theme(
    axis.title.x = element_text(size = 15), # Setzt die Schriftgröße der X-Achsen-Beschriftung
    axis.title.y = element_text(size = 15),  # Setzt die Schriftgröße der Y-Achsen-Beschriftung
    axis.text = element_text(size = 14)
  )
ggsave("plots/part_effects_zent/part_eff_s(distanz_unterzentrum).png", 
       plot = p, width = 7, height = 6)
p <- draw(model_gam_zentral, select = "s.1(distanz_unterzentrum)", partial_match = TRUE) +
  theme_minimal() +
  labs(
    title = NULL,               # Entfernt den Haupttitel
    x = "Distanz zum Unterzentrum",  # Setzt die neue X-Achsen-Beschriftung
    y = "Partieller Effekt"
  ) +
  scale_y_continuous(limits = c(-15, 17)) + # Setzt die Y-Achsen-Grenzen
  geom_hline(yintercept = 0, color = "red", linetype = 2) + # fügt eine vertikale rot gestrichelte Linie bei x=0 hinzu
  theme(
    axis.title.x = element_text(size = 15), # Setzt die Schriftgröße der X-Achsen-Beschriftung
    axis.title.y = element_text(size = 15),  # Setzt die Schriftgröße der Y-Achsen-Beschriftung
    axis.text = element_text(size = 14)
  )
ggsave("plots/part_effects_zent/part_eff_s.1(distanz_unterzentrum).png", 
       plot = p, width = 7, height = 6)

# opnv_index
p <- draw(model_gam_zentral, select = "s(opnv_index)", partial_match = TRUE) + 
  theme_minimal() +
  labs(
    title = NULL,               # Entfernt den Haupttitel
    x = "ÖPNV-Index",  # Setzt die neue X-Achsen-Beschriftung
    y = "Partieller Effekt"
  ) +
  scale_y_continuous(limits = c(-8.5, 12.5)) + # Setzt die Y-Achsen-Grenzen
  geom_hline(yintercept = 0, color = "red", linetype = 2) + # fügt eine vertikale rot gestrichelte Linie bei x=0 hinzu
  theme(
    axis.title.x = element_text(size = 15), # Setzt die Schriftgröße der X-Achsen-Beschriftung
    axis.title.y = element_text(size = 15),  # Setzt die Schriftgröße der Y-Achsen-Beschriftung
    axis.text = element_text(size = 14)
  )
ggsave("plots/part_effects_zent/part_eff_s(opnv_index).png", 
       plot = p, width = 7, height = 6)
p <- draw(model_gam_zentral, select = "s.1(opnv_index)", partial_match = TRUE) +
  theme_minimal() +
  labs(
    title = NULL,               # Entfernt den Haupttitel
    x = "ÖPNV-Index",  # Setzt die neue X-Achsen-Beschriftung
    y = "Partieller Effekt"
  ) +
  scale_y_continuous(limits = c(-8.5, 12.5)) + # Setzt die Y-Achsen-Grenzen
  geom_hline(yintercept = 0, color = "red", linetype = 2) + # fügt eine vertikale rot gestrichelte Linie bei x=0 hinzu
  theme(
    axis.title.x = element_text(size = 15), # Setzt die Schriftgröße der X-Achsen-Beschriftung
    axis.title.y = element_text(size = 15),  # Setzt die Schriftgröße der Y-Achsen-Beschriftung
    axis.text = element_text(size = 14)
  )
ggsave("plots/part_effects_zent/part_eff_s.1(opnv_index).png", 
       plot = p, width = 7, height = 6)

# nahversaorgungs_index
p <- draw(model_gam_zentral, select = "s(nahversorgungs_index)", partial_match = TRUE) + 
  theme_minimal() +
  labs(
    title = NULL,               # Entfernt den Haupttitel
    x = "Nahversorgungsindex",  # Setzt die neue X-Achsen-Beschriftung
    y = "Partieller Effekt"
  ) +
  scale_y_continuous(limits = c(-15, 15)) + # Setzt die Y-Achsen-Grenzen
  geom_hline(yintercept = 0, color = "red", linetype = 2) + # fügt eine vertikale rot gestrichelte Linie bei x=0 hinzu
  theme(
    axis.title.x = element_text(size = 15), # Setzt die Schriftgröße der X-Achsen-Beschriftung
    axis.title.y = element_text(size = 15),  # Setzt die Schriftgröße der Y-Achsen-Beschriftung
    axis.text = element_text(size = 14)
  )
ggsave("plots/part_effects_zent/part_eff_s(nahversorgungs_index).png", 
       plot = p, width = 7, height = 6)

p <- draw(model_gam_zentral, select = "s.1(nahversorgungs_index)", partial_match = TRUE) +
  theme_minimal() +
  labs(
    title = NULL,               # Entfernt den Haupttitel
    x = "Nahversorgungsindex",  # Setzt die neue X-Achsen-Beschriftung
    y = "Partieller Effekt"
  ) +
  scale_y_continuous(limits = c(-15, 15)) + # Setzt die Y-Achsen-Grenzen
  geom_hline(yintercept = 0, color = "red", linetype = 2) + # fügt eine vertikale rot gestrichelte Linie bei x=0 hinzu
  theme(
    axis.title.x = element_text(size = 15), # Setzt die Schriftgröße der X-Achsen-Beschriftung
    axis.title.y = element_text(size = 15),  # Setzt die Schriftgröße der Y-Achsen-Beschriftung
    axis.text = element_text(size = 14)
  )
ggsave("plots/part_effects_zent/part_eff_s.1(nahversorgungs_index).png", 
       plot = p, width = 7, height = 6)

# hauspreis_index
p <- draw(model_gam_zentral, select = "s(hauspreis_index)", partial_match = TRUE) + 
  theme_minimal() +
  labs(
    title = NULL,               # Entfernt den Haupttitel
    x = "Hauspreisindex",  # Setzt die neue X-Achsen-Beschriftung
    y = "Partieller Effekt"
  ) +
  scale_y_continuous(limits = c(-10, 20)) + # Setzt die Y-Achsen-Grenzen
  geom_hline(yintercept = 0, color = "red", linetype = 2) + # fügt eine vertikale rot gestrichelte Linie bei x=0 hinzu
  theme(
    axis.title.x = element_text(size = 15), # Setzt die Schriftgröße der X-Achsen-Beschriftung
    axis.title.y = element_text(size = 15),  # Setzt die Schriftgröße der Y-Achsen-Beschriftung
    axis.text = element_text(size = 14)
  )
ggsave("plots/part_effects_zent/part_eff_s(hauspreis_index).png", 
       plot = p, width = 7, height = 6)
p <- draw(model_gam_zentral, select = "s.1(hauspreis_index)", partial_match = TRUE) +
  theme_minimal() +
  labs(
    title = NULL,               # Entfernt den Haupttitel
    x = "Hauspreisindex",  # Setzt die neue X-Achsen-Beschriftung
    y = "Partieller Effekt"
  ) +
  scale_y_continuous(limits = c(-10, 20)) + # Setzt die Y-Achsen-Grenzen
  geom_hline(yintercept = 0, color = "red", linetype = 2) + # fügt eine vertikale rot gestrichelte Linie bei x=0 hinzu
  theme(
    axis.title.x = element_text(size = 15), # Setzt die Schriftgröße der X-Achsen-Beschriftung
    axis.title.y = element_text(size = 15),  # Setzt die Schriftgröße der Y-Achsen-Beschriftung
    axis.text = element_text(size = 14)
  )
ggsave("plots/part_effects_zent/part_eff_s.1(hauspreis_index).png", 
       plot = p, width = 7, height = 6)





# distanz_bushaltestelle außerhalb
p <- draw(model_gam_ausserhalb, select = "s(distanz_bushaltestelle)", partial_match = TRUE) + 
  theme_minimal() +
  labs(
    title = NULL,               # Entfernt den Haupttitel
    x = "Distanz zur Bushaltestelle",  # Setzt die neue X-Achsen-Beschriftung
    y = "Partieller Effekt"
  ) +
  scale_y_continuous(limits = c(-4, 4)) + # Setzt die Y-Achsen-Grenzen
  geom_hline(yintercept = 0, color = "red", linetype = 2) + # fügt eine vertikale rot gestrichelte Linie bei x=0 hinzu
  theme(
    axis.title.x = element_text(size = 15), # Setzt die Schriftgröße der X-Achsen-Beschriftung
    axis.title.y = element_text(size = 15),  # Setzt die Schriftgröße der Y-Achsen-Beschriftung
    axis.text = element_text(size = 14)
  )
ggsave("plots/part_effects_aus/part_eff_s(distanz_bushaltestelle).png", 
       plot = p, width = 7, height = 6)

# hauspreis_index
p <- draw(model_gam_ausserhalb, select = "s(hauspreis_index)", partial_match = TRUE) + 
  theme_minimal() +
  labs(
    title = NULL,               # Entfernt den Haupttitel
    x = "Hauspreisindex",  # Setzt die neue X-Achsen-Beschriftung
    y = "Partieller Effekt"
  ) +
  scale_y_continuous(limits = c(-17.5, 25)) + # Setzt die Y-Achsen-Grenzen
  geom_hline(yintercept = 0, color = "red", linetype = 2) + # fügt eine vertikale rot gestrichelte Linie bei x=0 hinzu
  theme(
    axis.title.x = element_text(size = 15), # Setzt die Schriftgröße der X-Achsen-Beschriftung
    axis.title.y = element_text(size = 15),  # Setzt die Schriftgröße der Y-Achsen-Beschriftung
    axis.text = element_text(size = 14)
  )
ggsave("plots/part_effects_aus/part_eff_s(hauspreis_index).png", 
       plot = p, width = 7, height = 6)

p <- draw(model_gam_ausserhalb, select = "s.1(hauspreis_index)", partial_match = TRUE) + 
  theme_minimal() +
  labs(
    title = NULL,               # Entfernt den Haupttitel
    x = "Hauspreisindex",  # Setzt die neue X-Achsen-Beschriftung
    y = "Partieller Effekt"
  ) +
  scale_y_continuous(limits = c(-17.5, 25)) + # Setzt die Y-Achsen-Grenzen
  geom_hline(yintercept = 0, color = "red", linetype = 2) + # fügt eine vertikale rot gestrichelte Linie bei x=0 hinzu
  theme(
    axis.title.x = element_text(size = 15), # Setzt die Schriftgröße der X-Achsen-Beschriftung
    axis.title.y = element_text(size = 15),  # Setzt die Schriftgröße der Y-Achsen-Beschriftung
    axis.text = element_text(size = 14)
  )
ggsave("plots/part_effects_aus/part_eff_s.1(hauspreis_index).png", 
       plot = p, width = 7, height = 6)





# Prior der unbereingten Modelle
probs <- predict(model_gam_zentral,
                 newdata = model_data_zentral_complete,
                 type = "response")
all_levels <- as.character(0:2)
colnames(probs) <- all_levels
prior_model <- colMeans(probs)
prior_model


probs <- predict(model_gam_ausserhalb,
                 newdata = model_data_ausserhalb_complete,
                 type = "response")
all_levels <- as.character(0:2)
colnames(probs) <- all_levels
prior_model <- colMeans(probs)
prior_model
