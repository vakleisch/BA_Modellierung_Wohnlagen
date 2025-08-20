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
model_gam_zentral_ohne <- readRDS("modelle/gam_model_zentral_ohne.rds")

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



# Effekte der kategorialen Variable straßentyp (Odds-Ratios)
# Zentral
 visualize_odds_ratios(model_gam_zentral, file_name = "Oddsratio_plots/OR_model_gam_zentral.png")
 visualize_odds_ratios(model_gam_zentral_ohne, file_name = "Oddsratio_plots/OR_model_gam_zentral_ohne.png")

# Außerhalb
visualize_odds_ratios(model_gam_ausserhalb, file_name = "Oddsratio_plots/OR_model_gam_ausserhalb.png")



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
