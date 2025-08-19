# EDA

library(here)

source(here("daten", "data_edit.R"))

library(ggplot2)
library(data.table)
library(dplyr)


# Farbpalette für die Wohnlagen

wohnlage_farben <- c(
  "durchschnittliche Lage (außerhalb)" = "#e8f5a4",
  "gute Lage (außerhalb)" = "#afe391",
  "beste Lage (außerhalb)" = "#7FCDBB",
  "zentrale durchschnittliche Lage" = "#41B6C4",
  "zentrale gute Lage" = "#1f5a82",
  "zentrale beste Lage" = "#271352"
)



# 1) 
# Visualisierung der aktuellen Wohnlagen in München
# Wohnlagenkarte plotten
karte_muc <- ggplot(wohnlagen_muc2) +
  geom_sf(aes(fill = Wohnlage), color = "white", size = 0.1) +
  scale_fill_manual(values = wohnlage_farben) +
  theme_minimal() +
  labs(fill = "Wohnlage")+
  theme_minimal() +
  theme(
    axis.text = element_blank(),       # entfernt Gradzahlen
    axis.ticks = element_blank(),      # entfernt Achsenticks
    panel.grid = element_blank(),      # entfernt Gitterlinien
    panel.border = element_blank(),    # entfernt Rahmen
    axis.title = element_blank())       # entfernt "Longitude"/"Latitude"

# mit Grenzen
karte_muc_grenzen<- ggplot(wohnlagen_muc2) +
  geom_sf(aes(fill = Wohnlage), color = "white", size = 0.1) +
  geom_sf(data = wohnlage_grenzen, color = "black", size = 0.05) +
  scale_fill_manual(values = wohnlage_farben) +
  theme_minimal() +
  labs(fill = "Wohnlage") +
  theme(
    axis.text = element_blank(),       # entfernt Gradzahlen
    axis.ticks = element_blank(),      # entfernt Achsenticks
    panel.grid = element_blank(),      # entfernt Gitterlinien
    panel.border = element_blank(),    # entfernt Rahmen
    axis.title = element_blank()) 

# data_final: Jede Wohnung mit Wohnlage als Karte visualisieren
karte_punkte <- ggplot(data = data_with_classified_wohnlage) +
  geom_sf(aes(color = Wohnlage), size = 0.1, alpha = 0.3) +
  geom_sf(data = wohnlage_grenzen, color = "black", size = 0.05)+
  scale_color_manual(values = wohnlage_farben) +
  labs(
    fill = "Wohnlage",
    color = "Wonlage",
  ) +
  guides(
    color = guide_legend(override.aes = list(alpha = 1, size = 2))  # Punkt in Legende ohne Transparenz
  ) +
  theme_minimal()+
  theme(
    axis.text = element_blank(),       # entfernt Gradzahlen
    axis.ticks = element_blank(),      # entfernt Achsenticks
    panel.grid = element_blank(),      # entfernt Gitterlinien
    panel.border = element_blank(),    # entfernt Rahmen
    axis.title = element_blank())

# Extra: Alle Wohnungen ohne Wohnlage visualisieren
karte_na <- ggplot(data = data_without_wohnlage) +
  geom_sf(color = "grey50", size = 0.5, alpha = 0.5) +
  geom_sf(data = wohnlage_grenzen, color = "black", size = 0.05)+
  theme_minimal() +
  theme(
    axis.text = element_blank(),       # entfernt Gradzahlen
    axis.ticks = element_blank(),      # entfernt Achsenticks
    panel.grid = element_blank(),      # entfernt Gitterlinien
    panel.border = element_blank(),    # entfernt Rahmen
    axis.title = element_blank())


# Barplot nach Gesamtfläche der jeweiligen Lagen
ggplot(wohnlage_flaeche, aes(y = Wohnlage, x = as.numeric(gesamtflaeche), fill = Wohnlage)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = wohnlage_farben) +
  labs(title = "Gesamtfläche je Wohnlage (in m²)",
       y = "Wohnlage",
       x = "Fläche (m²)") +
  theme_minimal() +
  theme(legend.position = "none")

# Anteile der Lagen in der Stadt insgesamt 
head(wohnlage_flaeche)

# Barplot nach Fläche der jeweiligen Lagen (in Prozent)
wohnlagen_anteil_muc <- ggplot(wohnlage_flaeche, aes(y = Wohnlage, x = anteil/100, fill = Wohnlage)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = wohnlage_farben)  +
  labs(
    y = "Wohnlage",
    x = "Räumlicher Anteil") +
  theme_minimal() +
  theme(legend.position = "none")+
  theme(text = element_text(size = 16.5))


# Plot nach Fläche der jeweiligen Lagen (in Prozent) - zentrale Lagen
wohnlagen_anteil_muc_zentral <- ggplot(wohnlage_flaeche_zentral, aes(y = Wohnlage, x = anteil/100, fill = Wohnlage)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = wohnlage_farben) +
  labs(
    y = "Wohnlage",
    x = "Räumlicher Anteil (zentrale Lagen)") +
  theme_minimal() +
  theme(legend.position = "none")+
  theme(text = element_text(size = 16.5))


# Plot nach Flächen der jeweiligen Lagen (in Prozent) - Lagen außerhalb
wohnlagen_anteil_muc_ausserhalb <- ggplot(wohnlage_flaeche_ausserhalb, aes(y = Wohnlage, x = anteil/100, fill = Wohnlage)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = wohnlage_farben) +
  labs(
    y = "Wohnlage",
    x = "Räumlicher Anteil (Lagen außerhalb)") +
  theme_minimal() +
  theme(legend.position = "none")+
  theme(text = element_text(size = 16.5))


# Absolute Häufigkeiten der Wohnlagen im Datensatz
ggplot(data_final_short, aes(x = Wohnlage, fill = Wohnlage)) +
  geom_bar() +
  scale_fill_manual(values = wohnlage_farben) +
  labs(title = "Anzahl der Fälle je Wohnlage (i360)",
       x = "Wohnlage",
       y = "Anzahl") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip()

# Relative Häufigkeiten der Wohnlagen im rohen Datensatz
wohnlagen_anteil_daten_roh <- ggplot(data_final, aes(x = Wohnlage, fill = Wohnlage)) +
  geom_bar(aes(y = (..count..) / sum(..count..)), position = "dodge") +
  scale_fill_manual(values = wohnlage_farben) +
  labs(
    x = "Wohnlage",
    y = "Relative Häufigkeit im rohen Datensatz") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip() +
  theme(text = element_text(size = 16.5))


# Relative Häufigkeiten der Wohnlagen im finalen Datensatz
load(here("daten", "model_data_complete.RData"))
wohnlagen_anteil_daten <- ggplot(model_data_complete, aes(x = Wohnlage, fill = Wohnlage)) +
  geom_bar(aes(y = (..count..) / sum(..count..)), position = "dodge") +
  scale_fill_manual(values = wohnlage_farben) +
  labs(
    x = "Wohnlage",
    y = "Relative Häufigkeit im finalen Datensatz") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip() +
  theme(text = element_text(size = 16.5))


# Relative Häufigkeiten der Wohnlagen im finalen Datensatz (zentrale Lagen)
load(here("daten", "model_data_zentral_complete.RData")) # Finalen Datensatz laden

wohnlagen_anteil_daten_zentral <- ggplot(model_data_zentral_complete, aes(x = Wohnlage, fill = Wohnlage)) +
  geom_bar(aes(y = (..count..) / sum(..count..)), position = "dodge") +
  scale_fill_manual(values = wohnlage_farben) +
  labs(
    x = "Wohnlage",
    y = "Relative Häufigkeit im finalen Datensatz") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip() +
  theme(text = element_text(size = 16.5))


# Relative Häufigkeiten der Wohnlagen im Datensatz (Lagen außerhalb)
load(here("daten", "model_data_ausserhalb_complete.RData")) # Finalen Datensatz laden

wohnlagen_anteil_daten_ausserhalb <- ggplot(model_data_ausserhalb_complete, aes(x = Wohnlage, fill = Wohnlage)) +
  geom_bar(aes(y = (..count..) / sum(..count..)), position = "dodge") +
  scale_fill_manual(values = wohnlage_farben) +
  labs(
    x = "Wohnlage",
    y = "Relative Häufigkeit im finalen Datensatz") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip() +
  theme(text = element_text(size = 16.5))




# 2) 
# Deskriptive Analyse mit model_data_zentral_complete (Finaler Datensatz)
options(scipen=999) # unterdrückt wissenschaftliche Notation

# Daten laden
load(here("daten", "model_data_complete.RData"))
load(here("daten", "model_data_zentral_complete.RData"))
load(here("daten", "model_data_ausserhalb_complete.RData"))

# Funktion, um Häufigkeiten zu erzeugen (für geom_text & straßentyp)
create_abs_counts <- function(data, var) {
  data %>%
    count({{ var }}) %>%
    rename(label = n)
}

# straßentyp (davor)
abs_counts_1 <- create_abs_counts(model_data_mit_stra_complete, straßentyp)

ggplot(model_data_mit_stra_complete, aes(x = straßentyp, fill = Wohnlage)) +
  geom_bar(color = "black", position = "fill") +
  geom_text(data = abs_counts_1, aes(x = straßentyp, y = 1.05, label = label),
            inherit.aes = FALSE, hjust = 0, size = 4) +
  scale_fill_manual(values = wohnlage_farben) +
  labs(x = "Straßentyp", y = "Proportion", fill = "Wohnlage") +
  coord_flip(clip = "off") +
  theme_minimal() +
  theme(legend.position = "None",
        text = element_text(size = 16.5),
        plot.margin = margin(5.5, 30, 5.5, 5.5))


# straßentyp (danach)
abs_counts_2 <- create_abs_counts(model_data_complete, straßentyp_gruppe)

ggplot(model_data_complete, aes(x = straßentyp_gruppe, fill = Wohnlage)) +
  geom_bar(color = "black", position = "fill") +
  geom_text(data = abs_counts_2, aes(x = straßentyp_gruppe, y = 1.05, label = label),
            inherit.aes = FALSE, hjust = 0, size = 4) +
  scale_fill_manual(values = wohnlage_farben) +
  labs(x = "Straßentyp", y = "Proportion", fill = "Wohnlage") +
  coord_flip(clip = "off") +
  theme_minimal() +
  theme(legend.position = "None",
        text = element_text(size = 16.5),
        plot.margin = margin(5.5, 30, 5.5, 5.5))


# straßentyp - nur zentral
# danach
abs_counts_3 <- create_abs_counts(model_data_zentral_complete, straßentyp_gruppe)

straßentyp_danach_zentral <- ggplot(model_data_zentral_complete, aes(x = straßentyp_gruppe, fill = Wohnlage)) +
  geom_bar(color = "black", position = "fill") +
  geom_text(data = abs_counts_3, aes(x = straßentyp_gruppe, y = 1.05, label = label),
            inherit.aes = FALSE, hjust = 0, size = 4) +
  scale_fill_manual(values = wohnlage_farben) +
  labs(x = "Straßentyp", y = "Proportion", fill = "Wohnlage") +
  coord_flip(clip = "off") +
  theme_minimal() +
  theme(legend.position = "None",
        text = element_text(size = 16.5),
        plot.margin = margin(5.5, 30, 5.5, 5.5))


# davor
abs_counts_4 <- create_abs_counts(model_data_mit_stra_zentral_complete, straßentyp)

straßentyp_davor_zentral <- ggplot(model_data_mit_stra_zentral_complete, aes(x = straßentyp, fill = Wohnlage)) +
  geom_bar(color = "black", position = "fill") +
  geom_text(data = abs_counts_4, aes(x = straßentyp, y = 1.05, label = label),
            inherit.aes = FALSE, hjust = 0, size = 4) +
  scale_fill_manual(values = wohnlage_farben) +
  labs(x = "Straßentyp", y = "Proportion", fill = "Wohnlage") +
  coord_flip(clip = "off") +
  theme_minimal() +
  theme(legend.position = "None",
        text = element_text(size = 16.5),
        plot.margin = margin(5.5, 30, 5.5, 5.5))


# straßentyp - nur außerhalb
# danach
abs_counts_5 <- create_abs_counts(model_data_ausserhalb_complete, straßentyp_gruppe)

straßentyp_danach_ausserhalb <- ggplot(model_data_ausserhalb_complete, aes(x = straßentyp_gruppe, fill = Wohnlage)) +
  geom_bar(color = "black", position = "fill") +
  geom_text(data = abs_counts_5, aes(x = straßentyp_gruppe, y = 1.05, label = label),
            inherit.aes = FALSE, hjust = 0, size = 4) +
  scale_fill_manual(values = wohnlage_farben) +
  labs(x = "Straßentyp", y = "Proportion", fill = "Wohnlage") +
  coord_flip(clip = "off") +
  theme_minimal() +
  theme(legend.position = "None",
        text = element_text(size = 16.5),
        plot.margin = margin(5.5, 30, 5.5, 5.5))


# davor
abs_counts_6 <- create_abs_counts(model_data_ausserhalb_mit_stra_complete, straßentyp)

straßentyp_davor_ausserhalb <- ggplot(model_data_ausserhalb_mit_stra_complete, aes(x = straßentyp, fill = Wohnlage)) +
  geom_bar(color = "black", position = "fill") +
  geom_text(data = abs_counts_6, aes(x = straßentyp, y = 1.05, label = label),
            inherit.aes = FALSE, hjust = 0, size = 4) +
  scale_fill_manual(values = wohnlage_farben) +
  labs(x = "Straßentyp", y = "Proportion", fill = "Wohnlage") +
  coord_flip(clip = "off") +
  theme_minimal() +
  theme(legend.position = "None",
        text = element_text(size = 16.5),
        plot.margin = margin(5.5, 30, 5.5, 5.5))



# Verteilung der metrischen Variablen 

# distanz_bahnhof 
ggplot(model_data_complete, aes(x = distanz_bahnhof)) +
  geom_density(adjust = 4, color = "black", linewidth = 1) +
  labs(x = "Distanz zum Bahnhof", y = "Dichte") +
  theme_minimal()+
  theme(text = element_text(size = 16.5))

# distanz_bahnhof (nur zentrale Lagen) 
ggplot(model_data_zentral_complete, aes(x = distanz_bahnhof)) +
  geom_density(adjust = 4, color = "black", linewidth = 1) +
  labs(x = "Distanz zum Bahnhof", y = "Dichte") +
  theme_minimal()+
  theme(text = element_text(size = 16.5))

# distanz_bahnhof (nur Lagen außerhalb) 
ggplot(model_data_ausserhalb_complete, aes(x = distanz_bahnhof)) +
  geom_density(adjust = 4, color = "black", linewidth = 1) +
  labs(x = "Distanz zum Bahnhof", y = "Dichte") +
  theme_minimal()+
  theme(text = element_text(size = 16.5))

# distanz_bahnhof (alle Lagen) - unterteilt nach Wohnlage
dbb<- ggplot(model_data_complete, aes(x = distanz_bahnhof, fill = Wohnlage)) +
  geom_density(adjust = 4, alpha = 0.5) +
  scale_fill_manual(values = wohnlage_farben) +
  labs(x = "Distanz zum Bahnhof", y = "Dichte") +
  theme_minimal()+
  theme(text = element_text(size = 16.5))

# distanz_bahnhof (zentrale Lagen) - unterteilt nach Wohnlage
dbz <- ggplot(model_data_zentral_complete, aes(x = distanz_bahnhof, fill = Wohnlage)) +
  geom_density(adjust = 4, alpha = 0.6) +
  scale_fill_manual(values = wohnlage_farben) +
  labs(x = "Distanz zum Bahnhof", y = "Dichte") +
  theme_minimal()+
  theme(text = element_text(size = 16.5))

# distanz_bahnhof (Lagen außerhalb) - unterteilt nach Wohnlage
dba <- ggplot(model_data_ausserhalb_complete, aes(x = distanz_bahnhof, fill = Wohnlage)) +
  geom_density(adjust = 4, alpha = 0.6) +
  scale_fill_manual(values = wohnlage_farben) +
  labs(x = "Distanz zum Bahnhof", y = "Dichte") +
  theme_minimal()+
  theme(text = element_text(size = 16.5))

# distanz_ubahn
# distanz_ubahn (zentrale Lagen) - unterteilt nach Wohnlage
duz <- ggplot(model_data_zentral_complete, aes(x = distanz_ubahn, fill = Wohnlage)) +
  geom_density(adjust = 4, alpha = 0.6) +
  scale_fill_manual(values = wohnlage_farben) +
  labs(x = "Distanz zur U-Bahn", y = "Dichte") +
  theme_minimal()+
  theme(text = element_text(size = 16.5))

# distanz_ubahn (Lagen außerhalb) - unterteilt nach Wohnlage
dua <- ggplot(model_data_ausserhalb_complete, aes(x = distanz_ubahn, fill = Wohnlage)) +
  geom_density(adjust = 4, alpha = 0.6) +
  scale_fill_manual(values = wohnlage_farben) +
  labs(x = "Distanz zur U-Bahn", y = "Dichte") +
  theme_minimal()+
  theme(text = element_text(size = 16.5))

# distanz_bushaltestelle
# distanz_bushaltestelle (zentrale Lagen) - unterteilt nach Wohnlage
dbuh <- ggplot(model_data_zentral_complete, aes(x = distanz_bushaltestelle, fill = Wohnlage)) +
  geom_density(adjust = 4, alpha = 0.6) +
  scale_fill_manual(values = wohnlage_farben) +
  labs(x = "Distanz zur Bushaltestelle", y = "Dichte") +
  theme_minimal()+
  theme(text = element_text(size = 16.5))

# distanz_bushaltestelle (Lagen außerhalb) - unterteilt nach Wohnlage
dbauh <- ggplot(model_data_ausserhalb_complete, aes(x = distanz_bushaltestelle, fill = Wohnlage)) +
  geom_density(adjust = 4, alpha = 0.6) +
  scale_fill_manual(values = wohnlage_farben) +
  labs(x = "Distanz zur Bushaltestelle", y = "Dichte") +
  theme_minimal()+
  theme(text = element_text(size = 16.5))

# distanz_unterzentrum
# distanz_unterzentrum (zentrale Lagen) - unterteilt nach Wohnlage
duz <- ggplot(model_data_zentral_complete, aes(x = distanz_unterzentrum, fill = Wohnlage)) +
  geom_density(adjust = 4, alpha = 0.6) +
  scale_fill_manual(values = wohnlage_farben) +
  labs(x = "Distanz zum Unterzentrum", y = "Dichte") +
  theme_minimal()+
  theme(text = element_text(size = 16.5))

# distanz_unterzentrum (Lagen außerhalb) - unterteilt nach Wohnlage
dua <- ggplot(model_data_ausserhalb_complete, aes(x = distanz_unterzentrum, fill = Wohnlage)) +
  geom_density(adjust = 4, alpha = 0.6) +
  scale_fill_manual(values = wohnlage_farben) +
  labs(x = "Distanz zum Unterzentrum", y = "Dichte") +
  theme_minimal()+
  theme(text = element_text(size = 16.5))

# distanz_mittelzentrum
# distanz_mittelzentrum (zentrale Lagen) - unterteilt nach Wohnlage
dmz <- ggplot(model_data_zentral_complete, aes(x = distanz_mittelzentrum, fill = Wohnlage)) +
  geom_density(adjust = 4, alpha = 0.6) +
  scale_fill_manual(values = wohnlage_farben) +
  labs(x = "Distanz zum Mittelzentrum", y = "Dichte") +
  theme_minimal()+
  theme(text = element_text(size = 16.5))

# distanz_mittelzentrum (Lagen außerhalb) - unterteilt nach Wohnlage
dma <- ggplot(model_data_ausserhalb_complete, aes(x = distanz_mittelzentrum, fill = Wohnlage)) +
  geom_density(adjust = 4, alpha = 0.6) +
  scale_fill_manual(values = wohnlage_farben) +
  labs(x = "Distanz zum Mittelzentrum", y = "Dichte") +
  theme_minimal()+
  theme(text = element_text(size = 16.5))

# distanz_bushaltestelle
# distanz_bushaltestelle (zentrale Lagen) - unterteilt nach Wohnlage
dbh <- ggplot(model_data_zentral_complete, aes(x = distanz_bushaltestelle, fill = Wohnlage)) +
  geom_density(adjust = 4, alpha = 0.6) +
  scale_fill_manual(values = wohnlage_farben) +
  labs(x = "Distanz zur Bushaltestelle", y = "Dichte") +
  theme_minimal()+
  theme(text = element_text(size = 16.5))

# distanz_bushaltestelle (Lagen außerhalb) - unterteilt nach Wohnlage
dbah <- ggplot(model_data_ausserhalb_complete, aes(x = distanz_bushaltestelle, fill = Wohnlage)) +
  geom_density(adjust = 4, alpha = 0.6) +
  scale_fill_manual(values = wohnlage_farben) +
  labs(x = "Distanz zur Bushaltestelle", y = "Dichte") +
  theme_minimal()+
  theme(text = element_text(size = 16.5))



# Verteilung Index Variablen 
# nahversorgungs_index (zentrale Lagen) - unterteilt nach Wohnlage
niz <- ggplot(model_data_zentral_complete, aes(x = nahversorgungs_index, fill = Wohnlage)) +
  geom_density(adjust = 4, alpha = 0.6) +
  labs(x = "Nahversorgungsindex", y = "Dichte") +
  scale_fill_manual(values = wohnlage_farben) +
  theme_minimal()+
  theme(text = element_text(size = 16.5))

# nahversorgungs_index (Lagen außerhalb) - unterteilt nach Wohnlage
nia <- ggplot(model_data_ausserhalb_complete, aes(x = nahversorgungs_index, fill = Wohnlage)) +
  geom_density(adjust = 4, alpha = 0.6) +
  scale_fill_manual(values = wohnlage_farben) +
  labs(x = "Nahversorgungsindex", y = "Dichte") +
  theme_minimal()+
  theme(text = element_text(size = 16.5))

# Unterschiede der Mittelwerte von nahversorgungs_index
mean(model_data_zentral_complete$nahversorgungs_index, na.rm = TRUE)
mean(model_data_ausserhalb_complete$nahversorgungs_index, na.rm = TRUE)

model_data_zentral_complete %>%
  group_by(Wohnlage) %>%
  summarise(mean_nahversorgungs_index = mean(nahversorgungs_index, na.rm = TRUE))

# Hauspreisindex
# hauspreis_index (zentrale Lagen) - unterteilt nach Wohnlage
hiz <- ggplot(model_data_zentral_complete, aes(x = hauspreis_index, fill = Wohnlage)) +
  geom_density(adjust = 4, alpha = 0.6) +
  labs(x = "Hauspreisindex", y = "Dichte") +
  scale_fill_manual(values = wohnlage_farben) +
  geom_vline(xintercept = 100, color = "red", linetype = "dashed", linewidth = 1) +
  theme_minimal()+
  theme(text = element_text(size = 16.5))

# hauspreis_index (Lagen außerhalb) - unterteilt nach Wohnlage
hia <- ggplot(model_data_ausserhalb_complete, aes(x = hauspreis_index, fill = Wohnlage)) +
  geom_density(adjust = 4, alpha = 0.6) +
  scale_fill_manual(values = wohnlage_farben) +
  labs(x = "Hauspreisindex", y = "Dichte") +
  geom_vline(xintercept = 100, color = "red", linetype = "dashed", size = 1) +
  theme_minimal()+
  theme(text = element_text(size = 16.5))

# Ist der Hauspreisindex abhängig von der Grundfläche? 
cor(data_final_short$grundfläche, data_final_short$hauspreis_index)

# Ist der Hauspreisindex abhängig von der Wohnlfläche? 
data_final_short %>%
  group_by(wohnfläche_klasse) %>%
  summarise(durchschnitt_index = mean(hauspreis_index, na.rm = TRUE))

# zentral
data_final_short_zentral %>%
  group_by(wohnfläche_klasse) %>%
  summarise(durchschnitt_index = mean(hauspreis_index, na.rm = TRUE))

# nicht_zentral
data_final_short_ausserhalb %>%
  group_by(wohnfläche_klasse) %>%
  summarise(durchschnitt_index = mean(hauspreis_index, na.rm = TRUE))

# Haben gewisse Wohnlagen eine größere Wohnflächen? 
data_final_short %>%
  group_by(Wohnlage) %>%
  summarise(durchschnitt_index = mean(as.numeric(wohnfläche_klasse), na.rm = TRUE))
data_final_short %>%
  group_by(Wohnlage) %>%
  summarise(durchschnitt_index = median(as.numeric(wohnfläche_klasse), na.rm = TRUE))


# opnv_index
# opnv_index (zentrale Lagen) - unterteilt nach Wohnlage
oiz <- ggplot(model_data_zentral_complete, aes(x = opnv_index, fill = Wohnlage)) +
  geom_density(adjust = 5, alpha = 0.6) +
  labs(x = "ÖPNV-Index", y = "Dichte") +
  scale_fill_manual(values = wohnlage_farben) +
  theme_minimal()+
  theme(text = element_text(size = 16.5))

# opnv_index (Lagen außerhalb) - unterteilt nach Wohnlage
oia <- ggplot(model_data_ausserhalb_complete, aes(x = opnv_index, fill = Wohnlage)) +
  geom_density(adjust = 5, alpha = 0.6) +
  labs(x = "ÖPNV-Index", y = "Dichte") +
  scale_fill_manual(values = wohnlage_farben) +
  theme_minimal()+
  theme(text = element_text(size = 16.5))

