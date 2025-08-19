# Ausführen, um alle benötigten Pakete runter zuladen

# --- Paket-Management ---

# Liste aller benötigten Pakete
packages <- c(
  "rgl", "here", "ggplot2", "sf", "patchwork", "dplyr", "leaflet", 
  "osmdata", "htmlwidgets", "tmaptools", "foreign", "haven", 
  "yardstick", "caret", "gratia", "corrplot", "car", "mgcv", 
  "reshape2", "mgcViz", "confintr", "rcompanion", "forcats", 
  "stringr", "nnet", "VGAM"
)

# Überprüfe, welche Pakete noch nicht installiert sind
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]

# Installiere die fehlenden Pakete
if(length(new_packages)) {
  install.packages(new_packages)
}
