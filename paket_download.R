# Ausführen, um alle benötigten Pakete runter zuladen

# --- Paket-Management ---

# Liste aller benötigten Pakete
packages <- c(
  "rgl", "here", "ggplot2", "patchwork", "dplyr", "leaflet", 
  "osmdata", "htmlwidgets", "tmaptools", "foreign", "haven", 
  "yardstick", "caret", "gratia", "corrplot", "car", "mgcv", 
  "reshape2", "mgcViz", "confintr", "rcompanion", "forcats", 
  "stringr", "nnet", "VGAM", "sf"
)

# Überprüfe, welche Pakete noch nicht installiert sind
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]

# Installiere die fehlenden Pakete
if(length(new_packages)) {
  install.packages(new_packages)
}

# ----------------------------------------------
# Sicherstellen, dass sf installiert ist (MAC)
# ----------------------------------------------

if (!requireNamespace("sf", quietly = TRUE)) {
  message("\nDas Paket 'sf' ist nicht installiert.")
  
  if (Sys.info()[["sysname"]] == "Darwin") {  # Darwin = macOS
    message("Hinweis für macOS-User:\n",
            "1. Installiere die Xcode Command Line Tools:\n",
            "   xcode-select --install\n",
            "2. Installiere Homebrew (falls nicht vorhanden):\n",
            "   /bin/bash -c \"$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)\"\n",
            "3. Installiere GDAL, PROJ und GEOS:\n",
            "   brew install gdal proj geos\n",
            "4. Danach in R:\n",
            "   install.packages('sf', type = 'source')\n")
  } else {
    message("Bitte installiere das Paket 'sf' mit:\n",
            "install.packages('sf')\n")
  }
  
  stop("Das Paket 'sf' fehlt. Bitte wie oben beschrieben installieren.")
}
