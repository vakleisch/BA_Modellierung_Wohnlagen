# Geodatenbasierte Modellierung der Wohnlagen in München

Dieses Repository enthält alle Daten, Skripte und Ergebnisse für die Bachelorarbeit mit dem Titel "Geodatenbasierte Modellierung der Wohnlagen in München". Das Ziel der Arbeit ist es, die Münchner Wohnlagenklassifizierung anhand von Geodaten zu analysieren, die zugrundeliegenden Zusammenhänge mit statistischen Modellen zu quantifizieren und die Vorhersagegüte zu evaluieren.

Dieses Dokument dient als Anleitung, um die Analysen und Ergebnisse vollständig zu reproduzieren.

## Verzeichnisstruktur 
Das Projekt ist in die folgenden Hauptordner gegliedert:

-   **/rohdaten/**: Enthält die ursprünglichen, unveränderten Rohdatensätze.
-   **/daten/**: Enthält die Skripte zur Datenaufbereitung (`data_read.R`, `data_edit.R`) sowie die daraus resultierenden, aufbereiteten Datensätze im `.RData`-Format.
-   **/modellierung/**: Beinhaltet alle zentralen R-Skripte für die statistische Analyse, von der Schätzung der GAMs (`final_gam.R`) über deren Analyse (`final_model_analysis.R`) und Evaluierung (`model_evaluation.R`) bis hin zur Erstellung der Grafiken (`fehlklassifikations_karten.R`, `plot_funktionen.R`).
-   **/modelle/**: Enthält die final geschätzten und gespeicherten GAM-Objekte im `.rds`-Format.
-   **/plots/**: Enthält alle in der Arbeit verwendeten, statischen Grafiken und Karten im `.png`-Format.
-   **/interaktive_karten/**: Enthält die als HTML-Dateien gespeicherten interaktiven Karten.

## Vorgehen zur Reproduktion der Ergebnisse
Um die Analyse von Anfang bis Ende durchzuführen, folgen Sie bitte diesen Schritten.

### Schritt 1: Pakete installieren
Führen Sie zunächst das Skript `paket_download.R` aus. Es überprüft automatisch, welche der für die Analyse benötigten R-Pakete auf Ihrem System fehlen und installiert diese anschließend.

### Schritt 2: Datensätze einlesen und verarbeiten
Führen Sie anschließend die Skripte `data_read.R` und `data_edit.R` aus um die Datensätze für die Analyse zu generieren.

### Schritt 3: Beliebige weitere Reihenfolge
Da die Modelle bereits unter `modelle` gespeichert sind, kann jede beliebige weitere Datei als nächstes ausgeführt werden. Achten Sie darauf, immer oben anzufangen, um die benötigten Pakete zu laden.
