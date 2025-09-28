

library(here)
library(mgcv)
library(dplyr)
library(mgcViz)
library(data.table)
library(nnet)
library(VGAM)
source(here("daten", "data_edit.R"))
source(here("modellierung", "model_evaluation.R"))
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
options(scipen = 999)
set.seed(476)


# Modelldaten laden
load(here("daten", "model_data_zentral_complete.RData"))
load(here("daten", "model_data_ausserhalb_complete.RData"))
load(here("daten", "model_data_complete.RData"))

# Keine Trennung in Zentral und Ausserhalb
model_data_simple <- rbind(model_data_zentral_complete,
                           model_data_ausserhalb_complete)

formula_list <- list(Wohnlage_numerisch ~ s(distanz_bahnhof, k=7, bs = "cr") +
                       s(distanz_mittelzentrum, k = 7, bs = "cr") +
                       s(opnv_index, k = 7, bs = "cr") +
                       s(distanz_unterzentrum, k = 7, bs = "cr") +
                       s(hauspreis_index,  k = 7, bs = "cr") +
                       straßentyp_gruppe + 
                       s(distanz_ubahn, k = 7, bs = "cr") +
                       s(distanz_bushaltestelle, k = 7, bs = "cr") +
                       s(nahversorgungs_index,  k = 7, bs = "cr"),
                     ~ s(distanz_bahnhof,  k = 7, bs = "cr") +
                       s(distanz_mittelzentrum,  k = 7, bs = "cr") +
                       s(opnv_index, k = 7, bs = "cr") +
                       s(distanz_unterzentrum,  k = 7, bs = "cr") +
                       s(hauspreis_index, k = 7, bs = "cr") +
                       straßentyp_gruppe + 
                       s(distanz_ubahn,  k = 7, bs = "cr") +
                       s(distanz_bushaltestelle,  k = 7, bs = "cr") +
                       s(nahversorgungs_index,  k = 7, bs = "cr")
)
gam_model_zusammen <- gam(
  formula = formula_list,
  data = model_data_simple,
  family = mgcv::multinom(K = 2), # weil 3 Kategorien
  method = "ML", 
  optimizer = "efs",
  control = gam.control(trace = TRUE, keepData = FALSE) # reduziert Größe 
)

 saveRDS(gam_model_zusammen, file = "modelle/gam_model_zusammen.rds")

evaluate_confusion_matrix(gam_model_zusammen, 
                          test_data = model_data_simple,
                          y_col = "Wohnlage_numerisch")
evaluate_confusion_matrix_equal_priors(gam_model_zusammen, 
                                       test_data = model_data_simple,
                                       y_col = "Wohnlage_numerisch")

# To do: schauen ob auch zentrale Lagen erkannt

# unbereinigt um prior

fehler_model_gam_zusammen <- missclassification_data(gam_model_zusammen, 
                                                     data = model_data_simple,
                                                     predict_fun = predict_labels_discr)

korrekt_model_gam_zusammen <- korrekte_vorhersagen(gam_model_zusammen,
                                                   data = model_data_simple,
                                                   predict_fun = predict_labels_discr)
table(fehler_model_gam_zusammen$Wohnlage)
table(korrekt_model_gam_zusammen$Wohnlage)

# außerhalb
table(fehler_model_gam_zusammen$Wohnlage)[1:3]/c(42186, 41144, 5859)
table(korrekt_model_gam_zusammen$Wohnlage)[1:3]/c(42186, 41144, 5859)

# zentral
table(fehler_model_gam_zusammen$Wohnlage)[4:6]/c(932, 2761, 527)
table(korrekt_model_gam_zusammen$Wohnlage)[4:6]/c(932, 2761, 527)


# bereinigt um prior
fehler_model_gam_zusammen <- missclassification_data(gam_model_zusammen, 
                                                     data = model_data_simple,
                                                     predict_fun = predict_labels_equal_priors)

korrekt_model_gam_zusammen <- korrekte_vorhersagen(gam_model_zusammen,
                                                   data = model_data_simple,
                                                   predict_fun = predict_labels_equal_priors)
table(fehler_model_gam_zusammen$Wohnlage)
table(korrekt_model_gam_zusammen$Wohnlage)

# außerhalb
table(fehler_model_gam_zusammen$Wohnlage)[1:3]/c(42186, 41144, 5859)
table(korrekt_model_gam_zusammen$Wohnlage)[1:3]/c(42186, 41144, 5859)

# zentral
table(fehler_model_gam_zusammen$Wohnlage)[4:6]/c(932, 2761, 527)
table(korrekt_model_gam_zusammen$Wohnlage)[4:6]/c(932, 2761, 527)
