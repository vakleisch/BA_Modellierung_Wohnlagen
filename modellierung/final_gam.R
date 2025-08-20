# Finale Modelle

library(here)
library(mgcv)
library(dplyr)
library(mgcViz)
library(data.table)
library(nnet)
library(VGAM)
source(here("daten", "data_edit.R"))
source(here("modellierung", "model_evaluation.R"))
set.seed(476)


# Modelldaten laden
load(here("daten", "model_data_zentral_complete.RData"))
load(here("daten", "model_data_ausserhalb_complete.RData"))
load(here("daten", "model_data_complete.RData"))


# 1) Flexible Modelle

# Modell für zentrale Lagen

# Modellformel (2 mal wiederholen, da 3 Kategorien)
formula_list1_zentral <- list(Wohnlage_numerisch ~ s(distanz_bahnhof, k=10, bs = "tp") +
                                s(distanz_mittelzentrum, k = 10, bs = "tp") +
                                s(opnv_index, k = 10, bs = "tp") +
                                s(distanz_unterzentrum, k = 10, bs = "tp") +
                                s(hauspreis_index,  k = 10, bs = "tp") +
                                straßentyp_gruppe + 
                                s(distanz_ubahn, k = 10, bs = "tp") +
                                s(distanz_bushaltestelle, k = 10, bs = "tp") +
                                s(nahversorgungs_index,  k = 10, bs = "tp"),
                              ~ s(distanz_bahnhof,  k = 10, bs = "tp") +
                                s(distanz_mittelzentrum,  k = 10, bs = "tp") +
                                s(opnv_index, k = 10, bs = "tp") +
                                s(distanz_unterzentrum,  k = 10, bs = "tp") +
                                s(hauspreis_index, k = 10, bs = "tp") +
                                straßentyp_gruppe + 
                                s(distanz_ubahn,  k = 10, bs = "tp") +
                                s(distanz_bushaltestelle,  k = 10, bs = "tp") +
                                s(nahversorgungs_index,  k = 10, bs = "tp")
)
gam_model_zentral <- gam(
  formula = formula_list1_zentral,
  data = model_data_zentral_complete,
  family = mgcv::multinom(K = 2), # weil 3 Kategorien
  method = "ML", 
  optimizer = "efs",
  control = gam.control(trace = TRUE, keepData = FALSE) # reduziert Größe 
)

# Modell speichern
# saveRDS(gam_model_zentral, file = "modelle/gam_model_zentral.rds")



# Modell für Lagen außerhalb

# Modellformel (2 mal wiederholen, da 3 Kategorien)
formula_list1_ausserhalb <- list(Wohnlage_numerisch ~ s(distanz_bahnhof, k=11, bs= "cr") +
                                   s(distanz_mittelzentrum, k = 11, bs = "cr") +
                                   s(opnv_index, k = 11, bs = "cr") +
                                   s(distanz_unterzentrum, k = 11, bs = "cr") +
                                   s(hauspreis_index,  k = 11, bs = "cr") +
                                   straßentyp_gruppe + 
                                   s(distanz_ubahn, k = 11, bs = "cr") +
                                   s(distanz_bushaltestelle, k = 11, bs = "cr") +
                                   s(nahversorgungs_index,  k = 11, bs = "cr"),
                                 ~ s(distanz_bahnhof,  k = 11, bs = "cr") +
                                   s(distanz_mittelzentrum,  k = 11, bs = "cr") +
                                   s(opnv_index, k = 11, bs = "cr") +
                                   s(distanz_unterzentrum,  k = 11, bs = "cr") +
                                   s(hauspreis_index, k = 11, bs = "cr") +
                                   straßentyp_gruppe + 
                                   s(distanz_ubahn,  k = 11, bs = "cr") +
                                   s(distanz_bushaltestelle,  k = 11, bs = "cr") +
                                   s(nahversorgungs_index,  k = 11, bs = "cr")
)

gam_model_ausserhalb <- gam(
  formula = formula_list1_ausserhalb,
  data = model_data_ausserhalb_complete,
  family = mgcv::multinom(K = 2), # weil 3 Kategorien
  method = "ML",
  select = TRUE,
  optimizer = "efs",
  control = gam.control(trace = TRUE, keepData = FALSE) # reduziert Größe 
)

# Modell speichern
# saveRDS(gam_model_ausserhalb, file = "modelle/gam_model_ausserhalb.rds")




# 2) Zentrales GAM ohne distanz_bahnhof fitten
formula_list2_zentral <- list(Wohnlage_numerisch ~ s(distanz_mittelzentrum, k = 10, bs = "tp") +
                                s(opnv_index, k = 10, bs = "tp") +
                                s(distanz_unterzentrum, k = 10, bs = "tp") +
                                s(hauspreis_index,  k = 10, bs = "tp") +
                                straßentyp_gruppe + 
                                s(distanz_ubahn, k = 10, bs = "tp") +
                                s(distanz_bushaltestelle, k = 10, bs = "tp") +
                                s(nahversorgungs_index,  k = 10, bs = "tp"),
                              ~ s(distanz_mittelzentrum,  k = 10, bs = "tp") +
                                s(opnv_index, k = 10, bs = "tp") +
                                s(distanz_unterzentrum,  k = 10, bs = "tp") +
                                s(hauspreis_index, k = 10, bs = "tp") +
                                straßentyp_gruppe + 
                                s(distanz_ubahn,  k = 10, bs = "tp") +
                                s(distanz_bushaltestelle,  k = 10, bs = "tp") +
                                s(nahversorgungs_index,  k = 10, bs = "tp")
)


gam_model_zentral_ohne <- gam(
  formula = formula_list2_zentral,
  data = model_data_zentral_complete,
  family = mgcv::multinom(K = 2), # weil 3 Kategorien
  method = "ML", 
  optimizer = "efs",
  control = gam.control(trace = TRUE, keepData = FALSE) # reduziert Größe 
)

# Modell speichern
# saveRDS(gam_model_zentral_ohne, file = "modelle/gam_model_zentral_ohne.rds")

