# test modif_metric
# -----------------------------------------#
# METRIC - Accessibilité aux équipements----
#                                          #
#       Liste des packages nécessaires     #       
# -----------------------------------------#

install.packages("mapview")
install.packages("mapsf")
install.packages("leaflet")
install.packages("sf")
install.packages("openxlsx")
install.packages("descr")     # utilisation de la fonction "freq"
install.packages("crossmap")
install.packages("Hmisc")
install.packages("remotes")

# Installation du package Metric
remotes::install_gitlab(repo = 'metric-osrm/metric-osrm-package',
                        host = 'git.lab.sspcloud.fr',
                        upgrade = 'never',
                        build = TRUE,
                        dependencies = TRUE,
                        force = TRUE)

#-------------------------------------------

library(mapview)
library(mapsf)
library(leaflet)
library(sf)
library(tidyverse)
library(openxlsx)
library(descr)   
library(crossmap)
library(Hmisc) 
library(metric.osrm) 

# Adresse du serveur, ici le serveur expérimental déployé sur la plateforme 
# du SSPCloud
options(osrm.server = "https://metric-osrm-backend.lab.sspcloud.fr/") 

# On précise le profil de routage, ici "driving" car le serveur 
# SSPCloud ne propose que du calcul de trajet routier en voiture
options(osrm.profile = "driving") 

#-------------------------------------------






