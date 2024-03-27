

# Création d'un programme simple pour former Caro au package Metric

# -----------------------------------------#
# METRIC - Accessibilité aux équipements----
#                                          #
#       Liste des packages nécessaires     #       
# -----------------------------------------#


install.packages("remotes")

# Installation du package Metric
remotes::install_gitlab(repo = 'metric-osrm/metric-osrm-package',
                        host = 'git.lab.sspcloud.fr',
                        upgrade = 'never',
                        build = TRUE,
                        dependencies = TRUE,
                        force = TRUE)

#-------------------------------------------

library(tidyverse)
library(metric.osrm) 

# Adresse du serveur, ici le serveur expérimental déployé sur la plateforme 
# du SSPCloud
options(osrm.server = "https://metric-osrm-backend.lab.sspcloud.fr/") 

# On précise le profil de routage, ici "driving" car le serveur 
# SSPCloud ne propose que du calcul de trajet routier en voiture
options(osrm.profile = "driving") 

#-------------------------------------------

