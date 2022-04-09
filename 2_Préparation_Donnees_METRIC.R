
# ----------------------------------------------#
#    METRIC - Accessibilité aux équipements ----
#    Création des table BPE par gamme, paniers  #       
# ----------------------------------------------#

# Année de la base BPE
AN <- 20

# Chargement de la BPE et de la liste d'équipements
# => résultats du programme "1_MEF_donnees_BPE"
BPE             <- readRDS(paste0("./Donnees/","BPE_20",AN,".RDS"))
Liste_Equip_BPE <- readRDS("./Donnees/Liste_Equip_BPE.RDS")
Liste_Equip_Regroup <- readRDS("./Donnees/Liste_Equip_Regroup.RDS")

# Liste des régions de France métropolitaine
Liste_REG    <- c("11", "24", "27", "28", "32", "44", "52", "53", "75",
                  "76", "84", "93", "94")

  # Une seule région : Hauts-de-France
  Liste_REG    <- c("32")


# Equipements "standards" : Gammmes et Paniers
Liste_GAMME  <- c("proximité", "intermédiaire", "supérieure")
Liste_PANIER <- c("Jeunes", "Parents", "Seniors", "VieCourante")

  # Un seul équipement
  Liste_GAMME  <- c("proximité")

#-----------------------------------------------------------------------#
# Liste personnalisée le cas échéant
# Exemple avec les équipements du domaine de la "Culture"
Liste_Equip <- c("F303", "F307", "F309", "F305", "F306", "F308", "F310")
Nom_Liste_Equip <- "Culture"

BPE <- BPE %>%
      mutate(LISTE_PERSO = 
                case_when(
                  EQUIP %in% Liste_Equip ~ Nom_Liste_Equip,
                  TRUE ~ paste0("Hors_", Nom_Liste_Equip)))

#-----------------------------------------------------------------------#


#-----------------------------------------------------------------------#
#  Création d'une table BPE par gamme / panier / Liste personnalisée ----
#                     + conversion en objet "sf"                        #
#-----------------------------------------------------------------------#

# Création de la fonction permettant de créer des bases au format "sf"
# Cf Doc de Metric.osrm (fonction convertTo)

sf_BPE <- function(CATEG){
  
if(CATEG %in% Liste_GAMME){
  Temp  <- BPE %>% filter(GAMME == .env$CATEG) %>% as.data.frame() 
    
}else if(CATEG %in% Liste_PANIER){
  Temp  <- BPE %>% filter(grepl(.env$Liste_PANIER, PANIER)) %>% 
                   as.data.frame()
  
}else if(CATEG == Nom_Liste_Equip){
  Temp  <- BPE %>% filter(LISTE_PERSO == Nom_Liste_Equip) %>% 
                            as.data.frame() 
}

  Temp_ <- convertTo(from = Temp[,c("ID", "LAMBERT_X", "LAMBERT_Y")],
                            to = "sf",
                            fromEpsg = 2154,
                            toEpsg   = 4326)

  Temp <- right_join(Temp, Temp_ , by = c("ID" = "id"))

  write_sf(Temp, paste0("./Donnees/BDD_BPE/", "sf_BPE_", CATEG, ".gpkg"),
           delete_layer = TRUE) 
}

# Lancement de la fonction
map(Liste_GAMME,  sf_BPE)
map(Liste_PANIER, sf_BPE)
map(Nom_Liste_Equip, sf_BPE)



# -------------------------------------------------------------#
# Création de 13 régions "élargies" (France métropolitaine) ---- 
# -------------------------------------------------------------#

# Problème de "l'effet frontière" : 
# => on peut bénéficier d'un équipement situé dans une autre région !
# Solution : création d'une zone tampon ("Buffer") autour de la région.

# Lecture du gpkg des régions de France métropolitaine
# Remarque : le fichier "REG_FM.gpkg" est le résultat d'un export de QGIS
sf_REG_FM <- st_read("./Donnees/REG_FM.gpkg")


# Création d'une fonction permettant la création d'une zone tampon par région
Buffer_REG <-function(REG, DISTANCE){
  
  # Buffer pour la gamme de proximité    
  # Filtre sur la région
  Temp <- sf_REG_FM %>% filter(code == .env$REG)

  # Transformer la projection ...
  Temp <- st_transform(Temp, crs = 4326)
  
  # Choix d'une marge autour du territoire d'intérêt
  Temp <- sf::st_buffer(Temp, dist = DISTANCE)
  
  # Sauvegarde
  write_sf(Temp, paste0(
    "./Donnees/Buffer_REG/","sf_REG_", "Buffer_", DISTANCE, "_", REG, ".gpkg"),
    delete_layer = TRUE)

}

# Lancement de la fonction
xmap(list(Liste_REG, 30000), Buffer_REG)
xmap(list(Liste_REG, 60000), Buffer_REG)
xmap(list(Liste_REG, 90000), Buffer_REG)



#-----------------------------------------------------------------------------#
#  Création d'une table BPE par gamme / panier... et par région "élargie"  ----
#-----------------------------------------------------------------------------#

# Création de la fonction
BPE_REG <- function(REG, CATEG, DISTANCE){
  
  BPE    <- st_read(paste0("./Donnees/BDD_BPE/sf_BPE_", CATEG, ".gpkg"), 
                    quiet = TRUE)
  
  Buffer <- st_read(paste0(
    "./Donnees/Buffer_REG/","sf_REG_Buffer_", DISTANCE, "_", REG, ".gpkg"), 
    quiet=TRUE)
  
  # Equipements inclus dans le Buffer !
  Temp <- st_join(BPE, Buffer, left = F)
  
  # Sauvegarde
  write_sf(Temp, paste0(
    "./Donnees/Donnees_Entree_Metric/","sf_BPE_", CATEG, "_", REG, ".gpkg"), 
    delete_layer = TRUE)
}


# Lancement de la fonction
xmap(list(Liste_REG, "proximité",     30000), BPE_REG)
    # xmap(list(Liste_REG, "intermédiaire", 60000), BPE_REG)
    # xmap(list(Liste_REG, "supérieure",    90000), BPE_REG)
    # 
xmap(list(Liste_REG, "Jeunes",      90000), BPE_REG)
    # xmap(list(Liste_REG, "Parents",     90000), BPE_REG)
    # xmap(list(Liste_REG, "Seniors",     90000), BPE_REG)
    # xmap(list(Liste_REG, "VieCourante", 90000), BPE_REG)

xmap(list(Liste_REG, Nom_Liste_Equip, 90000), BPE_REG)


# Vérification
# freq(sf_BPE_Culture_32$REG, plot = FALSE)





#----------------------------------------------------------------------#
#  Création d'une table par région avec les populations communales  ----
#----------------------------------------------------------------------#

# Importation des données 
# Population RP 2018 par commune (sur insee.fr)
Pop_Communales_2018 <- read.xlsx("./Donnees/base_cc_comparateur.xlsx",
                                 startRow = 6) %>%
  filter(!REG %in% c("01", "02", "03", "04", "06")) %>% 
  select(CODGEO, REG, LIBGEO, P18_POP) %>% 
  rename(POP = P18_POP)

 #Création de la fonction
Pop_reg <- function(REG){
  
    Temp  <- Pop_Communales_2018 %>% 
      filter(REG == .env$REG) %>%
      as.data.frame() 

    # Sauvegarde des tables BPE par gamme d'équipement
    saveRDS(Temp, paste0("./Donnees/Donnees_Entree_Metric/", "Pop_Reg_", REG, ".RDS"))
}

# Lancement de la fonction    
map(Liste_REG, Pop_reg)  
    









