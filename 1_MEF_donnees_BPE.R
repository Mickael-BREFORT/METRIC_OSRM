
# ------------------------------------------#
#   METRIC - Accessibilité aux équipements  #  
#                                           #
#       Préparation de la table BPE         #       
# ------------------------------------------#

# Noms des fichiers de la BPE récupérés sur insee.fr ----
#---------------------------------------------------------------------------#
  # Année de la base BPE
  AN <- 20
  
  # Base "ensemble" des équipements de la BPE ; Année 2020
  # => www.insee.fr/fr/statistiques/3568596?sommaire=3568656
  BDD_BPE <- paste0("bpe", AN, "_ensemble_xy.csv")
  
  # "Table de passage" qui liste les équipements de la BPE, donne leurs 
  # libellés et les rattachent à un "domaine" (Services, culture,...)
  # => www.insee.fr/fr/statistiques/3568629?sommaire=3568656#documentation
  Liste_Equipements_BPE <- paste0("BPE", AN, "_table_passage.csv")
  
  # Liste des équipements regroupés par gamme (proxi, inter, sup)
  # => www.insee.fr/fr/statistiques/3568650?sommaire=3568656
  Gammes <- paste0("BPE_gammes_20", AN, "_internet.xlsx")
#---------------------------------------------------------------------------#

  
# Importation de la BPE ----  
BPE <- read_delim(paste0("./Donnees/", BDD_BPE), delim = ";", trim_ws = TRUE,
                  show_col_types = FALSE)

# Importation de la liste des équipements ----  
Liste_Equip_BPE <- read.csv(paste0("./Donnees/", Liste_Equipements_BPE))

# Importation de la liste des regroupements par gamme (Proximité,...) ---- 
Gammes_BPE      <- read.xlsx(paste0("./Donnees/", Gammes),
                             sheet = "Feuil1") 

# Importation des fichiers de composition des paniers (CGET)
  # Liste récupéré sur insee.fr https://www.insee.fr/fr/statistiques/1908098
  # dans le classeur de données + mise en forme
  # Attention : Changement de code de l'équipement "Pharmacie" en 2020
Panier_Jeunes      <- read.xlsx("./Donnees/Compo_paniers_CGET.xlsx", 
                                sheet = "Jeunes")
Panier_Parents     <- read.xlsx("./Donnees/Compo_paniers_CGET.xlsx", 
                                sheet = "Parents")

Panier_Seniors     <- read.xlsx("./Donnees/Compo_paniers_CGET.xlsx", 
                                sheet = "Seniors")

Panier_VieCourante <- read.xlsx("./Donnees/Compo_paniers_CGET.xlsx",
                                sheet = "Vie courante")



# -------------------------------------------------------------#
# Mise en forme de la Base Permanente des Equipements (BPE) ---- 
# -------------------------------------------------------------#
BPE <- BPE %>%
  # On sélectionne les variables utiles
  select(DEPCOM, DEP, REG, TYPEQU, LAMBERT_X, LAMBERT_Y, QUALITE_XY) %>%
    
  # On ne conserve que les régions de France métropolitaine
  filter(!REG %in% c("01", "02", "03", "04", "06")) %>% 
    
  # On supprime les équipements non géolocalisés
  filter(!is.na(LAMBERT_X) | !is.na(LAMBERT_Y)) %>% 
    
  # Bien s'assurer que les variables coordonnées sont en numérique
  mutate(LAMBERT_X = as.numeric(LAMBERT_X),
         LAMBERT_Y = as.numeric(LAMBERT_Y))

# Création d'un identifiant unique pour chaque équipement  
BPE <- BPE %>% 
  mutate(ID = 1:nrow(BPE)) %>% 
  relocate(ID) %>% 
  rename(EQUIP = TYPEQU)


#-----------------------------------------------------------#  
# Stat sur la qualité de la géolocalisation des données 
freq(BPE$QUALITE_XY, plot = FALSE)
  #             Frequency Percent
  # Acceptable    113607   4.357
  # Bonne        2299969  88.198
  # Mauvaise      194153   7.445
  # Total        2607729 100.000
# Remarque : 2 509 équipements non géolocalisés en Métropole
#-----------------------------------------------------------#  



# -----------------------------------------------------------------------#
# Création d'une table permettant par la suite de distinguer             #
# parmi les équipements de la BPE, les  gammes de proximité,             #
# intermédiaire et supérieure (Variable GAMME),                          #
# les paniers jeunes, parents, seniors et vie courante (Variable PANIER) #                                             #
# -----------------------------------------------------------------------#

# Libellés des équipements de la BPE----
Liste_Equip_BPE <- Liste_Equip_BPE %>% 
  select(-X) %>%
  rename (EQUIP = TYPEQU) 

# Table regroupant les équipements par gamme----
    # La table importée est peu pratique. Les opérations suivantes 
    # permettent de la mettre en forme

# Liste des équipements regroupés dans les Gammes / Paniers de la BPE
# => IMPORTANT : susceptible de changer d'une année sur l'autre
# => bien lire la documentation à chaque mise à jour de la base
# et ajouter en conséquence la liste ci-dessous
Liste_Equip_Regroup_2equip <- c("BR01", "CR02", "AR01", "AR02",
                                "BR02", "CR01", "CR03", "CR04")

Liste_Equip_Regroup_3equip <- c("AR03", "FR01", "FR02", "ER01")

Liste_Equip_Regroup <- c(Liste_Equip_Regroup_2equip,
                         Liste_Equip_Regroup_3equip)


#---
Equip_sans_regroup <- Gammes_BPE %>% 
      filter(!EQUIP %in% c(Liste_Equip_Regroup_2equip,
                           Liste_Equip_Regroup_3equip)) %>% 
      select(EQUIP, GAMME)
    
Equip_regroup_2equip <- Gammes_BPE %>% 
      filter(EQUIP %in% Liste_Equip_Regroup_2equip) %>% 
      select(REGROUP1, REGROUP2, GAMME) %>% 
      pivot_longer(cols       =  starts_with('REGROUP'),
                   names_to   = "REGROUP",
                   values_to  = "EQUIP" ) %>% 
      select(EQUIP, GAMME) %>% 
      unique()
  
Equip_regroup_3equip <- Gammes_BPE %>% 
      filter(EQUIP %in% Liste_Equip_Regroup_3equip) %>% 
      select(REGROUP1, REGROUP2, REGROUP3, GAMME) %>% 
      pivot_longer(cols       =  starts_with('REGROUP'),
                   names_to   = "REGROUP",
                   values_to  = "EQUIP" ) %>% 
      select(EQUIP, GAMME) %>% 
      unique()

# Table mise en forme avec équipements de chaque gamme
# => 128 équipements dans les 3 gammes
Gammes_Equip <- bind_rows(Equip_sans_regroup, Equip_regroup_2equip,
                   Equip_regroup_3equip) %>% unique()

# On recrée la variable "EQUIP_REGROUP" avec les équipements regroupés
Gammes_Equip <- Gammes_Equip %>% 
      mutate(EQUIP_REGROUP = case_when(
                  EQUIP %in% c("A206", "A207", "A208") ~ "AR03",
                  EQUIP %in% c("B201", "B202")         ~ "BR01",
                  EQUIP %in% c("C104", "C105")         ~ "CR02",
                  EQUIP %in% c("F111", "F116", "F121") ~ "FR01",
                  EQUIP %in% c("A101", "A104", "A208") ~ "AR01",
                  EQUIP %in% c("A120", "A121")         ~ "AR02",
                  EQUIP %in% c("B103", "B309")         ~ "BR02",
                  EQUIP %in% c("C101", "C102")         ~ "CR01",
                  EQUIP %in% c("F112", "F114", "F120") ~ "FR02",
                  EQUIP %in% c("C301", "C304")         ~ "CR03",
                  EQUIP %in% c("C302", "C305")         ~ "CR04",
                  EQUIP %in% c("E107", "E108", "E109") ~ "ER01",
                  TRUE ~ EQUIP)) %>% 
      select(EQUIP, EQUIP_REGROUP, GAMME)
      

# Ajout des libellés regroupés à partir de la table "Gammes_BPE"
Libel_Equip_Regroup <- Gammes_BPE %>% 
  select(EQUIP, LIBEL_EQUIP) %>% 
  rename(LIB_EQUIP_REGROUP = LIBEL_EQUIP)

Gammes_Equip <- left_join(Gammes_Equip, Libel_Equip_Regroup, 
                   by = c("EQUIP_REGROUP" = "EQUIP")) %>% 
  select(EQUIP, EQUIP_REGROUP, LIB_EQUIP_REGROUP, GAMME)


#-----------------------------------------------------------------#
# Vérification
freq(Gammes_Equip$GAMME, plot = FALSE)    # => 128 équipements
#                 Frequency Percent
# intermédiaire        43   33.59
# proximité            34   26.56
# supérieure           51   39.84
# Total               128  100.00

# Equipements regroupés
Temp <- Gammes_Equip %>% select(EQUIP_REGROUP, GAMME) %>% unique() 
freq(Temp$GAMME, plot = FALSE)
#                 Frequency Percent
# intermédiaire        37   33.04
# proximité            28   25.00
# supérieure           47   41.96
# Total               112  100.00  
# => 112 équipements dans les 3 gammes  OK !!
#-----------------------------------------------------------------#


# Ajout des gammes à la liste des équipemnts de la BPE
Liste_Equip_BPE <- left_join(Liste_Equip_BPE, Gammes_Equip, by = "EQUIP")

Liste_Equip_BPE <- Liste_Equip_BPE %>% 
  mutate(EQUIP_REGROUP = 
           if_else(is.na(EQUIP_REGROUP), EQUIP, EQUIP_REGROUP),
         LIB_EQUIP_REGROUP = 
           if_else(is.na(LIB_EQUIP_REGROUP), LIB_EQUIP, LIB_EQUIP_REGROUP),
         GAMME = if_else(is.na(GAMME), "Hors_Gamme", GAMME))



# Ajout des paniers "Jeunes", "Parents", "Seniors" (CGET)... ----
# cf doc d'AT29 sous AUS
# ATTENTION : il s'agit d'"équipements regroupés" !!
Panier_Jeunes      <- Panier_Jeunes %>%  select(EQUIP) %>% mutate(Jeunes  = "1")
Panier_Parents     <- Panier_Parents %>% select(EQUIP) %>% mutate(Parents = "1")
Panier_Seniors     <- Panier_Seniors %>% select(EQUIP) %>% mutate(Seniors = "1")
Panier_VieCourante <- Panier_VieCourante %>% select(EQUIP) %>% 
                      mutate(VieCourante = "1")

Paniers <- full_join(Panier_Jeunes, Panier_Parents, by = "EQUIP") %>% 
              full_join(Panier_Seniors,             by = "EQUIP") %>% 
              full_join(Panier_VieCourante,         by = "EQUIP")

Paniers[is.na(Paniers)] <- "0"


# Création de la variable PANIER
Paniers <- Paniers %>% 
      mutate(PANIER = case_when(
        Jeunes == "1" & Parents == "0" & Seniors == "0" 
                  & VieCourante == "0" ~ "Jeunes",
        
        Jeunes == "1" & Parents == "1" & Seniors == "0" 
                  & VieCourante == "0" ~ "Jeunes, Parents",
        
        Jeunes == "1" & Parents == "0" & Seniors == "1" 
                  & VieCourante == "0" ~ "Jeunes, Seniors",
        
        Jeunes == "1" & Parents == "0" & Seniors == "0" 
                  & VieCourante == "1" ~ "Jeunes, VieCourante",
        
        Jeunes == "1" & Parents == "1" & Seniors == "1" 
                  & VieCourante == "0" ~ "Jeunes, Parents, Seniors",
        
        Jeunes == "1" & Parents == "1" & Seniors == "1" 
                  & VieCourante == "1" ~ "Jeunes, Parents, Seniors, VieCourante",
        
        #---
        Jeunes == "0" & Parents == "1" & Seniors == "0" 
                  & VieCourante == "0" ~ "Parents",
        
        Jeunes == "0" & Parents == "1" & Seniors == "1" 
                  & VieCourante == "0" ~ "Parents, Seniors",
        
        Jeunes == "0" & Parents == "1" & Seniors == "0" 
                  & VieCourante == "1" ~ "Parents, VieCourante",
        
        Jeunes == "0" & Parents == "1" & Seniors == "1" 
                  & VieCourante == "1" ~ "Parents, Seniors, VieCourante",
        
        #---
        Jeunes == "0" & Parents == "0" & Seniors == "1" 
                  & VieCourante == "0" ~ "Seniors",
        
        Jeunes == "0" & Parents == "0" & Seniors == "1" 
                  & VieCourante == "1" ~ "Seniors, VieCourante",
        
        #---
        Jeunes == "0" & Parents == "0" & Seniors == "0" 
        & VieCourante == "1" ~ "VieCourante",
        
        TRUE ~ "Autres")) %>% 
  select(EQUIP, PANIER)


# Pb avec les équipements regroupés...
# Lancer les étapes suivantes une à une pour bien comprendre
Panier_sans_regroup <- Paniers %>% 
  filter(!EQUIP %in% c("AR03", "BR01", "CR02", "FR01", "AR01", "AR02",
                       "BR02", "CR01", "FR02", "CR03", "CR04", "ER01")) 

Panier_avec_regroup <- Paniers %>% 
  filter(EQUIP %in% c("AR03", "BR01", "CR02", "FR01", "AR01", "AR02",
                       "BR02", "CR01", "FR02", "CR03", "CR04", "ER01")) %>% 
  rename(PANIER_Temp = PANIER)


Liste_Equip_BPE <- left_join(Liste_Equip_BPE, Panier_sans_regroup, 
                             by = "EQUIP") 

Liste_Equip_BPE <- left_join(Liste_Equip_BPE, Panier_avec_regroup, 
                             by = c("EQUIP_REGROUP" = "EQUIP"))

Liste_Equip_BPE <- Liste_Equip_BPE %>% 
  mutate(PANIER = if_else(is.na(PANIER), PANIER_Temp, PANIER))

Liste_Equip_BPE <- Liste_Equip_BPE %>% 
  mutate(PANIER = if_else(is.na(PANIER), "Hors_Panier", PANIER)) %>% 
  select(-PANIER_Temp)


#--------------------------------#
# Table BPE mise en forme !! ----
#--------------------------------#
BPE <- left_join(BPE, Liste_Equip_BPE, by = "EQUIP") %>% 
  select(ID, DEPCOM, DEP, REG, 
         EQUIP, LIB_EQUIP, EQUIP_REGROUP, LIB_EQUIP_REGROUP,
         SDOM,  LIB_SDOM,  DOM,           LIB_DOM, 
         GAMME, PANIER,  
         LAMBERT_X, LAMBERT_Y, QUALITE_XY)

#-----------------------------------------------------------------#
# Vérification
Temp <- BPE %>% select(EQUIP_REGROUP, GAMME) %>% unique() 
freq(Temp$GAMME, plot = FALSE) 
#                   Frequency Percent
# Hors_Gamme           59   34.50
# intermédiaire        37   21.64
# proximité            28   16.37
# supérieure           47   27.49
# Total               171  100.00


Temp <- BPE %>% select(EQUIP_REGROUP, PANIER) %>% unique() 
freq(Temp$PANIER, plot = FALSE) 
#                                           Frequency  Percent
# Hors_Panier                                 125  72.6744
# Jeunes                                        2   1.1628
# Jeunes, Parents                               6   3.4884
# Jeunes, Parents, Seniors                      3   1.7442
# Jeunes, Parents, Seniors, VieCourante         1   0.5814
# Jeunes, VieCourante                           1   0.5814
# Parents                                       7   4.0698
# Parents, Seniors                              1   0.5814
# Parents, VieCourante                          4   2.3256
# Seniors                                       6   3.4884
# Seniors, VieCourante                          5   2.9070
# VieCourante                                  11   6.3953
# Total                                       172 100.0000


# Sauvegarde des tables BPE par gamme d'équipement----
saveRDS(BPE, paste0("./Donnees/", "BPE_20", AN, ".RDS"))
saveRDS(Liste_Equip_BPE, "./Donnees/Liste_Equip_BPE.RDS")
saveRDS(Liste_Equip_Regroup, "./Donnees/Liste_Equip_Regroup.RDS")


# Suppression des tables devenues inutiles----
rm("BDD_BPE", "Equip_regroup_2equip", "Equip_regroup_3equip",
  "Equip_sans_regroup","Gammes", "Gammes_BPE", "Gammes_Equip",
  "Libel_Equip_Regroup", "Liste_Equipements_BPE", "Panier_avec_regroup", 
  "Panier_Jeunes", "Panier_Parents", "Panier_sans_regroup", "Panier_Seniors", 
  "Panier_VieCourante", "Paniers", "Temp")

gc()
 
