
# ---------------------------------------------------#
#        METRIC - Accessibilité aux équipements   ----
#    Calcul des durées et distance avec metric.osrm  #       
# ---------------------------------------------------#

# ATTENTION : Ce programme ne peut être lancé qu'à la suite du 
# programme nommé "2_Préparation_Donnees_METRIC"



# Liste des équipements de chaque Gamme / PANIER
Liste_Equip <- function(CATEG){
  if(CATEG %in% Liste_GAMME){
    Temp <- BPE %>% 
      filter(GAMME == .env$CATEG)  %>% 
      select(EQUIP) %>% unique() %>% pull()
    
  }else if(CATEG %in% Liste_PANIER){
    Temp <- BPE %>% 
      filter(grepl(.env$CATEG, PANIER)) %>% 
      select(EQUIP) %>% unique() %>% pull()
  
  }else if(CATEG == Nom_Liste_Equip){
    Temp <- BPE %>%
      filter(LISTE_PERSO == Nom_Liste_Equip) %>% 
      select(EQUIP) %>% unique() %>% pull()   }
  
  assign(paste0("Liste_Equip_", CATEG), Temp, envir = .GlobalEnv) 
  #saveRDS(Temp, "./Donnees/Liste_Equip_Jeunes.RDS")
  }

# Lancement de la fonction
map(Liste_GAMME,  Liste_Equip)
map(Liste_PANIER, Liste_Equip)
map(Nom_Liste_Equip, Liste_Equip)



# Fonction permettant d'obtenir les DUREES ou les DISTANCE par équipement

# Création de la fonction
Calcul_Duree <- function(REG, CATEG, EQUIP, INDICATEUR){
  
  CODGEO_REG <- readRDS(paste0("./Donnees/Donnees_Entree_Metric/Pop_Reg_", 
                               REG, ".RDS"))
  
  Coord_CODGEO <- codeComToCoord(codeInsee = CODGEO_REG$CODGEO,
                                 geo = "2021",
                                 type = "chx")

  Temp_BPE <- st_read(paste0("./Donnees/Donnees_Entree_Metric/sf_BPE_", 
                             CATEG, "_", REG, ".gpkg")) %>% 
              filter(EQUIP == .env$EQUIP) %>% 
              select(ID, geom)
  
  Temp <- metricOsrmTable(src = Coord_CODGEO,
                          dst = Temp_BPE,
                          faceAFace = FALSE,
                          stable = TRUE,
                          rayonMax = 150,
                          nbDstVolOiseau = 5,
                          nbDstMeasure = 1,
                          optiMeasure = INDICATEUR,
                          emprise = "FRM")
  
  Temp <- Temp %>%
    mutate(EQUIP = .env$EQUIP ) %>%
    select(idSrc, EQUIP, INDICATEUR) %>%
    rename(CODGEO = idSrc,
           DUREE  = INDICATEUR)

  Temp_Tot <<- bind_rows(Temp_Tot, Temp) %>%
    mutate(REG = .env$REG)

}


# Lancement de la fonction

Temp_Tot <<- data.frame()

Metric <- function(CATEG, Liste_REG){

  
  # Lancement de la fonction "Calcul_Duree" 
  xmap(list(Liste_REG, CATEG, get(paste0("Liste_Equip_", CATEG)), "duree"), 
       Calcul_Duree)

  Temp_Tot_ <<- left_join(Temp_Tot, Liste_Equip_BPE, by = "EQUIP") %>%
                    select(REG, CODGEO, EQUIP, EQUIP_REGROUP,
                           LIB_EQUIP_REGROUP, DUREE)

  Temp_Tot_1 <<- Temp_Tot_ %>%
    filter(EQUIP_REGROUP %in% Liste_Equip_Regroup)

  Temp_Tot_2 <<- Temp_Tot_ %>%
    filter(!EQUIP_REGROUP %in% Liste_Equip_Regroup)
  
  #saveRDS(Temp_Tot, paste0("./Resultats/Duree_Distance_", CATEG, ".RDS"))
  
}

# xmap(list("proximité", Liste_REG), Sauvegarde_Metric)
# xmap(list("intermédiaire", Liste_REG), Sauvegarde_Metric)
# xmap(list("supérieure", Liste_REG), Sauvegarde_Metric)
xmap(list("Culture", Liste_REG), Metric)







# IMPORTANT
# Faire 1 filtre sur les équipements regroupés pour prendre le temps minimum 
# Ex : police ou gendarmerie => même service...
# donc on prende le temps le plus court des 2




# Sauvegarde des tables BPE par gamme d'équipement----
Pop_Reg_94 <- readRDS(paste0("./Donnees/Donnees_Entree_Metric/", "Pop_Reg_", "94", ".RDS"))



DUREE <- Temp_Tot_2 %>% 
  pivot_wider(id_cols     = c("REG", "CODGEO"),  
              names_from  = EQUIP_REGROUP,
              values_from = DUREE
              ,
              names_glue  = "{EQUIP_REGROUP}"
              )

DUREE <- left_join(DUREE, Pop_Reg_94, by = c("REG", "CODGEO")) %>% 
  relocate(LIBGEO, .after = CODGEO)


# Fonction de calcul
calculMoyenne <- function(x) {Hmisc::wtd.mean(x, DUREE$POP, na.rm = TRUE)} 

liste_var <- "F303"

DUREE_2 <- DUREE %>% 
  group_by(REG) %>% 
  mutate(across(all_of(liste_var), list(Mean = calculMoyenne)))

    





# Et volià le résultat; en utilsant un mutate(across) au lieu de mutate_at

# Solution 1 : 
Temp1 <- Temp %>% 
  group_by(DEPT) %>% 
  mutate_at(vars(Paris_entrants, Picardie_entrants, Reste.France_entrants, Reste.IdF_entrants,  Paris_sortants,       
                 Picardie_sortants, Reste.France_sortants, Reste.IdF_sortants  ), 
            list(pct = calcul_pct))

# Solution 2 : avece mutate + acroos
# et je peux même utiliser une liste de variable (qui ne marche pas dans la solution 1) 

# Je crée une liste des variables
liste_var <- (colnames(Temp)[!colnames(Temp) %in% c("DEPT", "AGER18C")])

Temp2 <- Temp %>% 
  group_by(DEPT) %>% 
  mutate(across(c(liste_var), list(pct = calcul_pct)))

















DUREE_2b <- DUREE %>% 
  group_by(REG) %>% 
  mutate(DUREE_MOYENNE = Hmisc::wtd.mean(DUREE_F303, POP),
         DUREE_MOYENNE2 = mean(DUREE_F303))



DUREE_2b <- DUREE %>% 
  group_by(REG) %>% 
  mutate(DUREE_MOYENNE = Hmisc::wtd.mean(DUREE_F303, POP)) %>% 
  slice(1)



,
         DIST_MOYENNE  = Hmisc::wtd.mean(DISTANCE, POP))



DUREE_2 <- DUREE %>%
  group_by(REG) %>% 
  mutate(DUREE_F303, Hmisc::wtd.mean(DUREE_F303, POP))
    





DISTANCE <- Temp_Tot_2 %>% 
  pivot_wider(id_cols     = c("REG", "CODGEO"),  
              names_from  = EQUIP_REGROUP,
              values_from = DISTANCE
              ,
              names_glue  = "DISTANCE_{EQUIP_REGROUP}"
  )




c <- left_join(DUREE, b, by = "CODGEO")

DUREE <- DUREE %>% 
  mutate(DUREE_MOY = mean(colnames(DUREE)[-c(REG, CODGEO)], na.rm = TRUE))

aa <- colnames(DUREE)[!colnames(DUREE) %in% c(REG)]


DUREE <- DUREE %>% 
  mutate(DUREE_MOY = mean(starts_with("DUREE"), na.rm = TRUE))


freq(Temp_Tot$EQUIP,  plot = FALSE)

Equip_regroup_2equip <- Gammes_BPE %>% 
  filter(EQUIP %in% c("BR01", "CR02", "AR01", "AR02",
                      "BR02", "CR01", "CR03", "CR04")) %>% 
  select(REGROUP1, REGROUP2, GAMME) %>% 
  pivot_longer(
    cols       =  starts_with('REGROUP'),
    names_to   = "REGROUP",
    values_to  = "EQUIP" ) %>% 
  select(EQUIP, GAMME) %>% 
  unique()

Equip_regroup_3equip <- Gammes_BPE %>% 
  filter(EQUIP %in% c("AR03", "FR01", "FR02", "ER01")) %>% 




Duree_Dist_proximité %>%
  pivot_wider(id_cols = c("REG", "CODGEO", "EQUIP",
                          "EQUIP_REGROUP", "LIB"),
              names_from = ANNEE, 
              values_from = POPULATION)


freq(Duree_Equip_proximité$EQUIP, w =  plot = FALSE)



%>%
  mutate(DEP   = "00", LIBDEP = "France métropolitaine",
         REG   = "00", LIBREG = "France métropolitaine") %>% 
  relocate(c(REG, LIBREG, DEP, LIBDEP), .after = Date)





xmap(list(Liste_REG, "intermédiaire", Liste_Equip_intermédiaire), Calcul_Duree)
xmap(list(Liste_REG, "supérieure", Liste_Equip_supérieure), Calcul_Duree)











# Durree moyenne

Temp_Tot <<- bind_rows(Temp_Tot, Temp)
Temp <- Temp %>% 
  mutate(Duree_Moyenne = wtd.mean(duree, P18_POP)
         # ,
         # Duree_D1  = wtd.quantile(duree, P18_POP, probs = 0.1),
         # Duree_D2  = wtd.quantile(duree, P18_POP, probs = 0.2),
         # Duree_D3  = wtd.quantile(duree, P18_POP, probs = 0.3),
         # Duree_D4  = wtd.quantile(duree, P18_POP, probs = 0.4),
         # Duree_D5  = wtd.quantile(duree, P18_POP, probs = 0.5),
         # Duree_D6  = wtd.quantile(duree, P18_POP, probs = 0.6),
         # Duree_D7  = wtd.quantile(duree, P18_POP, probs = 0.7),
         # Duree_D8  = wtd.quantile(duree, P18_POP, probs = 0.8),
         # Duree_D9  = wtd.quantile(duree, P18_POP, probs = 0.9)
         
  ) %>% 
  slice(1) %>% 
  select(EQUIP,REG, starts_with("Duree"))


Temp_Tot <<- bind_rows(Temp_Tot, Temp)



# freq(Temp_BPE$REG, plot = FALSE)

Temp <- left_join(Temp, CODGEO_REG, by = "CODGEO")


# Durée moyenne
Temp <- Temp %>% 
  mutate(Duree_Moyenne = wtd.mean(duree, P18_POP)
         
         
         









# Pop à plus de 7 minutes 
plus7 <- Temp_2 %>% 
  filter(duree > 7) %>% 
  summarise(POP = (sum(P18_POP)/ sum(Temp_2$P18_POP))*100)



RP16_HDF %>% ungroup() %>% 
  summarise(Age_Moyen       = mean(Age),
            Age_moyen_Pond  = wtd.mean(Age, weights = Ipondi),
            Age_Med         = median(Age),
            Age_median_Pond = wtd.quantile(Age, weights = Ipondi,
                                           probs = 0.5)     ) 



write.xlsx(Temp, "./Temp.xlsx")

colnames(res)
















