
# ------------------------------------------#
#   METRIC - Accessibilité aux équipements  #  
#                                           #
#         Visualiser un équipement          #       
# ------------------------------------------#

#------------------------------------------------------------#
# Exemple de visualisation de la région élargie pour les HDF #
#------------------------------------------------------------#

# Paramètre
REG <- "32"
CATEG <- "Culture"
EQUIP <- "F303"    # Cinema

# Lecture des données
sf_REG_FM <- st_read("./Donnees/REG_FM.gpkg")
sf_REG    <- st_read(paste0(
              "./Donnees/Buffer_REG/sf_REG_Buffer_90000_", REG, ".gpkg"))
sf_BPE <- st_read(paste0(
           "./Donnees/Donnees_Entree_Metric/sf_BPE_",CATEG, "_", REG, ".gpkg"))

Equipement <- sf_BPE %>% filter(EQUIP == .env$EQUIP)


# Options globales pour les cartes avec mapview
mapviewOptions(basemaps = c("OpenStreetMap",
                            "CartoDB.Positron",
                            "CartoDB.DarkMatter",
                            "Esri.WorldImagery"))


# Cartographie avec mapview
mapview(sf_REG_FM, col.regions= "#26cce7")+
  mapview(sf_REG %>% st_cast("MULTILINESTRING"),
          color = "#FFC300", lwd = 6)+
  mapview(Equipement,
          color = "black",lwd = 6)

