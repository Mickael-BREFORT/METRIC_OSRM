

coord <- rio::import(file = system.file("extdata",
                                        "convertTo_3.xls",
                                        package = "metric.osrm"))

a <-convertTo(from = coord,
          to = "sp",
          fromEpsg = 2154)


Liste_GAMME  <- c("proximité")
Liste_PANIER <- c("Jeunes")


Liste_REG    <- c("32")