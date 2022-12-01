################################################################################
    # Création d'un derived data avec seulement l'échantillonage ARMS #
################################################################################

dat_path <- file.path("data/raw-data",
                      "Data_sans_UNAV-NR-OROS.csv")
data <- read.table(here::here(dat_path), 
                   header = TRUE, 
                   sep = ";", 
                   dec = ",")

data <- subset(data, 
               data$Nom_ARMS == "RUNA")


