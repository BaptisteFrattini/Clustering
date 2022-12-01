
#' Subset a raw_data table for a sampling campain
#'
#' @param raw_data the path to the raw data file
#' @param arms_id the ID of the arms to subset for
#'
#' @return the path to the subseted raw data file
#' @export
#'
data_arms <- function(raw_data,
                      arms_id){
  
  #arms_id = "RUNA"
  
  dat_path <- here::here(raw_data)
  data <- read.table(dat_path, 
                     header = TRUE, 
                     sep = ";", 
                     dec = ",")
  
  dat <- data[data$Nom_ARMS == arms_id, ]
  
  meta_names <- c("Latitude", "Longitude", "Incubation", "Saison_Récolte", "Sanctuaire", "Locality", "Site", "Num_ARMS", "Station", "Nom_ARMS",
                  "Nouveau.Nom", "ARMS", "B.M.H.", "O.F", "Numéro_Plaque", "Orientation", "Nom_ARMS.1", "Image_ID", "Image_name")
  
  meta_data <- dat[, meta_names]
  dat <- dat[, !(names(dat) %in% meta_names)]
  
  # clean data for zero sum columns
  
  dat <- dat[ , colSums(dat) != 0]
  
  
  out_d_path <- "data/derived-data" #Nom du chemin qui mène à data derived
  out_f_name <- paste0("data_", arms_id, ".csv") #Nom du fichier généré dans 
  #ce dossier
  meta_out_f_name <- paste0("metadata_", arms_id, ".csv") #
  out_f_path <- here::here(out_d_path, out_f_name)
  meta_out_f_path <- here::here(out_d_path, meta_out_f_name)
  write.csv(dat, file = out_f_path, row.names = FALSE)
  write.csv(meta_data, file = meta_out_f_path, row.names = FALSE)
  
  res <- c(out_f_path, meta_out_f_path)
  names(res) <- c("path_data", "path_meta")
  
  return(res)
  
}






