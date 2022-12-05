#' Subset a raw_data table for a sampling campain
#'
#' @param meta_and_data the path to the data or metadata 
#' @param campain_id the id of the campain to use; could be RUNA/CINA/SALA/RODA
#' @param ab_thresh the threshold; could be 99, 98,97... til 1
#' 
#'
#' @return the path to the reduce data file, with the threshold u gave be4 
#' 
#'
data_red <- function(meta_and_data, ab_thresh, campain_id) {
  
  #meta_and_data <- targets::tar_read("metadata_data") 
  #targets::tar_load("ab_thresh") 
  #targets::tar_load("campain_id")
  
  
  # On créer un objet qui prend le fichier qui contient les data dans le chemin 
  # "meta_and_data"
  data_path <- meta_and_data[!grepl("metadata", meta_and_data)]  
  
  # On lit ce tableau
  dat <- read.csv(data_path)
  
  #On calcul la proportion de zero pour chaque MSP
  prop_zero <- unlist(lapply(dat, function(c) {
    
    #c = dat[,1]
    
    sum(c == 0) / length(c) * 100
    
  }))
  
  prop_zero <- sort(prop_zero)
  
  dat <- dat[, names(prop_zero)]
  
  dat99 <- dat[, prop_zero < ab_thresh]
  
  path_to_derived_data <- "data/derived-data"
  dat_thresh_name <- paste0("dat", ab_thresh, ".csv")
  dat_thresh_path <- here::here(path_to_derived_data, dat_thresh_name)
  
  
  
  write.csv(dat99, file = dat_thresh_path, row.names = FALSE)
  
return(dat_thresh_path)
  
}
