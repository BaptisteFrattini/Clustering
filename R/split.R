
data_split <- function(meta_and_data, ab_thresh, campain_id) {
  
  #meta_and_data <- targets::tar_read("metadata_data") ; targets::tar_load("ab_thresh") ; targets::tar_load("campain_id")
  
  
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
  
  dat99 <- dat[, prop_zero < 99]
  
return()
  
  
  
}
