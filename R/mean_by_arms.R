#' Subset a raw_data table for a sampling campain
#'
#' @param meta_and_data the path to the metadata and data
#' @param dat_thresh_path the path to the file data red
#' @param ab_thresh the value of the threshold
#'
#' @return path to the data file, reduced and mean by site
#' @export
#'

mean_by_arms <- function(meta_and_data, dat_thresh_path, ab_thresh) {
  
  #meta_and_data = targets::tar_read("metadata_data") 
  #targets::tar_load("ab_thresh") 
  #dat_thresh_path = targets::tar_read("dattresh")
  
  
  meta_path <- meta_and_data[grepl("metadata", meta_and_data)] 
  meta <- read.csv(meta_path)
  arms_name <- meta$arms_name
  
  dat_red <- read.csv(dat_thresh_path)
  
  
  
  tab <- NULL
  U <- NULL
  
    for (i in 1:ncol(dat_red)) {
      
      U <- tapply(dat_red[,i], 
               arms_name, 
               mean)
      
      tab <- cbind(tab,
                U)
  }
  
  
  N <- colnames(dat_red)
  colnames(tab) <- N
  tab <- as.data.frame(tab)
  path_to_derived_data <- "data/derived-data"
  dat_thresh_red_name <- paste0("dat_red_", ab_thresh, ".csv")
  dat_thresh_red_path <- here::here(path_to_derived_data, dat_thresh_red_name)
  
  
  write.csv(tab, file = dat_thresh_red_path, row.names = TRUE)
  
  return(dat_thresh_red_path)
}

#data %>% dplyr::group_by(data$Nom Arms) %>% mean(1)
#flights %>% 
 # group_by(month) %>% 
  #mutate(mean_delay_month = mean(dep_delay, na.rm = TRUE)) %>% 
  #select(dep_delay, month, mean_delay_month)