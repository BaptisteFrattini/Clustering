#' Subset a raw_data table for a sampling campain
#'
#' @param 
#' @param 
#'
#' @return 
#' @export
#'

mean_by_arms <- function(meta_and_data, dat_thresh_path, ab_thresh) {
  
  #meta_and_data = targets::tar_read("metadata_data") 
  #dat_thresh_path = targets::tar_load("dattresh")
  
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
      print(i)
  }
  
  
  N <- colnames(dat_red)
  colnames(tab) <- N
  tab <- as.data.frame(tab)
 ## uu!!!!dat_thresh_red_name <- paste0("dat_red_", ab_thresh, ".csv")
 ## !!!! dat_thresh_red_path <- here::here(path_to_derived_data, dat_thresh_name)
  
  
  write.csv(tab, file = dat_thresh_path, row.names = FALSE)
  
  return(tab)
}

#data %>% dplyr::group_by(data$Nom Arms) %>% mean(1)
#flights %>% 
 # group_by(month) %>% 
  #mutate(mean_delay_month = mean(dep_delay, na.rm = TRUE)) %>% 
  #select(dep_delay, month, mean_delay_month)