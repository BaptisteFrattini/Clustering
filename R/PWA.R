#' Compute pairwise adonis from git pmartinezarbizu
#'
#' @param dat_thresh_path the path to the file data red
#' @param ab_thresh the value of the threshold
#' @param method_c
#' @param arms_id
#' @return path to the data file, reduced and mean by site
#' @export
#' 

pw_adonis <- function(dat_thresh_red_path, ab_thresh, arms_id){
  
  #dat_thresh_red_path = targets::tar_read(dattresh_arms)
  #ab_thresh = targets::tar_load("ab_thresh") 
  #method_c = targets::tar_load("clust_method") 
  #arms_id = targets::tar_load("campain_id") 
  
  dat_thresh_red_path <- read.csv(dat_thresh_red_path, 
                                  header = TRUE,
                                  row.names = 1)
  matrix.hel <- vegan::decostand(dat_thresh_red_path, "hellinger")
  matrix.dist <- vegan::vegdist(matrix.hel, "bray")
  
  source("R/pairwiseAdonis.R")
  library(vegan)
  vector <- c(substr(row.names(matrix.hel),1,5))
  
  pwa <- pairwise.adonis(matrix.dist, vector, p.adjust.m = "none")
  pwa <- as.data.frame(pwa)
  write.table(pwa, 
              file = paste0("outputs/df_pwa_",ab_thresh,"_", arms_id, ".csv"),
              dec = ",", 
              sep = ";",
              row.names = FALSE)
  
  return(pwa)
  
}