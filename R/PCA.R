#' plot a PCA or RDA from clustering
#'
#' @param dat_thresh_path the path to the file data red
#' @param ab_thresh the value of the threshold
#' @param method_c
#' @param arms_id
#' @return path to the data file, reduced and mean by site
#' @export
#'


pca_function <- function(dat_thresh_red_path, ab_thresh, method_c, arms_id) {
  
  #dat_thresh_red_path = targets::tar_read(dattresh_arms)
  #ab_thresh = targets::tar_load("ab_thresh") 
  #method_c = targets::tar_load("clust_method") 
  #arms_id = targets::tar_load("campain_id") 
  
  dat_thresh_red_path <- read.csv(dat_thresh_red_path, 
                                  header = TRUE,
                                  row.names = 1)
  matrix.hel <- vegan::decostand(dat_thresh_red_path, "hellinger")
 
  res.pca <- FactoMineR::PCA(dat_thresh_red_path, ncp = 3, graph = FALSE)
  

  plot <- factoextra::fviz_pca_biplot(res.pca)
  
  
  res.hcpc <- FactoMineR::HCPC(res.pca, nb.clust = 5,  graph = TRUE)

 
  
  factoextra::fviz_cluster(res.hcpc,
               repel = TRUE,            # Evite le chevauchement des textes
               show.clust.cent = TRUE, # Montre le centre des clusters
               palette = "jco",         # Palette de couleurs, voir ?ggpubr::ggpar
               ggtheme = ggplot2::theme_minimal(),
               main = "Factor map"
  )
  
  
}

  