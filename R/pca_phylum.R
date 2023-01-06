#' plot a PCA or RDA for ech phylum
#'
#' @param dat_thresh_path the path to the file data red
#' @param ab_thresh the value of the threshold
#' @param method_c clustering methd
#' @param arms_id campain id
#' @return 
#' @export
#'


pca_phylum <- function(dat_thresh_red_path, ab_thresh, method_c, arms_id) {
  
  #dat_thresh_red_path = targets::tar_read(mean_arms)
  #ab_thresh = targets::tar_load("ab_thresh") 
  #method_c = targets::tar_load("clust_method") 
  #arms_id = targets::tar_load("campain_id") 
  
  dat_thresh_red_path <- read.csv(dat_thresh_red_path, 
                                  header = TRUE,
                                  row.names = 1)
  matrix.hel <- vegan::decostand(dat_thresh_red_path, "hellinger")
  matrix.hel <- dat_thresh_red_path
  par(mfrow = c(2,2))
  #### Bryozoa ####
  
  bryo <- grepl("bryo",colnames(matrix.hel))
  matrix.h.bryo <- matrix.hel[,bryo]
  
  res.pca <- FactoMineR::PCA(matrix.h.bryo, ncp = 3, graph = FALSE)
  
  factoextra::fviz_pca_biplot(res.pca)
 
  
  #### Ascidiacea ####
  
  asc <- grepl("asc",colnames(matrix.hel))
  matrix.h.asc <- matrix.hel[,asc]
  
  res.pca <- FactoMineR::PCA(matrix.h.asc, ncp = 3, graph = FALSE)
  
  plot <- factoextra::fviz_pca_biplot(res.pca)
  plot
  
  #### Porifera ####
  
  por <- grepl("_spo",colnames(matrix.hel))
  matrix.h.por <- matrix.hel[,por]
  
  res.pca <- FactoMineR::PCA(matrix.h.por, ncp = 3, graph = FALSE)
  
 
  plot <- factoextra::fviz_pca_biplot(res.pca)
  plot
  
  #### Foraminifera ####
  
  foram <- grepl("_for",colnames(matrix.hel))
  matrix.h.foram <- matrix.hel[,foram]
  
  res.pca <- FactoMineR::PCA(matrix.h.foram, ncp = 3, graph = FALSE)
  
  
  plot <- factoextra::fviz_pca_biplot(res.pca)
  plot
  
  #### MSP pool ####
  
  matrix.hel.red <- matrix.hel[ , -grep("_for|_spo|asc|_bryo", colnames(matrix.hel))]
  
  foram.mean <- rowSums(matrix.h.foram)
  spo.mean <- rowSums(matrix.h.por)
  asc.mean <- rowSums(matrix.h.asc)
  bryo.mean <-rowSums(matrix.h.bryo)

  matrix.msp.pool <- as.data.frame(cbind(matrix.hel.red,foram.mean, spo.mean, asc.mean, bryo.mean))
  sum(matrix.msp.pool[1,])
  
  res.pca <- FactoMineR::PCA(matrix.msp.pool, ncp = 3, graph = FALSE)
  
  
  plot <- factoextra::fviz_pca_biplot(res.pca)
  plot
  
  return()
}