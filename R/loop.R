#' boucle qui donne les valeurs de c; 2-norm; nombre d'espèce, sihouette, et clustering 
#'
#' @param meta_and_data path to meta data and data
#' @param method_c clustering algorithm
#' @param arms_id sampling campain id
#' 
#'
#' @return df wth all the compute values 
#' @export
#'

loop <- function(meta_and_data, method_c, arms_id) {

  #meta_and_data = targets::tar_read("metadata_data")
  #method_c = targets::tar_load("clust_method") 
  #arms_id = targets::tar_load("campain_id")   
  
  
  df <- NULL
  c <- NULL
  dnorm <- NULL
  n_sp <- NULL
  n_clust <- NULL
  penalty_min <- NULL
  n_clust_sil <- NULL
  perc_zero <- NULL
  
  for (i in 50:100) {
    
    data_path <- meta_and_data[!grepl("metadata", meta_and_data)]  
    dat <- read.csv(data_path)
    prop_zero <- unlist(lapply(dat, function(c) {
      sum(c == 0) / length(c) * 100
    }))
    prop_zero <- sort(prop_zero)
    dat <- dat[, names(prop_zero)]
    dat <- dat[, prop_zero < i]
    
    meta_path <- meta_and_data[grepl("metadata", meta_and_data)] 
    meta <- read.csv(meta_path)
    arms_name <- meta$arms_name
    
    tab <- NULL
    U <- NULL
    
      for (v in 1:ncol(dat)) {
      
      U <- tapply(dat[,v], 
                  arms_name, 
                  mean)
      
      tab <- cbind(tab,
                   U)
    }
    
    N <- colnames(dat)
    colnames(tab) <- N
    tab <- as.data.frame(tab)
    
    matrix.hel = vegan::decostand(tab, "hellinger")
    matrix.dist = vegan::vegdist(matrix.hel)
    
    cluster <- hclust(matrix.dist, method = method_c)
    coph <- stats::cophenetic(cluster)
    c[i] <- cor(matrix.dist, coph)
    dnorm[i] <- clue::cl_dissimilarity(matrix.dist,
                                    cluster,
                                    method = "spectral") 
    n_sp[i] <- ncol(dat)
    
    obj <- NbClust::NbClust(data = matrix.hel, 
                            diss = matrix.dist, 
                            distance = NULL, 
                            method = method_c, 
                            index = "silhouette", 
                            min.nc = 2, 
                            max.nc = 26)
    
    penalty <- maptree::kgs(cluster = cluster, 
                            diss = matrix.dist, 
                            maxclust = 26)
    penalty <- sort(penalty)
    
    penalty_min[i] <- names(penalty[1])
    
    n_clust_sil[i] <- obj$Best.nc
    
    t <- ncol(matrix.hel)*nrow(matrix.hel)
    s <- sum(matrix.hel == 0)
    perc_zero[i] <- (s/t)*100
    #sil <- factoextra::fviz_nbclust(obj, method = "silhouette")

  }
  
  df <- cbind(c, dnorm, n_sp, n_clust_sil, penalty_min, perc_zero)
  df <- df[50:100,]
  thresh <- c(50:100)
  df <- cbind(thresh,df)
  df <- as.data.frame(df)
  
  write.table(df, 
              file = paste0("outputs/df_loop_", method_c, "_", arms_id, ".csv"),
              dec = ",", 
              sep = ";",
              row.names = FALSE)
  
  
  loop_name <- paste0("loop", "_", method_c,"_", arms_id, ".pdf")
  
  loop_path <- here::here("outputs", loop_name)
  
  pdf(file =  loop_path, width = 12, height = 6)
  
  par(mfrow = c(1, 2))

  plot(df$c~df$thresh, 
       type = "l", 
       main = paste0("Indice de corrélation cophénétique en 
                     fonction du threshold - ", method_c, " - \n", arms_id),
       xlab = "threshold",
       ylab = "c",
       las = 1,
       cex.axis = 0.5)
  #abline(h = min(df$c), col = "blue")
  #abline(v = 94, col = "blue")
  #axis(1, at = c(94),labels = c("94"), las = 1)
  #axis(2, at = 0.6107368, labels = "0.61", las = 1)
  plot(df$dnorm~df$thresh, 
       type = "l",
       main = paste0("Indice 2-norm en fonction du threshold - \n", method_c,
                     " - ", arms_id),
       xlab = "threshold",
       ylab = "2-norm",
       las = 1,
       cex.axis = 0.5)
  abline(h = min(df$dnorm), col = "blue", las = 1)
  abline(v = 86, col = "blue", las = 1)
  axis(1, at = c(86),labels = c("86"), las = 1)
  axis(2, at = 0.3571637, labels = "0.35", las = 1)
  
  dev.off()
  
  return(df)
}
