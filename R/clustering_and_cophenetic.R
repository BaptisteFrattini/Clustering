#' plot the clusterings and the cophenetic correlation c and 2norm
#'
#' @param dat_thresh_path the path to the file data red
#' @param ab_thresh the value of the threshold
#'
#' @return path to the data file, reduced and mean by site
#' @export
#'

clustering_and_cophenetic <- function(dat_thresh_red_path, ab_thresh,braycurtis) {
  #braycurtis = targets::tar_read(braycurtis)
  #dat_thresh_red_path = targets::tar_read(dattresh_arms)
  #ab_thresh = targets::tar_load("ab_thresh") 
  
  dat_thresh_red_path <- read.csv(dat_thresh_red_path, 
                                  header = TRUE,
                                  row.names = 1)
  matrix.hel = vegan::decostand(dat_thresh_red_path, "hellinger")
  matrix.dist = vegan::vegdist(matrix.hel)
  cluster.UPGMA <- hclust(matrix.dist, method = "average")
  
  clust_and_coph_name <- paste0("clust_and_coph_", ab_thresh, "_UPGMA.pdf")
  clust_and_coph_path <- here::here("outputs", clust_and_coph_name)
  
  pdf(file =  clust_and_coph_path, width = 12, height = 6)
  par(mfrow = c(1, 2))
  
  #first plot
  #custom dist computing for pvclust (does not recognize b-c distance)
  braycurtis <- function(x) {
    x <- as.matrix(x)
    x <- t(x)
    res <- vegan::vegdist(x)
    res <- as.dist(res)
    attr(res, "method") <- "braycurtis"
    return(res)
  }
  
  plot0 <- pvclust::pvclust(t(matrix.hel),
                      method.hclust = "average",
                      method.dist = braycurtis,
                      parallel = TRUE)
  plot(plot0)
  
  # Unbiased p-val = red
  # Bootstrap probability = green
  
  #plot1 <- plot(cluster.UPGMA,
    #       labels = row.names(dat_thresh_red_path)
    #       main = paste0("Clustering avec ", ncol(dat_thresh_red_path)," sp (thresh = ", ab_thresh, " ; method = UPGMA)"), cex = 1)
 
  # Cophenetic correlation computing
  spe.ch.UPGMA.coph <- stats::cophenetic(cluster.UPGMA)
  cor(matrix.dist, spe.ch.UPGMA.coph)
  
  # 2-norm value computing
  dnorm <- clue::cl_dissimilarity(matrix.dist,
                   cluster.UPGMA,
                   method = "spectral")    
  
  # plot the graph of the correlation
  plot2 <- plot(matrix.dist,spe.ch.UPGMA.coph,
           xlab = "UPGMA distance",
           ylab = "Cophenetic distance",
           asp = 1,
           xlim = c(0.1, 0.5),
           ylim = c(0.1, 0.4),
           panel.first = abline(0, 1),
           main = paste("Cophenetic correlation =", round(cor(matrix.dist, spe.ch.UPGMA.coph), 3), "\n 2-norm = ", round(dnorm,3)))

  dev.off()
 
  
  return(clust_and_coph_path)
}







