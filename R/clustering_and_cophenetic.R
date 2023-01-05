#' plot the clusterings and the cophenetic correlation c and 2norm
#'
#' @param dat_thresh_path the path to the file data red
#' @param ab_thresh the value of the threshold
#' @param method_c
#' @param arms_id
#' @return path to the data file, reduced and mean by site
#' @export
#'

clustering_and_cophenetic <- function(dat_thresh_red_path, ab_thresh, method_c, arms_id) {

  #dat_thresh_red_path = targets::tar_read(mean_arms)
  #ab_thresh = targets::tar_load("ab_thresh") 
  #method_c = targets::tar_load("clust_method") 
  #arms_id = targets::tar_load("campain_id") 
  
  dat_thresh_red_path <- read.csv(dat_thresh_red_path, 
                                  header = TRUE,
                                  row.names = 1)
  matrix.hel = vegan::decostand(dat_thresh_red_path, "hellinger")
  matrix.dist = vegan::vegdist(matrix.hel)
  cluster <- hclust(matrix.dist, method = method_c)
  
  clust_and_coph_name <- paste0("clust_and_coph_", ab_thresh, "_", method_c,"_", arms_id, ".pdf")
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
                      method.hclust = method_c,
                      method.dist = braycurtis,
                      parallel = TRUE)
  plot(plot0)

  # Unbiased p-val = red
  # Bootstrap probability = green
  
  
 
  # Cophenetic correlation computing
  coph <- stats::cophenetic(cluster)
  cor(matrix.dist, coph)
  
  # 2-norm value computing
  dnorm <- clue::cl_dissimilarity(matrix.dist,
                   cluster,
                   method = "spectral")    
  
  # plot the graph of the correlation
  plot2 <- plot(matrix.dist,coph,
           xlab = paste0("distance  de ", method_c),
           ylab = "Cophenetic distance",
           asp = 1,
           xlim = c(0.1, 0.5),
           ylim = c(0.1, 0.4),
           panel.first = abline(0, 1),
           main = paste("Cophenetic correlation =", round(cor(matrix.dist, coph), 3), "\n 2-norm = ", round(dnorm,3)))

  dev.off()
  #plot a clean dendrogram
  
  hc <- hclust(matrix.dist, method = method_c)
  plot1 <- gclus::reorder.hclust(hc,
                                 labels = row.names(dat_thresh_red_path),
                                 dis = matrix.dist, 
                                 maxclust = 26)

  penalty <- maptree::kgs(cluster = hc, 
                          diss = matrix.dist, 
                          maxclust = 26)
  penalty <- sort(penalty)
  
  penalty_min <- as.numeric(names(penalty[1]))
  
  clean_clust_name <- paste0("clean_clust_", ab_thresh, "_", method_c,"_", arms_id, ".pdf")
  clean_clust_path <- here::here("outputs", clean_clust_name)

  library(ggplot2)
  library(ggdendro)
  
  #convert cluster object to use with ggplot
  dendr <- ggdendro::dendro_data(hc, type="rectangle") 
  
  clust    <- cutree(hc,k=penalty_min)                    # find 2 clusters
  clust.df <- data.frame(label=names(clust), cluster=factor(clust))
  #dendr[["labels"]] has the labels, merge with clust.df based on label column
  dendr[["labels"]] <- merge(dendr[["labels"]],clust.df, by="label")
  
 aa <- ggplot() + 
    geom_segment(data=segment(dendr), aes(x=x, y=y, xend=xend, yend=yend)) + 
    geom_text(data=label(dendr), aes(x, y, label=label, hjust=-0.16, color=cluster)) +
    scale_colour_brewer("Clusters", palette = "Set1") +
    coord_flip() + scale_y_reverse(expand=c(0.2, 0)) + 
    theme(axis.line.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_rect(fill="white"),
          panel.grid=element_blank()) + ggtitle(paste0("Clustering avec ",
                     ncol(dat_thresh_red_path),
                     " sp (thresh = ", as.numeric(ab_thresh),
                     " ; method = ",
                     method_c,")")) 
 #+ geom_hline(yintercept = 0.2535)
 
 aa 
  
 ggsave(clean_clust_path, aa, width = 6, height = 8)

  
  
  return(clust_and_coph_path)
}







