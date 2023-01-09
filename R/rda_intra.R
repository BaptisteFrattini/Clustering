#' RDA for intra ARMS variations
#' 
#' RDA is a method combining regression and principal component analysis (PCA).
#' It is a direct extension of multiple regression analysis to 
#' model multivariate response data. RDA is an extremely 
#' powerful tool in the hands of ecologists, especially since the 
#' introduction of the Legendre and Gallagher (2001) transformations that 
#' opened RDA to the analysis of community
#' composition data (transformation-based RDA, or tb-RDA).
#'
#' @param meta_and_data
#' @param arms_id
#' @param dat_thresh
#' @param ab_thresh
#'
#' @return the path to the rda plot
#' @export
#' 


fun_rda_intra <- function(meta_and_data, arms_id, dat_thresh_path, ab_thresh) {
  
  # meta_and_data <- targets::tar_read(metadata_data)
  # arms_id = targets::tar_load("campain_id") 
  # dat_thresh_path = targets::tar_read("dattresh")
  # ab_thresh = targets::tar_load("ab_thresh") 
  
  
  #### load data and meta ####
  meta_path <- meta_and_data[grepl("metadata", meta_and_data)] 
  meta <- read.csv(meta_path)
  dat <- read.csv(dat_thresh_path)
  matrix.h <- vegan::decostand(dat, "hellinger")
  matrix.env <- as.data.frame(cbind(meta$b_m_t, meta$o_c, meta$Orientation, meta$station, meta$arms_name))
  matrix.env$V1 <- as.factor(matrix.env$V1)
  matrix.env$V2 <- as.factor(matrix.env$V2)
  matrix.env$V3 <- as.factor(matrix.env$V3)
  matrix.env$V4 <- as.factor(matrix.env$V4)
  matrix.env$V5 <- as.factor(matrix.env$V5)
  colnames(matrix.env) = c("b_m_t", "o_c", "u_d","station","arms")
  
  #### computing RDA ####
  library(vegan)
  part.rda <- vegan::rda(matrix.h ~ o_c + u_d + Condition(station),  
                       data = as.data.frame(matrix.env) ) 
  part.rda$CCA$v[c(2,4),] <- 0.4*(part.rda$CCA$v[c(2,4),1])
  anova.cca(part.rda, permutations = how(nperm = 999), by = "term")
  R <- RsquareAdj(part.rda)$adj.r.squared
  sc <- scores(part.rda, scaling = 2)
  spe.sc <- as.data.frame(sc$species)
  
  
  #### species selection based on sc2 scores ####
  # Cette formule applique la fonction any sur chaque ligne de spe.sc pour 
  # vérifier si au moins un élément de la ligne a une valeur absolue supérieure 
  # à 0,02, et retourne uniquement les lignes pour lesquelles cela est vrai.
  spe.sc.red <- spe.sc[apply(spe.sc, 1, function(x) any(abs(x) > 0.07)), ]
  nrow(spe.sc.red)
  
  
  sites.sc <- as.data.frame(sc$sites)
  centro.sc <- as.data.frame(sc$centroids)
  rownames(centro.sc) <- c("CLOSE","OPEN","DOWN","UP") 
  
  
  #### plot rda ####
  rda_name <- paste0("rda_", ab_thresh, "_", arms_id, ".pdf")
  rda_path <- here::here("outputs/intra", rda_name)
  pdf(file =  rda_path, width = 12, height = 8)
  
  plot(part.rda,
       type = "points",
       lty = 3,
       xlim = c(-0.5:0.5),
       scaling = 2,
       display = c("sp", "lc", "cn"),
       main = paste0("Triplot RDA - Bryozoa - scaling 2 - R2adj = ", round(R,3)))
  

  with(matrix.env, points(part.rda, display = "cn", col = "mediumblue",
                          scaling = 2, pch = 24, cex = 1.9, bg = "mediumblue"))
  
  text(spe.sc.red$RDA1 + 0.01, spe.sc.red$RDA2 + 0.01, row.names(spe.sc.red), adj = 0, cex = 0.8, font = 2)
  text(centro.sc$RDA1 + 0.01, centro.sc$RDA2 + 0.01, row.names(centro.sc), adj = c(1,-1), cex = 0.8, col = "blue", font =2)
  
  
  points(sites.sc$RDA1, sites.sc$RDA2, cex = 0.07)
  
  #arrows(0, 0,
  #       spe.sc[, 1] * 0.92,
  #       spe.sc[, 2] * 0.92,
  #       length = 0.05,
  #       col = "red" ,
  #       lwd = 2)

  return(rda_path)
   
}