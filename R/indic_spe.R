#' Give the indicator species for the group of sites determinated by the clustering
#'
#' @param meta_and_data path to meta data and data 
#'
#' @return fidg.1 et 2 data frame (indval result from clust 1 and clust 2)
#' @export
#'
indic_spe <- function(meta_and_data) {
  
  #meta_and_data <- targets::tar_read("metadata_data") 
  
  # Create an object that select the file that content data and
  # another object that content the meta data
  data_path <- meta_and_data[!grepl("metadata", meta_and_data)]
  dat <- read.csv(data_path)
  
  meta_path <- meta_and_data[grepl("metadata", meta_and_data)] 
  meta_dat <- read.csv(meta_path)
  
  arms_name <- meta_dat$arms_name
  
  tab <- NULL
  U <- NULL
  
  for (i in 1:ncol(dat)) {
    
    U <- tapply(dat[,i], 
                arms_name, 
                mean)
    
    tab <- cbind(tab,
                 U)
  }
  
  N <- colnames(dat)
  colnames(tab) <- N
  tab <- as.data.frame(tab)
  
  #Enter clustering suggestions
  clust.1 <- as.factor(c(rep("RUNA_1", 3), rep("RUNA_2_3", 6), rep("RUNA_4_5_6", 9), rep("RUNA_7_8_9", 9)))
  clust.2 <- as.factor(c(rep("RUNA_1_2_3_4_5_6", 18), rep("RUNA_7_8_9", 9)))
  
  iva.1 <- labdsv::indval(tab, clust.1, numitr = 1000)
  pval.1 <- iva.1$pval
  
  name.clust.1 <- levels(clust.1)
  gr.1 <- iva.1$maxcls[pval.1 <= 0.05]
  name.1 <- name.clust.1[c(gr.1)]
  iv.1 <- round(iva.1$indcls[pval.1 <= 0.05], 3)
  pv.1 <- as.numeric(iva.1$pval[pval.1 <= 0.05])

  
  fidg.1 <- data.frame(sp = names(gr.1),
                       name = name.1,
                       indval = iv.1,
                       pvalue = pv.1)
  fidg.1 <- fidg.1[order(fidg.1$name),]
  
  write.table(fidg.1, 
              file = "outputs/df_indval.1.csv", 
              dec = ",", 
              sep = ";",
              row.names = FALSE)
  
  iva.2 <- labdsv::indval(tab, clust.2, numitr = 1000)
  pval.2 <- iva.2$pval 
  
  name.clust.2 <- levels(clust.2)
  gr.2 <- iva.2$maxcls[pval.2 <= 0.05]
  name.2 <- name.clust.2[c(gr.2)]
  iv.2 <- round(iva.2$indcls[pval.2 <= 0.05], 3)
  pv.2 <- as.numeric(iva.2$pval[pval.2 <= 0.05])
  
  fidg.2 <- data.frame(sp = names(gr.2),
                       name = name.2,
                       indval = iv.2,
                       pvalue = pv.2)
  fidg.2 <- fidg.2[order(fidg.2$name),]

  write.table(fidg.2, 
              file = "outputs/df_indval.2.csv", 
              dec = ",", 
              sep = ";",
              row.names = FALSE)
  
  return(c(fidg.1,fidg.2))
}
