library(targets)

tar_source()

list(
   tar_target(raw_data, "data/raw-data/Data_sans_UNAV-NR-OROS.csv", format = "file")
   
  ,tar_target(campain_id, "CINA") 
  
  ,tar_target(metadata_data, data_arms(raw_data = raw_data, 
                                       arms_id = campain_id), format = "file")
  ,tar_target(ab_thresh, 94)
  
  ,tar_target(dattresh, data_red(meta_and_data = metadata_data, 
                                 ab_thresh = ab_thresh), format = "file")
  
  ,tar_target(mean_arms, mean_by_arms(meta_and_data = metadata_data,
                                          dat_thresh_path = dattresh,
                                          ab_thresh = ab_thresh), format = "file")
  
  ,tar_target(clust_method, "ward.D") #the agglomeration method to be used. 
                                    #This should be (an unambiguous abbreviation of) one of "ward.D", "ward.D2", "single", "complete", 
                                    #"average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC).
  
  ,tar_target(clust_and_coph, clustering_and_cophenetic(dat_thresh_red_path = mean_arms,
                                                        ab_thresh = ab_thresh,
                                                        method_c = clust_method,
                                                        arms_id = campain_id))
)
