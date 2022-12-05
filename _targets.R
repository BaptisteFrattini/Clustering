library(targets)

tar_source()

list(
   tar_target(raw_data, "data/raw-data/Data_sans_UNAV-NR-OROS.csv", format = "file")
  ,tar_target(campain_id, "RUNA")
  ,tar_target(ab_thresh, 99)
  ,tar_target(metadata_data, data_arms(raw_data = raw_data, arms_id = campain_id), format = "file")
)