############################################################
#
# make.R: build the project
#
############################################################

#fisrt run, use this : 
#install.packages(renv)

# dependences management

renv::init()
renv::install()
renv::status()
renv::snapshot()


# make the pipeline
targets::tar_visnetwork()
targets::tar_make()
targets::tar_visnetwork()


# check targets values

# tar_load("campain_id")
# targets::tar_read("metadata_data")
# 
# d <- read.csv(tar_read("working_data"))
