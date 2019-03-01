#######################################################################################
#
#   BTO R workshop: data.table example
#
#   Name: Samantha Franks
#   Date: 05/03/2019
#
#######################################################################################


# ======================   Variables to pass to setup code source  ===========

# Header code to install and load packages, set directory paths, set seed, capture session info

# project_details <- list(project_name, output_version_name, workspace_version_name)
# package_details <- c("package name 1", "package name 2")

project_details <- list(project_name="bto_workshops/2019-03", output_version_date="", workspace_version_date="")
package_details <- c("raster","sp","rgeos","rgdal","tidyverse","birdring","data.table","pryr")
seed_number <- 1


# =================================  Determine system env  ================================

# LOCAL
if(.Platform$OS =='windows') {
  cluster <- FALSE
  Mac <- FALSE
}

# HPCBTO
if(.Platform$OS=='unix' & Sys.getenv('USER')=='samf') {
  cluster <- TRUE
  Mac <- FALSE
  Wales <- FALSE
}

# Mac
if(.Platform$OS=='unix' & Sys.getenv('USER')=='samantha') {
  cluster <- FALSE
  Mac <- TRUE
  Wales <- FALSE
}

# =======================    Read header source code   =================

# header code source:
# 1. sets working directories
# 2. loads required packages
# 3. prints session info to file
# 4. sets seed for reproducibility

source(paste(project_details$project_name, "source", "source_setup_code.R", sep="/"))

# # BTO cluster
# if (cluster) source(paste("/users1/samf", "source_setup_code.R", sep="/"))
# 
# # either PC or Mac
# if (!cluster) {
#   if (!Mac) source(paste("C:/Users/samf/Documents/Git/source_code", "source_setup_code.R", sep="/"))
#   if (Mac) source(paste("/Volumes/SAM250GB/BTO PC Documents/Git/source_code", "source_setup_code.R", sep="/"))
# }

# project directories created:
# parentwd = Git
# projectwd = eurasian_african_bird_migration_atlas
# codewd = directory containing code, functions, source, etc
# datawd = directory containing data
# outputwd = directory containing outputs and results (within the appropriate version date)
# workspacewd = directory containing workspace files (.rds, .rda, .RData; within the appropriate version date)
# topoutputwd = top level output directory
# topworkspacewd= top level workspace directory


# =====================  Logic & control statements  ======================

## Any logic or control statements that determine code pathways go here




# =================================  Load functions =================================

## Load any functions here


# ==============================  Load data  =============================

# # --------  Set up data to use for the workshop  -------
# dt <- readRDS(paste(datawd, "GBT.rds", sep="/")) # load GBT ringing data as RDS file
# dt[, date := as.Date(date.numeric, format="%d%m%Y")]
# 
# # turn coordinates field into decimal lat/lon
# # decimal degrees = degrees + (minutes / 60) + (seconds / 3600)
# 
# dt[, lat := (
#   as.numeric(substr(dt$coordinates, 2, 3)) +               # degrees
#     (as.numeric(substr(dt$coordinates, 4,5)) / 60) +       # minutes
#     (as.numeric(substr(dt$coordinates, 6,7)) / 3600)) *    # seconds
#     (ifelse(substr(dt$coordinates, 1, 1)=="+", 1, -1))     # north or south
#   ]
# 
# 
# dt[, lon := (
#   as.numeric(substr(dt$coordinates, 9, 11)) +              # degrees
#     (as.numeric(substr(dt$coordinates, 12,13)) / 60) +     # minutes
#     (as.numeric(substr(dt$coordinates, 14,15)) / 3600)) *  # seconds
#     (ifelse(substr(dt$coordinates, 8, 8)=="+", 1, -1))     # east or west
#   ]
# 
# 
# keep_fields <- strsplit(c("ring, metal.ring, spec.byscheme, sex.byscheme, age.byscheme, date, place.code, lat, lon, condition, circumstances, distance"), ", ") %>% unlist
# lose_fields <- names(dt)[-which(names(dt) %in% keep_fields)]
# 
# dt[, c(lose_fields) := NULL]
# setnames(dt, c("ring","metal.ring","species","sex","age","place.code","condition","circumstances", "distance","lat","lon","date"))
# 
# saveRDS(dt, paste(datawd, "example_ringing_data.rds", sep="/"))


# --------  Load data to use for the workshop  --------
dt <- readRDS(paste(datawd, "example_ringing_data.rds", sep="/")) # load reduced GBT ringing data as RDS file


# ==============================  Work with data  =============================

# --------  Inspect  --------
dt
str(dt)

# --------  Subset  --------
dt[species=="05410", ] # subset all curlew
dt[species=="05410" & (place.code=="GBNK" | place.code=="GBLI") & year(date) >= 2010, ] # subset all curlew, encountered in Norfolk or Lincs, since 2010
dt[order(ring,date), ] # order all individuals by encounter history (by ring, then date)
dt[order(ring, -date), ] # or by reverse encounter history
