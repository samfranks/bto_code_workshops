### SEF 01/02/19


### KBJ 31/01/19

# Assign tenkms habitat classes using a two step method defined below:

# 10 landscape types derived from LCM2000 data: Broad-leaf Woodland; Conifer Woodland; Arable; Improved Grassland; Semi-natural Grassland; Mountain/Heath/Bog; Open Water; Urban; Coast; Sea.
# 10-km squares are defined as contributing to a landscape type using two steps, developed for Scottish landscape types: 
#       1) First, identify 1-km squares with 30% or more of the particular habitat.
#       2) Second, select a 10-km square as contributing to a particular landscape type if 70% or more of 1-km squares are defined as contributing to that landscape type (except for fragmented Montane, Coastal and Urban habitats, where the threshold is reduced to 30% or more).
# ... Therefore none of the landscape types is mutually exclusive.
# Broad landscape types are being used because:
#       (a) Bird Atlas data collection was designed to be reported at 10-km scale; and
#       (b) Scotland tends not to have large areas of discrete habitat types and, rather, habitat mosaics (i.e. variations over the landscape) are more usual.


#load  packages
library(readr) # for faster file reading than the base R functions
library(dplyr) # for data manipulation using pipes and groupings
library(BTOTools) # for rescaling 1kms to 10kms (rescale_1km_to_10km)
library(tidyr)

projectwd <- paste("C:/Users/samf/Documents/Git", "bto_workshops", "2019_02", sep="/")
# read data 
# area of LCM2000 habitat type per onekm
habitat_data <- read_csv(paste(projectwd, "onekm_LCM2000.csv", sep="/")) # adjust the path to where the data lives on your computer
names(habitat_data)
# names of condensed habitat types, which I've called  'Class'
lcm_codes <- read_csv(paste(projectwd, "LCM2000_class.csv", sep="/"))
names(lcm_codes)

# make habitat classes factors
habitat_data$Code <- as.factor(habitat_data$Code)
lcm_codes$Code <- as.factor(lcm_codes$Code)
lcm_codes$AggCode <- as.factor(lcm_codes$AggCode)

# join AggCodes (same breakdown as Class, just numbers for brevity) to the habitat data
# AggCodes are the 26 LCM classes simplified into 10 

habitat_data  <- inner_join(habitat_data, lcm_codes, by = "Code")
drop <- c("Code", "LCM2000_class", "Desc") # remove unwanted columns
habitat_data  <- habitat_data[ , !(names(habitat_data) %in% drop)]


# Calculate the area of each onekm

#### NB!! - Make sure this is done prior to combining broadleaf + coniferous woodland into its
#### own factor, as this will mess with the total area per onekm

habitat_data %>% 
  group_by(onekm) %>% # group_by sets up the grouping structure for subsequent operations
  summarise(TotArea = sum(Area)) -> onekm_tot_areas

##############

# Make an AWOOD class (All Woodland = broadleaf + coniferous woodland)

hab_BWOOD<-subset(habitat_data, habitat_data$AggCode == "1")
hab_CWOOD<-subset(habitat_data, habitat_data$AggCode == "2")

hab_BC_WOOD  <- rbind(hab_BWOOD, hab_CWOOD)

# add in columns for AggCode and Class

hab_BC_WOOD$AggCode <- "11"
hab_BC_WOOD$Class <- "AWOOD"

# Attach to habitat_data

habitat_data<- rbind(habitat_data, hab_BC_WOOD)
############

# calculate area of each habitat in each square

habitat_data %>%
  group_by(onekm, AggCode) %>% # use AggCode
  summarise(Area_Habitat = sum(Area)) -> onekm_habitat_areas

# combine 2 tibbles so that tot areas and habitat areas are in the same tibble

area_data <- inner_join(onekm_tot_areas, onekm_habitat_areas, by = "onekm")


#  calculate the proportion of each habitat class (AggCode) parcel in each onekm 
# (because there are sometimes more than one parcel of a particular class in a onekm square)

#### ====== SF
# ----- This way is faster
# You don't need to loop through each row because R performs vectorised (I think this is the right word to use) operations
area_data$Prop_Hab <- area_data$Area_Habitat / area_data$TotArea



# # ---- Loop not needed
# # add empty column
# area_data$Prop_Hab <- NA
# 
# system.time(
#   # calculate proportion of total area a habitat in each square
#   for (i in 1:nrow(area_data)) { 
#     prop = area_data$Area_Habitat[i] / area_data$TotArea[i]
#     area_data$Prop_Hab[i] <- prop
#   }
# )

# check adds up to 1 

area_data[!area_data$AggCode == "11",] %>% # only works without AWOOD included
  group_by(onekm) %>%
  summarise(Tot_prop = sum(Prop_Hab))# check!


#####################################################################################

# ------ SEF way

agg_code_values <- data.frame(AggCode=sort((unique(area_data$AggCode))), HabClass=c("BWOOD","CWOOD","ARABL","IGRAS","NGRAS","MHEBO","OWATR","URB","CST","SEA","AWOOD"))

area_data$hab_col_value <- ifelse(area_data$Prop_Hab >= 0.3, 1, 0)
area_data <- left_join(area_data, agg_code_values, by="AggCode")

# turn area_data from long to wide with HabClass as the new 'wide' variables, populated by value = hab_col_value using tidyr::spread()#
# this fills other HabClass columns with NA, so for value=NA then replace with 0 using mutate_at and replace_na
area_data <- tidyr::spread(area_data, key=HabClass, value=hab_col_value) %>% mutate_at(vars(ARABL:URB), replace_na, 0)


# # ------ KBJ way
# 
# # STEP 1
# # Identify 1-km squares with 30% or more of the particular habitat
# 
# 
# # Create columns of presence/absence based on a threshold for each habitat AggCode
# 
# # Create blank columns for each habitat classification
# area_data$BWOOD <- NA # 1
# area_data$CWOOD <- NA # 2
# area_data$ARABL <- NA # 3
# area_data$IGRAS <- NA # 4
# area_data$NGRAS <- NA # 5
# area_data$MHEBO <- NA # 6
# area_data$OWATR <- NA # 8
# area_data$URB <- NA # 7
# area_data$CST <- NA # 9
# area_data$SEA <- NA # 10
# area_data$AWOOD <- NA # 11 # All Woodland - extra one to match Scotland
# 
# #### Populate habitat columns ####
# # Doing each separately - is there a way to link all together?
# 
# # Note: this only works for lines in the dataframe that have the code for the habitat (31/01/19 update: I can't remember what I mean by this...)
# 
# # Mountain, heath, bog 
# for(i in 1:nrow(area_data)){
#   
#   if (area_data$AggCode[i] == "6" & # MHEBO	Mountain, heath, bog            
#       area_data$Prop_Hab[i] >= 0.3) {
#     
#     area_data$MHEBO[i] <- 1
#   } else {  
#     area_data$MHEBO[i] <- 0 
#   }
# }
# 
# # Semi-natural grass
# 
# for(i in 1:nrow(area_data)){
#   
#   if (area_data$AggCode[i] == "5" & # NGRAS	Semi-natural grass          
#       area_data$Prop_Hab[i] >= 0.3) {
#     
#     area_data$NGRAS[i] <- 1
#   } else {  
#     area_data$NGRAS[i] <- 0 
#   }
# }
# 
# # Improved grass
# 
# for(i in 1:nrow(area_data)){
#   
#   if (area_data$AggCode[i] == "4" & # IGRAS	Improved grassland              
#       area_data$Prop_Hab[i] >= 0.3) {
#     
#     area_data$IGRAS[i] <- 1
#   } else {  
#     area_data$IGRAS[i] <- 0 
#   }
# }
# 
# # Arable and horticulture         
# 
# for(i in 1:nrow(area_data)){
#   
#   if (area_data$AggCode[i] == "3" & # ARABL	Arable and horticulture         
#       area_data$Prop_Hab[i] >= 0.3) {
#     
#     area_data$ARABL[i] <- 1
#   } else {  
#     area_data$ARABL[i] <- 0 
#   }
# }
# 
# # Broad-leaved / mixed woodland
# 
# for(i in 1:nrow(area_data)){
#   
#   if (area_data$AggCode[i] == "1" & # BWOOD	Broad-leaved / mixed woodland        
#       area_data$Prop_Hab[i] >= 0.3) {
#     
#     area_data$BWOOD[i] <- 1
#   } else {  
#     area_data$BWOOD[i] <- 0 
#   }
# }
# 
# # Coniferous woodland
# 
# for(i in 1:nrow(area_data)){
#   
#   if (area_data$AggCode[i] == "2" & # CWOOD	Coniferous woodland
#       area_data$Prop_Hab[i] >= 0.3) {
#     
#     area_data$CWOOD[i] <- 1
#   } else {  
#     area_data$CWOOD[i] <- 0 
#   }
# }
# 
# 
# # All woodland
# 
# for(i in 1:nrow(area_data)){
#   
#   if (area_data$AggCode[i] == "11" & # AWOOD	All woodland
#       area_data$Prop_Hab[i] >= 0.3) {
#     
#     area_data$AWOOD[i] <- 1
#   } else {  
#     area_data$AWOOD[i] <- 0 
#   }
# }
# 
# # Coast
# 
# for(i in 1:nrow(area_data)){
#   
#   if (area_data$AggCode[i] == "9" & # CST
#       area_data$Prop_Hab[i] >= 0.3) {
#     
#     area_data$CST[i] <- 1
#   } else {  
#     area_data$CST[i] <- 0 
#   }
# }
# 
# # Urban
# 
# for(i in 1:nrow(area_data)){
#   
#   if (area_data$AggCode[i] == "7" & # URB
#       area_data$Prop_Hab[i] >= 0.3) {
#     
#     area_data$URB[i] <- 1
#   } else {  
#     area_data$URB[i] <- 0 
#   }
# }
# 
# # Sea
# 
# for(i in 1:nrow(area_data)){
#   
#   if (area_data$AggCode[i] == "10" & # SEA
#       area_data$Prop_Hab[i] >= 0.3) {
#     
#     area_data$SEA[i] <- 1
#   } else {  
#     area_data$SEA[i] <- 0 
#   }
# }
# 
# # Standing open water
# 
# for(i in 1:nrow(area_data)){
#   
#   if (area_data$AggCode[i] == "8" & # OWATR
#       area_data$Prop_Hab[i] >= 0.3) {
#     
#     area_data$OWATR[i] <- 1
#   } else {  
#     area_data$OWATR[i] <- 0 
#   }
# }


###################################
# make tibble that shows maximum value (either 0 or 1) for each category
# this will then be used in STEP 2 to calc proportion of onekms per tenkm that have habitat classifcation

area_data %>%
  group_by(onekm) %>%
  summarise(BWOOD = max(BWOOD), 
            CWOOD = max(CWOOD),
            AWOOD = max(AWOOD),
            ARABL = max(ARABL),
            IGRAS = max(IGRAS),
            NGRAS = max(NGRAS),
            MHEBO = max(MHEBO),
            OWATR = max(OWATR),
            URB = max(URB),
            CST = max(CST),
            SEA = max(SEA)) -> nihab1km


#######################################################################

# STEP 2
# select a 10-km square as contributing to a particular landscape type if 70% or more
# of 1-km squares are defined as contributing to that landscape type
# (except for fragmented Montane, Coastal and Urban habitats,
# where the threshold is reduced to 30% or more).

#### Use some of the birdatlas data and package to get 10kms
northernireland<-read.table(paste(projectwd, 'northernireland1km.csv', sep="/"), header=T,stringsAsFactors = F)

# get tenkm reference for each square
rescale<- rescale_1km_to_10km(northernireland, "onekm")

# join rescale tenkm reference to habitat data table
nihab1km  <- inner_join(nihab1km, rescale, by = "onekm")

# save this out for reference
# write_csv(nihab1km, "C:/Atlas/atlas_ni_birds/analysis/resources/nihab1km.csv")

#############
# Make a table with total number of onekms per tenkm 
# and number of onekms with habitat type per tenkm

nihab1km %>% 
  group_by(tenkm) %>% # group_by sets up the grouping structure for subsequent operations
  summarise(Sq_Count = length(unique(onekm)),
            BWOOD = sum(BWOOD)/Sq_Count, # sum of BWOOD onekms in tenkm divided by number of onekm squares to get proportion
            CWOOD = sum(CWOOD)/Sq_Count,
            AWOOD = sum(AWOOD)/Sq_Count,
            ARABL = sum(ARABL)/Sq_Count,
            IGRAS = sum(IGRAS)/Sq_Count,
            NGRAS = sum(NGRAS)/Sq_Count,
            MHEBO = sum(MHEBO)/Sq_Count,
            OWATR = sum(OWATR)/Sq_Count,
            URB = sum(URB)/Sq_Count,
            CST = sum(CST)/Sq_Count,
            SEA = sum(SEA)/Sq_Count) -> tenkm_habitat_prop_initial

# make a copy to add to

tenkm_habitat_prop <- tenkm_habitat_prop_initial
# Make a new table with a yes no for habitat types in a tenkm
# threshold is 0.7, except for fragmented Mountain/heath/bog, Coastal and Urban habitats,
# where the threshold is reduced to 30% or more


# can alter these thresholds to try to improve tenkm allocation to at least one habitat classification
threshold <- 0.6
smaller_threshold <- 0.3


# ------ SEF way



# ------ KBJ way

# second round of if else statements per habitat class

# Broadleaf woodland

for(i in 1:nrow(tenkm_habitat_prop)){
  
  if (tenkm_habitat_prop$BWOOD[i] >=smaller_threshold ){
    tenkm_habitat_prop$BWOOD[i] <- "Y"
  } else {  
    tenkm_habitat_prop$BWOOD[i] <- ""
  }
}

# Conifer woodland

for(i in 1:nrow(tenkm_habitat_prop)){
  
  if (tenkm_habitat_prop$CWOOD[i] >= smaller_threshold){
    tenkm_habitat_prop$CWOOD[i] <- ""
  } else {  
    tenkm_habitat_prop$CWOOD[i] <- ""
  }
}

# All woodland

for(i in 1:nrow(tenkm_habitat_prop)){
  
  if (tenkm_habitat_prop$AWOOD[i] >= smaller_threshold){
    tenkm_habitat_prop$AWOOD[i] <- "Y"
  } else {  
    tenkm_habitat_prop$AWOOD[i] <- ""
  }
}

# Arable 

for(i in 1:nrow(tenkm_habitat_prop)){
  
  if (tenkm_habitat_prop$ARABL[i] >= threshold){
    tenkm_habitat_prop$ARABL[i] <- "Y"
  } else {  
    tenkm_habitat_prop$ARABL[i] <- ""
  }
}

# Improved grassland 

for(i in 1:nrow(tenkm_habitat_prop)){
  
  if (tenkm_habitat_prop$IGRAS[i] >= threshold){
    tenkm_habitat_prop$IGRAS[i] <- "Y"
  } else {  
    tenkm_habitat_prop$IGRAS[i] <- ""
  }
}

# Semi-nat grassland 

for(i in 1:nrow(tenkm_habitat_prop)){
  
  if (tenkm_habitat_prop$NGRAS[i] >= threshold){
    tenkm_habitat_prop$NGRAS[i] <- "Y"
  } else {  
    tenkm_habitat_prop$NGRAS[i] <- ""
  }
}

# Mountain heath bog - 30% threshold

for(i in 1:nrow(tenkm_habitat_prop)){
  
  if (tenkm_habitat_prop$MHEBO[i] >= smaller_threshold){
    tenkm_habitat_prop$MHEBO[i] <- "Y"
  } else {  
    tenkm_habitat_prop$MHEBO[i] <- ""
  }
}

# Standing open water

for(i in 1:nrow(tenkm_habitat_prop)){
  
  if (tenkm_habitat_prop$OWATR[i] >= threshold){
    tenkm_habitat_prop$OWATR[i] <- "Y"
  } else {  
    tenkm_habitat_prop$OWATR[i] <- ""
  }
}

# Urban - 30% threshold

for(i in 1:nrow(tenkm_habitat_prop)){
  
  if (tenkm_habitat_prop$URB[i] >= smaller_threshold){
    tenkm_habitat_prop$URB[i] <- "Y"
  } else {  
    tenkm_habitat_prop$URB[i] <- ""
  }
}

# Coastal - 30% threshold

for(i in 1:nrow(tenkm_habitat_prop)){
  
  if (tenkm_habitat_prop$CST[i] >= smaller_threshold){
    tenkm_habitat_prop$CST[i] <- "Y"
  } else {  
    tenkm_habitat_prop$CST[i] <- ""
  }
}

# Sea - 30% threshold

for(i in 1:nrow(tenkm_habitat_prop)){
  
  if (tenkm_habitat_prop$SEA[i] >= threshold){
    tenkm_habitat_prop$SEA[i] <- "Y"
  } else {  
    tenkm_habitat_prop$SEA[i] <- ""
  }
}

#############################################################################

# Check whether tenkms have no habitat classification assigned...

# change all Y to 1 to count per tenkm
tenkm_habitat_num <- data.frame(lapply(tenkm_habitat_prop, function(x) as.numeric(x!="")))

for (i in 1:nrow(tenkm_habitat_num)) { 
  TOT = sum(tenkm_habitat_num[i, 3:13])
  tenkm_habitat_num$TOT[i] <- TOT
}

tot_unallocated <- length(which(tenkm_habitat_num$TOT ==0))
tot_tenkms <- nrow(tenkm_habitat_num)

# percentage not allocated at least 1 habitat
percent_unallocated <- tot_unallocated/tot_tenkms *100
percent_unallocated

# Have a quick look at how many tenkms have each classsification

length(which(tenkm_habitat_num$BWOOD ==1))
length(which(tenkm_habitat_num$CWOOD ==1))
length(which(tenkm_habitat_num$AWOOD ==1))
length(which(tenkm_habitat_num$ARABL ==1))
length(which(tenkm_habitat_num$IGRAS ==1))
length(which(tenkm_habitat_num$NGRAS ==1))
length(which(tenkm_habitat_num$MHEBO ==1))
length(which(tenkm_habitat_num$OWATR ==1))
length(which(tenkm_habitat_num$URB ==1))
length(which(tenkm_habitat_num$CST ==1))
length(which(tenkm_habitat_num$SEA ==1))

# have a play around with threshold and smaller_threshold values to see how this changes numbers allocated

##################################################################################

# Conclusion after some messing around with the thresholds...
# threshold has to be pretty low to get all squares assigned a habitat type- problematic.


# prepare to export to .csv

# get rid of Sq_Count column
tenkm_habitat <- tenkm_habitat_prop[, -2]

# save for use in NI Atlas analysis scripts
write_csv(tenkm_habitat, "C:/Atlas/atlas_ni_birds/analysis/resources/nihab10km.csv")
