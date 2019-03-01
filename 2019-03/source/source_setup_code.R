#######################################################################################
#
#
#   SOURCE CODE: Header code to install and load packages, set directory paths, print session info
#   
#
#######################################################################################



# ======================  SET SEED & PROJECT DETAILS  ====================

set.seed(seed_number)

project_name <- project_details$project_name

# output and workspaces sub-directory (e.g. for original analysis, revisions, accepted versions, etc)
output_version_date <- project_details$output_version_date
workspace_version_date <- project_details$workspace_version_date



# =================================  INSTALL and LOAD PACKAGES =================================


list.of.packages <- package_details

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, library, character.only=TRUE)



# =================================  SET DIRECTORY PATHS  ================================

# ---- Parent working directory, depending on workstation (usually the Git folder) ----

if (cluster) parentwd <- c("/users1/samf") # BTO cluster
if (!cluster) {
  if (!Mac) parentwd <- paste("C:/Users/samf/Documents/Git")
  if (Mac) parentwd <- paste("/Volumes/m3_bitlocker_disk2s1_/BTO PC Documents/Git")
}

# ---- Create sub-directories ----

projectwd <- paste(parentwd, project_name, sep="/")
codewd <- paste(projectwd, "code", sep="/")
datawd <- paste(projectwd, "data", sep="/")
outputwd <- paste(projectwd, "output", output_version_date, sep="/")
workspacewd <- paste(projectwd, "workspaces", workspace_version_date, sep="/")

# top level output and workspace directories
top_outputwd <- paste(projectwd, "output", sep="/")
top_workspacewd <- paste(projectwd, "workspaces", sep="/")

# create sub-directories if they don't exist (invisible() suppresses [[i]] NULL output from lapply)
dirs <- c(codewd, datawd, top_outputwd, top_workspacewd, outputwd, workspacewd)
lapply(dirs, function(x) {
  if (!dir.exists(x)) dir.create(x)
}) %>% invisible

options(digits=6)


# ================================  SESSION INFO  ===============================

# creates new session info .txt
# session info is captured in the most recent run of the code
# if using version control, then can see what the session_info was on each day you ran some code

session_info_file <- paste(projectwd, "session_info.txt", sep="/")
if (!file.exists(session_info_file)) file.create(session_info_file)

sink(session_info_file)
cat("\n# =============== Session info ==================\n", sep="\n")
cat("# ----  Date: ", date(), " --------\n\n")
print(sessionInfo())
sink()

