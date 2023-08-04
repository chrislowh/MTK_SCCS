##Data previously collected from Swathi, and taken up again after 2 years.
##This script reads in data (messy) I have right now, and to re-write useful files as .csv files to clean up things

#####################################################################################
## define working directories 
#data is not stored on github repository for patient confidentiality purposes
data_dir <- "C:/Users/chris/OneDrive - The University of Hong Kong/Montelukast and NEs/Research Project/R Environment/data/"

#currently, all scripts and results will be stored in working directory
work_dir <- getwd()
#####################################################################################

#loading packages
library(tidyverse)
library(data.table)

#set working directory to data_dir
setwd(data_dir)

#read in raw data, and re-write them into .csv files for better manipulation
#1. all diagnosis (MTK users)
# dataframe contains patient ID, dates and ICD-9 codes of all diagnosis for MTK users
load(file="raw/Montelukast_ddx.RData")
#rename object
MTK_dx <- diagnosis
rm(diagnosis)

#2. demographics (MTK users)
# dataframe contains patient ID, demographics (sex / age of birth / age of death) for MTK users
load(file="raw/Montelukast_demographics.RData")
#rename object
MTK_demo <- demo
rm(demo)

#3. dispensing records for montelukast 
# 'Montelukast' dataframe contains patient ID, date of dispensing, drug names, dosage, duration of dispensing (from 1999/12/01 to 2018/12/31)
# 'Mont1218' is a subset containing dispensing information for patients from 2012 to 2018 (no need to use this)
# 'Montelukast_new' is a list of patient IDs from 2012 to 2018 (no need to use this)
load(file="raw/Montelukast-new users.RData")
#rename object
MTK_rx <- Montelukast
rm(Montelukast, Mont1218, Montelukast_new)

#4. diagnosis records for patients with asthma (with or without MTK use)
load(file="raw/Asthma_all ddx.RData")
# This dataframe contains diagnosis records for all patients with asthma, from 1993 to 2019
# Containing diagnosis for all disorders, not only asthma
#rename object
asthma_dx <- asthma_ddx
rm(asthma_ddx)

#5. demographic information for patients with asthma (with or without MTK use)
# dataframe contains patient ID, demographics (sex / age of birth / age of death)
load(file="raw/Asthma_demo.RData") 
#named 'asthma_demo', no need rename

#6. dispensing records for all patients with asthma (with or without MTK)
asthma_rx <- readRDS(file='raw/Asthma_RX.RDS')


##saving these objects as .csv files, for more convenient manipulation (non-R applications)
df_names <- ls()

##for loop writing variables into .csv
for (i in df_names) {
  #get dataframe
  df <- get(i)
  #write as .csv files
  write.csv(df,file= paste0('raw/', i, '.csv'), row.names = FALSE)
  
}

# q()

# Check if the file exists
file_path <- "raw/Asthma_RX.RDS"
file.exists(file_path)

##checking data
MTK_dx_head <- MTK_dx[1:150,]

# Inpatient diagnoses and procedures?
# not sure if this is necessary, should all be coded under 'all diagnosis'?
load(file="raw/Montelukast_IP.RData")
load(file="raw/Asthma_IP.RData")


