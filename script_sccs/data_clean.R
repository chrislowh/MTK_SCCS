##This script produces cases and controls used in SCCS

#####################################################################################
## define working directories 
#data is not stored on github repository for patient confidentiality purposes
data_dir <- "C:/Users/chris/OneDrive - The University of Hong Kong/Montelukast and NEs/Research Project/R Environment/"

#currently, all scripts and results will be stored in working directory
work_dir <- getwd()
#####################################################################################

#loading packages
library(tidyverse)
library(data.table)

#set working directory to data_dir
setwd(data_dir)

#read in raw data, and re-write them into .csv files for better manipulation
#1. all diagnosis
# dataframe contains patient ID, dates and ICD-9 codes of all diagnosis for MTK users
load(file="data/raw/Montelukast_ddx.RData")
#rename object
MTK_dx <- diagnosis
rm(diagnosis)

#2. demographics
# dataframe contains patient ID, demographics (sex / age of birth / age of death) for MTK users
load(file="data/raw/Montelukast_demographics.RData")
#rename object
MTK_demo <- demo
rm(demo)

#3. prescription for montelukast users
# 'Montelukast' dataframe contains patient ID, date of dispensing, drug names, dosage, duration of dispensing (from 1999/12/01 to 2018/12/31)
# 'Mont1218' is a subset containing dispensing information for patients from 2012 to 2018 (no need to use this)
# 'Montelukast_new' is a list of patient IDs from 2012 to 2018 (no need to use this)
load(file="data/raw/Montelukast-new users.RData")
#rename object
MTK_rx <- Montelukast
rm(Montelukast, Mont1218, Montelukast_new)

# Check if the file exists
file_path <- "data/raw/Montelukast-new users.RData"
file.exists(file_path)

##checking data
MTK_dx_head <- MTK_dx[1:150,]

#Inpatient diagnoses and procedures? (not sure if this is necessary, should all be coded under 'all diagnosis'?)
load(file="data/raw/Montelukast_IP.RData")
#rename object
MTK_IP <- IP
rm(IP)

