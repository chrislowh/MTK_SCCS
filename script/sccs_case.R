##This script defines cases used in the self-controlled case series for the MTK project

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

#create summary dataframe containing all useful information (for constructing flowcharts afterwards)
summary <- data.frame(step = character(),
                      number = numeric())

#create a list of patients with index montelukast prescription between 1 Jan 2011 to 12 Dec 2018
#set working directory
setwd(data_dir)

#read in all prescription for montelukast (from 1999/12/01 to 2018/12/31)
MTK_rx <- read.csv('raw/MTK_rx.csv', header = TRUE)

#create a vector containing a list of all patients with montelukast prescription
MTK_pt_list <- unique(MTK_rx$Reference.Key.)

#add length of patients into summary
summary <- rbind(summary, 
                 data.frame(step = '# of patients with montelukast prescription between 1 Jan 2011 to 12 Dec 2018', 
                            number = print(length(MTK_pt_list))
                            ) 
                 )

try <- MTK_rx[1:150,]

unique(try$Reference.Key.)