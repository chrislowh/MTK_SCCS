#This script processes SCCS dataframe and report the descriptives of the cohort

#####################################################################################
## define working directories 
#data is not stored on github repository for patient confidentiality purposes
data_dir <- "C:/Users/chris/OneDrive - The University of Hong Kong/Montelukast and NEs/Research Project/R Environment/data/"

#currently, all scripts and results will be stored in working directory
work_dir <- getwd()
#####################################################################################

##loading packages
library(dplyr)
library(lubridate)
library(gtsummary)
library(gt)
library(magrittr)

##read in cleaned data
#set working directory to data
setwd(data_dir)
#read csv
MTK_master <- read.csv('cleaned/MTK_SCCS.csv', header = TRUE)

#set working directory
setwd(work_dir)

#convert age variables to numeric
MTK_master[, which(grepl('^age',colnames(MTK_master)))] <- apply(MTK_master[, which(grepl('^age',colnames(MTK_master)))], 2, function(x) as.numeric(as.character(x)))

#assign the type of asthma diagnosis that occured on or after the date of first montelukast dispensing
MTK_master$asthma_dx_type_1st <- apply(MTK_master, 1, function(x) {
  #get fist asthma diagnosis
  type_1st <- unlist(strsplit(x['asthma_dx_type'], ';'))[1]
  #assign to characters
  if (type_1st == 'I\n') { type_1st = 'Inpatient diagnoses'
  } else if (type_1st =='O\n') { type_1st = 'Outpatient visits'
  } else if (type_1st == 'A\n') { type_1st = 'Emergency admission'
  } else if (type_1st == 'E\n') { type_1st = 'Emergency admission'}
  #paste as vectors
  paste(type_1st)
})

#get number of asthma hospitalization
#on or before first date of montelukast dispensing
MTK_master$ip_1rx <- apply(MTK_master, 1, function(x) {
  #get indices of diagnoses that occurred on or before start date of observation
  dx_date_vec <- unlist(strsplit(x['asthma_dx_date'], ';'))
  MTK_1st <- unlist(strsplit(x['MTK_start_date'], ';'))[1]
  i <- which(as.Date(dx_date_vec) <= as.Date(MTK_1st))
  
  #get vector of asthma types (these diagnosis are all records) and subset by indices
  dx_type_vec <- unlist(strsplit(x['asthma_dx_type'], ';'))[i]
  
  #get number of hospitalization by getting Inpatienet diagnoses and Emergency admission
  num_ip <- sum(dx_type_vec == 'I\n' | dx_type_vec == 'A\n' | dx_type_vec == 'E\n')
  #code as >=3 if more than 3
  if (as.numeric(num_ip) >= 3) { num_ip = 'More than 3'}
  #paste
  paste(num_ip)
})


#convert variables to factor
factor <- 'sex|^asthma_dx_type_1st$|^ip_1rx$'
MTK_master[, which(grepl(factor,colnames(MTK_master)))] <- apply(MTK_master[, which(grepl(factor,colnames(MTK_master)))], 2, function(x) as.factor(as.character(x)))

#list of labels for the table
#demographics
label_demo <- list(sex ~ 'Gender',
     age_start_year ~ 'Age (years) at start of observation',
     age_rx_year ~ 'Age (years) at montelukast initiation',
     ip_1rx ~ 'Number of hospitalization at montelukast initiation',
     asthma_dx_type_1st ~ 'Type of first asthma diagnosis')

#comorbidities
#list of comorbidity
comorbid <- c('copd', 'ar', 'sinu', 'obesity', 'gerd', 'eczema', 'dm', 'ihd', 'arr', 'hf', 'htn', 'op')
comorbid_full <- c('Chronic obstructive pulmonary disease', 'Allergic Rhinitis', 'Sinusitis', 'Obesity', 'Gastroesophageal Reflux Disease (GERD)', 'Eczema', 'Diabetes Mellitus', 'Ischemic Heart Disease', 'Arrhythmia', 'Heart Failure', 'Hypertension', 'Osteoporosis')
#function
label_comorbid <- lapply(seq_along(comorbid), function(i) {
  short <- comorbid[i]
  full <- comorbid_full[i]
  as.formula(paste0("`", short, "_1rx` ~ '", full, "'"))
})

#medications
#list of medications
med <- c('SABA', 'MX', 'ICS_LABA', 'SAMA')
med_full <- c('Short-acting beta agonists (SABA)', 'Methylxanthines', 'Inhaled corticosteroids (ICS) and/or long-acting beta agonists (LABA)', 'Short acting muscarinic antagonists (SAMA)')
#function
label_med <- lapply(seq_along(med), function(i) {
  short <- med[i]
  full <- med_full[i]
  as.formula(paste0("`", short, "_1rx` ~ '", full, "'"))
})


#total
label_tbl <- c(label_demo, label_comorbid, label_med)

#select indices for columns to be included
col_i_vec <- c(
  #sex
  which(colnames(MTK_master) == 'sex'),
  #age
  which(colnames(MTK_master) == 'age_start_year' | colnames(MTK_master) == 'age_rx_year' ),
  #type of 1st asthma diagnosis
  which(colnames(MTK_master) == 'asthma_dx_type_1st'),
  #number of hospitalization on montelukast initiation
  which(colnames(MTK_master) == 'ip_1rx'),
  #comorbidities (on montelukast initiation)
  which(colnames(MTK_master) %in% paste0(comorbid, '_1rx')),
  #baseline medications
  which(colnames(MTK_master) %in% paste0(med, '_1rx'))
)


#summary for tables
#full
des_full <- MTK_master %>%
                  select(all_of(col_i_vec))

#save outputs as png and docx respectively
#png (as gtable)
des_full_gt <- des_full %>% 
  tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})",all_categorical() ~ "{n} ({p}%)"), 
              label = label_tbl) %>%
  as_gt()
gtsave(des_full_gt, filename = paste0(work_dir,'/output/descriptives/summary_full.png'))
gtsave(des_full_gt, filename = paste0(work_dir,'/output/descriptives/summary_full.docx'))

#by gender
des_sex_gt <- MTK_master %>%
  select(all_of(col_i_vec)) %>%
  tbl_summary(by = sex,
              statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)"), 
              label = label_tbl[-c(which(sapply(label_tbl, function(x) any(grepl('sex', x, ignore.case = TRUE)))))]) %>%
  add_p() %>%
  as_gt()
#save outputs as png and docx respectively
gtsave(des_sex_gt, filename = paste0(work_dir,'/output/descriptives/summary_by_gender.png'))
#problems saving by gender table somehow, so switched to rtf format
gtsave(des_sex_gt, filename = paste0(work_dir,'/output/descriptives/summary_by_gender.rtf'))
