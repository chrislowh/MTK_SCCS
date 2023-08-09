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

#set working directory
setwd(data_dir)

#read in all prescription for montelukast (from 1999/12/01 to 2018/12/31)
MTK_rx <- read.csv('raw/MTK_rx.csv', header = TRUE)

#add length of patients into summary
summary <- rbind(summary, 
                 data.frame(step = '# of patients with montelukast prescription from 1999/12/01 to 2018/12/31 from original dataset', 
                            number = print(length(unique(MTK_rx$Reference.Key.))) #23611
                            ))

#create empty dataframe for storing cleaned data
MTK_rx_cleaned <- NULL

####################################################################################
# Data cleaning steps and identifying cases for SCCS
####################################################################################

#----- 1. remove prescription records with NA information on reference key, dispensing date or duration -----
MTK_rx <- MTK_rx[complete.cases(MTK_rx[,c('Reference.Key.', "Dispensing.Date..yyyy.mm.dd..", "Dispensing.Duration.", "Dispensing.Duration.Unit.")]),]

#add length of patients into summary
summary <- rbind(summary, 
                 data.frame(step = '# of patients with with NA on dispensing dates', 
                            number = summary[1,2] - (length(unique(MTK_rx$Reference.Key.))) #211
                 ))



#----- 2. filter off prescription records out of study period (from 2011/01/01 to 2018/12/31) -----
MTK_rx <- MTK_rx[
  (as.Date(MTK_rx$Dispensing.Date..yyyy.mm.dd..) >= '2011-01-01' & as.Date(MTK_rx$Dispensing.Date..yyyy.mm.dd..) <= '2018-12-31') ,]
#add length of patients into summary
summary <- rbind(summary, 
                 data.frame(step = '# of patients with only montelukast prescriptions outside study period', 
                            number = summary[1,2] - sum(summary[c(2:nrow(summary)), 2]) - (length(unique(MTK_rx$Reference.Key.))) #2959
                 ))



#----- 3. filter off prescription records that 'Keep Record only', as no medications are dispensed -----
MTK_rx <- MTK_rx[!grepl('Keep Record only',MTK_rx$Action.Status.),]

#add length of patients into summary
summary <- rbind(summary, 
                 data.frame(step = '# of patients with montelukast prescription on keep record only without actual dispensing', 
                            number = summary[1,2] - sum(summary[c(2:nrow(summary)), 2]) - (length(unique(MTK_rx$Reference.Key.))) #87
                 ))



#----- 4. Get a vector of patient list to begin with -----
MTK_pt <- unique(MTK_rx$Reference.Key.)



#----- 5. For every patient, merge prescription records -----
for (i in 1:length(MTK_pt)) {
  
  #create a vector storing all MTK prescription records arranged by dispensing date
    #get prescription records for the patient
    rx = MTK_rx[MTK_rx$Reference.Key. == MTK_pt[i],]
    #arrange rx by date of dispensing
    rx = rx %>% arrange(rx$Dispensing.Date..yyyy.mm.dd..)
    #get vector of dates and paste to patient list
    start_date = rx[,names(rx) == 'Dispensing.Date..yyyy.mm.dd..']
    end_date = NULL
    #get dispensing duration (as days)
    duration = rx[,names(rx) == 'Dispensing.Duration.']
    #convert date to days
    for (date in 1:length(duration)) {
      #if dispensing duration is weeks, multiply the value by 7, then -1 because the first date of taking the drug is date of dispensing
      #note: dispensing duration can only be days or weeks, years is not available in CDARS as dispensing unit
      if (rx[date,names(rx) == 'Dispensing.Duration.Unit.'] == 'Week(s)')
      {duration[date] = duration[date]*7 - 1}
      #get drug end dates, by adding date of dispensing by duration of dispensing
      end_date = c(end_date, as.character(as.Date(start_date[date]) + duration[date]))
    }
    #paste the information into cleaned dataframe
    MTK_rx_cleaned <- rbind(MTK_rx_cleaned,
                            data.frame(ID = MTK_pt[i],
                                       MTK_start_date = paste0(start_date, collapse = ';'),
                                       MTK_end_date = paste0(end_date, collapse = ';'),
                                       MTK_duration = paste0(duration, collapse = ';')
                            )
                            )

}

#remove MTK_rx to save memory
rm(MTK_rx)




#----- 6. Extract diagnoses data for each patient, and get patients with asthma diagnosis on or before 1st prescription of MTK -----
#read in diagnosis for asthma patients (from 1993/01/06 to 2019/12/31)
dx_all <- read.csv('raw/asthma_dx.csv', header = TRUE)

#get diagnosis records for patients who are montelukast users only
dx_MTK <- dx_all[dx_all$Reference.Key. %in% MTK_pt,]

#insert diagnosis date for MTK users, if available
#get diagnosis for MTK users
dx_asthma_MTK <- dx_MTK[grepl('^493', dx_MTK$All.Diagnosis.Code..ICD9..), ]

#denote null vectors
dx_date_vec <- dx_type_vec <- asthma_stat_vec <- NULL

#For every patient, merge asthma diagnosis records
for (i in 1:length(MTK_pt)) {
  #clean up values from previous loop
  dx_date <- dx_type <- MTK_1st <- NA
  #get asthma diagnosis for particular patient
  dx_asthma_MTK_pt <- dx_asthma_MTK[dx_asthma_MTK$Reference.Key. == MTK_pt[i],]
  #arrange by diagnosis date
  dx_asthma_MTK_pt %>% arrange(Reference.Date.)
  #if asthma diagnosis is present, continue cleaning
  if (nrow(dx_asthma_MTK_pt) != 0) {
    #get date of asthma diagnoses as vector
    dx_date <- dx_asthma_MTK_pt[, names(dx_asthma_MTK_pt) == 'Reference.Date.']
    #get type of diagnosis
    dx_type <- dx_asthma_MTK_pt[, names(dx_asthma_MTK_pt) == 'Patient.Type..IP.OP.A.E..']
    #get date of first montelukast prescription
    MTK_1st <- unlist(strsplit(MTK_rx_cleaned[MTK_rx_cleaned$ID == MTK_pt[i], names(MTK_rx_cleaned) == 'MTK_start_date'], ';'))[1]
    #code as TRUE if asthma diagnosis is present on or before the day of MTK prescription
    if (as.Date(dx_date[1]) <= as.Date(MTK_1st)) {asthma_stat = TRUE} 
    else if (as.Date(dx_date[1]) > as.Date(MTK_1st)) {asthma_stat = FALSE}
  } 
  #if asthma diagnosis is not present at all, code asthma status as false and remove from dataframe
  else { asthma_stat = FALSE }
  
  #summarize as vectors
  dx_date_vec <- c(dx_date_vec, paste0(dx_date, collapse = ';'))
  dx_type_vec <- c(dx_type_vec, paste0(dx_type, collapse = ';'))
  asthma_stat_vec <- c(asthma_stat_vec, asthma_stat)
}

#merge to prescription data
MTK_rx_cleaned <- cbind(MTK_rx_cleaned, 
                        asthma_dx_date = dx_date_vec,
                        asthma_dx_type = dx_type_vec,
                        asthma_status = asthma_stat_vec)

#only keep patients with eligible asthma status
MTK_rx_cleaned <- MTK_rx_cleaned[MTK_rx_cleaned$asthma_status == TRUE,]

#update MTK patient list
MTK_pt <- unique(MTK_rx_cleaned$ID)

#add length of patients into summary
summary <- rbind(summary, 
                 data.frame(step = '# of patients without asthma diagnosis before montelukast initation', 
                            number = summary[1,2] - sum(summary[c(2:nrow(summary)), 2]) - (length(unique(MTK_rx_cleaned$ID))) #18070
                 ))

#cross-checking for patient number, print warning message if the number of patients in MTK_pt is not equal to cleaned dataframe
if (length(MTK_pt) != nrow(MTK_rx_cleaned)) { print('Warning: # in patient list does not match dataframe sum')}



#############################################################################
##temporary function for backup
##remove after script completed
MTK_rx_backup <- MTK_rx_cleaned
######################################################################


#----- 7. Extract demographics data for each patient, then ascertain observation period start date and end date -----

#read in demographics data
demo_all <- read.csv('raw/asthma_demo.csv', header = TRUE)

#join with vector containing eligible patients so far
demo_MTK <- inner_join(
            data.frame(ID = MTK_pt,
                       study_start_date = '2011-01-01')
                       , demo_all, join_by(ID == Reference.Key.) )

#subset (ID, study start date, sex, date of birth and date of death) and rename data.frame
demo_MTK <- demo_MTK[, which(colnames(demo_MTK) %in% c('ID', 'study_start_date', 'Sex.', 'Date.of.Birth..yyyy.mm.dd..', 'Date.of.Registered.Death.'))]
colnames(demo_MTK) <- c('ID', 'study_start_date', 'sex', 'date_birth', 'date_death')

#join with prescription data
MTK_rx_cleaned <- inner_join(demo_MTK, MTK_rx_cleaned)

#filter off patients with missing date of birth information
MTK_rx_cleaned <- MTK_rx_cleaned[complete.cases(MTK_rx_cleaned[,c('sex','date_birth')]),]

#add length of patients into summary
summary <- rbind(summary, 
                 data.frame(step = '# of patients without age or sex information', 
                            number = sum(is.na(MTK_rx_cleaned$sex) == TRUE) + sum(is.na(MTK_rx_cleaned$date_birth) == TRUE)) #18070
                 )

#insert the start and end date of observation period
MTK_rx_cleaned$obs_start <- apply(MTK_rx_cleaned, 1 , function(x) {
  #date of birth
  dob <- x['date_birth']
  #start date of study period
  study_start <- x['study_start_date']
  #get whichever is later
  obs_start <- max(as.Date(dob), as.Date(study_start), na.rm = TRUE)
  #paste results
  paste(obs_start)
})

MTK_rx_cleaned$obs_end <- apply(MTK_rx_cleaned, 1 , function(x) {
  #date of birth
  dod <- x['date_death']
  #start date of study period
  study_end <- '2018-12-31'
  #get whichever is earlier
  obs_end <- min(as.Date(dod), as.Date(study_end), na.rm = TRUE)
  #paste results
  paste(obs_end)
})


#calculate age at start of observation period


#----- 8. Extract comorbidity diagnoses data for each patient, then ascertain baseline comorbidity status. Also filter patients with COPD here -----

#list of relevant diagnosis; with relevant references
#list of comorbidities
comorbid <- c('copd', 'ar', 'sinu', 'obesity', 'gerd', 'eczema', 'dm', 'ihd', 'arr', 'hf', 'htn', 'op')
#chronic obstructive pulmonary disease (COPD)
icd_copd <- '^491|^492|^496'
#allergic rhinitis (AR) https://bmcpediatr.biomedcentral.com/articles/10.1186/s12887-019-1594-4
icd_ar <- "^477"
#sinusitis (sinu) https://jamanetwork.com/journals/jamaotolaryngology/fullarticle/647261
icd_sinu <- "^473"
#obesity https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/1471-2288-11-173
icd_obesity <- "^278"
#gastroesophageal reflux disease; refer to American Academy of Pediatrics coding
icd_gerd <- "530.11|530.81"
#eczema https://aacijournal.biomedcentral.com/articles/10.1186/s13223-023-00785-4
icd_eczema <- "^691|^692"
#diabetes (DM) https://www.ncbi.nlm.nih.gov/books/NBK368403/table/sb203.t5/
icd_dm <- "^250"
#ischemic heart disease (IHD) https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3589967/
icd_ihd <- "^410|^411|^412|^413|^414"
#arrhythmia (arr) https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6613689/
icd_arr <- "^427"
#heart failure (HF) https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6613689/
icd_hf <- "^428"
#hypertension (HTN) https://pubmed.ncbi.nlm.nih.gov/19858407/
icd_hn <- "^401|^402|^403|^404|^405"
#osteroporosis (OP)
icd_op <- "^715|^733"

#get diagnosis records for patients who are montelukast users only (this will cover the dx_MTK object from previous section, as the list of eligible MTK users have been updated)
dx_MTK <- dx_all[dx_all$Reference.Key. %in% MTK_pt,]

#extract diagnoses by disease, and insert diagnosis date if available
for (i in 1:length(comorbid)) {
  #get the vector for diagnosis
  icd_dx <- get(paste0('icd_', comorbid[i]))
  
  #for every patient, extract and merge the diagnoses data
  for (pt in 1:length(MTK_pt)) {
    #get the diagnosis data for the patient
    dx_pt <- dx_MTK[dx_MTK$Reference.Key. == MTK_pt[pt] & grepl(icd_dx, dx_MTK$All.Diagnosis.Code..ICD9..), ]
    #arrange by diagnosis date
    dx_pt %>% arrange(Reference.Date.)
    
  }
  
}

dx_pt <- dx_MTK[grepl(icd_dx, dx_MTK$All.Diagnosis.Code..ICD9..), ]

id = 25365, i = , pt = 131

#----- 9. Extract neuropsychiatric outcomes, then filter off patients with -----

#----- 10. Extract baseline medications (asthma controllers, antidepressants, antipsychotics) and report baseline status -----


#----- 11. Save output -----