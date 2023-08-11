##This script defines cases used in the self-controlled case series for the MTK project

##Data cleaning is performed by Chris Wai Hang Lo on personal laptop, using R version 4.3.1
##Estimated running time for current script: 12 hours 45 minutes

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
  (as.Date(MTK_rx$Dispensing.Date..yyyy.mm.dd..) >= '2010-01-01' & as.Date(MTK_rx$Dispensing.Date..yyyy.mm.dd..) <= '2019-12-31') ,]
#add length of patients into summary
summary <- rbind(summary, 
                 data.frame(step = '# of patients with only montelukast prescriptions outside study period', 
                            number = summary[1,2] - sum(summary[c(2:nrow(summary)), 2]) - (length(unique(MTK_rx$Reference.Key.))) #2353
                 ))



#----- 3. filter off prescription records that 'Keep Record only', as no medications are dispensed -----
MTK_rx <- MTK_rx[!grepl('Keep Record only',MTK_rx$Action.Status.),]

#add length of patients into summary
summary <- rbind(summary, 
                 data.frame(step = '# of patients with montelukast prescription on keep record only without actual dispensing', 
                            number = summary[1,2] - sum(summary[c(2:nrow(summary)), 2]) - (length(unique(MTK_rx$Reference.Key.))) #91
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
                            number = summary[1,2] - sum(summary[c(2:nrow(summary)), 2]) - (length(unique(MTK_rx_cleaned$ID))) #18701
                 ))

#cross-checking for patient number, print warning message if the number of patients in MTK_pt is not equal to cleaned dataframe
if (length(MTK_pt) != nrow(MTK_rx_cleaned)) { print('Warning: # in patient list does not match dataframe sum')}

#remove variables to save memory
rm(dx_asthma_MTK)
rm(dx_MTK)

#----- 7. Extract demographics data for each patient, then ascertain observation period start date and end date -----

#read in demographics data
demo_all <- read.csv('raw/asthma_demo.csv', header = TRUE)

#join with vector containing eligible patients so far
demo_MTK <- inner_join(
            data.frame(ID = MTK_pt,
                       study_start_date = '2010-01-01')
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
                            number = sum(is.na(MTK_rx_cleaned$sex) == TRUE) + sum(is.na(MTK_rx_cleaned$date_birth) == TRUE)) #0
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
  study_end <- '2019-12-31'
  #get whichever is earlier
  obs_end <- min(as.Date(dod), as.Date(study_end), na.rm = TRUE)
  #paste results
  paste(obs_end)
})

#calculate age at start of observation period
MTK_rx_cleaned$age_start_day <- apply(MTK_rx_cleaned, 1, function(x) {
  #age by days
  age_day <- as.numeric(difftime(as.Date(x['obs_start']), as.Date(x['date_birth']), units = "days"))
  #paste results
  paste(age_day)
})

#convert age to years
MTK_rx_cleaned$age_start_year <- apply(MTK_rx_cleaned, 1, function(x) {
  #age by years, round down to nearest integer by floor()
  age_start_year <- floor(as.numeric(x['age_start_day']) / 365.25)
  #paste results
  paste(age_start_year)
})

#calculate age at day of 1st montelukast prescription
MTK_rx_cleaned$age_rx_day <- apply(MTK_rx_cleaned, 1, function(x) {
  #age by days
  age_rx_day <- as.numeric(difftime(as.Date(unlist(strsplit(x['MTK_start_date'], ';'))[1]), as.Date(x['date_birth']), units = "days"))
  #paste results
  paste(age_rx_day)
})

#convert age to years
MTK_rx_cleaned$age_rx_year <- apply(MTK_rx_cleaned, 1, function(x) {
  #age by years, round down to nearest integer by floor()
  age_rx_year <- floor(as.numeric(x['age_rx_day']) / 365.25)
  #paste results
  paste(age_rx_year)
})

#cross-checking for patient number, print warning message if the number of patients in MTK_pt is not equal to cleaned dataframe
if (length(MTK_pt) != nrow(MTK_rx_cleaned)) { print('Warning: # in patient list does not match dataframe sum')}

#remove demographics for all patients to save memory
rm(demo_all)

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
icd_htn <- "^401|^402|^403|^404|^405"
#osteroporosis (OP)
icd_op <- "^715|^733"

#get diagnosis records for patients who are montelukast users only (this will cover the dx_MTK object from previous section, as the list of eligible MTK users have been updated)
dx_MTK <- dx_all[dx_all$Reference.Key. %in% MTK_pt,]

#looping over each comorbidity, extract diagnoses by disease, and insert status if available
for (i in 1:length(comorbid)) {
  
  #get the vector of ICD-9-CM codes corresponding to the particular comorbidity
  icd_dx = get(paste0('icd_', comorbid[i]))
  #clear from previous loop for comorbidity
  dx_pt_list <- NULL
  dx_pt_rx_list <- NULL
  
  #assign dates for each comorbidity in the for loop, for each patient
  for (pt in 1:nrow(MTK_rx_cleaned)) {
    #get patient ID
    id = MTK_rx_cleaned[pt,'ID']
    #get diagnosis data
    dx_pt = dx_MTK[grepl(icd_dx, dx_MTK$All.Diagnosis.Code..ICD9..) & dx_MTK$Reference.Key. == id ,]
    #arrange by date of diagnosis
    dx_pt %>% arrange(Reference.Date.)
    #select diagnosis on or before the start date of observation period
    dx_pt_pre = dx_pt[as.Date(dx_pt$Reference.Date.) <= as.Date(MTK_rx_cleaned[pt,'obs_start']), ]
    #ascertain status for comorbidity, with TRUE or FALSE
    #if contain at least one diagnosis on or before the start date of observation period, code as TRUE (with that comorbidity present)
    dx_pt_list[pt] <- ifelse(nrow(dx_pt_pre) != 0, TRUE, FALSE)
    
    #select diagnosis on or before the date of montelukast initiation
    dx_pt_rx = dx_pt[as.Date(dx_pt$Reference.Date.) <= as.Date(  unlist(strsplit(MTK_rx_cleaned[pt, 'MTK_start_date'], ';'))[1]), ]
    #if contain at least one diagnosis on or before montelukast initiation, code as TRUE (with that comorbidity present)
    dx_pt_rx_list[pt] <- ifelse(nrow(dx_pt_rx) != 0, TRUE, FALSE)
  }
  #pasting the vector to each patient
  MTK_rx_cleaned[, paste0(comorbid[i], '_start')] <- dx_pt_list
  MTK_rx_cleaned[, paste0(comorbid[i], '_1rx')] <- dx_pt_rx_list
  }

#add length of patients into summary
summary <- rbind(summary, 
                 data.frame(step = '# of patients with baseline COPD on or before start of observation period', 
                            number =  nrow(MTK_rx_cleaned[MTK_rx_cleaned$copd_start == TRUE,])  #66
                 ))

#only keep patients without basline COPD
MTK_rx_cleaned <- MTK_rx_cleaned[MTK_rx_cleaned$copd_start == FALSE,]

#update MTK patient list
MTK_pt <- unique(MTK_rx_cleaned$ID)

#cross-checking for patient number, print warning message if the number of patients in MTK_pt is not equal to cleaned dataframe
if (length(MTK_pt) != nrow(MTK_rx_cleaned)) { print('Warning: # in patient list does not match dataframe sum')}


#random checking for comorbidity assignment
#checking for HTN
#id = 7584, i = 11, pt = 106 
#checking for copd
#id = 900395, i = 2, pt = 1313
#should be true


#----- 9. Extract neuropsychiatric outcomes, then filter off patients with any of the outcomes on or before observation starts -----
# composite neuropsychiatric outcomes (primary outcome)
ne <- c('ne','anx', 'dep', 'slp', 'sa', 'dem', 'adhd', 'asd', 'other')
# secondary outcomes
# anxiety disorder (anx) https://pubmed.ncbi.nlm.nih.gov/36206879/
icd_anx <- '^300|^293.84'
# depression (dep) https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6323719/
# ICD-9-CM code 300.4 is counted twice here (mixed anxiety and depressive disorders)
icd_dep <- "^296.2|^296.3|^296.5|^300.4|^309|^311"
# sleeping disorder (slp) https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7299484/
icd_slp <- '^291.82|^291.85|^307.4|^327.11|^327.12|^327.2|^327.3|^327.42|^327.43|^327.52|^327.53|^327.59|^327.8|^347|^780.5'
# suicidal attempt and self-inflicted injury (SA) https://pubmed.ncbi.nlm.nih.gov/36206879/
# from E590-E959, all charcters start with 'E95' can be included
icd_sa <- '^E95' 
# dementia (dem) https://pubmed.ncbi.nlm.nih.gov/16224307/
icd_dem <- '^290'
# attention-deficit hyperactivity disorder (ADHD)
icd_adhd <- '^314'
# autistic spectrum disorder (asd)
icd_asd <- "^299"

# other diagnosis (included in primary outcome but not stratifying in secondary outcomes)
# https://jamanetwork.com/journals/jamainternalmedicine/fullarticle/2517922
# diagnosis codes in supplementary materials, and filtered for codes listed above
icd_other <- '292.11|292.12|292.2|292.81|292.84|292.85|292.89|292.9|293.0|293.1|293.81|293.82|293.83|293.89|293.9|294.0|295.13|295.14|295.33|295.34|295.43|295.44|295.63|295.64|295.73|295.74|295.83|295.84|295.93|295.94|296.01|296.02|296.03|296.04|296.40|296.41|296.42|296.43|296.44|296.60|296.61|296.62|296.63|296.64|296.90|296.99|297.8|297.9|298.0|298.1|298.2|298.3|298.8|298.9|308.0|308.1|308.2|327.00|327.01|327.02|327.09|327.10|327.14|327.19|780.01|780.02|780.09|780.1|780.93|780.97|781.1|E930.3|E930.8|E930.9|E941.9|E947.8|E947.9|V40.0|V40.1|V40.2|V40.31|V40.39|V40.9'

#combine all the above diagnoses into composite NE
icd_ne <- paste(icd_anx, icd_dep, icd_slp, icd_sa, icd_dem, icd_adhd, icd_asd, icd_other, sep = "|")


#looping over each neuropsychiatric outcome, extract diagnoses by disease, and insert status if available
for (i in 1:length(ne)) {
  
  #get the vector of ICD-9-CM codes corresponding to the particular comorbidity
  icd_dx = get(paste0('icd_', ne[i]))
  #clear from previous loop for comorbidity
  #this vector contains TRUE/FALSE status on whether the patient has NE diagnosis on or before the start date of observation
  dx_pt_pre <- NULL
  #this vector contains TRUE/FALSE status on whether the patient has NE diagnosis after the start date of observation, but before the end of observation period
  dx_pt_post <- NULL
  #this vector contains the date of diagnosis for NE after the observation period
  dx_date_post <- NULL
  
  #assign dates for each comorbidity in the for loop, for each patient
  for (pt in 1:nrow(MTK_rx_cleaned)) {
    #get patient ID
    id = MTK_rx_cleaned[pt,'ID']
    #get diagnosis data
    dx_pt = dx_MTK[grepl(icd_dx, dx_MTK$All.Diagnosis.Code..ICD9..) & dx_MTK$Reference.Key. == id ,]
    #arrange by date of diagnosis
    dx_pt %>% arrange(Reference.Date.)
    
    ##### ----- Baseline NEs ------ #####
    #get the list of dates for splitting into pre-/post- start date of observation
    ne_date = dx_pt$Reference.Date.
    #if any of the dates present on or before observation start date, code as TRUE (as baseline NE)
    if (any(as.Date(ne_date) <= as.Date(MTK_rx_cleaned[pt,'obs_start']))) {
      dx_pt_pre[pt] <- TRUE
    } else {
        dx_pt_pre[pt] <- FALSE
    }
    ##### ------------------------ #####
    
    ##### ----- Post-baseline NEs (actual outcomes) ------ #####
    #get the diagnosis presented after the start date of observation period, but on or before the end of observation period
    dx_post <- dx_pt[as.Date(dx_pt$Reference.Date.) > as.Date(MTK_rx_cleaned[pt,'obs_start']) & as.Date(dx_pt$Reference.Date.) > as.Date(MTK_rx_cleaned[pt,'obs_start']), ]
    
    #code as TRUE if the number of diagnosis records is not 0
    if (nrow(dx_post != 0)) {
      dx_pt_post[pt] <- TRUE
    } else {
      dx_pt_post[pt] <- FALSE
    }
    
    #code the dates of diagnosis together
    dx_date_post[pt] <- paste0(dx_post$Reference.Date., collapse = ';')
    
    ##### ----------------------------------------------- #####
    
  }
  
  #pasting the vector to each patient
  MTK_rx_cleaned[, paste0(ne[i], '_pre')] <- dx_pt_pre
  MTK_rx_cleaned[, paste0(ne[i], '_post')] <- dx_pt_post
  MTK_rx_cleaned[, paste0(ne[i], '_date')] <- dx_date_post
  
}

#add length of patients into summary
summary <- rbind(summary, 
                 data.frame(step = '# of patients with baseline NE on or before start of observation period', 
                            number =  nrow(MTK_rx_cleaned[MTK_rx_cleaned$ne_pre == TRUE,])
                 ))

#only keep patients without basline NE
MTK_rx_cleaned <- MTK_rx_cleaned[MTK_rx_cleaned$ne_pre == FALSE,]

#update MTK patient list
MTK_pt <- unique(MTK_rx_cleaned$ID)

#cross-checking for patient number, print warning message if the number of patients in MTK_pt is not equal to cleaned dataframe
if (length(MTK_pt) != nrow(MTK_rx_cleaned)) { print('Warning: # in patient list does not match dataframe sum')}

#remove variables to save memory
rm(dx_all)
rm(dx_MTK)

#cross check
# i = 2 (as anxiety), id = 2236138, 770270, 6146591, pt = 1875, 1074, 347
# i = 7 (as ASD), id = 202371, 770270, 8906903, pt = 1554, 1074, 990 
# pt = 1875, it should be baseline anxiety
# pt = 347, it should be anxiety as outcome

#----- 10. Extract baseline medications (asthma controllers) and report baseline status -----
#read in prescription data
rx_all <- read.csv('raw/asthma_rx.csv', header = TRUE)

#dataframe containing IDs
MTK_id <- as.data.frame(MTK_rx_cleaned[,colnames(MTK_rx_cleaned) == 'ID'])
colnames(MTK_id) <- c('ID')

#select eligible participants only
rx_elig <- inner_join(MTK_id, rx_all, join_by(ID == Reference.Key))

#remove variable to save memory
rm(rx_all)

#list of baseline medications
med <- c('SABA', 'MX', 'anti_IL', 'anti_IgE', 'ICS_LABA', 'SAMA')
#short-acting beta agonists (SABA)
SABA <- paste(c("SALBUTAMOL", "VENTOLIN", "SYNTALIN", "ASTHALIN","ALBUTEROL", "PROVENTIL","TERBUTALINE", "BRICANYL"), collapse = "|")
#methylxanthines (MX)
MX <- paste(c("THEOPHYLLINE", "NUELIN"), collapse = "|")
#anti-interleukins (anti-IL)
anti_IL <- paste(c("BENRALIZUMAB", "FASENRA","MEPOLIZUMAB", "NUCALA","DUPILUMAB", "DUPIXENT"), collapse = "|")
#anti-IgE (anti-IgE)
anti_IgE <- paste(c("OMALIZUMAB", "XOLAIR"), collapse = '|')
#Inhaled corticosteroids +/- Long-acting beta agonists (ICS_LABA)
ICS_LABA <- paste(c("BECLOMETHASONE", "BECLATE", "BECLAZONE", "BECLOFORTE", "BUDESONIDE", "PULMICORT", "CICLESONIDE", "ALVESCO", "FLUTICASONE", "FLIXOTIDE", "SALMETEROL", "SEREVENT", "FORMOTEROL", "VILANTEROL", "FOSTER", "SYMBICORT", "VANNAIR", "FLUTIFORM", "RELVAR ELLIPTA", "SERETIDE", "SIRDUPLA"), collapse = "|")
#short-acting antimuscarinics (SAMA)
SAMA <- paste(c("IPRATROPIUM", "ATROVENT", "TIOTROPIUM"), collapse = "|")

##looping over each medication outcome, extract prescription data, and insert status if available
for (i in 1:length(med)) {
  
  #get the vector of ICD-9-CM codes corresponding to the particular comorbidity
  med_rx = get(med[i])
  #clear from previous loop for comorbidity
  #this vector contains TRUE/FALSE status on whether patients has exposure to that drug on the start date of observation (by patient)
  #the length of this vector should be equal to the number of patients
  med_rx_vec <- NULL
  
  #assign dates for each comorbidity in the for loop, for each patient
  for (pt in 1:nrow(MTK_rx_cleaned)) {
    #get patient ID
    id = MTK_rx_cleaned[pt,'ID']
    #get prescription data for the drug class for the particular patient
    rx_pt = rx_elig[grepl(med_rx, rx_elig$Drug.Name) & rx_elig$ID == id ,]
    #filter off keep record only prescriptions
    rx_pt = rx_pt[!grepl('Keep Record only',rx_pt$Action.Status),]
    #get a list of drug codes within the same drug class, to ascertain exposure for each drug within the same drug class
    drug_code = unique(rx_pt$Drug.Item.Code)
    #get a vector on TRUE/FALSE status on whether the patient has exposure to that drug on the start date of observation (by drug codes)
    drug_rx_vec <- NULL
    
    #only run if length of drug code is not 0, otherwise code as no exposure for the drug
    if (length(drug_code) != 0) {
    #get the duration of exposure for each drug code within the same drug class, and classify as 'TRUE' if any of the exposure duration is in overlap with start of observation
    for(drug in 1:length(drug_code)) {
      #get records for the particular drug
      rx_drug <- rx_pt[rx_pt$Drug.Item.Code == drug_code[drug],]
      #arrange rx_drug by date of dispensing
      rx_drug = rx_drug %>% arrange(rx_drug$Dispensing.Date..yyyy.mm.dd.)
      #get vector of starting dates for each dispensing
      start_date = rx_drug[,names(rx_drug) == 'Dispensing.Date..yyyy.mm.dd.']
      end_date = NULL
      #get dispensing duration (as days)
      duration = rx_drug[,names(rx_drug) == 'Dispensing.Duration']
      
      #convert date to days
      for (date in 1:length(duration)) {
        #if dispensing duration is weeks, multiply the value by 7, then -1 because the first date of taking the drug is date of dispensing
        #note: dispensing duration can only be days or weeks, years is not available in CDARS as dispensing unit
        if (rx_drug[date,names(rx_drug) == 'Dispensing.Duration.Unit'] == 'Week(s)')
        {duration[date] = duration[date]*7 - 1}
    
        #set up a check for cycle(s) dispensing unit, which should have been filtered already
        #from manual checks on eligible prescriptions, only 1 row of prescription contains 'Cycle(s)' as unit of dispensing duration. The drug is tranexamic acid, which is irrelevant to our analysis.
        if (rx_drug[date,names(rx_drug) == 'Dispensing.Duration.Unit'] == 'Cycles(s)')
        {print(paste0('Patient with ID', id ,' has prescription records with dispensing duration expressed as Cycle(s).'))}
        #if this error message is printed, we will look into the issue again
        
        #get drug end dates, by adding date of dispensing by duration of dispensing
        end_date = c(end_date, as.character(as.Date(start_date[date]) + duration[date]))
      }
      
      #Note: Now we have 'start dates' and 'end dates' for each dispensing record
      #NULL vector containing durations for each dispensing episode identified
      rx_epi <- NULL
      #get start date of first dispensing record and assign as the initial starting date of the dispensing episode
      start_rx <- start_date[1]
      #get end date of first dispensing (replaceable, later becomes end date of the dispensing episode)
      end_rx <- end_date[1]
      
      #merge them into a continuous episode, if more than one prescription
      if (length(start_date) > 1) {
      for (date in 2:length(start_date)) {
        #compare end date and start date of next dispensing, replace value as 'end_rx' (end date of dispensing episode) if start date of next dispensing is <= 'end_rx'
        if(as.Date(start_date[date]) <= as.Date(end_rx)) {
          end_rx = max(as.Date(end_rx), as.Date(end_date[date]))
        } else {
        #if not, start a new episode
          #paste start and end date of current dispensing episode to 'rx_epi' vector
          rx_epi <- c(rx_epi, paste(start_rx, end_rx, sep = ';'))
          #re-set starting date and end date for new prescription episode
          start_rx = start_date[date]
          end_rx = end_date[date]}
      }
      
      #now all dispensing episodes have been merged nicely, except for the last dispensing episode which hasn't been updated.
      #an additional line to modify the final dispensing episode to correct starting and end date
      rx_epi[length(rx_epi)+1] = paste(start_rx, end_rx, sep = ';')
      } else if (length(start_date) == 1) {
        #if only one dispensing record, paste start and end date as the episode
        rx_epi[length(rx_epi)+1] = paste(start_rx, end_rx, sep = ';')
      } else if (length(start_date) == 0) {
        #if no dispensing record, code baseline med for that drug code to be FALSE
        bl_med_stat = FALSE}
      
      #get the start date of observation period for the patient
      obs_start = MTK_rx_cleaned[pt,'obs_start']
      
      #split the dispensing episodes and covert to date objects
      rx_duration = strsplit(rx_epi, split = ';')
      rx_start_dates = as.Date(sapply(rx_duration, `[`, 1))
      rx_end_dates = as.Date(sapply(rx_duration, `[`, 2))
      
      #check if the start of observation date falls within range
      bl_med_stat <- rx_start_dates <= as.Date(obs_start) & as.Date(obs_start) <= rx_end_dates
      
      #if start of observation date falls into any of the range, then code baseline status as true
      if (any(bl_med_stat)) {drug_rx_vec[drug] = TRUE
      } else {
        drug_rx_vec[drug] = FALSE}
    } # this is the close bracket for loop on each drug code
    } else {
      #if there are no drug codes for the class, code this directly as FALSE
      drug_rx_vec = FALSE
    }
      #Note; the 'drug_rx_vec' contains a logical vector of TRUE/FALSE on whether the patient is exposed to the drugs within the same drug class, stratified by drug codes
      #if any of the drug codes within the class is true, then the patient is exposed to that drug class and code status for the patient as TRUE
      if (any(drug_rx_vec)) {med_rx_vec[pt] = TRUE
      } else {
        med_rx_vec[pt] = FALSE}
  } # this is the close bracket for loop on patient
  
  #paste the vector to MTK_rx_cleaned dataframe
  MTK_rx_cleaned[, paste0(med[i], '_start')] <- med_rx_vec
  
  #cross checking message
  if(length(med_rx_vec) != nrow(MTK_rx_cleaned)) {print('Error message: the length of TRUE/FALSE status vector () is not equal to the number of patients in the dataframe')}
  
} # this is the close bracket for loop on each drug class

#----- 11. Extract medications at 1st MTK rx (asthma controllers), and report status -----
##looping over each medication outcome, extract prescription data, and insert status if available
for (i in 1:length(med)) {
  
  #get the vector of ICD-9-CM codes corresponding to the particular comorbidity
  med_rx = get(med[i])
  #clear from previous loop for comorbidity
  #this vector contains TRUE/FALSE status on whether patients has exposure to that drug on the start date of observation (by patient)
  #the length of this vector should be equal to the number of patients
  med_rx_vec <- NULL
  
  #assign dates for each comorbidity in the for loop, for each patient
  for (pt in 1:nrow(MTK_rx_cleaned)) {
    #get patient ID
    id = MTK_rx_cleaned[pt,'ID']
    #get prescription data for the drug class for the particular patient
    rx_pt = rx_elig[grepl(med_rx, rx_elig$Drug.Name) & rx_elig$ID == id ,]
    #filter off keep record only prescriptions
    rx_pt = rx_pt[!grepl('Keep Record only',rx_pt$Action.Status),]
    #get a list of drug codes within the same drug class, to ascertain exposure for each drug within the same drug class
    drug_code = unique(rx_pt$Drug.Item.Code)
    #get a vector on TRUE/FALSE status on whether the patient has exposure to that drug on the start date of observation (by drug codes)
    drug_rx_vec <- NULL
    
    #only run if length of drug code is not 0, otherwise code as no exposure for the drug
    if (length(drug_code) != 0) {
      #get the duration of exposure for each drug code within the same drug class, and classify as 'TRUE' if any of the exposure duration is in overlap with start of observation
      for(drug in 1:length(drug_code)) {
        #get records for the particular drug
        rx_drug <- rx_pt[rx_pt$Drug.Item.Code == drug_code[drug],]
        #arrange rx_drug by date of dispensing
        rx_drug = rx_drug %>% arrange(rx_drug$Dispensing.Date..yyyy.mm.dd.)
        #get vector of starting dates for each dispensing
        start_date = rx_drug[,names(rx_drug) == 'Dispensing.Date..yyyy.mm.dd.']
        end_date = NULL
        #get dispensing duration (as days)
        duration = rx_drug[,names(rx_drug) == 'Dispensing.Duration']
        
        #convert date to days
        for (date in 1:length(duration)) {
          #if dispensing duration is weeks, multiply the value by 7, then -1 because the first date of taking the drug is date of dispensing
          #note: dispensing duration can only be days or weeks, years is not available in CDARS as dispensing unit
          if (rx_drug[date,names(rx_drug) == 'Dispensing.Duration.Unit'] == 'Week(s)')
          {duration[date] = duration[date]*7 - 1}
          
          #set up a check for cycle(s) dispensing unit, which should have been filtered already
          #from manual checks on eligible prescriptions, only 1 row of prescription contains 'Cycle(s)' as unit of dispensing duration. The drug is tranexamic acid, which is irrelevant to our analysis.
          if (rx_drug[date,names(rx_drug) == 'Dispensing.Duration.Unit'] == 'Cycles(s)')
          {print(paste0('Patient with ID', id ,' has prescription records with dispensing duration expressed as Cycle(s).'))}
          #if this error message is printed, we will look into the issue again
          
          #get drug end dates, by adding date of dispensing by duration of dispensing
          end_date = c(end_date, as.character(as.Date(start_date[date]) + duration[date]))
        }
        
        #Note: Now we have 'start dates' and 'end dates' for each dispensing record
        #NULL vector containing durations for each dispensing episode identified
        rx_epi <- NULL
        #get start date of first dispensing record and assign as the initial starting date of the dispensing episode
        start_rx <- start_date[1]
        #get end date of first dispensing (replaceable, later becomes end date of the dispensing episode)
        end_rx <- end_date[1]
        
        #merge them into a continuous episode, if more than one prescription
        if (length(start_date) > 1) {
          for (date in 2:length(start_date)) {
            #compare end date and start date of next dispensing, replace value as 'end_rx' (end date of dispensing episode) if start date of next dispensing is <= 'end_rx'
            if(as.Date(start_date[date]) <= as.Date(end_rx)) {
              end_rx = max(as.Date(end_rx), as.Date(end_date[date]))
            } else {
              #if not, start a new episode
              #paste start and end date of current dispensing episode to 'rx_epi' vector
              rx_epi <- c(rx_epi, paste(start_rx, end_rx, sep = ';'))
              #re-set starting date and end date for new prescription episode
              start_rx = start_date[date]
              end_rx = end_date[date]}
          }
          
          #now all dispensing episodes have been merged nicely, except for the last dispensing episode which hasn't been updated.
          #an additional line to modify the final dispensing episode to correct starting and end date
          rx_epi[length(rx_epi)+1] = paste(start_rx, end_rx, sep = ';')
        } else if (length(start_date) == 1) {
          #if only one dispensing record, paste start and end date as the episode
          rx_epi[length(rx_epi)+1] = paste(start_rx, end_rx, sep = ';')
        } else if (length(start_date) == 0) {
          #if no dispensing record, code baseline med for that drug code to be FALSE
          bl_med_stat = FALSE}
        
        #get the start date of first montelukast prescription for the patient
        MTK_1st <- unlist(strsplit(MTK_rx_cleaned[pt, names(MTK_rx_cleaned) == 'MTK_start_date'], ';'))[1]
        
        #split the dispensing episodes and covert to date objects
        rx_duration = strsplit(rx_epi, split = ';')
        rx_start_dates = as.Date(sapply(rx_duration, `[`, 1))
        rx_end_dates = as.Date(sapply(rx_duration, `[`, 2))
        
        #check if the date of 1st montelukast prescription falls within range
        bl_med_stat <- rx_start_dates <= as.Date(MTK_1st) & as.Date(MTK_1st) <= rx_end_dates
        
        #if the date of 1st montelukast prescription falls into any of the range, then code baseline status as true
        if (any(bl_med_stat)) {drug_rx_vec[drug] = TRUE
        } else {
          drug_rx_vec[drug] = FALSE}
      } # this is the close bracket for loop on each drug code
    } else {
      #if there are no drug codes for the class, code this directly as FALSE
      drug_rx_vec = FALSE
    }
    #Note; the 'drug_rx_vec' contains a logical vector of TRUE/FALSE on whether the patient is exposed to the drugs within the same drug class, stratified by drug codes
    #if any of the drug codes within the class is true, then the patient is exposed to that drug class and code status for the patient as TRUE
    if (any(drug_rx_vec)) {med_rx_vec[pt] = TRUE
    } else {
      med_rx_vec[pt] = FALSE}
  } # this is the close bracket for loop on patient
  
  #paste the vector to MTK_rx_cleaned dataframe
  MTK_rx_cleaned[, paste0(med[i], '_1rx')] <- med_rx_vec
  
  #cross checking message
  if(length(med_rx_vec) != nrow(MTK_rx_cleaned)) {print('Error message: the length of TRUE/FALSE status vector () is not equal to the number of patients in the dataframe')}
  
} # this is the close bracket for loop on each drug class

#remove variable to save memory
rm(rx_elig)

#----- 12. Summarize and save output -----
#calculate final number of cohort
summary <- rbind(summary, 
                 data.frame(step = '# of patients in final SCCS cohort', 
                            number = summary[1,2] - sum(summary[2:nrow(summary), 2]) #2055
                 ))

#MTK_rx_cleaned: dataframe containing patients' prescription records, baseline information and medication at baseline
write.csv(MTK_rx_cleaned,file='cleaned/MTK_SCCS.csv', row.names = FALSE)
#MTK_pt: containing patient list
write.csv(MTK_pt,file='cleaned/MTK_pt_SCCS.csv', row.names = FALSE)
#summary: flowchart summary
write.csv(summary,file='cleaned/summary_flow_SCCS.csv', row.names = FALSE)
