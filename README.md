# MTK_SCCS
This repository contains R codes for analysis used in the project named: 'Montelukast and the risk of neuropsychiatric events in patients with asthma: a self-controlled case series and nested case-control study'.

# Project History
The project is an extension of final year project of Chris Wai Hang Lo, who was a final-year pharmacy student at the Unviersity of Hong Kong. 
He is currently a PhD student at Social, Genetics and Developmental Psychiatry Centre at King's College London.

The project initially started in 2020 under the Undergrduate Research Fellwoship Programme by HKU, to conduct a cohort study investigating the relationship between montelukast and neuropsychiatric events.
After the completion of the study, the project was put on hold as Chris Wai Hang Lo and Swathi Pathadka have graduated from their repsective programmes, and thus left the Department of Pharmacology and Pharmacy, HKU.

The project was re-started in 2022, as a systematic review was carried out to investigate such relationships.
The manuscript has been accepted for publication by European Respiratory Review in July 2023.

Initial data collection was done by Swathi Pathadka in 2020, collating data from the CDARS database containing the following:

1. Dispensing records of montelukast from 1999-12-01 to 2018-12-31, regardless of indication
2. All diagnosis records and procedures for ?montelukast users
3. Demographics for patients with a dispensing record for montelukast, including sex, age of birth and age of death
4. All diagnosis records and procedures for patients with asthma (from?)
5. Dispensing records for all patients with asthma (from?)
6. Demographics for all patients with asthma (from?)

These were saved as RData and RDS files previously. As the project was

This repository contains the following scripts under the directory **/scripts/**:

**data_clean.R**: Data collected by Swathi Pathdaka were stored in RData and RDS files, and not processed. 
This short script reads in data and re-write relevant information as .csv files for more convenient manipulation.

**sccs_case_def.R**: 
