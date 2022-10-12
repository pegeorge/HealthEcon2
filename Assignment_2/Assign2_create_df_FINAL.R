


library(tidyverse)

rm(list = ls())

#set wd
setwd("C:/Users/pegeorg/OneDrive - Emory University/PhD/Health Econ 2/Assignments/Assignment_2/")

#MAC
setwd('~/OneDrive - Emory University/PhD/Health Econ 2/Assignments/Assignment_2/')


#Physician Fee Schedule 2010 Update database:   dataset with service-specific price shocks     --------------------------------- 
#         introduced by the 2010 fee schedule update.
PFS.df <- read.delim('data/input/PFS_update_data.txt')



##########                      2012                  #################


#MD-PPAS: The Medicare Data on Provider Practice and Specialty includes data on physician specialties,  -----------------------
#         practice IDs, demographics, and place of service


PPAS2012.df <- read.csv('data/input/MDPPAS/PhysicianData_2012/PhysicianData_2012.csv')
ls(PPAS2012.df)

PPAS2012.df = PPAS2012.df %>% 
  select(c(npi, sex, spec_broad, spec_prim_1, Year, pos_office, pos_asc, pos_opd))

#create the integration variable
PPAS2012.df = PPAS2012.df %>% 
  mutate(INT = pos_opd / (pos_opd + pos_asc + pos_office)) %>% 
  rename(NPI = npi)




#Medical utilization and payment data: These files provide data on the quantities and Medicare spending    --------------------------
#        of each physician and service. We'll use these data to capture total physician-level billing activity, 
#        and we'll use the service-level data to measure the revenue effects from our plausibly exogenous policy shock.


Medicare_Util_2012.df <- read.delim('data/input/Medicare_Util_Pay/2012/2012/Medicare_Provider_Util_Payment_PUF_CY2012.txt')

ls(Medicare_Util_2012.df)


#looking at only doctors
Medicare_Util_2012.df <- Medicare_Util_2012.df %>% 
  filter(NPPES_CREDENTIALS == 'M.D.' | NPPES_CREDENTIALS == 'MD')

#removing variables I don't need
Medicare_Util_2012.df = Medicare_Util_2012.df %>% 
  select(c(NPI, NPPES_CREDENTIALS, AVERAGE_MEDICARE_PAYMENT_AMT, BENE_UNIQUE_CNT, LINE_SRVC_CNT, HCPCS_CODE, NPPES_PROVIDER_ZIP))


# Combine the Medicare_Util and PPAS databases 

typeof(PPAS2012.df$NPI)

combined_2012 <- left_join(Medicare_Util_2012.df, PPAS2012.df, by = 'NPI')

combined_2012 = na.omit(combined_2012)

save(combined_2012, file = 'data/output/combined_2012.rda')

rm(list = ls())




##########                      2013                  #################


#MD-PPAS: The Medicare Data on Provider Practice and Specialty includes data on physician specialties,  -----------------------
#         practice IDs, demographics, and place of service


PPAS2013.df <- read.csv('data/input/MDPPAS/PhysicianData_2013/PhysicianData_2013.csv')
ls(PPAS2013.df)

PPAS2013.df = PPAS2013.df %>% 
  select(c(npi, sex, spec_broad, spec_prim_1, Year, pos_office, pos_asc, pos_opd))

#create the integration variable
PPAS2013.df = PPAS2013.df %>% 
  mutate(INT = pos_opd / (pos_opd + pos_asc + pos_office)) %>% 
  rename(NPI = npi)




#Medical utilization and payment data: These files provide data on the quantities and Medicare spending    --------------------------
#        of each physician and service. We'll use these data to capture total physician-level billing activity, 
#        and we'll use the service-level data to measure the revenue effects from our plausibly exogenous policy shock.


Medicare_Util_2013.df <- read.delim('data/input/Medicare_Util_Pay/2013/2013/Medicare_Provider_Util_Payment_PUF_CY2013.txt')

ls(Medicare_Util_2013.df)


#looking at only doctors
Medicare_Util_2013.df <- Medicare_Util_2013.df %>% 
  filter(NPPES_CREDENTIALS == 'M.D.' | NPPES_CREDENTIALS == 'MD')

#removing variables I don't need
Medicare_Util_2013.df = Medicare_Util_2013.df %>% 
  select(c(NPI, NPPES_CREDENTIALS, AVERAGE_MEDICARE_PAYMENT_AMT, BENE_UNIQUE_CNT, LINE_SRVC_CNT, HCPCS_CODE, NPPES_PROVIDER_ZIP))


# Combine the Medicare_Util and PPAS databases 

typeof(PPAS2013.df$NPI)

combined_2013 <- left_join(Medicare_Util_2013.df, PPAS2013.df, by = 'NPI')

combined_2013 = na.omit(combined_2013)

save(combined_2013, file = 'data/output/combined_2013.rda')

rm(list = ls())







##########                      2014                  #################


#MD-PPAS: The Medicare Data on Provider Practice and Specialty includes data on physician specialties,  -----------------------
#         practice IDs, demographics, and place of service


PPAS2014.df <- read.csv('data/input/MDPPAS/PhysicianData_2014/PhysicianData_2014.csv')
ls(PPAS2014.df)

PPAS2014.df = PPAS2014.df %>% 
  select(c(npi, sex, spec_broad, spec_prim_1, Year, pos_office, pos_asc, pos_opd))

#create the integration variable
PPAS2014.df = PPAS2014.df %>% 
  mutate(INT = pos_opd / (pos_opd + pos_asc + pos_office)) %>% 
  rename(NPI = npi)




#Medical utilization and payment data: These files provide data on the quantities and Medicare spending    --------------------------
#        of each physician and service. We'll use these data to capture total physician-level billing activity, 
#        and we'll use the service-level data to measure the revenue effects from our plausibly exogenous policy shock.


Medicare_Util_2014.df <- read.delim('data/input/Medicare_Util_Pay/2014/2014/Medicare_Provider_Util_Payment_PUF_CY2014.txt')

ls(Medicare_Util_2014.df)

names(Medicare_Util_2014.df) = toupper(names(Medicare_Util_2014.df))

#looking at only doctors
Medicare_Util_2014.df <- Medicare_Util_2014.df %>% 
  filter(NPPES_CREDENTIALS == 'M.D.' | NPPES_CREDENTIALS == 'MD')

#removing variables I don't need
Medicare_Util_2014.df = Medicare_Util_2014.df %>% 
  select(c(NPI, NPPES_CREDENTIALS, AVERAGE_MEDICARE_PAYMENT_AMT, BENE_UNIQUE_CNT, LINE_SRVC_CNT, HCPCS_CODE, NPPES_PROVIDER_ZIP))


# Combine the Medicare_Util and PPAS databases 

typeof(PPAS2014.df$NPI)

combined_2014 <- left_join(Medicare_Util_2014.df, PPAS2014.df, by = 'NPI')

combined_2014 = na.omit(combined_2014)

save(combined_2014, file = 'data/output/combined_2014.rda')

rm(list = ls())






##########                      2015                  #################


#MD-PPAS: The Medicare Data on Provider Practice and Specialty includes data on physician specialties,  -----------------------
#         practice IDs, demographics, and place of service


PPAS2015.df <- read.csv('data/input/MDPPAS/PhysicianData_2015/PhysicianData_2015.csv')
ls(PPAS2015.df)

PPAS2015.df = PPAS2015.df %>% 
  select(c(npi, sex, spec_broad, spec_prim_1, Year, pos_office, pos_asc, pos_opd))

#create the integration variable
PPAS2015.df = PPAS2015.df %>% 
  mutate(INT = pos_opd / (pos_opd + pos_asc + pos_office)) %>% 
  rename(NPI = npi)




#Medical utilization and payment data: These files provide data on the quantities and Medicare spending    --------------------------
#        of each physician and service. We'll use these data to capture total physician-level billing activity, 
#        and we'll use the service-level data to measure the revenue effects from our plausibly exogenous policy shock.


Medicare_Util_2015.df <- read.delim('data/input/Medicare_Util_Pay/2015/2015/Medicare_Provider_Util_Payment_PUF_CY2015.txt')

ls(Medicare_Util_2015.df)

names(Medicare_Util_2015.df) = toupper(names(Medicare_Util_2015.df))


#looking at only doctors
Medicare_Util_2015.df <- Medicare_Util_2015.df %>% 
  filter(NPPES_CREDENTIALS == 'M.D.' | NPPES_CREDENTIALS == 'MD')

#removing variables I don't need
Medicare_Util_2015.df = Medicare_Util_2015.df %>% 
  select(c(NPI, NPPES_CREDENTIALS, AVERAGE_MEDICARE_PAYMENT_AMT, BENE_UNIQUE_CNT, LINE_SRVC_CNT, HCPCS_CODE, NPPES_PROVIDER_ZIP))


# Combine the Medicare_Util and PPAS databases 

typeof(PPAS2015.df$NPI)

combined_2015 <- left_join(Medicare_Util_2015.df, PPAS2015.df, by = 'NPI')

combined_2015 = na.omit(combined_2015)

save(combined_2015, file = 'data/output/combined_2015.rda')

rm(list = ls())






##########                      2016                  #################


#MD-PPAS: The Medicare Data on Provider Practice and Specialty includes data on physician specialties,  -----------------------
#         practice IDs, demographics, and place of service


PPAS2016.df <- read.csv('data/input/MDPPAS/PhysicianData_2016/PhysicianData_2016.csv')
ls(PPAS2016.df)

PPAS2016.df = PPAS2016.df %>% 
  select(c(npi, sex, spec_broad, spec_prim_1, Year, pos_office, pos_asc, pos_opd))

#create the integration variable
PPAS2016.df = PPAS2016.df %>% 
  mutate(INT = pos_opd / (pos_opd + pos_asc + pos_office)) %>% 
  rename(NPI = npi)




#Medical utilization and payment data: These files provide data on the quantities and Medicare spending    --------------------------
#        of each physician and service. We'll use these data to capture total physician-level billing activity, 
#        and we'll use the service-level data to measure the revenue effects from our plausibly exogenous policy shock.


Medicare_Util_2016.df <- read.delim('data/input/Medicare_Util_Pay/2016/2016/Medicare_Provider_Util_Payment_PUF_CY2016.txt')

ls(Medicare_Util_2016.df)

names(Medicare_Util_2016.df) = toupper(names(Medicare_Util_2016.df))


#looking at only doctors
Medicare_Util_2016.df <- Medicare_Util_2016.df %>% 
  filter(NPPES_CREDENTIALS == 'M.D.' | NPPES_CREDENTIALS == 'MD')

#removing variables I don't need
Medicare_Util_2016.df = Medicare_Util_2016.df %>% 
  select(c(NPI, NPPES_CREDENTIALS, AVERAGE_MEDICARE_PAYMENT_AMT, BENE_UNIQUE_CNT, LINE_SRVC_CNT, HCPCS_CODE, NPPES_PROVIDER_ZIP))


# Combine the Medicare_Util and PPAS databases 

typeof(PPAS2016.df$NPI)

combined_2016 <- left_join(Medicare_Util_2016.df, PPAS2016.df, by = 'NPI')

combined_2016 = na.omit(combined_2016)

save(combined_2016, file = 'data/output/combined_2016.rda')

rm(list = ls())






##########                      2017                  #################


#MD-PPAS: The Medicare Data on Provider Practice and Specialty includes data on physician specialties,  -----------------------
#         practice IDs, demographics, and place of service


PPAS2017.df <- read.csv('data/input/MDPPAS/PhysicianData_2017/PhysicianData_2017.csv')
ls(PPAS2017.df)

PPAS2017.df = PPAS2017.df %>% 
  select(c(npi, sex, spec_broad, spec_prim_1, Year, pos_office, pos_asc, pos_opd))

#create the integration variable
PPAS2017.df = PPAS2017.df %>% 
  mutate(INT = pos_opd / (pos_opd + pos_asc + pos_office)) %>% 
  rename(NPI = npi)




#Medical utilization and payment data: These files provide data on the quantities and Medicare spending    --------------------------
#        of each physician and service. We'll use these data to capture total physician-level billing activity, 
#        and we'll use the service-level data to measure the revenue effects from our plausibly exogenous policy shock.


Medicare_Util_2017.df <- read.delim('data/input/Medicare_Util_Pay/2017/2017/Medicare_Provider_Util_Payment_PUF_CY2017.txt')

ls(Medicare_Util_2017.df)

names(Medicare_Util_2017.df) = toupper(names(Medicare_Util_2017.df))


#looking at only doctors
Medicare_Util_2017.df <- Medicare_Util_2017.df %>% 
  filter(NPPES_CREDENTIALS == 'M.D.' | NPPES_CREDENTIALS == 'MD')

#removing variables I don't need
Medicare_Util_2017.df = Medicare_Util_2017.df %>% 
  select(c(NPI, NPPES_CREDENTIALS, AVERAGE_MEDICARE_PAYMENT_AMT, BENE_UNIQUE_CNT, LINE_SRVC_CNT, HCPCS_CODE, NPPES_PROVIDER_ZIP))


# Combine the Medicare_Util and PPAS databases 

typeof(PPAS2017.df$NPI)

combined_2017 <- left_join(Medicare_Util_2017.df, PPAS2017.df, by = 'NPI')

combined_2017 = na.omit(combined_2017)

save(combined_2017, file = 'data/output/combined_2017.rda')

rm(list = ls())






##########                      2018                  #################


#MD-PPAS: The Medicare Data on Provider Practice and Specialty includes data on physician specialties,  -----------------------
#         practice IDs, demographics, and place of service


PPAS2018.df <- read.csv('data/input/MDPPAS/PhysicianData_2018/PhysicianData_2018.csv')
ls(PPAS2018.df)

PPAS2018.df = PPAS2018.df %>% 
  select(c(npi, sex, spec_broad, spec_prim_1, Year, pos_office, pos_asc, pos_opd))

#create the integration variable
PPAS2018.df = PPAS2018.df %>% 
  mutate(INT = pos_opd / (pos_opd + pos_asc + pos_office)) %>% 
  rename(NPI = npi)




#Medical utilization and payment data: These files provide data on the quantities and Medicare spending    --------------------------
#        of each physician and service. We'll use these data to capture total physician-level billing activity, 
#        and we'll use the service-level data to measure the revenue effects from our plausibly exogenous policy shock.


Medicare_Util_2018.df <- read.delim('data/input/Medicare_Util_Pay/2018/2018/Medicare_Provider_Util_Payment_PUF_CY2018.txt')

ls(Medicare_Util_2018.df)

names(Medicare_Util_2018.df) = toupper(names(Medicare_Util_2018.df))

#looking at only doctors
Medicare_Util_2018.df <- Medicare_Util_2018.df %>% 
  filter(NPPES_CREDENTIALS == 'M.D.' | NPPES_CREDENTIALS == 'MD')

#removing variables I don't need
Medicare_Util_2018.df = Medicare_Util_2018.df %>% 
  select(c(NPI, NPPES_CREDENTIALS, AVERAGE_MEDICARE_PAYMENT_AMT, BENE_UNIQUE_CNT, LINE_SRVC_CNT, HCPCS_CODE, NPPES_PROVIDER_ZIP))


# Combine the Medicare_Util and PPAS databases 

typeof(PPAS2018.df$NPI)

combined_2018 <- left_join(Medicare_Util_2018.df, PPAS2018.df, by = 'NPI')

combined_2018 = na.omit(combined_2018)

save(combined_2018, file = 'data/output/combined_2018.rda')

rm(list = ls())











###################     creating the final database

rm(list = ls())


load(file = 'data/output/combined_2012.rda')
load(file = 'data/output/combined_2013.rda')
load(file = 'data/output/combined_2014.rda')


Assign2.df <- rbind(combined_2012, combined_2013, combined_2014)

Assign2.df = Assign2.df %>% group_by(NPI) %>% 
  filter(INT[1] == 0) %>% 
  filter(Year[1] == 2012)

Assign2.df = Assign2.df %>% ungroup()

rm(combined_2012, combined_2013, combined_2014)


load(file = 'data/output/combined_2015.rda')


Assign2.df = rbind(Assign2.df, combined_2015)

Assign2.df = Assign2.df %>% group_by(NPI) %>% 
  filter(INT[1] == 0) %>% 
  filter(Year[1] == 2012) %>% 
  ungroup() 

table(Assign2.df$Year)

rm(combined_2015)

save(Assign2.df, file = 'data/output/Assign2.df')


load('data/output/Assign2.df')

ls(Assign2.df)


load(file = 'data/output/combined_2016.rda')
combined_2016 = na.omit(combined_2016)

Assign2.df = rbind(Assign2.df, combined_2016)

Assign2.df = Assign2.df %>% group_by(NPI) %>% 
  filter(INT[1] == 0) %>% 
  filter(Year[1] == 2012) %>% 
  ungroup() 

table(Assign2.df$Year)

rm(combined_2016)



load(file = 'data/output/combined_2017.rda')

Assign2.df = rbind(Assign2.df, combined_2017)

Assign2.df = Assign2.df %>% group_by(NPI) %>% 
  filter(INT[1] == 0) %>% 
  filter(Year[1] == 2012) %>% 
  ungroup() 

table(Assign2.df$Year)

rm(combined_2017)




load(file = 'data/output/combined_2018.rda')


Assign2.df = rbind(Assign2.df, combined_2018)

Assign2.df = Assign2.df %>% group_by(NPI) %>% 
  filter(INT[1] == 0) %>% 
  filter(Year[1] == 2012) %>% 
  ungroup() 

table(Assign2.df$Year)

rm(combined_2018)



Assign2.df = Assign2.df %>% distinct()
save(Assign2.df, file = 'data/output/Assign2.df')

#make the database more managable - random 50% sample 
Assign2a.df = Assign2.df %>% group_by(NPI) %>%  sample_frac(0.5) %>% ungroup()

save(Assign2a.df, file = 'data/output/Assign2a.df')

rm(Assign2.df) 





######## create TAX.ID base
PPAS2009.df <- read.csv('data/input/MDPPAS/PhysicianData_2009/PhysicianData_2009.csv')

ls(PPAS2009.df)
taxid.base = PPAS2009.df %>% select(npi, group1)
taxid.base = na.omit(taxid.base)
taxid.base = taxid.base %>% rename(tax_id = group1)
save(taxid.base, file = 'data/output/taxid.base')

