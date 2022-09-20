
# Intro --------------------------------------------

## Author: Paul George
## Title: Empirical Exercise 1, Health Econ 2 
## Date Created: 8/30/22
## Date Edited: 8/31/22 - Paul George 
## Notes: This file creates the final dataframe to be used in assignment 1, POS_HCRIS_df


# Prelim ----------------------------------------------------------------------

rm(list=ls())

#PC
setwd('C:/Users/pegeorg/OneDrive - Emory University/PhD/Health Econ 2/Assignments/Assignment_1/')

#Mac
setwd('~/OneDrive - Emory University/PhD/Health Econ 2/Assignments/Assignment_1')


library(haven)
library(ggplot2)
library(scales)
library(readxl)
library(tidyverse)



# Databases to use -------------------------------------------------------------

# first, I will combine the POS dataframes into 1 dataframe, keeping only hospitals 
# notes
#   gnrl_cntl_type_cd = ownership type
#   prvdr_ctgry_cd = facility type (1 = hospital)
#   prvdr_num = CMS cert number
#   state_cd = state code


pos_df <- data.frame(matrix(ncol = 5, nrow = 0))
x <- c('prvdr_num', 'prvdr_ctgry_cd', 'gnrl_cntl_type_cd', 'state_cd', 'year')
colnames(pos_df) <- x


for (i in 2000:2018) {
  df1 <- read.csv(paste0('data/input/pos-data/pos',i,'.csv/pos',i,'.csv'))
  names(df1) <- tolower(names(df1))
  df1 <- df1 %>% filter(prvdr_ctgry_cd == 1)
  df2 <- df1 %>% select(prvdr_num, prvdr_ctgry_cd, gnrl_cntl_type_cd, state_cd)
  df2 <- df2 %>% mutate(year = i)
  pos_df <- rbind(pos_df, df2)
  rm(df1)
  rm(df2)
}

pos_df <- pos_df %>% rename(provider_number = prvdr_num)

save(pos_df, file = 'data/output/pos_df.rda')


# note, this database was created by combining the POS dataframes from 2000 to 2018
#    only keeping prvdr_ctgry_cd == 1 (which is the code for hospital)

load('data/output/pos_df.rda')

# (note, this database was created using Ian's code, _HCRIS_Data.R, and the data
#     from the class OneDrive Folder)
load('data/output/final.hcris.data.rda')



# Next, I will combine the dataframes, by provider number and year
#          using both number and year in case the categories change from year to year

POS_HCRIS_df <- right_join(pos_df, final.hcris.data, 
                           by = c('provider_number' = 'provider_number', 
                                  'year' = 'year'))


# note, a lot of NAs for state_cd 
table(POS_HCRIS_df$state_cd, useNA = 'always')


# This step will add the Medicaid expansion year to the dataframe, using an spreadsheet I made myself
#    using KFF data 

medicaid_expansion.df <- read_xlsx('data/input/medicaid expansion.xlsx')

POS_HCRIS_df <- right_join(medicaid_expansion.df, POS_HCRIS_df, 
                             by = c('State' = 'state_cd'))



save(POS_HCRIS_df, file = 'data/output/POS_HCRIS_df.rda')


