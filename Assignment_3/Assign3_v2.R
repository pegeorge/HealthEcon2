
library(tidyverse)
library(gt)
library(gtsummary)
library(modelsummary)
library(ggplot2)
library(broom)
library(fixest)
library(haven)
library(ggplot2)

rm(list = ls())

#setwd
setwd("C:/Users/pegeorg/OneDrive - Emory University/PhD/Health Econ 2/Assignments/Assignment_3/")

#MAC
#setwd('~/OneDrive - Emory University/PhD/Health Econ 2/Assignments/Assignment_2/')



main.df = read_dta('DataFiles/Data_main.dta')




#1.  create table 1                          --------------------------------------------------------

# first, I will determine the first year the plan existed

ls(main.df)
str(main.df)

main.df$orgParentCode = as.factor(main.df$orgParentCode)
levels(main.df$orgParentCode)
main.df$uniqueID = as.factor(main.df$uniqueID)

main.df = main.df %>% 
  group_by(uniqueID) %>% 
  arrange(uniqueID, year) %>% 
  mutate(year_of_plan = seq_along(year)) %>% 
  mutate(plan_entry_year = year[1])


# determine if firm offered plans in US
main.df = main.df %>% 
  group_by(orgParentCode) %>% 
  arrange(year) %>% 
  mutate(entry_year_firm_to_US = min(year)) %>% 
  mutate(exist_in_US_prior_dichot = ifelse(year > entry_year_firm_to_US, 1, 0))

# determine if firm offered plan in same state

main.df = main.df %>% 
  group_by(orgParentCode, state) %>% 
  arrange(year) %>% 
  mutate(entry_year_firm_to_state = year[1]) %>% 
  mutate(exist_in_st_prior_dichot = ifelse(year > entry_year_firm_to_state, 1, 0))


check.df = main.df %>% select(orgParentCode, uniqueID, year, state, year_of_plan, plan_entry_year, entry_year_firm_to_state, exist_in_st_prior_dichot) %>% 
  arrange(orgParentCode, state, year)



# create dataframe for Table 1

col_names = c('variable', 'subvariable', '2006', '2007', '2008', '2009', '2010')
Tab1.df = data.frame(matrix(ncol = 7, nrow = 0))
colnames(Tab1.df) = col_names

for (i in 1:5) {
  j = i + 1
  temp.df = main.df %>% filter(plan_entry_year == 2005+i) %>% filter(year == 2005 + i)
  Tab1.df[1, 2+i] = round(mean(temp.df$premium), 0)
  Tab1.df[2, 2+i] = round(sd(temp.df$premium), 0)
  
  Tab1.df[3, 2+i] = round(mean(temp.df$deductible), 0)
  Tab1.df[4, 2+i] = round(sd(temp.df$deductible), 0)
  
  bene.tab = as.data.frame(table(temp.df$benefit))
  b = bene.tab[2,2]/(bene.tab[1,2] + bene.tab[2,2])
  Tab1.df[5, 2+i] = round(b, 2)
  
  Tab1.df[6, 2+i] = ''
  
  exist_us.tab = as.data.frame(table(temp.df$exist_in_US_prior_dichot))
  e_u = exist_us.tab[2,2]/(exist_us.tab[2,2] + exist_us.tab[1,2])
  Tab1.df[7, 2+i] = round(e_u, 2)
  
  exist_st.tab = as.data.frame(table(temp.df$exist_in_st_prior_dichot))
  e_s = exist_st.tab[2,2]/(exist_st.tab[2,2] + exist_st.tab[1,2])
  Tab1.df[8, 2+i] = round(e_s, 2)
  
  Tab1.df[9, 2+i] = round(n_distinct(temp.df$orgParentCode), 0)
  Tab1.df[10, 2+i] = round(n_distinct(temp.df$uniqueID),0)
  
}

#add variables
Tab1.df$variable = list('Mean monthly premium', 'standard deviation', 'Mean deductible', 'standard deviation', 'Fraction enhanced benefit', 
                        'Fraction of plans already offered...', '', '', 'Number of Unique Firms', 'Number of Plans')

Tab1.df$subvariable = list('', '', '', '', '', '', 'in the US', 'in the same state', '', '')

#clean table
Tab1.df[7, 3] = 0
Tab1.df[8, 3] = 0
Tab1.df[7, 6] = 1


Table_1 = Tab1.df %>% 
  gt() %>% 
  tab_header(
    title = 'Table 1 - Descriptive Statistics'
  )

Table_1

Table_1 %>% gtsave(filename = 'TablesFigures/Table_1.png')



#2 - Recreate figure 3         -------------------------------------------------------------

library(rddtools)
library(rdrobust)

ls(main.df)
subsidy.df = read_dta('DataFiles/Data_subsidyinfo.dta')

ls(subsidy.df)
subsidy.df = subsidy.df %>% 
  pivot_longer(cols = c('s2006', 's2007', 's2008', 's2009', 's2010'), 
               names_to = 'syear') %>% 
  rename(subsidy = value) 

subsidy.df = subsidy.df %>% 
  mutate(year = ifelse(syear == 's2006', 2006, 
                       ifelse(syear == 's2007', 2007, 
                              ifelse(syear == 's2008', 2008, 
                                     ifelse(syear == 's2009', 2009, 2010)))))

main.df = main.df %>% ungroup()
  
combined.df = left_join(main.df, subsidy.df, by = c('year', 'PDPregion' ))

combined.df = combined.df %>% 
  mutate(LISPremium = premium - subsidy) %>% 
  mutate(proposedBenchmarkPlan = ifelse(LISPremium <=0, 1, 0))

#generate enrollment share (outcome variable)
combined.df = combined.df %>% 
  group_by(state, year) %>% 
  arrange(state, year) %>% 
  mutate(stateYrEnroll = sum(enrollment, na.rm = TRUE), 
         share = enrollment/stateYrEnroll, 
         lnS = log(share))

check.df = combined.df %>% select(state, year, enrollment, stateYrEnroll)

#remove NAs (they cause problems in graphs below)
combined.df = combined.df[!is.na(combined.df$lnS), ]

Fig3.df = combined.df %>% 
  filter(LISPremium >= -10.5 & LISPremium <= 10.5) %>% 
  filter(year == 2006)


#create the plot
p1 = rdplot(y = Fig3.df$lnS, x = Fig3.df$LISPremium, 
            c = 0, p = 1, h = 4, nbins = 20, 
            x.label = 'Monthly Premium - LIS Subsidy, 2006', 
            y.label = 'log enrollment share, 2006')

p1
class(p1$rdplot)
p1 = p1$rdplot

p2 = rdplot(y = Fig3.df$lnS, x = Fig3.df$LISPremium, c = 0, p = 4, nbins = 20, h = 10)
p2 = p2$rdplot

Figure3 = p1 + p2$layers[[2]] + p2$layers[[3]]
Figure3

Figure3 = Figure3 + 
  theme_classic() +
  labs(
    title = 'Figure 3', 
    caption = 'bin size = $0.50, linear regression bandwidth = 4, 4th order polynomial bandwith = 10'
  ) 

Figure3

ggsave(filename = 'TablesFigures/Figure3.png', plot = last_plot())


#3. Recreate Figure 3 from Ericson (2014) using J???,n=J+,n=10 and J???,n=J+,n=30. 


p4 = rdplot(y = Fig3.df$lnS, x = Fig3.df$LISPremium, 
            c = 0, p = 1, h = 4, nbins = 10, 
            x.label = 'Monthly Premium - LIS Subsidy, 2006', 
            y.label = 'log enrollment share, 2006')

p4 = p4$rdplot

p5 = rdplot(y = Fig3.df$lnS, x = Fig3.df$LISPremium, c = 0, p = 4, nbins = 10, h = 10)
p5 = p5$rdplot

Figure3_b = p4 + p5$layers[[2]] + p5$layers[[3]]
Figure3_b

Figure3_b = Figure3_b + 
  theme_classic() +
  labs(
    title = 'Figure 3 (revised)', 
    caption = 'bin size = $1.00, linear regression bandwidth = 4, 4th order polynomial bandwith = 10'
  ) 

Figure3_b



p4 = rdplot(y = Fig3.df$lnS, x = Fig3.df$LISPremium, 
            c = 0, p = 1, h = 4, nbins = 30, 
            x.label = 'Monthly Premium - LIS Subsidy, 2006', 
            y.label = 'log enrollment share, 2006')

p4 = p4$rdplot

p5 = rdplot(y = Fig3.df$lnS, x = Fig3.df$LISPremium, c = 0, p = 4, nbins = 30, h = 10)
p5 = p5$rdplot

Figure3_c = p4 + p5$layers[[2]] + p5$layers[[3]]
Figure3_c

Figure3_c = Figure3_c + 
  theme_classic() +
  labs(
    title = 'Figure 3 (revised)', 
    caption = 'bin size = $0.33, linear regression bandwidth = 4, 4th order polynomial bandwith = 10'
  ) 

Figure3_c

ggsave(filename = 'TablesFigures/Figure3_c.png', plot = last_plot())



#4. Use the rdrobust package in R (or Stata or Python) to find the optimal number of bins with an evenly-spaced binning strategy.  ----- 
#   Report this bin count and recreate your binned scatterplots from parts 2 and 3 based on the optimal bin number

#create the plot
p7 = rdplot(y = Fig3.df$lnS, x = Fig3.df$LISPremium, 
            c = 0, p = 1, h = 4, binselect = 'es', 
            x.label = 'Monthly Premium - LIS Subsidy, 2006', 
            y.label = 'log enrollment share, 2006')

p7
class(p7$rdplot)
p7 = p7$rdplot

p8 = rdplot(y = Fig3.df$lnS, x = Fig3.df$LISPremium, c = 0, p = 4, binselect = 'es', h = 10)
p8 = p8$rdplot

Figure4 = p7 + p8$layers[[2]] + p8$layers[[3]]
Figure4

Figure4 = Figure4 + 
  theme_classic() +
  labs(
    title = 'Figure 4', 
    caption = 'created bins using IMSE-optimal evenly-spaced method, which returns 7 bins on left, 6 on right'
  ) 

Figure4

ggsave(filename = 'TablesFigures/Figure4.png', plot = last_plot())


#5. Provide the results from the manipulation tests described in Cattaneo, Jansson, and Ma (2018).   ----------------
#   This test can be implemented with the rddensity package

library(rddensity)

rddens = rddensity(Fig3.df$LISPremium, c = 0)
summary(rddens)


# Using the above test, we obtain a p value of 0.38, which suggests there is not manipulation 
# around the threshold, which adds evidence to support the regression discontinuity design assumption. 







#6. Recreate Table 3 of Ericson (2014) using the same bandwidth of $4.00   ----------------------------------------------

#Table 3 focuses on plans who entered in 2006, create appropriate databases
Table3.df = combined.df %>% ungroup() %>% 
  filter(plan_entry_year == 2006)

ls(Table3.df)

#create variable for being below benchmark in 2006 (1 if below benchmark, 0 if above)
Table3.df = Table3.df %>% 
  group_by(uniqueID) %>% 
  mutate(proposedBenchmarkPlan_2006 = ifelse(proposedBenchmarkPlan == 1 & year == 2006, 1, 0))

Table3.df = Table3.df %>% 
  group_by(uniqueID) %>% 
  mutate(benchmark_in_2006 = case_when(
    1 %in% proposedBenchmarkPlan_2006 ~ 1, 
    TRUE ~ 0
  ))

Table3.df = Table3.df %>% 
  mutate(LISPremiumPos = ifelse(LISPremium > 0, LISPremium, 0), 
         LISPremiumNeg = ifelse(LISPremium < 0, LISPremium, 0), 
         LISPremiumPosSq = LISPremiumPos^2, 
         LISPremiumNegSq = LISPremiumNeg^2)



col_names = c('variable', 'subvariable', '2006', '2007', '2008', '2009', '2010')
Q6.df = data.frame(matrix(ncol = 7, nrow = 34))
colnames(Q6.df) = col_names

Q6.df$variable = list('Panel A - Linear', 'Below benchmark 2006', '', '', 'Premium - subsidy, 2006', '', '', '', '', '', '', 'obs', 'R2', 
                      'Panel B - Polynomial', ' Below benchmark, 2006', '', '', 'Premium - subsidy, 2006', 'obs', 'R2', 
                      'Panel C - Interactions', 'Below benchmark or de min', '', '', '', '', '', '', '', '', '', 'Premium - subsidy, 2006', 'obs', 'R2')

Q6.df$subvariable = list('', 'estimate', 'standard dev', 'p value', '', 'below estimate', 'standard dev', 'p value', 'above estimate', 'standard dev', 'p value', '', '', 
                         '', 'estimate', 'standard dev', 'p value', '', '', '', 
                         '', '', '2006 + Current year', '', '', '2006 and not current year', '', '', 'current year but not 2006', '', '', '', '', '')

table(Table3.df$benefit)

#only look at basic plans
Table3.df = Table3.df %>% filter(benefit == 'B')


for (i in 1:5) {
  temp.df = Table3.df %>% filter(year == i + 2005)
  temp.df = temp.df %>% filter(LISPremium < 4 & LISPremium > -4)
  
  #Panel A, Linear Model
  m1 = lm(lnS ~ benchmark_in_2006 + LISPremiumPos + LISPremiumNeg, data = temp.df)
  Q6.df[2, i + 2] = summary(m1)$coefficients['benchmark_in_2006', 'Estimate']
  Q6.df[3, i + 2] = summary(m1)$coefficients['benchmark_in_2006', 'Std. Error']
  Q6.df[4, i + 2] = summary(m1)$coefficients['benchmark_in_2006', 'Pr(>|t|)']
  
  Q6.df[6, i + 2] = summary(m1)$coefficients['LISPremiumNeg', 'Estimate']
  Q6.df[7, i + 2] = summary(m1)$coefficients['LISPremiumNeg', 'Std. Error']
  Q6.df[8, i + 2] = summary(m1)$coefficients['LISPremiumNeg', 'Pr(>|t|)']
  
  Q6.df[9, i + 2] = summary(m1)$coefficients['LISPremiumPos', 'Estimate']
  Q6.df[10, i + 2] = summary(m1)$coefficients['LISPremiumPos', 'Std. Error']
  Q6.df[11, i + 2] = summary(m1)$coefficients['LISPremiumPos', 'Pr(>|t|)']
  Q6.df[12, i + 2] = nobs(m1)
  Q6.df[13, i + 2] = summary(m1)$r.squared
  
  #Panel B, Polynomial model (note by default, SE are clustered at first fixed effect - here firm)
  m2 = feols(lnS ~ benchmark_in_2006 + LISPremiumPos + LISPremiumNeg + LISPremiumNegSq + LISPremiumPosSq |
               orgParentCode + state + btypedetail, data = temp.df)
  
  Q6.df[15, i + 2] = m2$coeftable['benchmark_in_2006', 'Estimate']
  Q6.df[16, i + 2] = m2$coeftable['benchmark_in_2006', 'Std. Error']
  Q6.df[17, i + 2] = m2$coeftable['benchmark_in_2006', 'Pr(>|t|)']
  
  Q6.df[19, i + 2] = nobs(m2)
  Q6.df[20, i + 2] = m2$sq.cor
  
  #Panel C, Interactions
  
  #have to do 2006 separately because of colinearity
  if (i == 1) {next}

  
  m3 = feols(lnS ~ benchmark_in_2006*proposedBenchmarkPlan + LISPremiumPos + LISPremiumNeg + LISPremiumNegSq + LISPremiumPosSq |
               orgParentCode + state + btypedetail, data = temp.df)
  
  
  Q6.df[23, i + 2] = m3$coeftable['benchmark_in_2006', 'Estimate'] + m3$coeftable['proposedBenchmarkPlan', 'Estimate'] + 
    m3$coeftable['benchmark_in_2006:proposedBenchmarkPlan', 'Estimate']
  Q6.df[26, i + 2] = m3$coeftable['benchmark_in_2006', 'Estimate']
  Q6.df[29, i + 2] = m3$coeftable['proposedBenchmarkPlan', 'Estimate']
  Q6.df[33, i + 2] = nobs(m3)
  Q6.df[34, i + 2] = m3$sq.cor
  }


#Panel C, Interactions

#have to do 2006 separately because of collinearity
i=1

temp.df = Table3.df %>% filter(year == i + 2005)
temp.df = temp.df %>% filter(LISPremium < 4 & LISPremium > -4)

m3 = feols(lnS ~ proposedBenchmarkPlan + LISPremiumPos + LISPremiumNeg + LISPremiumNegSq + LISPremiumPosSq |
             orgParentCode + state + btypedetail, data = temp.df)
m3

Q6.df[23, i + 2] = m3$coeftable['proposedBenchmarkPlan', 'Estimate']
Q6.df[24, i + 2] = m3$coeftable['proposedBenchmarkPlan', 'Std. Error']
Q6.df[25, i + 2] = m3$coeftable['proposedBenchmarkPlan', 'Pr(>|t|)']

Q6.df[33, i + 2] = nobs(m3)
Q6.df[34, i + 2] = m3$sq.cor

#clean up dataframe
Q6.df = Q6.df %>% mutate_if(is.numeric, round, digits = 3)

Q6.df[is.na(Q6.df)] = ''

Table_3_recreation = Q6.df %>% 
  gt() %>% 
  cols_align(
    align = 'left', 
    columns = 1:2
  ) %>% 
  tab_header(
    title = 'Table 3 - Recreation'
  )

Table_3_recreation  

Table_3_recreation %>% gtsave(filename = 'TablesFigures/Table_3_recreation.png')






#7 Re-estimate your RD results using the CE-optimal bandwidth (rdrobust will do this for you)  ------------------------------------
#  and compare the bandwidth and RD estimates to that in Table 3 of Ericson (2014)


#Table 3 focuses on plans who entered in 2006, create appropriate databases

test = rdbwselect(Table3.df$lnS, Table3.df$LISPremium, 
           bwselect = 'cercomb1')

test$bws
#note from the documentation, h specifies the main bandwidth, and b specifies the bias bandwidth
# thus, from above, we can see that our bandwidth per the specification should be 3.05 and 3.05

bandwidth = test$bws[1,1]

col_names = c('variable', 'subvariable', '2006', '2007', '2008', '2009', '2010')
Q7.df = data.frame(matrix(ncol = 7, nrow = 34))
colnames(Q7.df) = col_names

Q7.df$variable = list('Panel A - Linear', 'Below benchmark 2006', '', '', 'Premium - subsidy, 2006', '', '', '', '', '', '', 'obs', 'R2', 
                      'Panel B - Polynomial', ' Below benchmark, 2006', '', '', 'Premium - subsidy, 2006', 'obs', 'R2', 
                      'Panel C - Interactions', 'Below benchmark or de min', '', '', '', '', '', '', '', '', '', 'Premium - subsidy, 2006', 'obs', 'R2')

Q7.df$subvariable = list('', 'estimate', 'standard dev', 'p value', '', 'below estimate', 'standard dev', 'p value', 'above estimate', 'standard dev', 'p value', '', '', 
                         '', 'estimate', 'standard dev', 'p value', '', '', '', 
                         '', '', '2006 + Current year', '', '', '2006 and not current year', '', '', 'current year but not 2006', '', '', '', '', '')

#only look at basic plans
Table3.df = Table3.df %>% filter(benefit == 'B')


for (i in 1:5) {
  temp.df = Table3.df %>% filter(year == i + 2005)
  temp.df = temp.df %>% filter(LISPremium < bandwidth & LISPremium > -bandwidth)
  
  #Panel A, Linear Model
  m1 = lm(lnS ~ benchmark_in_2006 + LISPremiumPos + LISPremiumNeg, data = temp.df)
  Q7.df[2, i + 2] = summary(m1)$coefficients['benchmark_in_2006', 'Estimate']
  Q7.df[3, i + 2] = summary(m1)$coefficients['benchmark_in_2006', 'Std. Error']
  Q7.df[4, i + 2] = summary(m1)$coefficients['benchmark_in_2006', 'Pr(>|t|)']
  
  Q7.df[6, i + 2] = summary(m1)$coefficients['LISPremiumNeg', 'Estimate']
  Q7.df[7, i + 2] = summary(m1)$coefficients['LISPremiumNeg', 'Std. Error']
  Q7.df[8, i + 2] = summary(m1)$coefficients['LISPremiumNeg', 'Pr(>|t|)']
  
  Q7.df[9, i + 2] = summary(m1)$coefficients['LISPremiumPos', 'Estimate']
  Q7.df[10, i + 2] = summary(m1)$coefficients['LISPremiumPos', 'Std. Error']
  Q7.df[11, i + 2] = summary(m1)$coefficients['LISPremiumPos', 'Pr(>|t|)']
  Q7.df[12, i + 2] = nobs(m1)
  Q7.df[13, i + 2] = summary(m1)$r.squared
  
  #Panel B, Polynomial model (note by default, SE are clustered at first fixed effect - here firm)
  m2 = feols(lnS ~ benchmark_in_2006 + LISPremiumPos + LISPremiumNeg + LISPremiumNegSq + LISPremiumPosSq |
               orgParentCode + state + btypedetail, data = temp.df)
  
  Q7.df[15, i + 2] = m2$coeftable['benchmark_in_2006', 'Estimate']
  Q7.df[16, i + 2] = m2$coeftable['benchmark_in_2006', 'Std. Error']
  Q7.df[17, i + 2] = m2$coeftable['benchmark_in_2006', 'Pr(>|t|)']
  
  Q7.df[19, i + 2] = nobs(m2)
  Q7.df[20, i + 2] = m2$sq.cor
  
  #Panel C, Interactions
  
  #have to do 2006 separately because of colinearity
  if (i == 1) {next}
  
  
  m3 = feols(lnS ~ benchmark_in_2006*proposedBenchmarkPlan + LISPremiumPos + LISPremiumNeg + LISPremiumNegSq + LISPremiumPosSq |
               orgParentCode + state + btypedetail, data = temp.df)
  
  
  Q7.df[23, i + 2] = m3$coeftable['benchmark_in_2006', 'Estimate'] + m3$coeftable['proposedBenchmarkPlan', 'Estimate'] + 
    m3$coeftable['benchmark_in_2006:proposedBenchmarkPlan', 'Estimate']
  Q7.df[26, i + 2] = m3$coeftable['benchmark_in_2006', 'Estimate']
  Q7.df[29, i + 2] = m3$coeftable['proposedBenchmarkPlan', 'Estimate']
  Q7.df[33, i + 2] = nobs(m3)
  Q7.df[34, i + 2] = m3$sq.cor
}


#Panel C, Interactions

#have to do 2006 separately because of collinearity
i=1

temp.df = Table3.df %>% filter(year == i + 2005)
temp.df = temp.df %>% filter(LISPremium < 4 & LISPremium > -4)

m3 = feols(lnS ~ proposedBenchmarkPlan + LISPremiumPos + LISPremiumNeg + LISPremiumNegSq + LISPremiumPosSq |
             orgParentCode + state + btypedetail, data = temp.df)
m3

Q7.df[23, i + 2] = m3$coeftable['proposedBenchmarkPlan', 'Estimate']
Q7.df[24, i + 2] = m3$coeftable['proposedBenchmarkPlan', 'Std. Error']
Q7.df[25, i + 2] = m3$coeftable['proposedBenchmarkPlan', 'Pr(>|t|)']

Q7.df[33, i + 2] = nobs(m3)
Q7.df[34, i + 2] = m3$sq.cor

#clean up dataframe
Q7.df = Q7.df %>% mutate_if(is.numeric, round, digits = 3)

Q7.df[is.na(Q7.df)] = ''

Table_3_nb = Q7.df %>% 
  gt() %>% 
  cols_align(
    align = 'left', 
    columns = 1:2
  ) %>% 
  tab_header(
    title = 'Table 3 - New bandwidth'
  )

Table_3_nb

Table_3_nb %>% gtsave(filename = 'TablesFigures/Table_3_nb.png')







#8. Use the presence of Part D low-income subsidy as an IV for market     --------------------------------------------
# share to examine the effect of market share in 2006 on future premium changes.

#create ln(premium variable)
combined.df$lnPremium = log(combined.df$premium)

#exclude those plans who were not present in 2006
Q8.df = combined.df %>% filter(plan_entry_year == 2006)


Q8.df = Q8.df %>% 
  group_by(uniqueID) %>% 
  arrange(uniqueID, year) %>% 
  mutate(year_of_plan = seq_along(year)) %>% 
  mutate(lnS_2006 = lnS[1]) %>% 
  mutate(LIS_2006 = LIS[1]) %>% 
  ungroup()

ls(combined.df)

#fixed effects = orgParentCode, State; endo = lnS_2006; IV = LIS_2006
m4 = feols(lnPremium ~ as.factor(year_of_plan) + benefit | orgParentCode + state | lnS_2006 ~ LIS_2006, data = Q8.df)

summary(m4)

iv.summary <- msummary(list("IV"= m4),
                       shape=term + statistic ~ model, 
                       gof_map=NA,
                       coef_omit='Intercept|benefitE',
                       coef_rename=c("as.factor(year_of_plan)2"="Year of Plan 2",
                                     "as.factor(year_of_plan)3"="Year of Plan 3",
                                     "as.factor(year_of_plan)4"="Year of Plan 4",
                                     "as.factor(year_of_plan)5"="Year of Plan 5",
                                     "fit_lnS_2006"="Instrument",
                                     "practice_rev_change"="Instrument"),
                       #vcov = ~npi,
                       caption="Instrumental Variables Estimates",
                       output="gt",
                       label="ivmodels",
                       booktabs=T)

Table_5 = iv.summary %>% 
  tab_header(
    title = 'Table 5 - IV model', 
    subtitle = 'Low Income Subsidy in 2006 as instrument for enrollment share in 2006'
  )

Table_5

Table_5 %>% gtsave(filename = 'TablesFigures/Table_5.png')

