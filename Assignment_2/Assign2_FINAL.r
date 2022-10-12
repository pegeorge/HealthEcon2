


library(tidyverse)
library(gt)
library(gtsummary)
library(ggplot2)
library(broom)
library(fixest)

rm(list = ls())

#setwd
setwd("C:/Users/pegeorg/OneDrive - Emory University/PhD/Health Econ 2/Assignments/Assignment_2/")

#MAC
setwd('~/OneDrive - Emory University/PhD/Health Econ 2/Assignments/Assignment_2/')


#Physician Fee Schedule 2010 Update database:   dataset with service-specific price shocks     --------------------------------- 
#         introduced by the 2010 fee schedule update.
PFS.df <- read.delim('data/input/PFS_update_data.txt')



#note, this dataframe comes from file Assign2_create_df.R 
load('data/output/Assign2a.df')

ls(Assign2a.df)
Assign2a.df = Assign2a.df %>% filter(Year<2018)    #removing 2018 per assignment specifications 

#first, add yearly payment variable (which I take average payment * total # of unique claims)

checkdf = Assign2a.df[1:1000, ]

Assign2a.df = Assign2a.df %>% arrange(NPI, Year)
Assign2a.df = Assign2a.df %>% 
  mutate(partial_payment = AVERAGE_MEDICARE_PAYMENT_AMT*LINE_SRVC_CNT)

Assign2b.df = Assign2a.df %>% group_by(NPI, Year) %>% summarise(yearly_payment = sum(partial_payment), 
                                                                yearly_claims = sum(LINE_SRVC_CNT), 
                                                                yearly_patients = sum(BENE_UNIQUE_CNT))

Assign2c.df = full_join(Assign2a.df, Assign2b.df)


rm(Assign2a.df, Assign2b.df)



##########          Assign2c.df here 

Assign2c.df <- na.omit(Assign2c.df)
checkdf = Assign2c.df[1:10000,]

Assign2c.df$fNPI = as.factor(Assign2c.df$NPI)

class(Assign2c.df$fNPI)

Assign2c.df = Assign2c.df %>% group_by(NPI, Year) %>% 
  mutate(NUM = row_number())

# the averages make sense, but the ranges are clearly off. more than 1,000,000 yearly claims in some cases....
#     clearly not possible, so we will remove outliers

Assign2c.df = Assign2c.df %>% filter(yearly_claims < 9999)

summary(Assign2c.df$INT)

Assign2c.df = Assign2c.df %>% 
  mutate(INT_Group = ifelse(INT >= 0.75, 1, 0))

table(Assign2c.df$INT_Group, Assign2c.df$Year)

Assign2c.df$INT_Group = as.factor(Assign2c.df$INT_Group)



#note, short_df has only 1 row per physician per year
short.df = Assign2c.df %>% ungroup() %>%           
  filter(NUM == 1)

checkdf = short.df[1:1000, ]


# 1    Provide and discuss a table of simple summary statistics showing the mean, standard deviation,   ------------------------------
# min, and max of total physician-level Medicare spending, claims, and patients.


table(short.df$Year)
summary(short.df$yearly_payment)


Table_1 = short.df %>% select(Year, yearly_payment, yearly_claims, yearly_patients) %>%  
  tbl_summary(
  by = Year,
  type = all_continuous() ~ 'continuous2', 
  label = list(yearly_payment ~ "Yearly Payment", yearly_claims ~ "Yearly Claims", yearly_patients ~ "Yearly Patients"),
  statistic = all_continuous() ~ c("{mean} ({sd})", 
                               "{min}, {max}"), ) %>% 
  as_gt()

Table_1
Table_1a = Table_1 %>% 
  tab_header(
    title = md('Table 1: Summary Statistics'), 
    subtitle = md('by year, per physician'))

Table_1a  

Table_1a %>% gtsave(filename = 'TablesFigures/Table_1a.png')



#2     Form a proxy for integration using the ratio, INT, and Plot the mean of total    ------------------------
#   physician-level claims for integrated versus non-integrated physicians over time 
#   (just pick a few thresholds for defining "integration")


Q2.df = Assign2c.df %>% 
  group_by(INT_Group, Year) %>% 
  summarize(mean_claim = mean(yearly_claims))

Q2_Plot = ggplot(Q2.df) + 
  aes(x = Year, y = mean_claim, color = INT_Group) + 
  geom_point() + 
  geom_line() + 
  scale_y_continuous(name = 'Total Number of Claims', n.breaks = 8) +
  scale_x_continuous(name = 'Year', n.breaks = 7) +
  theme_classic() + 
  labs(title = 'Total Claims', 
       subtitle = "by Year and Integration Status", 
       caption = "Integrated is defined as >75% of claims being from a hospital setting") + 
  scale_color_discrete(name = "Integrated?", labels = c('No', 'Yes')) + 
  theme(legend.position = c(0.9, 0.5))

Q2_Plot

ggsave(filename = 'TablesFigures/Q2_Plot.png', plot = last_plot())

#3     Estimate the relationship between integration on total physician claims using OL  ----------------------------

# use short_df for this model 
ls(short.df)

#zip code will be time varying physician characteristics 
class(short.df$NPPES_PROVIDER_ZIP)

short.df$fZIP = substr(short.df$NPPES_PROVIDER_ZIP, 1, 5)

short.df$NPPES_PROVIDER_ZIP = as.factor(short.df$NPPES_PROVIDER_ZIP)

short.df$log_yearly_claims = log(short.df$yearly_claims)

head(short.df$yearly_claims)
head(short.df$log_yearly_claims)

#I initially tried to include Zip code as a time varying effect, but I was unable to surprisingly due to memory issues ... 
Q3.model = feols(log_yearly_claims ~ factor(INT_Group) | Year + NPI, data = short.df)

summary(Q3.model)
Q3.tab = tidy(Q3.model)

Q3.tab

ls(Q3.tab)

Q3.tab[1,1] = 'Integrated (Yes)'
Q3.tab = Q3.tab %>% 
  rename('standard error' = std.error)

Table_3 = Q3.tab %>% 
  gt() %>% 
  fmt_number(columns = 2:5, 
             decimals = 3) %>% 
  tab_header(
    title = md('OLS Summary'), 
    subtitle = md('Outcome = Yearly Claims')
  ) %>% 
  cols_align(align = 'center')

Table_3

Table_3 %>% gtsave(filename = 'TablesFigures/Table_3.png')



#4        How much should we be worried about endo ....                             -------------------------------------

library(robomit)
short.df$INT_Group = as.numeric(short.df$INT_Group)
summary(short.df$INT_Group)

columns = c('Adjusted beta', 'R2 Max', 'delta')
Q4.tab = data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(Q4.tab) = columns 
n = 1 
for (j in 1:5) { 
  for (i in 1:6) {
    dlta = -0.5 + j*0.5
    R2m = 0.4 + i*0.1
    Q4.adj = o_beta(y = "yearly_claims",                         # define the dependent variable name
       x = "INT_Group",                          # define the main independent variable name
       con = "NPI + Year",                    # other control variables
       delta = dlta,                              # define delta. This is usually set to 1
       R2max = R2m,                             # define the max R-square.
       type = "lm",                          # define model type
       data = short.df)                   # define dataset
    Q4.tab[n, 1] = Q4.adj[1,2]
    Q4.tab[n, 2] = R2m
    Q4.tab[n, 3] = dlta
    n = n + 1
  }
} 


Table_4 = Q4.tab %>% 
  gt() %>% 
  tab_header(
  title = md('Oster 2019 measures of endogeneity'), 
  subtitle = md('by R2 and Delta')
)

Table_4

Table_4 %>% gtsave(filename = 'TablesFigures/Table_4.png')




#5       Construct the change in Medicare payments achievable for an integrated versus       ---------------------------------
#        non-integrated physician practice due to the 2010 update to the physician fee schedule  


#below code is from Ian (modified by me...)
ls(PFS.df)
ls(Assign2c.df)

names(Assign2c.df) = tolower(names(Assign2c.df))
names(short.df) = tolower(names(short.df))

load('data/output/taxid.base')

ls(short.df)



Q5.df = inner_join(short.df, taxid.base, by = 'npi')

price.shock <- Q5.df %>%                        
  inner_join(PFS.df %>% 
               select(hcpcs, dprice_rel_2010, price_nonfac_orig_2010, price_nonfac_orig_2007), 
             by=c("hcpcs_code"="hcpcs")) %>%
  mutate_at(vars(dprice_rel_2010, price_nonfac_orig_2010, price_nonfac_orig_2007), replace_na, 0) %>%
  mutate(price_shock = case_when(
    i<=2013 ~ ((i-2009)/4)*dprice_rel_2010,
    i>2013  ~ dprice_rel_2010),
    denom = line_srvc_cnt*price_nonfac_orig_2010,
    numer = price_shock*line_srvc_cnt*price_nonfac_orig_2010) %>%
  group_by(npi) %>%
  summarize(phy_numer=sum(numer, na.rm=TRUE), phy_denom=sum(denom, na.rm=TRUE), tax_id=first(tax_id)) %>%
  ungroup() %>%
  mutate(phy_rev_change=phy_numer/phy_denom) %>%    
  group_by(tax_id) %>%
  summarize(practice_rev_change=sum(phy_rev_change, na.rm=TRUE)) %>%
  ungroup()



Q5.df = left_join(Q5.df, price.shock, by = 'tax_id')
ls(Q5.df)

## helpful for running fixed effect models with instruments in feols (fixest)   https://lrberge.github.io/fixest/reference/feols.html  

Q5.model = feols(log_yearly_claims ~ 1 | npi + year | factor(int_group) ~ practice_rev_change, data = Q5.df)    

summary(Q5.model)
summary(Q5.model, stage = 1:2)

summary(Q5.model, stage = 1:2)

Q5.table = Q5.model$coeftable

ls(Q5.table)

Q5.table = Q5.table %>% 
  rename(p.value = 'Pr(>|t|)', 
         'standard error' = 'Std. Error')

Q5.table = as.data.frame(Q5.table)   # I couldn't figure out how to export the 1st stage from feols 
Q5.table[2,1] = -0.90866
Q5.table[2,2] = 11592.2
Q5.table[2,3] = -7.8e-05
Q5.table[2,4] = 0.99994
Q5.table[2,5] = '1st stage'
Q5.table[1,5] = '2nd stage'

Q5.table = Q5.table %>% rename(Stage = V5)

Table_5 = Q5.table %>% gt() %>% 
  tab_header(
    title = md('IV Estimates'), 
    subtitle = md('1st and 2nd stage')) %>% 
  fmt_number(columns = 3:4, 
             decimals = 3) 

Table_5

Table_5 %>% gtsave(filename = 'TablesFigures/Table_5.png')




#6. Assess the "need" for IV by implementing a Durbin-Wu-Hausman test with an augmented regression.
Q6.df = na.omit(Q5.df)

Q6.model = feols(int ~ practice_rev_change | npi + year, data = Q6.df)    

Q6.df$q6_residuals = Q6.model$residuals

ls(Q6.df)

Q6.modelb = feols(yearly_claims ~ practice_rev_change + q6_residuals | npi + year, data = Q6.df)
summary(Q6.modelb)

# Note the very small P value. Thus, here, we reject the null hypothesis, which in this case, means
# our beta1 is inconsistent and therefore there is significant endogeneity. Thus, we do 'need' IV, or 
# some other way of overcoming the endogeneity of our regressors. 






# 7 Now let's pay attention to potential issues of weak instruments.
library(ivmodel)

Q6.df$constant = 1

Y = Q6.df$yearly_claims
D = Q6.df$int_group
Z = Q6.df$practice_rev_change
X = Q6.df$constant

Q7.tab = ivmodel(Y=Y, D=D, Z=Z, X=X)

Q7.sum = summary(Q7.tab)

Q7.tab2 = Q7.sum$AR

Q7.tab2 = as.data.frame(Q7.tab2)

Q7.tab2 = Q7.tab2 %>% 
  select(-ci.info)

ls(Q7.tab2)

Q7.tab2 = Q7.tab2 %>% 
  rename('lower bound' = ci.lower, 
         'upper bound' = ci.upper, 
         'f.statistic' = Fstat)

Table_7 = Q7.tab2 %>% gt() %>% 
  tab_header(
    title = md('Anderson-Rubin Wald statistic'), 
    subtitle = md('using ivmodel')) 

Table_7

Table_7 %>% gtsave(filename = 'TablesFigures/Table_7.png')


#Quite surprisingly, the estimates are very different using this method, as compared to the method in Equation (2). 
# I went back and double checked my work from before, and cannot figure out why there is such a large difference. 
# Further, for my 2SLS results, I do not even need to inflate the standard errors anymore than they are already
# quite inflated. 



#8. 
Q8.df = Q6.df
ls(Q8.df)
Q8.df$prv_1 = sample(Q8.df$practice_rev_change)

i = 1
for(i in 2:100){
  j = i - 1
  Q8.df[[paste('prv_', i, sep = '')]] <- sample(Q8.df[[paste('prv_', j, sep = '')]])
}



Q8.df = Q8.df %>% 
  mutate(prv_tot = select(., prv_1:prv_100) %>% rowSums(), 
         prv_mean = prv_tot/100, 
         prv_cent = practice_rev_change - prv_mean)

summary(Q8.df$practice_rev_change)
summary(Q8.df$prv_mean)

Q8.model = feols(log_yearly_claims ~ 1 | npi + year | factor(int_group) ~ prv_cent, data = Q8.df)    

summary(Q8.model)
summary(Q8.model, stage = 1:2)


Q8.table = Q8.model$coeftable

Q8.table = as.data.frame(Q8.table)   # I couldn't figure out how to export the 1st stage from feols 
Q8.table[2,1] = 4.62e-08
Q8.table[2,2] = 2.02e-08
Q8.table[2,3] = -2.28819
Q8.table[2,4] = 0.022128
Q8.table[2,5] = '1st stage'
Q8.table[1,5] = '2nd stage'

ls(Q8.table)
Q8.table= Q8.table %>% 
  rename(Stage = V5, 
         p.value = 'Pr(>|t|)', 
         'standard error' = 'Std. Error')

Table_8 = Q8.table %>% gt() %>% 
  tab_header(
    title = md('IV Estimates (Question 8)'), 
    subtitle = md('1st and 2nd stage')) %>% 
  fmt_number(columns = 1:4, 
             decimals = 3) 
Table_8

Table_8 %>% gtsave(filename = 'TablesFigures/Table_8.png')

