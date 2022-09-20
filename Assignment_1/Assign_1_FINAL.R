
# Intro --------------------------------------------


## Author: Paul George
## Title: Empirical Exercise 1, Health Econ 2 
## Date Created: 8/30/22
## Date Edited: 8/31/22 - Paul George 
## Notes: This code uses dataframe POS_HCRIS_df.rda, that I made separately
##         that combined POS and HCRIS data 



# Prelim ----------------------------------------------------------------------

rm(list=ls())

#PC
setwd('C:/Users/pegeorg/OneDrive - Emory University/PhD/Health Econ 2/Assignments/Assignment_1/')
load('data/output/POS_HCRIS_df.rda')

#mac
setwd('~/OneDrive - Emory University/PhD/Health Econ 2/Assignments/Assignment_1')
load('~/Downloads/POS_HCRIS_df.rda')


library(tidyverse)
library(gt)
library(gtExtras)
library(scales)
library(dplyr)
library(ggplot2)
library(tinytex)
library(modelsummary)
library(ggplot2)
library(here)
library(foreign)
library(tidyverse)
library(dplyr)
library(did)
library(HonestDiD)
library(Rglpk)

# Functions to use in the below code ---------------------------------------

# 
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summarized
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm), 
                     min  = min    (xx[[col]], na.rm=na.rm), 
                     max  = max    (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

# initial data management and cleanup   ------------------------------------------

# first create uncompensated care variable (because uncompensated care seems to be 
#      housed in two spots, uncomp_care and tot_uncomp_care_charges)  

POS_HCRIS_df <- POS_HCRIS_df %>% 
  mutate(uncomp_care_combined = coalesce(uncomp_care, tot_uncomp_care_charges))

summary(POS_HCRIS_df$uncomp_care)
summary(POS_HCRIS_df$tot_uncomp_care_charges)
summary(POS_HCRIS_df$uncomp_care_combined)


# next, look for and remove outliers, of the data of interest
boxplot(tot_pat_rev ~ year, dat = POS_HCRIS_df)
boxplot(uncomp_care_combined ~ year, dat = POS_HCRIS_df)

upper_pat_rev <- quantile(POS_HCRIS_df$tot_pat_rev, 0.975, na.rm = TRUE)
upper_uncomp_care <- quantile(POS_HCRIS_df$uncomp_care_combined, 0.975, na.rm = TRUE)

ls(POS_HCRIS_df)

POS_HCRIS_df <- POS_HCRIS_df %>% 
  filter(tot_pat_rev < upper_pat_rev | is.na(tot_pat_rev) == TRUE) %>% 
  filter(tot_pat_rev > 0 | is.na(tot_pat_rev) == TRUE) %>% 
  filter(uncomp_care_combined < upper_uncomp_care | is.na(uncomp_care_combined) == TRUE) %>% 
  filter(uncomp_care_combined > 0 | is.na(uncomp_care_combined) == TRUE)




# 1. Provide and discuss a table of simple summary statistics showing the mean, standard      ------------------------------------
#      deviation, min, and max of hospital total revenues and uncompensated care over time.

Tab1_uncompcare.df <- summarySE(POS_HCRIS_df, measurevar = 'uncomp_care_combined', groupvars = c('year'), na.rm = TRUE)
Tab1_totrev.df <- summarySE(POS_HCRIS_df, measurevar = 'tot_pat_rev', groupvars = c('year'), na.rm = TRUE)

Tab1_uncompcare.df[,c(3:8)] <- Tab1_uncompcare.df[,c(3:8)]/1000000
Tab1_uncompcare.df

Tab1_totrev.df[,c(3:8)] <- Tab1_totrev.df[,c(3:8)]/1000000
Tab1_totrev.df

#  since we are interested in uncomplicated care, we will only look at years 2003-2021 given 
#      data limitations seen in the tables above 

Tab1_uncompcare.df <- Tab1_uncompcare.df[7:25, 1:6 ]
Tab1_totrev.df <- Tab1_totrev.df[7:25, 1:6 ]


Tab1_uncompcare.tab <-  Tab1_uncompcare.df %>% 
  gt() %>% 
  fmt_number(columns = 3:6, 
             decimals = 3) %>% 
  cols_label(
    uncomp_care_combined = 'Total Uncompensated Care'
  ) %>% 
  tab_header(
    title = md('Uncompensated Care by Year'), 
    subtitle = md('in millions USD')
  ) %>% 
  cols_align(align = 'center')
  
Tab1_totrev.tab <-  Tab1_totrev.df %>% 
  gt() %>% 
  fmt_number(columns = 3:6, 
             decimals = 3) %>% 
  cols_label(
    tot_pat_rev = 'Total Revenue'
  ) %>% 
  tab_header(
    title = md('Total Revenue by Year'), 
    subtitle = md('in millions USD')
  ) %>% 
  cols_align(align = 'center')


list.tab1 <- list(Tab1_totrev.tab, Tab1_uncompcare.tab)

Table_1 <- gt_two_column_layout(list.tab1, vwidth = 100)

Table_1

Tab1_uncompcare.tab %>% gtsave(filename = 'TablesFigures/Table_1a.png')
Tab1_totrev.tab %>% gtsave(filename = 'TablesFigures/Table_1b.png')


# 2. Create a figure showing the mean hospital uncompensated care from 2000 to 2018.        ------------------------------------
#      Show this trend separately by hospital ownership type 
#      (private not for profit and private for profit).

# first, create a variable for the ownership code to use
#      gnrl_cntl_type_cd = 02 (private, not for profit)  or   = 04 (private, for profit)

POS_HCRIS_df <- POS_HCRIS_df %>% 
  mutate(ownership_code = ifelse(gnrl_cntl_type_cd == '02', 'Private (not for profit)', 
                                 ifelse(gnrl_cntl_type_cd == '04', 'Private (for profit)', 'Other')))

ls(POS_HCRIS_df)

Plot2_uncompcare.df <- POS_HCRIS_df %>% 
  filter(gnrl_cntl_type_cd == '02' | gnrl_cntl_type_cd == '04') %>% 
  filter(year > 2002 & year != 2022)


Plot2_uncompcare.tab <- summarySE(Plot2_uncompcare.df, measurevar = 'uncomp_care_combined', 
                                  groupvars = c('year', 'ownership_code'), na.rm = TRUE)

Plot2_uncompcare.tab[,c(4:9)] <- Plot2_uncompcare.tab[,c(4:9)]/1000000
Plot2_uncompcare.tab

Plot_2 <- ggplot(Plot2_uncompcare.tab) +
  aes(x = year, y = uncomp_care_combined, color = ownership_code) + 
  geom_errorbar(aes(ymin = uncomp_care_combined-se, ymax = uncomp_care_combined+se)) +
  geom_point() +
  geom_line(size = 1.4) + 
  scale_y_continuous(name = 'Uncompensated Care (millions USD)', n.breaks = 8) +
  scale_x_continuous(name = 'Year', n.breaks = 10) +
  theme_classic() + 
  labs(title = 'Total Uncompensated Care', 
       subtitle = "by Year and Ownership Type", 
       caption = " ") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank(), text = element_text(size = 20)) 
  

Plot_2

ggsave(filename = 'TablesFigures/Plot_2.png', plot = last_plot())





# 3. Using a simple DD identification strategy, estimate the effect of Medicaid expansion   ------------------------------------
#        on hospital uncompensated care using a traditional two-way fixed effects (TWFE)


library(lfe)
library(dplyr)


# first, create an indicator variable that is 1 when a hospital is in a state 
#       that has expanded (either that year or previous years), 0 otherwise

POS_HCRIS_df <- POS_HCRIS_df %>% 
  mutate(expanded_year = as.integer(ifelse(`Status of Medicaid Expansion Decision` == 'Not Adopted', 9999, 
                                `Status of Medicaid Expansion Decision`))) %>% 
  mutate(expanded_indicator = as.factor(ifelse(expanded_year <= year, 1, 0)))

table(POS_HCRIS_df$expanded_indicator)


# next, create the dataframe to use (again, I will focus on 2003-2021 given data constraints)
Q3.df <- POS_HCRIS_df %>% 
  filter(year > 2002 & year < 2022) %>% 
  filter(is.na(uncomp_care_combined) == FALSE) %>% 
  select(year, expanded_indicator, expanded_year, uncomp_care_combined, State, provider_number) %>% 
  filter(State != 'GU' & State != 'VI' & State != 'MP')

Q3.df$uncomp_care_combined <- Q3.df$uncomp_care_combined/1000000


Q3.df$State   <- as.factor(Q3.df$State)
Q3.df$provider_number   <- as.factor(Q3.df$provider_number)
str(Q3.df)

Q3_2.df <- Q3.df %>% 
  filter(expanded_year == 2014 | expanded_year == 9999) 

Q3_3.df <- Q3.df %>% 
  filter(expanded_year == 2015 | expanded_year == 9999) 

Q3_4.df <- Q3.df %>% 
  filter(expanded_year == 2016 | expanded_year == 9999) 


# https://statisticsglobe.com/fixed-effects-linear-regression (good post about TWFE using felm)
Q3.model1 <- felm(uncomp_care_combined ~ expanded_indicator | provider_number + year, data = Q3.df)
Q3.model2 <- felm(uncomp_care_combined ~ expanded_indicator | provider_number + year, data = Q3_2.df)
Q3.model3 <- felm(uncomp_care_combined ~ expanded_indicator | provider_number + year, data = Q3_3.df)
Q3.model4 <- felm(uncomp_care_combined ~ expanded_indicator | provider_number + year, data = Q3_4.df)

summary(Q3.model1)
Q3_1sum <- summary(Q3.model1)$coefficients
Q3_2sum <- summary(Q3.model2)$coefficients
Q3_3sum <- summary(Q3.model3)$coefficients
Q3_4sum <- summary(Q3.model4)$coefficients

Q3.model1$N

Q3.tab <- rbind(data.frame(Q3_1sum), data.frame(Q3_2sum), data.frame(Q3_3sum), data.frame(Q3_4sum))
model_names <- list(c('Model 1', 'Model 2', 'Model 3', 'Model 4'))
model_N <- list(c(Q3.model1$N, Q3.model2$N, Q3.model3$N, Q3.model4$N))
Q3.tab <- cbind(model_names, Q3.tab, model_N) %>% select(-t.value)
Q3.tab

Table_3 <- Q3.tab %>% 
  gt() %>% 
  cols_label(
    `c("Model 1", "Model 2", "Model 3", "Model 4")` = "Model", 
    `c(52819L, 41594L, 21096L, 19296L)` = "N", 
    `Pr...t..` = 'p value'
  ) %>% 
  fmt_number(columns = 2:4, 
             decimals = 2) %>% 
  tab_header(
    title = md('Table: Two Way Fixed Effects Models'), 
    subtitle = md('in millions USD')
  ) %>% 
  cols_align(align = 'center')

Table_3


Table_3 %>% gtsave(filename = 'TablesFigures/Table_3.png')


# 4. Estimate an 'event study' version of the specification in part 3:         ------------------------------------
#  great resource: https://lost-stats.github.io/Model_Estimation/Research_Design/event_study.html 

library(fixest)

# first, create the time to treat variable

str(Q3.df)

Q4.df <- Q3.df %>% 
  mutate(time_to_treat = ifelse(expanded_year < 9999, year - expanded_year, 0)) %>% 
  mutate(expanded_ever = ifelse(expanded_year < 9999, 1, 0))

Q4.df$expanded_ever <- as.factor(Q4.df$expanded_ever)

str(Q4.df)

table(Q4.df$time_to_treat)

# next, run our model 
#    (QUESTION!  do we do the interaction with which dummy variable?  expanded_ever or expanded_year? )
#     seems to me like it should be expanded_ever, but model seems to work better with expanded_year  

# model 1
Q4.model_1 <- feols(uncomp_care_combined ~ i(time_to_treat, expanded_ever, ref = -1) |      # key interaction term, time to treat x ever treated
                    provider_number + year,                                               # fixed effects 
                    cluster = ~State,                                                     # clustered SE (at the state level)
                  data = Q4.df)




# model 2
Q4_2.df <- Q4.df %>% filter(expanded_year == 9999 | expanded_year == 2014)


Q4.model_2 <- feols(uncomp_care_combined ~ i(time_to_treat, expanded_ever, ref = -1) |      # key interaction term, time to treat x ever treated
                      provider_number + year,                                               # fixed effects 
                    cluster = ~State,                                                     # clustered SE (at the state level)
                    data = Q4_2.df)


Q4_1sum <- modelsummary(Q4.model_1)
Q4_2sum <- summary(Q4.model_2)$coefficients

Q4.tab <- etable(Q4.model_1, Q4.model_2)

years_list <- list( 'Dependent Variable', '', -18, -17, -16, -15, -14, -13, -12, -11, -10, -9, -8, -7, -6, -5, -4, -3, -2, 0, 1, 2, 3, 4, 
              'Fixed Effects' , 'Provider Number', 'Year', '', 'SE Clustered', 'Observations', 'R2', 'Within R2' )

Q4.tab <- Q4.tab %>% mutate(col_1 = NA)

i=1
for (i in 1:32) {
  Q4.tab[i,3] <- years_list[[i]]
}

Q4.tab <- tail(Q4.tab, 22)


Table_4 <- Q4.tab %>% 
  gt() %>% 
  cols_label(
    `Q4.model_1` = "Model 1", 
    `Q4.model_2` = "Model 2", 
    `col_1` = 'Year Before Expansion in State'
    ) %>% 
  tab_header(
    title = md('Event Study Estimation'), 
    subtitle = md('')
  ) %>% 
  cols_align(align = 'center') %>% 
  cols_move_to_start(columns = col_1)

Table_4

Table_4 %>% gtsave(filename = 'TablesFigures/Table_4.png')


#5. Re-estimate your event study using the SA (Sun and Abraham) specification        ------------------------------------

Q5.df <- Q4.df %>% filter(expanded_year == 9999 | expanded_year == 2014 | expanded_year == 2015 | expanded_year == 2016)


Q5.model <- feols(uncomp_care_combined ~ sunab(expanded_year, year) |                       # key interaction term, time to treat x ever treated (note the changes from above)
                      provider_number + year,                                               # fixed effects 
                    cluster = ~State,                                                        # clustered SE (at the state level)
                    data = Q5.df)

summary(Q5.model)


summary(Q5.model)$nobs
Q5.tab <- summary(Q5.model)$coeftable
Q5.tab <- data.frame(Q5.tab)
Q5.tab <- Q5.tab %>% select(-t.value)
Year <- list(c(-13, -12, -11, -10, -9, -8, -7, -6, -5, -4, -3, -2, 0, 1, 2, 3, 4))
Q5.tab <- cbind(Year, Q5.tab)
ls(Q5.tab)
names(Q5.tab)[1] <- 'Year'
names(Q5.tab)[3] <- 'Standard Error'
names(Q5.tab)[4] <- 'p value'

Table_5 <- Q5.tab %>% 
  gt() %>% 
  fmt_number(columns = 2:4, 
             decimals = 2) %>% 
  tab_header(
    title = md('Table: Event Study '), 
    subtitle = md('Sun and Abraham method, Expansion Years 2014, 2015, 2016')
  ) %>% 
  cols_align(align = 'center')

Table_5

Table_5 %>% gtsave(filename = 'TablesFigures/Table_5.png')


#6. Present an event study graph based on the results in part 5    ----------------------------------

library(ggiplot)

Plot_6 <- ggiplot(Q5.model, geom_style = 'errorbar',
      xlab = 'Time to Medicaid Expansion', 
      main = 'Event Study, Sun and Abraham method', 
      sub = 'Includes expansion Years 2014, 2015, 2016') + theme_classic()

Plot_6

ggsave(filename = 'TablesFigures/Plot_6.png', plot = last_plot())

#7. Callaway and Sant'Anna (CS) offer a non-parametric solution that effectively calculates a set of group-time specific differences.

library(did)

Q7.df <- Q4.df

table(Q7.df$expanded_year)

ls(Q7.df)

str(Q7.df)
Q7.df$provider_number <- as.numeric(Q7.df$provider_number)

Q7.model <- att_gt(yname = 'uncomp_care_combined', 
                   tname = 'year', 
                   idname = 'provider_number', 
                   gname = 'expanded_year', 
                   control_group = 'notyettreated',
                   data = Q7.df
                   )

Q7.table <- modelsummary(Q7.model, 
             estimate = '{estimate}  ({std.error})', 
             statistic = NULL, 
             output = 'gt')

Q7.table %>% 
  tab_header(
    title = md('Callaway and SantAnna'), 
    subtitle = md('')
  ) %>% 
  cols_align(align = 'center') %>% 
  cols_label(`Model 1` = 'Coefficient  (Std Err)')

ggdid(Q7.model)

Q7.model.event_study <- aggte(Q7.model, type = 'dynamic')

Q7.table.event_study <- modelsummary(Q7.model.event_study, 
                         estimate = '{estimate}  ({std.error})', 
                         statistic = NULL, 
                         output = 'gt')

Q7.table.event_study %>% 
  tab_header(
    title = md('Callaway and SantAnna'), 
    subtitle = md('')
  ) %>% 
  cols_align(align = 'center') %>% 
  cols_label(`Model 1` = 'Coefficient  (Std Err)')

Plot_7 <- ggdid(Q7.model.event_study)

Plot_7

ggsave(filename = 'TablesFigures/Plot_7.png', plot = last_plot())


#8. Rambachan and Roth (RR) show that traditional tests of parallel pre-trends may be underpowered, 
#   and they provide an alternative estimator that essentially bounds the treatment effects by the 
#   size of an assumed violation in parallel trends.  

library(HonestDiD)

## -----------------------------------------------------------------------------

# from  https://github.com/pedrohcgs/CS_RR 

## -----------------------------------------------------------------------------

#' @title honest_did
#'
#' @description a function to compute a sensitivity analysis
#'  using the approach of Rambachan and Roth (2021)
#' @param es an event study
honest_did <- function(es, ...) {
  UseMethod("honest_did", es)
}


#' @title honest_did.AGGTEobj
#'
#' @description a function to compute a sensitivity analysis
#'  using the approach of Rambachan and Roth (2021) when
#'  the event study is estimating using the `did` package
#'
#' @param e event time to compute the sensitivity analysis for.
#'  The default value is `e=0` corresponding to the "on impact"
#'  effect of participating in the treatment.
#' @param type Options are "smoothness" (which conducts a
#'  sensitivity analysis allowing for violations of linear trends
#'  in pre-treatment periods) or "relative_magnitude" (which
#'  conducts a sensitivity analysis based on the relative magnitudes
#'  of deviations from parallel trends in pre-treatment periods).
#' @inheritParams HonestDiD::createSensitivityResults
#' @inheritParams HonestDid::createSensitivityResults_relativeMagnitudes
honest_did.AGGTEobj <- function(es,
                                e=0,
                                type=c("smoothness", "relative_magnitude"),
                                method=NULL,
                                bound="deviation from parallel trends",
                                Mvec=NULL,
                                Mbarvec=NULL,
                                monotonicityDirection=NULL,
                                biasDirection=NULL,
                                alpha=0.05,
                                parallel=FALSE,
                                gridPoints=10^3,
                                grid.ub=NA,
                                grid.lb=NA,
                                ...) {
  
  
  type <- type[1]
  
  # make sure that user is passing in an event study
  if (es$type != "dynamic") {
    stop("need to pass in an event study")
  }
  
  # check if used universal base period and warn otherwise
  if (es$DIDparams$base_period != "universal") {
    warning("it is recommended to use a universal base period for honest_did")
  }
  
  # recover influence function for event study estimates
  es_inf_func <- es$inf.function$dynamic.inf.func.e
  
  # recover variance-covariance matrix
  n <- nrow(es_inf_func)
  V <- t(es_inf_func) %*% es_inf_func / (n*n) 
  
  
  nperiods <- nrow(V)
  npre <- sum(1*(es$egt < 0))
  npost <- nperiods - npre
  
  baseVec1 <- basisVector(index=(e+1),size=npost)
  
  orig_ci <- constructOriginalCS(betahat = es$att.egt,
                                 sigma = V, numPrePeriods = npre,
                                 numPostPeriods = npost,
                                 l_vec = baseVec1)
  
  if (type=="relative_magnitude") {
    if (is.null(method)) method <- "C-LF"
    robust_ci <- createSensitivityResults_relativeMagnitudes(betahat = es$att.egt, sigma = V, 
                                                             numPrePeriods = npre, 
                                                             numPostPeriods = npost,
                                                             bound=bound,
                                                             method=method,
                                                             l_vec = baseVec1,
                                                             Mbarvec = Mbarvec,
                                                             monotonicityDirection=monotonicityDirection,
                                                             biasDirection=biasDirection,
                                                             alpha=alpha,
                                                             gridPoints=100,
                                                             grid.lb=-1,
                                                             grid.ub=1,
                                                             parallel=parallel)
    
  } else if (type=="smoothness") {
    robust_ci <- createSensitivityResults(betahat = es$att.egt,
                                          sigma = V, 
                                          numPrePeriods = npre, 
                                          numPostPeriods = npost,
                                          method=method,
                                          l_vec = baseVec1,
                                          monotonicityDirection=monotonicityDirection,
                                          biasDirection=biasDirection,
                                          alpha=alpha,
                                          parallel=parallel)
  }
  
  list(robust_ci=robust_ci, orig_ci=orig_ci, type=type)
}

#relative magnitute
Q8.model <- honest_did(Q7.model.event_study, type = 'relative_magnitude')
Q8.model$robust_ci <- Q8.model$robust_ci[-1,]
Q8.plot <- createSensitivityPlot_relativeMagnitudes(Q8.model$robust_ci, Q8.model$orig_ci)
Q8.plot
png(file = 'TablesFigures/Q8.plot.png')
plot(Q8.plot)
dev.off()

#smoothness (I couldn't get this one to work...)
Q8.model_smooth <- honest_did(Q7.model.event_study, type = 'smoothness')
Q8_smooth.plot <- createSensitivityPlot(Q8.model_smooth$robust_ci, Q8.model$orig_ci)
Q8_smooth.plot

