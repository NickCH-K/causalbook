
library(psych)
library(ggplot2)
library(ggpubr)
library(extrafont)
library(sensemakr)
library(tidyverse)
library(sjlabelled)

################################################################################
###DRUGS########################################################################
################################################################################

drug2 = read.csv("2012_Q234/DRUG_2012Q234_NEW.csv")
drug3 = read.csv("2013/DRUGS_2013_NEW.csv")
drug4 = read.csv("2014/DRUGS_2014_NEW.csv")

#Merging the datasets
#Identifying variables that are not repeated in every dataset
table(c(names(drug2), names(drug3), names(drug4)))

#The merging
drug = rbind(drug2, drug3, drug4)
names(drug) = names(drug2)

#Selection of the most common types of drugs
table(drug$OFFENCE, useNA = "ifany")
#Removing categories with a small sample size
drug = drug[which(drug$OFFENCE!="Other drugs offences"),]
drug = drug[which(drug$OFFENCE!="Conspiracy to supply"),]

##I get rid of cases where no sentence outcome is defined
table(drug$OUTCOME, useNA = "ifany")
drug = drug[which(drug$OUTCOME!="Other"),]

#Creating a dummy for custody or not
drug$cust = ifelse(drug$OUTCOME=="Immediate custody", 1, 0)

#Recoding first opportunity
drug$first = ifelse(drug$GP_FIRST_OPP=="Yes", 1, 0)

#Removing missing cases for previous convictions
drug = drug[which(drug$PREV_CONVICTIONS!="Not Answered"&drug$PREV_CONVICTIONS!="Not answered"),]

#Removing missing cases for DRG_CLASS and DRG_CULPABILITY
drug = drug[which(drug$DRG_CLASS!="Not Answered"),]
drug = drug[which(drug$DRG_CULPABILITY!="Not Answered"&drug$DRG_CULPABILITY!="Not Asked"),]

#Recoding DRG_REDUCING_SER_7
drug$DRG_REDUCING_SER_7 = ifelse(drug$DRG_REDUCING_SER_7=="-", "-", "Offender's vulnerability exploited")


#Removing variables not to be used in the model
summary(drug)
drug$UNIQUE_ID = drug$MONTH = drug$LENGTH = drug$OUTCOME = drug$LENGTH = drug$DRG_CATEGORY = 
  drug$GUILT_IND_POLICE = drug$GP_STAGE = drug$GP_DISCOUNT = drug$GP_FIRST_OPP = drug$DRG_INCREASING_SER_STAT_NONE = 
  drug$DRG_INCREASING_SER_OTHER_NONE = drug$DRG_REDUCING_SER_NONE = DRG_INCREASING_SER_OTHER_16 = NULL
summary(drug)

#Reordering previous convicitons so no previous convictions is the reference
drug$PREV_CONVICTIONS = factor(drug$PREV_CONVICTIONS)  #This is to remove unused labels
print(levels(drug$PREV_CONVICTIONS))  ## This will show the levels of the factor
drug$PREV_CONVICTIONS = factor(drug$PREV_CONVICTIONS, levels(drug$PREV_CONVICTIONS)[c(4,1,3,2)])  #To reorder the levels


#The logit models
#The naive model 
naivecust = glm(cust ~ AGE + GENDER, data = drug, family = "binomial")
summary(naivecust)

#The full model 
fullcust = glm(cust ~ AGE + GENDER + PREV_CONVICTIONS + DRG_CLASS + DRG_CULPABILITY + DRG_INCREASING_SER_STAT_2 +
                 DRG_INCREASING_SER_STAT_3 + DRG_INCREASING_SER_OTHER_1 + DRG_INCREASING_SER_OTHER_3 + DRG_INCREASING_SER_OTHER_3 +
                 DRG_INCREASING_SER_OTHER_4 + DRG_INCREASING_SER_OTHER_5 + DRG_INCREASING_SER_OTHER_6 + DRG_INCREASING_SER_OTHER_7 +
                 DRG_INCREASING_SER_OTHER_8 + DRG_INCREASING_SER_OTHER_9 + DRG_INCREASING_SER_OTHER_10 + DRG_INCREASING_SER_OTHER_11 +
                 DRG_INCREASING_SER_OTHER_12 + DRG_INCREASING_SER_OTHER_13 + DRG_INCREASING_SER_OTHER_14 + DRG_INCREASING_SER_OTHER_15 +
                 DRG_INCREASING_SER_OTHER_17 + DRG_INCREASING_SER_OTHER_18 + DRG_INCREASING_SER_OTHER_19 + DRG_INCREASING_SER_OTHER_20 +
                 DRG_INCREASING_SER_OTHER_21 + DRG_REDUCING_SER_1 + DRG_REDUCING_SER_2 + DRG_REDUCING_SER_3 + DRG_REDUCING_SER_4 +
                 DRG_REDUCING_SER_5 + DRG_REDUCING_SER_6 + DRG_REDUCING_SER_7 + DRG_REDUCING_SER_8 + DRG_REDUCING_SER_9 +  
                 DRG_REDUCING_SER_10 + DRG_REDUCING_SER_11 + DRG_REDUCING_SER_12 + DRG_REDUCING_SER_13 + DRG_REDUCING_SER_14 +
                 DRG_REDUCING_SER_15 + DRG_REDUCING_SER_16 + first + OFFENCE, data = drug, family = "binomial")
summary(fullcust)


drug$male = 1*(drug$GENDER == 'Male')

library(tidyverse)
drug <- drug %>%
  rename(custody = cust,
            first_offense = first,
         offense = OFFENCE,
         age = AGE,
         prev_convictions = PREV_CONVICTIONS,
         drg_class = DRG_CLASS,
         drg_culpability = DRG_CULPABILITY)  %>%
  select(-GENDER, -DRG_HARM,
         -DRG_INCREASING_SER_STAT_1,
         -DRG_INCREASING_SER_STAT_4,
         -DRG_INCREASING_SER_OTHER_2,
         -DRG_INCREASING_SER_OTHER_16)
drug <- drug %>%
  mutate(male = set_label(male, 'Is a male'),
         first_offense = set_label(first_offense, 'First offense'),
         prev_convictions = set_label(prev_convictions, 'Previous convictions, in bins of None, 1-3, 4-9, or 10+'),
         offense = set_label(offense, 'Offense type'),
         age = set_label(age, 'Age in ten-year bins'),
         custody = set_label(custody, 'Taken in to custody'),
         drg_class = set_label(drg_class, 'Type of drug'),
         drg_culpability = set_label(drg_culpability, 'Level of culpability for crime'))

drgcols = c('DRG_INCREASING_SER_STAT_2', 'DRG_INCREASING_SER_STAT_3', 'DRG_INCREASING_SER_OTHER_1', 'DRG_INCREASING_SER_OTHER_3', 'DRG_INCREASING_SER_OTHER_4', 'DRG_INCREASING_SER_OTHER_5', 'DRG_INCREASING_SER_OTHER_6', 'DRG_INCREASING_SER_OTHER_7', 'DRG_INCREASING_SER_OTHER_8', 'DRG_INCREASING_SER_OTHER_9', 'DRG_INCREASING_SER_OTHER_10', 'DRG_INCREASING_SER_OTHER_11', 'DRG_INCREASING_SER_OTHER_12', 'DRG_INCREASING_SER_OTHER_13', 'DRG_INCREASING_SER_OTHER_14', 'DRG_INCREASING_SER_OTHER_15', 'DRG_INCREASING_SER_OTHER_17', 'DRG_INCREASING_SER_OTHER_18', 'DRG_INCREASING_SER_OTHER_19', 'DRG_INCREASING_SER_OTHER_20', 'DRG_INCREASING_SER_OTHER_21', 'DRG_REDUCING_SER_1', 'DRG_REDUCING_SER_2', 'DRG_REDUCING_SER_3', 'DRG_REDUCING_SER_4', 'DRG_REDUCING_SER_5', 'DRG_REDUCING_SER_6', 'DRG_REDUCING_SER_7', 'DRG_REDUCING_SER_8', 'DRG_REDUCING_SER_9', 'DRG_REDUCING_SER_10', 'DRG_REDUCING_SER_11', 'DRG_REDUCING_SER_12', 'DRG_REDUCING_SER_13', 'DRG_REDUCING_SER_14', 'DRG_REDUCING_SER_15', 'DRG_REDUCING_SER_16')


for (col in drgcols) {
  uvals = unique(drug[[col]])
  titl = uvals[uvals != '-']
  if (length(titl) != 1) {
    stop(col)
  }
  titl = ifelse(str_sub(col,5,5) == 'I',
                paste0('Sentence-increasing indicator: ', titl),
                paste0('Sentence-reducing indicator: ', titl))
  drug[col] = set_label(1*(drug[[col]] != '-'), titl)
  names(drug)[names(drug) == col] = str_to_lower(col)
}

drug <- drug %>%
  relocate(custody, male, first_offense)
save(drug, file = 'drug.rda')
haven::write_dta(drug, 'ccdrug.dta')

# AND NOW THE ACTUAL RESULT



fullcust = lm(custody ~ AGE + GENDER + PREV_CONVICTIONS + DRG_CLASS + DRG_CULPABILITY + DRG_INCREASING_SER_STAT_2 +
                 DRG_INCREASING_SER_STAT_3 + DRG_INCREASING_SER_OTHER_1 + DRG_INCREASING_SER_OTHER_3 + DRG_INCREASING_SER_OTHER_3 +
                 DRG_INCREASING_SER_OTHER_4 + DRG_INCREASING_SER_OTHER_5 + DRG_INCREASING_SER_OTHER_6 + DRG_INCREASING_SER_OTHER_7 +
                 DRG_INCREASING_SER_OTHER_8 + DRG_INCREASING_SER_OTHER_9 + DRG_INCREASING_SER_OTHER_10 + DRG_INCREASING_SER_OTHER_11 +
                 DRG_INCREASING_SER_OTHER_12 + DRG_INCREASING_SER_OTHER_13 + DRG_INCREASING_SER_OTHER_14 + DRG_INCREASING_SER_OTHER_15 +
                 DRG_INCREASING_SER_OTHER_17 + DRG_INCREASING_SER_OTHER_18 + DRG_INCREASING_SER_OTHER_19 + DRG_INCREASING_SER_OTHER_20 +
                 DRG_INCREASING_SER_OTHER_21 + DRG_REDUCING_SER_1 + DRG_REDUCING_SER_2 + DRG_REDUCING_SER_3 + DRG_REDUCING_SER_4 +
                 DRG_REDUCING_SER_5 + DRG_REDUCING_SER_6 + DRG_REDUCING_SER_7 + DRG_REDUCING_SER_8 + DRG_REDUCING_SER_9 +  
                 DRG_REDUCING_SER_10 + DRG_REDUCING_SER_11 + DRG_REDUCING_SER_12 + DRG_REDUCING_SER_13 + DRG_REDUCING_SER_14 +
                 DRG_REDUCING_SER_15 + DRG_REDUCING_SER_16 + first + OFFENCE, data = drug)

model.dx = lm(Male ~ AGE + PREV_CONVICTIONS + DRG_CLASS + DRG_CULPABILITY + DRG_INCREASING_SER_STAT_2 +
               DRG_INCREASING_SER_STAT_3 + DRG_INCREASING_SER_OTHER_1 + DRG_INCREASING_SER_OTHER_3 + DRG_INCREASING_SER_OTHER_3 +
               DRG_INCREASING_SER_OTHER_4 + DRG_INCREASING_SER_OTHER_5 + DRG_INCREASING_SER_OTHER_6 + DRG_INCREASING_SER_OTHER_7 +
               DRG_INCREASING_SER_OTHER_8 + DRG_INCREASING_SER_OTHER_9 + DRG_INCREASING_SER_OTHER_10 + DRG_INCREASING_SER_OTHER_11 +
               DRG_INCREASING_SER_OTHER_12 + DRG_INCREASING_SER_OTHER_13 + DRG_INCREASING_SER_OTHER_14 + DRG_INCREASING_SER_OTHER_15 +
               DRG_INCREASING_SER_OTHER_17 + DRG_INCREASING_SER_OTHER_18 + DRG_INCREASING_SER_OTHER_19 + DRG_INCREASING_SER_OTHER_20 +
               DRG_INCREASING_SER_OTHER_21 + DRG_REDUCING_SER_1 + DRG_REDUCING_SER_2 + DRG_REDUCING_SER_3 + DRG_REDUCING_SER_4 +
               DRG_REDUCING_SER_5 + DRG_REDUCING_SER_6 + DRG_REDUCING_SER_7 + DRG_REDUCING_SER_8 + DRG_REDUCING_SER_9 +  
               DRG_REDUCING_SER_10 + DRG_REDUCING_SER_11 + DRG_REDUCING_SER_12 + DRG_REDUCING_SER_13 + DRG_REDUCING_SER_14 +
               DRG_REDUCING_SER_15 + DRG_REDUCING_SER_16 + first + OFFENCE, data = drug)

covnames = model.dx$coefficients |> names() 
covnames = covnames[2:length(covnames)]
r2yx.d <- group_partial_r2(fullcust, covariates = covnames)
r2dx   <- group_partial_r2(model.dx, covariates = covnames)

informal_adjusted_estimate <- adjusted_estimate(fullcust, 
                                                treatment = "GENDERMale", 
                                                r2dz.x = r2dx, 
                                                r2yz.dx = r2yx.d)

formal_bound <- ovb_bounds(model = fullcust,
                           treatment = 'GENDERMale',
                           benchmark_covariates = list('Total Adjustment' = covnames),
                           kd = 1, ky = 1)

library(extrafont)
font_import()
loadfonts()
ovb_contour_plot(fullcust, treatment = 'GENDERMale', lim = .9, lim.y = .9,
                 family = 'Garamond')
add_bound_to_contour(bounds = formal_bound, 
                     bound_label = 'Proper Bound')


library(ggpubr)
library(extrafont)
library(sensemakr)
library(tidyverse)

cc <- causaldata::ccdrug

# The . means "include all the variables you haven't
# mentioned yet" which is just custody
m <- lm(custody ~ ., data = cc)

#m <- lm(male ~ ., data = cc %>% select(-custody))

# The full set of covariates, everything but intercept and treatment
all_covs <- m %>% coef() %>% names()
all_covs <- all_covs[3:length(all_covs)]

# Create bounds with strength similar to different covariates
all_bounds <- ovb_bounds(m, treatment = 'male',
                        benchmark_covariates = list(
                          'All Covariates' = all_covs,
                          'Culpability' = all_covs[6:8],
                          'Drug Class' = all_covs[12:16]))

library(Cairo)
cairo_pdf('drugsensitivity.pdf', width = 5, height = 5,
          family = 'Garamond')
# Make a graph, with a big range for the "All Covariates" adjustment
ovb_contour_plot(m, treatment = 'male', 
                 lim = .3, lim.y = 1,
                 label.bump.x = .03,
                 label.bump.y = .01,
                 family = 'Garamond',
                 col.thr.line = 'black',
                 cex.label = .6,
                 list.par = list('cex' = 1.1,
                                 'pty' = 's'))
# And add our adjusted estimates
add_bound_to_contour(bounds = all_bounds, point.bg = 'black',
                     cex.label = .7)
dev.off()

cairo_pdf('extremedrug.pdf', width = 6, height = 5,
          family = 'Garamond')
ovb_extreme_plot(m, treatment = 'male', 
                 family = 'Garamond',
                 col = 'black',
                 list.par = list(col = 'black',
                                 family = 'Garamond',
                                 cex = 1.3),
                 lim = .1)
dev.off()

