#=============================================================================================================================================================================================================================
# PREREQUISITES, READING IN DATA SET, AND TIDYING DATA
#=============================================================================================================================================================================================================================

# PREREQS:
rm(list = ls()) #removes all variables stored previously in Environment (good habit to include)

library(Rmisc) # transform integers to factors, must be first or will mask other packages. We only need multiplot!
library(lubridate) #to modify date-time entries, if you do not have it: install.packages("lubridate")
library(scales) #allows to modify scientific notation for values
library(dplyr)
library(wesanderson)
library(tidyverse) #Run tidyverse, if you do not have: install.packages("tidyverse")
library(ggcorrplot) # pearon's correlation and heatmaps

# URL: https://www.kaggle.com/pavansubhasht/ibm-hr-analytics-attrition-dataset

#READ DATA SET AND STORE INTO OBJECT:
data <- read.csv("C:/Users/Kevin/Desktop/Employee-Attrition #7/WA_Fn-UseC_-HR-Employee-Attrition.csv")

employee_data <- as.tibble(data)

#Education
# 'Below College'
# 'College'
# 'Bachelor'
# 'Master'
# 'Doctor'

#EnvironmentSatisfaction
# 'Low'
# 'Medium'
# 'High'
# 'Very High'

#JobInvolvement
# 'Low'
# 'Medium'
# 'High'
# 'Very High'

#JobSatisfaction
# 'Low'
# 'Medium'
# 'High'
# 'Very High'

#PerformanceRating
# 'Low'
# 'Good'
# 'Excellent'
# 'Outstanding'

#RelationshipSatisfaction
# 'Low'
# 'Medium'
# 'High'
# 'Very High'

#WorkLifeBalance
# 'Bad'
# 'Good'
# 'Better'
# 'Best'

# NEED TO TIDY DATA SET:
summary(employee_data)
str(employee_data)

employee_data <- rename(employee_data, Age = ï..Age)

#=============================================================================================================================================================================================================================
# ANALYSIS & INITIAL BRAINSTORM
#=============================================================================================================================================================================================================================

# Potential research and analysis opportunities: (**) = green light analysis & (xx) = red light (cannot do it)
# Use a heat map to see the highest correlation between certain variables.We can group the variables together with positive and negative correlation.
# Analyze the variables together and try to utilize facet wrapping

#=============================================================================================================================================================================================================================
# 1. PEARSON'S CORRELATION - HEAT MAP
#=============================================================================================================================================================================================================================

corr_data <- employee_data %>%
  mutate(attrition = ifelse(Attrition == "No", 0, 1),
         gender = ifelse(Gender == "Female", 0, 1),
         overtime = ifelse(OverTime == "No", 0, 1)) %>%
  select(Age, attrition, DistanceFromHome, EnvironmentSatisfaction, gender, HourlyRate, JobSatisfaction, PercentSalaryHike,  overtime, TotalWorkingYears, WorkLifeBalance, YearsAtCompany:YearsWithCurrManager)


ggcorrplot(cor(corr_data), hc.order = TRUE, lab = TRUE, lab_size = 3) 
# based on this correlation plot, we are more interested in how outcome fares with the other numeric variables.
# Pregnancy, age, diabetes pedigree function, and insulin have a slightly positive correlation with outcome
# Glucose and BMI have a strong positive correlation with Outcome.
# Blood pressure and skin thickness does not appear to show either a positive or negative correlation with Outcome.
# This will be our overhead analysis and we will now dive a bit deeper into each and see how it correlates and if it matches our linear regression model.

cor(corr_data)

































