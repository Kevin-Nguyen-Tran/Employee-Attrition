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


