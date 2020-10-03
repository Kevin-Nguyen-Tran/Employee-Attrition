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
# facet wrap over the strong negative correlation

#=============================================================================================================================================================================================================================
# 1. PROPORTIONS OF EMPLOYEES WHO LEFT
#=============================================================================================================================================================================================================================

ggplot(employee_data, aes(x = Attrition)) +
  geom_bar(position = "stack")

employee_data %>%
  group_by(Attrition) %>%
  summarise(n = n()) # Within the data set, 1233 have stayed with their job, 237 have left
# This data set seems to be in favor of those who stay at work. However, we will analyze those who left and see what variables are good predicators
# First we will run a Pearson's Correlation - heat Map and see which variables have a positive, neutral, or negative correlation and explore each group


#=============================================================================================================================================================================================================================
# 2. PEARSON'S CORRELATION - HEAT MAP
#=============================================================================================================================================================================================================================

corr_data <- employee_data %>%
  mutate(attrition = ifelse(Attrition == "No", 0, 1),
         gender = ifelse(Gender == "Female", 0, 1),
         overtime = ifelse(OverTime == "No", 0, 1)) %>%
  select(Age, attrition, DistanceFromHome, EnvironmentSatisfaction, gender, HourlyRate, JobSatisfaction, PercentSalaryHike,  overtime, TotalWorkingYears, WorkLifeBalance, YearsAtCompany:YearsWithCurrManager)


ggcorrplot(cor(corr_data), hc.order = TRUE, lab = TRUE, lab_size = 2) 
# based on this correlation plot, we are more interested in how attrition fares with the other employee factors
# Can separate by positive corr and negative correlation
# Positive corr: overtime
# neutral corr (+- less than .1): hourly rate, distance from home, percent salary hike, gender, years since last promotion, work life balance.
# Negative corr: job satisfaction, age, total working years, years in current role, years at company, years with current manager, and environment sat

# This will be our overhead analysis and we will now dive a bit deeper into each and see how it correlates and if it matches our linear regression model.

cor(corr_data)

#=============================================================================================================================================================================================================================
# 3. POSITIVE CORRELATION TO ATTRITION
#=============================================================================================================================================================================================================================

# This means that, as each variable increases, so does attrition.
# Within our data set, overtime is the only variable that showed a positive correlation with attrition.
# As more people work overtime, there is an increase in attrition.

ggplot(employee_data, aes(x = OverTime, fill = Attrition)) +
  geom_bar(position = "fill")

# This bar chart tells us, that there is more attrition in those that decide to work overtime vs those that do not. However, is this difference in attrition statistically significant?

t.test(corr_data$overtime ~ corr_data$attrition, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)
# Null Hypothesis: states that there is no significant difference between the attrition of those who work and do not work overtime.
# Do to our p-value being less than 5% and the confidence interval does not include zero, we can reject the null hypothesis with 95% confidence and assume that there is a statisitcally significant difference in attrition of those who do and do not work overtime.

#=============================================================================================================================================================================================================================
# 4. NEUTRAL CORRELATION TO ATTRITION
#=============================================================================================================================================================================================================================
# neutral corr (+- less than .1): hourly rate, distance from home, percent salary hike, gender, years since last promotion, work life balance.


hr_att <- ggplot(employee_data, aes(x = Attrition, y = HourlyRate)) +
  geom_boxplot()

dist_att <- ggplot(employee_data, aes(x = Attrition, y = DistanceFromHome)) +
  geom_boxplot()

perc_att <- ggplot(employee_data, aes(x = Attrition, y = PercentSalaryHike)) +
  geom_boxplot()

years_att <- ggplot(employee_data, aes(x = Attrition, y = YearsSinceLastPromotion)) +
  geom_boxplot()

work_att <- ggplot(employee_data, aes(x = Attrition, y = WorkLifeBalance)) +
  geom_boxplot()

multiplot(hr_att, dist_att, perc_att, years_att, work_att, cols = 2)

ggplot(employee_data, aes(x = Gender, fill = Attrition)) +
  geom_bar(position = "fill")

# As we can see, the average of each variable are similar regardless of attrition status.
t.test(corr_data$HourlyRate ~ corr_data$attrition, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)
t.test(corr_data$DistanceFromHome ~ corr_data$attrition, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)
t.test(corr_data$PercentSalaryHike ~ corr_data$attrition, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)
t.test(corr_data$YearsSinceLastPromotion ~ corr_data$attrition, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)
t.test(corr_data$WorkLifeBalance ~ corr_data$attrition, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)
t.test(corr_data$gender ~ corr_data$attrition, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)

# hourly rate, percent salary hike, years since last promotion, and gender all show no significant difference in those who did and did not leave their job.
# Surprisingly, Distance from Home and Work Life Balance showed a statistically significant positive and negative difference between those who did and did not leave their jobs, respectively.
# according to the heat map, the distance from home had a 0.08 correlation and Work Life Balance had a -0.06 correlation.
# Therefore, we can reduce our window to +- 0.05 for the neutral window from +-0.1. This will accurately group our variables.

#=============================================================================================================================================================================================================================
# 5. NEGATIVE CORRELATION TO ATTRITION
#=============================================================================================================================================================================================================================
# Negative corr: job satisfaction, age, total working years, years in current role, years at company, years with current manager, and environment sat
# We will explore the negative correlation, which shows that as this variable increases, the likelihood of attrition will decrease!
# these are the variables that retain employees as it progresses!

ggplot(employee_data, aes(x = YearsInCurrentRole, y = YearsWithCurrManager)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(~ Attrition, nrow = 2) # might use, not sure yet

job_att <- ggplot(employee_data, aes(x = Attrition, y = JobSatisfaction)) +
  geom_boxplot()

age_att <- ggplot(employee_data, aes(x = Attrition, y = Age)) +
  geom_boxplot()

worky_att <- ggplot(employee_data, aes(x = Attrition, y = TotalWorkingYears)) +
  geom_boxplot()

yearscurr_att <- ggplot(employee_data, aes(x = Attrition, y = YearsInCurrentRole)) +
  geom_boxplot()

yearscomp_att <- ggplot(employee_data, aes(x = Attrition, y = YearsAtCompany)) +
  geom_boxplot()

yearsmgr_att <- ggplot(employee_data, aes(x = Attrition, y = YearsWithCurrManager)) +
  geom_boxplot()

env_att <- ggplot(employee_data, aes(x = Attrition, y = EnvironmentSatisfaction)) +
  geom_boxplot()

multiplot(job_att, age_att, worky_att, yearscurr_att, yearscomp_att, yearsmgr_att, env_att, cols = 2)

jobsat_t <- t.test(corr_data$JobSatisfaction ~ corr_data$attrition, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)
age_t <- t.test(corr_data$Age ~ corr_data$attrition, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)
workyrs_t <- t.test(corr_data$TotalWorkingYears ~ corr_data$attrition, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)
yrsrole_t <- t.test(corr_data$YearsInCurrentRole ~ corr_data$attrition, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)
yrscomp_t <- t.test(corr_data$YearsAtCompany ~ corr_data$attrition, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)
yrsmgr_t <- t.test(corr_data$YearsWithCurrManager ~ corr_data$attrition, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)
envsat_t <- t.test(corr_data$EnvironmentSatisfaction ~ corr_data$attrition, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)

# All t.tests above show a significant difference between the average of each variable in respect to attrition. 
# Therefore we can say with 95% confidence that each variable has a signicantly negative correlation with attrition.
# To retain employees, they are more satisfied with their job, have had more tenure in working experience, at the company, with current manager, and is satisfied with their environment.
# If we were to choose areas of focus, we would look at the p-values and see which one had the smallest and make those areas of focus, since the probability of retention would be greater

tribble(
  ~name, ~p.value,
  "Job Satisfaction", jobsat_t$p.value,
  "Age", age_t$p.value,
  "Total Working Years", workyrs_t$p.value,
  "Years In Current Role", yrsrole_t$p.value,
  "Years at Company", yrscomp_t$p.value,
  "Years with Current Mgr", yrsmgr_t$p.value,
  "Environment Satisfaction", envsat_t$p.value
)
# As shown in the table above, the p-value is smallest for Years In Current Role, Total Working Years, and years with current Manager.
# All are statistically significant, however, if we were to focus on a few variables and how we can retain employees, we need to ensure that we acquire tenured professionals, both in work experience and experience in their role, as well as maintain a good employee to manager relationship.

#=============================================================================================================================================================================================================================
# 6. LOGISTIC LINEAR REGRESSION - BINOMIAL
#=============================================================================================================================================================================================================================

predicted <- glm(attrition ~ ., family = "binomial", data = corr_data)
summary(predicted)

# According to this logistic linear regression model, it predicts the probability of attrition based on the given variables
# each variable is analyzed and categorized (with small p-values), as either a good or bad predictor of attrition
# based on the summary, Age, Distance from Home, Environment satisfaction, job satisfaction, overtime, total working years, work life balance, years in current role, years since last promotion and years with current manager are all good predictors.
# keep in mind, these good predictors are determined by their relationship to one another towards attrition












