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
  select(Age, attrition, DistanceFromHome, Education, NumCompaniesWorked, EnvironmentSatisfaction, gender, HourlyRate, JobSatisfaction, PercentSalaryHike,  overtime, TotalWorkingYears, WorkLifeBalance, YearsAtCompany:YearsWithCurrManager)


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

numcomp_att <- ggplot(employee_data, aes(x = Attrition, y = NumCompaniesWorked)) +
  geom_boxplot()

edu_att <- ggplot(employee_data, aes(x = Attrition, y = Education)) +
  geom_boxplot()

multiplot(hr_att, dist_att, perc_att, years_att, work_att, numcomp_att, edu_att, cols = 2)

ggplot(employee_data, aes(x = Gender, fill = Attrition)) +
  geom_bar(position = "fill")

# As we can see, the average of each variable are similar regardless of attrition status.
hourrate_t <- t.test(corr_data$HourlyRate ~ corr_data$attrition, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)
disthome_t <- t.test(corr_data$DistanceFromHome ~ corr_data$attrition, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)
percsalhike_t <- t.test(corr_data$PercentSalaryHike ~ corr_data$attrition, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)
yrslastpromo_t <- t.test(corr_data$YearsSinceLastPromotion ~ corr_data$attrition, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)
wrklifebal_t <- t.test(corr_data$WorkLifeBalance ~ corr_data$attrition, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)
gender_t <- t.test(corr_data$gender ~ corr_data$attrition, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)
numcompworked_t <- t.test(corr_data$NumCompaniesWorked ~ corr_data$attrition, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)
edu_t <-  t.test(corr_data$Education ~ corr_data$attrition, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)

tribble(
  ~name, ~p.value,
  "Hourly Rate", hourrate_t$p.value,
  "Distance From Home", disthome_t$p.value,
  "Percent Salary Hike", percsalhike_t$p.value,
  "Years Since Last Promotion", yrslastpromo_t$p.value,
  "Work Life Balance", wrklifebal_t$p.value,
  "Gender", gender_t$p.value,
  "Number of Companies Worked", numcompworked_t$p.value,
  "Education", edu_t$p.value
)

# hourly rate, percent salary hike, years since last promotion, Number of Companies Worked, education and gender all show no significant difference in those who did and did not leave their job.
# Surprisingly, Distance from Home and Work Life Balance showed a statistically significant (Less than 5%) positive and negative difference between those who did and did not leave their jobs, respectively.
# according to the heat map, the distance from home had a 0.08 correlation and Work Life Balance had a -0.06 correlation.
# Therefore, we can reduce our window to +- 0.05 for the neutral window from +-0.1. This will accurately group our variables.

#=============================================================================================================================================================================================================================
# 5. NEGATIVE CORRELATION TO ATTRITION
#=============================================================================================================================================================================================================================
# Negative corr: job satisfaction, age, total working years, years in current role, years at company, years with current manager, and environment sat
# We will explore the negative correlation, which shows that as this variable increases, the likelihood of attrition will decrease!
# these are the variables that retain employees as it progresses!

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


multiplot(job_att, age_att, worky_att, yearscurr_att, yearscomp_att, yearsmgr_att, env_att, cols = 3)

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
# It is interesting to see that Numbers of Companies Worked and Years Since Last Promotion is statistically significant and a good predictor of attrition when it showed no average difference (noted by the t.test and heat map) in earlier sections
# It is also surprising to see that years at company is also not statistically significant and a good predictor of attrition despite the t.test and heat map stating there was a significant difference.

#=============================================================================================================================================================================================================================
# 7. CATEGORICAL VARIABLE ANALYSIS
#=============================================================================================================================================================================================================================
# Business Travel, Department, Education Field, Job Role, Marital Status

unique(employee_data$BusinessTravel) #Travel Rarely, Travel Freq, Non-Travel
unique(employee_data$Department) # Sales, Research & Development, HR
unique(employee_data$EducationField) # Life Sciences, Other, Medical, Marketing, Tech degree, HR
unique(employee_data$JobRole) # Sales Exec, Research Scientist, Lab tech, manufacturing director, HC rep, Manager, Sales rep, research director, HR
unique(employee_data$MaritalStatus) # Single, Married, Divorced

businesstravel_plot <- ggplot(employee_data, aes(x = BusinessTravel, fill = Attrition)) +
  geom_bar(position = "fill")
# Those who travel more frequently have a higher attrition rate. So the more you travel, the more likely you are to leave your job within this data set.

department_plot <- ggplot(employee_data, aes(x = Department, fill = Attrition)) +
  geom_bar(position = "fill")
# Those who work in sales and hr have a higher attrition rate vs those in the research and development department.

educationfield_plot <- ggplot(employee_data, aes(x = EducationField, fill = Attrition)) +
  geom_bar(position = "fill")
# those with degrees or an educational background of HR, Marketing, and tech have higher attrition rate.
# those with degrees or an educational background of life sciences, medical, and other have similar and lower attrition rates.

jobrole_plot <- ggplot(employee_data, aes(x = JobRole, fill = Attrition)) +
  geom_bar(position = "fill")
# Sales reps have the highest attrition rate of all job roles in this data set, it is then those who work in HR and as a lab tech.
# Sales executives and research scientists have lower attrition than the above but still have high attrition rates.
# manufacturing directors, managers, hc reps, and research directors all have low attrition rates.

maritalstatus_plot <- ggplot(employee_data, aes(x = MaritalStatus, fill = Attrition)) +
  geom_bar(position = "fill")
# Those that are single have the highest attrition rates, those that are married and divorced have lower attrition rates.

#=============================================================================================================================================================================================================================
# 7. FACET WRAPPING - GOOD PREDICTORS - NUMERIC VS CATEGORICAL VARIABLES (ALWAYS INCLUDE ATTRITION)
#=============================================================================================================================================================================================================================
# Good predictors:
# Age, Distance from Home, num of companies worked, total working years, years in current role, years since last promotion and years with current manager area ll good num predictors
# Environment satisfaction, job satisfaction, overtime, work life balance are all good categorical predictors

facet_data <- employee_data %>%
  mutate(education = mapvalues(employee_data$Education,
            from = c(1, 2, 3, 4, 5),
            to = c("Below College", "College", "Bachelor", "Master", "Doctor")),
          jobsatisfaction = mapvalues(employee_data$JobSatisfaction,
                                      from = c(1, 2, 3, 4),
                                      to = c("Low", "Medium", "High", "Very High")),
         environmentsatisfaction = mapvalues(employee_data$EnvironmentSatisfaction,
                                             from = c(1, 2, 3, 4),
                                             to = c("Low", "Medium", "High", "Very High")),
         worklifebalance = mapvalues(employee_data$WorkLifeBalance,
                                             from = c(1, 2, 3, 4),
                                             to = c("Low", "Medium", "High", "Very High"))
         )

facet_data$jobsatisfaction_f <- factor(facet_data$jobsatisfaction, levels = c("Low", "Medium", "High", "Very High"))
facet_data$education_f <- factor(facet_data$education, levels = c("Below College", "College", "Bachelor", "Master", "Doctor"))
facet_data$environmentsatisfaction_f <- factor(facet_data$environmentsatisfaction, levels = c("Low", "Medium", "High", "Very High"))
facet_data$worklifebalance_f <- factor(facet_data$worklifebalance, levels = c("Low", "Medium", "High", "Very High"))
# This tidy's the data a bit more for when projected on a grid. Instead of going alphabetic, it will go by the levels specified above

ggplot(facet_data, aes(x = YearsInCurrentRole, y = YearsWithCurrManager)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_grid(Attrition ~ jobsatisfaction_f) 
# As seen in the above graph, years in current role and years with current manager has a positive linear relationship in relation to attrition and job satisfaction
# Let's look at each section separately by attrition: those without attrition: low job satisfaction shows a positive and then negative correlation in years with current role and current manager.
# this could imply internal management change.
# Medium, High, and Very High job satisfaction shows a consistent positive relationship, with no noticeable decline

# Those with attrition: Low, medium and high job satisfaction showed two declines in the relationship between years in current role and years with current manager.
# the dips occur at different lengths of time, people with low satisfaction tend to have a decline sooner than those with medium and high job satisfaction.
# as expected, those with very high job satisfaction but still left their job showed a positive correlation through out. This could imply that individuals might have suddenly left for a better opportunity towards the end of their tenure which explains no dips throughout their time

ggplot(facet_data, aes(x = TotalWorkingYears, y = YearsSinceLastPromotion)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_grid(Attrition ~ education_f) 
# As seen in the above graph, total working years and years since last promotion, there are varying relationships in relation to attrition and education
# Let's look at each section separately by attrition: those without attrition: over the total years of work there is an increase in years since last promotion for those with education levels of below college and a Bachelor's.
# We see that there is a dip in years since last promotion over total working years within education levels of college and master's. This means that these individuals had a promotion within their career.
# it is surprising to see that college students will receive a promotion within their jobs at a sooner rate than those with Bachelor's. However, this can be explained by the type of work.
# College students tend to have different types of positions (food service, sales associate, and etc) vs Bachelors (entry level) which might lead to easier promotions.

# Those with attrition: individuals with below college and college education (less than bachelor's) showed attrition after promotions (as noted by the dip). This can be explained by the type of jobs obtained with that particular education level. Such as call center, sales, receptionists, admin support, and etc.
# that regardless of promotion, these positions are expected to have higher turnover.
# As expected, those with a Bachelor's and Master's Degree who were not promoted for a long time over the total years working have left their jobs.
# some possible explanations can include feeling under appreciated for their tenure and effort, overlooking educational impact, and etc.
# Those with doctorates, showed an attrition even after promotions, could be due to suddenly finding a better opportunity 

ggplot(facet_data, aes(x = TotalWorkingYears, y = Age)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_grid(Attrition ~ jobsatisfaction_f) 
# As seen in the above graph, total working years and age all show a positive relationship! The explanation of no attrition showing more of a linear relationship than yes attrition could be due to sample size. As it increases, the more linear it becomes.

ggplot(facet_data, aes(x = TotalWorkingYears, y = Age)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_grid(Attrition ~ environmentsatisfaction_f) 
# As seen in the above graph, total working years and age all show a positive relationship! The explanation of no attrition showing more of a linear relationship than yes attrition could be due to sample size. As it increases, the more linear it becomes.

ggplot(facet_data, aes(x = TotalWorkingYears, y = Age)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_grid(Attrition ~ OverTime)
# the last 3 facet grids do not add much value to the EDA ***MIGHT NOT INCLUDE!!!!**

