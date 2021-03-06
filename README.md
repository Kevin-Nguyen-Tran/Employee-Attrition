---
title: "Employee Attrition EDA"
author: "Kevin Tran"
date: "10/5/2020"
output: html_document
---

```{r setup, include=FALSE}
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

employee_data <- rename(employee_data, Age = ï..Age)
```

## Abstract

*Key:*

Education
1 'Below College'
2 'College'
3 'Bachelor'
4 'Master'
5 'Doctor'

EnvironmentSatisfaction
1 'Low'
2 'Medium'
3 'High'
4 'Very High'

JobInvolvement
1 'Low'
2 'Medium'
3 'High'
4 'Very High'

JobSatisfaction
1 'Low'
2 'Medium'
3 'High'
4 'Very High'

PerformanceRating
1 'Low'
2 'Good'
3 'Excellent'
4 'Outstanding'

RelationshipSatisfaction
1 'Low'
2 'Medium'
3 'High'
4 'Very High'

WorkLifeBalance
1 'Bad'
2 'Good'
3 'Better'
4 'Best'

## Proportion of Attrition Within Dataset

```{r}
ggplot(employee_data, aes(x = Attrition)) +
  geom_bar(position = "stack", fill = wes_palette("GrandBudapest2", n = 2)) +
  theme_dark() +
  labs(x = "Attrition", 
       y = "Count",
       title = "Less Attrition in this Data Set",
       caption = "Source: IBM HR Analytics") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

employee_data %>%
  group_by(Attrition) %>%
  summarise(n = n())
```

Within the data set, 1233 have stayed with their employer and 237 have left their job.
This data set shows that roughly 20% of employees have left their employer. We will analyze attrition and its relationship to multiple variables to see their correlation and possible focus points of improvement.

First we will run a Pearson's Correlation heat map and see which variables have a positive, neutral, or negative correlation and explore each group and produce possible explanations for these relationships.

## Pearson's Correlation - Heat Map

```{r}
corr_data <- employee_data %>%
  mutate(attrition = ifelse(Attrition == "No", 0, 1),
         gender = ifelse(Gender == "Female", 0, 1),
         overtime = ifelse(OverTime == "No", 0, 1)) %>%
  select(Age, attrition, DistanceFromHome, Education, NumCompaniesWorked, EnvironmentSatisfaction, gender, HourlyRate, JobSatisfaction, PercentSalaryHike,  overtime, TotalWorkingYears, WorkLifeBalance, YearsAtCompany:YearsWithCurrManager)


ggcorrplot(cor(corr_data), hc.order = TRUE, lab = TRUE, lab_size = 2) +
  labs(title = "Correlation Between Variables and Attrition",
       subtitle = "Netural and Positive Correlation",
       caption = "Source: IBM HR Analytics") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
```

* First we have narrowed our data set to focus on variables of interest. To ensure that the correlation heat map correctly ran, all variables must be a numeric value. 

* Based on this correlation plot, we are more interested in how attrition fares with the other employee factors
* We can separate by positive correlation, neutral correlation. and negative correlation
  * Positive correlation: overtime and distance from home
  * Neutral correlation (+- less than .05): hourly rate, distance from home, percent salary hike, gender, years since last promotion, work life balance.
  * Negative correlation: job satisfaction, age, total working years, years in current role, years at company, years with current manager, and environment sat

* This will be our overhead analysis and we will now dive a bit deeper into each group and see if it matches our linear regression model (which we will formulate later in our analysis).

## Positive Correlation to Attrition
In this section, we will evaluate the positive correlation within our data set in relation to attrition. According to our heat map, we have two:
* OverTime
* Distance from Home

This indicates, that as overtime increases and work distance from home increases, so does the likelihood of attrition as shown below:

```{r}
ggplot(employee_data, aes(x = OverTime, fill = Attrition)) +
  geom_bar(position = "fill") +
  theme_dark() +
  labs(x = "Over Time", 
       y = "Proportion",
       title = "Over Time Employees",
       subtitle = "Have More Attrition",
       caption = "Source: IBM HR Analytics") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggplot(employee_data, aes(x = Attrition, y = DistanceFromHome)) +
  geom_boxplot() +
  theme_dark() +
  labs(x = "Attrition", 
       y = "Distance from Home",
       title = "More Distance from Work to Home",
       subtitle = "Has More Attrition",
       caption = "Source: IBM HR Analytics") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
```

This bar chart tells us, that there is more attrition in those that decide to work overtime vs those that do not. Also, this box plot tells us, that there is more attrition in those that have to commute greater distances than those that do not.

* Within this data set, attrition will occur when an individual will have to commute 10.63+ miles to work.
  * Commute tolerance will vary depending on job type, geographic location, and etc.

Although both plots show a positive correlation to attrition, is it statistically significant?
```{r}
t.test(corr_data$overtime ~ corr_data$attrition, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)

t.test(corr_data$DistanceFromHome ~ corr_data$attrition, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)

```

Null Hypothesis: states that there is no significant difference between the attrition of those who work and do not work overtime.
Null Hypothesis: states that there is no significant difference between the attrition of those who work closer to home and who works further from home.

Due to our p-value being less than 5% and the confidence interval does not include zero, we can reject both of the null hypotheses above with 95% confidence and assume that there is a statistically significant difference in attrition.

## Neutral Correlation to Attrition
In this section, we will evaluate the neutral correlation within our data set in relation to attrition. According to our heat map, we have 7 (+- .05):

* Hourly Rate
* Percent Salary Hike
* Years Since Last Promotion
* Number of Companies Worked For
* Education
* Gender

This indicates, that as each variable increases there will be no change in the likelihood of attrition as shown below:
```{r}
hr_att <- ggplot(employee_data, aes(x = Attrition, y = HourlyRate)) +
  geom_boxplot(fill = wes_palette("GrandBudapest1", n = 2)) +
  theme_dark() +
  labs(y = "Hourly Rate"
)

perc_att <- ggplot(employee_data, aes(x = Attrition, y = PercentSalaryHike)) +
  geom_boxplot(fill = wes_palette("GrandBudapest1", n = 2)) +
  theme_dark() +
  labs(y = "% Salary Hike"
  )

years_att <- ggplot(employee_data, aes(x = Attrition, y = YearsSinceLastPromotion)) +
  geom_boxplot(fill = wes_palette("GrandBudapest1", n = 2)) +
  theme_dark() +
  labs(y = "Yrs Since Promo"
  )

numcomp_att <- ggplot(employee_data, aes(x = Attrition, y = NumCompaniesWorked)) +
  geom_boxplot(fill = wes_palette("GrandBudapest1", n = 2)) +
  theme_dark() +
  labs(y = "# Companies Worked"
  )

edu_att <- ggplot(employee_data, aes(x = Attrition, y = Education)) +
  geom_boxplot(fill = wes_palette("GrandBudapest1", n = 2)) +
  theme_dark() +
  labs(y = "Education"
  )

multiplot(hr_att, perc_att, years_att, numcomp_att, edu_att, cols = 2)

ggplot(employee_data, aes(x = Gender, fill = Attrition)) +
  geom_bar(position = "fill")

# As we can see, the average of each variable are similar regardless of attrition status.
hourrate_t <- t.test(corr_data$HourlyRate ~ corr_data$attrition, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)
percsalhike_t <- t.test(corr_data$PercentSalaryHike ~ corr_data$attrition, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)
yrslastpromo_t <- t.test(corr_data$YearsSinceLastPromotion ~ corr_data$attrition, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)
gender_t <- t.test(corr_data$gender ~ corr_data$attrition, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)
numcompworked_t <- t.test(corr_data$NumCompaniesWorked ~ corr_data$attrition, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)
edu_t <-  t.test(corr_data$Education ~ corr_data$attrition, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)

tribble(
  ~name, ~p.value,
  "Hourly Rate", hourrate_t$p.value,
  "Percent Salary Hike", percsalhike_t$p.value,
  "Years Since Last Promotion", yrslastpromo_t$p.value,
  "Number of Companies Worked", numcompworked_t$p.value,
  "Education", edu_t$p.value,
  "Gender", gender_t$p.value
)
```

Hourly rate, percent salary hike, years since last promotion, Number of Companies Worked, education and gender all show no significant difference in those who did and did not leave their job. Which aligns with the pearson's correlation test.

* Therefore, within this data set, we can assume that the variables above do not affect attrition and will not be a good predictor when formulating a linear regression model (which will be in later sections).

## Negative Correlation to Attrition
In this section, we will evaluate the negative correlation within our data set in relation to attrition. According to our heat map, we have 8:

* Job Satisfaction
* Age
* Work Life Balance
* Total Working Years
* Years in Current Role
* Years at Company
* Years with Current Manager
* Environment Satisfaction


This indicates, that as each variable increases there will be less likelihood of attrition as shown below:
```{r}
job_att <- ggplot(employee_data, aes(x = Attrition, y = JobSatisfaction)) +
  geom_boxplot(fill = wes_palette("Darjeeling1", n = 2)) +
  theme_dark() +
  labs(y = "Job Satisfaction"
  )

age_att <- ggplot(employee_data, aes(x = Attrition, y = Age)) +
  geom_boxplot(fill = wes_palette("Darjeeling1", n = 2)) +
  theme_dark() +
  labs(y = "Age"
  )

work_att <- ggplot(employee_data, aes(x = Attrition, y = WorkLifeBalance)) +
  geom_boxplot(fill = wes_palette("GrandBudapest1", n = 2)) +
  theme_dark() +
  labs(y = "Work Life Balance"
  )

worky_att <- ggplot(employee_data, aes(x = Attrition, y = TotalWorkingYears)) +
  geom_boxplot(fill = wes_palette("Darjeeling1", n = 2)) +
  theme_dark() +
  labs(y = "Total Working Yrs"
  )

yearscurr_att <- ggplot(employee_data, aes(x = Attrition, y = YearsInCurrentRole)) +
  geom_boxplot(fill = wes_palette("Darjeeling1", n = 2)) +
  theme_dark() +
  labs(y = "Yrs in Curr Role"
  )

yearscomp_att <- ggplot(employee_data, aes(x = Attrition, y = YearsAtCompany)) +
  geom_boxplot(fill = wes_palette("Darjeeling1", n = 2)) +
  theme_dark() +
  labs(y = "Yrs at Company"
  )

yearsmgr_att <- ggplot(employee_data, aes(x = Attrition, y = YearsWithCurrManager)) +
  geom_boxplot(fill = wes_palette("Darjeeling1", n = 2)) +
  theme_dark() +
  labs(y = "Yrs w/ Curr Manager"
  )

env_att <- ggplot(employee_data, aes(x = Attrition, y = EnvironmentSatisfaction)) +
  geom_boxplot(fill = wes_palette("Darjeeling1", n = 2)) +
  theme_dark() +
  labs(y = "Env Satisfaction"
  )


multiplot(job_att, age_att, work_att, worky_att, yearscurr_att, yearscomp_att, yearsmgr_att, env_att, cols = 3)

jobsat_t <- t.test(corr_data$JobSatisfaction ~ corr_data$attrition, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)
age_t <- t.test(corr_data$Age ~ corr_data$attrition, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)
wrklifebal_t <- t.test(corr_data$WorkLifeBalance ~ corr_data$attrition, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)
workyrs_t <- t.test(corr_data$TotalWorkingYears ~ corr_data$attrition, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)
yrsrole_t <- t.test(corr_data$YearsInCurrentRole ~ corr_data$attrition, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)
yrscomp_t <- t.test(corr_data$YearsAtCompany ~ corr_data$attrition, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)
yrsmgr_t <- t.test(corr_data$YearsWithCurrManager ~ corr_data$attrition, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)
envsat_t <- t.test(corr_data$EnvironmentSatisfaction ~ corr_data$attrition, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)

```

```{r}
tribble(
  ~name, ~p.value,
  "Job Satisfaction", jobsat_t$p.value,
  "Age", age_t$p.value,
  "Work Life Balance", wrklifebal_t$p.value,
  "Total Working Years", workyrs_t$p.value,
  "Years In Current Role", yrsrole_t$p.value,
  "Years at Company", yrscomp_t$p.value,
  "Years with Current Mgr", yrsmgr_t$p.value,
  "Environment Satisfaction", envsat_t$p.value
)
```


* All t.tests above show a significant difference between the average of each variable in respect to attrition. 
  * Therefore we can say with 95% confidence that each variable has a significantly negative correlation with attrition.

* To retain employees, they are more satisfied with their job, have had more tenure and working experience at the company. Who have also stayed under their current manager and is satisfied with their environment.
  *If we were to choose areas of focus, we would look at the p-values and see which one had the smallest and make those areas of focus, since the probability of retention would be greater!

As shown in the table above, the p-value is smallest for Years In Current Role, Total Working Years, and years with current Manager.
* All were statistically significant, however, if we were to focus on a few variables and how we can retain employees: 
  * we need to ensure that we acquire tenured professionals, both in total work experience and experience in their role, as well as maintain a good employee to manager relationship.

## Logistic Linear Regression Model 

Within this section, we will build a binomial generalized linear model to predict the likelihood of attrition depending on all variables explored within the data analysis thus far.

When analyzing the results of the model, the variables with significant p-values (less than 5%) and large effect sizes will be categorized as good predictors of attrition.

Below is our Logistic Linear Regression Model and the summary of each variable in relation to eachother and how it corresponds to attrition:

```{r}
predicted <- glm(attrition ~ ., family = "binomial", data = corr_data)
summary(predicted)
```

Based on the summary: Age, Distance from Home, Environment satisfaction, job satisfaction, overtime, total working years, work life balance, years in current role, years since last promotion and years with current manager are all good predictors.

Top 3 Predictors

* Overtime had the smallest p-value as well as the largest effect size
* Environment satisfaction had a small p-value and the second largest effect size
* Job Satisfaction had a slightly larger p-value and the third largest effect size
  
It is interesting to see that Numbers of Companies Worked and Years Since Last Promotion is statistically significant and are good predictors of attrition when it showed no average difference (noted by the t.test and heat map) in earlier sections.
It is also surprising to see that years at company is also not statistically significant and a good predictor of attrition despite the t.test and heat map stating there was a significant difference.

* Keep in mind, these good/bad predictors are determined by their relationship to one another in regards to attrition
  * They are not individually analyzed in relation to attrition as the earlier sections do
  * The real world would operate more closely to the results of the generalized linear model as people will generally consider multiple variables together prior to leaving a job versus just one.
  
# Categorical Variable Analysis
In this section, we will evaluate the correlation between the categorical variables within our data set in relation to attrition. Within this data set we have 5:

* Business Travel
* Department
* Education Field
* Job Role
* Marital Status

We want to explore if someone who travels rarely will have a different likelihood of attrition vs someone who travels frequently and so on.

To do this I have created a bar chart for each categorical variable and visualized the proportion of attrition for each.

As shown below each categorical variable has a set of different levels:
```{r}
unique(employee_data$BusinessTravel) #Travel Rarely, Travel Freq, Non-Travel
unique(employee_data$Department) # Sales, Research & Development, HR
unique(employee_data$EducationField) # Life Sciences, Other, Medical, Marketing, Tech degree, HR
unique(employee_data$JobRole) # Sales Exec, Research Scientist, Lab tech, manufacturing director, HC rep, Manager, Sales rep, research director, HR
unique(employee_data$MaritalStatus) # Single, Married, Divorced
```

Below are the bar charts:
```{r}
ggplot(employee_data, aes(x = BusinessTravel, fill = Attrition)) +
  geom_bar(position = "fill")
```

* Those who travel more frequently have a higher attrition rate. So the more frequently you travel, the higher the likelihood you are to leave your job within this data set.

```{r}
ggplot(employee_data, aes(x = Department, fill = Attrition)) +
  geom_bar(position = "fill")
```

* Within this analysis, those who work in sales and HR have a higher attrition rate vs those in the research and development department.

```{r}
ggplot(employee_data, aes(x = EducationField, fill = Attrition)) +
  geom_bar(position = "fill")
```
* Those with degrees or an educational background of HR, Marketing, and tech have higher attrition rate than those with degrees or an educational background of life sciences and medical.

```{r}
ggplot(employee_data, aes(x = JobRole, fill = Attrition)) +
  geom_bar(position = "fill")
```

* Sales representatives have the highest attrition rate of all job roles in this data set, then those who work in HR and as a lab technician

* Sales executives and research scientists have lower attrition than the above but still have high attrition rates.

* Manufacturing directors, managers, healthcare representatives, and research directors all have low attrition rates.

```{r}
ggplot(employee_data, aes(x = MaritalStatus, fill = Attrition)) +
  geom_bar(position = "fill")
```

* Those that are single have the highest attrition rates, those that are married and divorced have lower attrition rates.

**Keep in mind that this outcome is not a representation of this particular data set population but not the general population. In order to make a statistical inference, we would need a larger data set.**

## Predictors of Interest - Numeric vs Categorical

Within this section of the analysis, I wanted to dive deeper into the good predictors and analyze their relationship to one another. We will facet wrap multiple variables together and analyze the relationship shown:

```{r}
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
```

```{r}
ggplot(facet_data, aes(x = YearsInCurrentRole, y = YearsWithCurrManager)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_grid(Attrition ~ jobsatisfaction_f) 
```

As seen in the above graph, years in current role and years with current manager has a positive linear relationship in relation to attrition and job satisfaction

Let's look at each section separately by attrition: 
* Those without attrition: low job satisfaction shows a positive and then negative correlation in years with current role and current manager.
  * This could imply internal management change.
* Medium, High, and Very High job satisfaction shows a consistent positive relationship, with no noticeable decline

* Those with attrition: Low, medium and high job satisfaction showed two declines in the relationship between years in current role and years with current manager.
  * The dips occur at different lengths of time, people with low satisfaction tend to have a decline sooner than those with medium and high job satisfaction.
  * As expected, those with very high job satisfaction but still left their job showed a positive correlation through out. This could imply that individuals might have suddenly left for a better opportunity towards the end of their tenure which explains no dips throughout their time with the current manager/current role.

```{r}
ggplot(facet_data, aes(x = TotalWorkingYears, y = YearsSinceLastPromotion)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_grid(Attrition ~ education_f) 
```

As seen in the above graph, total working years and years since last promotion, there are varying relationships in relation to attrition and education

Let's look at each section separately by attrition: 

* Those without attrition: over the total years of work there is an increase in years since last promotion for those with education levels of below college and a Bachelor's.
* We see that there is a dip in years since last promotion over total working years within education levels of college and master's. This means that these individuals had a promotion within their career.
  * It is surprising to see that college students will receive a promotion within their jobs at a sooner rate than those with Bachelor's. However, this can be explained by the type of work.
  * College students tend to have different types of positions (food service, sales associate, and etc) vs Bachelors (entry level corporate roles and etc) which might lead to quicker/easier promotions. Individuals with Bachelor Degrees have more competition for promotions and tend to require 1.5+ years of tenure with the company.

* Those with attrition: individuals with below college and college education (less than Bachelor's) showed attrition after promotions (as noted by the dip). This can be explained by the type of jobs obtained with that particular education level. Such as call center representatives, sales, receptionists, admin support, and etc.
  * That regardless of a promotion, these positions are expected to have higher turnover.
* As expected, those with a Bachelor's and Master's Degree who were not promoted for a long time over the total tenure have left their jobs.
  * Some possible explanations can include feeling under appreciated for their tenure and effort, overlooking educational impact, and etc.
* Those with doctorates, showed attrition even after promotions, could be due to suddenly finding a better opportunity 





































































































