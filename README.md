# Homework-6

#group memebers: Leonardo Alcaide, Sun Wo Kim, Maria Camilia Vergas, Nene Diallo, Arafa Begum

1. Results of lab 6: acs_subgroup1 <- acs2021_couples %>% filter((AGE >= 25) & (AGE <= 55) &
(SEX == "Male") & (FOODSTMP = 2))


#maybe linear regression
ggplot(data = acs_subgroup1, mapping = aes(x = AGE, y = FOODSTMP)) + geom_smooth(method = "lm", color ='blue') + theme_economist() + labs (title = "Age and Food Stamps for men", y = " Foodstamps", x = "Age")


acs_subgroup2 <- acs_subgroup %>% filter((AGE >= 25) & (AGE <= 55) &
(SEX == "Female") & (FOODSTMP = 2))

ggplot(data = acs_subgroup2, mapping = aes(x = AGE, y = FOODSTMP)) + geom_smooth(method = "lm", color = 'red') + theme_economist() + labs (title = "Age and Food Stamps for women", y = " Foodstamps", x = "Age")


model_1 <- lm(educ_numeric ~ AGE + h_educ_numeric, data = acs_subgroup)


summary(model_1)

model_1 <- lm(educ_numeric ~ AGE + h_educ_numeric, data = acs_subgroup1)

confint(lm(educ_numeric ~ AGE + h_educ_numeric, data = acs_subgroup1))

model_2 <- lm(educ_numeric ~ AGE + h_educ_numeric, data = acs_subgroup2)
summary(model_2)
confint(lm(educ_numeric ~ AGE + h_educ_numeric, data = acs_subgroup2))



2. Hypothesis tests:

   
library(ggplot2)
library(tidyverse)
library(haven)
library(forcats)
load("~/Desktop/ACS_2021_couples.RData")
View(acs2021_couples)
```
```{r}
#Subgroup:
#This analysis aims to examine the relationship between income levels and the age differences reported by couples, with a focus on household income and age. Specifically, I will investigate whether different age groups in the labor force exhibit larger age gaps in their relationships compared to those who are not in the labor force, using a 90% confidence level for the statistical tests.
acs2021_couples$age_diff <- acs2021_couples$AGE - acs2021_couples$h_age
summary(acs2021_couples$AGE[(acs2021_couples$SEX == "Female")&(acs2021_couples$h_sex == "Male")])
summary(acs2021_couples$h_age[(acs2021_couples$SEX == "Female")&(acs2021_couples$h_sex == "Male")])
acs_subgroup <- acs2021_couples %>%
  mutate(income_midpoint = case_when(
    HHINCOME < 25000 ~ 12500,
    HHINCOME >= 25000 & HHINCOME < 35000 ~ 30000,
    HHINCOME >= 35000 & HHINCOME < 50000 ~ 40000,
    HHINCOME >= 50000 & HHINCOME < 75000 ~ 62500,
    HHINCOME >= 75000 & HHINCOME < 100000 ~ 82500,
    HHINCOME >= 100000 & HHINCOME < 150000 ~ 125000,
    HHINCOME >= 150000 & HHINCOME < 200000 ~ 175000,
    HHINCOME >= 200000 ~ 225000,
    TRUE ~ NA_real_ ))
View(acs_subgroup)
acs_subgroup1 <- acs_subgroup %>%
 
   filter(AGE >= 25 & AGE <= 85, WKSWORK2 > 4, UHRSWORK >= 35)
View (acs_subgroup1)
```
```{r}
 model_1 <- lm((AGE - h_age) ~ income_midpoint, data = acs_subgroup1)
summary(model_1)
confint(model_1, level = 0.90)
require(AER)
# subset in order to plot...
NNobs <- length(acs_subgroup1$income_midpoint)
set.seed(12345) # just so you can replicate and get same "random" choices
graph_obs <- (runif(NNobs) < 0.1) # so something like just 1/10 as many obs
dat_graph <-subset(acs_subgroup1,graph_obs) 
plot(income_midpoint ~ jitter(AGE - h_age, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), 
     ylim = c(0,120000), data = acs_subgroup1)
#ALT Hyp: There is a relationship between income levels and age differences in couples, with labor force status influencing age gaps.
#Null Hyp: There is no relationship between income levels and age differences in couples.
# t-stat: 14.560 
#p-value: <2e-16
#Conf int: (-3.896278e-01) , (-3.105302e-01)
#Conclusion:At the 90% confidence level, we reject the null hypothesis, indicating a significant relationship between household income levels and age differences in couples. Specifically, as the income midpoint increases, the age difference between partners decreases, suggesting a negative relationship. This supports the hypothesis that different age groups in the labor force exhibit significant differences in age gaps compared to those not in the labor force.


                 
acs_subgroup1 <- acs2021_couples %>% filter((AGE >= 25) & (AGE <= 55) &
(SEX == "Male") & (FOODSTMP = 2))



ggplot(data = acs_subgroup1, mapping = aes(x = AGE, y = FOODSTMP)) + geom_smooth(method = "lm", color ='blue') + theme_economist() + labs (title = "Age and Food Stamps for men", y = " Foodstamps", x = "Age")


acs_subgroup2 <- acs_subgroup %>% filter((AGE >= 25) & (AGE <= 55) &
(SEX == "Female") & (FOODSTMP = 2))

ggplot(data = acs_subgroup2, mapping = aes(x = AGE, y = FOODSTMP)) + geom_smooth(method = "lm", color = 'red') + theme_economist() + labs (title = "Age and Food Stamps for women", y = " Foodstamps", x = "Age")


model_1 <- lm(educ_numeric ~ AGE + h_educ_numeric, data = acs_subgroup)


summary(model_1)

model_1 <- lm(educ_numeric ~ AGE + h_educ_numeric, data = acs_subgroup1)

#t statsic: 2e-16
#t value: 167.0
#Std. error: 0.0411340
#Estimate: 6.8675352 

confint(lm(educ_numeric ~ AGE + h_educ_numeric, data = acs_subgroup1))

model_2 <- lm(educ_numeric ~ AGE + h_educ_numeric, data = acs_subgroup2)
summary(model_2)
confint(lm(educ_numeric ~ AGE + h_educ_numeric, data = acs_subgroup2))

#null hypothesis: women use food stamps more than men 

#w > m 

#alt hypothesis: women do not use more food stamps than men  
#w = m 

#this is a left tailed test 

#confidence interval is 95% 

#z score is 1.645

#the t stat must be less or equal to the z score. 

#2e-16 is greater than -1.645. 

#Conclusion: Since the t stat is greater than the z score, then we must fail to reject the null hypothesis. Thus women do use more food stamps than men.



3. #Peri, Giovanni. “Immigrants, Productivity, and Labor Markets.” The Journal of Economic Perspectives, vol. 30, no. 4, 2016, pp. 3–29. JSTOR, http://www.jstor.org/stable/44028256. Accessed 30 Oct. 2024. 

#This article goes over immigration over the past 60 years. The data used is from the National census, national surveys, the world bank and United Nations. The national surveys may be inaccessible but the national census, the world bank and the United Nations are viable sources of data for the final project. They did use many econmertics techniques like confidence interval, different sample sizes. The question addressed was whether immigration affects wages but controlling an experiment in Miami and some cities in Demark found that wages are not greatly affected by immigration.  

 
#Glennon, Britta. “Skilled Immigrants, Firms, and the Global Geography of Innovation.” The Journal of Economic Perspectives, vol. 38, no. 1, 2024, pp. 3–26. JSTOR, https://www.jstor.org/stable/27282172. Accessed 31 Oct. 2024. 

 
#This article examines firm’s relationship with skilled workers in terms of inputs and outputs as well as immigration policies of Canada and the United States for skilled workers. The sources used were USCIS Nonimmigrant Worker Petitions and the USCIS H-1B Employer Data Hub. These data sources seem accessible as they are government sources so they will not be to view. The econometrics tools used were showing the percentages of immigrants in different sectors.  
