H1N1_Flu_Vaccines <- read.delim("C:/Users/DELL User/Desktop/Data Sets/H1N1_Flu_Vaccines.txt", header= FALSE)
View(H1N1_Flu_Vaccines)
#Summary(H1N1_Flu_Vaccines)
head(H1N1_Flu_Vaccines)
str(H1N1_Flu_Vaccines)

#### Renaming the variables using dplyr where we can use the pipe  %>% ie, putting the output in the leftside into the right side)

library(dplyr)

H1N1_Flu_Vaccines <- H1N1_Flu_Vaccines %>%
  rename(
    h1n1_concern = V1,
    h1n1_knowledge = V2,
    chronic_med_condition = V3,
    health_worker = V4,
    opinion_h1n1_sick_from_vacc = V5,
    opinion_seas_sick_from_vacc = V6,
    age = V7,
    sex = V8,
    income_poverty = V9,
    marital_status = V10,
    household_children = V11,
    h1n1_vaccine = V12,
    seasonal_vaccine = V13
  )


### making sure all sorted numeric data are rightly converted into numeric. ( age and household children)

H1N1_Flu_Vaccines$age <- as.numeric(H1N1_Flu_Vaccines$age)
H1N1_Flu_Vaccines$household_children <- as.numeric(H1N1_Flu_Vaccines$household_children)

unique(H1N1_Flu_Vaccines$age)


##### Converting cartegorical variables (yes and no) to factors... mutate is a function from dplyr that lets you create new columns or modify existing columns in your dataset.

H1N1_Flu_Vaccines <- H1N1_Flu_Vaccines %>%
  mutate(
    chronic_med_condition = factor(chronic_med_condition, levels = c("No", "Yes")),
    health_worker = factor(health_worker, levels = c("No", "Yes")),
    h1n1_vaccine = factor(h1n1_vaccine, levels = c("No", "Yes")),
    seasonal_vaccine = factor(seasonal_vaccine, levels = c("No", "Yes"))
  )

#### Converting Multi-category Variables to Factors

H1N1_Flu_Vaccines <- H1N1_Flu_Vaccines %>%
  mutate(
    h1n1_concern = factor(h1n1_concern),
    h1n1_knowledge = factor(h1n1_knowledge),
    opinion_h1n1_sick_from_vacc = factor(opinion_h1n1_sick_from_vacc),
    opinion_seas_sick_from_vacc = factor(opinion_seas_sick_from_vacc),
    sex = factor(sex),
    income_poverty = factor(income_poverty),
    marital_status = factor(marital_status)
  )



### checking for missing values...
#Quick summary of missing data in all columns
#Helps you decide which columns need cleaning
#Works for any number of columns, no loops needed

is.na(H1N1_Flu_Vaccines) ##checking 

colSums(is.na(H1N1_Flu_Vaccines)) ##counting


colSums(is.na(H1N1_Flu_Vaccines)) / nrow(H1N1_Flu_Vaccines) * 100 ### percentage of missing values per colums

#unique(H1N1_Flu_Vaccines$household_children)


#H1N1_Flu_Vaccines[is.na(H1N1_Flu_Vaccines$household_children), ]


### EXPLORATORY DATA ANALYSIS
##Goal: Get to know patterns, distributions, and relationships in your data.

summary(H1N1_Flu_Vaccines)



## Starting with the numerical variables 
summary(H1N1_Flu_Vaccines$age)
summary(H1N1_Flu_Vaccines$household_children)


### categorical variables


table(H1N1_Flu_Vaccines$seasonal_vaccine)
table(H1N1_Flu_Vaccines$h1n1_vaccine)


table(H1N1_Flu_Vaccines$sex)
table(H1N1_Flu_Vaccines$chronic_med_condition)
table(H1N1_Flu_Vaccines$health_worker)

## data visualization
library(ggplot2)

### starting with numerical variables, age and number of children in household

##AGE

ggplot(H1N1_Flu_Vaccines, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Age Distribution", x = "Age", y = "Count")


ggplot(H1N1_Flu_Vaccines, aes(x = household_children)) +
  geom_histogram( fill = "pink", color = "black") +
  labs(title = "Number of Children in Household", x = "Number of Children", y = "Count")

##### Categorical variables

## vacination by sex

##Put sex on the x-axis and color the bars based on who took the H1N1 vaccine
##“Dodge” = compare groups next to each other, not on top of each other rather on side by side.
###Draw a bar chart with sex on the x-axis, color the bars by vaccination status, show the vaccinated and unvaccinated side by side, and label the plot properly


ggplot(H1N1_Flu_Vaccines, aes(x = sex, fill = h1n1_vaccine)) +
  geom_bar(position = "dodge") +
  labs(title = "H1N1 Vaccination by Sex", x = "Sex", y = "Count") +
  scale_fill_manual(values = c("No" = "red", "Yes" = "green"))



ggplot(H1N1_Flu_Vaccines, aes(x = chronic_med_condition, fill = h1n1_vaccine)) +
  geom_bar(position = "dodge") +
  labs(title = "Vaccination by Chronic Condition", x = "Chronic Condition", y = "Count") +
  scale_fill_manual(values = c("No" = "red", "Yes" = "green"))

