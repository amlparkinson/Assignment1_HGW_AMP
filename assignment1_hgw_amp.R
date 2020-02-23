# Assignment 1 - Hannah Garcia-Wickstrum & Anne-Marie Parkinson hi

# This function determines global per capita growth rate based on average years of education completed, average daily kilocalorie intake per capita, HIV percent prevalence, and the gross domestic product.

# 'intercept' was derived from a multiple linear regression model (see below)
# 'education' coefficient was derived from a multple linear regression model, units = years (see below)
# 'kcal' coefficient was derived from a multple linear regression model, units = KCal (see below)
# 'hiv' coefficient was derived from a multiple linear regression model, units = % prevalence between ages 15 - 49
# 'gdp' (gross domestic product) coefficient was derived from a multiple linear regression model, units = USD ($)
# 'education_variable' is the global mean years of education completed, units = years
# 'kcal_variable' is the global average daily kilocalorie intake per capita, units = KCal
# 'hiv_variable' is the percent prevalence of the disease in people ages 15 - 49, units = %
# 'gdp_variable' is the global dollar amount of per capita gdp, units = USD ($)

library(tidyverse)
library(dplyr)

global_dNNdt = function(education_variable, kcal_variable, hiv_variable, gdp_variable, intercept=0.014, education=-0.001104, kcal=-0.000000004785, hiv=0.0005353, gdp =0.0000001285) {
  # set parameters for education and kilocalories
  education_variable = ifelse((education_variable < 0), return("education_variable cannot be less than zero"), education_variable) 
  if (education_variable > 30) return("education_variable cannot be > 30")
  kcal_variable = ifelse((kcal_variable < 0), NA, kcal_variable)
  if (kcal_variable > 25000) return("kcal_variable cannot be > 25000")
  if (hiv_variable < 0) return("hiv_variable cannot be < 0")
  hiv_variable = ifelse((hiv_variable > 100), return("hiv_variable cannot be > 100"), hiv_variable)
  if (gdp_variable < 0) return("gdp_variable cannot be < 0 ")
  result = intercept + (education*education_variable) + (kcal*kcal_variable) + (gdp*gdp_variable) + (hiv*hiv_variable)
  return(result)
} 

global_dNNdt(15, 12000, 10, 10000)

# Create a data frame
nsample = 100 