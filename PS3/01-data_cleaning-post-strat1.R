#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from https://usa.ipums.org/usa
# Author: Hyunseok Rha 
# Data: 02 November 2020
# Contact: hyunseok.rha@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data
# - Don't forget to gitignore it!

#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data.
setwd("C:/Users/Hyun/Desktop/UofT/5th/fall/sta304/ps3")
raw_data <- read_dta("./usa_00002.dta.gz")


# Add the labels
raw_data <- labelled::to_factor(raw_data)

# Renaming the columns to match with survey data 

reduced_data <-
  raw_data %>%
  rename(
    state = stateicp,
    gender = sex,
    race_ethnicity = race,
    hispanic = hispan
    )

# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_data <- 
  reduced_data %>% 
  select(
    state,
    gender, 
    age, 
    race_ethnicity, 
    hispanic,
    hhincome
    )
         

# Clean up data 

## Clean up age
reduced_data <- 
  reduced_data %>% 
  filter(age != "less than 1 year old") %>%
  filter(age != "90 (90+ in 1980 and 1990)")

reduced_data <- reduced_data %>% mutate(age=as.integer(age))
reduced_data <- reduced_data %>%
  mutate(age_group = case_when(
    age >= 10 & age < 20 ~ "10to19",
    age >= 20 & age < 30 ~ "20to29",
    age >= 30 & age < 40 ~ "30to39",
    age >= 40 & age < 50 ~ "40to49",
    age >= 50 & age < 60 ~ "50to59",
    age >= 60 & age < 70 ~ "60to69",
    age >= 70 & age < 80 ~ "70to79",
    age >= 80 & age < 90 ~ "80to89",
    age >= 90 ~ "90andmore"
  )) %>%
  filter(!is.na(age_group))

## Clean up household_income
reduced_data$household_income <- as.integer(reduced_data$hhincome)

reduced_data <- reduced_data %>%
  mutate(household_income = case_when(
    !is.na(household_income) & household_income <= 14999 ~ "Less than $14,999",
    !is.na(household_income) & household_income >= 15000 & household_income <= 19999 ~ "$15,000 to $19,999",
    !is.na(household_income) & household_income >= 20000 & household_income <= 24999 ~ "$20,000 to $24,999",
    !is.na(household_income) & household_income >= 25000 & household_income <= 29999 ~ "$25,000 to $29,999",
    !is.na(household_income) & household_income >= 30000 & household_income <= 34999 ~ "$30,000 to $34,999",
    !is.na(household_income) & household_income >= 35000 & household_income <= 39999 ~ "$35,000 to $39,999",
    !is.na(household_income) & household_income >= 40000 & household_income <= 44999 ~ "$40,000 to $44,999",
    !is.na(household_income) & household_income >= 45000 & household_income <= 49999 ~ "$45,000 to $49,999",
    !is.na(household_income) & household_income >= 50000 & household_income <= 54999 ~ "$50,000 to $54,999",
    !is.na(household_income) & household_income >= 55000 & household_income <= 59999 ~ "$55,000 to $59,999",
    !is.na(household_income) & household_income >= 60000 & household_income <= 64999 ~ "$60,000 to $64,999",
    !is.na(household_income) & household_income >= 65000 & household_income <= 69999 ~ "$65,000 to $69,999",
    !is.na(household_income) & household_income >= 70000 & household_income <= 74999 ~ "$70,000 to $74,999",
    !is.na(household_income) & household_income >= 75000 & household_income <= 79999 ~ "$75,000 to $79,999",
    !is.na(household_income) & household_income >= 80000 & household_income <= 84999 ~ "$80,000 to $84,999",
    !is.na(household_income) & household_income >= 85000 & household_income <= 89999 ~ "$85,000 to $89,999",
    !is.na(household_income) & household_income >= 90000 & household_income <= 94999 ~ "$90,000 to $94,999",
    !is.na(household_income) & household_income >= 95000 & household_income <= 99999 ~ "$95,000 to $99,999",
    !is.na(household_income) & household_income >= 100000 & household_income <= 124999 ~ "$100,000 to $124,999",
    !is.na(household_income) & household_income >= 125000 & household_income <= 149999 ~ "$125,000 to $149,999",
    !is.na(household_income) & household_income >= 150000 & household_income <= 174999 ~ "$150,000 to $174,999",
    !is.na(household_income) & household_income >= 175000 & household_income <= 199999 ~ "$175,000 to $199,999",
    !is.na(household_income) & household_income >= 200000 & household_income <= 249999 ~ "$200,000 to $249,999",
    !is.na(household_income) & household_income >= 20000 & household_income != 9999999 ~"$250,000 and above",
    is.na(household_income) | household_income == 9999999  ~ "unknown"
  ))

## Clean up race_ethnicity
reduced_data <- reduced_data %>%
  mutate(race_ethnicity = case_when(
    !is.na(race_ethnicity) & (race_ethnicity == "two major races" | race_ethnicity == "three or more major races") ~ "other race, nec",
    !is.na(race_ethnicity) ~ as.character(race_ethnicity)
  ))

## Clean up state 
reduced_data <- reduced_data %>%
  mutate(state = case_when(
    state == "district of columbia" ~ "Washington",
    !is.na(state) ~ as.character(state)
  )) %>%
  mutate(state = state.abb[match(state, tolower(state.name))]) %>%
  filter(!is.na(state))

# Saving the census data as a csv file in my
# working directory
write_csv(reduced_data, "./census_data.csv")