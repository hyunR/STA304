#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from https://www.voterstudygroup.org/publication/nationscape-data-set
# Author: Hyunseok Rha
# Data: 02 November 2020
# Contact: hyunseok.rha@utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from https://www.voterstudygroup.org/publication/nationscape-data-set
# - Don't forget to gitignore it!

#### Workspace setup ####
library(haven)
library(tidyverse)
setwd("C:/Users/Hyun/Desktop/UofT/5th/fall/sta304/ps3")
# Read in the raw data (You might need to change this if you use a different dataset)
raw_data <- read_dta("./ns20200625.dta")
# Add the labels
raw_data <- labelled::to_factor(raw_data)
# Just keep some variables
reduced_data <- 
  raw_data %>% 
  select(interest,
         registration,
         vote_2016,
         vote_intention,
         vote_2020,
         ideo5,
         employment,
         foreign_born,
         gender,
         census_region,
         hispanic,
         race_ethnicity,
         household_income,
         education,
         state,
         congress_district,
         age)

# Clean up data
reduced_data<-
  reduced_data %>%
  mutate(vote_trump = 
           ifelse(vote_2020=="Donald Trump", 1, 0))


## Clean up ages 
reduced_data <- reduced_data %>% mutate(age=as.integer(age))
reduced_data <- reduced_data %>%
  mutate(age_group = case_when(
    age >= 0 & age < 10 ~ "0to9",
    age >= 10 & age < 20 ~ "10to19",
    age >= 20 & age < 30 ~ "20to29",
    age >= 30 & age < 40 ~ "30to39",
    age >= 40 & age < 50 ~ "40to49",
    age >= 50 & age < 60 ~ "50to59",
    age >= 60 & age < 70 ~ "60to69",
    age >= 70 & age < 80 ~ "70to79",
    age >= 80 & age < 90 ~ "80to89",
    age >= 90 ~ "90andmore"
  ))

## Clean up race_ethnicity to match with census data 
reduced_data <- reduced_data %>%
  mutate(race_ethnicity = case_when(
    !is.na(race_ethnicity) & race_ethnicity == "White" ~ "white",
    !is.na(race_ethnicity) & race_ethnicity == "Black, or African American" ~ "black/african american/negro",
    is.na(race_ethnicity) | race_ethnicity == "Some other race" ~ "other race, nec",
    !is.na(race_ethnicity) & (
        race_ethnicity == "Asian (Asian Indian)" | 
        race_ethnicity == "Asian (Vietnamese)" | 
        race_ethnicity == "Asian (Korean)" | 
        race_ethnicity == "Asian (Filipino)" | 
        race_ethnicity == "Asian (Other)" | 
        race_ethnicity == "Pacific Islander (Native Hawaiian)" |
        race_ethnicity == "Pacific Islander (Other)" |
        race_ethnicity == "Pacific Islander (Samoan)" |
        race_ethnicity == "Pacific Islander (Guamanian)") ~ "other asian or pacific islander",
    !is.na(race_ethnicity) & race_ethnicity == "Asian (Chinese)" ~ "chinese",
    !is.na(race_ethnicity) & race_ethnicity == "Asian (Japanese)" ~ "japanese",
    !is.na(race_ethnicity) & race_ethnicity == "Some other race" ~ "other race, nec",
    !is.na(race_ethnicity) & race_ethnicity == "American Indian or Alaska Native" ~ "american indian or alaska native"
  ))

## Clean up hispanic column to match with census data 
reduced_data <- reduced_data %>%
  mutate(hispanic = case_when(
    !is.na(hispanic) & hispanic == "Not Hispanic" ~ "not hispanic",
    !is.na(hispanic) & hispanic == "Mexican" ~ "mexican",
    !is.na(hispanic) & hispanic == "Puerto Rican" ~ "puerto rican",
    !is.na(hispanic) & hispanic == "Cuban" ~ "cuban",
    !is.na(hispanic) ~ "other"
  ))

## Clean up gender column to match with census data 
reduced_data <- reduced_data %>%
  mutate(gender = case_when(
    !is.na(gender) & gender == "Female" ~ "female",
    !is.na(gender) & gender == "Male" ~ "male"
  ))

## Clean up household income
reduced_data <- reduced_data %>%
  mutate(household_income = case_when(
    is.na(household_income) ~ "unknown",
    !is.na(household_income) ~ as.character(household_income)
  ))


# Saving the survey/sample data as a csv file in my
# working directory
write_csv(reduced_data, "./survey_data.csv")