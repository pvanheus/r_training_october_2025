# Cleaning data using R
# based on https://tutorials.appliedepi.org/app/tutorial04_en
# Peter van Heusden (SANBI) and Djibril Dione
# October 2025
# environment set up -----
# install pacman in case it is not already installed
install.packages("pacman")

pacman::p_load(
  rio,
  here,
  skimr,
  tidyverse,
  janitor,
  epikit,
  lubridate
)

# exploratory data analysis ----
# the data is in the data/raw folder in linelist_raw.csv
linelist_raw <- import(here("data", "raw", "linelist_raw.csv"))

# number of rows (observations)
nrow(linelist_raw)

# number of columns (variables)
ncol(linelist_raw)

# column names
names(linelist_raw)

# summarise data from sex column
tabyl(linelist_raw, "sex")

# summarise the whole dataset's numeric columns
summary(linelist_raw)

# what class ("type") is the linelist_raw object
class(linelist_raw)

# what class is the "onset date" column
class(linelist_raw$`onset date`)

# what class is the "date of report" column
class(linelist_raw$`date of report`)

# what are the values in the "fever" column
# if the column name is a single name you can give it without quotes
tabyl(linelist_raw, fever)

# skim the data (from skimr package)
skim(linelist_raw)

# first version of a data cleaning pipe ----

# clean the column names and remove duplicate records
linelist <- linelist_raw %>%
  clean_names() %>%
  distinct()

# add a filter to select age > 10
linelist <- linelist_raw %>%
  clean_names() %>%
  distinct() %>%
  filter(age > 10)

# view the data.frame
View(linelist)

# do some manual renaming of columns too
linelist <- linelist_raw %>%
  clean_names() %>%
  distinct() %>%
  rename(date_onset = onset_date,
         date_report = date_of_report,
         district_res = adm3_name_res,
         district_det = adm3_name_det)

# selecting some columns for output ----

# select only some columns which columns to output
linelist %>%
  select(case_id, age, sex, lat, lon) %>%
  filter(age > 65)

# select everything except for epilink column
linelist %>%
  select(-epilink)

# select everything except for three columns
# using the c() - compose - function
linelist %>%
  select(-c(epilink, district_det, temp))

# the same thing done a different way
linelist %>%
  select(-epilink, -district_det, -temp)

# moving the data around using select() and everything()
linelist %>%
  select(lat, lon, everything())

# how select work? use help
help(select)

# keep the columns from fever to vomit
linelist %>%
  select(fever:vomit)

# keep the columns from 1 through 5
linelist %>%
  select(1:5)

# deduplication ----

# distinct() with empty parentheses will remove rows that are 100% identical
# to each other
linelist <- linelist_raw %>% 
  clean_names() %>% 
  rename(date_onset = onset_date,
         date_report = date_of_report,
         district_res = adm3_name_res,
         district_det = adm3_name_det) %>% 
  select(-row_num) %>% 
  distinct() 

# distinct(case_id) will consider a row identical if it has the same case_id
linelist %>% distinct(case_id)

# by default distinct(case_id) removes all other columns
# distinct(case_id, .keep_all = TRUE) keeps those columns
linelist %>%
  distinct(case_id, .keep_all = TRUE)

# also see the Epi R handbook chapter on data cleaning:
# https://epirhandbook.com/en/new_pages/cleaning.html

# columns and their classes ----
# getting dates from "character" to "date" type

# see what the dates look like - head() gives the first few examples
head(linelist$date_onset)

# it is in month/day/year format - use mdy on the date to test
mdy(linelist$date_onset)

# create a new column using mutate
# in this example bmi is added using a formula
# note that the result is not saved, so it does not change
# the values in linelist
# by default new columns appear on the far right of the data frame
# so we use select to display just the ones of interest
linelist %>%
  mutate(bmi = wt_kg / (ht_cm / 100) ^ 2) %>%
  select(case_id, wt_kg, ht_cm, bmi)


# using a string function, toupper(), create an uppercase
# column of district_res
linelist %>% 
  mutate(district_upper = toupper(district_res)) %>%
  select(case_id, district_res, district_upper)

# use mdy() to convert dates from character to string
linelist <- linelist_raw %>%
  clean_names() %>%
  distinct() %>%
  rename(date_onset = onset_date,
         date_report = date_of_report,
         district_res = adm3_name_res,
         district_det = adm3_name_det) %>%
  mutate(date_onset = mdy(date_onset)) %>%
  mutate(date_report = mdy(date_report))

# show the class of the date_onset column
class(linelist$date_onset)

# illustration that this makes plotting an epidemic curve possible
ggplot(data = linelist, mapping = aes(x = date_onset)) + geom_histogram()

# one can also write multiple mutate operations in a single function
# but keeping them separate can be easier to read
# use mdy() to convert dates from character to string
linelist <- linelist_raw %>%
  clean_names() %>%
  distinct() %>%
  rename(date_onset = onset_date,
         date_report = date_of_report,
         district_res = adm3_name_res,
         district_det = adm3_name_det) %>%
  mutate(date_onset = mdy(date_onset),
         date_report = mdy(date_report))

# cleaning up missing and incorrect data ----

# R expects missing values to be the special value NA
# space and "Unknown" and other values are not NA,
# e.g. in the "sex" column
tabyl(linelist, "sex")

# an illustration of changing all "Unknown" in the sex column to
# NA
linelist %>%
  mutate(sex = na_if(sex, "Unknown")) %>%
  tabyl(sex)

# the same thing as part of our data cleaning pipeline
linelist <- linelist_raw %>%
  clean_names() %>%
  distinct() %>%
  rename(date_onset = onset_date,
         date_report = date_of_report,
         district_res = adm3_name_res,
         district_det = adm3_name_det) %>%
  mutate(date_onset = mdy(date_onset)) %>%
  mutate(date_report = mdy(date_report)) %>%
  mutate(sex = na_if(sex, "Unknown"))

# recode() lets us replace date e.g. data we know is incorrect
# mutate(column_name = recode(column_name, "OLD value" = "NEW value"))
# e.g. (in this example we don't save the new data)
linelist %>%
  tabyl(hospital)

# we see we have some misspellings - "Mitilary Hospital"
# note that recode is different to rename in 2 ways:
# 1. it is used within mutate()
# 2. one writes "OLD value" = "NEW value" (in rename it is the other way round)
linelist %>%
  mutate(hospital = recode(hospital, 
                           "Mitilary Hospital" = "Military Hospital",
                           "Port Hopital" = "Port Hospital",
                           "Port" = "Port Hospital")) %>%
  tabyl(hospital)

# now as part of our data cleaning pipeline, saving results to a new column
# hospital_clean (so that we don't lose the old data and can always see which
# records had the mistakes before)
linelist <- linelist_raw %>%
  clean_names() %>%
  distinct() %>%
  rename(date_onset = onset_date,
         date_report = date_of_report,
         district_res = adm3_name_res,
         district_det = adm3_name_det) %>%
  mutate(date_onset = mdy(date_onset)) %>%
  mutate(date_report = mdy(date_report)) %>%
  mutate(sex = na_if(sex, "Unknown")) %>%
  mutate(hospital_clean = recode(hospital, 
                           "Mitilary Hospital" = "Military Hospital",
                           "Port Hopital" = "Port Hospital",
                           "Port" = "Port Hospital"))

# examine the changes - it creates a table summarising the different occurences
# of the values
linelist %>% 
  tabyl(hospital, hospital_clean)

# recording data according to logical statements
# logical statements are ones about whether something is TRUE or FALSE
# and use ifelse()
# ifelse() has three parameters:
# 1. test - the comparison to perform
# 2. true value - the value to return if test evaluates to true
# 3. false value - the value to return if test evaluates to false
linelist %>%
  mutate(adult = ifelse(age >= 18, "yes", "no")) %>%
  select(case_id, age, adult)

# you can also use named parameters
linelist %>%
  mutate(adult = ifelse(test = age >= 18, 
                        yes = "yes", 
                        no = "no")) %>%
  select(case_id, age, adult)

# note that in a logical test, == means "is equal to"
# in this example & is a "logical operator" which means AND
# learn more about logical operators in the section
# "Review Logical Operators" which is in the Simple Recoding section
# of Tutorial 4
linelist %>%
  mutate(sixty_year_old = ifelse(age == 60, "yes", "no")) %>%
  filter(age > 50 & age < 70) %>%
  select(case_id, age, sixty_year_old)

# cleaning up weights

# look at range of weight values
summary(linelist$wt_kg)

# some are negative - this must be a mistake
linelist %>%
  mutate(wt_kg = ifelse(wt_kg < 0, NA, wt_kg)) %>%
  select(wt_kg) %>%
  summary()

# adding it to our pipeline

linelist <- linelist_raw %>%
  clean_names() %>%
  distinct() %>%
  rename(date_onset = onset_date,
         date_report = date_of_report,
         district_res = adm3_name_res,
         district_det = adm3_name_det) %>%
  mutate(date_onset = mdy(date_onset)) %>%
  mutate(date_report = mdy(date_report)) %>%
  mutate(sex = na_if(sex, "Unknown")) %>%
  mutate(hospital_clean = recode(hospital, 
                                 "Mitilary Hospital" = "Military Hospital",
                                 "Port Hopital" = "Port Hospital",
                                 "Port" = "Port Hospital")) %>%
  mutate(wt_kg = ifelse(wt_kg < 0, NA, wt_kg))

# more complex recoding with case_when
# mutate(NEW_COLUMN = case_when(
# LOGICAL TEST 1 ~ VALUE when LOGICAL TEST 1 is true,
# LOGICAL TEST 2 ~ VALUE when LOGICAL TEST 2 is true
# ))
# if you use TRUE as a logical test, this is always TRUE, so
# you can use TRUE to mean "for all other rows", as in the 
# example below

linelist %>%
  mutate(case_def = case_when(
    lab_confirmed == TRUE              ~ "Confirmed",
    epilink == "yes" & fever == "yes"  ~ "Suspect",
    TRUE                               ~ "To investigate"
  )) %>%
  tabyl(case_def)

# using it in a pipe where we take linelist and update its value -
# using and update a value is an alternative to making one long 
# chained pipe but it *destroys* the values that were previously in
# the variable - this is why our pipe has been going from linelist_raw
# to linelist.
# 
# this is given as an example because sometimes you are happy to 
# discard the previous values and replace them

linelist <- linelist %>%
  mutate(case_def = case_when(
    lab_confirmed == TRUE              ~ "Confirmed",
    epilink == "yes" & fever == "yes"  ~ "Suspect",
    TRUE                               ~ "To investigate"
  ))

# adding the case_when to our pipe

linelist <- linelist_raw %>%
  clean_names() %>%
  distinct() %>%
  rename(date_onset = onset_date,
         date_report = date_of_report,
         district_res = adm3_name_res,
         district_det = adm3_name_det) %>%
  mutate(date_onset = mdy(date_onset)) %>%
  mutate(date_report = mdy(date_report)) %>%
  mutate(sex = na_if(sex, "Unknown")) %>%
  mutate(hospital_clean = recode(hospital, 
                                 "Mitilary Hospital" = "Military Hospital",
                                 "Port Hopital" = "Port Hospital",
                                 "Port" = "Port Hospital")) %>%
  mutate(wt_kg = ifelse(wt_kg < 0, NA, wt_kg)) %>%
  mutate(case_def = case_when(
    lab_confirmed == TRUE              ~ "Confirmed",
    epilink == "yes" & fever == "yes"  ~ "Suspect",
    TRUE                               ~ "To investigate"
  ))

# another case_when example

linelist %>%
  mutate(age_years = case_when(
    age_unit == "months" ~ age / 12,
    age_unit == "years" ~ age,
    TRUE ~ age
  ))

# and adding this to the pipe:
linelist <- linelist_raw %>%
  clean_names() %>%
  distinct() %>%
  rename(date_onset = onset_date,
         date_report = date_of_report,
         district_res = adm3_name_res,
         district_det = adm3_name_det) %>%
  mutate(date_onset = mdy(date_onset)) %>%
  mutate(date_report = mdy(date_report)) %>%
  mutate(sex = na_if(sex, "Unknown")) %>%
  mutate(hospital_clean = recode(hospital, 
                                 "Mitilary Hospital" = "Military Hospital",
                                 "Port Hopital" = "Port Hospital",
                                 "Port" = "Port Hospital")) %>%
  mutate(wt_kg = ifelse(wt_kg < 0, NA, wt_kg)) %>%
  mutate(case_def = case_when(
    lab_confirmed == TRUE              ~ "Confirmed",
    epilink == "yes" & fever == "yes"  ~ "Suspect",
    TRUE                               ~ "To investigate"
  )) %>%
  mutate(age_years = case_when(
    age_unit == "months" ~ age / 12,
    age_unit == "years" ~ age,
    TRUE ~ age
  ))

# age binning with age_categories() from the epikit package

linelist %>%
  mutate(age_group = age_categories(age_years, 
        breakers = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 70))) %>%
  select(case_id, age_years, age_group) %>%
  head(5)

# another way of using age_categories()

linelist %>%
  mutate(age_group = age_categories(age_years,
                                    upper = 70,
                                    by = 10)) %>%
  select(case_id, age_years, age_group) %>%
  head(5)

# add age_categories

linelist <- linelist_raw %>%
  clean_names() %>%
  distinct() %>%
  rename(date_onset = onset_date,
         date_report = date_of_report,
         district_res = adm3_name_res,
         district_det = adm3_name_det) %>%
  mutate(date_onset = mdy(date_onset)) %>%
  mutate(date_report = mdy(date_report)) %>%
  mutate(sex = na_if(sex, "Unknown")) %>%
  mutate(hospital_clean = recode(hospital, 
                                 "Mitilary Hospital" = "Military Hospital",
                                 "Port Hopital" = "Port Hospital",
                                 "Port" = "Port Hospital")) %>%
  mutate(wt_kg = ifelse(wt_kg < 0, NA, wt_kg)) %>%
  mutate(case_def = case_when(
    lab_confirmed == TRUE              ~ "Confirmed",
    epilink == "yes" & fever == "yes"  ~ "Suspect",
    TRUE                               ~ "To investigate"
  )) %>%
  mutate(age_years = case_when(
    age_unit == "months" ~ age / 12,
    age_unit == "years" ~ age,
    TRUE ~ age
  )) %>%
  mutate(age_group = age_categories(age_years, 
    breakers = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 70)))  

# prioritizing values ----

# sometimes we have missing data but we can compensate with 
# data from another column. 

# take the case where you have the location where as case was recorded
# but not the home location of the person involved, you can use the
# coalesce() function from the dplyr package (part of tidyverse)
# to use the detection location where the home location is missing.

# here is an example data frame
# create the demo dataset
case_locations <- data.frame(
  case = c("A", "B", "C", "D"),
  detection = c("Ankara", "Gaziantep", NA,  "Ankara"),
  residence = c("Istanbul", "Gaziantep", "Istanbul", NA))

# print the demo dataset
case_locations

# to be able to make a map we need to have a location for every case,
# so we can use the residence where that is available, otherwise we
# can use the detection location

# re-define case_locations data frame with extra column
case_locations <- case_locations %>% 
  mutate(map_location = coalesce(detection, residence))

# print data frame
case_locations

# the results will show the new column with a location that we can
# use when mapping data

# case detection residence map_location
# 1    A    Ankara  Istanbul       Ankara
# 2    B Gaziantep Gaziantep    Gaziantep
# 3    C      <NA>  Istanbul     Istanbul
# 4    D    Ankara      <NA>       Ankara

# in linelist we have two columns with the name "district" in them,
# district_res and district_det
linelist %>% 
  select(contains("district")) %>% 
  head(5)

# we can use coalesce() and mutate() to create a new district
# column that uses the value from district_res and if that doesn't
# exist, uses that from district_det

# here it is at the end of our final data cleaning pipe

# this pipe is long, and it is useful while developing it to test
# it piece by piece but once complete it gives a reproducible
# pipeline that captures all our data cleaning decisions in one
# place

linelist <- linelist_raw %>%
  clean_names() %>%
  distinct() %>%
  rename(date_onset = onset_date,
         date_report = date_of_report,
         district_res = adm3_name_res,
         district_det = adm3_name_det) %>%
  mutate(date_onset = mdy(date_onset)) %>%
  mutate(date_report = mdy(date_report)) %>%
  mutate(sex = na_if(sex, "Unknown")) %>%
  mutate(hospital_clean = recode(hospital, 
                                 "Mitilary Hospital" = "Military Hospital",
                                 "Port Hopital" = "Port Hospital",
                                 "Port" = "Port Hospital")) %>%
  mutate(wt_kg = ifelse(wt_kg < 0, NA, wt_kg)) %>%
  mutate(case_def = case_when(
    lab_confirmed == TRUE              ~ "Confirmed",
    epilink == "yes" & fever == "yes"  ~ "Suspect",
    TRUE                               ~ "To investigate"
  )) %>%
  mutate(age_years = case_when(
    age_unit == "months" ~ age / 12,
    age_unit == "years" ~ age,
    TRUE ~ age
  )) %>%
  mutate(age_group = age_categories(age_years, 
    breakers = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 70))) %>%
  mutate(district = coalesce(district_det, district_res))

# let's save the cleaned data to disk
# the export() function will guess out format from the 
# file ending we use
# .csv - comma separated value format
# .tsv - tab separated value format
# .rds - serialised R objects
# 
# Which one you use depends on where you want to use the data
# next. Do you want to load it in a spreadsheet? Import into
# a database? Load into R again for more processing or
# visualisation?

# here we use CSV and data to data/linelist_clean.csv

export(linelist, here("data", "linelist_clean.csv"))

# not included here but also worth reading is the section
# on filtering in https://tutorials.appliedepi.org/app/tutorial04_en

