# CRL+SHIFT+R for inserting new section
# Load packages -----------------------------------------------------------

pacman::p_load(
  rio,
  here,
  janitor,
  tidyverse,
  lubridate,
  epikit
)


# Data import -------------------------------------------------------------

linelist <- import(here('data', 'raw', 'linelist_raw.csv'))

# generate a frequency table for "hospital" variable
tabyl(linelist$hospital)

# Check the class or data type of a variable
class(linelist$age)

# Creating data cleaning pipeline -----------------------------------------

linelist_processed <- linelist %>% 
  # Clean column names: changes spaces, special characters Brackets etc with '_'
  clean_names() %>%                   
  # Renaming columns 
  rename(date_of_onset = onset_date,
         admin_name_res = adm3_name_res
  ) %>%                           
  # excludes columns "lat", "lon" and "epilink" from the datasets
  select(-c(lat, lon,epilink)) %>% 
  # Re-arrange column, starting with row_num, sex, age then all the other columns
  select(row_num, sex, age, everything()) %>%
  # Remove duplicate observations
  distinct() %>% 
  # Create new columns: 
  mutate(date_of_onset=mdy(date_of_onset), # convert the date_of_onset  from character to a date
         admin_name_res_ = toupper(admin_name_res) # convert admin_name_res to a upper case
  ) %>% 
  # harmonise and the hospital column to a categorical variable
  mutate(hospital = recode(hospital,
                           "Mitilary Hospital" = "Military Hospital",
                           "Port Hopital" = "Port Hospital",
                           "St. Mark's Maternity Hospital (SMMH)" = "SMMH",
                            "Port" = "Port Hospital")) %>% 
  # create age category variables 
  mutate(age_cat = age_categories(age,  
                       breakers = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 70)
                    )
  ) %>% 
  # filter all observation with age greater than 10
  filter(age > 10)

