
############################################################################
############################################################################
###                                                                      ###
###                        CCN QUARTERLY REPORT:                         ###
###                    DATA WRANGLING AND CALCULATION                    ###
###                                                                      ###
############################################################################
############################################################################

rm(list = ls())
library(readr)
library(dplyr)
library(tidyverse)
library(data.table)
library(bannerCommenter)

###..............................................................................
###..............................................................................
###                               Instructions:                               ...
###                                                                           ...
###  1. ADD CURRENT & HISTORICAL CLIENT DATA FOR THE QUARTER TO DATA FOLDER   ...
###                             2. Run the script                             ...
###                  3. STATS WILL APPEAR IN CONSOLE WINDOW                   ...
###..............................................................................
###..............................................................................

# 1. Grab the dfs we downloaded 

Current_Clients <- read_csv("data/Current Client List - Waiting List.csv")


Historical_Clients <- read_csv("data/Historical Client List - All Levels.csv")
Historical_Clients <- slice(Historical_Clients, 1:(n() - 2)) # remove bottom two rows

# 2. Grab the columns we need

#& ID != 25113 & ID != 24983 & ID != 23128

Historical <- Historical_Clients %>% 
  filter(`Referral Source` != 'HSN CAMHP' & `Referral Source` != 'CAMHP'  & `Referral Source` != 'HSN - Crisis' & Worker !='Manager Waitlist') %>%
  select(ID, Program, City, Worker, Location = "Location/County", "First Contact", "Waiting List", "Receiving Services", "Days Waiting to Services", Discharge, `Referral Source`)

Current <- Current_Clients %>%
  filter(`Referral Source` != 'HSN CAMHP' & `Referral Source` != 'CAMHP' & `Referral Source` != 'HSN - Crisis') %>%
  select(ID, Program, City, Location = "Location/County", Worker, 'Waiting List Date', 'Days Elapsed', `Referral Source`)

# The Historical df lists all those assigned in that quarter (no longer waiting) 

# The Current df lists all those who are still waiting

# 3. Create dfs for each category we're interested in

####################################################
# 3.1 GDD Urban
#####################################################

GDD_Urban_C <- Current %>%
  filter(Program == 'GDD Urban')

GDD_Urban_H <- Historical %>%
  filter(Program == 'GDD Urban')

##########################################
# 3.2 GDD Rural
##########################################

GDD_Rural_C <- Current %>%
  filter(Program == 'GDD Rural')

GDD_Rural_H <- Historical %>%
  filter(Program == 'GDD Rural')

####################################
# 4. Counselling & Therapy
###################################

CT_C <- Current %>%
  filter(Program == 'Counselling & Therapy') 

CT_H <- Historical %>%
  filter(Program == 'Counselling & Therapy') 

## Code for Rural and Urban in both the current waiting and historical waiting lists

CT_C <- CT_C %>% 
  mutate(LOC_Coding = case_when(
    Location == "Urban - Sudbury" ~ "Urban",
    Location %in% c("Rural - Manitoulin", "Rural - Espanola", "Rural - Chapleau", "Manitoulin District") ~ "Rural",
    TRUE ~ case_when(
      City %in% c("Espanola", "Manitowaning", "CHAPLEAU", "ESPANOLA", "KAGAWONG", "Spring Bay", "M'Chigeeng", 
                  "Gore Bay", "SHEGUIANDAH", "Wikwemikong", "Little Current", "LITTLE CURRENT", "EVANSVILLE", 
                  "Kagawong", "Chapleau", "Gogama", "NAIRN CENTRE") ~ "Rural",
      Worker %in% c("April Gosselin", "Emma Chapman", "Kelsey Weisner", "Pam Yachuk", "Tara Hughes", "Tim Lemega", "Doriane Jones", "Crystal Piche") ~ "Rural",
      TRUE ~ "Urban"
    )
  ))


CT_H <- CT_H %>% 
  mutate(LOC_Coding = case_when(
    Location == "Urban - Sudbury" ~ "Urban",
    Location %in% c("Rural - Manitoulin", "Rural - Espanola", "Rural - Chapleau", "Manitoulin District") ~ "Rural",
    TRUE ~ case_when(
      City %in% c("Espanola", "Manitowaning", "CHAPLEAU", "ESPANOLA", "KAGAWONG", "Spring Bay", "M'Chigeeng", 
                  "Gore Bay", "SHEGUIANDAH", "Wikwemikong", "Little Current", "LITTLE CURRENT", "EVANSVILLE", 
                  "Kagawong", "Chapleau", "Gogama", "NAIRN CENTRE") ~ "Rural",
      Worker %in% c("April Gosselin", "Emma Chapman", "Kelsey Weisner", "Pam Yachuk", "Tara Hughes", "Tim Lemega", "Doriane Jones", "Crystal Piche") ~ "Rural",
      TRUE ~ "Urban"
    )
  ))

############################
# C&T Urban
############################

CT_H_Urban <- CT_H %>%
  filter(LOC_Coding == 'Urban') 

CT_C_Urban <- CT_C %>%
  filter(LOC_Coding == 'Urban') 

############################
# C&T Rural
############################

CT_H_Rural <- CT_H %>%
  filter(LOC_Coding == 'Rural') 

CT_C_Rural <- CT_C %>%
  filter(LOC_Coding == 'Rural') 

############# General Function to calculate all stats needed for each ddf

stats_table <- function(df1, df2, round = 1) {
  clients_assigned <- length(df1$ID)
  ave_days_waiting_assigned <- round(mean(df1$'Days Waiting to Services'), round)
  med_days_waiting_assigned <- round(median(df1$'Days Waiting to Services'), round)
  clients_waiting <- length(unique(df2$ID))
  ave_days_waiting_current <- round(mean(df2$'Days Elapsed'), round)
  med_days_waiting_current <- round(median(df2$'Days Elapsed'), round)
  
  # Create a data frame with the statistics we need
  
  stats_df <- data.frame(
    Measure = c("Clients Assigned", "Average Days Waiting (Assigned)", "Median Days Waiting (Assigned)", "Clients Waiting", "Average Days Waiting (Current)", "Median Days Waiting (Current)"),
    Value = c(clients_assigned, ave_days_waiting_assigned, med_days_waiting_assigned, clients_waiting, ave_days_waiting_current, med_days_waiting_current)
  )
  
  # Round the values in the data frame
  stats_df$Value <- round(stats_df$Value, round)
  
  # Return the data frame
  return(stats_df)
}

## Call the function for each df
## ALWAYS place the HISTORIC df in the first spot and the CURRENT df in the second spot in the function

DevAssessments_Rural <- stats_table(GDD_Rural_H, GDD_Rural_C)
DevAssessments_Urban <- stats_table(GDD_Urban_H, GDD_Urban_C)

CounsellingTherapy_Rural <- stats_table(CT_H_Rural, CT_C_Rural)
CounsellingTherapy_Urban <- stats_table(CT_H_Urban, CT_C_Urban)

###........................................................................
###........................................................................
###                                                                     ...
###                    RUN THESE LINES FOR THE STATS                    ...
###                                                                     ...
###........................................................................
###........................................................................

DevAssessments_Rural
DevAssessments_Urban
CounsellingTherapy_Rural
CounsellingTherapy_Urban
