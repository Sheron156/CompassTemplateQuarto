############################################################################
############################################################################
###                                                                      ###
###                        STRICT POWER BI SCRIPT - VII                   ###
###                                                                      ###
############################################################################
############################################################################

rm(list = ls())

library(readr)
library(dplyr)
library(readxl)
library(data.table)
library(magrittr)
library(stringr)
library(cNORM)
#library(janitor)

##################################################################
##                       Reference Tables                       ##
##################################################################

# First grab our reference tables

ID_Lookup <- read_excel("M:/DATA & REPORTS/Clinical & Client Services/Clinical Data/interRAI/interRAI Data.xlsx", sheet = "ID Lookup")

ID_Lookup <- subset(ID_Lookup, select=-c(4, 5, 10:12, 18:34)) 

# Chart Number / EMHID / CRN lookup

Table1 <- subset(ID_Lookup, select=c(1:3)) ## this one is needed

# Chart Number / ID lookup

Table4 <- subset(ID_Lookup, select=c(4:7)) ## this one is needed

Table4 <- Table4 %>% rename("Chart Number" = "Chart Number...7", "Gender" = "Gender...9")

##################################################################
##                   Detailed Program History                   ##
##################################################################

filepath <- "M:/DATA & REPORTS/Clinical & Client Services/Clinical Data/Program Histories/Detailed Program History and Client Data"

# Get a list of file paths

files <- list.files(path=filepath, pattern="*.csv", full.names=TRUE)

# Read in each file as a data frame and subset the columns we want to keep

df_list <- lapply(files, function(file) {
  df <- read.csv(file)
  cols_to_keep <- c(1,2, 3, 61, 65)
  df[, cols_to_keep]
})

# Merge the data frames 

DetailedHistory_export <- do.call(rbind, df_list)

## Rename column headers

CIS_ID1 <- DetailedHistory_export %>%
  select("Client ID#" = 1,"Year of Birth" = 2, "Gender" = 3, "Chart Number" = 4, "BI CIS Client Identifier"=5 ) %>%
  distinct()

# remove duplicate rows

CIS_ID1 <- CIS_ID1 %>%
  distinct()

############################################################################
############################################################################
###                                                                      ###
###                          IDEAS WEB                                   ###
###                                                                      ###
############################################################################
############################################################################

# Load Codebook (variable descriptions and rules to keep as well as descriptors etc)

IdeasWeb_CodeBook <- read_excel("M:\\DATA & REPORTS\\Clinical & Client Services\\Clinical Data\\interRAI\\interRAI Matching Codebook.xlsx",sheet = "ideasWeb Variables")

# Grab old Ideas Web data

interRAI_ideasWeb <- read_excel("M:/DATA & REPORTS/Clinical & Client Services/Clinical Data/interRAI/ideasWeb interRAI ChYMH Export/interRAI Data ideasWeb.xlsx", na=c("99","n/a"))

# Rename problematic columns

interRAI_ideasWeb <- interRAI_ideasWeb %>% dplyr::rename(iP22a1=iP22a1...452, iP22a2=iP22a2...453, iP22a1_OLD=iP22a1...625, iP22a2_OLD=iP22a2...626)

# grab codes of the variables we want to keep 

keep1_1 <- filter(IdeasWeb_CodeBook, KEEP==TRUE)$`iCode ideasWeb` # subset the iCode ideasWeb values
keep1_2 <- filter(IdeasWeb_CodeBook, KEEP==TRUE)$`NewCode` # subset the newvalues

# change old names to new names

keep1_2 <- make.unique(keep1_2)

df1_2 <- dplyr::select(interRAI_ideasWeb,keep1_1)

df1_3 <- setnames(df1_2, old = keep1_1, new =keep1_2)  ### should improve this method I think

df1_3$SOURCE <- "ideasWeb" #label these as ideasWeb items

# format dates

# convert the 3 previous "history of concussion" columns to 1 column

df1_3$iI9<-ifelse((df1_3$iI9+df1_3$iI9.2+df1_3$iI9.1)==0,0,
                    ifelse((df1_3$iI9+df1_3$iI9.2+df1_3$iI9.1)==1,1,
                           ifelse((df1_3$iI9+df1_3$iI9.2+df1_3$iI9.1)==2,2,
                                  ifelse((df1_3$iI9+df1_3$iI9.2+df1_3$iI9.1)>=3,3,NA))))

# convert Formal Care - Behaviour Therapist to columns to 1

df1_3$iN16s <- ifelse(is.na(df1_3$iN16s),df1_3$iN16s.1,df1_3$iN16s)

# convert Formal Care - Behaviour Therapist to columns to 1

df1_3$iF8a <- ifelse(is.na(df1_3$iF8a),df1_3$iF8a.1,df1_3$iF8a)

# convert Strengths - Strong and supportive relationship with family

df1_3$iA11b <- ifelse(is.na(df1_3$iA11b),df1_3$iA11b.1,df1_3$iA11b)

# convert Parent/Primary Caregiver has Current Developmental or Mental Health issues

df1_3$iY7a <- ifelse(is.na(df1_3$iY7a),df1_3$iY7a.1,df1_3$iY7a)

# Convert NAs to true NA: school attendance (number of days absent)

df1_3$iBB9[df1_3$iBB9 == "NA" | is.na(df1_3$iBB9)] <- NA

# format columns

df1_4_3<- df1_3 %>% mutate_at(c('iB36a','iB36b','iBB12','iBB9','iN5a','iN5b','iN5c'),as.numeric)

df1_4_3$iB2 <- as.Date(with(df1_4_3, paste(iB2b,iB2c,iB2a,sep="-")),"%m-%d-%Y") ## 

df1_4_3$iT1 <- as.Date(with(df1_4_3,paste(iT1b,iT1c,iT1a, sep="-")),"%m-%d-%Y")

#df1_4_3$survey_type<-as.numeric(dplyr::recode(df1_4_3$survey_type, 'Initial'=1, 'Monitoring Assessment'=2, 'Discharge'=3, 'Readmission Initial'=1, 'Subsequent'=2, .default=4))
 
df1_4_3 <- df1_4_3 %>% dplyr::mutate_at(c("KinarkRiskVCP","KinarkRiskHTO","KinarkRiskFB","KinarkRiskOC"), ~as.integer(dplyr::recode(.,"no"=0,"maybe"=1,"yes"=2,"superyes"=3)))
 
df1_4_3 <- df1_4_3 %>% dplyr::mutate_at(c("KinarkRiskHTP","KinarkRiskSM"), ~as.integer(dplyr::recode(.,"no"=0,"yes"=1)))
 
df1_4_3 <- df1_4_3 %>% dplyr::mutate_at(c("KinarkRiskHTS"), ~as.integer(dplyr::recode(.,"no"=0,"maybe"=1,"superyes"=2)))

df1_4_3$CaseComplexityFA <- as.character(df1_4_3$CaseComplexityFA)

df1_4_3$CaseComplexityS <- as.character(df1_4_3$CaseComplexityS)

df1_4_3$cihiU3 <- as.character(df1_4_3$cihiU3)

# format TRUE false to 1 and 0

df1_4_3 <- df1_4_3 %>% dplyr::mutate_at(c("CaseComplexityFA","CaseComplexityS", "cihiU3"), ~as.integer(dplyr::recode(.,"TRUE"=1,"FALSE"=0)))

# remove extra columns

df1_4_4 <- dplyr::select(df1_4_3,-c("iB2a","iB2b","iB2c","iI9.1","iI9.2","iN16s.1","iT1a","iT1b","iT1c","iF8a.1","iA11b.1","iY7a.1"))

df1_5 <- df1_4_4 %>% dplyr::select(order(colnames(df1_4_4)))

df1_5$iU2 <- as.Date(df1_5$iU2, "%m/%d/%Y")
df1_5$iT1 <- as.Date(df1_5$iT1, "%m/%d/%Y")
df1_5$iA9 <- as.Date(df1_5$iA9, "%m/%d/%Y")

df1_6 <- df1_5

df1_6$iA8 <- as.character(df1_6$iA8)
df1_6$iA2 <- as.character(df1_6$iA2)
df1_6$cihiA2a <- as.character(df1_6$cihiA2a)

## This needs to be properly figured out still *gender/sex/etc)
# cihiA2a is new "sex" variable (from ideasweb)

df1_6 <- df1_6 %>%
  mutate(cihiA2a = case_when(
    is.na(cihiA2a)  ~ NA,
    cihiA2a == "UN" ~ "UNK",
    cihiA2a=="M"~"M",
    cihiA2a=="F"~"F",
    TRUE ~ NA
  ))

##################################################################
##                      EMHware ChYMH DATA                      ##
##################################################################

## Grab EMHware codebook

EMHware_CodeBook <- read_excel("M:\\DATA & REPORTS\\Clinical & Client Services\\Clinical Data\\interRAI\\interRAI Matching Codebook.xlsx", sheet = "EMHware Variables")

#### Grab new ChYMH data

ChYMH_folder <- "M:/DATA & REPORTS/Clinical & Client Services/Clinical Data/interRAI/EMHware interRAI ChYMH Export"

allfiles <- list.files(path=ChYMH_folder, pattern="*.csv", full.names=TRUE)

ChYMH_export <- plyr::ldply(allfiles, read_csv, show_col_types = FALSE)

# filter all those without NA in program

ChYMH_1 <- filter(ChYMH_export,!is.na(program_name)) # remove rows w/NAs in program_name

ChYMH_1 <- subset(ChYMH_1, !grepl("default", program_name)) # remove rows w/default as program

# define variables we want to keep (leave this out for now)

#keep2_1 <- filter(EMHware_CodeBook, KEEP==TRUE)$`iCode EMHware`

#ChYMH_2<- dplyr::select(ChYMH_1,all_of(keep2_1)) # can also use "any_of" command

ChYMH_1$SOURCE<-"EMHware"

# convert all true and false to 0 and 1
# use the code below to double to subset all columns that countain the string (to double check not removing important text)
#sub <- ChYMH_export[grep("true", ChYMH_export)] 

# ChYMH_1[ChYMH_1 == 'false'] <- 0 <--- this code stopped working on 08-21-2023 / not sure why but was getting POSIX error--which was weird
# ChYMH_1[ChYMH_1 == 'true'] <- 1

# ChYMH_1[ChYMH_1 == 'FALSE'] <- 0
# ChYMH_1[ChYMH_1 == 'TRUE'] <- 1

ChYMH_1 <- ChYMH_1 %>%
  mutate(across(everything(), ~sub("true", "1", .)))
ChYMH_1 <- ChYMH_1 %>%
  mutate(across(everything(), ~sub("false", "0", .)))
ChYMH_1 <- ChYMH_1 %>%
  mutate(across(everything(), ~sub("TRUE", "1", .)))
ChYMH_1 <- ChYMH_1 %>%
  mutate(across(everything(), ~sub("FALSE", "0", .)))



# NA erroneous dates
ChYMH_1$cihiAD4c[ChYMH_1$cihiAD4c == as.Date("1900-04-08")] <- NA 
ChYMH_1$iB2[ChYMH_1$iB2 == as.Date("1900-04-08")] <- NA 
ChYMH_1$iU2[ChYMH_1$iU2 == as.Date("1900-04-08")] <- NA 

# auto format column types

ChYMH_2 <- type_convert(ChYMH_1)

ChYMH_3 <- base::transform(ChYMH_2, client_id=as.character(client_id),iA5d=as.character(iA5d))

ChYMH_3$iA8 <- gsub("1", "Initial", ChYMH_3$iA8)
ChYMH_3$iA8 <- gsub("6First", "Initial", ChYMH_3$iA8) 
ChYMH_3$iA8 <- gsub("5First", "Initial", ChYMH_3$iA8)
ChYMH_3$iA8 <- gsub("5", "Discharge", ChYMH_3$iA8)
ChYMH_3$iA8 <- gsub("2", "Reassessment", ChYMH_3$iA8)
ChYMH_3$iA8 <- gsub("6", "Discharge", ChYMH_3$iA8)
ChYMH_3$iA8 <- gsub("4", "Reassessment", ChYMH_3$iA8)

ChYMH_4 <- subset(ChYMH_3, !grepl("default", program_name))

ChYMH <- ChYMH_4
Ideas <- df1_6

Ideas <- Ideas %>% mutate_if(is.integer, as.numeric)
#Ideas$iA9 <- as.Date(Ideas$iA9, "%m/%d/%Y")
Ideas$iB2 <- as.Date(Ideas$iB2, "%m/%d/%Y")
Ideas$iT1 <- as.Date(Ideas$iT1, "%m/%d/%Y")
#Ideas$iU2 <- as.Date(Ideas$iU2, "%m/%d/%Y")
ChYMH$iA11a <- as.numeric(ChYMH$iA11a)
ChYMH$iAA1a <- as.numeric(ChYMH$iAA1a)
ChYMH$iAA1c <- as.numeric(ChYMH$iAA1c)
ChYMH$iAA1d <- as.numeric(ChYMH$iAA1d)
ChYMH$iAA1e <- as.numeric(ChYMH$iAA1e)
ChYMH$iAA1f <- as.numeric(ChYMH$iAA1f)
ChYMH$iM7 <- as.numeric(ChYMH$iM7)
ChYMH$iN6c <- as.numeric(ChYMH$iN6c)

#common <- compare_df_cols_same(ChYMH, Ideas) # Code to check if any variables differ in format (each matching variable must have same format)

# ChYMH$client_id <- as.character(ChYMH$iN6c)
# ChYMH$iN6c <- as.numeric(ChYMH$iN6c)
# Join dfs

Combined <- dplyr::bind_rows(ChYMH, Ideas) # join the dfs


#Combined <- Combined %>%
#  filter(type_cd %in% c("chymh_op", "chymh_s_plus", "chymh_dd_op", "chymh_monitoring_op"))  # DOUBLEL CHECK THIS WITH NICK
rm(ChYMH_1, ChYMH_2, ChYMH_3, ChYMH_4, df1_2, df1_3, df1_4_3, df1_4_4, df1_5, df1_6)

############################################################################
############################################################################
###                                                                      ###
###                          CALCULATED COLUMNS                          ###
###                                                                      ###
############################################################################
############################################################################

##----------------------------
##   1. Assessment tool used  
##----------------------------

Combined <- mutate(Combined, Tool = if_else(is.na(instrument), 
                                            if_else(type_cd == "chymh_s_plus", "rscr", "cymh"), 
                                            instrument))
# rscr = screener
# chymh = ChYMH

##------------------------------------------------------------------------
##  2. Type of assessment (Initial, Reassessment, Monitoring, Discharge)   ASK NICK!!!!
##------------------------------------------------------------------------

Combined <- mutate(Combined, 
                   Type = if_else(is.na(type_cd),
                                  if_else(survey_type == "Readmission Initial", "Initial", survey_type), 
                                  if_else(type_cd == "chymh_monitoring_op", "Monitoring Assessment", 
                                          if_else(oA8 == "S-INTL", "Initial", 
                                                  if_else(oA8 == "S-SQNT", "Reassessment", # formerly "subsequent"
                                                          if_else(iA8 == "2", "Reassessment", iA8))))))
# iA8 = reason for assessment

Combined <- mutate(Combined, 
                   Type = if_else(is.na(Type), iA8, Type))

Combined$Type <- gsub("2", "Reassessment", Combined$Type) 

Combined$Type <- gsub("3", "Initial", Combined$Type) 

##----------------------
##  3. Reference date  
##---------------------

# iA9 = assessment reference date in EMHware
# reference_d = the same in Ideas web

Combined$ref <- Combined$iA9

#Combined$ref <- ifelse(is.na(Combined$iA9), Combined$reference_d, Combined$iA9)

##---------------------------------
##  4. Last assessment completed      ## MODIFY THIS
##---------------------------------

# if iT1 and iU2 are blank grab complete_d, if only iU2 is blank grab iT1, otherwise iU2 

Combined <- mutate(Combined,
                   compl = if_else(is.na(iU2), iT1, iU2))

# iT1 = last day of involvement w/program or agency
# iU2 = date assessment completed
# complete_d = date assessment completed IdeasWeb

##-------------------
##  5. CREATE EMHID                                  
##-------------------

CIS_ID1$'Client ID#' <- as.character(CIS_ID1$'Client ID#')
CIS_ID1$`BI CIS Client Identifier` <- as.character(CIS_ID1$`BI CIS Client Identifier`)
Table4$ID <- as.character(Table4$ID)

# If client_id has a valid value, return CIS_ID1$client ID#, otherwise match client_id with BI CIS Client 

Combined$EMHID <- ifelse(!is.na(Combined$client_id),
                         CIS_ID1$`Client ID#`[match(Combined$client_id, CIS_ID1$`BI CIS Client Identifier`)],
                         NA)

# check if there is a match between Combined$crn and Table1$CRN, and if there is, return the corresponding value from Table1$'EMHID Actual'
Combined$EMHID <- ifelse(is.na(Combined$EMHID) & !is.na(Combined$client_id),
                         Table1$`EMHID Actual`[match(Combined$client_id, Table1$CRN)],
                         Combined$EMHID)

# check if there is a match between Combined$iA5a and Table4$'Chart Number', and if there is, return the corresponding value from Table4$ID
Combined$EMHID <- ifelse(is.na(Combined$EMHID) & !is.na(Combined$iA5d),
                         Table4$ID[match(Combined$iA5d, Table4$`Chart Number`)],
                         Combined$EMHID)

# check if there is a match between Combined$iA5a and Table4$ID, and if there is, return the corresponding value from Table4$ID

Combined$EMHID <- ifelse(is.na(Combined$EMHID) & !is.na(Combined$iA5d),
                         Table4$ID[match(Combined$iA5d, Table4$ID)],
                         Combined$EMHID)

##---------------------
##  6. Calculate Age   
##---------------------

# grab all birth_years 

Combined$birth_year <- ifelse(!is.na(match(Combined$EMHID, CIS_ID1$"Client ID#")),
                              CIS_ID1$'Year of Birth'[match(Combined$EMHID, CIS_ID1$"Client ID#")],
                              Combined$iA3)

# Convert Combined$ref to a date format

#Combined$ref <- as.Date(Combined$ref)

# Now calculate age in years at the given reference date (Combined$ref)

Combined$Age <- as.integer((Combined$ref - as.Date(paste0(Combined$birth_year, "-01-01")))/365.25)

# Now age as continuous 

Combined$AgeN <- as.numeric((Combined$ref - as.Date(paste0(Combined$birth_year, "-01-01")))/365.25)

##------------------------------------------
##  7. Calculate FirstChYMH and last ChYMH  
##------------------------------------------

Combined <- Combined %>%
  group_by(EMHID) %>%
  mutate(MinRef = min(if_else(Tool != "rscr", ref, as.Date(Inf)), na.rm = FALSE),
         MaxRef = max(if_else(Tool != "rscr", ref, as.Date(-Inf)), na.rm = FALSE),
         FirstChYMH = as.integer(ref == MinRef),
         LastChYMH = as.integer(ref == MaxRef)) %>%
  ungroup() %>%
  select(-MinRef, -MaxRef)

##-------------------------------------------------------------------
##  8. For FirstAx and last AX variable (get clarification on this)  
##-------------------------------------------------------------------

Combined <- Combined %>%
  group_by(EMHID) %>%
  mutate(MinRef = min(ref, na.rm = FALSE),
         MaxRef = max(ref, na.rm = FALSE),
         FirstAx = as.integer(ref == MinRef),
         LastAx = as.integer(ref == MaxRef)) %>%
  ungroup() %>%
  select(-MinRef, -MaxRef)

##-----------------------------
##  9. First & last  Screener  
##-----------------------------
 
Combined <- Combined %>%
  group_by(EMHID) %>%
  mutate(MinRef = min(if_else(Tool == "rscr", ref, as.Date(Inf)), na.rm = FALSE),
         MaxRef = max(if_else(Tool == "rscr", ref, as.Date(-Inf)), na.rm = FALSE),
         FirstScreener = as.integer(ref == MinRef),
         LastScreener = as.integer(ref == MaxRef)) %>%
  ungroup() %>%
  select(-MinRef, -MaxRef)

#test <- Combined %>% select(EMHID, Tool, ref, FirstChYMH, LastChYMH, FirstAx, LastAx, FirstScreener, LastScreener)

##------------------
##  10. DSI_Scale 
##------------------

Combined <- Combined %>% mutate(validdsi = case_when(
  iE1ss %in% c(0,1,2,3,4) &
    iE1vv %in% c(0,1,2,3,4) &
    iE1ww %in% c(0,1,2,3,4) &
    iE1xx %in% c(0,1,2,3,4) &
    iE1yy %in% c(0,1,2,3,4) ~ 1, TRUE ~ 0))

Combined <- Combined %>% mutate(DSI__scale = case_when(validdsi == 1 ~ 
                                         (case_when(iE1ss %in% c(0,1,2,3) ~ + iE1ss, iE1ss == 4 ~ + 3)) +
                                         (case_when(iE1vv %in% c(0,1,2,3) ~ + iE1vv, iE1vv == 4 ~ + 3)) +
                                         (case_when(iE1ww %in% c(0,1,2,3) ~ + iE1ww, iE1ww == 4 ~ + 3)) +
                                         (case_when(iE1xx %in% c(0,1,2,3) ~ + iE1xx, iE1xx == 4 ~ + 3)) +
                                         (case_when(iE1yy %in% c(0,1,2,3) ~ + iE1yy, iE1yy == 4 ~ + 3))))

library(cNORM)


dsi.model <- readRDS("M:/DATA & REPORTS/Clinical & Client Services/Clinical Data/Models/dsimodel.rda")

Combined$DSI_norm <- predictNorm(Combined$DSI__scale, Combined$AgeN, dsi.model, minNorm = 30, maxNorm = 71)

test <- Combined %>% select(EMHID, AgeN, DSI__scale, DSI_norm)
##------------------
##  11. ANX Scores  
##------------------

Combined <- Combined %>% mutate(validnxs = case_when(
  iE1eee %in% c(0,1,2,3,4) & 
    iE1fff %in% c(0,1,2,3,4) & 
    iE1ggg %in% c(0,1,2,3,4) & 
    iE1iii %in% c(0,1,2,3,4) & 
    iE1kkk %in% c(0,1,2,3,4) & 
    iE1lll %in% c(0,1,2,3,4) & 
    iE1eeee %in% c(0,1,2,3,4) ~ 1, TRUE ~ 0))

Combined <- Combined %>% mutate(ANX__scale = case_when(validnxs == 1 ~ 
                                          iE1eee + iE1fff + iE1ggg + iE1iii + iE1kkk + iE1lll + iE1eeee))

anx.model <- readRDS("M:/DATA & REPORTS/Clinical & Client Services/Clinical Data/Models/anxmodel.rda")

Combined$ANX_norm <- predictNorm(Combined$ANX__scale, Combined$AgeN, anx.model, minNorm = 30, maxNorm = 80)

##--------------
##  12. CHAMHP  
##--------------

Combined <- Combined %>%
  mutate(CHAMHPS__scale =
           ifelse(
             Age <= 7,
             ifelse(
               iX14a == 0,
               ifelse(
                 iX2a %in% c(0, 1, 2),
                 ifelse(iE1rrr %in% c(0, 1), 0,
                        ifelse(iE1rrr %in% c(2, 3, 4), 2, NA)),
                 ifelse(iX2a %in% c(3, 4, 5), 2, NA)),
               ifelse(
                 iX14a == 1,
                 ifelse(iX2a %in% c(0, 1), 1,
                        ifelse(iX2a %in% c(2, 3, 4, 5), 3, NA)),
                 ifelse(iX14a %in% c(2, 3, 4),
                        ifelse(iE1lll == 0, 3,
                               ifelse(
                                 iE1lll %in% c(1, 2, 3, 4), 5, NA
                               )), NA))),
             ifelse(
               Age >= 8 & Age <= 11,
               ifelse(
                 iX14a %in% c(0, 1),
                 ifelse(
                   iX14b %in% c(0, 1),
                   ifelse(
                     iE3n %in% c(0, 1, 2),
                     ifelse(iEEE2 %in% c(0, 1), 0,
                            ifelse(iEEE2 %in% c(2, 3, 4), 2, NA)),
                     ifelse(iE3n %in% c(3, 4), 4, NA)
                   ),
                   ifelse(
                     iX14b == 2,
                     ifelse(iE1qq %in% c(0, 1, 2), 1,
                            ifelse(iE1qq %in% c(3, 4), 3, NA)),
                     ifelse(iX14b %in% c(3, 4), 4, NA)
                   )),
                 ifelse(
                   iX14a == 2,
                   ifelse(iE1vv %in% c(0, 1, 2), 3,
                          ifelse(iE1vv %in% c(3, 4), 5, NA)),
                   ifelse(iX14a %in% c(3, 4), 5, NA)
                 )),
               ifelse(Age >= 12,
                      ifelse(
                        iX14a %in% c(0, 1),
                        ifelse(
                          iX14b %in% c(0, 1),
                          ifelse(
                            iE1iii %in% c(0, 1, 2),
                            ifelse(
                              iE1xx %in% c(0, 1, 2),
                              ifelse(
                                iY1ao %in% c(0, 1, 2),
                                ifelse(iX1d == 0, 1, 2),
                                ifelse(iY1ao %in% c(3, 4, 5), 3, NA)
                              ),
                              ifelse(iE1xx %in% c(3, 4), 3, NA)
                            ),
                            ifelse(iE1iii %in% c(3, 4), 3, NA)
                          ),
                          ifelse(iX14b == 2,
                                 3,
                                 ifelse(iX14b %in% c(3, 4),
                                        5,
                                        NA))),
                        ifelse(
                          iX14a == 2,
                          ifelse(
                            iX1c %in% c(0, 1, 2, 3, 4),
                            ifelse(
                              iBB5f %in% c(0, 8),
                              ifelse(iX2a %in% c(0, 1),
                                     3,
                                     ifelse(iX2a %in% c(2, 3, 4, 5),
                                            4,
                                            NA)),
                              ifelse(iBB5f == 1, 4, NA)
                            ),
                            ifelse(iX1c == 5, 4, NA)
                          ),
                          ifelse(iX14a %in% c(3, 4),
                                 ifelse(
                                   iEEE2 %in% c(0, 1),
                                   ifelse(iE1qqq == 0,
                                          5,
                                          ifelse(iE1qqq %in% c(1, 2, 3, 4),
                                                 6,
                                                 NA)),
                                   ifelse(iEEE2 %in% c(2, 3, 4),
                                          6,
                                          NA)),
                                 NA))),NA))))

#test <- Combined %>% filter(Age >= 12 & iX14a >=3) %>% select(EMHID,Age, iX14a, iBB5f, iEEE2, iE1qqq, CHAMHPS__scale)

##------------------
##  13. DABS scale  
##------------------

Combined <- Combined %>% mutate(validdabs = case_when( 
  iE3l %in% c(0,1,2,3,4) &
    iE3m %in% c(0,1,2,3,4) &
    iE3n %in% c(0,1,2,3,4) &
    iE3q %in% c(0,1,2,3,4) &
    iE3r %in% c(0,1,2,3,4) ~ 1, TRUE ~ 0))

Combined <- Combined %>% mutate(DABS__scale = case_when(validdabs == 1 ~
                                          iE3l + iE3m + iE3n + iE3q + iE3r))

dabs.model <- readRDS("M:/DATA & REPORTS/Clinical & Client Services/Clinical Data/Models/dabsmodel.rda")

Combined$DABS_norm <- predictNorm(Combined$DABS__scale, Combined$AgeN, dabs.model, minNorm = 15, maxNorm = 95)


##--------------
##  ABS__scale  
##--------------

Combined <- Combined %>% mutate(validabs = case_when(
  iE3l %in% c(0,1,2,3,4) & 
    iE3m %in% c(0,1,2,3,4) & 
    iE3n %in% c(0,1,2,3,4) & 
    iE3p %in% c(0,1,2,3,4) ~ 1,
  TRUE ~ 0)) 

Combined <- Combined %>% mutate(ABS__scale = case_when(validabs==1 ~ 
                                                         (case_when(iE3l %in% c(0,1,2,3) ~ iE3l, iE3l == 4 ~ + 3)) + 
                                                         (case_when(iE3m %in% c(0,1,2,3) ~ + iE3m, iE3m == 4 ~ + 3)) +
                                                         (case_when(iE3n %in% c(0,1,2,3) ~ + iE3n, iE3n == 4 ~ + 3)) + 
                                                         (case_when(iE3p %in% c(0,1,2,3) ~ + iE3p, iE3p == 4 ~ + 3))))

abs.model <- readRDS("M:/DATA & REPORTS/Clinical & Client Services/Clinical Data/Models/absmodel.rda")

Combined$ABS_norm <- predictNorm(Combined$`ABS__scale`, Combined$AgeN, abs.model, minNorm = 20, maxNorm = 95)

##-------------------
##  14. SOCDIS scale: sCYMHANHEDO
##-------------------

Combined <- Combined %>% mutate(validedo = case_when(
  iE1qqq %in% c(0,1,2,3,4) &                                            
    iE1rrr %in% c(0,1,2,3,4) &                                                 
    iE1sss %in% c(0,1,2,3,4) &                                              
    iE1ttt %in% c(0,1,2,3,4) ~ 1, TRUE ~ 0))

Combined <- Combined %>% mutate('SOCDIS__scale' = case_when(validedo == 1 ~
                                              iE1qqq + iE1rrr + iE1sss + iE1ttt))

socdis.model <- readRDS("M:/DATA & REPORTS/Clinical & Client Services/Clinical Data/Models/socdismodel.rda")
Combined$SOCDIS_norm <- predictNorm(Combined$SOCDIS__scale, Combined$AgeN, socdis.model, minNorm = 35, maxNorm = 95)



##-----------------------
##  15. DISTRACT_scale   
##-----------------------

Combined <- Combined %>% mutate(validact = case_when(
  iE1oo %in% c(0,1,2,3,4) &
    iE1pp %in% c(0,1,2,3,4)&
    iE1qq %in% c(0,1,2,3,4)&
    iE1rr %in% c(0,1,2,3,4) ~ 1, TRUE ~ 0))

Combined <- Combined %>% mutate(DISTRACT__scale = case_when(validact == 1 ~ (
  iE1oo +iE1pp +iE1qq + iE1rr)))

distract.model <- readRDS("M:/DATA & REPORTS/Clinical & Client Services/Clinical Data/Models/distractmodel.rda")

Combined$DISTRACT_norm <- predictNorm(Combined$`DISTRACT__scale`, Combined$AgeN, distract.model, minNorm = 15, maxNorm = 99)

##--------------------------
##  16. KinarkRiskHTO_comb  
##--------------------------

Combined$KinarkRiskHTO_comb <- Combined$KinarkRiskHTO

##--------------------------
##  17. KinarkRiskHTS_comb  
##--------------------------

Combined$KinarkRiskHTS_comb <- Combined$KinarkRiskHTS

##--------------------------
##  18. KinarkRiskHTP_comb  
##--------------------------

Combined$KinarkRiskHTP_comb<- Combined$KinarkRiskHTP

##-------------------------
##  19. KinarkRiskSM_comb  
##-------------------------

Combined$KinarkRiskSM_comb <- Combined$KinarkRiskSM

##--------------------------
##  20. KinarkRiskVCP_comb  
##--------------------------
Combined$KinarkRiskVCP_comb<- Combined$KinarkRiskVCP

##--------------------------
##  21. KinarkRiskFB_comb  
##--------------------------

Combined$KinarkRiskFB_comb <- Combined$KinarkRiskFB

##-------------------------
##  22. KinarkRiskOC_comb  
##-------------------------
Combined$KinarkRiskOC_comb <- Combined$KinarkRiskOC

##-------------
##  23. Acute  
##-------------

Combined$Acute <- Combined$CaseComplexityFA

##---------------
##  24. Complex  
##---------------

Combined$Complex <- Combined$CaseComplexityS

##-------------------------
##  25. KinarkRiskHTO_cat  
##-------------------------

Combined <- Combined %>%
  mutate(KinarkRiskHTO_cat = case_when(
    KinarkRiskHTO == 0 ~ "No Risk",
    KinarkRiskHTO == 1 ~ "Potential Risk",
    KinarkRiskHTO == 2 ~ "Risk Identified",
    KinarkRiskHTO == 3 ~ "Serious Risk"
  ))

##-------------------------
##  26. KinarkRiskHTS_cat  
##-------------------------

Combined <- Combined %>%
  mutate(KinarkRiskHTS_cat = case_when(
    KinarkRiskHTS  == 0 ~ "No Risk",
    KinarkRiskHTS  == 1 ~ "Potential Risk",
    KinarkRiskHTS  == 2 ~ "Risk Identified",
    KinarkRiskHTS  == 3 ~ "Serious Risk"
  ))

##------------------------
##  27. KinarkRiskSM_cat  
##------------------------

Combined <- Combined %>%
  mutate(KinarkRiskSM_cat = case_when(
    is.na(KinarkRiskSM )  ~ NA,
    KinarkRiskSM  == 0 ~ "No Risk",
    KinarkRiskSM  == 1 ~ "Potential Risk",
    KinarkRiskSM  == 2 ~ "Risk Identified",
    KinarkRiskSM  == 3 ~ "Serious Risk"
  ))

##------------------------
##  28. KinarkRiskSM_cat  
##------------------------

Combined <- Combined %>%
  mutate(KinarkRiskHTP_cat = case_when(
    is.na(KinarkRiskHTP )  ~ NA,
    KinarkRiskHTP  == 0 ~ "No Risk",
    KinarkRiskHTP  == 1 ~ "Potential Risk",
    KinarkRiskHTP  == 2 ~ "Risk Identified",
    KinarkRiskHTP  == 3 ~ "Serious Risk"
  ))

##-------------------------
##  29. KinarkRiskVCP_cat  
##-------------------------

Combined <- Combined %>%
  mutate(KinarkRiskVCP_cat = case_when(
    is.na(KinarkRiskVCP )  ~ NA,
    KinarkRiskVCP  == 0 ~ "No Risk",
    KinarkRiskVCP  == 1 ~ "Potential Risk",
    KinarkRiskVCP  == 2 ~ "Risk Identified",
    KinarkRiskVCP  == 3 ~ "Serious Risk"
  ))

##------------------------
##  30. KinarkRiskFB_cat  
##------------------------

Combined <- Combined %>%
  mutate(KinarkRiskFB_cat = case_when(
    is.na(KinarkRiskFB )  ~ NA,
    KinarkRiskFB  == 0 ~ "No Risk",
    KinarkRiskFB  == 1 ~ "Potential Risk",
    KinarkRiskFB  == 2 ~ "Risk Identified",
    KinarkRiskFB  == 3 ~ "Serious Risk"
  ))

##------------------------
##  31. KinarkRiskOC_cat  
##------------------------

Combined <- Combined %>%
  mutate(KinarkRiskOC_cat = case_when(
    is.na(KinarkRiskOC )  ~ NA,
    KinarkRiskOC  == 0 ~ "No Risk",
    KinarkRiskOC  == 1 ~ "Potential Risk",
    KinarkRiskOC  == 2 ~ "Risk Identified",
    KinarkRiskOC  == 3 ~ "Serious Risk"
  ))

##-----------------
##  32. Acute_cat  
##-----------------

Combined <- Combined %>%
  mutate(Acute_cat = case_when(
    is.na(Acute)  ~ NA,
    Acute == 1 ~ "Yes",
    Acute == 0 ~ "No"
  ))

##-------------------
##  33. Complex_cat  
##-------------------

Combined <- Combined %>%
  mutate(Complex_cat = case_when(
    is.na(Complex)  ~ NA,
    Complex == 1 ~ "Yes",
    Complex == 0 ~ "No"
  ))

##-----------------------------------------------
##  34.  ED_Screen, ID_Screen & Comorbid screen  
##-----------------------------------------------

Combined$ED_Screen <- Combined$iW29

Combined$ID_Screen <- Combined$iA47

Combined$ComorbidMedical_Screen <- Combined$iA48  

##------------------
##  43. Positive Symptoms Scale  
##------------------

Combined <- Combined %>% mutate(validpss = case_when(iE1mmm %in% c(0,1,2,3,4) & 
                                                       iE1ooo %in% c(0,1,2,3,4) & 
                                                       iE1nnn %in% c(0,1,2,3,4) & 
                                                       iE1ppp %in% c(0,1,2,3,4) ~ 1,
                                                     TRUE ~ 0))

Combined <- Combined %>% mutate(PSS__scale = case_when(validpss == 1 ~ 
                                                     (case_when(iE1mmm %in% c(0,1,2,3) ~ iE1mmm, iE1mmm == 4 ~ + 3)) + 
                                                     (case_when(iE1ooo %in% c(0,1,2,3) ~ + iE1ooo, iE1ooo == 4 ~ + 3)) +
                                                     (case_when(iE1nnn %in% c(0,1,2,3) ~ + iE1nnn, iE1nnn == 4 ~ + 3)) +
                                                    (case_when(iE1ppp %in% c(0,1,2,3) ~ + iE1ppp, iE1ppp == 4 ~ + 3)))) 
Combined$sCYPSS <- Combined$PSS__scale

pss.model <- readRDS("M:/DATA & REPORTS/Clinical & Client Services/Clinical Data/Models/pssmodel.rda")

Combined$PSS_norm <- predictNorm(Combined$`PSS__scale`, Combined$AgeN, pss.model, minNorm = 30, maxNorm = 95)

##---------------
##  36.  Trauma  
##---------------

Combined <- Combined %>%
  mutate(Trauma = case_when(
    is.na(iY1b)  ~ NA,
    iY1b == 1 ~ "Caused Intense Fear",
    iY1b == 0 ~ "No or Not Applicable",
    iY1b == 8 ~ "Did Not Respond"
  ))


##--------------------
##  37.  RISSK_scale  
##--------------------

Combined <- Combined %>% mutate(validssk = case_when(
  iE1ss %in% c(0:4) &
    iE1tt %in% c(0:4) &
    iE1vv %in% c(0:4) &
    iE1ww %in% c(0:4) &
    iE1xx %in% c(0:4) &
    iE1yy %in% c(0:4) &
    iE1aaa %in% c(0:4) &
    iE1rrr %in% c(0:4) &
    iE1ttt %in% c(0:4) &
    iE3w %in% c(0:4) &
    iP19h %in% c(0,1,8) &
    iX1a %in% c(0:5) &
    iX1b %in% c(0,1,8) & 
    iX1c %in% c(0:5) &
    iX1d %in% c(0,1) ~ 1, TRUE ~ 0),
  xdsi = iE1ss + iE1tt + iE1vv + iE1ww + iE1xx +iE1yy + iE1aaa + iE1rrr + iE1ttt)

Combined <- Combined %>% mutate(RISSK__scale = case_when(validssk == 1 ~ 
                                           (case_when(iX1c == 0 ~
                                                        case_when(iX1d == 0 ~ 
                                                                    case_when(iX1a == 0 ~ 
                                                                                case_when(iE3w == 0 ~ 
                                                                                            case_when (xdsi >= 0 & xdsi <= 17 ~ 0,TRUE ~ 1),TRUE ~ 1),TRUE ~ 3)),
                                                      iX1c %in% c(1,2,3) ~ 
                                                        case_when(iX1a %in% c(0,1,2) ~ 
                                                                    case_when(iX1b %in% c(0,8) ~ 
                                                                                case_when(xdsi %in% c(0,1) ~ 1, 
                                                                                          xdsi >= 2 & xdsi <= 12 ~ 2, 
                                                                                          xdsi >= 13 & xdsi <= 36 ~ 3),TRUE ~ 
                                                                                case_when(iX1d == 0 ~ 2),TRUE ~ 4)),
                                                      iX1c == 4 ~ 
                                                        case_when(iX1d == 0 ~ 3,TRUE ~ 
                                                                    case_when(iX1b == 0 ~ 
                                                                                case_when(iP19h == 0 ~ 4, TRUE ~ 5),TRUE ~ 5)),
                                                      iX1c == 5 ~ 
                                                        case_when(iX1b %in% c(0,8) ~ 
                                                                    case_when (iX1d == 0 ~ 3,TRUE ~ 5),iX1a %in% c(0,1,2,3) ~ 5,TRUE ~ 6)))))

rissk.model <- readRDS("M:/DATA & REPORTS/Clinical & Client Services/Clinical Data/Models/risskmodel.rda")

Combined$`RISSK_norm` <- predictNorm(Combined$`RISSK__scale`, Combined$AgeN, rissk.model, minNorm = 30, maxNorm = 80)


##-----------------
##  40. RIO_scale  
##-----------------

Combined <- Combined %>% mutate(validrio = case_when(
  iE1oo %in% c(0,1,2,3,4) &
    iE3l %in% c(0,1,2,3,4) &
    iE3m %in% c(0,1,2,3,4) & 
    iE3n %in% c(0,1,2,3,4) & 
    iE3q %in% c(0,1,2,3,4) &
    iP19h %in% c(0,1,8) &
    iX2a %in% c(0,1,2,3,4,5) &
    iX2b %in% c(0,1,2,3,4,5) &
    iX2c %in% c(0,1,2,3,4,5) ~ 1, TRUE ~ 0))

Combined <- Combined %>% mutate(xrio = case_when(validrio == 1 ~ iX2a + iX2b + iX2c))

Combined <- Combined %>% mutate(xbeh = case_when(validrio ==1 ~ 
                                       (case_when(iE3l %in% c(2,3,4) ~ iE3l-1) +
                                          case_when(iE3n %in% c(2,3,4) ~ iE3n-1))))

Combined <- Combined %>% mutate(RIO__scale = case_when(validrio == 1 ~ 
                                         (case_when(iE3m == 0 ~ 
                                                      (case_when(xrio == 0 ~ 
                                                                   case_when(iE3q & iE3n %in% c(1,2,3,4) ~ 1,TRUE ~ 0))) +
                                                      (case_when(xrio %in% c(1,2) ~ 1, TRUE ~ 0)) +
                                                      (case_when(xrio %in% c(3,4) ~ 3, TRUE ~ 0)) +
                                                      (case_when(xrio >= 5 ~ 3, TRUE ~ 0)), TRUE ~ 0) +
                                            case_when(iE3m == 1 ~ 
                                                        (case_when(xrio %in% c(0,1) ~ 
                                                                     ((case_when(iE3q == 0 ~ 1, TRUE ~ 0)) +
                                                                        case_when(iE3q >= 1 ~ 
                                                                                    (case_when(iE1oo == 0 ~ 1, TRUE ~ 0) +
                                                                                       (case_when(iE1oo %in% c(1,2,3) ~ 2, TRUE ~ 0)) +
                                                                                       (case_when(iE1oo == 4 ~ 3, TRUE ~ 0))))), TRUE ~ 0) +
                                                           case_when(xrio %in% c(2,3,4,5) ~ 3, TRUE ~ 0) +
                                                           case_when(xrio >= 6 ~ case_when(iE3q & iE3n %in%
                                                                                            c(1,2,3,4) ~ 5,TRUE ~ 4), TRUE ~ 0)),
                                                      TRUE ~ 0) +
                                            case_when(iE3m == 2 ~
                                                        (case_when(xrio >= 0 & xrio <= 6 ~ 
                                                                     (case_when(xbeh %in% c(0,1) ~ 3, TRUE ~ 0) +
                                                                        case_when(xbeh >= 2 ~ 4,TRUE ~ 5)), TRUE ~ 0)), TRUE ~ 0) +
                                            case_when(iE3m == 3 ~ 
                                                        (case_when(xrio %in% c(0,1,2,3,4) ~ 3, TRUE ~ 0) +
                                                           case_when(xrio >= 5 ~ case_when(iP19h %in% c(0,8) ~ 4,TRUE ~ 5))), 
                                                      TRUE ~ 0) +
                                            case_when(iE3m == 4 ~ 
                                                        (case_when(xrio %in% c(0,1,2) ~ 4, TRUE ~ 0) +
                                                           case_when(xrio >= 3 & xrio <= 8 ~ 5, TRUE ~ 0)+ 
                                                           case_when(xrio >= 9 ~ 6, TRUE ~ 0)), TRUE ~ 0))))
##---------------------------------------
##  41. SOS__scale SEVERITY OF SELF HARM  
##---------------------------------------

Combined <- Combined %>% mutate(validsos = case_when(
  iC1d %in% c(0,1,2,3,4,5) & #  decision making
    iC2a %in% c(0,1) & #  short term memory (brooklyn had iC2a) <-- look this up
    iD1 %in% c(0,1,2,3,4) &  # understoof
    iE1mmm %in% c(0,1,2,3,4) &  #hallucinations
    iE1nnn %in% c(0,1,2,3,4) & # command hallucinations
    iE1ooo %in% c(0,1,2,3,4) & # delusions
    iE1ppp %in% c(0,1,2,3,4) & # Abnormal thought processes
    iE1ss %in% c(0,1,2,3,4) & # Sad facial expressions
    iE1vv %in% c(0,1,2,3,4) & # negative statements
    iE1ww %in% c(0,1,2,3,4) & # self-deprecating
    iE1xx %in% c(0,1,2,3,4) & # guilt or shame
    iE1yy %in% c(0,1,2,3,4) & # hopelessness
    iG2j %in% c(0,1,2,3,4,5,6,8) & #Eating
    iX1b %in% c(0,1,8) & # suicidal intent
    iX1c %in% c(0,1,2,3,4,5) & # considered self-injury
    iX1d %in% c(0,1) & #others concerned about self-injury
    iX1e %in% c(0,1) ~ 1, TRUE ~ 0)) # suicide plan


Combined <- Combined %>% mutate(xcsp1 = case_when(
  iC1d %in% c(0,1,2,3,4,5) &
    iD1 %in% c(0,1,2,3,4) &
    iC2a %in% c(0,1) ~ 1, TRUE ~ 0))


Combined <- Combined %>% mutate(xcps2 = case_when(
  iC1d %in% c(0,1,2,3,4,5) &
    iD1 %in% c(0,1,2,3,4) ~ 1, TRUE ~ 0))


# Impairment count

Combined <- Combined %>% mutate(xcps1 = case_when(validsos == 1 ~
                                                    (
                                                      case_when(iC1d %in% c(1, 2, 3) ~ 1, TRUE ~ 0) +
                                                        case_when(iD1 %in% c(1, 2, 3, 4) ~ 1, TRUE ~ 0) +
                                                        case_when(iC2a == 1 ~ +1, TRUE ~ 0)
                                                    )))

# Severe Imp count

Combined <- Combined %>% mutate(xcps2 = case_when(validsos == 1 ~
                                                    (
                                                      case_when(iC1d == 3 ~ 1, TRUE ~ 0) +
                                                        case_when(iD1 %in% c(3, 4) ~ +1, TRUE ~ 0)
                                                    )))
# cog performance count

Combined <- Combined %>% mutate(xcps = case_when(validsos == 1 ~
                                                   (
                                                     case_when(iC1d ==  5 ~ 6, TRUE ~ 0) +
                                                       case_when(iC1d == 4 ~ (
                                                         case_when(iC1d %in% c(6, 8) ~ 6, TRUE ~ 0) +
                                                           case_when(iG2j %in% c(0, 1, 2, 3, 4, 5) ~ 5, TRUE ~ 0)
                                                       ), TRUE ~ 0) +
                                                       case_when(iC1d >= 0 &
                                                                   iC1d < 4 ~ (
                                                                     case_when(xcps1 %in% c(2, 3) ~
                                                                                 (
                                                                                   case_when(xcps2 == 2 ~ 4, TRUE ~ 0) +
                                                                                     case_when(xcps2 == 1 ~ 3, TRUE ~ 0) +
                                                                                     case_when(xcps2 == 0 ~ 2, TRUE ~ 0)
                                                                                 ), TRUE ~ 0) +
                                                                       case_when(xcps1 == 1 ~ 1, TRUE ~ 0) +
                                                                       case_when(xcps1 == 0 ~ 0, TRUE ~ 0)
                                                                   )))))
# Depressive Severity count

Combined <- Combined %>% mutate(xdsi = case_when(
  validsos == 1 ~
    (
      case_when(iE1vv == 0 ~ 0, iE1vv %in% c(1, 2) ~ 1, iE1vv %in% c(3, 4) ~ 2) +
        case_when(iE1ss == 0 ~ 0, iE1ss %in% c(1, 2) ~ 1, iE1ss %in% c(3, 4) ~ 2) +
        case_when(iE1ww == 0 ~ 0, iE1ww %in% c(1, 2) ~ 1, iE1ww %in% c(3, 4) ~ 2) +
        case_when(iE1xx == 0 ~ 0, iE1xx %in% c(1, 2) ~ 1, iE1xx %in% c(3, 4) ~ 2) +
        case_when(iE1yy == 0 ~ 0, iE1yy %in% c(1, 2) ~ 1, iE1yy %in% c(3, 4) ~ 2)
    )
))

# Positive Sumptoms count


Combined <- Combined %>% mutate(xpss = case_when(validsos == 1 ~
                                                   (case_when(iE1mmm == 0 ~ +0, iE1mmm %in% c(1, 2) ~ +1, iE1mmm %in% c(3, 4) ~ +2) +
                                                       case_when(iE1nnn == 0 ~ +0, iE1nnn %in% c(1, 2) ~ +1, iE1nnn %in% c(3, 4) ~ +2) +
                                                       case_when(iE1ooo == 0 ~ +0, iE1ooo %in% c(1, 2) ~ +1, iE1ooo %in% c(3, 4) ~ +2) +
                                                       case_when(iE1ppp == 0 ~ +0, iE1ppp %in% c(1, 2) ~ +1, iE1ppp %in% c(3, 4) ~ +2)
                                                   )))

## CY Severity of Self Harm Scale

Combined <-
  Combined %>% mutate(SOS__scale = case_when(validsos == 1 ~
                                               (case_when(iX1c == 5 ~
                                                             (case_when(iX1b == 1 ~
                                                                           (case_when(xdsi >= 6 ~ 6, TRUE ~ 0) +
                                                                               case_when(xdsi < 6 ~ 5, TRUE ~ 0)), TRUE ~ 0) +
                                                                 case_when(iX1b %in% c(0, 8) ~(
                                                                       case_when(xdsi >= 6 & iX1e == 1 ~ 5, TRUE ~ 0) +
                                                                         case_when(xdsi >= 6 &
                                                                                     iX1e == 0 ~ 3, TRUE ~ 0) +
                                                                         case_when(xdsi < 6 &
                                                                                     iX1d == 1 ~ 4, TRUE ~ 0) +
                                                                         case_when(xdsi < 6 &
                                                                                     iX1d == 0 ~ 2, TRUE ~ 0)),
                                                                   TRUE ~ 0)), TRUE ~ 0) +
                                                   case_when(iX1c %in% c(3, 4) ~ (
                                                                 case_when(iX1b == 1 ~ 4, TRUE ~ 0) +
                                                                   case_when(iX1b %in% c(0, 8) ~ 3, TRUE ~ 0)
                                                               ), TRUE ~ 0) +
                                                   case_when(iX1c %in% c(0, 1, 2) ~(
                                                                 case_when(iX1b == 1 ~
                                                                             (case_when(xdsi >= 4 ~ 4, TRUE ~ 0) +
                                                                                 case_when(xdsi < 4 ~ 1, TRUE ~ 0)), TRUE ~ 0) +
                                                                   case_when(
                                                                     iX1b %in% c(0, 8) ~(
                                                                         case_when(xpss >= 3 & xcps >= 3 ~ 3, TRUE ~ 0) +
                                                                           case_when(xpss >= 3 & xcps < 3 ~ 2, TRUE ~ 0) +
                                                                           case_when(xpss < 3 & iX1d == 1 ~ 2, TRUE ~ 0) +
                                                                           case_when(xpss < 3 &
                                                                                       iX1d == 0 &
                                                                                       xcps >= 1 ~ 1, TRUE ~ 0) +
                                                                           case_when(xpss < 3 &
                                                                                       iX1d == 0 &
                                                                                       xcps == 0 ~ 0, TRUE ~ 0)),TRUE ~ 0)), TRUE ~ 0))))

Combined$sCYSOS <- Combined$SOS__scale

sos.model <- readRDS("M:/DATA & REPORTS/Clinical & Client Services/Clinical Data/Models/sosmodel.rda")

Combined$SOS_norm <- predictNorm(Combined$SOS__scale, Combined$AgeN, sos.model, minNorm = 15, maxNorm = 90)


##-----------------
##  42. RHO_scale  
##-----------------

# old rho calculation
#Combined$RHO__scale <- ifelse(is.na(Combined$sCYRHO), ifelse(is.na(Combined$rhoscale_Scale), NA, Combined$rhoscale_Scale), Combined$sCYRHO)

Combined <- Combined %>% mutate(validrho = case_when(
  iE1ooo %in% c(0,1,2,3,4) & 
    iE1yyy %in% c(0,1,2,3,4) & 
    iE6 %in% c(0,1,2,3,4,5) & 
    iE7 %in% c(0,1,2) & 
    iX2a %in% c(0,1,2,3,4,5) & 
    iX2b %in% c(0,1,2,3,4,5) & 
    iX2c %in% c(0,1,2,3,4,5) &
    ABS__scale %in% c(0:12) &
    PSS__scale %in% c(0:12) & 
    iX4 %in% c(0,1,2) ~ 1,
  TRUE ~ 0))


Combined <- Combined %>% mutate(xrho1 = case_when(
  iX2a %in% c(4,5) | 
    iX2b %in% c(4,5) | 
    iX2c %in% c(4,5) | 
    iX4 == 2 ~ 2,
  iX2a %in% c(1,2,3) |
    iX2b %in% c(1,2,3) | 
    iX2c %in% c(1,2,3) | 
    iX4 == 1 ~ 1,
  TRUE ~ 0),
  xrho2 = iX2a + iX2b + iX2c)

Combined <- Combined %>% mutate(RHO__scale = case_when(validrho == 1 ~ case_when(
  xrho1 == 2 ~ case_when(
    xrho2 >= 10 ~ case_when(
      iE1ooo %in% c(1,2,3,4) ~ 6, iE1ooo == 0 ~ 5),
    xrho2 <10 ~ case_when(
      iE7 == 2 ~ 5, iE7 == 1 & iE1yyy %in% c(1,2,3,4) ~ 4,
      iE7 == 1 & iE1yyy == 0 ~ 2, iE7 == 0 ~ 3)),
  xrho1 == 1 ~ case_when(PSS__scale >= 2 ~ 4, PSS__scale < 2 ~ 2),
  xrho1 == 0 ~ case_when(ABS__scale >= 3 ~ 3, ABS__scale < 3 ~ case_when(
    PSS__scale >= 4 ~ 1, PSS__scale < 4 & iE6 %in% c(2,3,4,5) ~ 1,
    PSS__scale < 4 & iE6 %in% c(0,1) ~ 0)))))

Combined$sCYRHO <- Combined$RHO__scale

##------------------
##  43. PAIN_scale  
##------------------

#Combined$PAIN__scale <- ifelse(is.na(Combined$sPAIN), ifelse(is.na(Combined$pain_Scale), NA, Combined$pain_Scale), Combined$sPAIN)


##--------------------
##  44. IADLC__scale  
##--------------------

Combined <- Combined %>% mutate(validiadlc = case_when(
  iG1ab %in% c(0,1,2,3,4,5,6) & 
    iG1bb %in% c(0,1,2,3,4,5,6) & 
    iG1cb %in% c(0,1,2,3,4,5,6) & 
    iG1db %in% c(0,1,2,3,4,5,6) & 
    iG1eb %in% c(0,1,2,3,4,5,6) & 
    iG1fb %in% c(0,1,2,3,4,5,6) & 
    iG1gb %in% c(0,1,2,3,4,5,6) & 
    iG1hb %in% c(0,1,2,3,4,5,6) & 
    iG1nb %in% c(0,1,2,3,4,5,6) & 
    iG1ob %in% c(0,1,2,3,4,5,6) & 
    iZZ1ab %in% c(0,1,2,3,4,5,6) &
    Age >= 0 & Age <= 130 ~ 1, TRUE ~ 0))

Combined <- Combined %>% mutate(IADLC__scale = case_when(validiadlc == 1 ~ case_when(
  Age >= 12 & Age < 19 ~ 
    iG1ab + iG1bb + iG1cb + iG1db + iG1eb + iG1fb + iG1gb + iG1hb + iG1nb + iG1ob
  + iZZ1ab)))

iadlc.model <- readRDS("M:/DATA & REPORTS/Clinical & Client Services/Clinical Data/Models/iadlcmodel.rda")

Combined$IADLC_norm <- predictNorm(Combined$IADLC__scale, Combined$AgeN, iadlc.model, minNorm = 40, maxNorm = 80)

##-----------------------------------------------
##  sCYPS PARENTING STRENGTHS (for RICHY calc) -- 
##-----------------------------------------------

Combined <- Combined %>% mutate(validps = case_when(
  iP15a %in% c(0,1,2) &
    iP15b %in% c(0,1,2) &
    iP15c %in% c(0,1,2) &
    iP15d %in% c(0,1,2) &
    iP15e %in% c(0,1,2) &
    iP15f %in% c(0,1,2) ~ 1, TRUE ~ 0))

Combined <- Combined %>% 
  mutate(PARENTING__scale = 
           case_when(validps == 1 ~ (case_when(iP15a == 0 ~ + 2, iP15a == 1 ~ + 1, iP15a == 2 ~ + 0)) + 
                       (case_when(iP15b == 0 ~ + 2, iP15b == 1 ~ + 1, iP15b == 2 ~ + 0)) +
                      (case_when(iP15c == 0 ~ + 2, iP15c == 1 ~ + 1, iP15c == 2 ~ + 0)) +
                      (case_when(iP15d == 0 ~ + 2, iP15d == 1 ~ + 1, iP15d == 2 ~ + 0)) +
                      (case_when(iP15e == 0 ~ + 2, iP15e == 1 ~ + 1, iP15e == 2 ~ + 0)) +
                      (case_when(iP15f == 0 ~ + 2, iP15f == 1 ~ + 1, iP15f == 2 ~ + 0))))

Combined <- Combined %>% 
  mutate(PARENTING__scalerev = 
           case_when(validps == 1 ~ (case_when(iP15a == 0 ~ + 0, iP15a == 1 ~ + 1, iP15a == 2 ~ + 2)) + 
                       (case_when(iP15b == 0 ~ + 0, iP15b == 1 ~ + 1, iP15b == 2 ~ + 2)) +
                       (case_when(iP15c == 0 ~ + 0, iP15c == 1 ~ + 1, iP15c == 2 ~ + 2)) +
                       (case_when(iP15d == 0 ~ + 0, iP15d == 1 ~ + 1, iP15d == 2 ~ + 2)) +
                       (case_when(iP15e == 0 ~ + 0, iP15e == 1 ~ + 1, iP15e == 2 ~ + 2)) +
                       (case_when(iP15f == 0 ~ + 0, iP15f == 1 ~ + 1, iP15f == 2 ~ + 2))))

parenting.model <- readRDS("M:/DATA & REPORTS/Clinical & Client Services/Clinical Data/Models/parentingmodel.rda")

Combined$PARENTING_norm <- predictNorm(Combined$PARENTING__scalerev, Combined$AgeN, parenting.model, minNorm = 25, maxNorm = 95)

##--------------------
##  CAREDIS__scale  Caregiver Distress Algorithm --
##--------------------

Combined <- Combined %>% mutate(ProactiveItem = 
                      (case_when(iE15a== 0 ~ + 0, iE15a %in% c(1,2,3,4,5) ~ + 1)) +
                      (case_when(iE15d== 0 ~ + 0, iE15d %in% c(1,2,3,4,5) ~ + 1)) + 
                      (case_when(iE15e== 0 ~ + 0, iE15e %in% c(1,2,3,4,5) ~ + 1)) + 
                      (case_when(iX16c== 0 ~ + 0, iX16c %in% c(1,2,3,4,5) ~ + 1)) +        
                      (case_when(iX2a== 0 ~ + 0, iX2a %in% c(1,2,3,4,5) ~ + 1)) +           
                      (case_when(iX2b== 0 ~ + 0, iX2b %in% c(1,2,3,4,5) ~ + 1)) +           
                      (case_when(iX2c== 0 ~ + 0, iX2c %in% c(1,2,3,4,5) ~ + 1)))

Combined <- Combined %>% mutate(devMHsubst_Issues = 
                      (case_when(iY7a & iY1ap == 0 ~ 0, TRUE ~ 0) +
                         case_when(iY7a == 1 ~ 1, TRUE ~ 0) +
                         case_when(iY1ap %in% c(1,2,3,4,5) ~ 1)))

Combined <- Combined %>% mutate(familyfunction = 
                      (case_when(iF8a == 1 ~ 1, TRUE ~ 0) +
                         case_when(iF8a == 0 ~  0, TRUE ~ 0) + 
                         case_when(iP19d | iP19e == 1 ~  1, TRUE ~ 0) + 
                         case_when(iP19d & iP19e == 0 ~  0, TRUE ~ 0) + 
                         case_when(iY7a & iY1ap == 0 ~  0, TRUE ~ 0) + 
                         case_when(iY7a == 1 | iY1ap %in% c(1,2,3,4,5) ~  1, TRUE ~ 0) + 
                         case_when(iY7b == 1 ~  1, TRUE ~ 0) + 
                         case_when(iY7b == 0 ~  0, TRUE ~ 0)))

Combined <- Combined %>% mutate(validiccared = case_when(
  iJ5a %in% c(0,1,2,3) & 
    iE1zzz %in% c(0,1,2,3,4) & 
    iE1ccc %in% c(0,1,2,3,4) & 
    iY9 %in% c(0,1) & 
    iX1a %in% c(0,1,2,3,4,5) & 
    iE1hhh %in% c(0,1,2,3,4) & 
    iP19a %in% c(0,1,8) & 
    iE1tt %in% c(0,1,2,3,4) & 
    iY1au %in% c(0,1,2,3,4,5) & 
    iE1ss %in% c(0,1,2,3,4) & 
    iE6 %in% c(0,1,2,3,4,5) & 
    iC9b %in% c(0,1,8) & 
    iE18a %in% c(0,1) &
    PARENTING__scale %in% c(0:12) & 
    PSS__scale %in% c(0:12) & 
    DABS__scale %in% c(0:20) ~ 1, TRUE ~ 0))

Combined <- Combined %>% mutate(CAREDIS__scale = case_when(validiccared == 1 ~ 
                                  (case_when(ProactiveItem == 0 ~ case_when(
                                            PARENTING__scale == 12 ~ case_when(
                                              ABS__scale == 0 ~ case_when(
                                                devMHsubst_Issues == 0 ~ case_when(
                                                  iE1ccc == 0 ~ 1, iE1ccc %in% c(1,2,3,4) ~ 2),
                                                devMHsubst_Issues == 1 ~ 2),
                                              ABS__scale >= 1 ~ case_when(
                                                iC9a == 0 ~ case_when(
                                                  iY9 == 0 ~ 1, iY9 == 1 ~ 2),
                                                iC9a %in% c(1,8) ~ case_when(
                                                  iX1a == 0 ~ 2, iX1a %in% c(1,2,3,4,5) ~ 3))),
                                            PARENTING__scale %in% c(10,11) ~ case_when(
                                              devMHsubst_Issues == 0 ~ 1, 
                                              devMHsubst_Issues == 1 ~ 3),
                                            PARENTING__scale %in% c(0,1,2,3,4,5,6,7,8,9) ~ case_when(
                                              iJ5a == 0 ~ case_when(
                                                iE1zzz == 0 ~ 2, iE1zzz %in% c(1,2,3,4) ~ 3),
                                              iJ5a %in% c(1,2,3) ~ 4)), TRUE ~ 0) +
                                             
                                             case_when(ProactiveItem %in% c(1,2) ~ case_when(
                                               familyfunction == 0 ~ case_when(
                                                 sCYDABS %in% c(0,1,2,3,4,5,6,7) ~ case_when(
                                                   iY1au == 0 ~ 1, iY1au %in% c(1,2,3,4,5) ~ 3),
                                                 sCYDABS >= 8 ~ case_when(
                                                   iE1ss == 0 ~ 1, iE1ss %in% c(1,2,3,4) ~ 3)),
                                               familyfunction == 1 ~ case_when(
                                                 iE1hhh == 0 ~ case_when(
                                                   iY9 == 0 ~ 2, iY9 == 1 ~ 4),
                                                 iE1hhh %in% c(1,2,3,4) ~ 4),
                                               familyfunction >= 2 ~ case_when(
                                                 iP19a == 0 ~ case_when(
                                                   iE1ttt == 0 ~ 2, iE1ttt %in% c(1,2,3,4) ~ 3),
                                                 iP19a == 1 ~ 4)), TRUE ~ 0) + 
                                             
                                             case_when(ProactiveItem %in% c(3,4,5,6,7) ~ case_when(
                                               PARENTING__scale %in% c(7,8,9,10,11,12) ~ case_when(
                                                 iE6 %in% c(1,2,3,4,5) ~ 5, 
                                                 iE6 == 0 ~ case_when(
                                                   devMHsubst_Issues == 0 ~ case_when(
                                                     iX1a %in% c(0,1) ~ case_when(
                                                       sCYDABS %in% c(0,1,2,3,4,5,6,7,8,9,10) ~ 2, 
                                                       sCYDABS >= 11 ~ 4),
                                                     iX1a %in% c(2,3,4,5) ~ 4),
                                                   devMHsubst_Issues == 1 ~ case_when(
                                                     iE18a == 1 ~ 3, 
                                                     iE18a == 0 ~ 5))),
                                               PARENTING__scale %in% c(0,1,2,3,4,5,6) ~ case_when(
                                                 iC9b == 0 ~ 4, iC9b %in% c(1,8) ~ 5)), TRUE ~ 0))))

##-----------------
##  COMM__scale  --
##-----------------

Combined <- Combined %>% mutate(validcomm = case_when(
  iD1 %in% c(0,1,2,3,4) & 
    iD2 %in% c(0,1,2,3,4) ~ 1, TRUE ~ 0))

Combined <- Combined %>% mutate(COMM__scale = case_when(validcomm == 1 ~ iD1 + iD2))

##--------------------------------------------
##   CAS / Psychiatrist / Family Engagment  --
##--------------------------------------------

Combined <- Combined %>%
  mutate(CAS = case_when(
    is.na(iN16w)  ~ NA,
    iN16w > 0 ~ "Involved"
  ))

Combined <- Combined %>%
  mutate(Psychiatrist = case_when(
    is.na(iN16o)  ~ NA,
    iN16w > 0 ~ "Involved"
  ))

Combined <- Combined %>%
  mutate(FamilyEngaged = case_when(
    is.na(iZ10)  ~ NA,
    iZ10 > 0 ~ "Involved"
  ))

##------------------------
##   Physical Activity  --
##------------------------

Combined <- Combined %>% 
  mutate(cCYMHPA = case_when(
  iG6a %in% c(0,1) ~ 1, TRUE ~ 0))


Combined$'Physical Activity' <-
  ifelse(is.na(Combined$cCYMHPA),
    NA,
      ifelse(
        Combined$cCYMHPA == 1,
        "Triggered",
        ifelse(Combined$cCYMHPA == 0,
            "Not Triggered",
            NA
          )))

##------------------------
##  Attachment          --
##------------------------

 Combined <-  Combined %>% mutate(validattach = case_when(
  iB33 %in% c(0,1,2) & 
    iB34a %in% c(0,1,2,3) & 
    iB34b %in% c(0,1,2,3) & 
    iB35 %in% c(0,1) &
    iP16a %in% c(0,1,8) & 
    iP16b %in% c(0,1,8) & 
    iY1am %in% c(0,1,2,3,4,5) & 
    iY1an %in% c(0,1,2,3,4,5) & 
    iY1ao %in% c(0,1,2,3,4,5) & 
    iY1ar %in% c(0,1,2,3,4,5) & 
    iY1at %in% c(0,1,2,3,4,5) & 
    iY1au %in% c(0,1,2,3,4,5) & 
    iY7a %in% c(0,1) & 
    T_age <= 12 ~ 1, TRUE ~ 0))

 Combined <-  Combined %>% mutate(xcymhattach1 = case_when(validattach == 1 ~ 
                                               (case_when(iB33 == 2 ~ + 1, TRUE ~ 0) +
                                                  case_when(iB34a %in% c(1,2) ~ + 1, TRUE ~ 0) +
                                                  case_when(iB34b %in% c(1,2) ~ + 1, TRUE ~ 0) +
                                                  case_when(iB34c %in% c(1,2) ~ + 1, TRUE ~ 0) +
                                                  case_when(iB35 == 1 ~ + 1, TRUE ~ 0) +
                                                  case_when(iY1am %in% c(1,2,3,4,5) ~ + 1, TRUE ~ 0) +
                                                  case_when(iY1an %in% c(1,2,3,4,5) ~ + 1, TRUE ~ 0) +
                                                  case_when(iY1ao %in% c(1,2,3,4,5) ~ + 1, TRUE ~ 0) +
                                                  case_when(iY1ap %in% c(1,2,3,4,5) ~ + 1, TRUE ~ 0) +
                                                  case_when(iY1ar %in% c(1,2,3,4,5) ~ + 1, TRUE ~ 0) +
                                                  case_when(iY1at %in% c(1,2,3,4,5) ~ + 1, TRUE ~ 0) +
                                                  case_when(iY1au %in% c(1,2,3,4,5) ~ + 1, TRUE ~ 0) +
                                                  case_when(iY7a == 1 ~ + 1, TRUE ~ 0))))

 Combined <-  Combined %>% mutate(xcymhattach2 = case_when(validattach == 1 ~ 
                                               (case_when(iP16a == 1 ~ + 1, TRUE ~ 0) +
                                                  case_when(iP16b ==1 ~ + 1, TRUE ~ 0) +
                                                  case_when(iP16c == 1 ~ + 1, TRUE ~ 0))))
 
  Combined <-  Combined %>% mutate(Attachment = case_when( 
   xcymhattach1 >= 1 & xcymhattach2 >= 1 ~ 1, 
   xcymhattach1 == 0 | xcymhattach2 == 0 ~ 0))

  
  ##################################################################
  ##                         Caffeine Use                         ##
  ##################################################################
  
 Combined <-  Combined %>% mutate('Caffeine Use' = case_when(
    iW10b %in% c(2,3) ~ 1, 
    iW10b < 2 ~ 0))

  Combined$'Caffeine Use'[Combined$'Caffeine Use'== 1] <- "Triggered"
  Combined$'Caffeine Use'[Combined$'Caffeine Use'== 0] <- "Not Triggered"


  
  #################################################################
  ##                      Caregiver Distress                      ##
  #################################################################

   Combined <-  Combined %>% mutate(xcymhcaredist = case_when(iB32a | iB32b == 1 ~ + 1, TRUE ~ 0) +
                                                     case_when(iP19g == 1 ~ + 1, TRUE ~ 0) +
                                                     case_when(iQ4 == 1 ~ + 1, TRUE ~ 0) +
                                                     case_when(iY1ap %in% c(2,3,4,5) ~ + 1, TRUE ~ 0) +
                                                     case_when(iY7a == 1 ~ + 1, TRUE ~ 0) +
                                                     case_when(iY8a == 1 ~ + 1, TRUE ~ 0) +
                                                     case_when(iY8b == 1 ~ + 1, TRUE ~ 0) +
                                                     case_when(iY9 == 1 ~ + 1, TRUE ~ 0))
  
 Combined <- Combined %>% mutate('Caregiver Distress' = case_when(
    xcymhcaredist >= 2 ~ 1, TRUE ~ 0))
  
 Combined$'Caregiver Distress'[Combined$'Caregiver Distress'== 1] <- "Triggered"
 Combined$'Caregiver Distress'[Combined$'Caregiver Distress'== 0] <- "Not Triggered"
 
 #################################################################
 ##                        Communication    : cCYMHCOMM                    ##
 #################################################################
 
 
  Combined <-  Combined %>% mutate('Communication' = case_when(
   iD1 %in% c(2,3,4) | iD2 %in% c(2,3,4) ~ 1, 
   iD1 < 2 & iD2 < 2 ~ 0))
 
 Combined$'Caregiver Distress'[Combined$'Caregiver Distress'== 1] <- "Triggered"
 Combined$'Caregiver Distress'[Combined$'Caregiver Distress'== 0] <- "Not Triggered"
 
 #################################################################
 ##                    Control Interventions                    ##
 #################################################################
 
  Combined <-  Combined %>% mutate(validctrlint = case_when(
   iE1nnn %in% c(0,1,2,3,4) & 
     iM7 %in% c(0,1,2,3,4,5,6,7,8,9) & 
     iN6c %in% c(0,1,2,3,4,5) & 
     iX1a %in% c(0,1,2,3,4,5) & 
     iX2a %in% c(0,1,2,3,4,5) & 
     iX4 %in% c(0,1,2) & 
     iAA1a %in% c(0,1,2,3,4,5) & 
     iAA1c %in% c(0,1,2,3,4,5) & 
     iAA1f %in% c(0,1,2,3,4,5) &
     sCYABS %in% c(0:12) & 
     sCYPSS %in% c(0:12) ~ 1, TRUE ~ 0))
 
  Combined <-  Combined %>% mutate(xcymhctrlint1 = case_when(validctrlint == 1 ~ 
                                                 (case_when(iM7 %in% c(1,2,3,4,5,6,7,8,9) ~ + 1, TRUE ~ 0) +
                                                    case_when(iN6c %in% c(1,2,3,4,5) ~ + 1, TRUE ~ 0) +
                                                    case_when(iAA1a %in% c(1,2,3,4,5) ~ + 1, TRUE ~ 0) +
                                                    case_when(iAA1c %in% c(1,2,3,4,5) ~ + 1, TRUE ~ 0) + 
                                                    case_when(iAA1f %in% c(1,2,3,4,5) ~ + 1, TRUE ~ 0))))
 
  Combined <-  Combined %>% mutate(xcymhctrlint2 = case_when(validctrlint == 1 ~ 
                                                 (case_when(iE1nnn %in% c(2,3,4) ~ + 1, TRUE ~ 0) + 
                                                    case_when(iX1a == 5 ~ + 1, TRUE ~ 0) +
                                                    case_when(iX2a == 5 ~ + 1, TRUE ~ 0) + 
                                                    case_when(iX4 == 2 ~ + 1, TRUE ~ 0) + 
                                                    case_when(sCYABS >= 5 ~ + 1, TRUE ~ 0) +
                                                    case_when(sCYPSS >= 8 ~ + 1, TRUE ~ 0))))
  
  Combined <- Combined %>% mutate('Control Interventions'= case_when(validctrlint == 1 ~ 
                                                 (case_when(xcymhctrlint1 & xcymhctrlint2 >= 1 ~ 2, TRUE ~ 0) +
                                                    case_when(xcymhctrlint1 >= 1 & xcymhctrlint2 == 0 ~ 1, TRUE ~ 0)))) 
  
  Combined$'Control Interventions'[Combined$'Control Interventions'== 1] <- "Triggered to minimize the need for control interventions (young persons not in a psychiatric emergency situation)"
  Combined$'Control Interventions'[Combined$'Control Interventions'== 0] <- "Not Triggered"
  
  ##################################################################
  ##                    Criminality Prevention                    ##
  ##################################################################
  
  
   Combined <-  Combined %>%
    mutate('Criminality Prevention' = case_when(
    iE1dddd %in% c(1,2,3,4) | iE15a %in% c(2,3,4,5) | iX2a %in% c(2,3,4,5) ~ 1, 
    iE1dddd < 1 & iE15a < 2 & iX2a < 2 ~ 0))

  Combined$'Criminality Prevention'[Combined$'Criminality Prevention'== 1] <- "Triggered"
  Combined$'Criminality Prevention'[Combined$'Criminality Prevention'== 0] <- "Not Triggered"
  
  
  #################################################################
  ##                          Education: cCYMHEDU                         
  #################################################################
  
   Combined <-  Combined %>% mutate(xiBB5a = case_when(
    iBB4c %in% c(2,3) ~ as.numeric(iBB5a), 
    iBB4c == 1 ~ 0))
  
   Combined <-  Combined %>% mutate(xiBB5b = case_when(
    iBB4c %in% c(2,3) ~ as.numeric(iBB5b), 
    iBB4c == 1 ~ 0))
  
   Combined <-  Combined %>% mutate(xiBB5e = case_when(
    iBB4c %in% c(2,3) ~ as.numeric(iBB5e), 
    iBB4c == 1 ~ 0))
  
   Combined <-  Combined %>% mutate(xiBB5f = case_when(
    iBB4c %in% c(2,3) ~ as.numeric(iBB5f), 
    iBB4c == 1 ~ 0))
  
   Combined <-  Combined %>% mutate(xiBB10 = case_when(
    iBB4c %in% c(2,3) ~ as.numeric(iBB10), 
    iBB4c == 1 ~ 0))
  
   Combined <-  Combined %>% mutate(xiBB11a = case_when(
    iBB4c %in% c(1,2,3) ~ as.numeric(iBB11a)))
  
   Combined <-  Combined %>% mutate(xiBB11b = case_when(
    iBB4c %in% c(1,2,3) ~ as.numeric(iBB11b)))
  
   Combined <-  Combined %>% mutate(xiBB14a = case_when(
    iBB4c %in% c(1,2,3) ~ as.numeric(iBB14a)))
   
    Combined <-  Combined %>% mutate(validedu = case_when(
     iBB2 %in% c(0,1,2,3,8) &
       xiBB5a %in% c(0,1,8) & 
       xiBB5b %in% c(0,1,8) & 
       xiBB5e %in% c(0,1,8) & 
       xiBB5f %in% c(0,1,8) &
       xiBB10 %in% c(0,1,2,3) & 
       xiBB11a %in% c(0,1) & 
       xiBB11b %in% c(0,1) & 
       xiBB14a %in% c(0,1,2,3,8) ~ 1, TRUE ~ 0))
   
    Combined <-  Combined %>% mutate(xcymhedu1 = case_when(validedu == 1 ~ 
                                               (case_when(xiBB5a == 1 ~ + 1, TRUE ~ 0) +
                                                  case_when(xiBB5b == 1 ~ + 1, TRUE ~ 0) +
                                                  case_when(xiBB5e == 1 ~ + 1, TRUE ~ 0) + 
                                                  case_when(xiBB5f == 1 ~ + 1, TRUE ~ 0) + 
                                                  case_when(xiBB10 %in% c(1,3) ~ + 1, TRUE ~ 0) +
                                                  case_when(xiBB14a %in% c(2,3) ~ + 1, TRUE ~ 0))))
   
    Combined <-  Combined %>% mutate(xcymhedu2 = case_when(validedu == 1 ~ 
                                               (case_when(xiBB11a | xiBB11b == 1 ~ + 1, TRUE ~ 0) + 
                                                  case_when(iBB2 != 0 & iBB2 != 1 & iBB2 != 2 & iBB2 != 3 ~ + 1, TRUE ~ 0)))) 
   
   
     Combined <-  Combined %>% mutate(Education = case_when(validedu == 1 ~ case_when(
      xcymhedu1 >= 1 ~ "Triggered due to risk of dropping out of school",  
      xcymhedu1 == 0 ~ case_when(
        xcymhedu2 == 2 ~ "Triggered due to current disrupted education", 
        xcymhedu2 < 2 ~ "Not Triggered"))))
   
##################################################################
##                  Hazardous Fire Involvement                  ##
##################################################################

 Combined <-  Combined %>% mutate(validfire = case_when(
  iC1d %in% c(0,1,2,3,4,5) & 
    iE1oo %in% c(0,1,2,3,4) & 
    iE1mmm %in% c(0,1,2,3,4) & 
    iE1nnn %in% c(0,1,2,3,4) & 
    iE1ooo %in% c(0,1,2,3,4) & 
    iP15e %in% c(0,1,2,8) & 
    iX18 %in% c(0,1,2,3,4,5) & 
    iY1am %in% c(0,1,2,3,4,5) & 
    iY1an %in% c(0,1,2,3,4,5) & 
    iY1ao %in% c(0,1,2,3,4,5) ~ 1, TRUE ~ 0))

 Combined <- Combined %>% mutate(xcymhfire = case_when(validfire == 1 ~ case_when(
  iC1d %in% c(3,4) ~ + 1, 
  iE1oo %in% c(2,3,4) ~ + 1,
  iE1mmm %in% c(2,3,4) ~ + 1, 
  iE1nnn %in% c(2,3,4) ~ + 1, 
  iE1ooo %in% c(2,3,4) ~ + 1, 
  iP15e %in% c(2,8) ~ + 1, 
  iY1am %in% c(1,2,3,4,5) | iY1an %in% c(1,2,3,4,5) | 
    iY1ao %in% c(1,2,3,4,5) ~ + 1)))
 
  Combined <-  Combined %>% mutate(`Hazardous Fire Involvement` = case_when(validfire == 1 ~ case_when(
   iX18 %in% c(2,3,4,5) ~ case_when(
     xcymhfire >= 2 ~ "Triggered at high risk", 
     xcymhfire == 1 ~ "Triggered at moderate risk", 
     xcymhfire == 0 ~ "Triggered at low risk"), 
   iX18 < 2 ~ "Not Triggered")))

  ##################################################################
  ##                           Gambling                           ##
  ##################################################################
  
   Combined <-  Combined %>% mutate(validgamb = case_when(
    iW8 %in% c(0,1) &
      cihiU3 == 1 ~ 1, TRUE ~ 0))
   
  Combined <-  Combined %>% mutate(Gambling = case_when(validgamb == 1 ~ case_when(
    iW8 == 1 ~ "Triggered", 
    iW8 < 1 ~ "Not Triggered")))
  
  ##################################################################
  ##                        Harm to Others                        ##
  ##################################################################
  
  Combined <- Combined %>% mutate(validharmoth = case_when(
    iE1oo %in% c(0,1,2,3,4) & 
      sCYRHO %in% c(0,1,2,3,4,5,6) ~ 1, TRUE ~ 0))
 
   Combined <- Combined %>% mutate('Harm to Others' = case_when(validharmoth == 1 ~ case_when(
    sCYRHO %in% c(5,6) ~ 2, 
    sCYRHO %in% c(3,4) ~ case_when(
      iE1oo %in% c(2,3,4) ~ 2, 
      iE1oo < 2 ~ 1),
    sCYRHO <= 2 ~ 0)))
   
   Combined$'Harm to Others'[Combined$'Harm to Others' == 2] <- "Triggered due to high risk of harm to others"
   Combined$'Harm to Others'[Combined$'Harm to Others' == 1] <- "Triggered due to moderate risk of harm to others"
   Combined$'Harm to Others'[Combined$'Harm to Others' == 0] <- "Not Triggered"

   
   ##################################################################
   ##                       Informal Support                       ##
   ##################################################################
   
   Combined <- Combined %>% mutate(xcymhinfsupp =  case_when(iP18a %in% c(2,3) ~ + 1, TRUE ~ 0) +
                                                     case_when(iP18b == 3 ~ + 1, TRUE ~ 0) +
                                                     case_when(iP18c == 3 ~ + 1, TRUE ~ 0) +
                                                     case_when(iP18d == 3 ~ + 1, TRUE ~ 0) +
                                                     case_when(iP18e == 3 ~ + 1, TRUE ~ 0))
   Combined <- Combined %>% mutate('Informal Support' = case_when(xcymhinfsupp >= 2 ~ 1, TRUE ~ 0) + 
                                                     case_when(xcymhinfsupp < 2 ~ 0, TRUE ~ 0))
   
   Combined$'Informal Support'[Combined$'Informal Support' == 1] <- "Triggered"
   Combined$'Informal Support'[Combined$'Informal Support' == 0] <- "Not Triggered"

   
   ##################################################################
   ##                    Interpersonal Conflict                    ##
   ##################################################################
   
   Combined <- Combined %>% mutate(xiBB5e = (case_when(
     iBB4c %in% c(0,1) ~ 0)+ 
       case_when(iBB4c %in% c(2,3) ~ as.numeric(iBB5e), TRUE ~ 0)))
   
   Combined <- Combined %>% mutate(validipcon = case_when(
     iP19a %in% c(0,1,8) &
       iP19b %in% c(0,1,8) &
       iP19c %in% c(0,1,8) &
       iP19d %in% c(0,1,8) &
       iP19e %in% c(0,1,8) &
       iP19f %in% c(0,1,8) &
       xiBB5e %in% c(0,1,2,3) & 
       iBB5e %in% c(0,1,8) ~ 1, TRUE ~ 0))
   
   Combined <- Combined %>% mutate(xcymhipcon = case_when(validipcon == 1 ~ 
                                                (case_when(iP19a | iP19d == 1 ~ + 1, TRUE ~ 0) +
                                                   case_when(iP19b | iP19e | iP19f == 1 ~ + 1, TRUE ~ 0) +
                                                   case_when(iP19c | xiBB5e == 1 ~ + 1, TRUE ~ 0))))
   
   Combined <- Combined %>% mutate('Interpersonal Conflict' = case_when(validipcon == 1 ~ 
                                                (case_when(xcymhipcon >= 2 ~ 2, TRUE ~ 0) +
                                                   case_when(xcymhipcon == 1 ~ 1, TRUE ~ 0) +
                                                   case_when(xcymhipcon == 0 ~ 0, TRUE ~ 0))))
   
   Combined$'Interpersonal Conflict'[Combined$'Interpersonal Conflict' == 1] <- "Triggered to reduce conflict within a specific domain"
   Combined$'Interpersonal Conflict'[Combined$'Interpersonal Conflict' == 0] <- "Not Triggered"
   Combined$'Interpersonal Conflict'[Combined$'Interpersonal Conflict' == 2] <- "Triggered to reduce widespread conflict"
   
   #################################################################
   ##                         Life Skills                         ##
   #################################################################
   
   Combined <- Combined %>% mutate(xiBB5e = (case_when(
     iBB4c %in% c(0,1) ~ 0)+ 
       case_when(iBB4c %in% c(2,3) ~ as.numeric(iBB5e), TRUE ~ 0)))
   
   Combined <- Combined %>% mutate(validipcon = case_when(
     iP19a %in% c(0,1,8) &
       iP19b %in% c(0,1,8) &
       iP19c %in% c(0,1,8) &
       iP19d %in% c(0,1,8) &
       iP19e %in% c(0,1,8) &
       iP19f %in% c(0,1,8) &
       xiBB5e %in% c(0,1,2,3) & 
       iBB5e %in% c(0,1,8) ~ 1, TRUE ~ 0))
   
   Combined <- Combined %>% mutate(xcymhipcon = case_when(validipcon == 1 ~ 
                                                (case_when(iP19a | iP19d == 1 ~ + 1, TRUE ~ 0) +
                                                   case_when(iP19b | iP19e | iP19f == 1 ~ + 1, TRUE ~ 0) +
                                                   case_when(iP19c | xiBB5e == 1 ~ + 1, TRUE ~ 0))))
   
   Combined <- Combined %>% mutate('Life Skills' = case_when(validipcon == 1 ~ 
                                                (case_when(xcymhipcon >= 2 ~ 2, TRUE ~ 0) +
                                                   case_when(xcymhipcon == 1 ~ 1, TRUE ~ 0) +
                                                   case_when(xcymhipcon == 0 ~ 0, TRUE ~ 0))))
   
   Combined$'Life Skills'[Combined$'Life Skills' == 0] <- "Not Triggered"
   Combined$'Life Skills'[Combined$'Life Skills' == 1] <- "Triggered for IADL assistance"
   Combined$'Life Skills'[Combined$'Life Skills' == 2] <- "Triggered for ADL assistance"
   
   
   ##################################################################
   ##                     Medication Adherence                     ##
   ##################################################################
   
   Combined <- Combined %>% mutate('Medication Adherence' = case_when(
     iM3 == 2 | iM4 == 1 | iM12 == 0 ~ 1, 
     iM3 != 2 & iM4 != 1 & iM12 != 0 ~ 0))
   
   Combined$'Medication Adherence'[Combined$'Medication Adherence' == 1] <- "Triggered"
   Combined$'Medication Adherence'[Combined$'Medication Adherence' == 0] <- "Not Triggered"

   
   #################################################################
   ##                      Medication Review                      ##
   #################################################################
   
   Combined <- Combined %>% mutate(xcymhmedrev = case_when(
     iJ2c %in% c(2,3,4) ~ + 1, 
     iJ2k %in% c(2,3,4) | iJ2y %in% c(2,3,4) ~ + 1, 
     iJ2n %in% c(2,3,4) | iJ2x %in% c(2,3,4) ~ + 1, 
     iJ2dd %in% c(2,3,4) ~ + 1, 
     iJ2gg %in% c(2,3,4) ~ + 1, 
     iJ2ll %in% c(2,3,4) ~ + 1, 
     iJ3 %in% c(1,2,3) ~ + 1, 
     iJ9a == 1 | iJ9b == 1 | iJ9c == 1 | iJ9d == 1 | iJ9e == 1 | 
       iJ9f == 1 | iJ9g == 1 ~ + 1, 
     iR9 == 1 ~ + 1))
   
   Combined <- Combined %>% mutate('Medication Review' = case_when(
     iM5 == 1 ~ 1, 
     iM5 != 1 ~ case_when(
       iM3 %in% c(0,1) & xcymhmedrev >= 1 ~ 1, 
       iM3 != 0 | iM3 != 1 | xcymhmedrev == 0 ~ 0)))
   
   Combined$'Medication Review'[Combined$'Medication Review' == 1] <- "Triggered"
   Combined$'Medication Review'[Combined$'Medication Review' == 0] <- "Not Triggered"

   
   ##################################################################
   ##                          Parenting2                          ##
   ##################################################################
   
   Combined <- Combined %>% mutate(xcymhparent = case_when(iP15a == 2 ~ + 1, TRUE ~ 0) +
                                                    case_when(iP15b == 2 ~ + 1, TRUE ~ 0) + 
                                                    case_when(iP15c == 2 ~ + 1, TRUE ~ 0) +
                                                    case_when(iP15d == 2 ~ + 1, TRUE ~ 0) +
                                                    case_when(iP15e == 2 ~ + 1, TRUE ~ 0) + 
                                                    case_when(iP15f == 2 ~ + 1, TRUE ~ 0))
   
   Combined <- Combined %>% mutate('Parenting2' = case_when(xcymhparent >= 2 ~ 1, TRUE ~ 0) +
                                                    case_when(xcymhparent < 2 ~ 0, TRUE ~ 0))
   Combined$'Parenting2'[Combined$'Parenting2' == 1] <- "Triggered"
   Combined$'Parenting2'[Combined$'Parenting2' == 0] <- "Not Triggered"
   
   
   #################################################################
   ##                   Readmission (cCYMHREAD)                   ##
   #################################################################
   
   Combined <- Combined %>% mutate(validread = case_when(
     iB12 %in% c(0,1) & 
       iB33 %in% c(0,1,2) & 
       iB38a %in% c(0,1) & 
       iB38b %in% c(0,1) & 
       iE5 %in% c(0,1,2,3) & 
       iP19h %in% c(0,1,8) & 
       iV1 %in% c(0,1,2) & 
       sCYPSS >= 0 & sCYPSS <= 12 & 
       sCYRHO >= 0 & sCYPSS <= 6 & 
       sCYSOS >= 0 & sCYPSS <= 6 ~ 1, TRUE ~ 0))
   
   Combined <- Combined %>% mutate(xcymhread1 = case_when(validread == 1 ~ 
                                                (case_when(iB12 == 1 ~ + 1, TRUE ~ 0) +
                                                   case_when(iB33 == 2 ~ + 1, TRUE ~ 0) +
                                                   case_when(iB38a == 1 ~ + 1, TRUE ~ 0) +
                                                   case_when(iB38b == 1 ~ + 1, TRUE ~ 0) + 
                                                   case_when(iP19h == 1 ~ + 1, TRUE ~ 0) +
                                                   case_when(sCYPSS >= 2 ~ + 1, TRUE ~ 0) +
                                                   case_when(sCYRHO >= 3 ~ + 1, TRUE ~ 0) +
                                                   case_when(sCYSOS >= 3 ~ + 1, TRUE ~ 0))))
   
   Combined <- Combined %>% mutate(xcymhread2 = case_when(validread == 1 ~ 
                                                (case_when(iE5 %in% c(2,3) ~  1, TRUE ~ 0) +
                                                   case_when(iV1 == 2 ~ 1, TRUE ~ 0) +
                                                   case_when(iV1 == 1 ~ case_when(xcymhread1 >= 1 ~ 1, TRUE ~ 0), TRUE ~ 0)))) 
   
   
   Combined <- Combined %>% mutate(Readmission = case_when(validread == 1 ~ 
                                               (case_when(xcymhread2 >= 1 ~ 1, TRUE ~ 0) +
                                                  case_when(xcymhread2 == 0 ~ 0, TRUE ~ 0))))
   
   Combined$Readmission[Combined$Readmission == 1] <- "Triggered"
   Combined$Readmission[Combined$Readmission == 0] <- "Not Triggered"
   
   
   ##################################################################
   ##        Suicidality and Purposeful Self Harm: cCYMHSSH        ##
   ##################################################################
   
   Combined <- Combined %>% mutate('Suicidality and Purposeful Self-Harm' = case_when(
     sCYSOS %in% c(5,6) ~ 2, 
     sCYSOS %in% c(3,4) ~ case_when(
       iE1oo >= 2 ~ 2, 
       iE1oo < 2 ~ 1),
     sCYSOS < 3 ~ 0))
   
   Combined$'Suicidality and Purposeful Self-Harm'[Combined$'Suicidality and Purposeful Self-Harm' == 0] <- "Not Triggered"
   Combined$'Suicidality and Purposeful Self-Harm'[Combined$'Suicidality and Purposeful Self-Harm' == 1] <- "Triggered due to moderate risk of harm to self"
   Combined$'Suicidality and Purposeful Self-Harm'[Combined$'Suicidality and Purposeful Self-Harm' == 2] <- "Triggered due to high risk of harm to self" 

   
   ##################################################################
   ##                  Sexual Behaviour: cCYMHSEX                  ##
   ##################################################################
   
   Combined <- Combined %>% mutate(validsex = case_when( 
     iE3o %in% c(0,1,2,3,4) & # inapprop. sexual behav.
       iE19a %in% c(0,1) & # inapprop. sexual knowledge
       iW27 %in% c(0,1) ~ 1, TRUE ~ 0)) # family concern about risky sexual beh
   
   #iJ10a and iX3 are from the Adolescent Supplement only. The CAP can be calculated without these.
   
   Combined <- Combined %>% mutate('Sexual Behaviour' = case_when(validsex == 1 ~ case_when(
     (iE3o %in% c(2,3,4) | iE19a == 1 | iJ10a == 1 | iX3 == 1 | iW27 == 1) ~ 1, TRUE ~ 0)))
 
   Combined$'Sexual Behaviour'[Combined$'Sexual Behaviour' == 1] <- "Triggered"
   Combined$'Sexual Behaviour'[Combined$'Sexual Behaviour' == 0] <- "Not Triggered"
   
   
   #################################################################
   ##                      Sleep Disturbance                      ##
   #################################################################
   
   Combined <- Combined %>% mutate(validsleep = case_when(
     iE1lll %in% c(0,1,2,3,4) &
       iE1yyy %in% c(0,1,2,3,4) & 
       iE1zzz %in% c(0,1,2,3,4) & 
       iE1aaaa %in% c(0,1,2,3,4) & 
       iE1bbbb %in% c(0,1,2,3,4) & 
       iJ4 %in% c(0,1,2,3,4) ~ 1, TRUE ~ 0))
   
   Combined <- Combined %>% mutate(xcymhsleep = case_when(validsleep == 1 ~ 
                                                (case_when(iE1lll %in% c(2,3,4) ~ + 1, TRUE ~ 0) +
                                                   case_when(iE1yyy %in% c(2,3,4) ~ + 1, TRUE ~ 0) +
                                                   case_when(iE1zzz %in% c(2,3,4) ~ + 1, TRUE ~ 0) + 
                                                   case_when(iE1aaaa %in% c(2,3,4) ~ + 1, TRUE ~ 0) +
                                                   case_when(iE1bbbb %in% c(2,3,4) ~ + 1, TRUE ~ 0) +
                                                   case_when(iJ4 %in% c(1,2,3,4) ~ + 1, TRUE ~ 0))))
  
   Combined <- Combined %>% mutate('Sleep Disturbance' = case_when(validsleep == 1 ~ case_when(
     xcymhsleep >= 2 ~ "Triggered", TRUE ~ "Not Triggered")))
   
   
   ##################################################################
   ##          Social and Peer Relationships: cCYMHSOCREL          ##
   ##################################################################
   
   Combined <- Combined %>% mutate(validsocrel = case_when(
     iE1qqq %in% c(0,1,2,4) & 
       iE1ttt %in% c(0,1,2,3,4) & 
       iE14d %in% c(0,1) & 
       iE15e %in% c(0,1,2,3,4,5) & 
       iF13 %in% c(0,1) & 
       iF15e %in% c(0,1) & 
       iF15f %in% c(0,1) & 
       iP8 %in% c(0,1) & 
       iY1aq %in% c(0,1,2,3,4,5) ~ 1, TRUE ~ 0))
   
   Combined <- Combined %>% mutate(xcymhsocrel1 = case_when(validsocrel == 1 ~ 
                                                  (case_when(iE1qqq %in% c(1,2,3,4) ~ + 1, TRUE ~ 0) +
                                                     case_when(iE1ttt %in% c(1,2,3,4) ~ + 1, TRUE ~ 0) +
                                                     case_when(iE14d == 1 ~ + 1, TRUE ~ 0) +
                                                     case_when(iF15e == 0 ~ + 1, TRUE ~ 0) +
                                                     case_when(iF15f == 0 ~ + 1, TRUE ~ 0) +
                                                     case_when(iP8 == 0 ~ + 1, TRUE ~ 0) + 
                                                     case_when(iY1aq %in% c(2,3,4,5) ~ + 1, TRUE ~ 0))))
   
   Combined <- Combined %>% mutate(xcymhsocrel2 = case_when(validsocrel == 1 ~ 
                                                  (case_when(iE15e %in% c(2,3,4,5) ~ + 1, TRUE ~ 0) +
                                                     case_when(iF13 == 1 ~ + 1, TRUE ~ 0)))) 
   
   Combined <- Combined %>% mutate('Social and Peer Relationships' = case_when(validsocrel == 1 ~ 
                                                 (case_when(xcymhsocrel1 >= 2 ~ 2, TRUE ~ 0) +
                                                    case_when(xcymhsocrel1 < 2 ~ 
                                                                (case_when(xcymhsocrel2 >= 1 ~ 1, TRUE ~ 0) +
                                                                   case_when(xcymhsocrel2 == 0 ~ 0, TRUE ~ 0)), TRUE ~ 0))))
 
   Combined$'Social and Peer Relationships'[Combined$'Social and Peer Relationships' == 0] <- "Not Triggered"
   Combined$'Social and Peer Relationships'[Combined$'Social and Peer Relationships' == 1] <- "Triggered to reduce maladaptive or antisocial peer interactions"
   Combined$'Social and Peer Relationships'[Combined$'Social and Peer Relationships' == 2] <- "Triggered to reduce social withdrawal or isolation"
   
   
   #################################################################
   ##                     Strengths: cCYMHSTR                     ##
   #################################################################
   
   Combined <- Combined %>% mutate(validstr = case_when(
     iC7 %in% c(0,1,2) & 
       iC9a %in% c(0,1,8) & 
       iF8b %in% c(0,1) & 
       iF15a %in% c(0,1) ~ 1, TRUE ~ 0))
   
   Combined <- Combined %>% mutate(xcymhstr = case_when(validstr == 1 ~
                                              (case_when(iC7 == 2 ~ + 1, TRUE ~ 0) +
                                                 case_when(iC9a %in% c(1,8) ~ + 1, TRUE ~ 0) +
                                                 case_when(iF8b == 0 ~ + 1, TRUE ~ 0) +
                                                 case_when(iF15a == 0 ~ + 1, TRUE ~ 0))))
   
   Combined <- Combined %>% mutate('Strengths' = case_when(validstr == 1 ~ 
                                              case_when(xcymhstr >= 3 ~ "Triggered", TRUE ~ "NOt Triggered")))
   
  
   
   ##################################################################
   ##                  Substance Use:cCYMHSUBUSE                   ##
   ##################################################################
   
   Combined <- Combined %>% mutate(validsubuse = case_when(
     iM6 %in% c(0,1) & 
       iW2 %in% c(0,1,2,3,4) &
       iW3a %in% c(0,1,2,3,4,5) &
       iW3b %in% c(0,1,2,3,4,5) &
       iW3c %in% c(0,1,2,3,4,5) &
       iW3d %in% c(0,1,2,3,4,5) &
       iW3e %in% c(0,1,2,3,4,5) &
       iW3f %in% c(0,1,2,3,4,5) ~ 1, TRUE ~ 0))
   
   Combined <- Combined %>% mutate(xcymhsubuse = case_when(validsubuse == 1 ~ 
                                                 (case_when(iM6 == 1 ~ + 1, TRUE ~ 0) +
                                                    case_when(iW2 %in% c(1,2,3,4) ~ + 1, TRUE ~ 0) +
                                                    case_when(iW3a %in% c(1,2,3,4,5) ~ + 1, TRUE ~ 0) +
                                                    case_when(iW3b %in% c(1,2,3,4,5) ~ + 1, TRUE ~ 0) +
                                                    case_when(iW3c %in% c(1,2,3,4,5) ~ + 1, TRUE ~ 0) + 
                                                    case_when(iW3d %in% c(1,2,3,4,5) ~ + 1, TRUE ~ 0) + 
                                                    case_when(iW3e %in% c(1,2,3,4,5) ~ + 1, TRUE ~ 0) + 
                                                    case_when(iW3f %in% c(1,2,3,4,5) ~ + 1, TRUE ~ 0))))
   
   Combined <- Combined %>% mutate('Substance Use' = case_when(validsubuse == 1 ~ case_when(
     xcymhsubuse >= 1 ~ "Triggered", TRUE ~ "Not Triggered")))
   
   
   #################################################################
   ##          Support Systems for Discharge: cCYMHSSDIS          ##
   #################################################################
   
   Combined <- Combined %>% mutate(validssdis = case_when(
     iA11b %in% c(1:15) & 
       iA12e %in% c(1:6) & 
       iR16d %in% c(0,1,2,8) & 
       iR16e %in% c(0,1,2,8) & 
       iY8a %in% c(0,1,8) ~ 1, TRUE ~ 0))
   # iA11b replaced iA11j
   
   Combined <- Combined %>% mutate('Support Systems for Discharge' = case_when(validssdis == 1 ~ case_when(
     iA11b %in% c(5,8) | iA12e %in% c(1,5) | iY8a == 1 | iR16d %in% c(0,2) | 
       iR16e %in% c(0,2) ~ "Triggered", TRUE ~ "Not Triggered")))
   
   
   ##################################################################
   ##             Tobacco and Nicotine Use: cCYMHTOBAC             ##
   ##################################################################
   Combined <- Combined %>% mutate(validtobac = case_when(
     iW20a %in% c(0,1,2,3) & 
       iW20b %in% c(0,1,2,3) ~ 1, TRUE ~ 0))
   
   Combined <- Combined %>% mutate('Tobacco and Nicotine Use' = case_when(validtobac == 1 ~ case_when(
     iW20a == 3 | iW20b == 3 ~ "Triggered to prevent long-term tobacco use", 
     iW20a %in% c(1,2) | iW20b %in% c(1,2) ~ "Triggered to reduce or cease daily tobacco use", 
     iW20a == 0 & iW20b == 0 ~ "Not Triggered")))
   
   
   #################################################################
   ##                   Transitions: cCYMHTRANS                   ##
   #################################################################
   
   Combined <- Combined %>% mutate(validtrans = case_when(
     iZ12 %in% c(0,1) ~ 1, TRUE ~ 0))
   Combined <- Combined %>% mutate('Transitions' = case_when(validtrans == 1 ~ case_when(
     iZ12 == 1 ~ "Triggered", TRUE ~ "Not Triggered")))

   
   ##################################################################
   ##              Traumatic Life Events: cCYMHTRAUMA              ##
   ##################################################################
   
   Combined <- Combined %>% mutate(validtrauma = case_when(
     iB34a %in% c(0,1,2,3) & 
       iB34b %in% c(0,1,2,3) & 
       iB34c %in% c(0,1,2,3) & 
       iE1ddd %in% c(0,1,2,3,4) & 
       iE1fff %in% c(0,1,2,3,4) & 
       iE1iii %in% c(0,1,2,3,4) & 
       iE1jjj %in% c(0,1,2,3,4) & 
       iE1kkk %in% c(0,1,2,3,4) & 
       iE1lll %in% c(0,1,2,3,4) & 
       iE1eeee %in% c(0,1,2,3,4) & 
       iY1aa %in% c(0,1,2,3,4,5) & 
       iY1aj %in% c(0,1,2,3,4,5) &
       iY1ak %in% c(0,1,2,3,4,5) &
       iY1al %in% c(0,1,2,3,4,5) &
       iY1am %in% c(0,1,2,3,4,5) &
       iY1an %in% c(0,1,2,3,4,5) &
       iY1ao %in% c(0,1,2,3,4,5) &
       iY1aq %in% c(0,1,2,3,4,5) &
       iY1ar %in% c(0,1,2,3,4,5) &
       iY1at %in% c(0,1,2,3,4,5) &
       iY1au %in% c(0,1,2,3,4,5) &
       iY1b %in% c(0,1,8) & 
       iY2a %in% c(0,1) & 
       iY2c %in% c(0,1) & 
       iY2d %in% c(0,1) ~ 1, TRUE ~ 0))
   
   Combined <- Combined %>% mutate(xcymhtrauma1 = case_when(validtrauma == 1 ~ 
                                                  (case_when(iY1aa %in% c(4,5) ~ + 1, TRUE ~ 0) +
                                                     case_when(iY1aj %in% c(4,5) ~ + 1, TRUE ~ 0) +
                                                     case_when(iY1ak %in% c(4,5) ~ + 1, TRUE ~ 0) +
                                                     case_when(iY1am %in% c(4,5) ~ + 1, TRUE ~ 0) +
                                                     case_when(iY1an %in% c(4,5) ~ + 1, TRUE ~ 0) +
                                                     case_when(iY1ao %in% c(4,5) ~ + 1, TRUE ~ 0) +
                                                     case_when(iY1aq %in% c(4,5) ~ + 1, TRUE ~ 0) +
                                                     case_when(iY1ar %in% c(4,5) ~ + 1, TRUE ~ 0) +
                                                     case_when(iY1at %in% c(4,5) ~ + 1, TRUE ~ 0) +
                                                     case_when(iY1au %in% c(4,5) ~ + 1, TRUE ~ 0))))
   
   Combined <- Combined %>% mutate(xcymhtrauma2 = case_when(validtrauma == 1 ~ 
                                                  (case_when(iY2a == 1 ~ + 1, TRUE ~ 0) +
                                                     case_when(iY2c == 1 ~ + 1, TRUE ~ 0) +
                                                     case_when(iY2d == 1 ~ + 1, TRUE ~ 0))))
   
   Combined <- Combined %>% mutate(xcymhtrauma3 = case_when(validtrauma == 1 ~ 
                                                  (case_when(iB34a | iB34b | iB34c %in% c(1,2,3) ~ + 1, TRUE ~ 0) +
                                                     case_when(iY1aa %in% c(1,2,3,4,5) ~ + 1, TRUE ~ 0) +
                                                     case_when(iY1aj %in% c(1,2,3,4,5) ~ + 1, TRUE ~ 0) +
                                                     case_when(iY1ak %in% c(1,2,3,4,5) ~ + 1, TRUE ~ 0) +
                                                     case_when(iY1al %in% c(1,2,3,4,5) ~ + 1, TRUE ~ 0) +
                                                     case_when(iY1am %in% c(1,2,3,4,5) ~ + 1, TRUE ~ 0) +
                                                     case_when(iY1an %in% c(1,2,3,4,5) ~ + 1, TRUE ~ 0) +
                                                     case_when(iY1ao %in% c(1,2,3,4,5) ~ + 1, TRUE ~ 0) +
                                                     case_when(iY1aq %in% c(1,2,3,4,5) ~ + 1, TRUE ~ 0) +
                                                     case_when(iY1ar %in% c(1,2,3,4,5) ~ + 1, TRUE ~ 0) +
                                                     case_when(iY1at %in% c(1,2,3,4,5) ~ + 1, TRUE ~ 0) +
                                                     case_when(iY1au %in% c(1,2,3,4,5) ~ + 1, TRUE ~ 0))))
   
   Combined <- Combined %>% mutate(xcymhtrauma4 = case_when(validtrauma == 1 ~ 
                                                  (case_when(iE1ddd %in% c(1,2,3,4) ~ + 1, TRUE ~ 0) +
                                                     case_when(iE1fff %in% c(1,2,3,4) ~ + 1, TRUE ~ 0) +
                                                     case_when(iE1iii %in% c(1,2,3,4) ~ + 1, TRUE ~ 0) +
                                                     case_when(iE1jjj %in% c(1,2,3,4) ~ + 1, TRUE ~ 0) +
                                                     case_when(iE1kkk %in% c(1,2,3,4) ~ + 1, TRUE ~ 0) +
                                                     case_when(iE1lll %in% c(1,2,3,4) ~ + 1, TRUE ~ 0) +
                                                     case_when(iE1eeee %in% c(1,2,3,4) ~ + 1, TRUE ~ 0)))) 
   
   Combined <- Combined %>% mutate('Traumatic Life Events' = case_when(validtrauma == 1 ~ case_when(
     xcymhtrauma1 >= 1 | xcymhtrauma2 >= 1 ~"Triggered to address immediate safety concerns", 
     xcymhtrauma1 == 0 & xcymhtrauma2 == 0 ~ case_when(
       xcymhtrauma3 >= 1 ~ case_when(
         iY1b == 1 | xcymhtrauma4 >=2 ~ "Triggered to reduce the impact of prior traumatic life events", 
         iY1b < 1 & xcymhtrauma4 < 2 ~ "Not Triggered"),
       xcymhtrauma3 == 0 ~ "Not Triggered"))))

   
   #################################################################
   ##                    Video Gaming: cCYMHVG                    ##
   #################################################################
   
   Combined <- Combined %>% mutate(validvg = case_when(
     iW21 %in% c(0,1,2,3) ~ 1, TRUE ~ 0))
   Combined <- Combined %>% mutate('Video Gaming' = case_when(validvg == 1 ~ case_when(
     iW21 %in% c(2,3) ~ "Triggered", 
     iW21 < 2 ~ "Not Triggered")))
   
   #################################################################
   ##                  Eating/Weight: cCYMHWTMGT                  ##
   #################################################################
   
   Combined <- Combined %>% mutate(validwtmgt = case_when(
     iK2i %in% c(0,1) & 
       iK2j %in% c(0,1) & 
       iK2k %in% c(0,1) & 
       iK8a %in% c(0,1) & 
       iK8b %in% c(0,1) & 
       iK8c %in% c(0,1) ~ 1, TRUE ~ 0))  
   Combined <- Combined %>% mutate('Eating/Weight' = case_when(validwtmgt == 1 ~ case_when(
     iK2k == 1 | iK2k == 1 | iK2i == 1 ~ "Triggered for weight concerns", 
     iK2j < 1 & iK2k < 1 & iK2i < 1 ~ case_when(
       iK8a == 1 | iK8b ==1 | iK8c == 1 ~ "Triggered due to problematic eating behaviour", 
       iK8a < 1 & iK8b < 1 & iK8c < 1 ~ "Not Triggered"))))
   
  ##--------------
##  EXT__scale  
##--------------

Combined <- Combined %>% mutate(validexts = case_when(
  iE15a %in% c(0,1,2,3,4,5) &
    iE15d %in% c(0,1,2,3,4,5) &
    iE15e %in% c(0,1,2,3,4,5) &
    iE1oo %in% c(0,1,2,3,4) &
    iE3m %in% c(0,1,2,3,4) &
    iE3r %in% c(0,1,2,3,4) &
    iE3s %in% c(0,1,2,3,4) &
    iE3t %in% c(0,1,2,3,4) &
    iX16c %in% c(0,1,2,3,4,5) & 
    iX2a %in% c(0,1,2,3,4,5) & 
    iX2b %in% c(0,1,2,3,4,5) & 
    iX2c %in% c(0,1,2,3,4,5) ~ 1, TRUE ~ 0))

Combined <- Combined %>% mutate(EXT__scale = case_when(validexts == 1 ~ 
                                                         (case_when(iE15a == 0 ~ 0, iE15a %in% c(1,2,3,4,5) ~ 1)) + 
                                                         (case_when(iE15d == 0 ~ 0, iE15d %in% c(1,2,3,4,5) ~ 1)) + 
                                                         (case_when(iE15e == 0 ~ 0, iE15e %in% c(1,2,3,4,5) ~ 1)) + 
                                                         (case_when(iE1oo == 0 ~ 0, iE1oo %in% c(1,2,3,4) ~ 1)) + 
                                                         (case_when(iE3m == 0 ~ 0, iE3m %in% c(1,2,3,4) ~ 1)) +
                                                         (case_when(iE3r == 0 ~ 0, iE3r %in% c(1,2,3,4) ~ 1)) +
                                                         (case_when(iE3s == 0 ~ 0, iE3s %in% c(1,2,3,4) ~ 1)) +
                                                         (case_when(iE3t == 0 ~ 0, iE3t %in% c(1,2,3,4) ~ 1)) +
                                                         (case_when(iX16c == 0 ~ 0, iX16c %in% c(1,2,3,4,5) ~ 1)) +
                                                         (case_when(iX2a == 0 ~ 0, iX2a %in% c(1,2,3,4,5) ~ 1)) +
                                                         (case_when(iX2b == 0 ~ 0, iX2b %in% c(1,2,3,4,5) ~ 1)) +
                                                         (case_when(iX2c == 0 ~ 0, iX2c %in% c(1,2,3,4,5) ~ 1))))

ext.model <- readRDS("M:/DATA & REPORTS/Clinical & Client Services/Clinical Data/Models/extmodel.rda")

Combined$`EXT_norm` <- predictNorm(Combined$`EXT__scale`, Combined$AgeN, ext.model, minNorm = 15, maxNorm = 100)

##--------------
##  INT__scale  
##--------------

Combined <- Combined %>% mutate(validints = case_when(
  iE1eee %in% c(0,1,2,3,4) & 
    iE1eeee %in% c(0,1,2,3,4) &
    iE1fff %in% c(0,1,2,3,4) &
    iE1kkk %in% c(0,1,2,3,4) &
    iE1rrr %in% c(0,1,2,3,4) &
    iE1sss %in% c(0,1,2,3,4) &
    iE1ttt %in% c(0,1,2,3,4) &
    iE1uu %in% c(0,1,2,3,4) &
    iE1vv %in% c(0,1,2,3,4) &
    iE1ww %in% c(0,1,2,3,4) &
    iE1xx %in% c(0,1,2,3,4) &
    iE1yy %in% c(0,1,2,3,4) ~ 1, TRUE ~ 0))

Combined <- Combined %>% mutate(INT__scale = case_when(validints == 1 ~ 
                                                         iE1eee + iE1eeee + iE1fff + iE1kkk +
                                                         iE1rrr + iE1sss + iE1ttt +  iE1uu + iE1vv +
                                                         iE1ww + iE1xx + iE1yy))

int.model <- readRDS("M:/DATA & REPORTS/Clinical & Client Services/Clinical Data/Models/intmodel.rda")

Combined$`INT_norm` <- predictNorm(Combined$`INT__scale`, Combined$AgeN, int.model, minNorm = 15, maxNorm = 90)

##--------------
##  ADL__scale  
##--------------

Combined <- Combined %>% mutate(validadl = case_when(
  iG2a %in% c(0,1,2,3,4,5,6,8) & 
    iG2b %in% c(0,1,2,3,4,5,6,8) & 
    iG2c %in% c(0,1,2,3,4,5,6,8) &
    iG2d %in% c(0,1,2,3,4,5,6,8) ~ 1, TRUE ~ 0))

Combined <- Combined %>% mutate(ADL__scale = case_when(validadl == 1 ~ 
                                         (case_when(iG2a == 0 ~ 0, iG2a %in% c(1,2,3,4,5) ~ 1, iG2a %in% c(6,8) ~ 2))+ 
                                         (case_when(iG2b == 0 ~ 0, iG2b %in% c(1,2,3,4,5) ~ 1, iG2b %in% c(6,8) ~ 2))+
                                         (case_when(iG2c == 0 ~ 0, iG2c %in% c(1,2,3,4,5) ~ 1, iG2c %in% c(6,8) ~ 2))+
                                         (case_when(iG2d == 0 ~ 0, iG2d %in% c(1,2,3,4,5) ~ 1, iG2d %in% c(6,8) ~ 2))))

adl.model <- readRDS("M:/DATA & REPORTS/Clinical & Client Services/Clinical Data/Models/adlmodel.rda")

Combined$`ADL_norm` <- predictNorm(Combined$`ADL__scale`, Combined$AgeN, adl.model, minNorm = 40, maxNorm = 100)

##--------------
##  PEER__scale  
##--------------

Combined <- Combined %>% mutate(valideer = case_when(
  iP19b %in% c(0,1,8) &
    iP19e %in% c(0,1,8) &
    iP19f %in% c(0,1,8) ~ 1, TRUE ~ 0))

Combined <- Combined %>% mutate(PEER__scale = case_when(valideer == 1 ~ 
                                                         (case_when(iP19b == 1 ~ + 1, TRUE ~ 0) +
                                                            case_when(iP19f == 1 ~ + 1, TRUE ~ 0) +
                                                            case_when(iP19e == 1 ~ + 1, TRUE ~ 0))))

peer.model <- readRDS("M:/DATA & REPORTS/Clinical & Client Services/Clinical Data/Models/peermodel.rda")

Combined$PEER_norm <- predictNorm(Combined$PEER__scale, Combined$AgeN, peer.model, minNorm = 45, maxNorm = 75)

##--------------
##  FAM__scale  
##--------------

Combined <- Combined %>% mutate(validily = case_when(
  iF8a %in% c(0,1) &
    iP19d %in% c(0,1,8) &
    iP19h %in% c(0,1,8) &
    iY7a %in% c(0,1) &
    iY7b %in% c(0,1) &
    iY8a %in% c(0,1,8) ~ 1, TRUE ~ 0))

Combined <- Combined %>% mutate(FAM__scale = case_when(validily == 1 ~ 
                                                         case_when(iP19d %in% c(0,1) & iP19h %in% c(0,1) & iY8a %in% c(0,1) ~
                                                                     case_when(iF8a == 0 ~ (1 + iP19d + iP19h + iY7a + iY7b + iY8a), 
                                                                               iF8a == 1 ~ (iP19d + iP19h + iY7a + iY7b + iY8a)))))

fam.model <- readRDS("M:/DATA & REPORTS/Clinical & Client Services/Clinical Data/Models/fammodel.rda")

Combined$`FAM_norm` <- predictNorm(Combined$`FAM__scale`, Combined$AgeN, fam.model, minNorm = 45, maxNorm = 100)


##--------------------
##  STRENGTHS__scale  (Relational Strengths: sCYRelStren)
##--------------------

Combined <- Combined %>% mutate(validrelstren = case_when(
  iP8 %in% c(0,1) & 
    iF15c %in% c(0,1) &
    iF8a %in% c(0,1) &
    iF15d %in% c(0,1) &
    iF15e %in% c(0,1) &
    iF15f %in% c(0,1) ~ 1, TRUE ~ 0))

Combined <- Combined %>% mutate(STRENGTHS__scale = case_when(validrelstren == 1 ~ 
                                                               (case_when(iP8 == 0 ~ 1, iP8 == 1 ~ 0)) +
                                                               (case_when(iF15c == 0 ~ 1, iF15c == 1 ~ 0)) +
                                                               (case_when(iF8a == 0 ~ 1, iF8a == 1 ~ 0)) +
                                                               (case_when(iF15d == 0 ~ 1, iF15d == 1 ~ 0)) +
                                                               (case_when(iF15e == 0 ~ 1, iF15e == 1 ~ 0)) +
                                                               (case_when(iF15f == 0 ~ 1, iF15f == 1 ~ 0))))

strengths.model <- readRDS("M:/DATA & REPORTS/Clinical & Client Services/Clinical Data/Models/strengthsmodel.rda")

Combined$`STRENGTHS_norm` <- predictNorm(Combined$`STRENGTHS__scale`, Combined$AgeN, strengths.model, minNorm = 35, maxNorm = 80)


##----------------------
##  SCHLDISRUPT__scale  
##----------------------

Combined <- Combined %>% mutate(validsdes = case_when(
  iBB5a %in% c(0,1,8) & 
    iBB5b %in% c(0,1,8) & 
    iBB5e %in% c(0,1,8) & 
    iBB5f %in% c(0,1,8) & 
    iBB11a %in% c(0,1) & 
    iBB11b %in% c(0,1) & 
    iBB10 %in% c(0,1,2,3) & 
    iBB14a %in% c(0,1,2,3,8) ~ 1, TRUE ~ 0))

Combined <- Combined %>% mutate(SCHLDISRUPT__scale = case_when(validsdes == 1 ~ 
                                                                 (case_when(iBB5a == 1 ~ + 1, TRUE ~ 0) +
                                                                    case_when(iBB5b == 1 ~ + 1, TRUE ~ 0) +
                                                                    case_when(iBB5e == 1 ~ + 1, TRUE ~ 0) +
                                                                    case_when(iBB5f == 1 ~ + 1, TRUE ~ 0) +
                                                                    case_when(iBB10 %in% c(1,2,3) ~ + 1, TRUE ~ 0) +
                                                                    case_when(iBB11a == 1 ~ + 1, TRUE ~ 0) + 
                                                                    case_when(iBB11b == 1 ~ + 1, TRUE ~ 0) + 
                                                                    case_when(iBB14a %in% c(2,3) ~ + 1, TRUE ~ 0))))

schldisrupt.model <- readRDS("M:/DATA & REPORTS/Clinical & Client Services/Clinical Data/Models/schldisruptmodel.rda")

Combined$`SCHLDISRUPT_norm` <- predictNorm(Combined$`SCHLDISRUPT__scale`, Combined$AgeN, schldisrupt.model, minNorm = 40, maxNorm = 80)

##------------------
##  SENSORY__scale  
##------------------

Combined <- Combined %>% mutate(validSensDiff = case_when( 
  iD28 %in% c(0,1,2,3,4) &
    iD29 %in% c(0,1,2,3,4) &
    iE3aa %in% c(0,1,2,3,4) &
    oE3ab %in% c(0,1,2,3,4) & 
    oE3ac %in% c(0,1,2,3,4) ~ 1, TRUE ~ 0))

Combined <- Combined %>% mutate(SENSORY__scale = case_when(validSensDiff == 1 ~ 
                                                             iD28 + iD29 + iE3aa + oE3ab + oE3ac))
Combined$SENSORY__log <- log(Combined$SENSORY__scale+1)

sensory.model <- readRDS("M:/DATA & REPORTS/Clinical & Client Services/Clinical Data/Models/sensorymodel.rda")

Combined$`SENSORY_norm` <- predictNorm(Combined$`SENSORY__log`, Combined$AgeN, sensory.model, minNorm = 40, maxNorm = 80)

##----------------
##  INT-S__scale  
##----------------

Combined <- Combined %>% mutate('INT-S__scale' = iE1eee + iE1eeee + iE1fff + iE1kkk +
                                       iE1rrr + iE1sss + iE1ttt + iE1vv +
                                       iE1ww + iE1xx + iE1yy)

ints.model <- readRDS("M:/DATA & REPORTS/Clinical & Client Services/Clinical Data/Models/intsmodel.rda")
Combined$`INT-S_norm` <- predictNorm(Combined$`INT-S__scale`, Combined$AgeN, ints.model, minNorm = 15, maxNorm = 90)



##----------------
##  EXT-S__scale  
##----------------

Combined <- Combined %>% mutate(validexts = case_when(
  iE15a %in% c(0,1,2,3,4,5) &
    iE15e %in% c(0,1,2,3,4,5) &
    iE1oo %in% c(0,1,2,3,4) &
    iE3l %in% c(0,1,2,3,4) &
    iX2a %in% c(0,1,2,3,4,5) & 
    iX2b %in% c(0,1,2,3,4,5) & 
    iX2c %in% c(0,1,2,3,4,5) ~ 1, TRUE ~ 0))

Combined <- Combined %>% mutate('EXT-S__scale' = case_when(validexts == 1 ~ 
                                                             (case_when(iE15a == 0 ~ 0, iE15a %in% c(1,2,3,4,5) ~ 1)) + 
                                                             (case_when(iE15e == 0 ~ 0, iE15e %in% c(1,2,3,4,5) ~ 1)) + 
                                                             (case_when(iE1oo == 0 ~ 0, iE1oo %in% c(1,2,3,4) ~ 1)) + 
                                                             (case_when(iE3l == 0 ~ 0, iE3m %in% c(1,2,3,4) ~ 1)) +
                                                             (case_when(iX2a == 0 ~ 0, iX2a %in% c(1,2,3,4,5) ~ 1)) +
                                                             (case_when(iX2b == 0 ~ 0, iX2b %in% c(1,2,3,4,5) ~ 1)) +
                                                             (case_when(iX2c == 0 ~ 0, iX2c %in% c(1,2,3,4,5) ~ 1))))

exts.model <- readRDS("M:/DATA & REPORTS/Clinical & Client Services/Clinical Data/Models/extsmodel.rda")

Combined$`EXT-S_norm` <- predictNorm(Combined$`EXT-S__scale`, Combined$AgeN, pss.model, minNorm = 20, maxNorm = 100)


##----------------
##  AUT__scale  
##----------------

Combined <- Combined %>% mutate(validssc = case_when(iC7 %in% c(0,1,2) &
                                                       iE3aa %in% c(0,1,2,3,4) &
                                                       iE14a %in% c(0,1) &  
                                                       iE14b %in% c(0,1) &                    
                                                       iE14c %in% c(0,1) ~ 1, TRUE ~ 0))

Combined <- Combined %>% mutate(AUT__scale = case_when(validssc == 1 ~ 
                                                    (case_when(iE14a == 1 ~ 1, TRUE ~ 0)) + 
                                                    (case_when(iE14b == 1 ~ 1, TRUE ~ 0)) + 
                                                    (case_when(iE14c == 1 ~ 1, TRUE ~ 0)) + 
                                                    (case_when(iE3aa %in% c(1,2,3,4) ~ 1, TRUE ~ 0)) + 
                                                    (case_when(iC7== 2 ~ 1, TRUE ~ 0))))

aut.model <- readRDS("M:/DATA & REPORTS/Clinical & Client Services/Clinical Data/Models/autmodel.rda")

Combined$AUT_norm <- predictNorm(Combined$AUT__scale, Combined$AgeN, aut.model, minNorm = 30, maxNorm = 95)

##----------------
## COG__scale  
##----------------

Combined <- Combined %>% mutate(validcognitive = case_when(
  iC1d %in% c(0,1,2,3,4,5) &
    iC2a %in% c(0,10) &
    iC2b %in% c(0,1) & 
    iD1 %in% c(0,1,2,3,4) & 
    iD2 %in% c(0,1,2,3,4) ~ 1, TRUE ~ 0))

Combined <- Combined %>% mutate(COG__scale = case_when(validcognitive == 1 ~ 
                                                                (case_when(iC1d == 0 ~ 0, iC1d %in% c(1,2,3,4,5) ~ 1)) +
                                                                (case_when(iC2a == 0 ~ 0, iC2a == 1 ~ 1)) +
                                                                (case_when(iC2b == 0 ~ 0, iC2b == 1 ~ 1)) +
                                                                (case_when(iD1 == 0 ~ 0, iD1 %in% c(1,2,3,4) ~ 1)) +
                                                                (case_when(iD2 == 0 ~ 0, iD2 %in% c(1,2,3,4) ~ 1))))

cog.model <- readRDS("M:/DATA & REPORTS/Clinical & Client Services/Clinical Data/Models/cogmodel.rda")

Combined$COG_norm <- predictNorm(Combined$COG__scale, Combined$AgeN, cog.model, minNorm = 40, maxNorm = 75)


##----------------
##  MANIA__scale  
##----------------

Combined <- Combined %>% mutate(validmania_old = case_when(
  iE1bbb %in% c(0,1,2,3,4) &
    iE1zz %in% c(0,1,2,3,4) &
    iE1llll %in% c(0,1,2,3,4) &
    iE1ccc %in% c(0,1,2,3,4) &
    iE1aaa %in% c(0,1,2,3,4) &
    iE6 %in% c(0,1,2,3,4,5) ~ 1, TRUE ~ 0))

Combined <-Combined %>% mutate(MANIA__scale = case_when(validmania_old == 1 ~ case_when
                                                        (iE6 == 0 ~  0, 
                                                          iE6 == 1 ~  1, 
                                                          iE6 == 2 ~  1, 
                                                          iE6 == 3 ~  1, 
                                                          iE6 == 4 ~  1,
                                                          iE6 == 5 ~  2) + iE1bbb + iE1zz
                                                        + iE1lll + iE1ccc + iE1aaa))

##----------------
##  SLEEP__scale  
##----------------

Combined <- Combined %>% mutate(valideep = case_when(
  iE1yyy %in% c(0,1,2,3,4) &
    iE1zzz %in% c(0,1,2,3,4)&
    iE1aaaa %in% c(0,1,2,3,4)&
    iE1bbbb %in% c(0,1,2,3,4) ~ 1, TRUE ~ 0))

Combined <- Combined %>% mutate(SLEEP__scale = case_when(valideep == 1 ~ (iE1yyy +iE1zzz +iE1aaaa +
                                                                  iE1bbbb)))

sleep.model <- readRDS("M:/DATA & REPORTS/Clinical & Client Services/Clinical Data/Models/sleepmodel.rda")

Combined$SLEEP_norm <- predictNorm(Combined$SLEEP__scale, Combined$AgeN, sleep.model, minNorm = 30, maxNorm = 80)

##-------------------------
##  Psychiatric Diagnosis  
##-------------------------

Combined <- Combined %>% 
  mutate(`Psychiatric Diagnoses` = case_when(
    is.na(iCC10b) & is.na(iCC1d) ~ NA,
    !is.na(iCC10b) & rowSums(across(iCC10b:iCC10u, ~ . %in% c(2, 3, 4))) >= 1 ~ 1,
    !is.na(iCC1d) & rowSums(across(iCC1d:iCC1u, ~ . %in% c(1, 2, 3, 4))) >= 1 ~ 1,
    TRUE ~ 0
  ))


##---------------------
##  Medical Diagnosis    #### DOUBLE CCHECK THIS
##---------------------

Combined <- Combined %>%
  mutate(`Medical Diagnoses` = case_when(
    Tool != "rscr" & !is.na(iI1aa) & rowSums(across(iI1aa:iI1z)) >= 1 ~ 1,
    !is.na(iA48) ~ iA48,
    TRUE ~ 0
  ))

##----------
##  ID/GGD  
##----------

Combined <- Combined %>%
  mutate('ID/GDD' = if_else(
    Tool != "rscr" & (
      iCC10b %in% c(1, 2, 3, 4) |
        iCC10r %in% c(1, 2, 3, 4) |
        iCC1r %in% c(1, 2, 3, 4) |
        iCC9 %in% c(5, 6)
    ), 1, 0),
    if_else(
      is.na(iA47),
      if_else(
        iA48 == 1, 1,
        if_else(iA48 == 0, 0, iA48)
      ),
      if_else(iA47 == 1, 1,
              if_else(iA47 == 0, 0, iA47)
      )))

##----------------------
##  Residential Issues  
##----------------------

Combined$'Residential Issues' <- ifelse(
  (Combined$iB12 == 1 | Combined$iB38c == 1 | Combined$iB38a == 1 | Combined$iB38b == 1 | Combined$iE5 %in% c(1,2,3) | (Combined$iA11b != 1 & !is.na(Combined$iA11b) & Combined$iA11b != 99)),
  1,
  ifelse(
    (!is.na(Combined$iB12) & Combined$iB12 != 99) | (!is.na(Combined$iB38c) & Combined$iB38c != 99) | (!is.na(Combined$iB38a) & Combined$iB38a != 99) | (!is.na(Combined$iB38b) & Combined$iB38b != 99) | (!is.na(Combined$iE5) & Combined$iE5 != 99) | (!is.na(Combined$iA11b) & Combined$iA11b != 99),
    0,
    NA)
)

##---------
##  xsdsi  
##---------

Combined <- Combined %>% mutate(xsdsi =
                                  iE1ss + iE1tt + iE1vv + iE1ww + iE1xx + iE1yy + 
                                  iE1aaa + iE1rrr + iE1ttt)
##---------
##  xsSOS  
##---------

Combined <- Combined %>% mutate(xsSOS =  case_when(
  iX1c == 5 ~ case_when(iX1b == 1 ~ 6, iX1b %in% c(0,8) ~ case_when(
    iX1e == 1 ~ 6, iX1e == 0 ~ case_when(xsdsi >= 9 ~ 5, xsdsi < 9 ~ case_when(
      iX1d == 1 ~ 4, iX1d == 0 ~ case_when(iE1nnn %in% c(1,2,3,4) ~ 5, 
                                           iE1nnn == 0 ~ 3))))),
  iX1c %in% c(3,4) ~ case_when(iX1b == 1 ~ 4, iX1b %in% c(0,8) ~ 3),
  
  iX1c %in% c(1,2) ~ case_when(iX1b == 1 ~ case_when(xsdsi >= 6 ~ 4,
                                                     xsdsi < 6 ~ case_when(
                                                       iX1d == 1 ~ case_when(
                                                         iE1nnn %in% c(1,2,3,4) ~ 5,
                                                         iE1nnn == 0 ~ 3), 
                                                       iX1d == 0 ~ case_when(
                                                         iE1nnn %in% c(1,2,3,4) ~ 4,
                                                         iE1nnn == 0 ~ 3))),
                               iX1b %in% c(0,8) ~ case_when(iE1nnn %in% 
                                                              c(1,2,3,4) ~ 3,
                                                            iE1nnn == 0 ~ 
                                                              case_when(
                                                                iX1d == 1 ~ 2,
                                                                iX1d == 0 ~ 1))),
  iX1c == 0 ~ 0))

##-------------------------
##  Valid RICHY variables  
##-------------------------

Combined <- Combined %>% mutate(validrichy = case_when(
  iE1yyy %in% c(0,1,2,3,4) & 
    iE3q %in% c(0,1,2,3,4) & 
    iE15e %in% c(0,1,2,3,4,5) &
    iJ2k %in% c(0,1,2,3,4) & 
    iJ2y %in% c(0,1,2,3,4) & 
    iJ2dd %in% c(0,1,2,3,4) & 
    iJ2gg %in% c(0,1,2,3,4) & 
    iJ2ll %in% c(0,1,2,3,4) &
    iJ3 %in% c(0,1,2,3) & 
    iJ9a %in% c(0,1) & 
    iJ9b %in% c(0,1) & 
    iJ9c %in% c(0,1) & 
    iJ9d %in% c(0,1) & 
    iJ9e %in% c(0,1) & 
    iJ9f %in% c(0,1) & 
    iJ9g %in% c(0,1) & 
    iP15a %in% c(0,1,2,8) &
    iP15b %in% c(0,1,2,8) &
    iP15c %in% c(0,1,2,8) &
    iP15d %in% c(0,1,2,8) &
    iP15e %in% c(0,1,2,8) &
    iP15f %in% c(0,1,2,8) &
    iP19b %in% c(0,1,8) &
    iP19e %in% c(0,1,8) &
    iX2b %in% c(0,1,2,3,4,5) &
    iX16c %in% c(0,1,2,3,4,5) &
    iY1al %in% c(0,1,2,3,4,5) &
    iY1am %in% c(0,1,2,3,4,5) & 
    iY1an %in% c(0,1,2,3,4,5) &
    iY1aq %in% c(0,1,2,3,4,5) &
    iY1ao %in% c(0,1,2,3,4,5) &
    iY1au %in% c(0,1,2,3,4,5) &
    iE1eee %in% c(0,1,2,3,4) & 
    iE1fff %in% c(0,1,2,3,4) & 
    iE1ggg %in% c(0,1,2,3,4) & 
    iE1hhh %in% c(0,1,2,3,4) & 
    iE1iii %in% c(0,1,2,3,4) & 
    iE1kkk %in% c(0,1,2,3,4) & 
    iE1lll %in% c(0,1,2,3,4) & 
    iE1eeee %in% c(0,1,2,3,4) & 
    iF8a %in% c(0,1) &
    iP19d %in% c(0,1,8) &
    iP19h %in% c(0,1,8) & ## identical to this point
    iY7a %in% c(0,1) &
    iY7b %in% c(0,1) &
    iY8a %in% c(0,1,8) & 
    iE1oo %in% c(0,1,2,3,4) &
    iE1nnn %in% c(0,1,2,3,4) & 
    iX1b %in% c(0,1,8) & 
    iX1c %in% c(0,1,2,3,4,5) & 
    iX1d %in% c(0,1) &
    iX1e %in% c(0,1) & 
    iE1ss %in% c(0,1,2,3,4) & 
    iE1tt %in% c(0,1,2,3,4) &
    iE1vv %in% c(0,1,2,3,4) &
    iE1ww %in% c(0,1,2,3,4) &
    iE1xx %in% c(0,1,2,3,4) &
    iE1yy %in% c(0,1,2,3,4) &
    iE1aaa %in% c(0,1,2,3,4) &
    iE1rrr %in% c(0,1,2,3,4) &
    iE1ttt %in% c(0,1,2,3,4) &
    iX2a %in% c(0,1,2,3,4,5) & 
    iX2c %in% c(0,1,2,3,4,5) &
    Age >= 0 & Age <= 130 &
    PARENTING__scale %in% c(0:12) ~ 1, TRUE ~ 0))

Combined <- Combined %>% mutate(validSSH = case_when(
  iE1oo %in% c(0,1,2,3,4) & 
    xsSOS %in% c(0,1,2,3,4,5,6) ~ 1, TRUE ~ 0))

##-----------
##  xparent  ## added ip15e and iP15f (as per SAS)
##-----------

Combined <- Combined %>% mutate(xparent = case_when(
  iP15a == 8 | iP15b == 8 | iP15c == 8 | iP15d == 8 | iP15e == 8 | iP15f ==8 ~ 1, TRUE ~ 0)) ## CHECK THIS!!!!!!!!!!!!!!!!!!!

##----------
##  xabuse  
##----------

Combined <- Combined %>% mutate(xabuse = case_when(
  iY1al %in% c(1,2,3,4,5) ~ + 1, iY1am %in% c(1,2,3,4,5) ~ + 1, 
  iY1an %in% c(1,2,3,4,5) ~ + 1, iY1aq %in% c(1,2,3,4,5) ~ + 1, 
  iY1ao %in% c(1,2,3,4,5) ~ + 1, iY1au %in% c(1,2,3,4,5) ~ + 1))

##-----------
##  xmedsym  
##-----------

Combined <- Combined %>% mutate(xmedsym = case_when(
  iJ2k %in% c(1,2,3,4) ~ + 1, iJ2y %in% c(1,2,3,4) ~ + 1, 
  iJ2dd %in% c(1,2,3,4) ~ + 1, iJ2gg %in% c(1,2,3,4) ~ + 1, 
  iJ2ll %in% c(1,2,3,4) ~ + 1, iJ3 %in% c(1,2,3) ~ + 1, iJ9a == 1 ~ + 1,
  iJ9b == 1 ~ + 1, iJ9c == 1 ~ + 1, iJ9d == 1 ~ + 1, iJ9e == 1 ~ + 1, 
  iJ9f == 1 ~ + 1, iJ9g == 1 ~ + 1))

##-----------
##  xsANXIETY  
##-----------

Combined <- Combined %>% mutate(xsANXIETY =  
                      iE1eee + iE1fff + iE1ggg + iE1hhh + iE1iii + iE1kkk + iE1lll + iE1eeee)

##------------
##  xsFAMILY  
##------------

Combined <- Combined %>% mutate(xsFAMILY = case_when(
  iP18d != 8 & iP19h !=8 & iY8a != 8 ~ case_when(
    iF8a == 0 ~ 1 + iP19d + iP19h + iY7a + iY7b + iY8a,
    iF8a == 1 ~ iP19d + iP19h + iY7a + iY7b + iY8a)))


##---------
##  ssSOSval  
##---------

Combined$ssSOSval <- ifelse(
  Combined$iX1c %in% c(0, 1, 2, 3, 4, 5, 8) &
    Combined$iX1e %in% c(0, 1, 2, 3, 4, 5, 8) &
    Combined$xsdsi %in% 0:36 &
    Combined$iX1d %in% c(0, 1, 2, 3, 4, 5, 8) &
    Combined$iE1nnn %in% c(0, 1, 2, 3, 4, 5, 8) &
    Combined$iX1b %in% c(0, 1, 2, 3, 4, 5, 8),
  1, 0
)


##---------
##  xcSSH  
##---------

Combined <- Combined %>% mutate(xcSSH = case_when(
  xsSOS %in% c(5,6) ~ 2, xsSOS %in% c(3,4) ~ case_when(
    iE1oo >= 2 ~ 2, iE1oo < 2 ~ 1), xsSOS < 3 ~ 0))


##---------
##  sRHOval  
##---------

Combined$sRHOval <- ifelse(
  Combined$iX2a %in% c(0,1,2,3,4,5) &
    Combined$iX2c %in% c(0,1,2,3,4,5) &
    Combined$iX2b %in% c(0,1,2,3,4,5) &
    Combined$iX16c %in% c(0,1,2,3,4,5) &
    Combined$iE1mmm %in% c(0,1,2,3,4,5) &
    Combined$iE1nnn %in% c(0,1,2,3,4,5) &
    Combined$iE1ooo %in% c(0,1,2,3,4,5) &
    Combined$iE1ppp %in% c(0,1,2,3,4,5),
  1, 0)

#################################################
# xsRHO 
#################################################

Combined <- Combined %>% mutate(xsRHO = case_when(
  iX2a %in% c(4,5) ~ 6, iX2a %in% c(2,3) ~ case_when( 
    iX2c %in% c(4,5) | iX2b %in% c(4,5) | iX16c %in% c(4,5) ~ 6, iE1mmm %in%
      c(2,3,4) | iE1nnn %in% c(2,3,4) | iE1ooo %in% c(2,3,4) | iE1ppp %in% 
      c(2,3,4) ~ 5, TRUE ~ 4),
  iX2a %in% c(0,1) ~ case_when(iX2c %in% c(4,5) | iX2b %in% c(4,5) | 
                                 iX16c %in% c(4,5) ~ case_when(
                                   iE1mmm %in% c(2,3,4) | iE1nnn %in% c(2,3,4) 
                                   | iE1ooo %in% c(2,3,4) | iE1ppp %in% 
                                     c(2,3,4) ~ 3, TRUE ~ 2), TRUE ~
                                 case_when(
                                   iE1mmm %in% c(2,3,4) | iE1nnn %in% c(2,3,4) 
                                   | iE1ooo %in% c(2,3,4) | iE1ppp %in% 
                                     c(2,3,4) ~ 1, TRUE ~ 0))))

#########################################################
# xcHARMOTH
#################################################

Combined <- Combined %>% mutate(xcHARMOTH = case_when(
  xsRHO %in% c(5,6) ~ 2, xsRHO %in% c(3,4) ~ case_when(
    iE1oo %in% c(2,3,4) ~ 2, iE1oo < 2 ~ 1), xsRHO < 2 ~ 0))

#########################################################
# RICHY__scale Calculation * NEW
#################################################

Combined <- Combined %>% mutate(RIChY_scale = case_when(
  Age <= 7 ~ case_when(
    iX2b %in% c(3,4,5) ~ 3, iX2b %in% c(0,1,2) ~ case_when(
      iE3q %in% c(2,3,4) ~ 1, iE3q %in% c(0,1) ~ 0)),
  
  Age %in% c(8,9,10,11) ~ case_when(
    iP19b == 8 | iP19e == 8 ~ case_when(
      xsANXIETY >= 7 ~ 5, xsANXIETY %in% c(0,1,2,3,4,5,6) ~ 3),
    iP19b != 8 & iP19e != 8 ~ case_when(
      iE15e %in% c(2,3,4,5) | iP19b == 1 ~ case_when(
        PARENTING__scale >= 3 | xparent == 1 ~ 4, 
        PARENTING__scale %in% c(0,1,2) ~ case_when(
          iE1yyy %in% c(2,3,4) ~ 3, iE1yyy %in% c(0,1) ~ 2)), ## FOUND ERROR should iE1yyy %in% c(2, 3, 4) ~ 3???
      iE15e %in% c(0,1) & iP19b %in% c(0, 8) ~ case_when( # 0 or 8
        xsFAMILY >= 3 | iY8a == 8 ~ 3, xsFAMILY %in% c(0,1,2) ~ case_when(
          xabuse >= 1 | xmedsym >= 1 ~ 1, xabuse == 0 & xmedsym == 0 ~ 0)))), 
    Age >= 12 ~ case_when(
    iE15e %in% c(2,3,4,5) | iP19b == 1 ~ case_when(
      xsANXIETY >= 10 ~ 3, xsANXIETY <= 9 ~ 5), 
    iE15e %in% c(0,1) & iP19b %in% c(0,8) ~ case_when( ## this one too
      xcSSH %in% c(1,2) ~ 4, xcSSH == 0 ~ case_when(
        xcHARMOTH %in% c(1,2) ~ 4, xcHARMOTH == 0 ~ case_when(
          xsFAMILY >= 1 | iY8a == 8 ~ case_when(
            xparent == 1 ~ 3, xparent == 0 ~ 2), xsFAMILY == 0 ~ 0))))))    


# test <- Combined %>% select(EMHID, validrichy, Age, iX2b, iE3q, iP19b, iP19e, xsANXIETY, iP19b, iP19e, iE15e, iP19b, PARENTING__scale, xparent, iE1yyy, xsFAMILY, iY8a, xabuse, xmedsym, xcSSH, xcHARMOTH, RIChY_scale, RIChY_scale2)

#########################################################
# ALEIC__scale
#################################################

Combined <- Combined %>% 
    mutate(ALEIC__scale = rowSums(
    data.frame(
      PhysAbuse = ifelse(iY1an %in% c(1,2,3,4,5), 1, 0),
      EmoAbuse = ifelse(iP19d == 1 | iY1ao %in% c(1, 2, 3, 4, 5), 1, 0),
      SexAbuse = ifelse(iY1am %in% c(1,2,3,4,5), 1, 0),
      WitnessViolence = ifelse(iP19g == 1 | iY1au %in% c(1, 2, 3, 4, 5), 1, 0),
      PhysNeglect = ifelse(iB34b %in% c(1,2,3) | iB34c%in% c(1,2,3) | iQ5a ==1 | iQ5b ==1, 1, 0),
      EmoNeglect = ifelse(iB34a %in% c(1, 2, 3), 1, 0),
      Neglect = ifelse(iY1at %in% c(1, 2, 3, 4, 5) | iY9 ==1 | iY8a %in% c(1, 2, 3, 4, 5), 1, 0),
      Parental = ifelse(iY1ap %in% c(1, 2, 3, 4, 5)| iY7a ==1, 1, 0),
      Placement = ifelse(iB35 ==1 | iB12 ==1, 1, 0),
      Bullying = ifelse(iY1aq %in% c(1, 2, 3, 4, 5), 1, 0),
      MHStressors = ifelse(iY7b ==1| iBB11b ==1, 1, 0),
      DeathLoss = ifelse(iY1ac %in% c(1, 2, 3, 4, 5)| iY1ar %in% c(1, 2, 3, 4, 5), 1, 0),
      Accident = ifelse(iY1aa %in% c(1, 2, 3, 4, 5), 1, 0),
      Disaster = ifelse(iY1ak %in% c(1,2,3,4,5)| iY1al %in% c(1,2,3,4,5), 1, 0),
      WarZone = ifelse(iY1aj %in% c(1,2,3,4,5), 1, 0),
      Migration = ifelse(iY1ai %in% c(1,2,3,4,5), 1, 0)
    ), na.rm=TRUE))

aleic.model <- readRDS("M:/DATA & REPORTS/Clinical & Client Services/Clinical Data/Models/aleicmodel.rda")

Combined$ALEIC_norm <- predictNorm(Combined$ALEIC__scale, Combined$AgeN, dsi.model, minNorm = 10, maxNorm = 100)

#########################################################
# TSS__scale
#################################################

Combined <- Combined %>% 
  mutate(TSS__scale = case_when(
    is.na(ALEIC__scale) ~ NA,
    TRUE ~ (ifelse(iE1pp %in% 1:5, 1, 0) +
              ifelse(iE1ss %in% 1:5, 1, 0) +
              ifelse(iE1vv %in% 1:5, 1, 0) +
              ifelse(iE1ww %in% 1:5, 1, 0) +
              ifelse(iE1xx %in% 1:5, 1, 0) +
              ifelse(iE1yy %in% 1:5, 1, 0) +
              ifelse(iE1aaa %in% 1:5, 1, 0) +
              ifelse(iE1iii %in% 1:5, 1, 0) +
              ifelse(iE1jjj %in% 1:5, 1, 0) +
              ifelse(iE1kkk %in% 1:5, 1, 0) +
              ifelse(iE1lll %in% 1:5, 1, 0) +
              ifelse(iE1eeee %in% 1:5, 1, 0) +
              ifelse(iE1qqq %in% 1:5, 1, 0) +
              ifelse(iE1sss %in% 1:5, 1, 0) +
              ifelse(iE1ttt %in% 1:5, 1, 0) +
              ifelse(iE1yyy %in% 1:5, 1, 0) +
              ifelse(iE1zzz %in% 1:5, 1, 0) +
              ifelse(iB10a %in% c(0, 8), 0, ifelse(iB10a == 1, 1, 0)) +
              ifelse(iY1b %in% c(0, 8), 0, ifelse(iY1b == 1, 1, 0)))))

tss.model <- readRDS("M:/DATA & REPORTS/Clinical & Client Services/Clinical Data/Models/tssmodel.rda")

Combined$TSS_norm <- predictNorm(Combined$TSS__scale, Combined$AgeN, tss.model, minNorm = 20, maxNorm = 72)


############################################################################
############################################################################
###                                                                      ###
###                  SELECT COLUMNS FOR INTERRAI BI CSV                  ###
###                                                                      ###
############################################################################
############################################################################
# goals met, sex, etc. have changed names
# discharge_a to discharge_d have changed to oGoal1:oGoal4
# PAIN__scale temporarily removed

interRAI <- Combined %>%
 select("iA3", "client_id", "iA2", "ref", "compl", "survey_type", "program_name", "instrument", "oV5", "oGoal1", "oGoal2", "oGoal3", "oGoal4", "iA5a", "record_id", "client_id", "worker_id", "program_id", "cihiA2a", "cihiA2c", "iA2", "Tool", "Type", "EMHID", "Age", "AgeN", "FirstChYMH", "FirstAx", "FirstScreener", "LastAx", "LastChYMH", "LastScreener", "DSI__scale", "DSI_norm","ANX__scale","ANX_norm", "CHAMHPS__scale", "DABS__scale", "DABS_norm", "SOCDIS__scale", "SOCDIS_norm", "DISTRACT__scale", "DISTRACT_norm" ,"KinarkRiskHTO_comb", "KinarkRiskHTS_comb", "KinarkRiskHTP_comb", "KinarkRiskSM_comb", "KinarkRiskVCP_comb", "KinarkRiskFB_comb", "KinarkRiskOC_comb", "Acute", "Complex", "KinarkRiskHTO_cat", "KinarkRiskHTS_cat", "KinarkRiskHTP_cat", "KinarkRiskSM_cat", "KinarkRiskVCP_cat", "KinarkRiskFB_cat", "KinarkRiskOC_cat", "Acute_cat", "Complex_cat", "ED_Screen", "ID_Screen", "ComorbidMedical_Screen", "PSS__scale", "PSS_norm", "Trauma", "RISSK__scale", "RIO__scale", "SOS__scale", "RHO__scale", "IADLC__scale", "CAREDIS__scale", "COG_norm", "PARENTING__scale", "PARENTING_norm", "COMM__scale", "CAS", "Psychiatrist", "FamilyEngaged", "Physical Activity", "Attachment", "Caffeine Use", "Caregiver Distress", "Communication", "Control Interventions", "Criminality Prevention", "Education", "Hazardous Fire Involvement", "Gambling", "Harm to Others", "Informal Support", "Interpersonal Conflict", "Life Skills", "Medication Adherence", "Medication Review", "Parenting2", "Readmission", "Suicidality and Purposeful Self-Harm", "Sexual Behaviour", "Sleep Disturbance", "Social and Peer Relationships", "Strengths", "Substance Use", "Support Systems for Discharge", "Tobacco and Nicotine Use", "Transitions", "Traumatic Life Events", "Video Gaming", "Eating/Weight", "ABS__scale", "ABS_norm", "EXT__scale","EXT_norm", "INT__scale", "INT_norm", "ADL__scale", "PEER__scale", "FAM__scale", "FAM_norm", "STRENGTHS__scale", "SCHLDISRUPT__scale", "SENSORY__scale", "INT-S__scale", "INT-S_norm", "EXT-S__scale", "EXT-S_norm", "AUT__scale", "COG__scale", "MANIA__scale", "SLEEP__scale","AUT_norm", "Psychiatric Diagnoses", "Medical Diagnoses", "ID/GDD", "Residential Issues", "RIChY_scale", "ALEIC__scale","ALEIC_norm", "TSS__scale", "TSS_norm", "ADL_norm", "PEER_norm", "RISSK_norm", "IADLC_norm", "SOS_norm", "SENSORY_norm", "SLEEP_norm", "STRENGTHS_norm", "SCHLDISRUPT_norm")

rm(ChYMH_export, Combined, DetailedHistory_export, ID_Lookup, interRAI_ideasWeb, Table1, Table4, df_list) # Remove unnecessary columns

#test <- Combined %>% select(ends_with("_norm"))
#test2 <- interRAI %>% select(ends_with("_norm"))

# save(list = ls(all.names = TRUE), file= "all.rda")
# ## restore the saved values to the current environment
# local({
#   load("all.rda")
#  # ls()
# })

#write.csv(interRAI, "InterRAI_b.csv")
