
###########################################################################
###########################################################################
###                                                                     ###
###                     INTERRAI TO POWER BI SCRIPT                     ###
###                                                                     ###
###########################################################################
###########################################################################

rm(list = ls())

library(readr)
library(dplyr)
library(readxl)
library(data.table)
library(magrittr)
library(stringr)

##################################################################
##                       Reference Tables                       ##
##################################################################

# First grab our reference tables

ID_Lookup <- read_excel("M:/DATA & REPORTS/Clinical & Client Services/Clinical Data/interRAI/interRAI Data.xlsx", sheet = "ID Lookup")

ID_Lookup <- subset(ID_Lookup, select=-c(4, 5, 10:12, 18:34)) 

# Chart Number / EMHID / CRN lookup

#Table1 <- subset(ID_Lookup, select=c(1:3)) ## this one is needed

# Chart Number / ID lookup

#Table4 <- subset(ID_Lookup, select=c(4:7)) ## this one is needed

#Table4 <- Table4 %>% rename("Chart Number" = "Chart Number...7", "Gender" = "Gender...9")

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

##################################################################
##                  Pull EMHware interRAI Data                  ##
##################################################################

## Grab EMHware codebook

EMHware_CodeBook <- read_excel("M:\\DATA & REPORTS\\Clinical & Client Services\\Clinical Data\\interRAI\\interRAI Matching Codebook.xlsx", sheet = "EMHware Variables")

#### Grab new ChYMH data

ChYMH_folder <- "M:/DATA & REPORTS/Clinical & Client Services/Clinical Data/interRAI/EMHware interRAI ChYMH Export"

allfiles <- list.files(path=ChYMH_folder, pattern="*.csv", full.names=TRUE)

ChYMH_export <- plyr::ldply(allfiles, read_csv, show_col_types = FALSE)

ChYMH_1 <- filter(ChYMH_export,!is.na(program_name)) # remove rows w/NAs in program_name

ChYMH_1 <- subset(ChYMH_1, !grepl("default", program_name)) # remove rows w/default as program

ChYMH_1$SOURCE<-"EMHware"

# convert all true and false to 0 and 1
# use the code below to subset all columns that contain the string (to double check not removing important text)
#sub <- ChYMH_export[grep("true", ChYMH_export)] 

ChYMH_1 <- ChYMH_1 %>%
  mutate(across(everything(), ~sub("true", "1", .)))
ChYMH_1 <- ChYMH_1 %>%
  mutate(across(everything(), ~sub("false", "0", .)))
ChYMH_1 <- ChYMH_1 %>%
  mutate(across(everything(), ~sub("TRUE", "1", .)))
ChYMH_1 <- ChYMH_1 %>%
  mutate(across(everything(), ~sub("FALSE", "0", .)))

# auto format column types

ChYMH_2 <- type_convert(ChYMH_1)

ChYMH_3 <- transform(ChYMH_2,cihiU3=as.numeric(cihiU3),client_id=as.character(client_id),iA5d=as.character(iA5d))


ChYMH_3$iA8 <- gsub("1", "Initial", ChYMH_3$iA8)
ChYMH_3$iA8 <- gsub("6First", "Initial", ChYMH_3$iA8) 
ChYMH_3$iA8 <- gsub("5First", "Initial", ChYMH_3$iA8)
ChYMH_3$iA8 <- gsub("5", "Discharge", ChYMH_3$iA8)
ChYMH_3$iA8 <- gsub("2", "Reassessment", ChYMH_3$iA8)
ChYMH_3$iA8 <- gsub("6", "Discharge", ChYMH_3$iA8)
ChYMH_3$iA8 <- gsub("4", "Reassessment", ChYMH_3$iA8)

ChYMH_4 <- subset(ChYMH_3, !grepl("default", program_name))

# Renaming columns

df <- ChYMH_4 %>%
  rename("ref"="iA9",
         "ED_Screen"="iW29",
         "ID_Screen"="iA47",
         "ComorbidMedical_Screen" = "iA48")

############################################################################
############################################################################
###                                                                      ###
###                          CALCULATED COLUMNS                          ###
###                                                                      ###
############################################################################
############################################################################

##------------------------------------------------------------------------
##  1. Client ID   
##------------------------------------------------------------------------


CIS_ID1$'Client ID#' <- as.character(CIS_ID1$'Client ID#')
CIS_ID1$`BI CIS Client Identifier` <- as.character(CIS_ID1$`BI CIS Client Identifier`)
#Table4$ID <- as.character(Table4$ID)

# If client_id has a valid value, return CIS_ID1$client ID#, otherwise match client_id with BI CIS Client 

df$EMHID <- ifelse(!is.na(df$client_id),
                         CIS_ID1$`Client ID#`[match(df$client_id, CIS_ID1$`BI CIS Client Identifier`)],
                         NA)

##----------------------------
##   1. Assessment tool used  
##----------------------------

df <- df %>% mutate(Tool = if_else(type_cd == "chymh_s_plus","rscr", 
                                            "cymh"))

##------------------------------------------------------------------------
##  2. Type of assessment (Initial, Reassessment, Monitoring, Discharge)   
##------------------------------------------------------------------------

df <- df %>% mutate(Type = if_else(Tool =="chymh_monitoring_op", "Monitoring Assessment",
                                                       if_else(is.na(iA8), oA8, iA8))) 

df$Type <- recode(df$Type,`S-INTL`="Initial",
          `2`= "Reassessment",
          `3`="Initial",
          `7`="Other",
        `S-SQNT`="Reassessment",
         `Readmission Initial`="Initial")
             
##---------------------------------
##  4. Last assessment completed      
##---------------------------------


df <- mutate(df,
                   compl = if_else(is.na(iU2), iT1, iU2))

##---------------------
##  6. Calculate Age   
##---------------------

# calculate age in years at given reference date (df$ref)

df$Age <- as.integer((df$ref - as.Date(paste0(df$iA3, "-01-01")))/365.25)

##------------------------------------------
##  7. Calculate FirstChYMH and last ChYMH  
##------------------------------------------

df <- df %>%
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

df <- df %>%
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
 
df <- df %>%
  group_by(EMHID) %>%
  mutate(MinRef = min(if_else(Tool == "rscr", ref, as.Date(Inf)), na.rm = FALSE),
         MaxRef = max(if_else(Tool == "rscr", ref, as.Date(-Inf)), na.rm = FALSE),
         FirstScreener = as.integer(ref == MinRef),
         LastScreener = as.integer(ref == MaxRef)) %>%
  ungroup() %>%
  select(-MinRef, -MaxRef)

#test <- df %>% select(EMHID, Tool, ref, FirstChYMH, LastChYMH, FirstAx, LastAx, FirstScreener, LastScreener)

##------------------
##  10. DSI_Scale 
##------------------

df <- df %>% mutate(validdsi = case_when(
  iE1ss %in% c(0,1,2,3,4) &
    iE1vv %in% c(0,1,2,3,4) &
    iE1ww %in% c(0,1,2,3,4) &
    iE1xx %in% c(0,1,2,3,4) &
    iE1yy %in% c(0,1,2,3,4) ~ 1, TRUE ~ 0))

df <- df %>% mutate(DSI__scale = case_when(validdsi == 1 ~ 
                                         (case_when(iE1ss %in% c(0,1,2,3) ~ + iE1ss, iE1ss == 4 ~ + 3)) +
                                         (case_when(iE1vv %in% c(0,1,2,3) ~ + iE1vv, iE1vv == 4 ~ + 3)) +
                                         (case_when(iE1ww %in% c(0,1,2,3) ~ + iE1ww, iE1ww == 4 ~ + 3)) +
                                         (case_when(iE1xx %in% c(0,1,2,3) ~ + iE1xx, iE1xx == 4 ~ + 3)) +
                                         (case_when(iE1yy %in% c(0,1,2,3) ~ + iE1yy, iE1yy == 4 ~ + 3))))

##------------------
##  11. ANX Scores  
##------------------

df <- df %>% mutate(validnxs = case_when(
  iE1eee %in% c(0,1,2,3,4) & 
    iE1fff %in% c(0,1,2,3,4) & 
    iE1ggg %in% c(0,1,2,3,4) & 
    iE1iii %in% c(0,1,2,3,4) & 
    iE1kkk %in% c(0,1,2,3,4) & 
    iE1lll %in% c(0,1,2,3,4) & 
    iE1eeee %in% c(0,1,2,3,4) ~ 1, TRUE ~ 0))

df <- df %>% mutate(ANX__scale = case_when(validnxs == 1 ~ 
                                          iE1eee + iE1fff + iE1ggg + iE1iii + iE1kkk + iE1lll + iE1eeee))

##--------------
##  12. CHAMHP  
##--------------

df <- df %>%
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

#test <- df %>% filter(Age >= 12 & iX14a >=3) %>% select(EMHID,Age, iX14a, iBB5f, iEEE2, iE1qqq, CHAMHPS__scale)

##------------------
##  13. DABS scale  
##------------------

df <- df %>% mutate(validdabs = case_when( 
  iE3l %in% c(0,1,2,3,4) &
    iE3m %in% c(0,1,2,3,4) &
    iE3n %in% c(0,1,2,3,4) &
    iE3q %in% c(0,1,2,3,4) &
    iE3r %in% c(0,1,2,3,4) ~ 1, TRUE ~ 0))

df <- df %>% mutate(DABS__scale = case_when(validdabs == 1 ~
                                          iE3l + iE3m + iE3n + iE3q + iE3r))

##--------------
##  ABS__scale  
##--------------

df <- df %>% mutate(validabs = case_when(
  iE3l %in% c(0,1,2,3,4) & 
    iE3m %in% c(0,1,2,3,4) & 
    iE3n %in% c(0,1,2,3,4) & 
    iE3p %in% c(0,1,2,3,4) ~ 1,
  TRUE ~ 0)) 

df <- df %>% mutate(ABS__scale = case_when(validabs==1 ~ 
                                                         (case_when(iE3l %in% c(0,1,2,3) ~ iE3l, iE3l == 4 ~ + 3)) + 
                                                         (case_when(iE3m %in% c(0,1,2,3) ~ + iE3m, iE3m == 4 ~ + 3)) +
                                                         (case_when(iE3n %in% c(0,1,2,3) ~ + iE3n, iE3n == 4 ~ + 3)) + 
                                                         (case_when(iE3p %in% c(0,1,2,3) ~ + iE3p, iE3p == 4 ~ + 3))))
##-------------------
##  14. SOCDIS scale: sCYMHANHEDO
##-------------------

df <- df %>% mutate(validedo = case_when(
  iE1qqq %in% c(0,1,2,3,4) &                                            
    iE1rrr %in% c(0,1,2,3,4) &                                                 
    iE1sss %in% c(0,1,2,3,4) &                                              
    iE1ttt %in% c(0,1,2,3,4) ~ 1, TRUE ~ 0))

df <- df %>% mutate('SOCDIS__scale' = case_when(validedo == 1 ~
                                              iE1qqq + iE1rrr + iE1sss + iE1ttt))

##-----------------------
##  15. DISTRACT_scale   
##-----------------------

df <- df %>% mutate(validact = case_when(
  iE1oo %in% c(0,1,2,3,4) &
    iE1pp %in% c(0,1,2,3,4)&
    iE1qq %in% c(0,1,2,3,4)&
    iE1rr %in% c(0,1,2,3,4) ~ 1, TRUE ~ 0))

df <- df %>% mutate(DISTRACT__scale = case_when(validact == 1 ~ (
  iE1oo +iE1pp +iE1qq + iE1rr)))

##--------------------------
##  16. KinarkRiskHTO_comb  
##--------------------------

df$KinarkRiskHTO_comb <- df$KinarkRiskHTO

##--------------------------
##  17. KinarkRiskHTS_comb  
##--------------------------

df$KinarkRiskHTS_comb <- df$KinarkRiskHTS

##--------------------------
##  18. KinarkRiskHTP_comb  
##--------------------------

df$KinarkRiskHTP_comb<- df$KinarkRiskHTP

##-------------------------
##  19. KinarkRiskSM_comb  
##-------------------------

df$KinarkRiskSM_comb <- df$KinarkRiskSM

##--------------------------
##  20. KinarkRiskVCP_comb  
##--------------------------
df$KinarkRiskVCP_comb<- df$KinarkRiskVCP

##--------------------------
##  21. KinarkRiskFB_comb  
##--------------------------

df$KinarkRiskFB_comb <- df$KinarkRiskFB

##-------------------------
##  22. KinarkRiskOC_comb  
##-------------------------
df$KinarkRiskOC_comb <- df$KinarkRiskOC

##-------------
##  23. Acute  
##-------------

df$Acute <- df$CaseComplexityFA

##---------------
##  24. Complex  
##---------------

df$Complex <- df$CaseComplexityS

##-------------------------
##  25. KinarkRiskHTO_cat  
##-------------------------

df <- df %>%
  mutate(KinarkRiskHTO_cat = case_when(
    KinarkRiskHTO == 0 ~ "No Risk",
    KinarkRiskHTO == 1 ~ "Potential Risk",
    KinarkRiskHTO == 2 ~ "Risk Identified",
    KinarkRiskHTO == 3 ~ "Serious Risk"
  ))

##-------------------------
##  26. KinarkRiskHTS_cat  
##-------------------------

df <- df %>%
  mutate(KinarkRiskHTS_cat = case_when(
    KinarkRiskHTS  == 0 ~ "No Risk",
    KinarkRiskHTS  == 1 ~ "Potential Risk",
    KinarkRiskHTS  == 2 ~ "Risk Identified",
    KinarkRiskHTS  == 3 ~ "Serious Risk"
  ))

##------------------------
##  27. KinarkRiskSM_cat  
##------------------------

df <- df %>%
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

df <- df %>%
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

df <- df %>%
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

df <- df %>%
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

df <- df %>%
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

df <- df %>%
  mutate(Acute_cat = case_when(
    is.na(Acute)  ~ NA,
    Acute == 1 ~ "Yes",
    Acute == 0 ~ "No"
  ))

##-------------------
##  33. Complex_cat  
##-------------------

df <- df %>%
  mutate(Complex_cat = case_when(
    is.na(Complex)  ~ NA,
    Complex == 1 ~ "Yes",
    Complex == 0 ~ "No"
  ))



##------------------
##  43. Positive Symptoms Scale  
##------------------

df <- df %>% mutate(validpss = case_when(iE1mmm %in% c(0,1,2,3,4) & 
                                                       iE1ooo %in% c(0,1,2,3,4) & 
                                                       iE1nnn %in% c(0,1,2,3,4) & 
                                                       iE1ppp %in% c(0,1,2,3,4) ~ 1,
                                                     TRUE ~ 0))

df <- df %>% mutate(PSS__scale = case_when(validpss == 1 ~ 
                                                     (case_when(iE1mmm %in% c(0,1,2,3) ~ iE1mmm, iE1mmm == 4 ~ + 3)) + 
                                                     (case_when(iE1ooo %in% c(0,1,2,3) ~ + iE1ooo, iE1ooo == 4 ~ + 3)) +
                                                     (case_when(iE1nnn %in% c(0,1,2,3) ~ + iE1nnn, iE1nnn == 4 ~ + 3)) +
                                                    (case_when(iE1ppp %in% c(0,1,2,3) ~ + iE1ppp, iE1ppp == 4 ~ + 3)))) 
df$sCYPSS <- df$PSS__scale

##---------------
##  36.  Trauma  
##---------------

df <- df %>%
  mutate(Trauma = case_when(
    is.na(iY1b)  ~ NA,
    iY1b == 1 ~ "Caused Intense Fear",
    iY1b == 0 ~ "No or Not Applicable",
    iY1b == 8 ~ "Did Not Respond"
  ))


##--------------------
##  37.  RISSK_scale  
##--------------------

df <- df %>% mutate(validssk = case_when(
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

df <- df %>% mutate(RISSK__scale = case_when(validssk == 1 ~ 
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

##-----------------
##  40. RIO_scale  
##-----------------

df <- df %>% mutate(validrio = case_when(
  iE1oo %in% c(0,1,2,3,4) &
    iE3l %in% c(0,1,2,3,4) &
    iE3m %in% c(0,1,2,3,4) & 
    iE3n %in% c(0,1,2,3,4) & 
    iE3q %in% c(0,1,2,3,4) &
    iP19h %in% c(0,1,8) &
    iX2a %in% c(0,1,2,3,4,5) &
    iX2b %in% c(0,1,2,3,4,5) &
    iX2c %in% c(0,1,2,3,4,5) ~ 1, TRUE ~ 0))

df <- df %>% mutate(xrio = case_when(validrio == 1 ~ iX2a + iX2b + iX2c))

df <- df %>% mutate(xbeh = case_when(validrio ==1 ~ 
                                       (case_when(iE3l %in% c(2,3,4) ~ iE3l-1) +
                                          case_when(iE3n %in% c(2,3,4) ~ iE3n-1))))

df <- df %>% mutate(RIO__scale = case_when(validrio == 1 ~ 
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

df <- df %>% mutate(validsos = case_when(
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


df <- df %>% mutate(xcsp1 = case_when(
  iC1d %in% c(0,1,2,3,4,5) &
    iD1 %in% c(0,1,2,3,4) &
    iC2a %in% c(0,1) ~ 1, TRUE ~ 0))


df <- df %>% mutate(xcps2 = case_when(
  iC1d %in% c(0,1,2,3,4,5) &
    iD1 %in% c(0,1,2,3,4) ~ 1, TRUE ~ 0))


# Impairment count

df <- df %>% mutate(xcps1 = case_when(validsos == 1 ~
                                                    (
                                                      case_when(iC1d %in% c(1, 2, 3) ~ 1, TRUE ~ 0) +
                                                        case_when(iD1 %in% c(1, 2, 3, 4) ~ 1, TRUE ~ 0) +
                                                        case_when(iC2a == 1 ~ +1, TRUE ~ 0)
                                                    )))

# Severe Imp count

df <- df %>% mutate(xcps2 = case_when(validsos == 1 ~
                                                    (
                                                      case_when(iC1d == 3 ~ 1, TRUE ~ 0) +
                                                        case_when(iD1 %in% c(3, 4) ~ +1, TRUE ~ 0)
                                                    )))
# cog performance count

df <- df %>% mutate(xcps = case_when(validsos == 1 ~
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

df <- df %>% mutate(xdsi = case_when(
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


df <- df %>% mutate(xpss = case_when(validsos == 1 ~
                                                   (case_when(iE1mmm == 0 ~ +0, iE1mmm %in% c(1, 2) ~ +1, iE1mmm %in% c(3, 4) ~ +2) +
                                                       case_when(iE1nnn == 0 ~ +0, iE1nnn %in% c(1, 2) ~ +1, iE1nnn %in% c(3, 4) ~ +2) +
                                                       case_when(iE1ooo == 0 ~ +0, iE1ooo %in% c(1, 2) ~ +1, iE1ooo %in% c(3, 4) ~ +2) +
                                                       case_when(iE1ppp == 0 ~ +0, iE1ppp %in% c(1, 2) ~ +1, iE1ppp %in% c(3, 4) ~ +2)
                                                   )))

## CY Severity of Self Harm Scale

df <-
  df %>% mutate(SOS__scale = case_when(validsos == 1 ~
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

df$sCYSOS <- df$SOS__scale

##-----------------
##  42. RHO_scale  
##-----------------

df <- df %>% mutate(validrho = case_when(
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


df <- df %>% mutate(xrho1 = case_when(
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

df <- df %>% mutate(RHO__scale = case_when(validrho == 1 ~ case_when(
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

df$sCYRHO <- df$RHO__scale

##--------------------
##  44. IADLC__scale  
##--------------------

df <- df %>% mutate(validiadlc = case_when(
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

df <- df %>% mutate(IADLC__scale = case_when(validiadlc == 1 ~ case_when(
  Age >= 12 & Age < 19 ~ 
    iG1ab + iG1bb + iG1cb + iG1db + iG1eb + iG1fb + iG1gb + iG1hb + iG1nb + iG1ob
  + iZZ1ab)))

##-----------------------------------------------
##  sCYPS PARENTING STRENGTHS (for RICHY calc) -- 
##-----------------------------------------------

df <- df %>% mutate(validps = case_when(
  iP15a %in% c(0,1,2) &
    iP15b %in% c(0,1,2) &
    iP15c %in% c(0,1,2) &
    iP15d %in% c(0,1,2) &
    iP15e %in% c(0,1,2) &
    iP15f %in% c(0,1,2) ~ 1, TRUE ~ 0))

df <- df %>% mutate(PARENTING__scale = case_when(validps == 1 ~
                                                               (case_when(iP15a == 0 ~ + 2, iP15a == 1 ~ + 1, iP15a == 2 ~ + 0)) +
                                                               (case_when(iP15b == 0 ~ + 2, iP15b == 1 ~ + 1, iP15b == 2 ~ + 0)) +
                                                               (case_when(iP15c == 0 ~ + 2, iP15c == 1 ~ + 1, iP15c == 2 ~ + 0)) +
                                                               (case_when(iP15d == 0 ~ + 2, iP15d == 1 ~ + 1, iP15d == 2 ~ + 0)) +
                                                               (case_when(iP15e == 0 ~ + 2, iP15e == 1 ~ + 1, iP15e == 2 ~ + 0)) +
                                                               (case_when(iP15f == 0 ~ + 2, iP15f == 1 ~ + 1, iP15f == 2 ~ + 0))))
##--------------------
##  CAREDIS__scale  Caregiver Distress Algorithm --
##--------------------

df <- df %>% mutate(ProactiveItem = 
                      (case_when(iE15a== 0 ~ + 0, iE15a %in% c(1,2,3,4,5) ~ + 1)) +
                      (case_when(iE15d== 0 ~ + 0, iE15d %in% c(1,2,3,4,5) ~ + 1)) + 
                      (case_when(iE15e== 0 ~ + 0, iE15e %in% c(1,2,3,4,5) ~ + 1)) + 
                      (case_when(iX16c== 0 ~ + 0, iX16c %in% c(1,2,3,4,5) ~ + 1)) +        
                      (case_when(iX2a== 0 ~ + 0, iX2a %in% c(1,2,3,4,5) ~ + 1)) +           
                      (case_when(iX2b== 0 ~ + 0, iX2b %in% c(1,2,3,4,5) ~ + 1)) +           
                      (case_when(iX2c== 0 ~ + 0, iX2c %in% c(1,2,3,4,5) ~ + 1)))

df <- df %>% mutate(devMHsubst_Issues = 
                      (case_when(iY7a & iY1ap == 0 ~ 0, TRUE ~ 0) +
                         case_when(iY7a == 1 ~ 1, TRUE ~ 0) +
                         case_when(iY1ap %in% c(1,2,3,4,5) ~ 1)))

df <- df %>% mutate(familyfunction = 
                      (case_when(iF8a == 1 ~ 1, TRUE ~ 0) +
                         case_when(iF8a == 0 ~  0, TRUE ~ 0) + 
                         case_when(iP19d | iP19e == 1 ~  1, TRUE ~ 0) + 
                         case_when(iP19d & iP19e == 0 ~  0, TRUE ~ 0) + 
                         case_when(iY7a & iY1ap == 0 ~  0, TRUE ~ 0) + 
                         case_when(iY7a == 1 | iY1ap %in% c(1,2,3,4,5) ~  1, TRUE ~ 0) + 
                         case_when(iY7b == 1 ~  1, TRUE ~ 0) + 
                         case_when(iY7b == 0 ~  0, TRUE ~ 0)))

df <- df %>% mutate(validiccared = case_when(
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

df <- df %>% mutate(CAREDIS__scale = case_when(validiccared == 1 ~ 
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

df <- df %>% mutate(validcomm = case_when(
  iD1 %in% c(0,1,2,3,4) & 
    iD2 %in% c(0,1,2,3,4) ~ 1, TRUE ~ 0))

df <- df %>% mutate(COMM__scale = case_when(validcomm == 1 ~ iD1 + iD2))

##--------------------------------------------
##   CAS / Psychiatrist / Family Engagment  --
##--------------------------------------------

df <- df %>%
  mutate(CAS = case_when(
    is.na(iN16w)  ~ NA,
    iN16w > 0 ~ "Involved"
  ))

df <- df %>%
  mutate(Psychiatrist = case_when(
    is.na(iN16o)  ~ NA,
    iN16w > 0 ~ "Involved"
  ))

df <- df %>%
  mutate(FamilyEngaged = case_when(
    is.na(iZ10)  ~ NA,
    iN16w > 0 ~ "Involved"
  ))

##------------------------
##   Physical Activity  --
##------------------------

df <- df %>% 
  mutate(cCYMHPA = case_when(
  iG6a %in% c(0,1) ~ 1, TRUE ~ 0))


df$'Physical Activity' <-
  ifelse(is.na(df$cCYMHPA),
    NA,
      ifelse(
        df$cCYMHPA == 1,
        "Triggered",
        ifelse(df$cCYMHPA == 0,
            "Not Triggered",
            NA
          )))

##------------------------
##  Attachment          --
##------------------------

 df <-  df %>% mutate(validattach = case_when(
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

 df <-  df %>% mutate(xcymhattach1 = case_when(validattach == 1 ~ 
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

 df <-  df %>% mutate(xcymhattach2 = case_when(validattach == 1 ~ 
                                               (case_when(iP16a == 1 ~ + 1, TRUE ~ 0) +
                                                  case_when(iP16b ==1 ~ + 1, TRUE ~ 0) +
                                                  case_when(iP16c == 1 ~ + 1, TRUE ~ 0))))
 
  df <-  df %>% mutate(Attachment = case_when( 
   xcymhattach1 >= 1 & xcymhattach2 >= 1 ~ 1, 
   xcymhattach1 == 0 | xcymhattach2 == 0 ~ 0))

  
  ##################################################################
  ##                         Caffeine Use                         ##
  ##################################################################
  
 df <-  df %>% mutate('Caffeine Use' = case_when(
    iW10b %in% c(2,3) ~ 1, 
    iW10b < 2 ~ 0))

  df$'Caffeine Use'[df$'Caffeine Use'== 1] <- "Triggered"
  df$'Caffeine Use'[df$'Caffeine Use'== 0] <- "Not Triggered"


  
  #################################################################
  ##                      Caregiver Distress                      ##
  #################################################################

   df <-  df %>% mutate(xcymhcaredist = case_when(iB32a | iB32b == 1 ~ + 1, TRUE ~ 0) +
                                                     case_when(iP19g == 1 ~ + 1, TRUE ~ 0) +
                                                     case_when(iQ4 == 1 ~ + 1, TRUE ~ 0) +
                                                     case_when(iY1ap %in% c(2,3,4,5) ~ + 1, TRUE ~ 0) +
                                                     case_when(iY7a == 1 ~ + 1, TRUE ~ 0) +
                                                     case_when(iY8a == 1 ~ + 1, TRUE ~ 0) +
                                                     case_when(iY8b == 1 ~ + 1, TRUE ~ 0) +
                                                     case_when(iY9 == 1 ~ + 1, TRUE ~ 0))
  
 df <- df %>% mutate('Caregiver Distress' = case_when(
    xcymhcaredist >= 2 ~ 1, TRUE ~ 0))
  
 df$'Caregiver Distress'[df$'Caregiver Distress'== 1] <- "Triggered"
 df$'Caregiver Distress'[df$'Caregiver Distress'== 0] <- "Not Triggered"
 
 #################################################################
 ##                        Communication    : cCYMHCOMM                    ##
 #################################################################
 
 
  df <-  df %>% mutate('Communication' = case_when(
   iD1 %in% c(2,3,4) | iD2 %in% c(2,3,4) ~ 1, 
   iD1 < 2 & iD2 < 2 ~ 0))
 
 df$'Caregiver Distress'[df$'Caregiver Distress'== 1] <- "Triggered"
 df$'Caregiver Distress'[df$'Caregiver Distress'== 0] <- "Not Triggered"
 
 #################################################################
 ##                    Control Interventions                    ##
 #################################################################
 
  df <-  df %>% mutate(validctrlint = case_when(
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
 
  df <-  df %>% mutate(xcymhctrlint1 = case_when(validctrlint == 1 ~ 
                                                 (case_when(iM7 %in% c(1,2,3,4,5,6,7,8,9) ~ + 1, TRUE ~ 0) +
                                                    case_when(iN6c %in% c(1,2,3,4,5) ~ + 1, TRUE ~ 0) +
                                                    case_when(iAA1a %in% c(1,2,3,4,5) ~ + 1, TRUE ~ 0) +
                                                    case_when(iAA1c %in% c(1,2,3,4,5) ~ + 1, TRUE ~ 0) + 
                                                    case_when(iAA1f %in% c(1,2,3,4,5) ~ + 1, TRUE ~ 0))))
 
  df <-  df %>% mutate(xcymhctrlint2 = case_when(validctrlint == 1 ~ 
                                                 (case_when(iE1nnn %in% c(2,3,4) ~ + 1, TRUE ~ 0) + 
                                                    case_when(iX1a == 5 ~ + 1, TRUE ~ 0) +
                                                    case_when(iX2a == 5 ~ + 1, TRUE ~ 0) + 
                                                    case_when(iX4 == 2 ~ + 1, TRUE ~ 0) + 
                                                    case_when(sCYABS >= 5 ~ + 1, TRUE ~ 0) +
                                                    case_when(sCYPSS >= 8 ~ + 1, TRUE ~ 0))))
  
  df <- df %>% mutate('Control Interventions'= case_when(validctrlint == 1 ~ 
                                                 (case_when(xcymhctrlint1 & xcymhctrlint2 >= 1 ~ 2, TRUE ~ 0) +
                                                    case_when(xcymhctrlint1 >= 1 & xcymhctrlint2 == 0 ~ 1, TRUE ~ 0)))) 
  
  df$'Control Interventions'[df$'Control Interventions'== 1] <- "Triggered to minimize the need for control interventions (young persons not in a psychiatric emergency situation)"
  df$'Control Interventions'[df$'Control Interventions'== 0] <- "Not Triggered"
  
  ##################################################################
  ##                    Criminality Prevention                    ##
  ##################################################################
  
  
   df <-  df %>%
    mutate('Criminality Prevention' = case_when(
    iE1dddd %in% c(1,2,3,4) | iE15a %in% c(2,3,4,5) | iX2a %in% c(2,3,4,5) ~ 1, 
    iE1dddd < 1 & iE15a < 2 & iX2a < 2 ~ 0))

  df$'Criminality Prevention'[df$'Criminality Prevention'== 1] <- "Triggered"
  df$'Criminality Prevention'[df$'Criminality Prevention'== 0] <- "Not Triggered"
  
  
  #################################################################
  ##                          Education: cCYMHEDU                         
  #################################################################
  
   df <-  df %>% mutate(xiBB5a = case_when(
    iBB4c %in% c(2,3) ~ as.numeric(iBB5a), 
    iBB4c == 1 ~ 0))
  
   df <-  df %>% mutate(xiBB5b = case_when(
    iBB4c %in% c(2,3) ~ as.numeric(iBB5b), 
    iBB4c == 1 ~ 0))
  
   df <-  df %>% mutate(xiBB5e = case_when(
    iBB4c %in% c(2,3) ~ as.numeric(iBB5e), 
    iBB4c == 1 ~ 0))
  
   df <-  df %>% mutate(xiBB5f = case_when(
    iBB4c %in% c(2,3) ~ as.numeric(iBB5f), 
    iBB4c == 1 ~ 0))
  
   df <-  df %>% mutate(xiBB10 = case_when(
    iBB4c %in% c(2,3) ~ as.numeric(iBB10), 
    iBB4c == 1 ~ 0))
  
   df <-  df %>% mutate(xiBB11a = case_when(
    iBB4c %in% c(1,2,3) ~ as.numeric(iBB11a)))
  
   df <-  df %>% mutate(xiBB11b = case_when(
    iBB4c %in% c(1,2,3) ~ as.numeric(iBB11b)))
  
   df <-  df %>% mutate(xiBB14a = case_when(
    iBB4c %in% c(1,2,3) ~ as.numeric(iBB14a)))
   
    df <-  df %>% mutate(validedu = case_when(
     iBB2 %in% c(0,1,2,3,8) &
       xiBB5a %in% c(0,1,8) & 
       xiBB5b %in% c(0,1,8) & 
       xiBB5e %in% c(0,1,8) & 
       xiBB5f %in% c(0,1,8) &
       xiBB10 %in% c(0,1,2,3) & 
       xiBB11a %in% c(0,1) & 
       xiBB11b %in% c(0,1) & 
       xiBB14a %in% c(0,1,2,3,8) ~ 1, TRUE ~ 0))
   
    df <-  df %>% mutate(xcymhedu1 = case_when(validedu == 1 ~ 
                                               (case_when(xiBB5a == 1 ~ + 1, TRUE ~ 0) +
                                                  case_when(xiBB5b == 1 ~ + 1, TRUE ~ 0) +
                                                  case_when(xiBB5e == 1 ~ + 1, TRUE ~ 0) + 
                                                  case_when(xiBB5f == 1 ~ + 1, TRUE ~ 0) + 
                                                  case_when(xiBB10 %in% c(1,3) ~ + 1, TRUE ~ 0) +
                                                  case_when(xiBB14a %in% c(2,3) ~ + 1, TRUE ~ 0))))
   
    df <-  df %>% mutate(xcymhedu2 = case_when(validedu == 1 ~ 
                                               (case_when(xiBB11a | xiBB11b == 1 ~ + 1, TRUE ~ 0) + 
                                                  case_when(iBB2 != 0 & iBB2 != 1 & iBB2 != 2 & iBB2 != 3 ~ + 1, TRUE ~ 0)))) 
   
   
     df <-  df %>% mutate(Education = case_when(validedu == 1 ~ case_when(
      xcymhedu1 >= 1 ~ "Triggered due to risk of dropping out of school",  
      xcymhedu1 == 0 ~ case_when(
        xcymhedu2 == 2 ~ "Triggered due to current disrupted education", 
        xcymhedu2 < 2 ~ "Not Triggered"))))
   
##################################################################
##                  Hazardous Fire Involvement                  ##
##################################################################

 df <-  df %>% mutate(validfire = case_when(
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

 df <- df %>% mutate(xcymhfire = case_when(validfire == 1 ~ case_when(
  iC1d %in% c(3,4) ~ + 1, 
  iE1oo %in% c(2,3,4) ~ + 1,
  iE1mmm %in% c(2,3,4) ~ + 1, 
  iE1nnn %in% c(2,3,4) ~ + 1, 
  iE1ooo %in% c(2,3,4) ~ + 1, 
  iP15e %in% c(2,8) ~ + 1, 
  iY1am %in% c(1,2,3,4,5) | iY1an %in% c(1,2,3,4,5) | 
    iY1ao %in% c(1,2,3,4,5) ~ + 1)))
 
  df <-  df %>% mutate(`Hazardous Fire Involvement` = case_when(validfire == 1 ~ case_when(
   iX18 %in% c(2,3,4,5) ~ case_when(
     xcymhfire >= 2 ~ "Triggered at high risk", 
     xcymhfire == 1 ~ "Triggered at moderate risk", 
     xcymhfire == 0 ~ "Triggered at low risk"), 
   iX18 < 2 ~ "Not Triggered")))

  ##################################################################
  ##                           Gambling                           ##
  ##################################################################
  
   df <-  df %>% mutate(validgamb = case_when(
    iW8 %in% c(0,1) &
      cihiU3 == 1 ~ 1, TRUE ~ 0))
   
  df <-  df %>% mutate(Gambling = case_when(validgamb == 1 ~ case_when(
    iW8 == 1 ~ "Triggered", 
    iW8 < 1 ~ "Not Triggered")))
  
  ##################################################################
  ##                        Harm to Others                        ##
  ##################################################################
  
  df <- df %>% mutate(validharmoth = case_when(
    iE1oo %in% c(0,1,2,3,4) & 
      sCYRHO %in% c(0,1,2,3,4,5,6) ~ 1, TRUE ~ 0))
 
   df <- df %>% mutate('Harm to Others' = case_when(validharmoth == 1 ~ case_when(
    sCYRHO %in% c(5,6) ~ 2, 
    sCYRHO %in% c(3,4) ~ case_when(
      iE1oo %in% c(2,3,4) ~ 2, 
      iE1oo < 2 ~ 1),
    sCYRHO <= 2 ~ 0)))
   
   df$'Harm to Others'[df$'Harm to Others' == 2] <- "Triggered due to high risk of harm to others"
   df$'Harm to Others'[df$'Harm to Others' == 1] <- "Triggered due to moderate risk of harm to others"
   df$'Harm to Others'[df$'Harm to Others' == 0] <- "Not Triggered"

   
   ##################################################################
   ##                       Informal Support                       ##
   ##################################################################
   
   df <- df %>% mutate(xcymhinfsupp =  case_when(iP18a %in% c(2,3) ~ + 1, TRUE ~ 0) +
                                                     case_when(iP18b == 3 ~ + 1, TRUE ~ 0) +
                                                     case_when(iP18c == 3 ~ + 1, TRUE ~ 0) +
                                                     case_when(iP18d == 3 ~ + 1, TRUE ~ 0) +
                                                     case_when(iP18e == 3 ~ + 1, TRUE ~ 0))
   df <- df %>% mutate('Informal Support' = case_when(xcymhinfsupp >= 2 ~ 1, TRUE ~ 0) + 
                                                     case_when(xcymhinfsupp < 2 ~ 0, TRUE ~ 0))
   
   df$'Informal Support'[df$'Informal Support' == 1] <- "Triggered"
   df$'Informal Support'[df$'Informal Support' == 0] <- "Not Triggered"

   
   ##################################################################
   ##                    Interpersonal Conflict                    ##
   ##################################################################
   
   df <- df %>% mutate(xiBB5e = (case_when(
     iBB4c %in% c(0,1) ~ 0)+ 
       case_when(iBB4c %in% c(2,3) ~ as.numeric(iBB5e), TRUE ~ 0)))
   
   df <- df %>% mutate(validipcon = case_when(
     iP19a %in% c(0,1,8) &
       iP19b %in% c(0,1,8) &
       iP19c %in% c(0,1,8) &
       iP19d %in% c(0,1,8) &
       iP19e %in% c(0,1,8) &
       iP19f %in% c(0,1,8) &
       xiBB5e %in% c(0,1,2,3) & 
       iBB5e %in% c(0,1,8) ~ 1, TRUE ~ 0))
   
   df <- df %>% mutate(xcymhipcon = case_when(validipcon == 1 ~ 
                                                (case_when(iP19a | iP19d == 1 ~ + 1, TRUE ~ 0) +
                                                   case_when(iP19b | iP19e | iP19f == 1 ~ + 1, TRUE ~ 0) +
                                                   case_when(iP19c | xiBB5e == 1 ~ + 1, TRUE ~ 0))))
   
   df <- df %>% mutate('Interpersonal Conflict' = case_when(validipcon == 1 ~ 
                                                (case_when(xcymhipcon >= 2 ~ 2, TRUE ~ 0) +
                                                   case_when(xcymhipcon == 1 ~ 1, TRUE ~ 0) +
                                                   case_when(xcymhipcon == 0 ~ 0, TRUE ~ 0))))
   
   df$'Interpersonal Conflict'[df$'Interpersonal Conflict' == 1] <- "Triggered to reduce conflict within a specific domain"
   df$'Interpersonal Conflict'[df$'Interpersonal Conflict' == 0] <- "Not Triggered"
   df$'Interpersonal Conflict'[df$'Interpersonal Conflict' == 2] <- "Triggered to reduce widespread conflict"
   
   #################################################################
   ##                         Life Skills                         ##
   #################################################################
   
   df <- df %>% mutate(xiBB5e = (case_when(
     iBB4c %in% c(0,1) ~ 0)+ 
       case_when(iBB4c %in% c(2,3) ~ as.numeric(iBB5e), TRUE ~ 0)))
   
   df <- df %>% mutate(validipcon = case_when(
     iP19a %in% c(0,1,8) &
       iP19b %in% c(0,1,8) &
       iP19c %in% c(0,1,8) &
       iP19d %in% c(0,1,8) &
       iP19e %in% c(0,1,8) &
       iP19f %in% c(0,1,8) &
       xiBB5e %in% c(0,1,2,3) & 
       iBB5e %in% c(0,1,8) ~ 1, TRUE ~ 0))
   
   df <- df %>% mutate(xcymhipcon = case_when(validipcon == 1 ~ 
                                                (case_when(iP19a | iP19d == 1 ~ + 1, TRUE ~ 0) +
                                                   case_when(iP19b | iP19e | iP19f == 1 ~ + 1, TRUE ~ 0) +
                                                   case_when(iP19c | xiBB5e == 1 ~ + 1, TRUE ~ 0))))
   
   df <- df %>% mutate('Life Skills' = case_when(validipcon == 1 ~ 
                                                (case_when(xcymhipcon >= 2 ~ 2, TRUE ~ 0) +
                                                   case_when(xcymhipcon == 1 ~ 1, TRUE ~ 0) +
                                                   case_when(xcymhipcon == 0 ~ 0, TRUE ~ 0))))
   
   df$'Life Skills'[df$'Life Skills' == 0] <- "Not Triggered"
   df$'Life Skills'[df$'Life Skills' == 1] <- "Triggered for IADL assistance"
   df$'Life Skills'[df$'Life Skills' == 2] <- "Triggered for ADL assistance"
   
   
   ##################################################################
   ##                     Medication Adherence                     ##
   ##################################################################
   
   df <- df %>% mutate('Medication Adherence' = case_when(
     iM3 == 2 | iM4 == 1 | iM12 == 0 ~ 1, 
     iM3 != 2 & iM4 != 1 & iM12 != 0 ~ 0))
   
   df$'Medication Adherence'[df$'Medication Adherence' == 1] <- "Triggered"
   df$'Medication Adherence'[df$'Medication Adherence' == 0] <- "Not Triggered"

   
   #################################################################
   ##                      Medication Review                      ##
   #################################################################
   
   df <- df %>% mutate(xcymhmedrev = case_when(
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
   
   df <- df %>% mutate('Medication Review' = case_when(
     iM5 == 1 ~ 1, 
     iM5 != 1 ~ case_when(
       iM3 %in% c(0,1) & xcymhmedrev >= 1 ~ 1, 
       iM3 != 0 | iM3 != 1 | xcymhmedrev == 0 ~ 0)))
   
   df$'Medication Review'[df$'Medication Review' == 1] <- "Triggered"
   df$'Medication Review'[df$'Medication Review' == 0] <- "Not Triggered"

   
   ##################################################################
   ##                          Parenting2                          ##
   ##################################################################
   
   df <- df %>% mutate(xcymhparent = case_when(iP15a == 2 ~ + 1, TRUE ~ 0) +
                                                    case_when(iP15b == 2 ~ + 1, TRUE ~ 0) + 
                                                    case_when(iP15c == 2 ~ + 1, TRUE ~ 0) +
                                                    case_when(iP15d == 2 ~ + 1, TRUE ~ 0) +
                                                    case_when(iP15e == 2 ~ + 1, TRUE ~ 0) + 
                                                    case_when(iP15f == 2 ~ + 1, TRUE ~ 0))
   
   df <- df %>% mutate('Parenting2' = case_when(xcymhparent >= 2 ~ 1, TRUE ~ 0) +
                                                    case_when(xcymhparent < 2 ~ 0, TRUE ~ 0))
   df$'Parenting2'[df$'Parenting2' == 1] <- "Triggered"
   df$'Parenting2'[df$'Parenting2' == 0] <- "Not Triggered"
   
   
   #################################################################
   ##                   Readmission (cCYMHREAD)                   ##
   #################################################################
   
   df <- df %>% mutate(validread = case_when(
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
   
   df <- df %>% mutate(xcymhread1 = case_when(validread == 1 ~ 
                                                (case_when(iB12 == 1 ~ + 1, TRUE ~ 0) +
                                                   case_when(iB33 == 2 ~ + 1, TRUE ~ 0) +
                                                   case_when(iB38a == 1 ~ + 1, TRUE ~ 0) +
                                                   case_when(iB38b == 1 ~ + 1, TRUE ~ 0) + 
                                                   case_when(iP19h == 1 ~ + 1, TRUE ~ 0) +
                                                   case_when(sCYPSS >= 2 ~ + 1, TRUE ~ 0) +
                                                   case_when(sCYRHO >= 3 ~ + 1, TRUE ~ 0) +
                                                   case_when(sCYSOS >= 3 ~ + 1, TRUE ~ 0))))
   
   df <- df %>% mutate(xcymhread2 = case_when(validread == 1 ~ 
                                                (case_when(iE5 %in% c(2,3) ~  1, TRUE ~ 0) +
                                                   case_when(iV1 == 2 ~ 1, TRUE ~ 0) +
                                                   case_when(iV1 == 1 ~ case_when(xcymhread1 >= 1 ~ 1, TRUE ~ 0), TRUE ~ 0)))) 
   
   
   df <- df %>% mutate(Readmission = case_when(validread == 1 ~ 
                                               (case_when(xcymhread2 >= 1 ~ 1, TRUE ~ 0) +
                                                  case_when(xcymhread2 == 0 ~ 0, TRUE ~ 0))))
   
   df$Readmission[df$Readmission == 1] <- "Triggered"
   df$Readmission[df$Readmission == 0] <- "Not Triggered"
   
   
   ##################################################################
   ##        Suicidality and Purposeful Self Harm: cCYMHSSH        ##
   ##################################################################
   
   df <- df %>% mutate('Suicidality and Purposeful Self-Harm' = case_when(
     sCYSOS %in% c(5,6) ~ 2, 
     sCYSOS %in% c(3,4) ~ case_when(
       iE1oo >= 2 ~ 2, 
       iE1oo < 2 ~ 1),
     sCYSOS < 3 ~ 0))
   
   df$'Suicidality and Purposeful Self-Harm'[df$'Suicidality and Purposeful Self-Harm' == 0] <- "Not Triggered"
   df$'Suicidality and Purposeful Self-Harm'[df$'Suicidality and Purposeful Self-Harm' == 1] <- "Triggered due to moderate risk of harm to self"
   df$'Suicidality and Purposeful Self-Harm'[df$'Suicidality and Purposeful Self-Harm' == 2] <- "Triggered due to high risk of harm to self" 

   
   ##################################################################
   ##                  Sexual Behaviour: cCYMHSEX                  ##
   ##################################################################
   
   df <- df %>% mutate(validsex = case_when( 
     iE3o %in% c(0,1,2,3,4) & # inapprop. sexual behav.
       iE19a %in% c(0,1) & # inapprop. sexual knowledge
       iW27 %in% c(0,1) ~ 1, TRUE ~ 0)) # family concern about risky sexual beh
   
   #iJ10a and iX3 are from the Adolescent Supplement only. The CAP can be calculated without these.
   
   df <- df %>% mutate('Sexual Behaviour' = case_when(validsex == 1 ~ case_when(
     (iE3o %in% c(2,3,4) | iE19a == 1 | iJ10a == 1 | iX3 == 1 | iW27 == 1) ~ 1, TRUE ~ 0)))
 
   df$'Sexual Behaviour'[df$'Sexual Behaviour' == 1] <- "Triggered"
   df$'Sexual Behaviour'[df$'Sexual Behaviour' == 0] <- "Not Triggered"
   
   
   #################################################################
   ##                      Sleep Disturbance                      ##
   #################################################################
   
   df <- df %>% mutate(validsleep = case_when(
     iE1lll %in% c(0,1,2,3,4) &
       iE1yyy %in% c(0,1,2,3,4) & 
       iE1zzz %in% c(0,1,2,3,4) & 
       iE1aaaa %in% c(0,1,2,3,4) & 
       iE1bbbb %in% c(0,1,2,3,4) & 
       iJ4 %in% c(0,1,2,3,4) ~ 1, TRUE ~ 0))
   
   df <- df %>% mutate(xcymhsleep = case_when(validsleep == 1 ~ 
                                                (case_when(iE1lll %in% c(2,3,4) ~ + 1, TRUE ~ 0) +
                                                   case_when(iE1yyy %in% c(2,3,4) ~ + 1, TRUE ~ 0) +
                                                   case_when(iE1zzz %in% c(2,3,4) ~ + 1, TRUE ~ 0) + 
                                                   case_when(iE1aaaa %in% c(2,3,4) ~ + 1, TRUE ~ 0) +
                                                   case_when(iE1bbbb %in% c(2,3,4) ~ + 1, TRUE ~ 0) +
                                                   case_when(iJ4 %in% c(1,2,3,4) ~ + 1, TRUE ~ 0))))
  
   df <- df %>% mutate('Sleep Disturbance' = case_when(validsleep == 1 ~ case_when(
     xcymhsleep >= 2 ~ "Triggered", TRUE ~ "Not Triggered")))
   
   
   ##################################################################
   ##          Social and Peer Relationships: cCYMHSOCREL          ##
   ##################################################################
   
   df <- df %>% mutate(validsocrel = case_when(
     iE1qqq %in% c(0,1,2,4) & 
       iE1ttt %in% c(0,1,2,3,4) & 
       iE14d %in% c(0,1) & 
       iE15e %in% c(0,1,2,3,4,5) & 
       iF13 %in% c(0,1) & 
       iF15e %in% c(0,1) & 
       iF15f %in% c(0,1) & 
       iP8 %in% c(0,1) & 
       iY1aq %in% c(0,1,2,3,4,5) ~ 1, TRUE ~ 0))
   
   df <- df %>% mutate(xcymhsocrel1 = case_when(validsocrel == 1 ~ 
                                                  (case_when(iE1qqq %in% c(1,2,3,4) ~ + 1, TRUE ~ 0) +
                                                     case_when(iE1ttt %in% c(1,2,3,4) ~ + 1, TRUE ~ 0) +
                                                     case_when(iE14d == 1 ~ + 1, TRUE ~ 0) +
                                                     case_when(iF15e == 0 ~ + 1, TRUE ~ 0) +
                                                     case_when(iF15f == 0 ~ + 1, TRUE ~ 0) +
                                                     case_when(iP8 == 0 ~ + 1, TRUE ~ 0) + 
                                                     case_when(iY1aq %in% c(2,3,4,5) ~ + 1, TRUE ~ 0))))
   
   df <- df %>% mutate(xcymhsocrel2 = case_when(validsocrel == 1 ~ 
                                                  (case_when(iE15e %in% c(2,3,4,5) ~ + 1, TRUE ~ 0) +
                                                     case_when(iF13 == 1 ~ + 1, TRUE ~ 0)))) 
   
   df <- df %>% mutate('Social and Peer Relationships' = case_when(validsocrel == 1 ~ 
                                                 (case_when(xcymhsocrel1 >= 2 ~ 2, TRUE ~ 0) +
                                                    case_when(xcymhsocrel1 < 2 ~ 
                                                                (case_when(xcymhsocrel2 >= 1 ~ 1, TRUE ~ 0) +
                                                                   case_when(xcymhsocrel2 == 0 ~ 0, TRUE ~ 0)), TRUE ~ 0))))
 
   df$'Social and Peer Relationships'[df$'Social and Peer Relationships' == 0] <- "Not Triggered"
   df$'Social and Peer Relationships'[df$'Social and Peer Relationships' == 1] <- "Triggered to reduce maladaptive or antisocial peer interactions"
   df$'Social and Peer Relationships'[df$'Social and Peer Relationships' == 2] <- "Triggered to reduce social withdrawal or isolation"
   
   
   #################################################################
   ##                     Strengths: cCYMHSTR                     ##
   #################################################################
   
   df <- df %>% mutate(validstr = case_when(
     iC7 %in% c(0,1,2) & 
       iC9a %in% c(0,1,8) & 
       iF8b %in% c(0,1) & 
       iF15a %in% c(0,1) ~ 1, TRUE ~ 0))
   
   df <- df %>% mutate(xcymhstr = case_when(validstr == 1 ~
                                              (case_when(iC7 == 2 ~ + 1, TRUE ~ 0) +
                                                 case_when(iC9a %in% c(1,8) ~ + 1, TRUE ~ 0) +
                                                 case_when(iF8b == 0 ~ + 1, TRUE ~ 0) +
                                                 case_when(iF15a == 0 ~ + 1, TRUE ~ 0))))
   
   df <- df %>% mutate('Strengths' = case_when(validstr == 1 ~ 
                                              case_when(xcymhstr >= 3 ~ "Triggered", TRUE ~ "NOt Triggered")))
   
  
   
   ##################################################################
   ##                  Substance Use:cCYMHSUBUSE                   ##
   ##################################################################
   
   df <- df %>% mutate(validsubuse = case_when(
     iM6 %in% c(0,1) & 
       iW2 %in% c(0,1,2,3,4) &
       iW3a %in% c(0,1,2,3,4,5) &
       iW3b %in% c(0,1,2,3,4,5) &
       iW3c %in% c(0,1,2,3,4,5) &
       iW3d %in% c(0,1,2,3,4,5) &
       iW3e %in% c(0,1,2,3,4,5) &
       iW3f %in% c(0,1,2,3,4,5) ~ 1, TRUE ~ 0))
   
   df <- df %>% mutate(xcymhsubuse = case_when(validsubuse == 1 ~ 
                                                 (case_when(iM6 == 1 ~ + 1, TRUE ~ 0) +
                                                    case_when(iW2 %in% c(1,2,3,4) ~ + 1, TRUE ~ 0) +
                                                    case_when(iW3a %in% c(1,2,3,4,5) ~ + 1, TRUE ~ 0) +
                                                    case_when(iW3b %in% c(1,2,3,4,5) ~ + 1, TRUE ~ 0) +
                                                    case_when(iW3c %in% c(1,2,3,4,5) ~ + 1, TRUE ~ 0) + 
                                                    case_when(iW3d %in% c(1,2,3,4,5) ~ + 1, TRUE ~ 0) + 
                                                    case_when(iW3e %in% c(1,2,3,4,5) ~ + 1, TRUE ~ 0) + 
                                                    case_when(iW3f %in% c(1,2,3,4,5) ~ + 1, TRUE ~ 0))))
   
   df <- df %>% mutate('Substance Use' = case_when(validsubuse == 1 ~ case_when(
     xcymhsubuse >= 1 ~ "Triggered", TRUE ~ "Not Triggered")))
   
   
   #################################################################
   ##          Support Systems for Discharge: cCYMHSSDIS          ##
   #################################################################
   
   df <- df %>% mutate(validssdis = case_when(
     iA11b %in% c(1:15) & 
       iA12e %in% c(1:6) & 
       iR16d %in% c(0,1,2,8) & 
       iR16e %in% c(0,1,2,8) & 
       iY8a %in% c(0,1,8) ~ 1, TRUE ~ 0))
   # iA11b replaced iA11j
   
   df <- df %>% mutate('Support Systems for Discharge' = case_when(validssdis == 1 ~ case_when(
     iA11b %in% c(5,8) | iA12e %in% c(1,5) | iY8a == 1 | iR16d %in% c(0,2) | 
       iR16e %in% c(0,2) ~ "Triggered", TRUE ~ "Not Triggered")))
   
   
   ##################################################################
   ##             Tobacco and Nicotine Use: cCYMHTOBAC             ##
   ##################################################################
   df <- df %>% mutate(validtobac = case_when(
     iW20a %in% c(0,1,2,3) & 
       iW20b %in% c(0,1,2,3) ~ 1, TRUE ~ 0))
   
   df <- df %>% mutate('Tobacco and Nicotine Use' = case_when(validtobac == 1 ~ case_when(
     iW20a == 3 | iW20b == 3 ~ "Triggered to prevent long-term tobacco use", 
     iW20a %in% c(1,2) | iW20b %in% c(1,2) ~ "Triggered to reduce or cease daily tobacco use", 
     iW20a == 0 & iW20b == 0 ~ "Not Triggered")))
   
   
   #################################################################
   ##                   Transitions: cCYMHTRANS                   ##
   #################################################################
   
   df <- df %>% mutate(validtrans = case_when(
     iZ12 %in% c(0,1) ~ 1, TRUE ~ 0))
   df <- df %>% mutate('Transitions' = case_when(validtrans == 1 ~ case_when(
     iZ12 == 1 ~ "Triggered", TRUE ~ "Not Triggered")))

   
   ##################################################################
   ##              Traumatic Life Events: cCYMHTRAUMA              ##
   ##################################################################
   
   df <- df %>% mutate(validtrauma = case_when(
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
   
   df <- df %>% mutate(xcymhtrauma1 = case_when(validtrauma == 1 ~ 
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
   
   df <- df %>% mutate(xcymhtrauma2 = case_when(validtrauma == 1 ~ 
                                                  (case_when(iY2a == 1 ~ + 1, TRUE ~ 0) +
                                                     case_when(iY2c == 1 ~ + 1, TRUE ~ 0) +
                                                     case_when(iY2d == 1 ~ + 1, TRUE ~ 0))))
   
   df <- df %>% mutate(xcymhtrauma3 = case_when(validtrauma == 1 ~ 
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
   
   df <- df %>% mutate(xcymhtrauma4 = case_when(validtrauma == 1 ~ 
                                                  (case_when(iE1ddd %in% c(1,2,3,4) ~ + 1, TRUE ~ 0) +
                                                     case_when(iE1fff %in% c(1,2,3,4) ~ + 1, TRUE ~ 0) +
                                                     case_when(iE1iii %in% c(1,2,3,4) ~ + 1, TRUE ~ 0) +
                                                     case_when(iE1jjj %in% c(1,2,3,4) ~ + 1, TRUE ~ 0) +
                                                     case_when(iE1kkk %in% c(1,2,3,4) ~ + 1, TRUE ~ 0) +
                                                     case_when(iE1lll %in% c(1,2,3,4) ~ + 1, TRUE ~ 0) +
                                                     case_when(iE1eeee %in% c(1,2,3,4) ~ + 1, TRUE ~ 0)))) 
   
   df <- df %>% mutate('Traumatic Life Events' = case_when(validtrauma == 1 ~ case_when(
     xcymhtrauma1 >= 1 | xcymhtrauma2 >= 1 ~"Triggered to address immediate safety concerns", 
     xcymhtrauma1 == 0 & xcymhtrauma2 == 0 ~ case_when(
       xcymhtrauma3 >= 1 ~ case_when(
         iY1b == 1 | xcymhtrauma4 >=2 ~ "Triggered to reduce the impact of prior traumatic life events", 
         iY1b < 1 & xcymhtrauma4 < 2 ~ "Not Triggered"),
       xcymhtrauma3 == 0 ~ "Not Triggered"))))

   
   #################################################################
   ##                    Video Gaming: cCYMHVG                    ##
   #################################################################
   
   df <- df %>% mutate(validvg = case_when(
     iW21 %in% c(0,1,2,3) ~ 1, TRUE ~ 0))
   df <- df %>% mutate('Video Gaming' = case_when(validvg == 1 ~ case_when(
     iW21 %in% c(2,3) ~ "Triggered", 
     iW21 < 2 ~ "Not Triggered")))
   
   #################################################################
   ##                  Eating/Weight: cCYMHWTMGT                  ##
   #################################################################
   
   df <- df %>% mutate(validwtmgt = case_when(
     iK2i %in% c(0,1) & 
       iK2j %in% c(0,1) & 
       iK2k %in% c(0,1) & 
       iK8a %in% c(0,1) & 
       iK8b %in% c(0,1) & 
       iK8c %in% c(0,1) ~ 1, TRUE ~ 0))  
   df <- df %>% mutate('Eating/Weight' = case_when(validwtmgt == 1 ~ case_when(
     iK2k == 1 | iK2k == 1 | iK2i == 1 ~ "Triggered for weight concerns", 
     iK2j < 1 & iK2k < 1 & iK2i < 1 ~ case_when(
       iK8a == 1 | iK8b ==1 | iK8c == 1 ~ "Triggered due to problematic eating behaviour", 
       iK8a < 1 & iK8b < 1 & iK8c < 1 ~ "Not Triggered"))))
   
  ##--------------
##  EXT__scale  
##--------------

df <- df %>% mutate(validexts = case_when(
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

df <- df %>% mutate(EXT__scale = case_when(validexts == 1 ~ 
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

##--------------
##  INT__scale  
##--------------

df <- df %>% mutate(validints = case_when(
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

df <- df %>% mutate(INT__scale = case_when(validints == 1 ~ 
                                                         iE1eee + iE1eeee + iE1fff + iE1kkk +
                                                         iE1rrr + iE1sss + iE1ttt +  iE1uu + iE1vv +
                                                         iE1ww + iE1xx + iE1yy))

##--------------
##  ADL__scale  
##--------------

df <- df %>% mutate(validadl = case_when(
  iG2a %in% c(0,1,2,3,4,5,6,8) & 
    iG2b %in% c(0,1,2,3,4,5,6,8) & 
    iG2c %in% c(0,1,2,3,4,5,6,8) &
    iG2d %in% c(0,1,2,3,4,5,6,8) ~ 1, TRUE ~ 0))

df <- df %>% mutate(ADL__scale = case_when(validadl == 1 ~ 
                                         (case_when(iG2a == 0 ~ 0, iG2a %in% c(1,2,3,4,5) ~ 1, iG2a %in% c(6,8) ~ 2))+ 
                                         (case_when(iG2b == 0 ~ 0, iG2b %in% c(1,2,3,4,5) ~ 1, iG2b %in% c(6,8) ~ 2))+
                                         (case_when(iG2c == 0 ~ 0, iG2c %in% c(1,2,3,4,5) ~ 1, iG2c %in% c(6,8) ~ 2))+
                                         (case_when(iG2d == 0 ~ 0, iG2d %in% c(1,2,3,4,5) ~ 1, iG2d %in% c(6,8) ~ 2))))

##--------------
##  PEER__scale  
##--------------

df <- df %>% mutate(valideer = case_when(
  iP19b %in% c(0,1,8) &
    iP19e %in% c(0,1,8) &
    iP19f %in% c(0,1,8) ~ 1, TRUE ~ 0))

df <- df %>% mutate(PEER__scale = case_when(valideer == 1 ~ 
                                                         (case_when(iP19b == 1 ~ + 1, TRUE ~ 0) +
                                                            case_when(iP19f == 1 ~ + 1, TRUE ~ 0) +
                                                            case_when(iP19e == 1 ~ + 1, TRUE ~ 0))))

##--------------
##  FAM__scale  
##--------------

df <- df %>% mutate(validily = case_when(
  iF8a %in% c(0,1) &
    iP19d %in% c(0,1,8) &
    iP19h %in% c(0,1,8) &
    iY7a %in% c(0,1) &
    iY7b %in% c(0,1) &
    iY8a %in% c(0,1,8) ~ 1, TRUE ~ 0))

df <- df %>% mutate(FAM__scale = case_when(validily == 1 ~ 
                                                         case_when(iP19d %in% c(0,1) & iP19h %in% c(0,1) & iY8a %in% c(0,1) ~
                                                                     case_when(iF8a == 0 ~ (1 + iP19d + iP19h + iY7a + iY7b + iY8a), 
                                                                               iF8a == 1 ~ (iP19d + iP19h + iY7a + iY7b + iY8a)))))


##--------------------
##  STRENGTHS__scale  (Relational Strengths: sCYRelStren)
##--------------------

df <- df %>% mutate(validrelstren = case_when(
  iP8 %in% c(0,1) & 
    iF15c %in% c(0,1) &
    iF8a %in% c(0,1) &
    iF15d %in% c(0,1) &
    iF15e %in% c(0,1) &
    iF15f %in% c(0,1) ~ 1, TRUE ~ 0))

df <- df %>% mutate(STRENGTHS__scale = case_when(validrelstren == 1 ~ 
                                                               (case_when(iP8 == 0 ~ 1, iP8 == 1 ~ 0)) +
                                                               (case_when(iF15c == 0 ~ 1, iF15c == 1 ~ 0)) +
                                                               (case_when(iF8a == 0 ~ 1, iF8a == 1 ~ 0)) +
                                                               (case_when(iF15d == 0 ~ 1, iF15d == 1 ~ 0)) +
                                                               (case_when(iF15e == 0 ~ 1, iF15e == 1 ~ 0)) +
                                                               (case_when(iF15f == 0 ~ 1, iF15f == 1 ~ 0))))

##----------------------
##  SCHLDISRUPT__scale  
##----------------------

df <- df %>% mutate(validsdes = case_when(
  iBB5a %in% c(0,1,8) & 
    iBB5b %in% c(0,1,8) & 
    iBB5e %in% c(0,1,8) & 
    iBB5f %in% c(0,1,8) & 
    iBB11a %in% c(0,1) & 
    iBB11b %in% c(0,1) & 
    iBB10 %in% c(0,1,2,3) & 
    iBB14a %in% c(0,1,2,3,8) ~ 1, TRUE ~ 0))

df <- df %>% mutate(SCHLDISRUPT__scale = case_when(validsdes == 1 ~ 
                                                                 (case_when(iBB5a == 1 ~ + 1, TRUE ~ 0) +
                                                                    case_when(iBB5b == 1 ~ + 1, TRUE ~ 0) +
                                                                    case_when(iBB5e == 1 ~ + 1, TRUE ~ 0) +
                                                                    case_when(iBB5f == 1 ~ + 1, TRUE ~ 0) +
                                                                    case_when(iBB10 %in% c(1,2,3) ~ + 1, TRUE ~ 0) +
                                                                    case_when(iBB11a == 1 ~ + 1, TRUE ~ 0) + 
                                                                    case_when(iBB11b == 1 ~ + 1, TRUE ~ 0) + 
                                                                    case_when(iBB14a %in% c(2,3) ~ + 1, TRUE ~ 0))))


##------------------
##  SENSORY__scale  
##------------------

df <- df %>% mutate(validSensDiff = case_when( 
  iD28 %in% c(0,1,2,3,4) &
    iD29 %in% c(0,1,2,3,4) &
    iE3aa %in% c(0,1,2,3,4) &
    oE3ab %in% c(0,1,2,3,4) & 
    oE3ac %in% c(0,1,2,3,4) ~ 1, TRUE ~ 0))

df <- df %>% mutate(SENSORY__scale = case_when(validSensDiff == 1 ~ 
                                                             iD28 + iD29 + iE3aa + oE3ab + oE3ac))

##----------------
##  INT-S__scale  
##----------------

df <- df %>% mutate('INT-S__scale' = iE1eee + iE1eeee + iE1fff + iE1kkk +
                                       iE1rrr + iE1sss + iE1ttt + iE1vv +
                                       iE1ww + iE1xx + iE1yy)

##----------------
##  EXT-S__scale  
##----------------

df <- df %>% mutate(validexts = case_when(
  iE15a %in% c(0,1,2,3,4,5) &
    iE15e %in% c(0,1,2,3,4,5) &
    iE1oo %in% c(0,1,2,3,4) &
    iE3l %in% c(0,1,2,3,4) &
    iX2a %in% c(0,1,2,3,4,5) & 
    iX2b %in% c(0,1,2,3,4,5) & 
    iX2c %in% c(0,1,2,3,4,5) ~ 1, TRUE ~ 0))

df <- df %>% mutate('EXT-S__scale' = case_when(validexts == 1 ~ 
                                                             (case_when(iE15a == 0 ~ 0, iE15a %in% c(1,2,3,4,5) ~ 1)) + 
                                                             (case_when(iE15e == 0 ~ 0, iE15e %in% c(1,2,3,4,5) ~ 1)) + 
                                                             (case_when(iE1oo == 0 ~ 0, iE1oo %in% c(1,2,3,4) ~ 1)) + 
                                                             (case_when(iE3l == 0 ~ 0, iE3m %in% c(1,2,3,4) ~ 1)) +
                                                             (case_when(iX2a == 0 ~ 0, iX2a %in% c(1,2,3,4,5) ~ 1)) +
                                                             (case_when(iX2b == 0 ~ 0, iX2b %in% c(1,2,3,4,5) ~ 1)) +
                                                             (case_when(iX2c == 0 ~ 0, iX2c %in% c(1,2,3,4,5) ~ 1))))

##----------------
##  AUT__scale  
##----------------

df <- df %>% mutate(validssc = case_when(iC7 %in% c(0,1,2) &
                                                       iE3aa %in% c(0,1,2,3,4) &
                                                       iE14a %in% c(0,1) &  
                                                       iE14b %in% c(0,1) &                    
                                                       iE14c %in% c(0,1) ~ 1, TRUE ~ 0))

df <- df %>% mutate(AUT__scale = case_when(validssc == 1 ~ 
                                                    (case_when(iE14a == 1 ~ 1, TRUE ~ 0)) + 
                                                    (case_when(iE14b == 1 ~ 1, TRUE ~ 0)) + 
                                                    (case_when(iE14c == 1 ~ 1, TRUE ~ 0)) + 
                                                    (case_when(iE3aa %in% c(1,2,3,4) ~ 1, TRUE ~ 0)) + 
                                                    (case_when(iC7== 2 ~ 1, TRUE ~ 0))))

##----------------
## COG__scale  
##----------------

df <- df %>% mutate(validcognitive = case_when(
  iC1d %in% c(0,1,2,3,4,5) &
    iC2a %in% c(0,10) &
    iC2b %in% c(0,1) & 
    iD1 %in% c(0,1,2,3,4) & 
    iD2 %in% c(0,1,2,3,4) ~ 1, TRUE ~ 0))

df <- df %>% mutate(COG__scale = case_when(validcognitive == 1 ~ 
                                                                (case_when(iC1d == 0 ~ 0, iC1d %in% c(1,2,3,4,5) ~ 1)) +
                                                                (case_when(iC2a == 0 ~ 0, iC2a == 1 ~ 1)) +
                                                                (case_when(iC2b == 0 ~ 0, iC2b == 1 ~ 1)) +
                                                                (case_when(iD1 == 0 ~ 0, iD1 %in% c(1,2,3,4) ~ 1)) +
                                                                (case_when(iD2 == 0 ~ 0, iD2 %in% c(1,2,3,4) ~ 1))))

##----------------
##  MANIA__scale  
##----------------

df <- df %>% mutate(validmania_old = case_when(
  iE1bbb %in% c(0,1,2,3,4) &
    iE1zz %in% c(0,1,2,3,4) &
    iE1llll %in% c(0,1,2,3,4) &
    iE1ccc %in% c(0,1,2,3,4) &
    iE1aaa %in% c(0,1,2,3,4) &
    iE6 %in% c(0,1,2,3,4,5) ~ 1, TRUE ~ 0))

df <-df %>% mutate(MANIA__scale = case_when(validmania_old == 1 ~ case_when
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

df <- df %>% mutate(valideep = case_when(
  iE1yyy %in% c(0,1,2,3,4) &
    iE1zzz %in% c(0,1,2,3,4)&
    iE1aaaa %in% c(0,1,2,3,4)&
    iE1bbbb %in% c(0,1,2,3,4) ~ 1, TRUE ~ 0))

df <- df %>% mutate(SLEEP__scale = case_when(valideep == 1 ~ (iE1yyy +iE1zzz +iE1aaaa +
                                                                          iE1bbbb)))

##-------------------------
##  Psychiatric Diagnosis  
##-------------------------

df <- df %>% 
  mutate(`Psychiatric Diagnoses` = case_when(
    is.na(iCC10b) & is.na(iCC1d) ~ NA,
    !is.na(iCC10b) & rowSums(across(iCC10b:iCC10u, ~ . %in% c(2, 3, 4))) >= 1 ~ 1,
    !is.na(iCC1d) & rowSums(across(iCC1d:iCC1u, ~ . %in% c(1, 2, 3, 4))) >= 1 ~ 1,
    TRUE ~ 0
  ))


##---------------------
##  Medical Diagnosis    #### DOUBLE CCHECK THIS
##---------------------

df <- df %>%
  mutate(`Medical Diagnoses` = case_when(
    Tool != "rscr" & !is.na(iI1aa) & rowSums(across(iI1aa:iI1z)) >= 1 ~ 1,
    !is.na(ComorbidMedical_Screen) ~ ComorbidMedical_Screen,
    TRUE ~ 0
  ))

##----------
##  ID/GGD  
##----------

df <- df %>%
  mutate('ID/GDD' = if_else(
    Tool != "rscr" & (
      iCC10b %in% c(1, 2, 3, 4) |
        iCC10r %in% c(1, 2, 3, 4) |
        iCC1r %in% c(1, 2, 3, 4) |
        iCC9 %in% c(5, 6)
    ), 1, 0),
    if_else(
      is.na(ID_Screen),
      if_else(
        ComorbidMedical_Screen == 1, 1,
        if_else(ComorbidMedical_Screen == 0, 0, ComorbidMedical_Screen)
      ),
      if_else(ID_Screen == 1, 1,
              if_else(ID_Screen == 0, 0, ID_Screen)
      )))

##----------------------
##  Residential Issues  
##----------------------

df$'Residential Issues' <- ifelse(
  (df$iB12 == 1 | df$iB38c == 1 | df$iB38a == 1 | df$iB38b == 1 | df$iE5 %in% c(1,2,3) | (df$iA11b != 1 & !is.na(df$iA11b) & df$iA11b != 99)),
  1,
  ifelse(
    (!is.na(df$iB12) & df$iB12 != 99) | (!is.na(df$iB38c) & df$iB38c != 99) | (!is.na(df$iB38a) & df$iB38a != 99) | (!is.na(df$iB38b) & df$iB38b != 99) | (!is.na(df$iE5) & df$iE5 != 99) | (!is.na(df$iA11b) & df$iA11b != 99),
    0,
    NA)
)

##---------
##  xsdsi  
##---------

df <- df %>% mutate(xsdsi =
                                  iE1ss + iE1tt + iE1vv + iE1ww + iE1xx + iE1yy + 
                                  iE1aaa + iE1rrr + iE1ttt)
##---------
##  xsSOS  
##---------

df <- df %>% mutate(xsSOS =  case_when(
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

df <- df %>% mutate(validrichy = case_when(
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

df <- df %>% mutate(validSSH = case_when(
  iE1oo %in% c(0,1,2,3,4) & 
    xsSOS %in% c(0,1,2,3,4,5,6) ~ 1, TRUE ~ 0))

##-----------
##  xparent  ## added ip15e and iP15f (as per SAS)
##-----------

df <- df %>% mutate(xparent = case_when(
  iP15a == 8 | iP15b == 8 | iP15c == 8 | iP15d == 8 | iP15e == 8 | iP15f ==8 ~ 1, TRUE ~ 0)) ## CHECK THIS!!!!!!!!!!!!!!!!!!!

##----------
##  xabuse  
##----------

df <- df %>% mutate(xabuse = case_when(
  iY1al %in% c(1,2,3,4,5) ~ + 1, iY1am %in% c(1,2,3,4,5) ~ + 1, 
  iY1an %in% c(1,2,3,4,5) ~ + 1, iY1aq %in% c(1,2,3,4,5) ~ + 1, 
  iY1ao %in% c(1,2,3,4,5) ~ + 1, iY1au %in% c(1,2,3,4,5) ~ + 1))

##-----------
##  xmedsym  
##-----------

df <- df %>% mutate(xmedsym = case_when(
  iJ2k %in% c(1,2,3,4) ~ + 1, iJ2y %in% c(1,2,3,4) ~ + 1, 
  iJ2dd %in% c(1,2,3,4) ~ + 1, iJ2gg %in% c(1,2,3,4) ~ + 1, 
  iJ2ll %in% c(1,2,3,4) ~ + 1, iJ3 %in% c(1,2,3) ~ + 1, iJ9a == 1 ~ + 1,
  iJ9b == 1 ~ + 1, iJ9c == 1 ~ + 1, iJ9d == 1 ~ + 1, iJ9e == 1 ~ + 1, 
  iJ9f == 1 ~ + 1, iJ9g == 1 ~ + 1))

##-----------
##  xsANXIETY  
##-----------

df <- df %>% mutate(xsANXIETY =  
                      iE1eee + iE1fff + iE1ggg + iE1hhh + iE1iii + iE1kkk + iE1lll + iE1eeee)

##------------
##  xsFAMILY  
##------------

df <- df %>% mutate(xsFAMILY = case_when(
  iP18d != 8 & iP19h !=8 & iY8a != 8 ~ case_when(
    iF8a == 0 ~ 1 + iP19d + iP19h + iY7a + iY7b + iY8a,
    iF8a == 1 ~ iP19d + iP19h + iY7a + iY7b + iY8a)))


##---------
##  ssSOSval  
##---------

df$ssSOSval <- ifelse(
  df$iX1c %in% c(0, 1, 2, 3, 4, 5, 8) &
    df$iX1e %in% c(0, 1, 2, 3, 4, 5, 8) &
    df$xsdsi %in% 0:36 &
    df$iX1d %in% c(0, 1, 2, 3, 4, 5, 8) &
    df$iE1nnn %in% c(0, 1, 2, 3, 4, 5, 8) &
    df$iX1b %in% c(0, 1, 2, 3, 4, 5, 8),
  1, 0
)


##---------
##  xcSSH  
##---------

df <- df %>% mutate(xcSSH = case_when(
  xsSOS %in% c(5,6) ~ 2, xsSOS %in% c(3,4) ~ case_when(
    iE1oo >= 2 ~ 2, iE1oo < 2 ~ 1), xsSOS < 3 ~ 0))


##---------
##  sRHOval  
##---------

df$sRHOval <- ifelse(
  df$iX2a %in% c(0,1,2,3,4,5) &
    df$iX2c %in% c(0,1,2,3,4,5) &
    df$iX2b %in% c(0,1,2,3,4,5) &
    df$iX16c %in% c(0,1,2,3,4,5) &
    df$iE1mmm %in% c(0,1,2,3,4,5) &
    df$iE1nnn %in% c(0,1,2,3,4,5) &
    df$iE1ooo %in% c(0,1,2,3,4,5) &
    df$iE1ppp %in% c(0,1,2,3,4,5),
  1, 0)

#################################################
# xsRHO 
#################################################

df <- df %>% mutate(xsRHO = case_when(
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

df <- df %>% mutate(xcHARMOTH = case_when(
  xsRHO %in% c(5,6) ~ 2, xsRHO %in% c(3,4) ~ case_when(
    iE1oo %in% c(2,3,4) ~ 2, iE1oo < 2 ~ 1), xsRHO < 2 ~ 0))

#########################################################
# RICHY__scale Calculation * NEW
#################################################

df <- df %>% mutate(RIChY_scale = case_when(
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


# test <- df %>% select(EMHID, validrichy, Age, iX2b, iE3q, iP19b, iP19e, xsANXIETY, iP19b, iP19e, iE15e, iP19b, PARENTING__scale, xparent, iE1yyy, xsFAMILY, iY8a, xabuse, xmedsym, xcSSH, xcHARMOTH, RIChY_scale, RIChY_scale2)

#########################################################
# ALEIC__scale
#################################################

df <- df %>% 
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

#########################################################
# TSS__scale
#################################################

df <- df %>% 
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


############################################################################
############################################################################
###                                                                      ###
###                  SELECT COLUMNS FOR INTERRAI BI CSV                  ###
###                                                                      ###
############################################################################
############################################################################


interRAI <- df %>%
 select("iA3", "client_id", "iA2", "ref", "compl", "program_name",  "oV5", "oGoal1", "oGoal2", "oGoal3", "oGoal4", "iA5a", "record_id", "client_id", "worker_id", "program_id", "cihiA2a", "cihiA2c", "iA2", "Tool", "Type", "EMHID", "Age", "FirstChYMH", "FirstAx", "FirstScreener", "LastAx", "LastChYMH", "LastScreener", "DSI__scale", "ANX__scale", "CHAMHPS__scale", "DABS__scale", "SOCDIS__scale", "DISTRACT__scale", "KinarkRiskHTO_comb", "KinarkRiskHTS_comb", "KinarkRiskHTP_comb", "KinarkRiskSM_comb", "KinarkRiskVCP_comb", "KinarkRiskFB_comb", "KinarkRiskOC_comb", "Acute", "Complex", "KinarkRiskHTO_cat", "KinarkRiskHTS_cat", "KinarkRiskHTP_cat", "KinarkRiskSM_cat", "KinarkRiskVCP_cat", "KinarkRiskFB_cat", "KinarkRiskOC_cat", "Acute_cat", "Complex_cat", "ED_Screen", "ID_Screen", "ComorbidMedical_Screen", "PSS__scale", "Trauma", "RISSK__scale", "RIO__scale", "SOS__scale", "RHO__scale", "IADLC__scale", "CAREDIS__scale", "PARENTING__scale", "COMM__scale", "CAS", "Psychiatrist", "FamilyEngaged", "Physical Activity", "Attachment", "Caffeine Use", "Caregiver Distress", "Communication", "Control Interventions", "Criminality Prevention", "Education", "Hazardous Fire Involvement", "Gambling", "Harm to Others", "Informal Support", "Interpersonal Conflict", "Life Skills", "Medication Adherence", "Medication Review", "Parenting2", "Readmission", "Suicidality and Purposeful Self-Harm", "Sexual Behaviour", "Sleep Disturbance", "Social and Peer Relationships", "Strengths", "Substance Use", "Support Systems for Discharge", "Tobacco and Nicotine Use", "Transitions", "Traumatic Life Events", "Video Gaming", "Eating/Weight", "ABS__scale", "EXT__scale", "INT__scale", "ADL__scale", "PEER__scale", "FAM__scale", "STRENGTHS__scale", "SCHLDISRUPT__scale", "SENSORY__scale", "INT-S__scale", "EXT-S__scale", "AUT__scale", "COG__scale", "MANIA__scale", "SLEEP__scale", "Psychiatric Diagnoses", "Medical Diagnoses", "ID/GDD", "Residential Issues", "RIChY_scale", "ALEIC__scale", "TSS__scale")

rm(ChYMH_export, df) # Remove unnecessary dfs


