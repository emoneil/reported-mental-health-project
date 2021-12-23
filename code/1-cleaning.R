# load libraries
library(tidyverse)
library(haven)
library(cdlTools)
library(stringr)

# load raw data
brfss_2019_raw = read_xpt("../data/raw/brfss_2019_raw.xpt")
nmhss_2019_raw = read_csv("../data/raw/nmhss_2019_raw.csv")

# view number of observations for each level of intended response variable
table(brfss_2019_raw$MENTHLTH)

# clean BRFSS data
brfss_2019_raw_ca = brfss_2019_raw %>% filter(`_STATE` == 6)
brfss_2019_raw_nj = brfss_2019_raw %>% filter(`_STATE` == 34)
cols_missing <- function(df){ # helper function
  as.vector(which(colSums(is.na(df)) != 0))
}

cols_missing_ca = brfss_2019_raw_ca %>% cols_missing(.)
cols_missing_ca_names = names(brfss_2019_raw_ca[,cols_missing_ca])

# ensure as many states are in the original dataset are represented
# select california data
brfss_ca = brfss_2019_raw %>% 
  dplyr::select(`_STATE`, `_URBSTAT`, MENTHLTH, HLTHPLN1, MEDCOST, CHECKUP1, 
                BPHIGH4,  TOLDHI2, ADDEPEV3, DIABETE4, 
                MARITAL, EDUCA, RENTHOM1, VETERAN3, EMPLOY1, CHILDREN, INCOME2,
                WEIGHT2, HEIGHT3, DEAF, BLIND, SMOKE100, ALCDAY5, 
                EXERANY2, STRENGTH, FRUIT2, FVGREEN1, FRENCHF1, POTATOE1, VEGETAB2,
                FLUSHOT7, TETANUS1, PNEUVAC4, HIVTST7,
                # following variables (adverse childhood experiences) answered by far fewer individuals
                #ACEDEPRS, ACEDRINK, ACEDRUGS, ACEPRISN, ACEDIVRC, ACEPUNCH, ACEHURT1,
                #ACESWEAR, ACETOUCH, ACETTHEM, ACEHVSEX,
                #FOODSTMP, 
                `_SEX`, #SEXOR, TRNSGNDR,
                `_IMPRACE`,
                `_MENT14D`, # computed mental health status
                `_HCVU651`, `_RFHYPE5`, `_CHOLCH2`, `_RFCHOL2`,  
                `_RACE`, `_RACEGR3`,
                `_AGEG5YR`, `_AGE_G`, `_BMI5`, `_RFBMI5`,  `_BMI5CAT`,
                `_CHLDCNT`, `_EDUCAG`, `_INCOMG`, `_INCOMG`, 
                `_SMOKER3`, `_RFSMOK3`,
                `DRNKANY5`, `DROCDY3_`, `_RFBING5`, `_DRNKWK1`, `_RFDRHV7`, 
                `_TOTINDA`, `_PACAT2`, `_PAINDX2`, `_PASTRNG`, `_PAREC2`,
                `FRUTDA2_`,  `GRENDA1_`, `FRNCHDA_`, `POTADA1_`, `VEGEDA2_`,
                `_FRUTSU1`, `_VEGESU1`, 
                `_FRTLT1A`, `_VEGLT1A`,
                NUMMEN, NUMWOMEN, 
                PHYSHLTH, PERSDOC2, 
                CVDINFR4, CVDSTRK3, ASTHMA3) %>%
  filter(`_STATE` == 6)

# remove rows with 2 or more NA values
brfss_ca = brfss_ca[rowSums(is.na(brfss_ca)) < 2,]
cols_missing_ca_updated = brfss_ca %>% cols_missing(.)
cols_missing_ca_updated_names = names(brfss_ca[,cols_missing_ca_updated]) # "TETANUS1"

# select columns including features and response (some later commented out due to lack of data across states)
brfss_data = brfss_2019_raw %>% tibble() %>%
  #mutate(SEXOR =ifelse(!is.na(SOMALE), SOMALE, ifelse(!is.na(SOFEMALE), SOFEMALE, NA))) %>%
  dplyr::select(`_STATE`, `_URBSTAT`, MENTHLTH, HLTHPLN1, MEDCOST, CHECKUP1, 
                BPHIGH4,  TOLDHI2, ADDEPEV3, DIABETE4, 
                MARITAL, EDUCA, RENTHOM1, VETERAN3, EMPLOY1, CHILDREN, INCOME2,
                WEIGHT2, HEIGHT3, DEAF, BLIND, SMOKE100, `DROCDY3_`, 
                EXERANY2, STRENGTH, FRUIT2, FVGREEN1, FRENCHF1, POTATOE1, VEGETAB2,
                FLUSHOT7, #TETANUS1, 
                PNEUVAC4, HIVTST7,
                # following variables (adverse childhood experiences) answered by far fewer individuals
                #ACEDEPRS, ACEDRINK, ACEDRUGS, ACEPRISN, ACEDIVRC, ACEPUNCH, ACEHURT1,
                #ACESWEAR, ACETOUCH, ACETTHEM, ACEHVSEX,
                #FOODSTMP, 
                `_SEX`, #SEXOR, TRNSGNDR,
                `_IMPRACE`,
                `_MENT14D`, # computed mental health status
                `_HCVU651`, `_RFHYPE5`, `_CHOLCH2`, `_RFCHOL2`,  
                `_RACE`, `_RACEGR3`,
                `_AGEG5YR`, `_AGE_G`, `_BMI5`, `_RFBMI5`,  `_BMI5CAT`,
                `_CHLDCNT`, `_EDUCAG`, `_INCOMG`, `_INCOMG`, 
                `_SMOKER3`, `_RFSMOK3`,
                `DRNKANY5`, `DROCDY3_`, `_RFBING5`, `_DRNKWK1`, `_RFDRHV7`, 
                `_TOTINDA`, `_PACAT2`, `_PAINDX2`, `_PASTRNG`, `_PAREC2`,
                `FRUTDA2_`,  `GRENDA1_`, `FRNCHDA_`, `POTADA1_`, `VEGEDA2_`,
                `_FRUTSU1`, `_VEGESU1`, 
                `_FRTLT1A`, `_VEGLT1A`,
                PHYSHLTH, PERSDOC2, 
                CVDINFR4, CVDSTRK3, ASTHMA3) %>%
  rename("STATE" = "_STATE")

table(brfss_data$`_MENT14D`)
table(brfss_data$STATE)
n_distinct(brfss_data$`STATE`)

# clean NMHSS data
nmhss_data = nmhss_2019_raw %>%
  mutate(LST = fips(LST, to = "FIPS")) %>% 
           rename("STATE" = "LST") %>% 
           group_by(STATE) %>%
  summarise(MHDIAGEVAL = sum(which(MHDIAGEVAL == 1)),
            TREATCOGTHRPY = sum(which(TREATCOGTHRPY == 1)),
            DIETEXERCOUNSEL = sum(which(DIETEXERCOUNSEL == 1)),
            MHHOUSING = sum(which(MHHOUSING == 1)),
            SUPPHOUSING = sum(which(SUPPHOUSING == 1)),
            SUPPEMPLOY = sum(which(SUPPEMPLOY == 1)),
            MHEMGCY = sum(which(MHEMGCY == 1)),
            MHSUICIDE = sum(which(MHSUICIDE == 1)),
            SED = sum(which(SED == 1)),
            SPMI = sum(which(SPMI == 1)),
            SRVC63 = sum(which(SRVC63 == 1)),
            SRVC31 = sum(which(SRVC31 == 1)),
            SPECGRPEATING = sum(which(SPECGRPEATING == 1)),
            POSTTRAUM = sum(which(POSTTRAUM == 1)),
            SRVC116 = sum(which(SRVC116 == 1)),
            SRVC62 = sum(which(SRVC62 == 1)),
            LANG = sum(which(LANG == 1)),
            FEESCALE = sum(which(FEESCALE == 1)),
            PAYASST = sum(which(PAYASST == 1)),
            REVCHK1 = sum(which(REVCHK1 == 1)),
            REVCHK8 = sum(which(REVCHK8 == 1)),
            REVCHK5 = sum(which(REVCHK5 == 1)),
            REVCHK10 = sum(which(REVCHK10 == 1)),
            FUNDSMHA = sum(which(FUNDSMHA == 1)),
            FUNDSTATEWELFARE = sum(which(FUNDSTATEWELFARE == 1)),
            FUNDSTATEJUV = sum(which(FUNDSTATEJUV == 1)),
            FUNDSTATEEDUC = sum(which(FUNDSTATEEDUC == 1)),
            FUNDOTHSTATE = sum(which(FUNDOTHSTATE == 1)),
            FUNDLOCALGOV = sum(which(FUNDLOCALGOV ==1)))

cor(nmhss_data %>% select(MHDIAGEVAL, DIETEXERCOUNSEL, MHHOUSING, SUPPEMPLOY, MHEMGCY,
                          MHSUICIDE, SED, SPMI, LANG, FEESCALE, PAYASST, REVCHK1,
                          REVCHK8, REVCHK5))

# join BRFSS data with NMHSS data
mental_health_data = inner_join(brfss_data, nmhss_data, by = "STATE")

table(mental_health_data$MENTHLTH)
table(mental_health_data$`_MENT14D`)

# remove those who did not know/not sure or refused and edit PHYSHLTH too
mental_health_data = mental_health_data %>% 
  filter(MENTHLTH != 77) %>%
  filter(MENTHLTH != 99) %>%
  mutate(MENTHLTH = ifelse(MENTHLTH == 88, 0, MENTHLTH)) %>%
  filter(PHYSHLTH != 77) %>%
  filter(PHYSHLTH != 99) %>%
  mutate(PHYSHLTH = ifelse(PHYSHLTH == 88, 0, PHYSHLTH)) %>%
  # remove those who did not know/not sure or refused/missing for _DRNKWK1
  filter(`_DRNKWK1` != 99900)

colnames(mental_health_data)
# rename columns that begin with underscore
mental_health_data = mental_health_data %>% 
  rename_all(funs(stringr::str_replace_all(., '_', '')))


# select features, removing those heavily correlated (e.g., calculated variables) 
# as well as categorical variables with too many levels
factor_cols = c("SEX", "URBSTAT", "MARITAL", "EDUCAG", "RENTHOM1",
                "VETERAN3", "EMPLOY1", "CHLDCNT", "INCOMG", "RACE", 
                "AGEG5YR", "HLTHPLN1", "MEDCOST", "CHECKUP1", "FLUSHOT7",
                "PNEUVAC4", "HIVTST7", "CHOLCH2", "PERSDOC2", "BPHIGH4", 
                "TOLDHI2", "DIABETE4", "DEAF", "BLIND", 
                "SMOKER3", "EXERANY2", "PASTRNG", "CVDINFR4", 
                "CVDSTRK3", "ASTHMA3", "ADDEPEV3")
mental_health_data = mental_health_data %>% 
  select(
    MENTHLTH,
    STATE,
    # select features summarized from nmhss data:
    MHDIAGEVAL, DIETEXERCOUNSEL, MHHOUSING, SUPPEMPLOY, MHEMGCY,
    MHSUICIDE, SED, SPMI, LANG, FEESCALE, PAYASST, REVCHK1,
    REVCHK8, REVCHK5,
    # select features from brfss data:
    # social, demographic, economic
    SEX, URBSTAT, MARITAL, EDUCAG, RENTHOM1, VETERAN3, EMPLOY1, CHLDCNT, 
    INCOMG, RACE, AGEG5YR, DEAF, BLIND,
    # clinical care
    HLTHPLN1, MEDCOST, CHECKUP1, FLUSHOT7,  
    PNEUVAC4, HIVTST7, CHOLCH2, PERSDOC2,
    # health including diet
    PHYSHLTH, BPHIGH4,  TOLDHI2, DIABETE4, ADDEPEV3,
    BMI5, SMOKER3, DRNKWK1,
    EXERANY2, PASTRNG, FRUTSU1, VEGESU1, FRNCHDA, 
    CVDINFR4, CVDSTRK3, ASTHMA3
    ) %>% na.omit() %>%
  mutate(across(all_of(factor_cols), factor))

n_distinct(mental_health_data$state)


# rename all columns
mental_health_data = mental_health_data %>%
  rename(
    "mental_health" = "MENTHLTH",
    "state" = "STATE",
    "diag_eval" = "MHDIAGEVAL",
    "diet_exer_counsel" = "DIETEXERCOUNSEL",
    "housing_services" = "MHHOUSING",
    "employ_services" = "SUPPEMPLOY",
    "emergency_services" = "MHEMGCY",
    "suicide_prev_services" = "MHSUICIDE",
    "sed_services" = "SED",
    "smi_services" = "SPMI",
    "other_languages" = "LANG",
    "fee_scale" = "FEESCALE",
    "pay_assist" = "PAYASST",
    "payment_cash" = "REVCHK1",
    "payment_medicare" = "REVCHK8",
    "payment_medicaid" = "REVCHK5",
    
    "health_coverage" = "HLTHPLN1",
    "med_cost" = "MEDCOST",
    "check_up" = "CHECKUP1",
    "chol_check" = "CHOLCH2",
    "personal_doctor" = "PERSDOC2",
    "flu_shot" = "FLUSHOT7",
    "pneumonia_shot" = "PNEUVAC4",
    "HIV_test" = "HIVTST7",
    
    "sex" = "SEX",
    "age" = "AGEG5YR",
    "marital" = "MARITAL",
    "race" = "RACE",
    "education" = "EDUCAG",
    "income" = "INCOMG",
    "urban_status" = "URBSTAT",
    "own_home" = "RENTHOM1",
    "veteran" = "VETERAN3",
    "employment" = "EMPLOY1",
    "child_count" = "CHLDCNT",
    "deaf" = "DEAF",
    "blind" = "BLIND",
    
    "physical_health" = "PHYSHLTH",
    "bmi" = "BMI5",
    "exercise" = "EXERANY2",
    "strength_activity_index" = "PASTRNG",
    "high_blood_pressure" = "BPHIGH4",
    "high_cholesterol" = "TOLDHI2",
    "diabetes" = "DIABETE4",
    "heart_attack" = "CVDINFR4",
    "stroke" = "CVDSTRK3",
    "asthma" = "ASTHMA3",
    "depression" = "ADDEPEV3",
    "smoker" = "SMOKER3",
    "alcohol_consumption" = "DRNKWK1",
    "fruits" = "FRUTSU1",
    "vegetables" = "VEGESU1",
    "french_fries" = "FRNCHDA",
  )

# write cleaned data to file
write_tsv(mental_health_data, file = "../data/clean/mental_health_data.tsv")

