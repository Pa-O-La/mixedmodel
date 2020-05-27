# Data upload
mydirdi <- paste0(getwd(),"/data/datain/")
mydirdo <- paste0(getwd(),"/data/dataout/")

# CAREERS table - first level (Bachelor's degree) careers
careers <- read.csv(paste0(mydirdi,'careers.csv'))
# we keep only people that are less than 30 years old because older people have
# different paces and different ways to handle the studies, so they cannot be used
# to predict younger people. Also, a maximum grade different than 100 from the high
# school means that those people are older (for example, 60 as maximum grade was
# used in the past)
careers <- subset(careers, careers$CARR_ING_ETA<31 & careers$TIT_CONS_VOTO_FS==100)

# EXAMS tables
# Not passed exams aggregated by semester (S), year (Y), and total (T)
exams_not_passed_aggr <- read.csv(paste0(mydirdi,'exams_not_passed_aggr.csv'))
# Passed exmaes aggregated as above
exams_passed_aggr <- read.csv(paste0(mydirdi,'exams_passed_aggr.csv'))

# MOBILITY table (abroad studies)
mobility <- read.csv(paste0(mydirdi,'SPEET_MOBILITY.csv'))
# there's no mobility on first years
no_mobility <- merge(mobility, careers, by.x = c('CARR_AN_ID', 'SI_CNT_AA'), by.y = c('CARR_AN_ID', 'CARR_INGR_AA'))


# Data Preparation

# EXAMS PASSED AGGREGATED TABLE
# we keep the aggregated information by year
exams_pass_aggr_year <- subset(exams_passed_aggr, exams_passed_aggr$AGGR_TYPE=='A')
# we keep just some selected features: CARR_AN_ID, STUD_ATTFRM_FRQ_AA, CFU_FALLIMENTI, CFU_PASSATI, MEDIA_PESATA
exams_pass_aggr_year_sel <- exams_pass_aggr_year[c(1,2,6,7,8)]

# EXAMS NOT PASSED AGGREGATED TABLE
# we keep the aggregated information by year
exams_not_pass_aggr_year <- subset(exams_not_passed_aggr, exams_not_passed_aggr$AGGR_TYPE=='A')
# we keep just some selected features: CARR_AN_ID, STUD_ATTFRM_FRQ_AA, CFU_FALLIMENTI
exams_not_pass_aggr_year_sel <- exams_not_pass_aggr_year[c(1,2,6)]

# JOIN between exams passed and exams not passed modified tables by CAREER ID and YEAR
exams_aggr_year_sel <- merge(exams_pass_aggr_year_sel, exams_not_pass_aggr_year_sel, by.x = c('CARR_AN_ID', 'STUD_ATTFRM_FRQ_AA'), by.y = c('CARR_AN_ID', 'STUD_ATTFRM_FRQ_AA'), all = TRUE)
# we substitute 0 (zero) to null values of the features CFU_FALLIMENTI and CFU_PASSATI
exams_aggr_year_sel$CFU_FALLIMENTI.y[ is.na(exams_aggr_year_sel$CFU_FALLIMENTI.y)] <- 0
exams_aggr_year_sel$CFU_FALLIMENTI.x[ is.na(exams_aggr_year_sel$CFU_FALLIMENTI.x)] <- 0
exams_aggr_year_sel$CFU_PASSATI[ is.na(exams_aggr_year_sel$CFU_PASSATI)] <- 0
# we sum the two features CFU_FALLIMENTI from the JOIN operation into a new variable FAILED_CFU
exams_aggr_year_sel <- transform(exams_aggr_year_sel, FAILED_CFU=exams_aggr_year_sel$CFU_FALLIMENTI.x+exams_aggr_year_sel$CFU_FALLIMENTI.y)
# we remove the CFU_FALLIMENTI.x/y features
exams_aggr_year_sel <- exams_aggr_year_sel[c(-3,-6)]

# JOIN of exams table with the CAREERS table
dataset <- merge(careers, exams_aggr_year_sel, by.x = c('CARR_AN_ID', 'CARR_INGR_AA'), by.y= c('CARR_AN_ID','STUD_ATTFRM_FRQ_AA') , all.x = TRUE )



# Data Cleaning
# remove dataset$STUD_AMM_VOTO <0
dataset <- dataset[ dataset$STUD_AMM_VOTO >0 | is.na(dataset$STUD_AMM_VOTO)  , ]

# TODO 1766 NA      --->?????? remove o substitute with median/mean?
summary(dataset$STUD_AMM_VOTO)

# remove CARR_DETT_FLTP features because the values are all the same (CL - Corso di Laurea)
drops <- c("CARR_DETT_FLTP")
dataset <- dataset[ , !(names(dataset) %in% drops)]

# we convert the feature IMM_CDS_ID to factor type instead of numerical
dataset$IMM_CDS_ID <- as.factor(dataset$IMM_CDS_ID)

# we drop CDS_POLI_EDU_FLD feature beacuse the values are all the same (I - Ingegneria)
drops <- c("CDS_POLI_EDU_FLD")
dataset <- dataset[ , !(names(dataset) %in% drops)]

# we convert the feature UIS_CDS_ID to factor type instead of numerical
dataset$UIS_CDS_ID <- as.factor(dataset$UIS_CDS_ID)

# we drop the features HOM_GEO_PRV_CD and HOM_GEO_REG_DN because 
# we just keep the name of the province. We cannot keep the province ID code
# because Naples has as ID code "NA" that is interpreted as null value
drops <- c("HOM_GEO_PRV_CD", "HOM_GEO_REG_DN")
dataset <- dataset[ , !(names(dataset) %in% drops)]

# TODO there are only 47 people that has an S in HOM_IMM_CHANGED_COUNTRY
# can we remove them???
summary(dataset$HOM_IMM_CHANGED_COUNTRY)

# TODO there are 2 people with no TIT_MED_TP_CD_ELAB. Remove them?
summary(dataset$TIT_MED_TP_CD_ELAB)

# we remove PREVIOUSSTUDIES feature because it's just the explanation
# of the TIT_MED_TP_CD_ELAB feature
drops <- c("PREVIOUSSTUDIES")
dataset <- dataset[ , !(names(dataset) %in% drops)]

# TODO TIT_MED_GEO_PRV_CD contains "NA" that is interpreted as null value so 
# we have to substitute them. How can we know if they are from Naples 
# or null values?
# Meaning of "n.d." values?

# we drop the feature TIT_MED_GEO_REG because 
# we just keep the name of the province.
drops <- c("TIT_MED_GEO_REG")
dataset <- dataset[ , !(names(dataset) %in% drops)]

# TIT_MED_GEO_STT_ID should be interpreted as a categorical value as it is
# a country ID
dataset$TIT_MED_GEO_STT_ID <- as.factor(dataset$TIT_MED_GEO_STT_ID)

# we drop the feature TIT_MED_STT_DN because 
# we just keep the country ID.
drops <- c("TIT_MED_STT_DN")
dataset <- dataset[ , !(names(dataset) %in% drops)]

# TODO TIT_CONS_VOTO has one null value. Remove it?
summary(dataset$TIT_CONS_VOTO)

# we drop the feature TIT_CONS_VOTO_FS because the values are all 100
drops <- c("TIT_CONS_VOTO_FS")
dataset <- dataset[ , !(names(dataset) %in% drops)]

# TODO meaning of "-" tax?
summary(dataset$TAX)

# TODO CFU_PASSATI contains 1471 null values
summary(dataset$CFU_PASSATI)

# TODO there are 10127 students with NA as MEDIA_PESATA
# Likely, most of them, has 0 in CFU_PASSATI. So, MEDIA_PESATA could be replaced
# just with 0
summary(dataset$MEDIA_PESATA)

# TODO there are 1471 null values. They seems the same of CFU_PASSATI.
# Thus, there are 1471 people with no CFU information. Should we delete them
# or just put 0? Why do they have a null value?
summary(dataset$FAILED_CFU)