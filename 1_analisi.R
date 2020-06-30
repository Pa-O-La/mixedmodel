library(dplyr)

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
#exams_passed_aggr <- read.csv(paste0(mydirdi,'exams_passed_aggr.csv'))
exams_passed_aggr <- read.csv(paste0(mydirdi,'exams_passed_aggr_vw.csv'))
exams_passed_aggr <- rename (exams_passed_aggr, STUD_ATTFRM_FRQ_AA = STUD_ACQSZ_CFU_AA )
exams_passed_aggr <- rename (exams_passed_aggr, STUD_ATTFRM_FRQ_SEM = STUD_ACQSZ_CFU_SEM_CALC )                      
colnames(exams_passed_aggr)


# MOBILITY table (abroad studies)
mobility <- read.csv(paste0(mydirdi,'SPEET_MOBILITY.csv'))
# there's no mobility on first years
no_mobility <- merge(mobility, careers, by.x = c('CARR_AN_ID', 'SI_CNT_AA'), by.y = c('CARR_AN_ID', 'CARR_INGR_AA'))


# Data Preparation

# EXAMS PASSED AGGREGATED TABLE
# we keep the aggregated information by year
exams_pass_aggr_year <- subset(exams_passed_aggr, exams_passed_aggr$AGGR_TYPE=='A')
# we keep just some selected features: CARR_AN_ID, STUD_ATTFRM_FRQ_AA, CFU_FALLIMENTI, CFU_PASSATI, MEDIA_PESATA
exams_pass_aggr_year_sel <- exams_pass_aggr_year[c( 'CARR_AN_ID', 'STUD_ATTFRM_FRQ_AA', 'CFU_FALLIMENTI', 'CFU_PASSATI', 'MEDIA_PESATA')]

# EXAMS NOT PASSED AGGREGATED TABLE
# we keep the aggregated information by year
exams_not_pass_aggr_year <- subset(exams_not_passed_aggr, exams_not_passed_aggr$AGGR_TYPE=='A')
# we keep just some selected features: CARR_AN_ID, STUD_ATTFRM_FRQ_AA, CFU_FALLIMENTI
exams_not_pass_aggr_year_sel <- exams_not_pass_aggr_year[c('CARR_AN_ID', 'STUD_ATTFRM_FRQ_AA', 'CFU_FALLIMENTI')]

# JOIN between exams passed and exams not passed modified tables by CAREER ID and YEAR
exams_aggr_year_sel <- merge(exams_pass_aggr_year_sel, exams_not_pass_aggr_year_sel, by.x = c('CARR_AN_ID', 'STUD_ATTFRM_FRQ_AA'), by.y = c('CARR_AN_ID', 'STUD_ATTFRM_FRQ_AA'), all = TRUE)
# we substitute 0 (zero) to null values of the features CFU_FALLIMENTI and CFU_PASSATI
exams_aggr_year_sel$CFU_FALLIMENTI.y[ is.na(exams_aggr_year_sel$CFU_FALLIMENTI.y)] <- 0
exams_aggr_year_sel$CFU_FALLIMENTI.x[ is.na(exams_aggr_year_sel$CFU_FALLIMENTI.x)] <- 0
exams_aggr_year_sel$CFU_PASSATI[ is.na(exams_aggr_year_sel$CFU_PASSATI)] <- 0
# we sum the two features CFU_FALLIMENTI from the JOIN operation into a new variable FAILED_CFU
exams_aggr_year_sel <- transform(exams_aggr_year_sel, FAILED_CFU=exams_aggr_year_sel$CFU_FALLIMENTI.x+exams_aggr_year_sel$CFU_FALLIMENTI.y)
# we remove the CFU_FALLIMENTI.x/y features
drops <- c('CFU_FALLIMENTI.x','CFU_FALLIMENTI.y')
exams_aggr_year_sel <- exams_aggr_year_sel[ , !(names(exams_aggr_year_sel) %in% drops)]


# JOIN of exams table with the CAREERS table
dataset <- merge(careers, exams_aggr_year_sel, by.x = c('CARR_AN_ID', 'CARR_INGR_AA'), by.y= c('CARR_AN_ID','STUD_ATTFRM_FRQ_AA') , all.x = TRUE )

# COURSES CATALOGUE --> added in careers
#courses_catalogue <- read.csv(paste0(mydirdi,'course_cat.csv'))
#drops <- c("CDS_DN", "SEDE", "CDS_SIGLA")
#courses_catalogue <- courses_catalogue[ , !(names(courses_catalogue) %in% drops)]
#dataset <- merge(dataset, courses_catalogue, by.x = c('IMM_CDS_ID'), by.y= c('CDS_ID') , all.x = TRUE )

# Dataset description
str(dataset)

# Data Cleaning
# remove dataset$STUD_AMM_VOTO<0 (7 negative values)
dataset <- dataset[ dataset$STUD_AMM_VOTO >0 | is.na(dataset$STUD_AMM_VOTO)  , ]

# 1766 NA. We substitute them with the median value
summary(dataset$STUD_AMM_VOTO)
median_missing = median(dataset$STUD_AMM_VOTO, na.rm=TRUE)

dataset  <- mutate(dataset, STUD_AMM_VOTO_REPLACED_MEDIAN  = ifelse(is.na(dataset$STUD_AMM_VOTO), median_missing, dataset$STUD_AMM_VOTO))
drops <- c("STUD_AMM_VOTO")
dataset <- dataset[ , !(names(dataset) %in% drops)]
summary(dataset$STUD_AMM_VOTO)

# remove CARR_DETT_FLTP features because the values are all the same (CL - Corso di Laurea)
drops <- c("CARR_DETT_FLTP")
dataset <- dataset[ , !(names(dataset) %in% drops)]

# we drop IMM_CDS_ID because different IDs can correspond to the same course
drops <- c("IMM_CDS_ID", "CDS_DN")
dataset <- dataset[ , !(names(dataset) %in% drops)]

# we drop CDS_POLI_EDU_FLD feature beacuse the values are all the same (I - Ingegneria)
drops <- c("CDS_POLI_EDU_FLD")
dataset <- dataset[ , !(names(dataset) %in% drops)]

# we drop the feature UIS_CDS_ID because we focus on the first step of the career
drops <- c("UIS_CDS_ID")
dataset <- dataset[ , !(names(dataset) %in% drops)]

# we drop the features HOM_GEO_PRV_CD and HOM_GEO_REG_DN because 
# we just keep the name of the province. We cannot keep the province ID code
# because Naples has as ID code "NA" that is interpreted as null value
# and foreign countries all have "n.a." as code, thus we could not know the exact name of
# the country
drops <- c("HOM_GEO_PRV_CD", "HOM_GEO_REG_DN")
dataset <- dataset[ , !(names(dataset) %in% drops)]

# there are only 47 people that has an S in HOM_IMM_CHANGED_COUNTRY
# thus we drop the column
drops <- c("HOM_IMM_CHANGED_COUNTRY")
dataset <- dataset[ , !(names(dataset) %in% drops)]

# there are 2 people with no TIT_MED_TP_CD_ELAB, thus we remove them
summary(dataset$TIT_MED_TP_CD_ELAB)
dataset <- dataset[dataset$TIT_MED_TP_CD_ELAB != "- ", ]

# we remove PREVIOUSSTUDIES feature because it's just the explanation
# of the TIT_MED_TP_CD_ELAB feature
drops <- c("PREVIOUSSTUDIES")
dataset <- dataset[ , !(names(dataset) %in% drops)]

# TIT_MED_GEO_PRV_CD contains "NA" that is interpreted as null value so 
# we have to substitute them. Instead, null values are denoted as "n.a."
# Meaning of "n.d." values?
dataset$TIT_MED_GEO_PRV_CD <- as.character(dataset$TIT_MED_GEO_PRV_CD)
dataset[is.na(dataset$TIT_MED_GEO_PRV_CD) & dataset$TIT_MED_GEO_REG=="Campania", "TIT_MED_GEO_PRV_CD"] <- "NAP"
dataset$TIT_MED_GEO_REG <- as.character(dataset$TIT_MED_GEO_REG)
dataset[dataset$TIT_MED_GEO_PRV_CD == "n.a.", "TIT_MED_GEO_PRV_CD"] <- dataset[dataset$TIT_MED_GEO_PRV_CD == "n.a.", "TIT_MED_GEO_REG"]
dataset$TIT_MED_GEO_PRV_CD[which(dataset$TIT_MED_GEO_PRV_CD == "n.d.")] <- NA
dataset$TIT_MED_GEO_PRV_CD <- as.factor(dataset$TIT_MED_GEO_PRV_CD)
dataset <- dataset[complete.cases(dataset[, "TIT_MED_GEO_PRV_CD"]), ]
#remove the duplicated column
drops <- c("TIT_MED_GEO_PRV_CD_2")
dataset <- dataset[ , !(names(dataset) %in% drops)]


# we drop the feature TIT_MED_GEO_REG because 
# we just keep the name of the province.
drops <- c("TIT_MED_GEO_REG")
dataset <- dataset[ , !(names(dataset) %in% drops)]

# We first transform TIT_MED_STT_DN as character to add the new level Estero
# Estero is given in case the country ID is different than "1", where 1 is Italy
# Than we drop the column TIT_MED_GEO_STT_ID because we keep the names of the
# countries
# The creation of this variables serves as a flag to say if the high school
# provenience is Italia or Estero (abroad)
dataset$TIT_MED_STT_DN <- as.character(dataset$TIT_MED_STT_DN)
dataset[dataset$TIT_MED_GEO_STT_ID!=1, "TIT_MED_STT_DN"] <- "Estero"
dataset$TIT_MED_STT_DN <- as.factor(dataset$TIT_MED_STT_DN)
drops <- c("TIT_MED_GEO_STT_ID")
dataset <- dataset[ , !(names(dataset) %in% drops)]

# we drop the feature TIT_MED_STT_DN because 
# we just keep the country ID.
drops <- c("TIT_MED_STT_DN")
dataset <- dataset[ , !(names(dataset) %in% drops)]

# TIT_CONS_VOTO has one null value thus we remove it
summary(dataset$TIT_CONS_VOTO)
dataset <- dataset[complete.cases(dataset[, "TIT_CONS_VOTO"]), ]
summary(dataset$TIT_CONS_VOTO)

# we drop the feature TIT_CONS_VOTO_FS because the values are all 100
drops <- c("TIT_CONS_VOTO_FS")
dataset <- dataset[ , !(names(dataset) %in% drops)]

# we remove the student with "-" in the tax features (they are 106 values and they are all
# related to 2010)
summary(dataset$TAX)
dataset <- dataset[dataset$TAX != "-", ]
summary(dataset$TAX)

# CFU_PASSATI contains 1471 null values
summary(dataset$CFU_PASSATI)
dataset[is.na(dataset$CFU_PASSATI), "CFU_PASSATI"] <- 0
summary(dataset$CFU_PASSATI)

# there are 10127 students with NA as MEDIA_PESATA
# Likely, most of them, has 0 in CFU_PASSATI. So, MEDIA_PESATA could be replaced
# just with 0
summary(dataset$MEDIA_PESATA)
dataset[is.na(dataset$MEDIA_PESATA) & dataset$CFU_PASSATI==0, "MEDIA_PESATA"] <- 0
summary(dataset$MEDIA_PESATA)

# there are 1471 null values. They seems the same of CFU_PASSATI.
# Thus, there are 1471 people with no CFU information. Should we delete them
# or just put 0? Why do they have a null value?
summary(dataset$FAILED_CFU)
dataset[is.na(dataset$FAILED_CFU), "FAILED_CFU"] <- 0
summary(dataset$FAILED_CFU)

# we drop CARR_FLST because STATUS has the same value with the distinction between Early and Late dropout
drops <- c("CARR_FLST")
dataset <- dataset[ , !(names(dataset) %in% drops)]

# Update of the levels
dataset <- droplevels(dataset)

# We remove null rows and update row names (indices)
# complete.cases(dataset)
# is.na(dataset)

#dataset <- na.omit(dataset)

# Load package for saving csv files
library(data.table)

# Write CSV
fwrite(dataset, paste0(mydirdo,'dataset.csv'))


