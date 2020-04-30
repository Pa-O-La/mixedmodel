#esplorazione dati

mydirdi <- paste0(getwd(),"/data/datain/")
mydirdo <- paste0(getwd(),"/data/dataout/")

#descrizione dei corsi di laurea
degree_desc <- read.csv(paste0(mydirdi,'SPEET_DEGREE_INFORMATION.csv'))

#not passed exams
exams_not_passed <- read.csv(paste0(mydirdi, 'SPEET_EXAMS_NOT_PASSED.csv'))
# passed exams
exams_passed <- read.csv(paste0(mydirdi, 'SPEET_EXAMS_PASSED.csv'))
# passed exams aggregated table
exams_passed_aggr <- read.csv(paste0(mydirdi, 'SPEET_EXAMS_PASSED_AGGR.csv'))
#statistiche degli esami
exams_stats <- read.csv(paste0(mydirdi,'SPEET_EXAMS_STATS.csv'))
#dati di mobilità
mobility <- read.csv(paste0(mydirdi,'SPEET_MOBILITY.csv'))
#carriere 1 livello
careers <- read.csv(paste0(mydirdi,'SPEET_POP_PLUS.csv'))
#titolo medio
prev_studies <- read.csv(paste0(mydirdi,'SPEET_PREV_STUDIES.csv'))
#residenze e domicilio per capire se essere pendolari o fuori casa influisce
home <- read.csv(paste0(mydirdi,'SPEET_RESIDENZE.csv'))


table(degree_desc)
summary(degree_desc)
factor (degree_desc$SCOREIMPROVEMENT)

levels(degree_desc$NUMBERATTEMPTSTOBEEVALUATEDONEYEAR)


#PA filtra i 3 campi
library(dplyr)
degree_desc_ours<-select(degree_desc, starts_with('DEGREEID', 'CDS_POLI_EDU_FLD','DEGREENATURE'))
c(degree_desc$CDS_ID, degree_desc$DEGREENATURE)

factor(degree_desc$NUMBERATTEMPTSTOENROLESUBJECT)
#NUMBERATTEMPTSTOENROLESUBJECT
#NUMBERATTEMPTSTOBEEVALUATEDONEYEAR
#SCOREIMPROVEMENT
#degree_desc = degree_desc[] togliere colonne inutili
degree_desc_ours<-degree_desc (DEGREEID, CDS_POLI_EDU_FLD, DEGREENATURE)
head(degree_desc_ours)
