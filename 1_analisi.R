#esplorazione dati

mydirdi <- paste0(getwd(),"/data/datain/")
mydirdo <- paste0(getwd(),"/data/dataout/")

#carriere 1 livello
carreers <- read.csv(paste0(mydirdi,'SPEET_POP_PLUS.csv'))
#descrizione dei corsi di laurea
degree_desc <- read.csv(paste0(mydirdi,'SPEET_DEGREE_INFORMATION.csv'))
#esami
exams <- read.csv(paste0(mydirdi,'SPEET_EXAMS.csv'))
#not passed exams
exams_not_passed <- read.csv(paste0(mydirdi, 'SPEET_EXAMS_NOT_PASSED.csv'))
# passed exams
exams_passed <- read.csv(paste0(mydirdi, 'SPEET_EXAMS_PASSED.csv'))
# passed exams aggregated table
exams_passed_aggr <- read.csv(paste0(mydirdi, 'SPEET_EXAMS_PASSED_AGGR.csv'))
#statistiche degli esami
exams_stats <- read.csv(paste0(mydirdi,'SPEET_EXAMS_STATS.csv'))
#titolo medio
prev_studies <- read.csv(paste0(mydirdi,'SPEET_PREV_STUDIES.csv'))
#residenze e domicilio per capire se essere pensolari o fuori casa influisce
home <- read.csv(paste0(mydirdi,'SPEET_RESIDENZE.csv'))
#dati di mobilità
mobility <- read.csv(paste0(mydirdi,'SPEET_MOBILITY.csv'))


table(degree_desc)
