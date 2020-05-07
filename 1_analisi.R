#esplorazione dati

mydirdi <- paste0(getwd(),"/data/datain/")
mydirdo <- paste0(getwd(),"/data/dataout/")

#descrizione dei corsi di laurea
degree_desc <- read.csv(paste0(mydirdi,'SPEET_DEGREE_INFORMATION.csv'))


#esami
#esami non passati ma con numeri di tentativi
exams_not_passed_aggr <- read.csv(paste0(mydirdi,'speet_exams_not_passed_aggr.csv'))
#esami passati
exams_passed_aggr <- read.csv(paste0(mydirdi,'speet_exams_passed_aggr.csv'))
#esami validati --> valutare se influiscono 
#pensare ad un'analisi futura sulla correlazione del dropout
#exams_validated_aggr <- read.csv(paste0(mydirdi,'speet_exams_validated_aggr.csv'))

#dati di mobilità
mobility <- read.csv(paste0(mydirdi,'SPEET_MOBILITY.csv'))
#carriere 1 livello
#careers <- read.csv(paste0(mydirdi,'SPEET_POP_PLUS.csv'))
#titolo medio
#prev_studies <- read.csv(paste0(mydirdi,'SPEET_PREV_STUDIES.csv'))
#residenze e domicilio per capire se essere pendolari o fuori casa influisce
#home <- read.csv(paste0(mydirdi,'SPEET_RESIDENZE.csv'))
#esami
#exams <- read.csv(paste0(mydirdi,'SPEET_EXAMS.csv'))
#esami passati
#exams_passed_aggr <- read.csv(paste0(mydirdi,'speet_exams_passed_aggr.csv'))
#esami non passati ma con numeri di tentativi
#exams_not_passed_aggr <- read.csv(paste0(mydirdi,'speet_exams_not_passed_aggr.csv'))
#esami validati
#exams_validated_aggr <- read.csv(paste0(mydirdi,'speet_exams_validated_aggr.csv'))

#carriere 1 livello
careers <- read.csv(paste0(mydirdi,'careers.csv'))

table(degree_desc)
summary(degree_desc)
factor (degree_desc$SCOREIMPROVEMENT)

levels(degree_desc$NUMBERATTEMPTSTOBEEVALUATEDONEYEAR)



class(degree_desc)
names(degree_desc)
table(exams_passed_aggr$MEDIA_PESATA)
str(exams_passed_aggr)

#valutazione esami
md<-as.data.frame(iris[which(iris$Species=='virginica'),])

stud <- exams_passed_aggr[which(exams_passed_aggr$CARR_AN_ID == 307342),]
stud_fallimenti <- exams_not_passed_aggr[which(exams_not_passed_aggr$CARR_AN_ID == 307342),]

stud_2010 <- subset (exams_passed_aggr, exams_passed_aggr$CARR_AN_ID == 307342 
                                     & exams_passed_aggr$STUD_ATTFRM_FRQ_AA==2010 
                                     & exams_passed_aggr$TIPO_AGGREGAZIONE =='A')
stud_2011_1sem <- subset (exams_passed_aggr, exams_passed_aggr$CARR_AN_ID == 307342 
                          & exams_passed_aggr$STUD_ATTFRM_FRQ_AA==2011 
                          & exams_passed_aggr$TIPO_AGGREGAZIONE =='S'
                          & exams_passed_aggr$STUD_ATTFRM_FRQ_SEM ==1)
stud_pass <- stud_2010
stud_pass <- append(stud_2011_1sem, )


test<-subset(careers, careers$CARR_ING_ETA<31 & careers$TIT_CONS_VOTO_FS==100)

#there's no mobility on first years
my_mob<-merge(mobility, careers, by.x = c('CARR_AN_ID', 'SI_CNT_AA'), by.y = c('CARR_AN_ID', 'CARR_INGR_AA'))

a<-transform(a, pl= a$CARR_INGR_AA+1)

#exams

#passed by years
my_ex_p_yy <- subset(exams_passed_aggr, exams_passed_aggr$TIPO_AGGREGAZIONE=='A')
my_ex_p_yy<-my_ex_p_yy[c(1,2,6,7,8)]
#not passed by years
my_ex_n_p_yy <- subset(exams_not_passed_aggr, exams_not_passed_aggr$TIPO_AGGREGAZIONE=='A')
my_ex_n_p_yy<-my_ex_n_p_yy[c(1,2,6)]

my_ex_all_yy <- merge(my_ex_p_yy, my_ex_n_p_yy, by.x = c('CARR_AN_ID', 'STUD_ATTFRM_FRQ_AA'), by.y = c('CARR_AN_ID', 'STUD_ATTFRM_FRQ_AA'), all = TRUE)
#sub NA with 0
my_ex_all_yy$CFU_FALLIMENTI.y[ is.na(my_ex_all_yy$CFU_FALLIMENTI.y)] <- 0
my_ex_all_yy$CFU_FALLIMENTI.x[ is.na(my_ex_all_yy$CFU_FALLIMENTI.x)] <- 0
my_ex_all_yy$CFU_PASSATI[ is.na(my_ex_all_yy$CFU_PASSATI)] <- 0

my_ex_all_yy<-transform(my_ex_all_yy, failed_cfu=my_ex_all_yy$CFU_FALLIMENTI.x+my_ex_all_yy$CFU_FALLIMENTI.y)
my_ex_all_yy<-my_ex_all_yy[c(-3,-6)]

#remove duplicate
my_ex_all_yy<-unique(my_ex_all_yy)

my_car_ex_1y <- merge(careers, my_ex_all_yy, by.x = c('CARR_AN_ID', 'CARR_INGR_AA'), by.y= c('CARR_AN_ID','STUD_ATTFRM_FRQ_AA') , all.x = TRUE )
