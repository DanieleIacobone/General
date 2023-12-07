rm(list=ls(all=TRUE))
### Tecnologie digitali per le organizzazioni ###
### Esercitazione 3 (07/11/23)

library(tidyverse)


df_flights<-read_csv("C:\\Users\\Tamara\\Dropbox\\PC\\Downloads\\Flights22jan.csv")


########################################################
# Operazioni multicolonna
#######################################################


# Controlliamo dove sono i NA
colSums(is.na(df_flights))

# Iniziamo con la colonna CancellationCode
unique(df_flights$CancellationCode)

str_replace_all(df_flights$CancellationCode, NA, 0)
# Non va bene, i valori sono numerici, non caratteri alfanumerici

# Esistono vari modi
replace_na(df_flights$CancellationCode,"NO DELAY")

prova<-df_flights %>% mutate(CancellationCode = ifelse(is.na(CancellationCode), "NO DELAY", CancellationCode))
unique(prova$CancellationCode)
# Ora per tutte le colonne

# Modo semplice, ma limitato
prova$Tail_Number<-as.character(prova$Tail_Number)
prova<-df_flights%>%select( FlightDate, Reporting_Airline, DestStateName, WeatherDelay,  NASDelay)
prova %>% replace(is.na(.), 0)

# Questo va bene se si vuole fare la sostituzione su tutte le colonne.
# Se invece si deve controllare l'elenco delle colonne allora bisogna 
# ricorrere ad altro

df_flights %>% mutate(across(.cols=  WeatherDelay:LateAircraftDelay, ~ replace_na(.,0)))


###############################################################
# Condizione Logiche
###############################################################
df_flights$ArrDelayMinutes

df_delay_little<-df_flights[df_flights$ArrDelayMinutes <= 60, ]
df_delay_big<-df_flights[df_flights$ArrDelayMinutes > 60, ]


unique(df_delay$ArrDelayMinutes)

df_flights[(df_flights$DestStateName =="New York") & (df_flights$ArrDelayMinutes >= 60),] # specific state and delay
# L'output sono i voli con destinazione New York con o con più di 60 min di ritardo. 

df_flights[!(df_flights$DestStateName =="New York") & !(df_flights$ArrDelayMinutes >= 60),]

df_flights[!(df_flights$DestStateName =="New York") & !(df_flights$DestStateName =="Washington"),]

df_flights[(df_flights$DestStateName =="New York") | (df_flights$DestStateName =="Washington"),]

unique(df_flights$FlightDate) 
unique(df_flights$DestStateName)# date , states several 

voli_date<- df_flights[(df_flights$FlightDate=="2022-01-17") & ((df_flights$DestStateName =="New York") | (df_flights$DestStateName =="Washington")),]
unique(voli_date$DestStateName)

#################################################################
# Operazioni su date
################################################################

# Load the lubridate package
library(lubridate)

str(voli_date)
voli_date$FlightDate<-as.character(voli_date$FlightDate)

# Convert character dates to Date objects
voli_date2$FlightDate <- ymd(voli_date2$FlightDate)

# Extract components from the dates
voli_date2$Year_flight <- year(voli_date2$FlightDate)
voli_date2$month_flight <- month(voli_date2$FlightDate)
voli_date2$day_flight <- day(voli_date2$FlightDate)

# Calculate the difference between two dates
unique(df_flights$FlightDate)
df_flights$FlightDate<-ymd(df_flights$FlightDate)

date_diff <- max(voli_date2$FlightDate) - min(voli_date2$FlightDate)
date_diff <- max(df_flights$FlightDate) - min(df_flights$FlightDate)

#################################################################
# Operazioni su stringhe
################################################################
bici<-read_csv("D:\\Desktop\\teaching\\TDO_22\\TDO_EXTRA_3\\Berlino_furti_bici.csv")
unique(bici$TYPE_OF_BICYCLE)
sub<-bici[str_detect(bici$TYPE_OF_BICYCLE,"bike"),] #str_detect permette di selezionare i osservazione (tipo character) con i valori specifiche, Qua con la funzione str_detect seleziona tutte le righe dove TYPE_OF_BICYCLE ha value "bike"
sub$TYPE_OF_BICYCLE<-str_replace_all(sub$TYPE_OF_BICYCLE,	"bike",	"")  #con la funzione str_replace_all trovo tutti i osservazioni che hanno "bike" e tolgo questa parte - la sostituisco con niente "" (non c'è niente dentro i apici doppi)



###### operazioni logiche ###########
#not various bycycles and demage more than 500
bici_nonvarie<- bici[(!bici$TYPE_OF_BICYCLE=="various bicycles") & (bici$DAMAGES > 500),]

unique(bici_nonvarie$TYPE_OF_BICYCLE)


#cargo bike stolen in 2022
bici_nonvarie$TIME_START_DATE<-ymd(bici_nonvarie$TIME_START_DATE)
bici_nonvarie$Year<-year(bici_nonvarie$TIME_START_DATE)

unique(bici_nonvarie$Year)
bici_cargo<- bici_nonvarie[(bici_nonvarie$TYPE_OF_BICYCLE=="cargo bike") & (bici_nonvarie$Year == 2022),]

