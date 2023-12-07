rm(list=ls(all=TRUE))
### Tecnologie digitali per le organizzazioni ###
### Esercitazione 1 (02/11/22)

## Programma di oggi
## 1) Working directories
## 2) Aprire diversi tipi di csv file
## 3) Oggetti e data frame
## 4) Statistiche descrittive
## 5) Operazioni di data tidying


################# 1) Impostando la working directory ####################
getwd() #mostra la working directory corrente: dove si salvano tutti i file se non specifichiamo altrimenti e dove R cercerà i datasets che vogliamo usare

#Possiamo sempre cambiare la working directory
setwd("D:\Desktop\teaching\TDO_22") # Perché dà l'errore e cosa dobbiamo cambiare?

setwd("D:\\Desktop\\teaching\\TDO_22") # per indicare il passaggio alla cartella dobbiamo usare doppio backlash "\\" 
#or
setwd("D:/Desktop/teaching/TDO_22") # o "/"
########################################################################



################# 2) Diversi tipi di csv file ####################

#Proviamo a leggere i diversi dataset
# Dataset1, samstats_protests: summary statistics sulle proteste e uso di Social Media in Belarus 2022. File CSV con ";" come separatore
# Dataset2, IT_snap_election: Facebook campaigning di Partito Democratico 2022. File csv con "," come separatore
# Dataset3, uguale a dataset 2, ma in formato txt (tab-delimited)

install.packages("tidyverse") #se non è stato ancora installato
library(tidyverse)

df1<-read_csv("D:/Desktop/teaching/sumstats_protests.csv") #Perché dà l'errore? 
df1<-read_csv("D:/Desktop/teaching/TDO_22/sumstats_protests.csv") #Dobbiamo sempre controllare che abbiamo indicato il passaggio/nome corretto

head(df1)# Il df risulta avere solo una colonna - è un errore. Come lo possiamo correggere?
df1<-read_csv2("sumstats_protests.csv") # Perché questa volta non ho usato il nome assoluto di file (non ho indicato il passaggio completo) 
head(df1)
tail(df1)
df1<-read_delim("sumstats_protests.csv")# opzione alternativa. Si sceglie il separatore in automatico (ma a volte si può scegliere il separatore sbagliato)
head(df1,10) 
tail(df1,7)
tail(df1,-13) #che parte del df verrà mostrata in questo caso?
head(df1,-10)
head(df1, c(7,2))
# 1) Come posso visualizzare le prime 9 righe e 4 colonne?  
# 2) E le ultime 10 righe e 3 colonne?

#####
head(df1, c(9,4))
tail(df1, c(10,3))
#####

#Come dobbiamo aprire il secondo dataset, se è un file csv con il separatore ","
#####Errori comuni####
df2<-read_csv(IT_snap_election) #dobbiamo sempre specificare l'estensione del file, csv nel nostro caso
df2<-read_csv(IT_snap_election.csv) #non dobbiamo dimenticare di mettere gli apici
df2<-read_csv("IT_snap_election.csv") #quante colonne ci sono in questo dataset? tutte le variabili hanno valori numerici?
view(df2)
#####

#Come dobbiamo aprire il terzo dataset, se è un file in formato txt (tab-delimited).
#Può essere chiamato anche con .tsv o genericamente con .csv
#####
df3<-read_delim("IT_snap_election.txt") 
df3<-read_tsv("IT_snap_election.txt")
view(df3)
#####

remove(df1,df3)   #per togliere oggetti che non usiamo dopo 
########################################################################



################# 3) Oggetti e dataframe ################# 
var1=27 #numerici
var2=13
var3=var1+var2 # con le variabili numeriche si possono effettuare le operazione aritmetiche
var4=var1-var2
var5=var1*var2
var6=var1/var2
var7="Hello world" #alfanumerici

#dataframes
df2 #quante osservazioni ha il nostro df? Quali variabili abbiamo?

df_prova=read_csv("IT_snap_election.csv",skip = 17) # Cosa è cambiato rispetto al df2?
remove(df_prova)
########################################################################



################# 4. Statistiche discretive ################# 
#funzioni principali per visualizzare il df
head(df2)
tail(df2)
View(df2)
df2$Page.Category #per visualizzare solo una colonna specifica 
df2$Message[1:20] #per visualizzare solo le osservazione (righe) da 1 a 20 di una colonna(variabile) specifica  

#Visualizzare il df + info sul tipo delle variabili (e.g. numerici, alfanumerici, le date etc.)
str(df2)
glimpse(df2)

#statistica descrittiva
summary(df2)  

# Qual è il valore medio di Comments & Likes? Perche non abbiamo la mediana e media per i variabili "Post.Created.Date" e "Message"? Qual'è il valore massimo di "angry" reactions? E valore minimo di shares?


install.packages("inspectdf")
library(inspectdf)
library(readr)
inspect_na(df2)%>%print(n=Inf)#vediamo se abbiamo i valori mancanti. Quali variabili hanno i valori mancanti e quanti osservazioni ci mancano? 


install.packages("dlookr")
library(dlookr)
describe(df2)
describe(df2)%>%View()
diagnose(df2)#visualizza informazioni più dettagliate sui valori mancanti
diagnose_numeric(df2)#solo per le variabili numerici
plot_box_numeric(df2)
########################################################################



################# 5. Data tidying ################# 
#proviamo a rinominare alcuni variabili (Page.Name -> Party, Page.Admin.Top.Country -> Country)

rename(df2,Page.Name = Party, Page.Admin.Top.Country = Country) #Perché dà l'errore?  
rename(df2,Party = Page.Name, Country = Page.Admin.Top.Country) #Prima dobbiamo indicare il nuovo nome della colonna e poi il nome esistente non viceversa.
view(df2) #perché i nomi delle colonne sono rimasti uguali
df2<-rename(df2,Party = Page.Name, Country = Page.Admin.Top.Country)

#Provate a rinominare il Post.Created.Date->Date e Post.Created.Time->Time e salvare i risultati in nuovo dataframe "df3"
#####

df2=rename(df2,Date = Post.Created.Date, Time = Post.Created.Time) 

#invece di "<-" possiamo usare "="
#####

#proviamo a cambiare la posizione di alcune colonne
relocate(df2, Message,Link) #Message e Link diventano prime due colonne

df3=df2%>%relocate(Message, Link, .after = Party)# adesso la sequenza delle colonne diventa: Party, Message, Link, etc

#Come possiamo mettere il "Type" before "Link", e Page.Category Country come ultime colonne?
#####

df3%>%relocate(Type, .before = Link) %>%
  relocate(Page.Category,Country, .after = Care)
######

#Proviamo a ordinare il df secondo i valori delle variabili (crescente/decrescente)

df3%>%arrange(Total.Interactions) 

#Prime righe - post che hanno ottenuto meno Interactions; le ultime righe - posts con il numero dei Interactions più alto

df_likes=arrange(df3,desc(Likes)) # Adesso le prime osservazioni in df. quale sono?
df_dates_likes=arrange(df3,desc(Date),Likes)

#Come possiamo ordinare i valori di Date in maniera decrescente a parità di valori in maniera crescente per Comments? 
#####
df_dates_comments=arrange(df3,desc(Date),Comments)
#####


#Come possiamo selezionare le colonne specifiche?
?select #come possiamo selezionare le colonne multiple?
#range
select(df3,1:5)
select(df3,Party:Country)
#variabili specifiche
select(df3,c(Message,Likes,Comments,Date))
#tutte tranne Link,Page.Category,Country
select(df3,!c(Link,Page.Category,Country))
#colonna che inizia con pattern specifico
select(df3, starts_with("Total"))


#Proviamo a creare le nuove colonne
df3$Date #per selezionare la colonna specifica, ma può essere usato per la creazione di colonna nuove. Basta metter dopo $ il nome di colonna che vogliamo creare
df3$Network=FB #da l'errore
df3$Network="FB" #dobbiamo usare doppi apici
df3[[20]]="Legislative"
#per rimuovere la colonna:
df3[[20]]="NULL"#da l'errore
df3[[20]]=NULL#senza apici
#colonne con i valori calcolati
df3$emoji_reactions= df3$Love+df3$Haha+df3$Wow+df3$Sad+df3$Angry+df3$Care
df3$share_negative=((df3$Sad+df3$Angry)/df3$emoji_reactions)*100
#errori frequenti
df3$emoji_reactions= df$Love+df3$Haha#controllate sempre il nome di df
df3$emoji_reactions= df3$love+df3$Haha#...e anche il nome di variabile


#Altro modo per creare le colonne è tramite la funzione "mutate". Mutate può essere usato con pipe (%>%)
df3 = mutate(df3, emoji_reactions = Love+ Haha+ Wow+ Sad+ Angry+ Care)
df3 = mutate(df3, share_negative = ((Sad+ Angry)/emoji_reactions)*100) #sono identici ai risultati ottenuti con df3$variabile

########################################################################
#Come possiamo usare pipe con mutate e altre funzioni? 
df2 %>% 
  select(1, 17,18, 4:6, 7:16) %>% #riordiniamo le colonne
  mutate(positive = Wow + Haha, #creiamo la colonna di somma di reazioni positive,
         negative = Sad + Angry,#creiamo la colonna di somma di reazioni positive
         negative_share = (negative/(Love+Wow+Haha+Sad+Angry+Care)/negative)*100 ) %>% #percentuale dei emoji negativi a reazioni totali
  arrange(desc(negative_share)) #ordiniamo i valori di negative share in maniera decrescente
#Come possiamo salvare i risultati in un altro df?





rm(list=ls(all=TRUE))
#################################
df2<-read_csv("IT_snap_election.csv")
df2[df2$Love == 103, ]
df2[df2$Type != "Photo", ]
df2[df2$Shares <= 50, ]
df2[(df2$Likes > 50) & (df2$Type == "Native Video"), ]
df2[df2$Likes > 1000 & df2$Type == "Native Video", ]

df3<-df2[(df2$Likes > 1000) | (df2$Comments > 800), ]
#[] are for logical conditions, so we display the elements of it_election that satisfy the condition specified in []
result <- it_election %>%
  filter(Likes > 500, Type == "Native Video")






df_flights<-read_csv("C:\\Users\\Tamara\\Dropbox\\PC\\Downloads\\Flights22jan (1).csv")

df_flights$ArrDelayMinutes
df_delay<-df_flights[df_flights$ArrDelayMinutes >= 60, ]


df_flights[(df_flights$DestStateName =="New York") & (df_flights$ArrDelayMinutes >= 60),] # specific state and delay

df_flights[(df_flights$DestStateName =="New York") | (df_flights$DestStateName =="Washington"),]


unique(df_flights$FlightDate) # date , states several 
pro<- df_flights[(df_flights$FlightDate=="2022-01-17")& ((df_flights$DestStateName =="New York") | (df_flights$DestStateName =="Washington")),]
