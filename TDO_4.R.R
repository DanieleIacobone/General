##################################################

# Esercitazione Extra 4

##################################################

library(tidyverse)
library(lubridate)

'Usiamo il dataset Berlino_furti_bici.csv tratto dagli open data 
della città di Berlino e riguardante le denunce di furto di biciclette.
I dati sono stati preventivamente elaborati per normalizzare le date

La descrizione delle colonne è la seguente:

| COLONNA                         | DESCRIZIONE                                            |
|---------------------------------|--------------------------------------------------------|
| CREATED_AM                      | data di inserimento                                    |
| TIME_START_DATE / TIME_END-DATE | intervallo di date nel quale è stato commesso il furto |
| TIME_START_HOUR / TIME_END-HOUR | intervallo di ore nel quale è stato commesso il furto  |
| LOR                             | riferimento geografico a zone di Berlino               |
| DAMAGES                         | valore stimato della bicicletta                        |
| TYPE_OF_BYCICLE                 | tipo di bicicletta                                     |
| OFFENSE                         | tipo di reato                                          |
| REASON FOR DETECTION            | motivo della chiamata                                  |
'

df = read_csv("/Users/Daniele/Desktop/tdo/dataset/Berlino_furti_bici.csv")

########################################################
# Statistiche descrittive, valori mancanti, tipi di dato
#######################################################

any(is.na(df)) # non ci sono valori mancanti

str(df) #i tipi di dati sono coerenti

summary(df) #niente da segnalare

########################################################
# Operazioni di aggregazione
#######################################################

# Per una variabile: TYPE_OF_BICYCLE
df %>% group_by(TYPE_OF_BICYCLE) %>% summarize(Numero=n(),
                                               Tot_Valore=sum(DAMAGES))
# Ordiniamo
df %>% group_by(TYPE_OF_BICYCLE) %>% 
  summarize(Numero=n(),Tot_Valore=sum(DAMAGES)) %>%
  arrange(desc(Numero))

# Per una variabile: mese
df %>% group_by(month(CREATED_AM)) %>% summarize(Numero=n(),
                                               Tot_Valore=sum(DAMAGES))
# Rinominiamo la prima colonna
df %>% group_by(month(CREATED_AM)) %>% 
  summarize(Numero=n(),Tot_Valore=sum(DAMAGES)) %>%
  rename(Mese=1)

# DOMANDA: i valori totale dei furti per mese sono confrontabili?
# Tip: il 2022 non è completo

# Rifacciamo, questa volta per anno e mese
yer_month<-df %>% group_by(year(CREATED_AM), month(CREATED_AM)) %>% 
  summarize(Numero=n(),Tot_Valore=sum(DAMAGES)) %>%
  rename(c(Anno=1, Mese=2))

# Come prima ma i mesi con i nomi 
a =df %>% group_by(year(CREATED_AM),
                month(CREATED_AM, 
                      label=TRUE)) %>% 
  summarize(Numero=n(),Tot_Valore=sum(DAMAGES)) %>%
  rename(c(Anno=1, Mese=2)) %>%
  arrange(Mese)
  
a %>%
  ggplot(aes(x = Anno, y = Numero)) + 
  geom_bar(stat = "identity",
           position = "dodge",
           width = 0.4,
           height = 3) + 
  facet_wrap(~ Mese) +
  theme_light() +
  ggtitle("Furti per mese") +
  labs(x = "Anno", y = "N. Furti") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

"DOMANDE: 
- i mesi che tipo di dato hanno?
- rispettano l'ordinamento corretto? provare
"
str(df_gr1) # Mese: Ord.factor w/ 12 levels "Gen"<"Feb"<"Mar"<..: 1 2 3 4 5 6 7 8 9 10 
arrange(df_gr1, desc(Mese)) # rispettano l'ordinamento

# Ordiniamo per valore
arrange(df_gr1, desc(Tot_Valore))

# Ordiniamo per valore mantenendo i gruppi originali (x Anno)
arrange(df_gr1, desc(Tot_Valore), .by_group = TRUE)
'DOMANDA: perché risultano esserci più furti nel 2022 rispetto ai
corrispondenti mesi del 2021?'

# Raggruppiamo per anno e giorni, senza mesi, i giorni con il nome 
# calcoliamo sempre numero e valore, poi ordiniamo per numero di furti
df %>% group_by(year(CREATED_AM),
                wday(CREATED_AM, label=TRUE)) %>% 
  summarize(Numero=n(),Tot_Valore=sum(DAMAGES)) %>%
  rename(c(Anno=1, Giorno=2)) %>%
  arrange(desc(Numero))

# Raggruppiamo per anno, mese e giorni, mesi e giorni con il nome 
# calcoliamo numero e valore 
df %>% group_by(year(CREATED_AM),
                month(CREATED_AM, label=TRUE),
                wday(CREATED_AM, label=TRUE)) %>% 
  summarize(Numero=n(),Tot_Valore=sum(DAMAGES)) %>%
  rename(c(Anno=1, Mese=2, Giorno=3))

# Come il precedente ma vogliamo per ogni mese i primi tre giorni per
# numero di furti
a<-df %>% group_by(year(CREATED_AM),
                month(CREATED_AM, label=TRUE),
                wday(CREATED_AM, label=TRUE)) %>% 
  summarize(Numero=n(),Tot_Valore=sum(DAMAGES)) %>%
  rename(c(Anno=1, Mese=2, Giorno=3)) %>%
  slice_max(Numero, n=3)

# Uguale ma per valore dei furti
df %>% group_by(year(CREATED_AM),
                month(CREATED_AM, label=TRUE),
                wday(CREATED_AM, label=TRUE)) %>% 
  summarize(Numero=n(),Tot_Valore=sum(DAMAGES)) %>%
  rename(c(Anno=1, Mese=2, Giorno=3)) %>%
  slice_max(Tot_Valore, n=3)

# DOMANDA: sono diversi i risultati, proponete una spiegazione ragionevole.

# Raggruppare e calcolare i risultati poi per ogni anno 
# i tre mesi con numero inferiore di furti e i tre mesi con numero maggiore
df %>% group_by(year(CREATED_AM),
                month(CREATED_AM, label=TRUE)) %>% 
  summarize(Numero=n(),Tot_Valore=sum(DAMAGES)) %>%
  rename(c(Anno=1, Mese=2)) %>%
  slice_min(Numero, n=3)

df %>% group_by(year(CREATED_AM),
                month(CREATED_AM, label=TRUE)) %>% 
  summarize(Numero=n(),Tot_Valore=sum(DAMAGES)) %>%
  rename(c(Anno=1, Mese=2)) %>%
  slice_max(Numero, n=3)

########################################################
# Creazione di colonne calcolate
#######################################################

# Vogliamo la percentuale mensile di furti di biciclette rispetto al totale annuale

# Prima ricaviamo i risultati poi creiamo la colonna
df %>% group_by(year(CREATED_AM),
                month(CREATED_AM, label=TRUE)) %>% 
  summarize(Numero=n(),Tot_Valore=sum(DAMAGES)) %>%
  rename(c(Anno=1, Mese=2)) -> df_gr1
df_gr1

df_gr1 %>% mutate(Tot_N_anno= Numero / sum(Numero),
                  Tot_Val_anno= Tot_Valore / sum(Tot_Valore))

########################################################
# Trasformazioni long-wide
#######################################################

# Trasformare da long a wide la colonna dei tipi di biciclette
# Prima aggreghiamo per data e ora del furto
df %>% group_by(TIME_START_DATE, TIME_START_HOUR, TYPE_OF_BICYCLE) %>% 
  summarize(Tot_Valore= sum(DAMAGES)) -> df_long

df_long %>% pivot_wider(names_from = TYPE_OF_BICYCLE ,values_from = Tot_Valore)

# DOMANDA: provare a trasformare wide-long senza avere aggregato, ma solo indicando
# le due colonne TIME_START_DATE e TIME_START_HOUR nel parametro id_cols=
# Cosa vuol dire il Warning che si genera?
# Usare view() per vedere i valori

df %>% pivot_wider(id_cols = c(TIME_START_DATE,TIME_START_HOUR), 
                   names_from = TYPE_OF_BICYCLE ,
                   values_from = DAMAGES) %>% view()

# Rifacciamo per avere valori mano sparsi (meno NA) senza l'ora del furto.
# Salvare

df %>% group_by(TIME_START_DATE, TYPE_OF_BICYCLE) %>% 
  summarize(Tot_Valore= sum(DAMAGES)) %>% 
  pivot_wider(names_from = TYPE_OF_BICYCLE ,values_from = Tot_Valore) -> df_long
df_long

########################################################
# Operazioni multicolonna
#######################################################

# Usando df_long ipotizziamo che i valori mancanti corrispondano a 0 furti.
# Facciamo le sostituzioni su tutte le colonne.

# Controlliamo dove sono i NA
colSums(is.na(df_long))

# Iniziamo con la colonna bicycle.

str_replace_all(df_long$bicycle, NA, 0)
# Non va bene, i valori sono numerici, non caratteri alfanumerici

# Esistono vari modi
replace_na(df_long$bicycle,0)

df_long %>% mutate(bycicle = ifelse(is.na(bicycle), 0, bicycle))

# Ora per tutte le colonne

# Modo semplice, ma limitato
df_long %>% replace(is.na(.), 0)

# Questo va bene se si vuole fare la sostituzione su tutte le colonne.
# Se invece si deve controllare l'elenco delle colonne allora bisogna 
# ricorrere ad altro

df_long %>% mutate(across(.cols= bicycle:`cargo bike`, ~ replace_na(.,0)))
