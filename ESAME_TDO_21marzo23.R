######################################################

'ESAME DI TECNOLOGIE DIGITALI PER LE ORGANIZZAZIONI
21 marzo 2023

NOME E COGNOME: Daniele Iacobone

MATRICOLA: 992756

'

# PUNTO 1 -----------------------------------------------------------------
'
A)
1. Produrre le statistiche descrittive semplici delle colonne numeriche: dimensioni, 
tipi di dati
2. Determinare se ci sono valori mancanti nelle colonne
   RISPONDERE ALLA DOMANDA 1.
3. Cambiare il nome della prima colonna da PROVINCIA DI RESIDENZA a Provincia

B)
Trasformare il data frame in long, mantenendo inalterata la colonna Provincia e 
trasformando tutte le colonne relative alle automobili.
Salvare in df_long

C)
Da *df_long* selezionare tutte le righe, escluse quelle con:
1. Totale nella seconda colonna (name se non si è dato un nome proprio)
2. TOTALE nella prima (Provincia)
3. valori mancanti nella prima (Provincia)

Salvare in **df_long2**
  
'
# SOLUZIONI PUNTO 1 -------------------------------------------------------

####  RISPOSTE PUNTO A) 
library(tidyverse)

df=read_csv2('~/teddy-g4/Tecnologie Digitali/Esami2023/ESAMETDO_21marzo23/Auto_immatricolazioni_Sett2022.csv')
df

str(df)
colSums(is.na(df))

# DOMANDA 1: QUALE MARCA HA IL NUMERO MAGGIORE DI VALORI MANCANTI?
# RISPOSTA: .......Mitsubishi

df = rename(df, Provincia = `PROVINCIA DI RESIDENZA`)
head(df)

####  RISPOSTE PUNTO B) 

df_long = pivot_longer(df, cols = `Alfa Romeo`:`Totale`)
df_long

####  RISPOSTE PUNTO C) 

df_long %>% filter( name != 'Totale' &
                    Provincia != 'TOTALE' &
                    !is.na(Provincia) ) -> df_long2

##########################################################################
#
# PUNTO 2 -----------------------------------------------------------------
"
A)
1. Leggere il dataset regioni_province.csv e salvare nel data frame prov
2. Eseguire join con df_long2 (presente anche come df_long2.csv nel materiale d'esame). Specificare le colonne che si usano come chiavi. Salvare in df_join

B)
3. Trovare i nomi delle province di df_long2 che non hanno avuto corrispondenza nella join e salvare in no_match
RISPONDERE A DOMANDA 2

4. Modificare il data frame df_long2 con i nomi corretti delle province. Salvare.
   Rifare la join del punto A.2 e salvare in df_join.

"
# SOLUZIONI PUNTO 2 -------------------------------------------------------

####  RISPOSTE PUNTO A) 
# 1)

prov = read_csv('~/teddy-g4/Tecnologie Digitali/Esami2023/ESAMETDO_21marzo23/regioni_province.csv')
head(prov)

df2_long = read_csv('~/teddy-g4/Tecnologie Digitali/Esami2023/ESAMETDO_21marzo23/df_long2.csv')
head(df2_long)

prov$Provincia = toupper(prov$Provincia)

df2_long %>% inner_join(prov, by='Provincia')

####  RISPOSTE PUNTO B) 

df2_long %>% anti_join(prov, by='Provincia') -> a1
unique(a1$Provincia)      # "AOSTA", "VERBANIA"

prov %>% anti_join(df2_long, by='Provincia') -> a1
unique(a1$Provincia)     # "VERBANO-CUSIO-OSSOLA", "VALLE D'AOSTA/VALLÉE D'AOSTE"

# DOMANDA 2: Scrivere i nomi delle province di df_long2 che non hanno avuto corrispondenza nella join con prov?
# RISPOSTA: ...."AOSTA", "VERBANIA"

df2_long$Provincia %>% str_replace_all('AOSTA',"VALLE D'AOSTA/VALLÉE D'AOSTE") %>% 
  str_replace_all('VERBANIA',"VERBANO-CUSIO-OSSOLA") -> df2_long$Provincia
head(df2_long)
tail(df2_long)

df2_long %>% inner_join(prov, by='Provincia') -> df_join
#write_csv(df_join, '~/teddy-g4/Tecnologie Digitali/Esami2023/ESAMETDO_21marzo23/df_join')


# PUNTO 3 GRUPPI E AGGREGAZIONI -------------------------------------
"
 Usare il data frame df_join (disponibile anche come df_join.csv). 
 
 A. Raggruppare per marca di auto e provincia, e ricavare il totale delle immatricolazioni, 
 poi ordinare in modo decrescente

 B. Ricavare, per ogni regione e marca di auto, il totale delle immatricolazioni, 
 per ogni regione selezionare solo la marca con il numero maggiore e ordinare in modo decrescente.
 Scrivere come unica istruzione in pipe.
 
 C. Ordinare in modo decrescente per immatricolazioni.
 RISPONDERE A DOMANDA 3
'
 # SOLUZIONI PUNTO 3 -------------------------------------------------------
"
df_join = read_csv('~/teddy-g4/Tecnologie Digitali/Esami2023/ESAMETDO_21marzo23/df_join.csv')
head(df_join)

####  RISPOSTE PUNTO A)

df_join %>% group_by(variable, Provincia) %>% 
  summarise(Tot_Marca = sum(value, na.rm=TRUE)) %>% 
              arrange(desc(Tot_Marca))
####  RISPOSTE PUNTO B)

df_join %>% group_by(Regione, variable) %>% 
  summarise(Tot_Marca = sum(value, na.rm=TRUE)) %>% 
  slice_max(order_by = Tot_Marca, n=1) %>% 
  arrange(desc(Tot_Marca))

####  RISPOSTE PUNTO C)

df_join %>% arrange(desc(value)) 

# DOMANDA 3: CHE DIFFERENZA C'È TRA IL RISULTATO DEL PUNTO 1 E DEL PUNTO 3?
# RISPOSTA: .....NESSUNA


