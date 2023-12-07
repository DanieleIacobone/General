'ESAME DI TECNOLOGIE DIGITALI PER LE ORGANIZZAZIONI'
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

'A)'

library(tidyverse)
auto <- read_csv2("Desktop/tdo/dataset/14-MIMS-provincia-settembre_633eeb3a566ce.csv")

'1.'
str(auto)
summary(auto)
glimpse(auto)

'2.'
sum(is.na(auto))
# Ci sono 482 valori NAs
view(colSums(is.na(auto)))
# DOMANDA 1: QUALE MARCA HA IL NUMERO MAGGIORE DI VALORI MANCANTI?
# Mitsubishi


'3. Cambiare nome variabile da PROVINCIA DI RESIDENZA a Provincia'

names(auto)
auto <- auto %>%
  rename("Provincia" = "PROVINCIA DI RESIDENZA")

'B)'

df_long <- auto %>%
  pivot_longer(cols = 2:37)

'C)'
names(df_long)
sum(is.na(df_long$Provincia))
df_long2 <- df_long %>%
  filter(!((`name` == "Totale") | (`Provincia` == "TOTALE") | (is.na(`Provincia`))))
  
sum(is.na(df_long2$Provincia))

# Sono state selezionate solo le righe richieste

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

'A)'
'1.'
prov <- read_csv2("Desktop/tdo/dataset/Codici-statistici-e-denominazioni-al-30_06_2021.csv",
                 locale = locale(encoding = "ISO-8859-1"))

'2.'
# Come chiave verranno usate le colonne Denominazione dell'Unità territoriale sovracomunale...
# E la chiave per il dataframe df_long2 sarà Provincia

# Cambio il nome
names(prov)
prov <- prov %>%
  rename(PROVINCIA = "Denominazione dell'Unità territoriale sovracomunale (valida a fini statistici)") %>%
  relocate(`PROVINCIA`, .before = "Codice Regione") %>%
  view()

# Riscrivo le righe della colonna provincia in maiuscolo dato che sono cosi nel df_long2
# Controllo che la colonna sia di tipo carattere prima di usare la funzione toupper()
is.character(prov$PROVINCIA)
prov <- prov %>%
  mutate(PROVINCIA = toupper(PROVINCIA))

# Eseguo la join
df_join <- prov %>% 
  inner_join(df_long2, by = c("PROVINCIA" = "Provincia"),
             relationship = "many-to-many")

df_anti1 <- prov %>%
  anti_join(df_long2, by = c("PROVINCIA" = "Provincia"))

unique(df_anti1$PROVINCIA)

df_anti2 <- df_long2 %>%
  anti_join(prov, by = c("Provincia" = "PROVINCIA"))

unique(df_anti2$Provincia)

# Al posto di anti_join uso la funzione filter. Prima devo eseguire la full_join

df_full <- prov %>%
  full_join(df_long2, by = c("PROVINCIA" = "Provincia"),
            keep = TRUE) %>%
  view()

# Ora uso la funzione filter

non_join_dflong2 <- df_full %>%
  filter((!is.na(`PROVINCIA`) & is.na(`Provincia`)))

unique(non_join_dflong2$PROVINCIA)

# Ora l'altro dataset

non_join_prov <- df_full %>%
  filter(is.na(`PROVINCIA`) & !is.na(`Provincia`))

unique(non_join_prov$Provincia)

# Correggo i valori che non corrispondono tenendo giusto il dataset ISTAT

df_long2$Provincia <- str_replace_all(df_long2$Provincia, 
                                     c("ASCOLIPICENO" = "ASCOLI PICENO",
                                       "BARLETTA-ANDRIATRANI" = "BARLETTA-ANDRIA-TRANI",
                                       "BOLZANO-BOZEN" = "BOLZANO/BOZEN",
                                       "FORLI' CESENA" = "FORLÌ-CESENA",
                                       "PESARO URBINO" = "PESARO E URBINO",
                                       "REGGIO DI CALABRIA" = "REGGIO CALABRIA",
                                       "REGGIO EMILIA" = "REGGIO NELL'EMILIA",
                                       "VERBANIA" = "VERBANO-CUSIO-OSSOLA"))

prov$PROVINCIA <- str_replace_all(prov$PROVINCIA,
                                  c("VALLE D'AOSTA/VALLÉE D'AOSTE" = "VALLE D'AOSTA"))


# Eseguo nuovamente la join

df_join <- prov %>% 
  inner_join(df_long2, by = c("PROVINCIA" = "Provincia"),
             relationship = "many-to-many")

df_anti1 <- prov %>%
  anti_join(df_long2, by = c("PROVINCIA" = "Provincia"))

df_anti2 <- df_long2 %>%
  anti_join(prov, by = c("Provincia" = "PROVINCIA"))

# Tutti i nomi delle province sono stati corretti

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
 # DOMANDA 3: CHE DIFFERENZA C'È TRA IL RISULTATO DEL PUNTO 1 E DEL PUNTO 3?
"

"A"
df_join %>%
  group_by(name, PROVINCIA) %>%
  summarise(TOT = sum(value, 
                      na.rm = TRUE)) %>%
  arrange(desc(TOT))

"B"
df_join %>%
  group_by(`Denominazione Regione`, `name`) %>%
  summarise(TOT = sum(n())) %>%
  slice_max(order_by = name, n = 1) %>%
  arrange(desc(TOT))

"C"
df_join %>%
  arrange(desc(value))

# Nel punto 1 le Province e le Marche delle auto sono raggruppate
# Il risultato viene aggregato
# Nel punto 3 non avviene nessun raggruppamento e/o aggregazione
# Ma solo ordinato in modo decrescente basato sui valori


