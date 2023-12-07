library(tidyverse)

#######################################################################
#
# Dati: IMMATRICOLAZIONE AUTOVEICOLI (Settembre 2022, dati MINISTERO DELLE INFRASTRUTTURE E DELLA MOBILITA' SOSTENIBILI)
# Dataset: 14-MIMS-provincia-settembre_633eeb3a566ce.csv (d. Min_Mobilita)

auto=read_csv2("Desktop/tdo/dataset/14-MIMS-provincia-settembre_633eeb3a566ce.csv")
head(auto)

# ANALISI VALORI MANCANTI E TIPO DI DATI

str(auto) # i tipi di dato sono coerenti

colSums(is.na(auto)) # ci sono diversi valori mancanti, per tutte le colonne

# Vediamo quali sono le righe con NA nella provincia di residenza

filter(auto, is.na(`PROVINCIA DI RESIDENZA`)) %>% view()

# Queste sono righe vuote, possiamo eliminarle

auto=filter(auto, !is.na(`PROVINCIA DI RESIDENZA`))
auto

#######################################################################
#
# Dati: CODICI ENTITÀ TERRITORIALI 
# dataset: Codici-statistici-e-denominazioni-al-30_06_2021.csv (d. ISTAT)

read_csv2("Desktop/tdo/dataset/Codici-statistici-e-denominazioni-al-30_06_2021.csv")

# produce un errore non immediatamente interpretabile: Error in nchar(x, "width") : invalid multibyte string, element 1
# probabile un errore dovuto alla codifica del set di caratteri
# provando con il comando file, ci dice: Non-ISO extended-ASCII text, with very long lines, with CRLF line terminators
# cioè non riesce a dedurre il tipo di codifica
# proviamo con ISO-8859-1 che è comune in Europa

codici=read_csv2("Desktop/tdo/dataset/Codici-statistici-e-denominazioni-al-30_06_2021.csv", 
          locale=locale(encoding = "ISO-8859-1"))
codici   # OK, lo legge correttamente

str(codici)     # i tipi di dato sono coerenti
colSums(is.na(codici))   # non ci sono NA

##########################################################################
"
Vogliamo aggiungere al data frame auto le colonne su Regione (nome e codice) e Provincia (codice).
Possiamo fare una join con il nome della provincia.

Data frame auto:
Chiave: PROVINCIA DI RESIDENZA (sono tutte maiuscole)

Data frame codici: 
Chiave: `Denominazione dell'Unità territoriale sovracomunale (valida a fini statistici)` (prima lettera maiuscola)
"
# Rinominiamo le colonne delle province e codici provincia

codici = rename(codici, PROVINCIA=`Denominazione dell'Unità territoriale sovracomunale (valida a fini statistici)`,
                COD_PROV=`Codice dell'Unità territoriale sovracomunale (valida a fini statistici)`)
names(codici) 
codici

# Prima controlliamo quante sono le province nei due data frame

unique(auto$`PROVINCIA DI RESIDENZA`) # 108

codici%>%group_by(PROVINCIA)  # 107

# In auto c'è un valore in più, dovremo trovare qual è

codici%>%
  select(`Codice Regione`, COD_PROV, `Denominazione Regione`, PROVINCIA) %>%
  group_by(`Denominazione Regione`,`PROVINCIA`) %>% 
  summarize(Num=n()) -> codici_PR
  
#filter(!duplicated(.)) -> codici_PR
view(codici_PR)

# Proviamo a eseguire la join  

auto %>% inner_join(codici_PR, by = c("PROVINCIA DI RESIDENZA" = "PROVINCIA"))

# zero righe, i valori con le maiuscole diverse vengono interpretati come diversi
# dobbiamo portarli a una forma comune --> portiamo a tutte maiuscole i valori di PROVINCIA

codici_PR=mutate(codici_PR, PROVINCIA= toupper(PROVINCIA))
codici_PR

auto %>% 
  inner_join(codici_PR, by = c("PROVINCIA DI RESIDENZA" = "PROVINCIA")) %>% 
  view()

# sono 98 righe, erano 107 quelle di partenza di codici, quindi abbiamo 9 valori che non hanno corrispondenza con i 
# corrispondenti di auto
# troviamoli

#
#  FULL-JOIN

# Usiamo l'opzione keep=TRUE per conservare entrambe le colonne delle chiavi, ci serviranno
auto %>% full_join(codici_PR, by = c("PROVINCIA DI RESIDENZA" = "PROVINCIA"), keep=TRUE) ->full
names(full)  # sono 116 righe


# Spostiamo in testa le colonne che ci interessano per lavorare meglio

full=relocate(full, c(`Denominazione Regione`,PROVINCIA), .after = `PROVINCIA DI RESIDENZA`)
full

# Ora possiamo vedere bene i valori che non hanno avuto un match su entrambe le chiavi
# Ad esempio, AOSTA ha NA in PROVINCIA e nelle colonne delle entità territoriali, quindi vuol dire che non c'è in quel data frame
# Viceversa, ci saranno righe con NA in `PROVINCIA DI RESIDENZA` e saranno quelle con un valore in PROVINCIA che non compare 
# nel data frame delle auto.

# Abbiamo i nostri due criteri per ottenere gli elenchi di chiavi che non hanno avuto un match.

filter(full, is.na(PROVINCIA)) %>% select(1)

"
Questi i nomi di province del df auto che non compaiono nelle entità territoriali
# A tibble: 9 × 1
`PROVINCIA DI RESIDENZA`
<chr>                   
  1 AOSTA                   
2 ASCOLIPICENO            
3 BARLETTA-ANDRIATRANI    
4 BOLZANO-BOZEN           
5 FORLI' CESENA           
6 PESARO URBINO           
7 REGGIO DI CALABRIA      
8 REGGIO EMILIA           
9 VERBANIA   
"

filter(full, is.na(`PROVINCIA DI RESIDENZA`)) %>% select(3)

"
Questi i nomi di province del df codici_PR che non compaiono nelle auto
# A tibble: 9 × 1
  PROVINCIA                   
  <chr>                       
1 VERBANO-CUSIO-OSSOLA        
2 VALLE D'AOSTA/VALLÉE D'AOSTE
3 BOLZANO/BOZEN               
4 REGGIO NELL'EMILIA          
5 FORLÌ-CESENA                
6 PESARO E URBINO             
7 ASCOLI PICENO               
8 BARLETTA-ANDRIA-TRANI       
9 REGGIO CALABRIA 
"
# Consideriamo corretto l'elenco di ISTAT e modifichiamo i nomi nel df auto.
# Qui si procede manualmente.

auto$`PROVINCIA DI RESIDENZA` = str_replace_all(auto$`PROVINCIA DI RESIDENZA`, 
                c("AOSTA" = "VALLE D'AOSTA/VALLÉE D'AOSTE",
                  "ASCOLIPICENO" = "ASCOLI PICENO",
                  "BARLETTA-ANDRIATRANI" = "BARLETTA-ANDRIA-TRANI",
                  "BOLZANO-BOZEN" = "BOLZANO/BOZEN",
                  "FORLI' CESENA" = "FORLÌ-CESENA",
                  "PESARO URBINO" = "PESARO E URBINO",
                  "REGGIO DI CALABRIA" = "REGGIO CALABRIA",
                  "REGGIO EMILIA" = "REGGIO NELL'EMILIA",
                  "VERBANIA" = "VERBANO-CUSIO-OSSOLA")
                )

# rifacciamo la join

auto %>% inner_join(codici_PR, by = c("PROVINCIA DI RESIDENZA" = "PROVINCIA")) -> auto2
auto2

# Bene, sono 107 quindi abbiamo creato tutte le corrispondenze.

#########################################################################
#
# con ANTI-JOIN

auto %>% anti_join(codici_PR, by = join_by(`PROVINCIA DI RESIDENZA` == PROVINCIA)) %>%
  select(1)

codici_PR %>% anti_join(auto, by = join_by(PROVINCIA == `PROVINCIA DI RESIDENZA`)) %>%
  select(1,2)


#########################################################################
#
# portiamo in formato long, esclusa la colonna Totale da eliminare dal risultato

auto2 %>% pivot_longer(cols = "Alfa Romeo":"Altre", names_to = "Marche", values_to = "Immatricolate") %>% 
  select(-Totale) -> auto2_long
auto2_long

########################################################################
#
# OPERAZIONI DI AGGREGAZIONE

######## PER REGIONE

# Totale immatricolate per regione per marca
auto2_long %>% group_by(`Denominazione Regione`, Marche) %>% 
  summarize(Tot_Imm=sum(Immatricolate, na.rm = TRUE)) 

# Totale immatricolate per regione per marca ordinate decrescenti mantenendo i gruppi
auto2_long %>% group_by(`Denominazione Regione`, Marche) %>% 
  summarize(Tot_Imm=sum(Immatricolate), na.rm = TRUE) %>% 
  arrange(desc(Tot_Imm), .by_group = TRUE)

# Totale immatricolate per regione per marca selezionando le prime tre per numero di immatricolazioni per ogni regione
auto2_long %>% group_by(`Denominazione Regione`, Marche) %>% 
  summarize(Tot_Imm=sum(Immatricolate), na.rm = TRUE) %>% 
  slice_max(order_by = Tot_Imm, n=3) %>% 
  arrange(desc(Tot_Imm))

# Dal risultato precedente, ordinare per valori su tutte le regioni e selezionare le prime 10
auto2_long %>% group_by(`Denominazione Regione`, Marche) %>% 
  summarize(Tot_Imm=sum(Immatricolate), na.rm = TRUE) %>% 
  slice_max(order_by = Tot_Imm, n=3) %>% 
  arrange(desc(Tot_Imm)) %>%
  head(10)

########## RIFARE TUTTE LE AGGREGAZIONI PER PROVINCIA

# Totale immatricolate per provincia per marca
auto2_long %>% group_by(`PROVINCIA DI RESIDENZA`, Marche) %>% 
  summarize(Tot_Imm=sum(Immatricolate), na.rm = TRUE) 

# Totale immatricolate per provincia per marca ordinate decrescenti mantenendo i gruppi
auto2_long %>% group_by(`PROVINCIA DI RESIDENZA`, Marche) %>% 
  summarize(Tot_Imm=sum(Immatricolate), na.rm = TRUE) %>% 
  arrange(desc(Tot_Imm), .by_group = TRUE)

# Totale immatricolate per provincia per marca selezionando le prime tre per numero di immatricolazioni per ogni regione
auto2_long %>% group_by(`PROVINCIA DI RESIDENZA`, Marche) %>% 
  summarize(Tot_Imm=sum(Immatricolate), na.rm = TRUE) %>% 
  slice_max(order_by = Tot_Imm, n=3) %>% 
  arrange(desc(Tot_Imm))

# Dal risultato precedente, ordinare per valori su tutte le regioni e selezionare le prime 10
auto2_long %>% group_by(`PROVINCIA DI RESIDENZA`, Marche) %>% 
  summarize(Tot_Imm=sum(Immatricolate), na.rm = TRUE) %>% 
  slice_max(order_by = Tot_Imm, n=3) %>% 
  arrange(desc(Tot_Imm)) %>%
  head(10)

########## PER MARCA

# per ogni marca trovare la regione dove le immatricolazioni sono in numero maggiore
auto2_long %>% group_by(Marche,`Denominazione Regione`) %>% 
  summarize(Tot_Imm=sum(Immatricolate), na.rm = TRUE) %>% 
  slice_max(order_by = Tot_Imm, n=1) 

# per ogni marca trovare la regione dove le immatricolazioni sono in numero minore
auto2_long %>% group_by(Marche,`Denominazione Regione`) %>% 
  summarize(Tot_Imm=sum(Immatricolate), na.rm = TRUE) %>% 
  slice_min(order_by = Tot_Imm, n=1) 

# per ogni marca trovare la provincia dove le immatricolazioni sono in numero maggiore
auto2_long %>% group_by(Marche,`PROVINCIA DI RESIDENZA`) %>% 
  summarize(Tot_Imm=sum(Immatricolate), na.rm = TRUE) %>% 
  slice_max(order_by = Tot_Imm, n=1) 

# per ogni marca trovare la provincia dove le immatricolazioni sono in numero minore
auto2_long %>% group_by(Marche,`PROVINCIA DI RESIDENZA`) %>% 
  summarize(Tot_Imm=sum(Immatricolate), na.rm = TRUE) %>% 
  slice_min(order_by = Tot_Imm, n=1) 

#############################################################################
