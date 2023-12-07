# Esercizio Joins
library(tidyverse)
gini <- read_csv("/Users/danieleiacobone/Desktop/tdo/dataset/API_SI.POV.GINI_DS2_en_csv_v2_4651513.csv")
mil <- read_csv("/Users/danieleiacobone/Desktop/tdo/dataset/military-expenditure-share-gdp.csv")

view(mil)
view(gini)

# Convertiamo da long a wide
mil2 <- mil %>% 
  pivot_wider(names_from = Year,
              values_from = military_expenditure_share_gdp)

sort(unique(mil$Year))

# Selezioniamo solo alcuni anni per semplificare
mil2 <- select(mil2, 1:2, `2018`:`2020`)
gini2 <- select(gini, 1:2, `2018`:`2020`)

# Eseguiamo la inner join con il nome nazione
gini2 %>% 
  inner_join(mil2, by = c("Country Name" = "Entity"),
             suffix = c(".gini", ".mil"))

# Eseguiamo la inner join con il codice nazione
inner <- gini2 %>%
  inner_join(mil2, by = c("Country Code" = "Code"))

# Eseguiamo la full join, dato che viene mantenuta una sola chiave, 
# si può usare parametro keep = TRUE

full <- gini2 %>%
  full_join(mil2, by = c("Country Code" = "Code"),
            keep = TRUE) %>%
  view()

# Per vedere i codici che non sono presenti nel primo 
#ma sono nel secondo dataset

full %>%
  filter((!is.na(`Country Name`)) & (is.na(Entity))) %>%
  select(`Country Name`) %>%
  view()

full %>%
  filter((is.na(`Country Name`)) & (!is.na(Entity))) %>%
  select(Entity) %>%
  view()

# Cambiamo i nomi dei non match

mil2$Entity <- str_replace(mil2$Entity, "Brunei", "Brunei Darussalam")

# In alternativa si può usare anti_join
gini2 %>%
  anti_join(mil2, by = join_by(`Country Code` == `Code`)) %>%
  view()

mil2 %>%
  anti_join(gini2, by = join_by(Code == `Country Code`))

# L'anti join restituisce le righe che non hanno ottenuto il match positivo
# Restituisce solo le colonne del primo dataset per cui non c'è match 
# Nel primo caso i non match di gini2, nel secondo caso i non match di mil2










