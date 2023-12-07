# Esercizio di data pivoting

# Carico la libreria di tidyverse
library(tidyverse)

# Carico il dataset
pop <- read.csv("/Users/Daniele/Desktop/tdo/dataset/population-and-demography.csv")

# Trasformo da "wide" a "long" le colonne che corrispondono alle varie fasce di età
pop1 <- pop %>%
  pivot_longer(cols = 4:22) %>%
  view()

# Ora provo a trasformare da "long" a "wide" i dati appena trasformati
pop2 <- pop1 %>%
  pivot_wider(id_cols = 1:2,
              names_from = "name",
              values_from = "value") %>%
  view()

# Esercizi su pivoting

uk <- read_csv("/Users/Daniele/Desktop/tdo/dataset/UK Gender Pay Gap Data - 2022 to 2023.csv")

# Convertiamo da wider a longer le variabili che contengono quartili. 

uk1 <- uk %>%
  pivot_longer(cols = 13:20) %>%
  view()

# Oppure

uk2 <- uk %>%
  pivot_longer(cols = c("MaleLowerQuartile", "FemaleLowerQuartile")) %>%
  view()

# Oppure

#ends_with, start_with e contains, 
#permettono di selezionare le variabili che finiscono, iniziano o contengono 
#un determinato valore. 

uk3 <- uk %>%
  pivot_longer(cols = ends_with("Quartile")) %>%
  view()

uk4 <- uk %>%
  pivot_longer(cols = starts_with( c("Male", "Female"))) %>%
  select(1,2, "name", "value") %>%
  view()

uk5 <- uk %>%
  pivot_longer(cols = contains( c("Male", "Female"))) %>%
  select(1,2, "name", "value") %>%
  view()

