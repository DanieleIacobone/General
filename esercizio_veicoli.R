# Esercizio autoveicoli

auto <- read_csv2("/Users/danieleiacobone/Desktop/tdo/dataset/14-MIMS-provincia-settembre_633eeb3a566ce.csv")
str(auto)
colSums(is.na(auto))

auto %>%
  filter(is.na(`PROVINCIA DI RESIDENZA`))

read_csv2("Desktop/tdo/dataset/Codici-statistici-e-denominazioni-al-30_06_2021.csv")  

# C'è un errore nella codifica del set di caratteri

codici <- read_csv2("Desktop/tdo/dataset/Codici-statistici-e-denominazioni-al-30_06_2021.csv",
          locale = locale(encoding = "ISO-8859-1"))

# Ora viene letto correttamente

str(codici)
colSums(is.na(codici))
view(codici)

colnames(codici)
codici <- codici %>%
  rename(PROVINCIA = "Denominazione dell'Unità territoriale sovracomunale (valida a fini statistici)")

unique(codici$PROVINCIA)








