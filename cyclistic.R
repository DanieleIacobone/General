# Cyclistic Analysis 

library(tidyverse)

# Carico il dataset di Gennaio e lo analizzo
gen <- read_csv("analysis_projects/Cyclistic/dataset/gennaio.csv")
str(gen)
colSums(is.na(gen))
summary(gen)
head(gen)
names(gen)
unique(month(gen$started_at))

# Eseguo le stesse operazioni sugli altri datasets

feb <- read_csv("analysis_projects/Cyclistic/dataset/febbraio.csv")
str(feb)
colSums(is.na(feb))
summary(feb)
head(feb)
names(feb)
unique(month(feb$started_at))

mar <- read_csv("analysis_projects/Cyclistic/dataset/marzo.csv")
str(mar)
colSums(is.na(mar))
summary(mar)
head(mar)
names(mar)
unique(month(mar$started_at))

apr <- read_csv("analysis_projects/Cyclistic/dataset/aprile.csv")
str(apr)
colSums(is.na(apr))
summary(apr)
head(apr)
names(apr)
unique(month(apr$started_at))

# Unisco i quattro dataset in uno unico e lo analizzo

firstq <- bind_rows(gen, feb, mar, apr)
names(firstq)
summary(firstq)
colSums(is.na(firstq))
unique(firstq$member_casual)
unique(firstq$rideable_type)

# Aggiungo colonne che identificano giorno, mese, anno

firstq2 <- firstq %>%
  mutate(Day = day(started_at),
         Month = month(started_at),
         Year = year(started_at),
         Weekday = weekdays(started_at))

# Sposto le colonne appena create dopo la variabile "started_at"

firstq2 <- firstq2 %>%
  relocate(Day, .before = started_at) %>%
  relocate(Month, .after = Day) %>%
  relocate(Year, .after = Month) %>%
  relocate(Weekday, .before = Day)

# Ora calcolo la variabile ride lenght come differenza tra ended_at e started_at

firstq2 <- firstq2 %>%
  mutate(ride_length = as.numeric(difftime(ended_at, started_at)))

# Il risultato è in secondi, ma è stato convertito in numeric per fare analisi
# Ci sono sia valori negativi, che valori molto alti 
# Controlliamo i valori NA nella colonna ride length

sum(is.na(firstq2$ride_length))

# Non sono presenti
# Per l'analisi non consideriamo i valori negativi in ride_length

f3 <- firstq2 %>% 
  filter(ride_length > 0)

# Analisi con statistica descrittiva di ride_length

summary(f3$ride_length)




