# Wczytanie potrzebnych bibliotek
library(tidyverse)
library(skimr)
library(naniar)

# Wczytanie danych
dane <- read.csv("World Happiness Report 2024.csv", sep = ";")

# Podgląd danych
head(dane)

# Sprawdzenie liczby braków w każdej kolumnie
colSums(is.na(dane))
# Procent braków dla każdej kolumny
miss_var_summary(dane)
dane <- na.omit(dane)  

# Podstawowe statystyki opisowe
summary(dane)
# Statystyki rozszerzone
skim(dane)

# Liczba krajów i lat
dane %>%
  summarise(liczba_krajow = n_distinct(Country.name),
            liczba_lat = n_distinct(year))
#ile lat jest dla każdego kraju 
country_counts <- table(dane$Country.name)
print(sort(country_counts))
hist(country_counts)

# Zliczamy liczbę lat, dla których każdy kraj ma dane
country_year_counts <- table(dane$Country.name, dane$year)
# Liczymy, ile krajów ma dokładnie 18 lat
countries_with_18_years <- sum(rowSums(country_year_counts == 1) >14 )
# Wyświetlamy wynik
print(countries_with_18_years)
# Tworzymy tabelę, która pokazuje liczbę wystąpień krajów w każdym roku
yearly_country_counts <- table(dane$year)
# Tworzymy data frame z latami i liczbą krajów w danym roku
yearly_country_df <- data.frame(year = as.numeric(names(yearly_country_counts)), 
                                country_count = as.numeric(yearly_country_counts))

# Wyświetlamy wynik
print(yearly_country_df)

#wnioski: 

hist(dane$Positive.affect)
#1. w 2011 najwiecej krajów, w 2023 jest 120 krajów - regresja dla 2023 + regresja dla 2013



# Definiujemy interesujący nas zakres lat
lata_docelowe <- 2013:2023

# Filtrowanie danych tylko dla lat 2013–2023
dane_filtered <- dane %>%
  filter(year %in% lata_docelowe)

# Tworzymy tabelę: liczba wystąpień dla każdego kraju w tych latach
kraj_lata <- dane_filtered %>%
  group_by(Country.name) %>%
  summarise(liczba_lat = n_distinct(year)) %>%
  filter(liczba_lat == length(lata_docelowe))  

# Liczba takich krajów
liczba_krajow_bez_przerwy <- nrow(kraj_lata)
print(liczba_krajow_bez_przerwy)

# Tworzenie nowej tabeli tylko z danymi z 2023 roku
dane_2023 <- dane %>%
  filter(year == 2023)

# Podgląd danych
head(dane_2023)
#pozytywny efekt
model<-lm(Positive.affect~.-Country.name-year-Negative.affect,dane_2023)
summary(model)
model<-lm(Positive.affect~.-Country.name-year-Negative.affect-Perceptions.of.corruption-Social.support-Log.GDP.per.capita-Generosity,dane_2023)
summary(model)

#negatywny efekt
model<-lm(Negative.affect~.-Country.name-year-Positive.affect,dane_2023)
summary(model)

model<-lm(Negative.affect~.-Country.name-year-Positive.affect-Life.Ladder-Generosity-Perceptions.of.corruption,dane_2023)
summary(model)

