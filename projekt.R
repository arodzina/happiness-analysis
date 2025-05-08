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
dane <- na.omit(dane)  # Usuwa wiersze z brakującymi wartościami


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
min(country_counts) 

max(country_counts)
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
