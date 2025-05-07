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
> 
> # Sprawdzanie, czy dla każdego kraju w danym roku występuje przynajmniej jeden rekord
> complete_years_countries <- dane %>%
+   group_by(year, Country.name) %>%
+   summarise(record_count = n()) %>%
+   ungroup()
`summarise()` has grouped output by 'year'. You can override using the `.groups` argument.
> 
> # Sprawdzanie lat, w których brakują rekordy dla jakiegokolwiek kraju
> missing_years <- complete_years_countries %>%
+   group_by(year) %>%
+   summarise(all_countries_have_data = all(record_count > 0)) %>%
+   filter(all_countries_have_data == FALSE)
> 
> # Wyświetlanie lat, w których brakuje danych dla jakiegokolwiek kraju
> print(missing_years)
# A tibble: 0 × 2
# ℹ 2 variables: year <int>, all_countries_have_data <lgl>
> 
> library(dplyr)
> 
> # Sprawdzanie, czy dla każdego kraju w danym roku występuje przynajmniej jeden rekord
> complete_years_countries <- dane %>%
+   group_by(year, Country.name) %>%
+   summarise(record_count = n(), .groups = "drop")
> 
> # Sprawdzanie lat, w których brakują rekordy dla jakiegokolwiek kraju
> missing_years <- complete_years_countries %>%
+   group_by(year) %>%
+   summarise(all_countries_have_data = all(record_count > 0), .groups = "drop") %>%
+   filter(all_countries_have_data == FALSE)
> 
> # Wyświetlanie lat, w których brakuje danych dla jakiegokolwiek kraju
> print(missing_years)
# A tibble: 0 × 2
# ℹ 2 variables: year <int>, all_countries_have_data <lgl>
> library(dplyr)
> 
> # Zliczamy liczbę rekordów dla każdego kraju w każdym roku
> country_year_counts <- dane %>%
+   group_by(year, Country.name) %>%
+   summarise(record_count = n(), .groups = "drop")
> 
> # Liczymy, ile krajów występuje w każdym roku
> country_count_per_year <- country_year_counts %>%
+   group_by(year) %>%
+   summarise(countries_in_year = n_distinct(Country.name), .groups = "drop")
> 
> # Liczymy łączną liczbę krajów (zakładając, że liczba krajów jest stała w czasie)
> total_countries <- n_distinct(dane$Country.name)
> 
> # Sprawdzamy, w których latach występują dane dla wszystkich krajów
> years_with_all_countries <- country_count_per_year %>%
+   filter(countries_in_year == total_countries)
> 
> # Wyświetlamy wyniki
> print(years_with_all_countries)
# A tibble: 0 × 2
# ℹ 2 variables: year <int>, countries_in_year <int>
> #ile lat jest dla każdego kraju 
> country_counts <- table(dane$Country.name)
> print(country_counts)

 
> hist(country_counts)
> min(country_counts)
[1] 1
> max(country_counts)
[1] 18
> # Zliczamy liczbę lat, dla których każdy kraj ma dane
> country_year_counts <- table(dane$Country.name, dane$year)
> 
> # Liczymy, ile krajów ma dokładnie 18 lat
> countries_with_18_years <- sum(rowSums(country_year_counts == 1) == 18)
> 
> # Wyświetlamy wynik
> print(countries_with_18_years)
[1] 27
> countries_with_18_years <- sum(rowSums(country_counts == 1) == 18)
Error in rowSums(country_counts == 1) : 
  'x' must be an array of at least two dimensions

> # Zliczamy liczbę lat, dla których każdy kraj ma dane
> country_year_counts <- table(dane$Country.name, dane$year)
> 
> # Liczymy, ile krajów ma dokładnie 18 lat
> countries_with_18_years <- sum(rowSums(country_year_counts == 1) == 18)
> 
> # Wyświetlamy wynik
> print(countries_with_18_years)
[1] 27
> country_year_counts <- table(dane$Country.name, dane$year)
> 
> # Liczymy, ile krajów ma dokładnie 18 lat
> countries_with_18_years <- sum(rowSums(country_year_counts == 1) >14 )
> 
> # Wyświetlamy wynik
> print(countries_with_18_years)
[1] 93
> # Tworzymy tabelę, która pokazuje liczbę wystąpień krajów w każdym roku
> yearly_country_counts <- table(dane$year)
> 
> # Tworzymy data frame z latami i liczbą krajów w danym roku
> yearly_country_df <- data.frame(year = as.numeric(names(yearly_country_counts)), 
+                                 country_count = as.numeric(yearly_country_counts))
> 
> # Wyświetlamy wynik
> print(yearly_country_df)
#ile państw ma rekordy w danych latach
# #   year country_count
# 1  2005             1
# 2  2006            74
# 3  2007            91
# 4  2008           100
# 5  2009           105
# 6  2010           111
# 7  2011           132
# 8  2012           121
# 9  2013           124
# 10 2014           129
# 11 2015           128
# 12 2016           125
# 13 2017           132
# 14 2018           129
# 15 2019           129
# 16 2020           105
# 17 2021           114
# 18 2022           127
# 19 2023           120
