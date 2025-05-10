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


library(dplyr)

# Zakres lat
lata_docelowe <- 2013:2023

# Filtrowanie danych do wybranych lat
dane_filtered <- dane %>%
  filter(year %in% lata_docelowe)

# Wyszukiwanie krajów z danymi we wszystkich 6 latach
kraje_pelne_dane <- dane_filtered %>%
  group_by(Country.name) %>%
  summarise(liczba_lat = n_distinct(year)) %>%
  filter(liczba_lat == length(lata_docelowe))

# Lista krajów z pełnymi danymi
kraje_z_danymi <- kraje_pelne_dane$Country.name

# Ostateczne dane tylko dla tych krajów
dane_finalne <- dane_filtered %>%
  filter(Country.name %in% kraje_z_danymi)

# Wyświetlenie liczby krajów i przykładowych danych
cat("Liczba krajów z pełnymi danymi (2018–2023):", length(kraje_z_danymi), "\n")
print(dane_finalne)


# Tworzenie nowej tabeli tylko z danymi z 2023 roku
dane_2023 <- dane %>%
  filter(year == 2023)
dane_2023 <- dane_2023 %>% select(-year)
#korelacje 
library(corrplot)

num_data <- dane_2023 %>%
  select(where(is.numeric))  # tylko kolumny liczbowe
cor_matrix <- cor(num_data, use = "complete.obs")  # ignoruje NA
corrplot(cor_matrix, method = "color", 
         tl.col = "black", tl.cex = 0.8, number.cex = 0.7,
         addCoef.col = "black", order = "hclust")
#z macierzy korelacji wynika, że Generosity nie wpływa na nic. PKB, długośc życia, drabina życiowa i social support są silnie zależne od siebie. Nevative effect ma mocniejsze korelacje z pozostałymi zmiennymi niż positive effect. Positive effect ma prawie wszystkie dodatnie korelacje a negative ujemne

#modele dla 2023
#pozytywny efekt
model<-lm(Positive.affect~.-Country.name-year-Negative.affect,dane_2023)
summary(model)
model<-lm(Positive.affect~.-Country.name-Negative.affect-Perceptions.of.corruption-Social.support-Log.GDP.per.capita-Generosity,dane_2023)
summary(model)

#negatywny efekt
model<-lm(Negative.affect~.-Country.name-year-Positive.affect,dane_2023)
summary(model)

model<-lm(Negative.affect~.-Country.name-year-Positive.affect-Life.Ladder-Generosity-Perceptions.of.corruption,dane_2023)
summary(model)

#rok 2013
dane_2013 <- dane_2013 %>% select(-year)
#korelacje 

dane_2013 <- dane %>%
  filter(year == 2013)
dane_2013 <- dane_2013 %>% select(-year)
num_data <- dane_2013 %>%
  select(where(is.numeric))  # tylko kolumny liczbowe

cor_matrix <- cor(num_data, use = "complete.obs")  # ignoruje NA
corrplot(cor_matrix, method = "color", 
         tl.col = "black", tl.cex = 0.8, number.cex = 0.7,
         addCoef.col = "black", order = "hclust")

model<-lm(Positive.affect~.-Country.name-Negative.affect,dane_2013)
summary(model)

model<-lm(Positive.affect~.-Country.name-Negative.affect-Perceptions.of.corruption -Generosity -Healthy.life.expectancy.at.birth,dane_2013)
summary(model)
#negatywny efekt
model<-lm(Negative.affect~.-Country.name-year-Positive.affect,dane_2023)
summary(model)

model<-lm(Freedom.to.make.life.choices~.-Country.name-Life.Ladder-Negative.affect-Log.GDP.per.capita-Healthy.life.expectancy.at.birth,dane_2013)
summary(model)




#scatter plot 
# Załaduj bibliotekę ggplot2
library(ggplot2)

# Wykres
ggplot(dane, aes(x = `Log.GDP.per.capita`, y = `Life.Ladder`, color = factor(year))) +
  geom_point(alpha = 0.7) +
  scale_color_viridis_d() + # Używa palety 'viridis' dla kolorów
  labs(title = 'Log GDP per Capita vs. Life Ladder', 
       x = 'Log GDP per Capita', 
       y = 'Life Ladder') +
  theme_minimal() +
  theme(panel.grid.major = element_line(linetype = "dashed", color = "grey")) +
  theme(legend.title = element_blank()) # Opcjonalnie, usuwa tytuł legendy


###################

# Zakres lat
lata_docelowe <- 2006:2023

# Filtrowanie danych do tych lat
dane_filtered <- dane %>%
  filter(year %in% lata_docelowe)

# Znajdź kraje, które mają dane dla wszystkich tych lat
kraje_kompletne <- dane_filtered %>%
  group_by(Country.name) %>%
  summarise(liczba_lat = n_distinct(year)) %>%
  filter(liczba_lat == length(lata_docelowe)) %>%
  pull(Country.name)

# Losowo wybieramy 15 krajów z tych, które mają pełne dane
set.seed(123)  # dla powtarzalności
wybrane_kraje <- sample(kraje_kompletne, 20)

# Filtrowanie danych dla wybranych krajów
dane_15_krajow <- dane_filtered %>%
  filter(Country.name %in% wybrane_kraje)

# Wykres zmian Life Ladder
ggplot(dane_15_krajow, aes(x = year, y = Life.Ladder, color = Country.name)) +
  geom_line() +
  geom_point(size = 1.5) +
  labs(title = "Zmiany Life Ladder (2013–2023)",
       x = "Rok", 
       y = "Life Ladder") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  guides(color = guide_legend(ncol = 4))

ggplot(dane_15_krajow, aes(x = year, y = Life.Ladder)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue") +
  facet_wrap(~ Country.name, ncol = 3) +
  labs(title = "Zmiany Life Ladder (2013–2023)",
       x = "Rok", y = "Life Ladder") +
  theme_minimal() +
  theme(strip.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1))


library(ggplot2)
library(dplyr)

# 1. Oblicz rozstęp
rozstepy <- dane %>%
  group_by(Country.name) %>%
  summarise(rozstep_LL = max(Life.Ladder, na.rm = TRUE) - min(Life.Ladder, na.rm = TRUE)) %>%
  arrange(desc(rozstep_LL)) %>%
  slice_head(n = 6)

# 2. Dane dla top 6 krajów
top6_kraje <- rozstepy$Country.name
dane_top6 <- dane %>%
  filter(Country.name %in% top6_kraje)

# 3. Dołączamy wartości rozstępu do danych
dane_top6 <- dane_top6 %>%
  left_join(rozstepy, by = "Country.name")

# 4. Tworzenie wykresu z tekstem
ggplot(dane_top6, aes(x = year, y = Life.Ladder)) +
  geom_line(color = "darkorange", size = 1) +
  geom_point(color = "darkorange", size = 2) +
  facet_wrap(~ Country.name, ncol = 3, scales = "free_y") +
  labs(title = "6 krajów z największą zmiennością Life Ladder (w czasie)",
       x = "Rok", y = "Life Ladder") +
  theme_minimal(base_size = 13) +
  theme(strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(data = . %>% group_by(Country.name) %>% summarise(x = max(year), y = max(Life.Ladder, na.rm = TRUE), 
                                                              label = paste0("Rozstęp: ", round(unique(rozstep_LL), 2))),
            aes(x = x, y = y, label = label),
            inherit.aes = FALSE, hjust = 1.1, vjust = 1.1, size = 3.5, color = "black")


# 1. Oblicz rozstęp
rozstepy2 <- dane %>%
  group_by(Country.name) %>%
  summarise(rozstep_LL = max(Life.Ladder, na.rm = TRUE) - min(Life.Ladder, na.rm = TRUE)) %>%
  arrange(asc(rozstep_LL)) %>%
  slice_head(n = 6)

# 2. Dane dla top 6 krajów
top6_kraje <- rozstepy$Country.name
dane_top6 <- dane %>%
  filter(Country.name %in% top6_kraje)

# 3. Dołączamy wartości rozstępu do danych
dane_top6 <- dane_top6 %>%
  left_join(rozstepy, by = "Country.name")

# 4. Tworzenie wykresu z tekstem
ggplot(dane_top6, aes(x = year, y = Life.Ladder)) +
  geom_line(color = "darkorange", size = 1) +
  geom_point(color = "darkorange", size = 2) +
  facet_wrap(~ Country.name, ncol = 3, scales = "free_y") +
  labs(title = "6 krajów z największą zmiennością Life Ladder (w czasie)",
       x = "Rok", y = "Life Ladder") +
  theme_minimal(base_size = 13) +
  theme(strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(data = . %>% group_by(Country.name) %>% summarise(x = max(year), y = max(Life.Ladder, na.rm = TRUE), 
                                                              label = paste0("Rozstęp: ", round(unique(rozstep_LL), 2))),
            aes(x = x, y = y, label = label),
            inherit.aes = FALSE, hjust = 1.1, vjust = 1.1, size = 3.5, color = "black")


sredni_rozstep <- dane %>%
  group_by(Country.name) %>%
  summarise(rozstep = max(Life.Ladder, na.rm = TRUE) - min(Life.Ladder, na.rm = TRUE)) %>%
  summarise(sredni_rozstep = mean(rozstep, na.rm = TRUE))

print(sredni_rozstep)
median_rozstep <- dane %>%
  group_by(Country.name) %>%
  summarise(rozstep = max(Life.Ladder, na.rm = TRUE) - min(Life.Ladder, na.rm = TRUE)) %>%
  summarise(median_rozstep = median(rozstep, na.rm = TRUE))

print(median_rozstep)


library(ggplot2)
library(dplyr)

# Dane z 2023 roku
dane_2023 <- dane %>%
  filter(year == 2013)

# Sortowanie
dane_2023_sorted <- dane_2023 %>%
  arrange(Life.Ladder)

# Wykres punktowy
ggplot(dane_2023_sorted, aes(x = Life.Ladder, y = reorder(Country.name, Life.Ladder))) +
  geom_point(color = "darkblue", size = 2) +
  labs(title = "Life Ladder w krajach w 2023 roku",
       x = "Life Ladder",
       y = "Kraj") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 7))


library(ggplot2)
library(dplyr)

# Filtrowanie danych dla 2013 i 2023
dane_2013_2023 <- dane %>%
  filter(year %in% c(2013, 2023)) %>%
  group_by(Country.name) %>%
  summarise(
    zmiana_life_ladder = Life.Ladder[year == 2023] - Life.Ladder[year == 2013],
    .groups = "drop"
  )

# Wykres 1: Zmiana na plus
dane_plus <- dane_2013_2023 %>%
  filter(zmiana_life_ladder > 0) %>%
  arrange(desc(zmiana_life_ladder))

ggplot(dane_plus, aes(x = reorder(Country.name, zmiana_life_ladder), y = zmiana_life_ladder)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +  # Odwrócenie osi
  labs(title = "Zmiany Life Ladder na plus (2013-2023)",
       x = "Kraj",
       y = "Zmiana Life Ladder") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))

# Wykres 2: Zmiana na minus
dane_minus <- dane_2013_2023 %>%
  filter(zmiana_life_ladder < 0) %>%
  arrange(desc(zmiana_life_ladder))

ggplot(dane_minus, aes(x = reorder(Country.name, zmiana_life_ladder), y = zmiana_life_ladder)) +
  geom_col(fill = "darkred") +
  coord_flip() +  # Odwrócenie osi
  labs(title = "Zmiany Life Ladder na minus (2013-2023)",
       x = "Kraj",
       y = "Zmiana Life Ladder") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))


