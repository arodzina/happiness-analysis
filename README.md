# 📊 Modelowanie Dobrostanu Subiektywnego na podstawie World Happiness Report (R / Ekonometria)

**Happiness Analysis** to zaawansowany projekt z zakresu **analizy danych i ekonometrii** zrealizowany w języku **R**. Celem projektu jest zbudowanie i weryfikacja modelu regresji wielorakiej objaśniającego wskaźnik subiektywnego zadowolenia z życia (*Life Ladder*) na podstawie zmiennych społeczno-ekonomicznych pochodzących z corocznego raportu **World Happiness Report (2013–2023)**.

Projekt prezentuje pełny proces analityczny — od czyszczenia danych panelowych, przez zaawansowany dobór zmiennych objaśniających metodą Hellwiga i weryfikację założeń KMNK (diagnostyka ekonometryczna), aż po analizę różnic regionalnych i ewaluację prognostyczną na zbiorze testowym.

---

## 🌟 Główne Elementy i Metodologia

### 1. 🧹 Przygotowanie i Filtrowanie Danych
* Filtracja i ujednolicenie obserwacji dla 109 krajów posiadających pełny zestaw danych w latach **2013–2023**.
* Usunięcie braków danych i standaryzacja struktur (dane panelowe/przekrojowe).

### 2. 🔍 Dobór Zmiennych Objaśniających (Metoda Hellwiga)
* Zastosowanie **metody pojemności informacyjnej Hellwiga** do wyłonienia optymalnej kombinacji predyktorów o najwyższej wartości wyznacznika $H$.
* Analiza macierzy korelacji oraz likwidacja problemu współliniowości (VIF).

### 3. 📐 Diagnostyka Ekonometryczna Modelu (KMNK)
Weryfikacja założeń Klasycznej Metody Najmniejszych Kwadratów przy użyciu testów statystycznych:
* **Liniowość i specyfikacja funkcji**: Test RESET (Ramseya).
* **Współliniowość**: Wskaźnik VIF (Variance Inflation Factor).
* **Normalność reszt**: Test Shapiro-Wilka, skośność, kurtoza, wykresy Q-Q.
* **Heteroskedastyczność**: Test Breuscha-Pagana (`bptest`) oraz test Goldfelda-Quandta (`gqtest`).
* **Autokorelacja reszt**: Test Durbina-Watsona (`dwtest`) oraz test serii (`runs.test`).

### 4. 🌍 Analiza Wpływu Regionu i Efektów Czasowych
* Kategoryzacja państw według regionów geograficznych przy użyciu biblioteki `countrycode`.
* Analiza wariancji (ANOVA) oraz włączenie zmiennych sztucznych (*dummy variables*) dla regionów oraz lat.

### 5. 🎯 Walidacja i Prognozowanie (Train / Test Split)
* Podział danych na zbiór treningowy (90%) i testowy (10%).
* Ewaluacja predykcyjna na podstawie metryk: **MAE**, **RMSE**, **MAPE** oraz ocena trafności przedziałów predykcyjnych (*Hit Rate*).

---

## 🛠️ Technologie i Biblioteki R

* **Język**: R (RStudio)
* **Raportowanie**: RMarkdown (`sprawozdanie.Rmd`), HTML / PDF render
* **Manipulacja i Wizualizacja Danych**: `tidyverse` (`dplyr`, `ggplot2`), `reshape2`, `gridExtra`
* **Analiza i Diagnostyka Ekonometryczna**: `lmtest`, `car`, `sandwich`, `strucchange`, `moments`, `psych`, `e1071`, `randtests`
* **Uzupełnienia Geograficzne & ML**: `countrycode`, `caret`

---

## 📈 Kluczowe Wnioski z Analizy

1. **Główne determinanty szczęścia**: PKB na osobę (*Log GDP per capita*), wsparcie społeczne (*Social support*) oraz oczekiwana długość życia w zdrowiu (*Healthy life expectancy*) wykazują najsilniejszą dodatnią korelację ze wskaźnikiem *Life Ladder*.
2. **Istotność Percepcji Korupcji**: Poziom korupcji w rządzie/biznesie ujemnie wpływa na subiektywne zadowolenie z życia.
3. **Efekt Geograficzny**: Włączenie zmiennych regionalnych znacząco podnosi zdolność objaśniającą modelu ($R^2$), wskazując na istotne różnice w bazowym poziomie dobrostanu między regionami (np. kraje skandynawskie vs. Afryka Subsaharyjska).

---

## 📁 Struktura Repozytorium

```text
happiness-analysis/
├── World Happiness Report 2024.csv # Zbiór danych (dane źródłowe 2013-2023)
├── projekt.R                        # Skrypt R z pełną analizą i kodem źródłowym
├── sprawozdanie.Rmd                # Raport analityczny w formacie RMarkdown
├── sprawozdanie.html               # Wygenerowany raport gotowy do przeglądania
├── summary_table.html              # Podsumowanie tabelaryczne modeli
├── ekonometria_projekt.Rproj       # Plik projektu RStudio
└── README.md                       # Opis projektu
