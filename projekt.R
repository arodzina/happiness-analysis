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

trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-x86_64/contrib/4.4/ggplot2_3.5.2.tgz'
Content type 'application/x-gzip' length 4969013 bytes (4.7 MB)
==================================================
downloaded 4.7 MB

trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-x86_64/contrib/4.4/googledrive_2.1.1.tgz'
Content type 'application/x-gzip' length 1895627 bytes (1.8 MB)
==================================================
downloaded 1.8 MB

trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-x86_64/contrib/4.4/googlesheets4_1.1.1.tgz'
Content type 'application/x-gzip' length 513146 bytes (501 KB)
==================================================
downloaded 501 KB

trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-x86_64/contrib/4.4/haven_2.5.4.tgz'
Content type 'application/x-gzip' length 1136558 bytes (1.1 MB)
==================================================
downloaded 1.1 MB

trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-x86_64/contrib/4.4/hms_1.1.3.tgz'
Content type 'application/x-gzip' length 100286 bytes (97 KB)
==================================================
downloaded 97 KB

trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-x86_64/contrib/4.4/httr_1.4.7.tgz'
Content type 'application/x-gzip' length 478106 bytes (466 KB)
==================================================
downloaded 466 KB

trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-x86_64/contrib/4.4/jsonlite_2.0.0.tgz'
Content type 'application/x-gzip' length 1113578 bytes (1.1 MB)
==================================================
downloaded 1.1 MB

trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-x86_64/contrib/4.4/lubridate_1.9.4.tgz'
Content type 'application/x-gzip' length 1003291 bytes (979 KB)
==================================================
downloaded 979 KB

trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-x86_64/contrib/4.4/magrittr_2.0.3.tgz'
Content type 'application/x-gzip' length 233565 bytes (228 KB)
==================================================
downloaded 228 KB

trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-x86_64/contrib/4.4/modelr_0.1.11.tgz'
Content type 'application/x-gzip' length 203542 bytes (198 KB)
==================================================
downloaded 198 KB

trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-x86_64/contrib/4.4/pillar_1.10.2.tgz'
Content type 'application/x-gzip' length 657922 bytes (642 KB)
==================================================
downloaded 642 KB

trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-x86_64/contrib/4.4/purrr_1.0.4.tgz'
Content type 'application/x-gzip' length 565355 bytes (552 KB)
==================================================
downloaded 552 KB

trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-x86_64/contrib/4.4/ragg_1.4.0.tgz'
Content type 'application/x-gzip' length 12402648 bytes (11.8 MB)
==================================================
downloaded 11.8 MB

trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-x86_64/contrib/4.4/readr_2.1.5.tgz'
Content type 'application/x-gzip' length 1971235 bytes (1.9 MB)
==================================================
downloaded 1.9 MB

trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-x86_64/contrib/4.4/readxl_1.4.5.tgz'
Content type 'application/x-gzip' length 1091122 bytes (1.0 MB)
==================================================
downloaded 1.0 MB

trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-x86_64/contrib/4.4/reprex_2.1.1.tgz'
Content type 'application/x-gzip' length 493928 bytes (482 KB)
==================================================
downloaded 482 KB

trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-x86_64/contrib/4.4/rlang_1.1.6.tgz'
Content type 'application/x-gzip' length 1905612 bytes (1.8 MB)
==================================================
downloaded 1.8 MB

trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-x86_64/contrib/4.4/rstudioapi_0.17.1.tgz'
Content type 'application/x-gzip' length 318493 bytes (311 KB)
==================================================
downloaded 311 KB

trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-x86_64/contrib/4.4/rvest_1.0.4.tgz'
Content type 'application/x-gzip' length 298705 bytes (291 KB)
==================================================
downloaded 291 KB

trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-x86_64/contrib/4.4/stringr_1.5.1.tgz'
Content type 'application/x-gzip' length 314057 bytes (306 KB)
==================================================
downloaded 306 KB

trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-x86_64/contrib/4.4/tibble_3.2.1.tgz'
Content type 'application/x-gzip' length 689307 bytes (673 KB)
==================================================
downloaded 673 KB

trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-x86_64/contrib/4.4/tidyr_1.3.1.tgz'
Content type 'application/x-gzip' length 1323724 bytes (1.3 MB)
==================================================
downloaded 1.3 MB

trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-x86_64/contrib/4.4/xml2_1.3.8.tgz'
Content type 'application/x-gzip' length 522330 bytes (510 KB)
==================================================
downloaded 510 KB

trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-x86_64/contrib/4.4/tidyverse_2.0.0.tgz'
Content type 'application/x-gzip' length 428830 bytes (418 KB)
==================================================
downloaded 418 KB


The downloaded binary packages are in
	/var/folders/pn/4x2pm1px7sb2fdyqmxvfzzzr0000gn/T//RtmpbW9SvT/downloaded_packages
> install.packages("skimr")
Installing package into ‘/Users/ola/Library/R/x86_64/4.4/library’
(as ‘lib’ is unspecified)
also installing the dependency ‘repr’
trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-x86_64/contrib/4.4/repr_1.1.7.tgz'
Content type 'application/x-gzip' length 127338 bytes (124 KB)
==================================================
downloaded 124 KB

trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-x86_64/contrib/4.4/skimr_2.1.5.tgz'
Content type 'application/x-gzip' length 1224862 bytes (1.2 MB)
==================================================
downloaded 1.2 MB


The downloaded binary packages are in
	/var/folders/pn/4x2pm1px7sb2fdyqmxvfzzzr0000gn/T//RtmpbW9SvT/downloaded_packages
> install.packages("naniar")
Installing package into ‘/Users/ola/Library/R/x86_64/4.4/library’
(as ‘lib’ is unspecified)
also installing the dependencies ‘Rcpp’, ‘gridExtra’, ‘plyr’, ‘norm’, ‘visdat’, ‘viridis’, ‘UpSetR’
trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-x86_64/contrib/4.4/Rcpp_1.0.14.tgz'
Content type 'application/x-gzip' length 3338395 bytes (3.2 MB)
==================================================
downloaded 3.2 MB

trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-x86_64/contrib/4.4/gridExtra_2.3.tgz'
Content type 'application/x-gzip' length 1105877 bytes (1.1 MB)
==================================================
downloaded 1.1 MB

trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-x86_64/contrib/4.4/plyr_1.8.9.tgz'
Content type 'application/x-gzip' length 1026008 bytes (1001 KB)
==================================================
downloaded 1001 KB

trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-x86_64/contrib/4.4/norm_1.0-11.1.tgz'
Content type 'application/x-gzip' length 111798 bytes (109 KB)
==================================================
downloaded 109 KB

trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-x86_64/contrib/4.4/visdat_0.6.0.tgz'
Content type 'application/x-gzip' length 1147020 bytes (1.1 MB)
==================================================
downloaded 1.1 MB

trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-x86_64/contrib/4.4/viridis_0.6.5.tgz'
Content type 'application/x-gzip' length 3016821 bytes (2.9 MB)
==================================================
downloaded 2.9 MB

trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-x86_64/contrib/4.4/UpSetR_1.4.0.tgz'
Content type 'application/x-gzip' length 4302682 bytes (4.1 MB)
==================================================
downloaded 4.1 MB

trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-x86_64/contrib/4.4/naniar_1.1.0.tgz'
Content type 'application/x-gzip' length 2778414 bytes (2.6 MB)
==================================================
downloaded 2.6 MB


The downloaded binary packages are in
	/var/folders/pn/4x2pm1px7sb2fdyqmxvfzzzr0000gn/T//RtmpbW9SvT/downloaded_packages
> 
> # Wczytanie potrzebnych bibliotek
> library(tidyverse)
── Attaching core tidyverse packages ────────────────────────────────────────────────────────────────────── tidyverse 2.0.0 ──
✔ dplyr     1.1.4     ✔ readr     2.1.5
✔ forcats   1.0.0     ✔ stringr   1.5.1
✔ ggplot2   3.5.2     ✔ tibble    3.2.1
✔ lubridate 1.9.4     ✔ tidyr     1.3.1
✔ purrr     1.0.4     
── Conflicts ──────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
✖ dplyr::filter() masks stats::filter()
✖ dplyr::lag()    masks stats::lag()
ℹ Use the conflicted package to force all conflicts to become errors
> library(skimr)
> library(naniar)
Attaching package: ‘naniar’

The following object is masked from ‘package:skimr’:

    n_complete
> 
> # Wczytanie danych
> dane <- read.csv("World Happiness Report 2024.csv", sep = ";")
> 
> # Podgląd danych
> head(dane)
  Country.name year Life.Ladder Log.GDP.per.capita Social.support Healthy.life.expectancy.at.birth
1  Afghanistan 2008    3.723590           7.350416      0.4506623                             50.5
2  Afghanistan 2009    4.401778           7.508646      0.5523084                             50.8
3  Afghanistan 2010    4.758381           7.613900      0.5390752                             51.1
4  Afghanistan 2011    3.831719           7.581259      0.5211036                             51.4
5  Afghanistan 2012    3.782938           7.660506      0.5206367                             51.7
6  Afghanistan 2013    3.572100           7.680333      0.4835519                             52.0
  Freedom.to.make.life.choices Generosity Perceptions.of.corruption Positive.affect Negative.affect
1                    0.7181143 0.16405511                 0.8816863       0.4142970       0.2581955
2                    0.6788964 0.18729663                 0.8500354       0.4814214       0.2370924
3                    0.6001272 0.11786054                 0.7067661       0.5169067       0.2753238
4                    0.4959014 0.16009842                 0.7311085       0.4798347       0.2671747
5                    0.5309350 0.23415717                 0.7756198       0.6135128       0.2679191
6                    0.5779554 0.05924648                 0.8232041       0.5474170       0.2733281
> 
> # Sprawdzenie liczby braków w każdej kolumnie
> colSums(is.na(dane))
                    Country.name                             year                      Life.Ladder 
                               0                                0                                0 
              Log.GDP.per.capita                   Social.support Healthy.life.expectancy.at.birth 
                              28                               13                               63 
    Freedom.to.make.life.choices                       Generosity        Perceptions.of.corruption 
                              36                               81                              125 
                 Positive.affect                  Negative.affect 
                              24                               16 
> 
> # Procent braków dla każdej kolumny
> miss_var_summary(dane)
# A tibble: 11 × 3
   variable                         n_miss pct_miss
   <chr>                             <int>    <num>
 1 Perceptions.of.corruption           125    5.29 
 2 Generosity                           81    3.43 
 3 Healthy.life.expectancy.at.birth     63    2.67 
 4 Freedom.to.make.life.choices         36    1.52 
 5 Log.GDP.per.capita                   28    1.18 
 6 Positive.affect                      24    1.02 
 7 Negative.affect                      16    0.677
 8 Social.support                       13    0.550
 9 Country.name                          0    0    
10 year                                  0    0    
11 Life.Ladder                           0    0    
> 
> # Wizualizacja braków
> vis_miss(dane)
> 
> # Podstawowe statystyki opisowe
> summary(dane)
 Country.name            year       Life.Ladder    Log.GDP.per.capita Social.support   Healthy.life.expectancy.at.birth
 Length:2363        Min.   :2005   Min.   :1.281   Min.   : 5.527     Min.   :0.2282   Min.   : 6.72                   
 Class :character   1st Qu.:2011   1st Qu.:4.647   1st Qu.: 8.506     1st Qu.:0.7438   1st Qu.:59.20                   
 Mode  :character   Median :2015   Median :5.449   Median : 9.503     Median :0.8344   Median :65.10                   
                    Mean   :2015   Mean   :5.484   Mean   : 9.400     Mean   :0.8094   Mean   :63.40                   
                    3rd Qu.:2019   3rd Qu.:6.324   3rd Qu.:10.393     3rd Qu.:0.9038   3rd Qu.:68.55                   
                    Max.   :2023   Max.   :8.019   Max.   :11.676     Max.   :0.9873   Max.   :74.60                   
                                                   NA's   :28         NA's   :13       NA's   :63                      
 Freedom.to.make.life.choices   Generosity       Perceptions.of.corruption Positive.affect  Negative.affect  
 Min.   :0.2283               Min.   :-0.33955   Min.   :0.0352            Min.   :0.1789   Min.   :0.08274  
 1st Qu.:0.6607               1st Qu.:-0.11194   1st Qu.:0.6868            1st Qu.:0.5720   1st Qu.:0.20856  
 Median :0.7711               Median :-0.02161   Median :0.7985            Median :0.6634   Median :0.26217  
 Mean   :0.7503               Mean   : 0.00010   Mean   :0.7440            Mean   :0.6519   Mean   :0.27316  
 3rd Qu.:0.8617               3rd Qu.: 0.09357   3rd Qu.:0.8676            3rd Qu.:0.7373   3rd Qu.:0.32621  
 Max.   :0.9852               Max.   : 0.69957   Max.   :0.9833            Max.   :0.8836   Max.   :0.70459  
 NA's   :36                   NA's   :81         NA's   :125               NA's   :24       NA's   :16       
> 
> # Statystyki rozszerzone
> skim(dane)
── Data Summary ────────────────────────
                           Values
Name                       dane  
Number of rows             2363  
Number of columns          11    
_______________________          
Column type frequency:           
  character                1     
  numeric                  10    
________________________         
Group variables            None  

── Variable type: character ──────────────────────────────────────────────────────────────────────────────────────────────────
  skim_variable n_missing complete_rate min max empty n_unique whitespace
1 Country.name          0             1   4  25     0      165          0

── Variable type: numeric ────────────────────────────────────────────────────────────────────────────────────────────────────
   skim_variable                    n_missing complete_rate         mean     sd        p0      p25       p50       p75
 1 year                                     0         1     2015.        5.06   2005      2011     2015      2019     
 2 Life.Ladder                              0         1        5.48      1.13      1.28      4.65     5.45      6.32  
 3 Log.GDP.per.capita                      28         0.988    9.40      1.15      5.53      8.51     9.50     10.4   
 4 Social.support                          13         0.994    0.809     0.121     0.228     0.744    0.834     0.904 
 5 Healthy.life.expectancy.at.birth        63         0.973   63.4       6.84      6.72     59.2     65.1      68.6   
 6 Freedom.to.make.life.choices            36         0.985    0.750     0.139     0.228     0.661    0.771     0.862 
 7 Generosity                              81         0.966    0.0000956 0.161    -0.340    -0.112   -0.0216    0.0936
 8 Perceptions.of.corruption              125         0.947    0.744     0.185     0.0352    0.687    0.798     0.868 
 9 Positive.affect                         24         0.990    0.652     0.106     0.179     0.572    0.663     0.737 
10 Negative.affect                         16         0.993    0.273     0.0871    0.0827    0.209    0.262     0.326 
       p100 hist 
 1 2023     ▅▇▆▇▇
 2    8.02  ▁▂▇▇▃
 3   11.7   ▁▃▆▇▅
 4    0.987 ▁▁▂▆▇
 5   74.6   ▁▁▁▃▇
 6    0.985 ▁▂▅▇▇
 7    0.700 ▃▇▃▁▁
 8    0.983 ▁▁▁▅▇
 9    0.884 ▁▁▅▇▅
10    0.705 ▃▇▃▁▁
> 
> # Liczba krajów i lat
> dane %>%
+   summarise(liczba_krajow = n_distinct(Country.name),
+             liczba_lat = n_distinct(year))
  liczba_krajow liczba_lat
1           165         19
> dane <- read.csv("World Happiness Report 2024.csv", sep = ";")
> head(dane)
  Country.name year Life.Ladder Log.GDP.per.capita Social.support Healthy.life.expectancy.at.birth
1  Afghanistan 2008    3.723590           7.350416      0.4506623                             50.5
2  Afghanistan 2009    4.401778           7.508646      0.5523084                             50.8
3  Afghanistan 2010    4.758381           7.613900      0.5390752                             51.1
4  Afghanistan 2011    3.831719           7.581259      0.5211036                             51.4
5  Afghanistan 2012    3.782938           7.660506      0.5206367                             51.7
6  Afghanistan 2013    3.572100           7.680333      0.4835519                             52.0
  Freedom.to.make.life.choices Generosity Perceptions.of.corruption Positive.affect Negative.affect
1                    0.7181143 0.16405511                 0.8816863       0.4142970       0.2581955
2                    0.6788964 0.18729663                 0.8500354       0.4814214       0.2370924
3                    0.6001272 0.11786054                 0.7067661       0.5169067       0.2753238
4                    0.4959014 0.16009842                 0.7311085       0.4798347       0.2671747
5                    0.5309350 0.23415717                 0.7756198       0.6135128       0.2679191
6                    0.5779554 0.05924648                 0.8232041       0.5474170       0.2733281
> colSums(is.na(dane))
                    Country.name                             year                      Life.Ladder 
                               0                                0                                0 
              Log.GDP.per.capita                   Social.support Healthy.life.expectancy.at.birth 
                              28                               13                               63 
    Freedom.to.make.life.choices                       Generosity        Perceptions.of.corruption 
                              36                               81                              125 
                 Positive.affect                  Negative.affect 
                              24                               16 
> vis_miss(dane)
> View(dane)
> View(dane)
> dane_clean <- na.omit(dane)  # Usuwa wiersze z brakującymi wartościami
> 
> dane <- na.omit(dane)  
> colSums(is.na(dane))
                    Country.name                             year                      Life.Ladder 
                               0                                0                                0 
              Log.GDP.per.capita                   Social.support Healthy.life.expectancy.at.birth 
                               0                                0                                0 
    Freedom.to.make.life.choices                       Generosity        Perceptions.of.corruption 
                               0                                0                                0 
                 Positive.affect                  Negative.affect 
                               0                                0 
> summary(dane)
 Country.name            year       Life.Ladder    Log.GDP.per.capita Social.support   Healthy.life.expectancy.at.birth
 Length:2097        Min.   :2005   Min.   :2.179   Min.   : 5.527     Min.   :0.2902   Min.   : 6.72                   
 Class :character   1st Qu.:2011   1st Qu.:4.612   1st Qu.: 8.465     1st Qu.:0.7365   1st Qu.:58.66                   
 Mode  :character   Median :2015   Median :5.433   Median : 9.497     Median :0.8338   Median :65.10                   
                    Mean   :2015   Mean   :5.467   Mean   : 9.356     Mean   :0.8070   Mean   :63.27                   
                    3rd Qu.:2019   3rd Qu.:6.291   3rd Qu.:10.320     3rd Qu.:0.9051   3rd Qu.:68.68                   
                    Max.   :2023   Max.   :7.971   Max.   :11.676     Max.   :0.9873   Max.   :74.60                   
 Freedom.to.make.life.choices   Generosity         Perceptions.of.corruption Positive.affect  Negative.affect  
 Min.   :0.2575               Min.   :-0.3395468   Min.   :0.0352            Min.   :0.1789   Min.   :0.09432  
 1st Qu.:0.6591               1st Qu.:-0.1094579   1st Qu.:0.6911            1st Qu.:0.5717   1st Qu.:0.21109  
 Median :0.7692               Median :-0.0214623   Median :0.8007            Median :0.6650   Median :0.26383  
 Mean   :0.7489               Mean   : 0.0002447   Mean   :0.7471            Mean   :0.6537   Mean   :0.27481  
 3rd Qu.:0.8596               3rd Qu.: 0.0925846   3rd Qu.:0.8684            3rd Qu.:0.7404   3rd Qu.:0.32639  
 Max.   :0.9852               Max.   : 0.6995705   Max.   :0.9833            Max.   :0.8836   Max.   :0.70459  
> skim(dane)
── Data Summary ────────────────────────
                           Values
Name                       dane  
Number of rows             2097  
Number of columns          11    
_______________________          
Column type frequency:           
  character                1     
  numeric                  10    
________________________         
Group variables            None  

── Variable type: character ──────────────────────────────────────────────────────────────────────────────────────────────────
  skim_variable n_missing complete_rate min max empty n_unique whitespace
1 Country.name          0             1   4  24     0      155          0

── Variable type: numeric ────────────────────────────────────────────────────────────────────────────────────────────────────
   skim_variable                    n_missing complete_rate        mean     sd        p0      p25       p50       p75     p100
 1 year                                     0             1 2015.       4.97   2005      2011     2015      2019      2023    
 2 Life.Ladder                              0             1    5.47     1.14      2.18      4.61     5.43      6.29      7.97 
 3 Log.GDP.per.capita                       0             1    9.36     1.15      5.53      8.46     9.50     10.3      11.7  
 4 Social.support                           0             1    0.807    0.124     0.290     0.736    0.834     0.905     0.987
 5 Healthy.life.expectancy.at.birth         0             1   63.3      7.00      6.72     58.7     65.1      68.7      74.6  
 6 Freedom.to.make.life.choices             0             1    0.749    0.139     0.258     0.659    0.769     0.860     0.985
 7 Generosity                               0             1    0.000245 0.162    -0.340    -0.109   -0.0215    0.0926    0.700
 8 Perceptions.of.corruption                0             1    0.747    0.184     0.0352    0.691    0.801     0.868     0.983
 9 Positive.affect                          0             1    0.654    0.107     0.179     0.572    0.665     0.740     0.884
10 Negative.affect                          0             1    0.275    0.0855    0.0943    0.211    0.264     0.326     0.705
   hist 
 1 ▅▇▆▇▇
 2 ▁▅▇▇▃
 3 ▁▃▅▇▅
 4 ▁▁▂▆▇
 5 ▁▁▁▃▇
 6 ▁▂▅▇▆
 7 ▃▇▃▁▁
 8 ▁▁▁▅▇
 9 ▁▁▆▇▅
10 ▅▇▃▁▁
> dane %>%
+   summarise(liczba_krajow = n_distinct(Country.name),
+             liczba_lat = n_distinct(year))
  liczba_krajow liczba_lat
1           155         19
> country_counts <- table(dane$Country.name)
> print(country_counts)

             Afghanistan                  Albania                  Algeria                   Angola                Argentina 
                      13                       16                        8                        4                       18 
                 Armenia                Australia                  Austria               Azerbaijan                  Bahrain 
                      17                       16                       16                       16                        3 
              Bangladesh                  Belarus                  Belgium                   Belize                    Benin 
                      17                       13                       16                        2                       15 
                  Bhutan                  Bolivia   Bosnia and Herzegovina                 Botswana                   Brazil 
                       3                       18                       16                       14                       16 
                Bulgaria             Burkina Faso                  Burundi                 Cambodia                 Cameroon 
                      15                       16                        5                       15                       18 
                  Canada Central African Republic                     Chad                    Chile                 Colombia 
                      17                        5                       16                       18                       18 
                 Comoros      Congo (Brazzaville)         Congo (Kinshasa)               Costa Rica                  Croatia 
                       8                       13                       10                       18                       16 
                  Cyprus                  Czechia                  Denmark                 Djibouti       Dominican Republic 
                      15                       14                       17                        3                       18 
                 Ecuador                    Egypt              El Salvador                  Estonia                 Eswatini 
                      18                        9                       18                       17                        4 
                Ethiopia                  Finland                   France                    Gabon                   Gambia 
                      10                       16                       17                       13                        5 
                 Georgia                  Germany                    Ghana                   Greece                Guatemala 
                      18                       16                       18                       16                       16 
                  Guinea                   Guyana                    Haiti                 Honduras                  Hungary 
                      13                        1                       11                       17                       16 
                 Iceland                    India                Indonesia                     Iran                     Iraq 
                      10                       17                       18                       14                       14 
                 Ireland                   Israel                    Italy              Ivory Coast                  Jamaica 
                      17                       17                       17                       12                        9 
                   Japan                   Jordan               Kazakhstan                    Kenya                   Kuwait 
                      16                        4                       18                       18                        3 
              Kyrgyzstan                     Laos                   Latvia                  Lebanon                  Lesotho 
                      18                       11                       17                       17                        5 
                 Liberia                    Libya                Lithuania               Luxembourg               Madagascar 
                      11                        5                       18                       13                       12 
                  Malawi                 Malaysia                     Mali                    Malta               Mauritania 
                      15                       16                       17                        9                       15 
               Mauritius                   Mexico                  Moldova                 Mongolia               Montenegro 
                      10                       17                       18                       16                       15 
                 Morocco               Mozambique                  Myanmar                  Namibia                    Nepal 
                      12                       11                       12                        9                       17 
             Netherlands              New Zealand                Nicaragua                    Niger                  Nigeria 
                      16                       17                       18                       15                       16 
         North Macedonia                   Norway                 Pakistan                   Panama                 Paraguay 
                      16                       13                       17                       17                       17 
                    Peru              Philippines                   Poland                 Portugal                    Qatar 
                      18                       17                       16                       16                        1 
                 Romania                   Russia                   Rwanda             Saudi Arabia                  Senegal 
                      16                       18                       11                        2                       18 
                  Serbia             Sierra Leone                Singapore                 Slovakia                 Slovenia 
                      16                       15                       13                       15                       16 
                 Somalia             South Africa              South Korea                    Spain                Sri Lanka 
                       3                       17                       17                       17                       17 
                   Sudan                 Suriname                   Sweden              Switzerland                    Syria 
                       5                        1                       17                       13                        7 
Taiwan Province of China               Tajikistan                 Tanzania                 Thailand                     Togo 
                       3                       12                       18                       18                       12 
     Trinidad and Tobago                  Tunisia                  Türkiye                   Uganda                  Ukraine 
                       5                       14                       17                       18                       18 
    United Arab Emirates           United Kingdom            United States                  Uruguay               Uzbekistan 
                       3                       17                       16                       18                       13 
               Venezuela                  Vietnam                    Yemen                   Zambia                 Zimbabwe 
                      13                       13                        9                       17                       18 
> hist(country_counts)
> library(dplyr)
> 
> # Sprawdzenie, czy dla każdego kraju w danym roku są dane
> complete_years <- dane %>%
+   group_by(year) %>%
+   summarise(all_data_present = all(!is.na(Life.Ladder) & !is.na(Log.GDP.per.capita) & 
+                                      !is.na(Social.support) & !is.na(Healthy.life.expectancy.at.birth) &
+                                      !is.na(Freedom.to.make.life.choices) & !is.na(Generosity) & 
+                                      !is.na(Perceptions.of.corruption) & !is.na(Positive.affect) &
+                                      !is.na(Negative.affect)))
> 
> # Wyświetlenie lat, w których dla każdego kraju w danym roku są dane
> print(complete_years)
# A tibble: 19 × 2
    year all_data_present
   <int> <lgl>           
 1  2005 TRUE            
 2  2006 TRUE            
 3  2007 TRUE            
 4  2008 TRUE            
 5  2009 TRUE            
 6  2010 TRUE            
 7  2011 TRUE            
 8  2012 TRUE            
 9  2013 TRUE            
10  2014 TRUE            
11  2015 TRUE            
12  2016 TRUE            
13  2017 TRUE            
14  2018 TRUE            
15  2019 TRUE            
16  2020 TRUE            
17  2021 TRUE            
18  2022 TRUE            
19  2023 TRUE            
> library(dplyr)
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

             Afghanistan                  Albania                  Algeria                   Angola 
                      13                       16                        8                        4 
               Argentina                  Armenia                Australia                  Austria 
                      18                       17                       16                       16 
              Azerbaijan                  Bahrain               Bangladesh                  Belarus 
                      16                        3                       17                       13 
                 Belgium                   Belize                    Benin                   Bhutan 
                      16                        2                       15                        3 
                 Bolivia   Bosnia and Herzegovina                 Botswana                   Brazil 
                      18                       16                       14                       16 
                Bulgaria             Burkina Faso                  Burundi                 Cambodia 
                      15                       16                        5                       15 
                Cameroon                   Canada Central African Republic                     Chad 
                      18                       17                        5                       16 
                   Chile                 Colombia                  Comoros      Congo (Brazzaville) 
                      18                       18                        8                       13 
        Congo (Kinshasa)               Costa Rica                  Croatia                   Cyprus 
                      10                       18                       16                       15 
                 Czechia                  Denmark                 Djibouti       Dominican Republic 
                      14                       17                        3                       18 
                 Ecuador                    Egypt              El Salvador                  Estonia 
                      18                        9                       18                       17 
                Eswatini                 Ethiopia                  Finland                   France 
                       4                       10                       16                       17 
                   Gabon                   Gambia                  Georgia                  Germany 
                      13                        5                       18                       16 
                   Ghana                   Greece                Guatemala                   Guinea 
                      18                       16                       16                       13 
                  Guyana                    Haiti                 Honduras                  Hungary 
                       1                       11                       17                       16 
                 Iceland                    India                Indonesia                     Iran 
                      10                       17                       18                       14 
                    Iraq                  Ireland                   Israel                    Italy 
                      14                       17                       17                       17 
             Ivory Coast                  Jamaica                    Japan                   Jordan 
                      12                        9                       16                        4 
              Kazakhstan                    Kenya                   Kuwait               Kyrgyzstan 
                      18                       18                        3                       18 
                    Laos                   Latvia                  Lebanon                  Lesotho 
                      11                       17                       17                        5 
                 Liberia                    Libya                Lithuania               Luxembourg 
                      11                        5                       18                       13 
              Madagascar                   Malawi                 Malaysia                     Mali 
                      12                       15                       16                       17 
                   Malta               Mauritania                Mauritius                   Mexico 
                       9                       15                       10                       17 
                 Moldova                 Mongolia               Montenegro                  Morocco 
                      18                       16                       15                       12 
              Mozambique                  Myanmar                  Namibia                    Nepal 
                      11                       12                        9                       17 
             Netherlands              New Zealand                Nicaragua                    Niger 
                      16                       17                       18                       15 
                 Nigeria          North Macedonia                   Norway                 Pakistan 
                      16                       16                       13                       17 
                  Panama                 Paraguay                     Peru              Philippines 
                      17                       17                       18                       17 
                  Poland                 Portugal                    Qatar                  Romania 
                      16                       16                        1                       16 
                  Russia                   Rwanda             Saudi Arabia                  Senegal 
                      18                       11                        2                       18 
                  Serbia             Sierra Leone                Singapore                 Slovakia 
                      16                       15                       13                       15 
                Slovenia                  Somalia             South Africa              South Korea 
                      16                        3                       17                       17 
                   Spain                Sri Lanka                    Sudan                 Suriname 
                      17                       17                        5                        1 
                  Sweden              Switzerland                    Syria Taiwan Province of China 
                      17                       13                        7                        3 
              Tajikistan                 Tanzania                 Thailand                     Togo 
                      12                       18                       18                       12 
     Trinidad and Tobago                  Tunisia                  Türkiye                   Uganda 
                       5                       14                       17                       18 
                 Ukraine     United Arab Emirates           United Kingdom            United States 
                      18                        3                       17                       16 
                 Uruguay               Uzbekistan                Venezuela                  Vietnam 
                      18                       13                       13                       13 
                   Yemen                   Zambia                 Zimbabwe 
                       9                       17                       18 
> hist(country_counts)
> country_counts <- table(dane$Country.name)
> print(sort(country_counts))

                  Guyana                    Qatar                 Suriname                   Belize 
                       1                        1                        1                        2 
            Saudi Arabia                  Bahrain                   Bhutan                 Djibouti 
                       2                        3                        3                        3 
                  Kuwait                  Somalia Taiwan Province of China     United Arab Emirates 
                       3                        3                        3                        3 
                  Angola                 Eswatini                   Jordan                  Burundi 
                       4                        4                        4                        5 
Central African Republic                   Gambia                  Lesotho                    Libya 
                       5                        5                        5                        5 
                   Sudan      Trinidad and Tobago                    Syria                  Algeria 
                       5                        5                        7                        8 
                 Comoros                    Egypt                  Jamaica                    Malta 
                       8                        9                        9                        9 
                 Namibia                    Yemen         Congo (Kinshasa)                 Ethiopia 
                       9                        9                       10                       10 
                 Iceland                Mauritius                    Haiti                     Laos 
                      10                       10                       11                       11 
                 Liberia               Mozambique                   Rwanda              Ivory Coast 
                      11                       11                       11                       12 
              Madagascar                  Morocco                  Myanmar               Tajikistan 
                      12                       12                       12                       12 
                    Togo              Afghanistan                  Belarus      Congo (Brazzaville) 
                      12                       13                       13                       13 
                   Gabon                   Guinea               Luxembourg                   Norway 
                      13                       13                       13                       13 
               Singapore              Switzerland               Uzbekistan                Venezuela 
                      13                       13                       13                       13 
                 Vietnam                 Botswana                  Czechia                     Iran 
                      13                       14                       14                       14 
                    Iraq                  Tunisia                    Benin                 Bulgaria 
                      14                       14                       15                       15 
                Cambodia                   Cyprus                   Malawi               Mauritania 
                      15                       15                       15                       15 
              Montenegro                    Niger             Sierra Leone                 Slovakia 
                      15                       15                       15                       15 
                 Albania                Australia                  Austria               Azerbaijan 
                      16                       16                       16                       16 
                 Belgium   Bosnia and Herzegovina                   Brazil             Burkina Faso 
                      16                       16                       16                       16 
                    Chad                  Croatia                  Finland                  Germany 
                      16                       16                       16                       16 
                  Greece                Guatemala                  Hungary                    Japan 
                      16                       16                       16                       16 
                Malaysia                 Mongolia              Netherlands                  Nigeria 
                      16                       16                       16                       16 
         North Macedonia                   Poland                 Portugal                  Romania 
                      16                       16                       16                       16 
                  Serbia                 Slovenia            United States                  Armenia 
                      16                       16                       16                       17 
              Bangladesh                   Canada                  Denmark                  Estonia 
                      17                       17                       17                       17 
                  France                 Honduras                    India                  Ireland 
                      17                       17                       17                       17 
                  Israel                    Italy                   Latvia                  Lebanon 
                      17                       17                       17                       17 
                    Mali                   Mexico                    Nepal              New Zealand 
                      17                       17                       17                       17 
                Pakistan                   Panama                 Paraguay              Philippines 
                      17                       17                       17                       17 
            South Africa              South Korea                    Spain                Sri Lanka 
                      17                       17                       17                       17 
                  Sweden                  Türkiye           United Kingdom                   Zambia 
                      17                       17                       17                       17 
               Argentina                  Bolivia                 Cameroon                    Chile 
                      18                       18                       18                       18 
                Colombia               Costa Rica       Dominican Republic                  Ecuador 
                      18                       18                       18                       18 
             El Salvador                  Georgia                    Ghana                Indonesia 
                      18                       18                       18                       18 
              Kazakhstan                    Kenya               Kyrgyzstan                Lithuania 
                      18                       18                       18                       18 
                 Moldova                Nicaragua                     Peru                   Russia 
                      18                       18                       18                       18 
                 Senegal                 Tanzania                 Thailand                   Uganda 
                      18                       18                       18                       18 
                 Ukraine                  Uruguay                 Zimbabwe 
                      18                       18                       18 
> hist(country_counts)
> # Zliczamy liczbę wystąpień krajów
> country_counts <- table(dane$Country.name)
> 
> # Sortujemy krajów wg liczby wystąpień i wyświetlamy
> print(sort(country_counts))

                  Guyana                    Qatar                 Suriname                   Belize 
                       1                        1                        1                        2 
            Saudi Arabia                  Bahrain                   Bhutan                 Djibouti 
                       2                        3                        3                        3 
                  Kuwait                  Somalia Taiwan Province of China     United Arab Emirates 
                       3                        3                        3                        3 
                  Angola                 Eswatini                   Jordan                  Burundi 
                       4                        4                        4                        5 
Central African Republic                   Gambia                  Lesotho                    Libya 
                       5                        5                        5                        5 
                   Sudan      Trinidad and Tobago                    Syria                  Algeria 
                       5                        5                        7                        8 
                 Comoros                    Egypt                  Jamaica                    Malta 
                       8                        9                        9                        9 
                 Namibia                    Yemen         Congo (Kinshasa)                 Ethiopia 
                       9                        9                       10                       10 
                 Iceland                Mauritius                    Haiti                     Laos 
                      10                       10                       11                       11 
                 Liberia               Mozambique                   Rwanda              Ivory Coast 
                      11                       11                       11                       12 
              Madagascar                  Morocco                  Myanmar               Tajikistan 
                      12                       12                       12                       12 
                    Togo              Afghanistan                  Belarus      Congo (Brazzaville) 
                      12                       13                       13                       13 
                   Gabon                   Guinea               Luxembourg                   Norway 
                      13                       13                       13                       13 
               Singapore              Switzerland               Uzbekistan                Venezuela 
                      13                       13                       13                       13 
                 Vietnam                 Botswana                  Czechia                     Iran 
                      13                       14                       14                       14 
                    Iraq                  Tunisia                    Benin                 Bulgaria 
                      14                       14                       15                       15 
                Cambodia                   Cyprus                   Malawi               Mauritania 
                      15                       15                       15                       15 
              Montenegro                    Niger             Sierra Leone                 Slovakia 
                      15                       15                       15                       15 
                 Albania                Australia                  Austria               Azerbaijan 
                      16                       16                       16                       16 
                 Belgium   Bosnia and Herzegovina                   Brazil             Burkina Faso 
                      16                       16                       16                       16 
                    Chad                  Croatia                  Finland                  Germany 
                      16                       16                       16                       16 
                  Greece                Guatemala                  Hungary                    Japan 
                      16                       16                       16                       16 
                Malaysia                 Mongolia              Netherlands                  Nigeria 
                      16                       16                       16                       16 
         North Macedonia                   Poland                 Portugal                  Romania 
                      16                       16                       16                       16 
                  Serbia                 Slovenia            United States                  Armenia 
                      16                       16                       16                       17 
              Bangladesh                   Canada                  Denmark                  Estonia 
                      17                       17                       17                       17 
                  France                 Honduras                    India                  Ireland 
                      17                       17                       17                       17 
                  Israel                    Italy                   Latvia                  Lebanon 
                      17                       17                       17                       17 
                    Mali                   Mexico                    Nepal              New Zealand 
                      17                       17                       17                       17 
                Pakistan                   Panama                 Paraguay              Philippines 
                      17                       17                       17                       17 
            South Africa              South Korea                    Spain                Sri Lanka 
                      17                       17                       17                       17 
                  Sweden                  Türkiye           United Kingdom                   Zambia 
                      17                       17                       17                       17 
               Argentina                  Bolivia                 Cameroon                    Chile 
                      18                       18                       18                       18 
                Colombia               Costa Rica       Dominican Republic                  Ecuador 
                      18                       18                       18                       18 
             El Salvador                  Georgia                    Ghana                Indonesia 
                      18                       18                       18                       18 
              Kazakhstan                    Kenya               Kyrgyzstan                Lithuania 
                      18                       18                       18                       18 
                 Moldova                Nicaragua                     Peru                   Russia 
                      18                       18                       18                       18 
                 Senegal                 Tanzania                 Thailand                   Uganda 
                      18                       18                       18                       18 
                 Ukraine                  Uruguay                 Zimbabwe 
                      18                       18                       18 
> 
> # Tworzymy histogram liczby wystąpień krajów
> hist(country_counts, main="Histogram liczby wystąpień krajów", xlab="Liczba wystąpień", ylab="Liczba krajów", col="lightblue", border="black")
> country_counts <- table(dane$Country.name)
> print(sort(country_counts))

                  Guyana                    Qatar                 Suriname                   Belize 
                       1                        1                        1                        2 
            Saudi Arabia                  Bahrain                   Bhutan                 Djibouti 
                       2                        3                        3                        3 
                  Kuwait                  Somalia Taiwan Province of China     United Arab Emirates 
                       3                        3                        3                        3 
                  Angola                 Eswatini                   Jordan                  Burundi 
                       4                        4                        4                        5 
Central African Republic                   Gambia                  Lesotho                    Libya 
                       5                        5                        5                        5 
                   Sudan      Trinidad and Tobago                    Syria                  Algeria 
                       5                        5                        7                        8 
                 Comoros                    Egypt                  Jamaica                    Malta 
                       8                        9                        9                        9 
                 Namibia                    Yemen         Congo (Kinshasa)                 Ethiopia 
                       9                        9                       10                       10 
                 Iceland                Mauritius                    Haiti                     Laos 
                      10                       10                       11                       11 
                 Liberia               Mozambique                   Rwanda              Ivory Coast 
                      11                       11                       11                       12 
              Madagascar                  Morocco                  Myanmar               Tajikistan 
                      12                       12                       12                       12 
                    Togo              Afghanistan                  Belarus      Congo (Brazzaville) 
                      12                       13                       13                       13 
                   Gabon                   Guinea               Luxembourg                   Norway 
                      13                       13                       13                       13 
               Singapore              Switzerland               Uzbekistan                Venezuela 
                      13                       13                       13                       13 
                 Vietnam                 Botswana                  Czechia                     Iran 
                      13                       14                       14                       14 
                    Iraq                  Tunisia                    Benin                 Bulgaria 
                      14                       14                       15                       15 
                Cambodia                   Cyprus                   Malawi               Mauritania 
                      15                       15                       15                       15 
              Montenegro                    Niger             Sierra Leone                 Slovakia 
                      15                       15                       15                       15 
                 Albania                Australia                  Austria               Azerbaijan 
                      16                       16                       16                       16 
                 Belgium   Bosnia and Herzegovina                   Brazil             Burkina Faso 
                      16                       16                       16                       16 
                    Chad                  Croatia                  Finland                  Germany 
                      16                       16                       16                       16 
                  Greece                Guatemala                  Hungary                    Japan 
                      16                       16                       16                       16 
                Malaysia                 Mongolia              Netherlands                  Nigeria 
                      16                       16                       16                       16 
         North Macedonia                   Poland                 Portugal                  Romania 
                      16                       16                       16                       16 
                  Serbia                 Slovenia            United States                  Armenia 
                      16                       16                       16                       17 
              Bangladesh                   Canada                  Denmark                  Estonia 
                      17                       17                       17                       17 
                  France                 Honduras                    India                  Ireland 
                      17                       17                       17                       17 
                  Israel                    Italy                   Latvia                  Lebanon 
                      17                       17                       17                       17 
                    Mali                   Mexico                    Nepal              New Zealand 
                      17                       17                       17                       17 
                Pakistan                   Panama                 Paraguay              Philippines 
                      17                       17                       17                       17 
            South Africa              South Korea                    Spain                Sri Lanka 
                      17                       17                       17                       17 
                  Sweden                  Türkiye           United Kingdom                   Zambia 
                      17                       17                       17                       17 
               Argentina                  Bolivia                 Cameroon                    Chile 
                      18                       18                       18                       18 
                Colombia               Costa Rica       Dominican Republic                  Ecuador 
                      18                       18                       18                       18 
             El Salvador                  Georgia                    Ghana                Indonesia 
                      18                       18                       18                       18 
              Kazakhstan                    Kenya               Kyrgyzstan                Lithuania 
                      18                       18                       18                       18 
                 Moldova                Nicaragua                     Peru                   Russia 
                      18                       18                       18                       18 
                 Senegal                 Tanzania                 Thailand                   Uganda 
                      18                       18                       18                       18 
                 Ukraine                  Uruguay                 Zimbabwe 
                      18                       18                       18 
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
