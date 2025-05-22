knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(caret)
library(car)
library(corrplot)
library(sandwich)
library(psych)
library(moments)
library(ggplot2)
library(strucchange)
library(lmtest)
library(countrycode)
library(gridExtra)
library(e1071)  
library(randtests)

data <- read.csv("World Happiness Report 2024.csv", sep = ";")
data_clean <- na.omit(data)

dim(data)     
dim(data_clean) 
dim(data)[1] - dim(data_clean)[1]

class_table <- data.frame(Column = names(data_clean), Class = sapply(data_clean, class))
print(class_table)

lata_docelowe <- 2013:2023

dane_filtered <- data_clean %>%
  filter(year %in% lata_docelowe)

kraje_pelne_dane <- dane_filtered %>%
  group_by(Country.name) %>%
  summarise(liczba_lat = n_distinct(year)) %>%
  filter(liczba_lat == length(lata_docelowe))

kraje_z_danymi <- kraje_pelne_dane$Country.name

dane_finalne <- dane_filtered %>%
  filter(Country.name %in% kraje_z_danymi)

cat("Liczba krajów z pełnymi danymi (2013–2023):", length(kraje_z_danymi), "\n")
cat("Liczba pozostałych wierszy:", nrow(dane_finalne), "\n")
cat("Państwa z pełnymi danymi:\n", paste(kraje_z_danymi, collapse = ", "), "\n")

num_data <- dane_finalne %>% select_if(is.numeric)
cor_matrix <- cor(num_data, use = "complete.obs")
print(round(cor_matrix, 2))
describe(dane_finalne %>% select(-Country.name, -year))
describe(dane_finalne %>% select(-Country.name, -year))

library(ggplot2)
library(gridExtra)
library(grid)
library(e1071)

vars_to_plot <- names(dane_finalne)[sapply(dane_finalne, is.numeric) & names(dane_finalne) != "year"]

for (var_name in vars_to_plot) {
  variable <- dane_finalne[[var_name]]
  num_bins <- 30
  binwidth <- (max(variable, na.rm = TRUE) - min(variable, na.rm = TRUE)) / num_bins
  
  histogram <- ggplot(dane_finalne, aes(x = .data[[var_name]])) + 
    geom_histogram(binwidth = binwidth, fill = "lightblue", color = "black") +
    labs(title = paste("Histogram -", var_name), x = var_name, y = "Częstość") +
    theme_minimal(base_size = 16)
  
  stats_df <- data.frame(
    Statystyki = c("Średnia", "Odch. std.", "Minimum", "Maksimum", "Mediana", "Skośność", "Kurtoza"),
    Wartość = round(c(mean(variable, na.rm = TRUE),
                      sd(variable, na.rm = TRUE),
                      min(variable, na.rm = TRUE),
                      max(variable, na.rm = TRUE),
                      median(variable, na.rm = TRUE),
                      skewness(variable, na.rm = TRUE),
                      kurtosis(variable, na.rm = TRUE)), 3)
  )
  
  stats_table <- tableGrob(stats_df, rows = NULL, theme = ttheme_default(base_size = 16))
  grid.arrange(histogram, stats_table, ncol = 2, widths = c(2.5, 1.5))
}

library(reshape2)
library(dplyr)

num_data <- dane_finalne %>% select_if(is.numeric)
cor_matrix <- cor(num_data, use = "complete.obs")
cor_melted <- melt(cor_matrix)

ggplot(cor_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0, limit = c(-1, 1), name = "Korelacja") +
  geom_text(aes(label = ifelse(Var1 == Var2, "", sprintf("%.2f", value))), size = 3.5, color = "black") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), axis.title = element_blank(), panel.grid = element_blank(), panel.border = element_blank(), plot.title = element_text(hjust = 0.5, face = "bold")) +
  coord_fixed() +
  labs(title = "Heatmapa korelacji")

set.seed(123)

train <- dane_finalne %>% slice_sample(prop = 0.9)
test <- anti_join(dane_finalne, train)

data_cleaned = dane_finalne %>% select(-Country.name, -year) 

hellwig <- function(data, n) {
  cor_matrix <- cor(data)
  R0 <- cor_matrix[1, -1]
  R <- cor_matrix[-1, -1]
  L <- 2^n - 1
  comb <- expand.grid(rep(list(c(T, F)), n))
  best_H <- 0
  best_k <- NULL
  R <- abs(R)
  
  for (i in 1:L) {
    k <- c(1:n)[unlist(comb[i, ])]
    H <- 0
    for (j in k) {
      H = H + R0[j]^2 / sum(R[j, k])
    }
    if (H > best_H) {
      best_H <- H
      best_k <- k
    }
  }
  return(colnames(data)[best_k + 1])
}

hellwig(data = data_cleaned, n = 6)

model <- lm(`Life.Ladder` ~ `Log.GDP.per.capita` + `Social.support` +
              `Healthy.life.expectancy.at.birth` + `Freedom.to.make.life.choices` +
              Generosity + `Perceptions.of.corruption`,
            data = train)
summary(model)
resettest(model)

model2 <- lm(`Life.Ladder` ~ `Log.GDP.per.capita` + `Social.support` +
               `Healthy.life.expectancy.at.birth` +
               `Perceptions.of.corruption` +`Freedom.to.make.life.choices` ,
             data = train)
summary(model2)
plot(model2)

vif(model2)



correlations_gdp <- cor(num_data, use = "complete.obs")[, "Log.GDP.per.capita"]
correlations_gdp <- correlations_gdp[names(correlations_gdp) != "Log.GDP.per.capita"]
cor_data <- data.frame(Zmienna = names(correlations_gdp), Korelacja = round(correlations_gdp, 2))
cor_data_sorted <- cor_data %>% arrange(desc(Korelacja))
print(cor_data_sorted)

model3 <- lm(`Life.Ladder` ~ `Social.support` +
               `Healthy.life.expectancy.at.birth` +
               `Perceptions.of.corruption` +`Freedom.to.make.life.choices` ,
             data = train)
summary(model3)
plot(model3)
vif(model3)

res <- model3$residuals
hist(res)
skewness(res)
kurtosis(res)
shapiro.test(res)
qqnorm(res); qqline(res)

bptest(model3)
gqtest(model3, point = 0.5,  data = train)

plot(train$Log.GDP.per.capita, train$Life.Ladder,
     xlab = "Log GDP per Capita", ylab = "Life Ladder",
     main = "Zależność: PKB a Szczęście", col = "steelblue", pch = 16)
abline(lm(Life.Ladder ~ Log.GDP.per.capita, data = train), col = "red", lwd = 2)

res <- residuals(lm(Life.Ladder ~ Log.GDP.per.capita, data = train))
plot(train$Log.GDP.per.capita, res,
     xlab = "Log GDP per Capita", ylab = "Reszty",
     main = "Reszty względem Log GDP", pch = 16, col = "grey")
abline(h = 0, col = "red")

runs.test(res)
dwtest(model3)
resettest(model3)
sctest(model3)

pred <- predict(model3, newdata = test, interval="p")
mae <- mean(abs(pred - test$`Life.Ladder`))
rmse <- sqrt(mean((pred - test$`Life.Ladder`)^2))
mae
rmse

df <- data.frame(
  Actual = test$Life.Ladder,
  Prediction = pred[, "fit"],
  Lower = pred[, "lwr"],
  Upper = pred[, "upr"]
)

df %>%
  mutate(hit = if_else(Actual >= Lower & Actual <= Upper, TRUE, FALSE)) %>%
  summarise(hitrate = mean(hit))

plot(test$Life.Ladder, pred[,"fit"], 
     xlab = "Rzeczywiste wartości szczęścia", 
     ylab = "Przewidywane wartości szczęścia", 
     main = "Porównanie rzeczywistych vs przewidywanych wartości szczęścia",
     col = "blue", pch = 16)
abline(0, 1, col = "red", lwd = 2)

ggplot(data.frame(resid = residuals(model3), fitted = fitted(model2)),
       aes(x = fitted, y = resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Reszty vs dopasowane wartości", x = "Dopasowane", y = "Reszty")

model_year <- lm(Life.Ladder ~ Log.GDP.per.capita + Social.support +
                   Healthy.life.expectancy.at.birth + Perceptions.of.corruption +
                   factor(year), data = train)
summary(model_year)
anova(model2, model_year)
AIC(model2, model_year)
vif(model_year)

pred <- predict(model_year, newdata = test)
mae <- mean(abs(pred - test$`Life.Ladder`))
rmse <- sqrt(mean((pred - test$`Life.Ladder`)^2))
mae
rmse
error <- test$Life.Ladder - pred
MAE <- mean(abs(error))
MAPE <- mean(abs(error / test$Life.Ladder))*100
RMSE <- sqrt(mean(error^2))
cat("MAE:", MAE, "\nMAPE:", MAPE, "\nRMSE:", RMSE, "\n")
resettest(model_year)

data_2 <- dane_finalne
data_2$Region <- countrycode(sourcevar = dane_finalne$Country.name, origin = "country.name", destination = "region")
data_2$RegionCode <- as.numeric(factor(data_2$Region))
levels(factor(data_2$Region))

ggplot(data_2, aes(x = factor(Region), y = Life.Ladder)) +
  geom_boxplot(fill = "skyblue", alpha = 0.7, outlier.color = "red") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "darkblue", position = position_dodge(width = 0.75)) +
  labs(title = "Rozkład szczęścia (Life Ladder) w regionach", x = "Region", y = "Life Ladder") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

data_2 %>%
  group_by(year, Region) %>%
  summarise(mean_life = mean(Life.Ladder, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = mean_life, color = factor(Region))) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "Średni poziom szczęścia w regionach na przestrzeni lat", x = "Rok", y = "Średni Life Ladder", color = "Region") +
  theme_minimal(base_size = 14)

anova_region <- aov(Life.Ladder ~ factor(Region), data = data_2)
summary(anova_region)

data_2cleaned <- data_2 %>% select(-year, -Country.name, -Region)
region_dummies <- model.matrix(~ factor(RegionCode) - 1, data = data_2cleaned)
hellwig_data <- cbind(data_2cleaned, region_dummies)
hellwig_data <- hellwig_data %>% select(-RegionCode)

hellwig(hellwig_data, 13)
cor(hellwig_data$Life.Ladder, region_dummies)

train1 <- data_2 %>% slice_sample(prop = 0.9) 
test1 <- anti_join(data_2, train)

model3 <- lm(`Life.Ladder` ~  `Social.support` +
               `Healthy.life.expectancy.at.birth` + `Perceptions.of.corruption` + factor(RegionCode),
             data = train1)
summary(model3)
anova(model3, model2)
AIC(model3, model2)
vif(model3)

pred1 <- predict(model3, newdata = test1, interval="prediction")
df <- data.frame(
  Actual = test1$Life.Ladder,
  Prediction = pred1[, "fit"],
  Lower = pred1[, "lwr"],
  Upper = pred1[, "upr"]
)

df %>%
  mutate(hit = if_else(Actual >= Lower & Actual <= Upper, TRUE, FALSE)) %>%
  summarise(hitrate = mean(hit))

dwtest(model3)
resettest(model3)
sctest(model3)


