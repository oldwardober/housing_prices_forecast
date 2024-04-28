
# CENY MIESZKAÑ A IMIGRACJA Z UKRAINY - SYMULACJA

setwd("C:/Users/kszaf/OneDrive/Pulpit/I rok/prognozowanie_symulacje/projekt-symulka") #change to your directory

#you have to install this pacakges if you don't have them (install.packages())
library(DataCombine) 
library(tidyverse)
library(MASS)
library(fpp2)


dane = readxl::read_excel("dane_ceny_mieszkan.xlsx")

summary(dane)

#tworzê lagi zmiennych

dane$gdp_lag1 = shift(dane$gdp, -1, reminder = FALSE)
dane$gdp_lag2 = shift(dane$gdp, -2, reminder = FALSE)
dane$pracujacy_lag1 = shift(dane$pracujacy, -1, reminder = FALSE)
dane$pracujacy_lag2 = shift(dane$pracujacy, -2, reminder = FALSE)
dane$stopa_lag1 = shift(dane$stopa, -1, reminder = FALSE)
dane$stopa_lag2 = shift(dane$stopa, -2, reminder = FALSE)
dane$cpi_lag1 = shift(dane$cpi, -1, reminder = FALSE)
dane$cpi_lag2 = shift(dane$cpi, -2, reminder = FALSE)
dane$ceny_lag1 = shift(dane$ceny, -1, reminder = FALSE)
dane$ceny_lag2 = shift(dane$ceny, -2, reminder = FALSE)

#stepwise regression (gdp_lag2, stopa_lag1, cpi)

dane2 = dane[dane$indeks>2 & dane$indeks < 48,]

full_model = lm(ceny ~ ceny_lag1 + ceny_lag2 + gdp + gdp_lag1 + gdp_lag2 + pracujacy + pracujacy_lag1 + pracujacy_lag2 + stopa + stopa_lag1 +
                      stopa_lag2 + cpi + cpi_lag1 + cpi_lag2, data = dane2)
summary(full_model)

step_model <- stepAIC(full_model, direction = "both", 
                      trace = FALSE)
summary(step_model)

full_model2 = lm(ceny ~ ceny_lag1 + gdp + gdp_lag1 + pracujacy + pracujacy_lag1 + stopa + stopa_lag1 +
                  cpi + cpi_lag1, data = dane2)
summary(full_model2)
step_model2 <- stepAIC(full_model2, direction = "both", 
                      trace = FALSE)
summary(step_model2)

model = lm(ceny ~ pracujacy_lag1 + stopa_lag1 + cpi, data = dane2)
summary(model)

stargazer::stargazer(model)

# SYMULACJA

#scenariusz bazowy (wzrost pracujacych 2.5%)
dane_bazowe = dane
dane_bazowe$pracujacy[c(49:51)] = 2.5
dane_bazowe$pracujacy_lag1 = shift(dane_bazowe$pracujacy, -1, reminder = FALSE)
dane_bazowe$pracujacy_lag2 = shift(dane_bazowe$pracujacy, -2, reminder = FALSE)

#scenariusz kontrfaktyczny (wzrost pracujacych 0.3%)
dane_kontrfaktyczne = dane
dane_kontrfaktyczne$pracujacy[c(48:51)] = 0.3
dane_kontrfaktyczne$pracujacy_lag1 = shift(dane_kontrfaktyczne$pracujacy, -1, reminder = FALSE)
dane_kontrfaktyczne$pracujacy_lag2 = shift(dane_kontrfaktyczne$pracujacy, -2, reminder = FALSE)

for(i in 48:51) {
  dane_bazowe$ceny[i] = model$coefficients[1] + model$coefficients[2]*dane_bazowe$pracujacy_lag1[i] +
    model$coefficients[3]*dane_bazowe$stopa_lag1[i] + model$coefficients[4]*dane_bazowe$cpi[i]
}
for(i in 48:51) {
  dane_kontrfaktyczne$ceny[i] = model$coefficients[1] + model$coefficients[2]*dane_kontrfaktyczne$pracujacy_lag1[i] +
    model$coefficients[3]*dane_kontrfaktyczne$stopa_lag1[i] + model$coefficients[4]*dane_kontrfaktyczne$cpi[i]
}

#biedny wykres
plot(c(1,2,3,4), dane_kontrfaktyczne$ceny[c(48:51)], type = 'b', col = 'blue', ylab = "Procentowa zmiana cen mieszkañ q/q", xlab = "Kwarta³ 2022 roku")
lines(c(1,2,3,4), dane_bazowe$ceny[c(48:51)], type = 'b', col = 'red')

#lepszy wykres
dane_wykres = as.data.frame(dane_bazowe$ceny[c(48:51)])
dane_wykres[,2] = as.data.frame(dane_kontrfaktyczne$ceny[c(48:51)])
dane_wykres[,3] = c(1,2,3,4)
dane_wykres[,4] = dane_wykres$ceny1 - dane_wykres$ceny2
names = c("ceny1", "ceny2", "kwarta³")
colnames(dane_wykres) = names

library(ggplot2)
g = ggplot(dane_wykres, aes(kwarta³)) + geom_line(aes(y=ceny2, color = 'Scenariusz kontrfaktyczny'), size = 0.75) +
  geom_point(aes(y=ceny2), size = 2) +
  geom_line(aes(y = ceny1, color = 'Scenariusz bazowy'), size = 0.75) + geom_point(aes(y = ceny1), size = 2)
g = g + labs(x = "Kwarta³ 2022 roku", y = "Procentowa zmiana cen mieszkañ q/q", color = " ")
g = g + theme(legend.position = c(0.75,0.85))
g
