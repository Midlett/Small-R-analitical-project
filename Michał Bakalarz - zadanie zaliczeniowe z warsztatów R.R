devtools::install_github("ropensci/essurvey")
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")

install.packages("ggplot2")
install.packages("tidyverse")
install.packages("essurvey")
install.packages("foreign")
install.packages("haven")
install.packages("ggmosaic")
install.packages("dply")
install.packages("stats")
install.packages("ggstatsplot")
install.packages("vcd")
install.packages("psych")
install.packages("rcompanion")
install.packages("car")
install.packages("report")
install.packages("rlang")
install.packages("crosstable")
install.packages("gtsummary")

library(ggplot2)
library(tidyverse)
library(essurvey)
library(foreign)
library(haven)
library(ggmosaic)
library(dplyr)
library(stats)
library(ggstatsplot)
library(vcd)
library(psych)
library(rcompanion)
library(car)
library(report)
library(rlang)
library(crosstable)
library(knitr)
library(flextable)
library(gtsummary)

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Zad1

setwd("C:/Users/midle/Desktop/Michał Bakalarz - zadanie zaliczeniowe z warsztatów R")

ess <- read_sav("ESS9e03_1.sav")
view(ess)

str(ess)

# do mojej analizy wybiorę zmienną porządkową stflife oraz zmienną nominalną gndr.
# poniżej opisane zmienne.

str(ess$gndr)
str(ess$stflife)


esspl = ess %>%
  filter(cntry == "PL") # tworzę przefiltrowany nowy zbiór z mieszkańcami PL

zad1 = data.frame(esspl$gndr, esspl$stflife)

sum(is.na(zad1))
czad1 = na.omit(zad1)

sum(is.na(czad1))

crosstab(czad1, row.vars = "esspl.gndr", col.vars = "esspl.stflife", type="c")

# tabelka krzyżowa nie wiem dlaczego ale nie czyta etytkiet.

tbl_cross(
  czad1,
  row = esspl.gndr,
  col = esspl.stflife,
  label = list(esspl.gndr ~ "Gender", 
               esspl.stflife ~ "How satisfied with life as a whole"),
  statistic = "{p}%", 
  digits = c(0, 1),
  percent = c("column"),
  margin = c("column", "row"),
  missing = c("ifany"), 
  missing_text = "Unknown",
  margin_text = "Total"
)

# tabelka krzyżowa 2 wariant.

tbl_cross(
  czad1,
  row = esspl.stflife,
  col = esspl.gndr,
  label = list(esspl.gndr ~ "Gender", 
               esspl.stflife ~ "How satisfied with life as a whole"),
  statistic = "{n} ({p}%)", 
  digits = c(0, 1),
  percent = c("column"),
  margin = c("column", "row"),
  missing = c("ifany"), 
  missing_text = "Unknown",
  margin_text = "Total"
)

tblzad1 = table(esspl$gndr, esspl$stflife)

# chi2 test
chisq.test(tblzad1) # brak zależności między płcią a satysfakcją z życia

# Fisher test

fisher = fisher.test(tblzad1, simulate.p.value=TRUE)
fisher # test fishera potwierdza brak zależności pomiędzy płcią a satysfakcją z życia

b = biserial(esspl$stflife, esspl$gndr)
b # korelacja dwuseryjna

# wykres słupkowy z podziałem na grupy wg kategorii drugiej zmiennej płeć

ggplot(czad1, aes(esspl.stflife)) + 
  geom_bar(color = "steelblue", fill = "steelblue") +
  facet_wrap(vars(esspl.gndr)) +
  labs(title = "Life satisfaction by gender",
       subtitle = "1 = Male, 2 = Female ",
       y = "Count of respondents", x = "How satisfied with life as a whole")
  
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Zad2

# lrscale, prtclhpl, ilościowa netustm, cntry, idno id,

str(esspl$lrscale) # Zmienna porządkowa, odpowiadająca na pytanie, "Placement on left right scale"

str(esspl$gndr) # zmienna nominalna płeć

zad2 = data.frame(esspl$gndr, esspl$lrscale) # df ze zmiennymi (płeć i usytuowanie na skali lewo prawo)

zad2

sum(is.na(zad2))
czad2 = na.omit(zad2) # usuwam braki danych

sum(is.na(czad2))

summary(czad2) # podsumowanie df

IQR(esspl$gndr)
IQR(na.omit(esspl$lrscale))

describe(esspl$gndr)
describe(esspl$lrscale)

czad2 %>%
  tbl_summary(
    by = esspl.gndr,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    digits = all_continuous() ~ 2,
    label = list(esspl.lrscale ~ "Placement on left right scale")
  ) %>% add_p()

# Analiza równości średnich (parametryczne): t-test i ANOVA
levels(esspl$lrscale)
table(esspl$lrscale)
table(as.integer(esspl$lrscale))

zależna2 = as.numeric(esspl$lrscale)
niezależna2 = esspl$gndr

describeBy(zależna2, group = niezależna2)

t.test(zależna2 ~ niezależna2) # t-test, średnie w grupach nie różnią się przyjmuje h0

res_aov = aov(zależna2 ~ niezależna2) # ANOVA
summary(res_aov)

# Sprawdzanie normalności rozkładu
par(mfrow = c(1, 2)) 
hist(res_aov$residuals) # histogram

qqPlot(res_aov$residuals,
       id = FALSE
) # QQ-plot

b2 = biserial(esspl$lrscale, esspl$gndr)
b2 # korelacja dwuseryjna

# histogramy w podziale na grupy wg kategorii drugiej zmiennej
ggplot(czad2, aes(esspl.lrscale)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "steelblue") +
  facet_wrap(vars(esspl.gndr))+
  labs(title = "Placement on left right scale by gender",
       subtitle = "1 = Male, 2 = Female ",
       y = "Count of respondents", x = "Placement on left right scale")

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Zad 3 netustm 

str(esspl)
str(esspl$netustm)
str(esspl$stfeco)


zad3 = data.frame(esspl$netustm, esspl$stfeco) # df ze zmiennymi ilościowymi ("Internet use, how much time on typical day, in minutes" i "How satisfied with present state of economy in country")

zad3

sum(is.na(zad3))
czad3 = na.omit(zad3) # usuwam braki danych

sum(is.na(czad3))

summary(czad3) # podsumowanie df

IQR(na.omit(esspl$stfeco))
IQR(na.omit(esspl$netustm))
 
czad3

reg_df = czad3[, c("esspl.stfeco", "esspl.netustm")]
names(reg_df)[1:2] = c("niezależna", "zależna")

sapply(reg_df, class)
reg_df = as.data.frame(sapply(reg_df, as.numeric)) # df do regresji

model = lm(zależna ~ niezależna, data = reg_df)
summary(model) # model Regresji

# wykres rozrzutu
ggplot(reg_df, aes(x = niezależna, y = zależna)) +
  geom_point() +
  stat_smooth()

# sprawdzanie korelacji
cor(reg_df$niezależna, reg_df$zależna, method = c("pearson", "kendall", "spearman"))
cor(na.omit(reg_df))
cor(reg_df, use = "pairwise.complete.obs")


