# pulizia variabili
rm(list=ls())
# pulizia console
cat("\014")
# pulizia plot (eliminare se si utilizza R Markdown)
dev.off()
library(stR)
library(fpp2)
library(psych)
library(stats)
library(olsrr)
library(readxl)
library(lmtest)
library(forecast)
library(ggfortify)
library(lubridate)
library(systemfit)
library(skedastic)
library(DataCombine)
library(dplyr)
library(missForest)
library(imputeTS)
library(MLmetrics)
library(xts)
library(stringr)
library(missForest)
library(tibble)
library(Rcpp)
setwd("C:/Users/danie/Desktop/DSLAB")

data_u1 <- read.csv("DATA/U1_DATECOMPLETE_CLEANED.csv")
data_u1$CONSUMO_REATTIVA_INDUTTIVA_PRELEVATA_y <- as.numeric(gsub(",", ".", data_u1$CONSUMO_REATTIVA_INDUTTIVA_PRELEVATA_y))
summary(data_u1)

######## ANALISI DESCRITTIVE - 15 MIN #######
ts_15_min = ts(data_u1$CONSUMO_REATTIVA_INDUTTIVA_PRELEVATA_y,
               frequency = 365, start = c(2018, 1), end = c(2020,365))

# Grafico consumo attiva prelevata
plot(ts_15_min, 
     col="yellow", 
     xlab = "Data", 
     ylab = "kw", 
     main="U1 - Consumo attivo prelevata", 
     cex.main=0.8, 
     type='l')

# Decomposizione
decomp <- stl(ts_15_min, s.window = "periodic", na.action = na.omit)
plot(decomp)

# Stagionalità ogni 15 min
ggseasonplot(ts_15_min, year.labels=TRUE, year.labels.left=TRUE)+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x=element_blank()) +
  ylab("Consumo attiva prelevata") +
  xlab("15 min frequenza") +
  ggtitle("U1 - Stagionalità: C.A.P ogni 15 min")

# Verifica residui ARIMA
# notiamo presenza correlazione, informazioni utili non utilizzate dal modello
checkresiduals(arima(ts_15_min))


#SOSTITUZIONE DEGLI ZERO CON NA
data_u1[data_u1 == 0] <- NA

statsNA(data_u1$CONSUMO_ATTIVA_PRELEVATA_y)

ggplot_na_distribution(data_u1$CONSUMO_ATTIVA_PRELEVATA_y)

set.seed(1)

# eliminiamo i null dal vettore colonna
db_senza_nulli <- na.omit(data_u1$CONSUMO_ATTIVA_PRELEVATA_y)
db_senza_nulli <- as.data.frame(db_senza_nulli)

# generiamo dei null artificialmente
db_test_nulli <- prodNA(db_senza_nulli, noNA = 0.00223)

# test dei vari algoritmi di imputazione
imputation <- na_interpolation(db_test_nulli)
imputation_kalman <- na_kalman(db_test_nulli)
imputation_sea <- na_seadec(db_test_nulli, find_frequency=TRUE)
imputation_mean <- na_mean(db_test_nulli)
imputation_locf <- na_locf(db_test_nulli)
imputation_seasplit <- na_seasplit(db_test_nulli, find_frequency=TRUE)
imputation_ma <- na_ma(db_test_nulli)

# plot risultati imputazione vs dati a disposizione
ggplot_na_imputations(db_test_nulli, imputation, db_senza_nulli)
ggplot_na_imputations(db_test_nulli, imputation_sea, db_senza_nulli)
ggplot_na_imputations(db_test_nulli, imputation_kalman, db_senza_nulli)
ggplot_na_imputations(db_test_nulli, imputation_mean, db_senza_nulli)
ggplot_na_imputations(db_test_nulli, imputation_locf, db_senza_nulli)
ggplot_na_imputations(db_test_nulli, imputation_seasplit, db_senza_nulli)
ggplot_na_imputations(db_test_nulli, imputation_ma, db_senza_nulli)

# test risultati ottenuti per decretare alg. imputazione migliore

# otteniamo indici dei valori nulli che sono stati imputati
index_null <- which(is.na(db_test_nulli), arr.ind = TRUE)
index_null <- as.vector(index_null)

# inseriamo valori imputati e reali nello stesso dataframe
db_for_evaluation <- cbind(imputation[index_null,],
                           imputation_sea[index_null,],
                           imputation_kalman[index_null,],
                           imputation_mean[index_null,],
                           imputation_locf[index_null,],
                           imputation_seasplit[index_null,],
                           imputation_ma[index_null,],
                           db_senza_nulli[index_null,])
db_for_evaluation <- as.data.frame(db_for_evaluation)
names(db_for_evaluation) <- c("interpolation", 
                              "seadec", 
                              "kalman", 
                              "mean",
                              "locf",
                              "seasplit",
                              "ma",
                              "original")
# TEST MSE
MSE(db_for_evaluation$interpolation, db_for_evaluation$original)
MSE(db_for_evaluation$seadec, db_for_evaluation$original)
MSE(db_for_evaluation$kalman, db_for_evaluation$original)
MSE(db_for_evaluation$mean, db_for_evaluation$original)
MSE(db_for_evaluation$locf, db_for_evaluation$original)
MSE(db_for_evaluation$seasplit, db_for_evaluation$original)
MSE(db_for_evaluation$ma, db_for_evaluation$original)

# TEST MAPE
MAPE(db_for_evaluation$interpolation, db_for_evaluation$original)*100
MAPE(db_for_evaluation$seadec, db_for_evaluation$original)*100
MAPE(db_for_evaluation$kalman, db_for_evaluation$original)*100
MAPE(db_for_evaluation$mean, db_for_evaluation$original)*100
MAPE(db_for_evaluation$locf, db_for_evaluation$original)*100
MAPE(db_for_evaluation$seasplit, db_for_evaluation$original)*100
MAPE(db_for_evaluation$ma, db_for_evaluation$original)*100

# TEST RMSE
RMSE(db_for_evaluation$interpolation, db_for_evaluation$original)
RMSE(db_for_evaluation$seadec, db_for_evaluation$original)
RMSE(db_for_evaluation$kalman, db_for_evaluation$original)
RMSE(db_for_evaluation$mean, db_for_evaluation$original)
RMSE(db_for_evaluation$locf, db_for_evaluation$original)
RMSE(db_for_evaluation$seasplit, db_for_evaluation$original)
RMSE(db_for_evaluation$ma, db_for_evaluation$original)

# sostituzione valori nulli con Kalman 
imputation_con_att <- na_kalman(data_u1$CONSUMO_ATTIVA_PRELEVATA_y) 
imputation_pot_max <- na_kalman(data_u1$POTENZA_MASSIMA) 

ggplot_na_imputations(data_u1$CONSUMO_ATTIVA_PRELEVATA_y, imputation_con_att)
ggplot_na_imputations(data_u1$POTENZA_MASSIMA, imputation_pot_max)


data_u1[['CONSUMO_ATTIVA_PRELEVATA_y']] <- imputation_con_att
data_u1[['POTENZA_MASSIMA_y']] <- imputation_pot_max

# colonne inutili
data_u1 <- subset(data_u1, select = -c(POD_y,FL_ORA_LEGALE_y, TIPO_DATO_y,ANNO_y))
# otteniamo dataset i cui dati sono stati interpolati, ci serve il tbats
write.csv(data_u1,"./U1_15min_kalman.csv", row.names = FALSE)
summary(data_u1)

ts_u1 <- ts(data_u1$CONSUMO_ATTIVA_PRELEVATA_y,
            start = c(2018,1),
            end = c(2021,1),
            frequency = 365)

## DECOMPOSIZIONE STL
decomp <- stl(ts_u1, s.window = "periodic", robust = TRUE)
plot(decomp)

decomp_mstl <- mstl(ts_u1)

plot(decomp_mstl)
plot(ts_u1, col = "yellow")
res <- remainder(decomp)

# GRAFICI DESCRITTIVI

par(mfrow=c(3,2),
    cex.axis=1, 
    cex.lab=1, 
    cex.main=1, 
    fg="white",
    col.axis="white", 
    col.lab="white", 
    col.main="white", 
    bg="black")

plot(res, 
     type="o", 
     pch=20, 
     col="white", 
     main="Residuals", 
     cex.main=1.4)

hist(res, 
     col="black", 
     main="Histogram", 
     cex.main=1.4)

qqnorm(res, 
       cex.main=1.4)

qqline(res)

par(mfrow=c(1,1))

acf(ts_u1, 
    cex.main=1.4)

pacf(ts_u1)

summary(data_u1)

##### FINE R ######

# In python prendiamo data_u1, aggreghiamo per ore ed
# effettuiamo join con dati meteo

##### FINE PYTHON ######

# leggo i dati u1 per ore arricchiti con il meteo su python
join_meteo_u1 <- read.csv("DATA/join_meteo_u1.csv")

# sostituzione valori nulli (temp) con Kalman 
imputation_temp <- na_kalman(join_meteo_u1$temp) 
join_meteo_u1[['temp']] <- imputation_temp

# cross-correlation sulle ore
corr = Ccf(as.ts(join_meteo_u1$temp), as.ts(join_meteo_u1$CONSUMO_ATTIVA_PRELEVATA_y))
corr_1 = Ccf(as.ts(join_meteo_u1$CONSUMO_ATTIVA_PRELEVATA_y), as.ts(join_meteo_u1$temp), 50)

# time series x ore (ci serve per ACF in seguito)
ts_u1_completo_ore <- ts(join_meteo_u1$CONSUMO_ATTIVA_PRELEVATA_,
   start = c(2018,1),
   frequency = 24)

# solo attributi x media
u1_completo_giorno_mean <- aggregate(list(temp=join_meteo_u1$temp, dwpt=join_meteo_u1$dwpt,
                                          rhum=join_meteo_u1$rhum,
                                          wdir=join_meteo_u1$wdir, wspd=join_meteo_u1$wspd,
                                          pres=join_meteo_u1$pres, kelvin=join_meteo_u1$kelvin, 
                                          POTENZA_MASSIMA=join_meteo_u1$POTENZA_MASSIMA), 
                                     by = list(data=join_meteo_u1$ONLY_DATA), mean)

# solo attributi x somma
u1_completo_giorno_sum <- aggregate(list(CONSUMO_ATTIVA_PRELEVATA=join_meteo_u1$CONSUMO_ATTIVA_PRELEVATA_y, 
                                         CONSUMO_REATTIVA_INDUTTIVA_PRELEVATA=join_meteo_u1$CONSUMO_REATTIVA_INDUTTIVA_PRELEVATA_y), 
                                    by = list(data=join_meteo_u1$ONLY_DATA), sum)

# u1 completo giorno
u1_completo_giorno = merge(x=u1_completo_giorno_mean, y=u1_completo_giorno_sum, by="data")

# cross-correlation sui giorni
corr_temp_cons = Ccf(u1_completo_giorno$temp, u1_completo_giorno$CONSUMO_ATTIVA_PRELEVATA)
corr_cons_temp = Ccf(as.ts(u1_completo_giorno$CONSUMO_ATTIVA_PRELEVATA), as.ts(u1_completo_giorno$temp))
plot(corr_cons_temp, main = "U1 - CCF tra consumi e temperatura", ylab = "CCF")

####### ANALISI DESCRITTIVE X MESE ########
u1_completo_giorno$data <- as.POSIXct(u1_completo_giorno$data, format='%Y-%m-%d')
u1_completo_mese = u1_completo_giorno %>%
  mutate(mese = format(data, "%m"), anno = format(data, "%Y")) %>%
  group_by(mese, anno) %>%
  summarise(CONSUMO_ATTIVA_PRELEVATA = mean(CONSUMO_ATTIVA_PRELEVATA))

u1_completo_mese <- u1_completo_mese[order(u1_completo_mese$anno), ]

ts_mese = ts(u1_completo_mese$CONSUMO_ATTIVA_PRELEVATA,
             frequency = 12, start = c(2018,1))

# Grafico x mese
plot(ts_mese,
     col="red",
     ylab = "C.A.P",
     xlab = "Mesi",
     main="C.A.P - Aggregata per mese",
     cex.main=0.8,
     type='l')

# Decomposizione 
ts_mese_na_omit = ts(na.omit(u1_completo_mese$CONSUMO_ATTIVA_PRELEVATA),
                     frequency = 12, start = c(2018,1))
decomp <- stl(ts_mese_na_omit, s.window = "periodic", na.action = na.omit)
plot(decomp)

# C.A.P grafico per mese
ggmonthplot(ts_mese) +
  ylab("C.A.P") +
  xlab("Mese") + 
  ggtitle("Subseries: C.A.P per mese")

# Stagionalità x mese
ggseasonplot(ts_mese, year.labels=TRUE, year.labels.left=TRUE)+
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("C.A.P") +
  ggtitle("Stagionalità: C.A.P per mese")


######## ANALISI DESCRITTIVE X GIORNO ########

ts_giorno = ts(u1_completo_giorno$CONSUMO_ATTIVA_PRELEVATA,
               frequency = 365, start = c(2018,1))

######## ACF
acf(ts_giorno, lag.max = 30)
acf(ts_u1_completo_ore, lag.max = 96, na.action = na.pass)

# Grafico x giorno
plot(ts_giorno, 
     col="red",
     ylab = "C.A.P",
     xlab = "Giorni",
     main="U1 - C.A.P - Aggregata per giorno",
     cex.main=0.8,
     type='l')

# Decomposizione 
ts_giorno_na_omit = ts(na.omit(u1_completo_giorno$CONSUMO_ATTIVA_PRELEVATA),
                       frequency = 365, start = c(2018,1))
decomp <- stl(ts_giorno_na_omit, s.window = "periodic", na.action = na.omit)
plot(decomp)

# C.A.P grafico per mese
ggmonthplot(ts_giorno) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x=element_blank()) +
  ylab("C.A.P") +
  xlab("Giorno") + 
  ggtitle("Subseries: C.A.P per giorno")

# Stagionalità x mese
ggseasonplot(ts_giorno, year.labels=TRUE, year.labels.left=TRUE)+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x=element_blank()) +
  ylab("C.A.P") +
  ggtitle("Stagionalità: C.A.P per giorno")

###### PREDIZIONE TBATS - TEST 2019 #######

ts_u1_att_pre <- ts(u1_completo_giorno$CONSUMO_ATTIVA_PRELEVATA,
                    start = c(2018,1),
                    end = c(2021,1),
                    frequency = 365)

training=window(ts_u1_att_pre, start = c(2018,1), end = c(2018,365))
validation = window(ts_u1_att_pre, start = c(2019, 1), end = c(2019,365))

pred.train <- tbats(training, seasonal.periods = c(7,365))
pred.train %>%
  # H = numero di giorni in avanti su cui prevedere
  forecast(h=365) %>%
  autoplot() + autolayer(validation)

## CALC MAPE
tbats_model = tbats(training, seasonal.periods = c(7, 365))
tbats_forecast = forecast(tbats_model, h=365)
MAPE(tbats_forecast$mean, validation) * 100

# APPRONDIMENTO ANALISI RESIDUI --> https://otexts.com/fpp2/residuals.html

# 15 min
checkresiduals(arima(ts_15_min))
# giorno
checkresiduals(arima(ts_giorno))
# mese
checkresiduals(arima(ts_mese))
# TBATS
checkresiduals(tbats_model)
jarque.bera.test(tbats_model$errors)
qqPlot(tbats_model$errors)
boxplot(u1_completo_giorno$CONSUMO_ATTIVA_PRELEVATA)

###### PREDIZIONE TBATS - CONSUMO ATTIVA PRELEVATA - BUSINESS #######

# nell'end bisogna inserire il mese successivo rispetto a quello
# in cui termina realmente la serie
ts_u1_att_pre <- ts(u1_completo_giorno$CONSUMO_ATTIVA_PRELEVATA,
            start = c(2018,1),
            end = c(2021,1),
            frequency = 365)

# inserire mese successivo rispetto a quello in cui termina la serie
training=window(ts_u1_att_pre, start = c(2018,1), end = c(2020,366))

# per farlo funzionare mettere 24,7,365 se ho le ore
# per farlo funzionare mettere 7,365 se ho raggruppato per giorni
pred.train <- tbats(training, seasonal.periods = c(7,365))
pred.train %>%
# H = numero di giorni in avanti su cui prevedere
  forecast(h=365) %>%
  autoplot()

###### PREDIZIONE TBATS - CONSUMO ATTIVA PRELEVATA - COVID #######

## c(ANNO, NUMERO DI GIORNI NO MESI)
## 2020, 53 sarebbe fino al 22 febbraio circa del 2020
training=window(ts_u1_att_pre, start = c(2018,1), end = c(2020,53))
validation=window(ts_u1_att_pre, start = c(2020,54), end= c(2020, 254))

pred.train <- tbats(training, seasonal.periods = c(7,30,365))
pred.train %>%
  forecast(h=150) %>%
  autoplot() + autolayer(validation)

## CALC MAPE
tbats_model = tbats(training, seasonal.periods = c(7,365))
tbats_forecast = forecast(tbats_model, h=150)
MAPE(tbats_forecast$mean, validation) * 100

## RETE NEURALE TEST

NNAR_U6 <- forecast(nnetar(training), h=300) +
autoplot(ts_u1_att_pre) + autolayer(NNAR_U6, series="NNAR", PI=FALSE) +
xlab("Year") + ylab(" kW") + 
ggtitle("Confronto NN_U6 con e senza pandemia")












