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
set.seed(1)

setwd("C:/Users/danie/Desktop/DSLAB")

###### dati ottenuti tramite script python
u6_compl <- read.csv("DATA/U6_DATECOMPLETE_CLEANED.csv")

u6_compl$DATA <-as.POSIXct(u6_compl$DATA, format='%d/%m/%Y %H:%M')

######## ANALISI DESCRITTIVE - 15 MIN #######
ts_15_min = ts(u6_compl$CONSUMO_ATTIVA_PRELEVATA,
               frequency = 365, start = c(2018, 1), end = c(2020,365))

# Grafico consumo attiva prelevata
plot(ts_15_min, 
     col="yellow", 
     xlab = "Data", 
     ylab = "kw", 
     main="U6 - Consumo attivo prelevata", 
     cex.main=0.8, 
     type='l')

# Decomposizione
decomp <- stl(ts_15_min, s.window = "periodic", na.action = na.omit)
plot(decomp)

# Stagionalità ogni 15 min
ggseasonplot(ts_15_min, year.labels=TRUE, year.labels.left=TRUE)+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x=element_blank()) +  # to center the plot title
  ylab("Consumo attiva prelevata") +
  xlab("15 min frequenza") +
  ggtitle("Stagionalità: C.A.P ogni 15 min")

# Verifica residui ARIMA
# notiamo presenza correlazione, informazioni utili non utilizzate dal modello
checkresiduals(arima(ts_15_min))

# dividiamo dati per giugno e suo complementare
u6_compl_no_giugno <- subset(u6_compl, DATA < "2020-06-01 00:00" | DATA > "2020-06-30 23:45")
u6_compl_giugno <- subset(u6_compl, DATA < "2020-07-01 00:00" & DATA > "2020-05-31 23:45")

# convertiamo gli 0 in NA
u6_compl_no_giugno[u6_compl_no_giugno == 0] <- NA
statsNA(u6_compl_no_giugno$CONSUMO_ATTIVA_PRELEVATA)

#### TEST IMPUTAZIONE SOLO SU "CONS. ATT. PREL."

# considero solo i NAN che non sono di Giugno
no_giugno_NAN <- u6_compl_no_giugno$CONSUMO_ATTIVA_PRELEVATA
no_giugno_NAN <- as.data.frame(no_giugno_NAN)

# elimino NAN per poi usare tale dataset per la verifica dell'imputazione
no_giugno_NAN_test <- na.omit(no_giugno_NAN)

# produzione artificiale dei NAN
no_giugno_NAN_train <- prodNA(no_giugno_NAN_test, noNA = 0.00202)

# test algoritmi imputazione
imputation <- na_interpolation(no_giugno_NAN_train)
imputation_2 <- na_seadec(no_giugno_NAN_train, find_frequency=TRUE)
imputation_3 <- na_kalman(no_giugno_NAN_train)
imputation_4 <- na_mean(no_giugno_NAN_train)
imputation_5 <- na_locf(no_giugno_NAN_train)
imputation_6 <- na_seasplit(no_giugno_NAN_train, find_frequency=TRUE)
imputation_7 <- na_ma(no_giugno_NAN_train)

# grafici x ogni imputazione
ggplot_na_imputations(no_giugno_NAN_train, imputation, no_giugno_NAN_test)
ggplot_na_imputations(no_giugno_NAN_train, imputation_2, no_giugno_NAN_test)
ggplot_na_imputations(no_giugno_NAN_train, imputation_3, no_giugno_NAN_test)
ggplot_na_imputations(no_giugno_NAN_train, imputation_4, no_giugno_NAN_test)
ggplot_na_imputations(no_giugno_NAN_train, imputation_5, no_giugno_NAN_test)
ggplot_na_imputations(no_giugno_NAN_train, imputation_6, no_giugno_NAN_test)
ggplot_na_imputations(no_giugno_NAN_train, imputation_7, no_giugno_NAN_test)

# indici dei valori imputati
indici_nulli <- which(is.na(no_giugno_NAN_train), arr.ind = TRUE)
indici_nulli <- as.data.frame(indici_nulli)
riga <- indici_nulli$row
riga <- as.vector(riga)

# creo DF per testare il grado di correttezza imputazioni
db_test_imputation <- cbind(imputation[riga,],
                 imputation_2[riga,],
                 imputation_3[riga,],
                 imputation_4[riga,],
                 imputation_5[riga,],
                 imputation_6[riga,],
                 imputation_7[riga,],
                 no_giugno_NAN_test[riga,])

db_test_imputation <- as.data.frame(db_test_imputation)

names(db_test_imputation) <- c("interpolation", 
                    "seadec", 
                    "kalman", 
                    "mean",
                    "locf",
                    "seasplit",
                    "ma",
                    "original")

# calcolo del MSE, del MAPE, del RMSE e del R2_Score
MSE(db_test_imputation$interpolation, db_test_imputation$original)
MSE(db_test_imputation$seadec, db_test_imputation$original)
MSE(db_test_imputation$kalman, db_test_imputation$original)
MSE(db_test_imputation$mean, db_test_imputation$original)
MSE(db_test_imputation$locf, db_test_imputation$original)
MSE(db_test_imputation$seasplit, db_test_imputation$original)
MSE(db_test_imputation$ma, db_test_imputation$original)

MAPE(db_test_imputation$interpolation, db_test_imputation$original)*100
MAPE(db_test_imputation$seadec, db_test_imputation$original)*100
MAPE(db_test_imputation$kalman, db_test_imputation$original)*100
MAPE(db_test_imputation$mean, db_test_imputation$original)*100
MAPE(db_test_imputation$locf, db_test_imputation$original)*100
MAPE(db_test_imputation$seasplit, db_test_imputation$original)*100
MAPE(db_test_imputation$ma, db_test_imputation$original)*100

RMSE(db_test_imputation$interpolation, db_test_imputation$original)
RMSE(db_test_imputation$seadec, db_test_imputation$original)
RMSE(db_test_imputation$kalman, db_test_imputation$original)
RMSE(db_test_imputation$mean, db_test_imputation$original)
RMSE(db_test_imputation$locf, db_test_imputation$original)
RMSE(db_test_imputation$seasplit, db_test_imputation$original)
RMSE(db_test_imputation$ma, db_test_imputation$original)

# R^2
R2_Score(db_test_imputation$interpolation, db_test_imputation$original)
R2_Score(db_test_imputation$seadec, db_test_imputation$original)
R2_Score(db_test_imputation$kalman, db_test_imputation$original)
R2_Score(db_test_imputation$mean, db_test_imputation$original)
R2_Score(db_test_imputation$locf, db_test_imputation$original)
R2_Score(db_test_imputation$seasplit, db_test_imputation$original)
R2_Score(db_test_imputation$ma, db_test_imputation$original)

# dai valori ottenuti notiamo come in media Kalman restituisca un risultato migliore

# imputiamo i valori nulli (tranne di Giugno) con Kalman
u6_inserimento_val_nulli <- na_kalman(u6_compl_no_giugno)

# imputiamo i valori di u6_compl_giugno come NA
u6_compl_giugno$CONSUMO_ATTIVA_PRELEVATA[u6_compl_giugno$CONSUMO_ATTIVA_PRELEVATA != -1] <- NA
u6_compl_giugno$CONSUMO_REATTIVA_INDUTTIVA_PRELEVATA[u6_compl_giugno$CONSUMO_REATTIVA_INDUTTIVA_PRELEVATA != -1] <- NA
u6_compl_giugno$POTENZA_MASSIMA[u6_compl_giugno$POTENZA_MASSIMA != -1] <- NA

# uniamo dataset imputato con kalman (senza giugno)
# al dataset contenente solo giugno (con tutti i val null)
u6_for_tbats <- rbind(u6_inserimento_val_nulli, u6_compl_giugno)

# ordiniamo per date
u6_for_tbats <- u6_for_tbats[order(u6_for_tbats$DATA), ]

######### ARRICCHIMENTO DI U6 CON I DATI METEO

# CREIAMO IL DATASET DI U6 AGGREGATO PER ORE
u6_for_tbats$SOLO_ORA <- hour(u6_for_tbats$DATA)
u6_for_tbats$SOLO_DATA <-format(u6_for_tbats$DATA, format= "%Y-%m-%d")

# creiamo un'altra colonna concatenando data e ora per poi aggregare
u6_for_tbats$CONCAT_DATA_ORA <- paste(u6_for_tbats$SOLO_DATA,
                                      u6_for_tbats$SOLO_ORA)

u6_for_tbats_ora = aggregate(u6_for_tbats$CONSUMO_ATTIVA_PRELEVATA,
                                  by=list(u6_for_tbats$CONCAT_DATA_ORA), 
                                  sum)

colnames(u6_for_tbats_ora)[1] <- "Data"
colnames(u6_for_tbats_ora)[2] <- "CONSUMO_ATTIVA_PRELEVATA"


u6_for_tbats_ora_2 = aggregate(u6_for_tbats$CONSUMO_REATTIVA_INDUTTIVA_PRELEVATA,
                                    by=list(u6_for_tbats$CONCAT_DATA_ORA), 
                                    sum)

colnames(u6_for_tbats_ora_2)[1] <- "Data"
colnames(u6_for_tbats_ora_2)[2] <- "CONSUMO_REATTIVA_INDUTTIVA_PRELEVATA"


u6_for_tbats_ora_3 = aggregate(u6_for_tbats$POTENZA_MASSIMA,
                                    by=list(u6_for_tbats$CONCAT_DATA_ORA), 
                                    mean)

colnames(u6_for_tbats_ora_3)[1] <- "Data"
colnames(u6_for_tbats_ora_3)[2] <- "POTENZA_MASSIMA"


# creiamo un unico dataset
u6_ora_meteo <- cbind(u6_for_tbats_ora,
                      u6_for_tbats_ora_2,
                      u6_for_tbats_ora_3)

# elimino colonne superflue
u6_ora_meteo <- subset(u6_ora_meteo, select = -c(3,5))

u6_ora_meteo$Data <- as.POSIXct(u6_ora_meteo$Data, format='%Y-%m-%d %H')

# leggo dati meteo
dati_meteo <- read.csv("DATA/meteo_completo.csv")
dati_meteo$data <- as.POSIXct(dati_meteo$data, format='%d/%m/%Y %H:%M')


# fix bug colonna kelvin
dati_meteo$kelvin <- dati_meteo$temp + 273.15

# colonna con stesso nome per svolgere join
colnames(u6_ora_meteo)[1] <- "data"
u6_meteo <- merge(x=u6_ora_meteo, y=dati_meteo, by="data",all.x=TRUE)

# elimino colonne superflue
u6_meteo <- subset(u6_meteo, select = -c(9,8,14,12))


# cross-correlation sulle ore
corr = Ccf(as.ts(u6_meteo$temp), as.ts(u6_meteo$CONSUMO_ATTIVA_PRELEVATA))
corr_1 = Ccf(as.ts(u6_meteo$CONSUMO_ATTIVA_PRELEVATA), as.ts(u6_meteo$temp), 50)

# creiamo dataset per giugno e complementare, poi imputiamo anche i dati meteo sul complementare
u6_meteo_no_giugno <- subset(u6_meteo, data < "2020-06-01 00:00" | data > "2020-06-30 23:00")
u6_meteo_giugno <- subset(u6_meteo, data < "2020-07-01 00:00" & data > "2020-05-31 23:00")

# imputiamo TUTTI i valori nulli
u6_meteo_no_giugno <- na_kalman(u6_meteo_no_giugno)

# otteniamo dataset completo x ore
u6_completo_ore <- rbind(u6_meteo_no_giugno, u6_meteo_giugno)

ts_u6_completo_ore <- ts(u6_completo_ore$CONSUMO_ATTIVA_PRELEVATA,
                            start = c(2018,1),
                            frequency = 24)

# AGGREGHIAMO X GIORNO
u6_completo_ore$SOLO_DATA <- format(u6_completo_ore$data, format= "%Y-%m-%d")

# solo attributi x media
u6_completo_giorno_mean <- aggregate(list(temp=u6_completo_ore$temp, dwpt=u6_completo_ore$dwpt,
                                          rhum=u6_completo_ore$rhum,
                                     wdir=u6_completo_ore$wdir, wspd=u6_completo_ore$wspd,
                                     pres=u6_completo_ore$pres, kelvin=u6_completo_ore$kelvin, 
                                     POTENZA_MASSIMA=u6_completo_ore$POTENZA_MASSIMA), 
                                     by = list(data=u6_completo_ore$SOLO_DATA), mean)

# solo attributi x somma
u6_completo_giorno_sum <- aggregate(list(CONSUMO_ATTIVA_PRELEVATA=u6_completo_ore$CONSUMO_ATTIVA_PRELEVATA, 
                                         CONSUMO_REATTIVA_INDUTTIVA_PRELEVATA=u6_completo_ore$CONSUMO_REATTIVA_INDUTTIVA_PRELEVATA), 
                                     by = list(data=u6_completo_ore$SOLO_DATA), sum)

# u6 completo giorno
u6_completo_giorno = merge(x=u6_completo_giorno_mean, y=u6_completo_giorno_sum, by="data")
u6_completo_giorno$data <- as.POSIXct(u6_completo_giorno$data, format='%Y-%m-%d')


# cross-correlation sui giorni
corr_temp_cons = Ccf(u6_completo_giorno$temp, u6_completo_giorno$CONSUMO_ATTIVA_PRELEVATA)
corr_cons_temp = Ccf(as.ts(u6_completo_giorno$CONSUMO_ATTIVA_PRELEVATA), as.ts(u6_completo_giorno$temp))
plot(corr_cons_temp, main = "U6 - CCF tra consumi e temperatura", ylab = "CCF")

########## VALIDAZIONE DEL TBATS SU MAGGIO 2020

ts_u6_completo_giorno <- ts(u6_completo_giorno$CONSUMO_ATTIVA_PRELEVATA,
                       start = c(2018,1),
                       end = c(2021,1),
                       frequency = 365)

# TRAINING -> PERIODO PRIMA DI MAGGIO
# VALIDATION -> MESE DI MAGGIO
training = window(ts_u6_completo_giorno, start = c(2018,1), end = c(2020,121))
validation = window(ts_u6_completo_giorno, start = c(2020,122), end = c(2020,152))

# prevedo Maggio (H=31)
pred.train <- tbats(training, seasonal.periods = c(7, 30, 365))
pred.train %>%
  forecast(h=31) %>%
  autoplot() + autolayer(validation)

# Controllo sul valore del MAPE
tbats_model = tbats(training, seasonal.periods = c(7,30, 365))
tbats_forecast = forecast(tbats_model, h=31)

# MAPE DANIELE+ALE -> 11.18%
MAPE(tbats_forecast$mean, validation) * 100

##### PREVISIONI SU GIUGNO (USATI X SOSTITUIRE I NAN IN GIUGNO 2020)

# TRAINING -> FINO A MAGGIO
training <- window(ts_u6_completo_giorno, start = c(2018,1), end = c(2020,152))
pred.train <- tbats(training, seasonal.periods = c(7, 30, 365))

pred.train %>%
  forecast(h=30) %>%
  autoplot()

tbats_model = tbats(training, seasonal.periods = c(7,30, 365))
tbats_forecast = forecast(tbats_model, h=30)

# valori previsti dal TBATS da sostituire a Giugno 2020
tbats_forecast$mean
valori_forecasting <- as.data.frame(tbats_forecast$mean)

# sostituisco i valori del mese di Giugno 2020
u6_completo_giorno[(u6_completo_giorno$data<"2020-07-01" & u6_completo_giorno$data>"2020-05-31"), "CONSUMO_ATTIVA_PRELEVATA"] <- tbats_forecast$mean

# write.table(u6_completo_giorno, "./u6_completo_giorno.csv",sep=";", row.names = FALSE)


####### ANALISI DESCRITTIVE X MESE ########
u6_completo_mese = u6_completo_giorno %>%
  mutate(mese = format(data, "%m"), anno = format(data, "%Y")) %>%
  group_by(mese, anno) %>%
  summarise(CONSUMO_ATTIVA_PRELEVATA = mean(CONSUMO_ATTIVA_PRELEVATA))

u6_completo_mese <- u6_completo_mese[order(u6_completo_mese$anno), ]

ts_mese = ts(u6_completo_mese$CONSUMO_ATTIVA_PRELEVATA,
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
ts_mese_na_omit = ts(na.omit(u6_completo_mese$CONSUMO_ATTIVA_PRELEVATA),
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

ts_giorno = ts(u6_completo_giorno$CONSUMO_ATTIVA_PRELEVATA,
             frequency = 365, start = c(2018,1))

######## ACF
acf(ts_giorno, lag.max = 30)
acf(ts_u6_completo_ore, lag.max = 96, na.action = na.pass)

# Grafico x giorno
plot(ts_giorno, 
     col="red",
     ylab = "C.A.P",
     xlab = "Giorni",
     main="U6 - C.A.P - Aggregata per giorno",
     cex.main=0.8,
     type='l')

# Decomposizione 
ts_giorno_na_omit = ts(na.omit(u6_completo_giorno$CONSUMO_ATTIVA_PRELEVATA),
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
boxplot(u6_completo_giorno$CONSUMO_ATTIVA_PRELEVATA)

###### PREDIZIONE TBATS - TEST 2019 - NON VA BENE #######

ts_u6_att_pre <- ts(u6_completo_giorno$CONSUMO_ATTIVA_PRELEVATA,
                    start = c(2018,1),
                    end = c(2020,366),
                    frequency = 365)

training=window(ts_u6_att_pre, start = c(2018,1), end = c(2018,365))
validation = window(ts_u6_att_pre, start = c(2019, 1), end = c(2019,365))

pred.train <- tbats(training, seasonal.periods = c(7,30,365))
pred.train %>%
  forecast(h=365) %>%
  autoplot() + autolayer(validation)

## CALC MAPE
tbats_model = tbats(training, seasonal.periods = c(7,30, 365))
tbats_forecast = forecast(tbats_model, h=365)
MAPE(tbats_forecast$mean, validation) * 100

###### PREDIZIONE TBATS - CONSUMO ATTIVA PRELEVATA - BUSINESS #######

ts_u6_att_pre <- ts(u6_completo_giorno$CONSUMO_ATTIVA_PRELEVATA,
                    start = c(2018,1),
                    end = c(2021,1),
                    frequency = 365)

training=window(ts_u6_att_pre, start = c(2018,1), end = c(2020,366))

pred.train <- tbats(training, seasonal.periods = c(7,365))
pred.train %>%
  forecast(h=365) %>%
  autoplot()

###### PREDIZIONE TBATS - CONSUMO ATTIVA PRELEVATA - COVID #######

training=window(ts_u6_att_pre, start = c(2018,1), end = c(2020,53))
validation=window(ts_u6_att_pre, start = c(2020,54), end= c(2020, 254))

pred.train <- tbats(training, seasonal.periods = c(7,30,365))
pred.train %>%
  forecast(h=150) %>%
  autoplot() + autolayer(validation)

## CALC MAPE
tbats_model = tbats(training, seasonal.periods = c(7,365))
tbats_forecast = forecast(tbats_model, h=150)
MAPE(tbats_forecast$mean, validation) * 100

##### VERIFICA CORRELAZIONE TRA CONSUMI DI U1 E DI U6
ts_u6_completo_giorno <- ts(u6_completo_giorno$CONSUMO_ATTIVA_PRELEVATA,
                            start = c(2018,1),
                            end = c(2021,1),
                            frequency = 365)

u1_completo_giorno <- read.csv("./DATA/u1_completo_giorno.csv")

ts_u1_completo_giorno <- ts(u1_completo_giorno$CONSUMO_ATTIVA_PRELEVATA,
                            start = c(2018,1),
                            end = c(2021,1),
                            frequency = 365)

Ccf(ts_u6_completo_giorno, ts_u1_completo_giorno)


##### CORRELAZIONE TRA TEMPERATURE

Ccf(ts_u6_completo_giorno, ts_u1_completo_giorno)

Ccf(u6_completo_giorno$CONSUMO_ATTIVA_PRELEVATA, u6_completo_giorno$temp)
Ccf(u6_completo_giorno$temp, u6_completo_giorno$temp)
Ccf(u1_completo_giorno$temp, u6_completo_giorno$temp)

