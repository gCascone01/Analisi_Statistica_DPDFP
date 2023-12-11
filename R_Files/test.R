install.packages('readxl')
library(readxl)
install.packages("ggplot2")
library(ggplot2)

#---- DEFINIZIONE DEI PATH ----

#pathGitProject_Gio = "C:/Users/user/Desktop/Magistrale/Statistica e Analisi dei Dati/SAD_Project"
#setwd(pathGitProject_Gio)
#source("Functions.R")

pathGitProject_Ciro = "C:/Users/UTENTE/git/SAD_Project"
setwd(pathGitProject_Ciro)
source("Functions.R")

#---- CARICO I DATASET ----

n_countries = 26

mydata = read_xlsx('./Datasets/PIL/Complete_Dataset.xlsx', 1)
data = as.matrix(mydata)

mydata15 = read_xlsx('./Datasets/PIL/less15.xlsx', 1)
data15 = as.matrix(mydata15)

mydataComp = read_xlsx('./Datasets/PIL/15-64.xlsx', 1)
dataComp = as.matrix(mydataComp)

mydata64 = read_xlsx('./Datasets/PIL/64.xlsx', 1)
data64 = as.matrix(mydata64)

#---- DATASET to MATRICI ----

dataset = matrix( as.double(matrix(data[,-1],nrow=n_countries)) , nrow=n_countries)

dataset15 = matrix( as.double(matrix(data15[,-1],nrow=n_countries)) , nrow=n_countries)

datasetComp = matrix( as.double(matrix(dataComp[,-1],nrow=n_countries)) , nrow=n_countries)

dataset64 = matrix( as.double(matrix(data64[,-1],nrow=n_countries)) , nrow=n_countries)

#---- PREPARAZIONE dataset ----

#Rimozione anni che non analizziamo

dataset=dataset[,-c(1:10,43:50)]

dataset15=dataset15[,-c(1:10,43:50)]

datasetComp=datasetComp[,-c(1:10,43:50)]

dataset64=dataset64[,-c(1:10,43:50)]

#Definizione nomi righe con nomi nazioni

countries = c("Austria", "Belgio", "Bulgaria", "Croazia", "Danimarca", "Estonia", "Finlandia", "Francia", "Germania", "Grecia", "Ungheria", "Irlanda", "Italia", "Lettonia", "Lituania", "Lussemburgo", "Malta", "Paesi Bassi", "Polonia", "Portogallo", "Repubblica Ceca", "Romania", "Slovacchia", "Slovenia", "Spagna", "Svezia")
rownames(dataset) = countries
rownames(dataset15) = countries
rownames(datasetComp) = countries
rownames(dataset64) = countries

# Variabili (DPM (D) e VSL (D)) in DATASET SEPARATI

D_Index = seq(1,42, by=2) # DEFINISCO GLI INDICI DELLE COLONNE PER OGNI VARIABILE
V_Index = seq(2,42, by=2) # D sta per Death, V per Value

D_dataset = dataset[, D_Index]
V_dataset = dataset[, V_Index]

D_dataset15 = dataset15[, D_Index]
V_dataset15 = dataset15[, V_Index]

D_datasetComp = datasetComp[, D_Index]
V_datasetComp = datasetComp[, V_Index]

D_dataset64 = dataset64[, D_Index]
V_dataset64 = dataset64[, V_Index]

years = seq(1995,2015)
colnames(D_dataset) = years
colnames(V_dataset) = years

colnames(D_dataset15) = years
colnames(V_dataset15) = years

colnames(D_datasetComp) = years
colnames(V_datasetComp) = years

colnames(D_dataset64) = years
colnames(V_dataset64) = years

#---- CALCOLO INDICI CENTRALITÀ e DISPERSIONE ----

D_min = min(D_dataset)
D_min15 = min(D_dataset15)
D_minComp = min(D_datasetComp)
D_min64 = min(D_dataset64)

V_min = min(V_dataset)
V_min15 = min(V_dataset15)
V_minComp = min(V_datasetComp)
V_min64 = min(V_dataset64)


D_max = max(D_dataset)
D_max15 = max(D_dataset15)
D_maxComp = max(D_datasetComp)
D_max64 = max(D_dataset64)

V_max = max(V_dataset)
V_max15 = max(V_dataset15)
V_maxComp = max(V_datasetComp)
V_max64 = max(V_dataset64)


D_mean = mean(D_dataset)
D_mean15 = mean(D_dataset15)
D_meanComp = mean(D_datasetComp)
D_mean64 = mean(D_dataset64)

# Media aritmetica
V_mean = mean(V_dataset)
V_mean15 = mean(V_dataset15)
V_meanComp = mean(V_datasetComp)
V_mean64 = mean(V_dataset64)


D_median = median(D_dataset)
D_median15 = median(D_dataset15)
D_medianComp = median(D_datasetComp)
D_median64 = median(D_dataset64)

V_median = median(V_dataset)
V_median15 = median(V_dataset15)
V_medianComp = median(V_datasetComp)
V_median64 = median(V_dataset64)



D_indiciCentr = rbind(c(D_min,D_max,D_mean,D_median),c(D_min15,D_max15,D_mean15,D_median15),c(D_minComp,D_maxComp,D_meanComp,D_medianComp),c(D_min64,D_max64,D_mean64,D_median64))
colnames(D_indiciCentr) = c("MIN", "MAX", "MEDIA", "MEDIANA")
rownames(D_indiciCentr) = c("ALL","LESS 15","COMP 15 - 64","GREATER 64")

V_indiciCentr = rbind(c(V_min,V_max,V_mean,V_median),c(V_min15,V_max15,V_mean15,V_median15),c(V_minComp,V_maxComp,V_meanComp,V_medianComp),c(V_min64,V_max64,V_mean64,V_median64))
colnames(V_indiciCentr) = c("MIN", "MAX", "MEDIA", "MEDIANA")
rownames(V_indiciCentr) = c("ALL","LESS 15","COMP 15 - 64","GREATER 64")

#---- CALCOLO INDICI DISPERSIONE ----

# Campo di Variazione

D_cdv = D_max - D_min
D_cdv15 = D_max15 - D_min15
D_cdvComp = D_maxComp - D_minComp
D_cdv64 = D_max64 - D_min64

V_cdv = V_max - V_min
V_cdv15 = V_max15 - V_min15
V_cdvComp = V_maxComp - V_minComp
V_cdv64 = V_max64 - V_min64

# Differenza interquartilica: meno sensibile a val estremi
D_diffInterq = unname(quantile(sort(D_dataset), probs = 0.75)) - unname(quantile(sort(D_dataset), probs = 0.25))
D_diffInterq15 = unname(quantile(sort(D_dataset15), probs = 0.75)) - unname(quantile(sort(D_dataset15), probs = 0.25))
D_diffInterqComp = unname(quantile(sort(D_datasetComp), probs = 0.75)) - unname(quantile(sort(D_datasetComp), probs = 0.25))
D_diffInterq64 = unname(quantile(sort(D_dataset64), probs = 0.75)) - unname(quantile(sort(D_dataset64), probs = 0.25))

V_diffInterq = unname(quantile(sort(V_dataset), probs = 0.75)) - unname(quantile(sort(V_dataset), probs = 0.25))
V_diffInterq15 = unname(quantile(sort(V_dataset15), probs = 0.75)) - unname(quantile(sort(V_dataset15), probs = 0.25))
V_diffInterqComp = unname(quantile(sort(V_datasetComp), probs = 0.75)) - unname(quantile(sort(V_datasetComp), probs = 0.25))
V_diffInterq64 = unname(quantile(sort(V_dataset64), probs = 0.75)) - unname(quantile(sort(V_dataset64), probs = 0.25))

# Varianza
D_var = var(as.vector(D_dataset))
D_var15 = var(as.vector(D_dataset15))
D_varComp = var(as.vector(D_datasetComp))
D_var64 = var(as.vector(D_dataset64))

V_var = var(as.vector(V_dataset))
V_var15 = var(as.vector(V_dataset15))
V_varComp = var(as.vector(V_datasetComp))
V_var64 = var(as.vector(V_dataset64))

# Varianza per colonne (Forse ha più senso farlo per righe, ragioniamoci bene) Cosa fare ?????
# D_var_col = apply(D_dataset, 2, var)
# V_var_col = apply(D_dataset, 2, var)

# Deviazione Standard totale: valore differisce dalla media aritmetica dei valori, in media quadratica di: x
D_sd = sd(D_dataset)
D_sd15 = sd(D_dataset15)
D_sdComp = sd(D_datasetComp)
D_sd64 = sd(D_dataset64)

V_sd = sd(V_dataset)
V_sd15 = sd(V_dataset15)
V_sdComp = sd(V_datasetComp)
V_sd64 = sd(V_dataset64)


D_indiciDisp = rbind(c(D_cdv,D_diffInterq,D_var,D_sd),c(D_cdv15,D_diffInterq15,D_var15,D_sd15),c(D_cdvComp,D_diffInterqComp,D_varComp,D_sdComp),c(D_cdv64,D_diffInterq64,D_var64,D_sd64))
colnames(D_indiciDisp) = c("CdV", "DIFF. INTERQ.", "VARIANZA", "DEVIAZ. STD")
rownames(D_indiciDisp) = c("ALL","LESS 15","COMP 15 - 64","GREATER 64")

V_indiciDisp = rbind(c(V_cdv,V_diffInterq,V_var,V_sd),c(V_cdv15,V_diffInterq15,V_var15,V_sd15),c(V_cdvComp,V_diffInterqComp,V_varComp,V_sdComp),c(V_cdv64,V_diffInterq64,V_var64,V_sd64))
colnames(V_indiciDisp) = c("CdV", "DIFF. INTERQ.", "VARIANZA", "DEVIAZ. STD")
rownames(V_indiciDisp) = c("ALL","LESS 15","COMP 15 - 64","GREATER 64")

# CV

# cv(D_dataset["Italia",])
# cv(D_dataset["Germania",])
# cv(D_dataset["Francia",])
# cv(D_dataset["Paesi Bassi",])
# cv(D_dataset["Spagna",])
# cv(D_dataset)
# 
# cv(V_dataset["Italia",])
# cv(V_dataset["Germania",])
# cv(V_dataset["Francia",])
# cv(V_dataset["Paesi Bassi",])
# cv(V_dataset["Spagna",])
# cv(V_dataset)

#---- QUARTILI ----

f_quantili(1,D_dataset,V_dataset, "ALL")
f_quantili(6,D_dataset,V_dataset, "ALL")
f_quantili(11,D_dataset,V_dataset, "ALL")
f_quantili(16,D_dataset,V_dataset, "ALL")

f_quantili(1,D_dataset15,V_dataset15, "LESS 15")
f_quantili(6,D_dataset15,V_dataset15, "LESS 15")
f_quantili(11,D_dataset15,V_dataset15, "LESS 15")
f_quantili(16,D_dataset15,V_dataset15, "LESS 15")

f_quantili(1,D_datasetComp,V_datasetComp, "COMP 15 - 64")
f_quantili(6,D_datasetComp,V_datasetComp, "COMP 15 - 64")
f_quantili(11,D_datasetComp,V_datasetComp, "COMP 15 - 64")
f_quantili(16,D_datasetComp,V_datasetComp, "COMP 15 - 64")

f_quantili(1,D_dataset64,V_dataset64, "GREATER 64")
f_quantili(6,D_dataset64,V_dataset64, "GREATER 64")
f_quantili(11,D_dataset64,V_dataset64, "GREATER 64")
f_quantili(16,D_dataset64,V_dataset64, "GREATER 64")

#---- FdDC - Funzione di Distribuzione Empirica Continua ----

for(country in countries){
  f_FdDC(country)
}

#---- DEFINISCO SERIE TEMPORALI ----

f_timeSeries(D_dataset, "DPM ALL")
f_timeSeries(V_dataset, "VSL ALL")

f_timeSeries(D_dataset15, "DPM LESS 15")
f_timeSeries(V_dataset15, "VSL LESS 15")

f_timeSeries(D_datasetComp, "DPM COMP 15 - 64")
f_timeSeries(V_datasetComp, "VSL COMP 15 - 64")

f_timeSeries(D_dataset64, "DPM GREATER 64")
f_timeSeries(V_dataset64, "VSL GREATER 64")


#---- DEFINISCO LE CLASSI ----

#CREO LE SUDDIVISIONI
labels_classi = c("Very Low", "Low", "Medium", "High", "Very High")
classi_D = c(D_min, D_min+(D_max-D_min)/n_countries, D_min+2*(D_max-D_min)/n_countries, 
             D_min+3*(D_max-D_min)/n_countries, D_min+4*(D_max-D_min)/n_countries, D_max)
classi_V = c(V_min, V_min+(V_max-V_min)/n_countries, V_min+2*(V_max-V_min)/n_countries, 
             V_min+3*(V_max-V_min)/n_countries, V_min+4*(V_max-V_min)/n_countries, V_max)

#---- GRAFICO A BARRE SOVRAPPOSTE ----

for(country in countries){
  f_barre_sovrapp_per_eta(country,D_dataset15,D_datasetComp,D_dataset64,"DPM")
}


#---- STATISTICA DESCRITTIVA BIVARIATA ----

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! vedi tabella a pag. 136, le tabelle le dobbiamo fare anche noi cosi? righe e colonne

# d1 = D_dataset["Germania",]
# d2 = V_dataset["Germania",]

d1 = colMeans(D_dataset)
d2 = colMeans(V_dataset)

# plot(d1,d2, col="red")
# abline(v=mean(d1), lty=1, col="magenta")
# abline(h=mean(d2), lty=1, col="magenta")
# abline(v=median(d1), lty=2, col="blue")
# abline(h=median(d2), lty=2, col="blue")
# legend("topright",c("Media", "Mediana"), pch=0, col=c("magenta", "blue"))

cov <- cov(d1, d2)
cov

cor <- cor(d1,d2)
cor

plot(d1,d2, col="red")
model <- lm(d2~d1)
abline(model, col="blue")

