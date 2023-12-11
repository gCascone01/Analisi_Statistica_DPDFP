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

mydata = read_xlsx('./Datasets/PIL/Complete_PIL.xlsx', 1)
data = as.matrix(mydata)

mydata15 = read_xlsx('./Datasets/PIL/less15.xlsx', 1)
data15 = as.matrix(mydata15)

mydataComp = read_xlsx('./Datasets/PIL/15-64.xlsx', 1)
dataComp = as.matrix(mydataComp)

mydata64 = read_xlsx('./Datasets/PIL/64.xlsx', 1)
data64 = as.matrix(mydata64)

#---- DAI DATASET COSTRUISCO LE MATRICI ----

# In questo file ci sono i TOP 5 Paesi per PIL

# Creo le matrici con i dati

dataset = matrix( as.double(matrix(data[,-1],nrow=5)) , nrow=5)

dataset15 = matrix( as.double(matrix(data15[,-1],nrow=5)) , nrow=5)

datasetComp = matrix( as.double(matrix(dataComp[,-1],nrow=5)) , nrow=5)

dataset64 = matrix( as.double(matrix(data64[,-1],nrow=5)) , nrow=5)

# Rimuovo gli anni che non esaminiamo

dataset=dataset[,-c(1:10,43:50)]

dataset15=dataset15[,-c(1:10,43:50)]

datasetComp=datasetComp[,-c(1:10,43:50)]

dataset64=dataset64[,-c(1:10,43:50)]

# Definisco i nomi delle Righe

countries = c("Francia", "Germania", "Italia", "Paesi Bassi", "Spagna")
rownames(dataset) = countries
rownames(dataset15) = countries
rownames(datasetComp) = countries
rownames(dataset64) = countries

# Divido i dataset in 2 matrici, una per variabile

D_Index = seq(1,42, by=2) # Definisco gli indici
V_Index = seq(2,42, by=2) # D = DPM, V = VSL

D_dataset = dataset[, D_Index]
V_dataset = dataset[, V_Index]

D_dataset15 = dataset15[, D_Index]
V_dataset15 = dataset15[, V_Index]

D_datasetComp = datasetComp[, D_Index]
V_datasetComp = datasetComp[, V_Index]

D_dataset64 = dataset64[, D_Index]
V_dataset64 = dataset64[, V_Index]

# Assegno nomi alle colonne

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
V_min = min(V_dataset)

D_max = max(D_dataset)
V_max = max(V_dataset)

D_mean = mean(D_dataset)
V_mean = mean(V_dataset)

D_median = median(D_dataset)
V_median = median(V_dataset)

D_cdv = D_max - D_min #Campo di Variazione
V_cdv = V_max - V_min

resume = rbind(c(D_min,D_max,D_cdv,D_mean,D_median),c(V_min,V_max,V_cdv,V_mean,V_median))
colnames(resume) = c("MIN", "MAX", "CdV", "MEAN", "MEDIAN")
rownames(resume) = c("DPM","VSL")
View(resume)

# Differenza interquartilica: meno sensibile a val estremi
D_diffInterq = unname(quantile(sort(D_dataset), probs = 0.75)) - unname(quantile(sort(D_dataset), probs = 0.25))
V_diffInterq = unname(quantile(sort(V_dataset), probs = 0.75)) - unname(quantile(sort(V_dataset), probs = 0.25))

# Varianza per tutto dataset
D_var = var(as.vector(D_dataset))
V_var = var(as.vector(V_dataset))

# Varianza per colonne (Forse ha più senso farlo per righe, ragioniamoci bene)
D_var_col = apply(D_dataset, 2, var)
V_var_col = apply(D_dataset, 2, var)

# Deviazione Standard totale: valore differisce dalla media aritmetica dei valori, in media quadratica di: x
D_sd = sd(D_dataset)
V_sd = sd(V_dataset)

resume = rbind(c(D_var,D_sd),c(V_var,V_sd))
colnames(resume) = c("VAR", "SD")
rownames(resume) = c("DPM","VSL")
View(resume)

# CV

cv(D_dataset["Italia",])
cv(D_dataset["Germania",])
cv(D_dataset["Francia",])
cv(D_dataset["Paesi Bassi",])
cv(D_dataset["Spagna",])
cv(D_dataset)

cv(V_dataset["Italia",])
cv(V_dataset["Germania",])
cv(V_dataset["Francia",])
cv(V_dataset["Paesi Bassi",])
cv(V_dataset["Spagna",])
cv(V_dataset)

#---- QUARTILI ----

f_quantili(1)
f_quantili(6)
f_quantili(11)
f_quantili(16)

#---- FdDC - Funzione di Distribuzione Empirica Continua ITALIA ----

f_FdDC("Italia")
f_FdDC("Germania")
f_FdDC("Francia")
f_FdDC("Spagna")
f_FdDC("Paesi Bassi")

#---- DEFINISCO SERIE TEMPORALI ----

f_timeSeries(D_dataset, V_dataset)


#---- DEFINISCO LE CLASSI ----

#CREO LE SUDDIVISIONI
labels_classi = c("Very Low", "Low", "Medium", "High", "Very High")
classi_D = c(D_min, D_min+(D_max-D_min)/n_countries, D_min+2*(D_max-D_min)/n_countries, 
             D_min+3*(D_max-D_min)/n_countries, D_min+4*(D_max-D_min)/n_countries, D_max)
classi_V = c(V_min, V_min+(V_max-V_min)/n_countries, V_min+2*(V_max-V_min)/n_countries, 
             V_min+3*(V_max-V_min)/n_countries, V_min+4*(V_max-V_min)/n_countries, V_max)

#---- GRAFICO A BARRE SO VRAPPOSTE ----

f_barre_D("Italia")
f_barre_D("Spagna")
f_barre_D("Germania")
f_barre_D("Spagna")
f_barre_D("Paesi Bassi")