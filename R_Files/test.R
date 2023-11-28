install.packages('xlsx')
library(xlsx)

# CARICO IL DATASET COMPLETO

mydata = read.xlsx('C:/Users/user/Desktop/Magistrale/Statistica e Analisi dei Dati/SAD_Project/Datasets/Complete_Dataset.xlsx', 1)
data = as.matrix(mydata)

# CREO LE DUE MATRICI

dataset = matrix(data[,-1],nrow=26) # nrow = 27 quando aggiungiamo Cipro
dataset = matrix( as.double(dataset) , nrow=26) # idem

countries = c("Austria", "Belgio", "Bulgaria", "Croazia", "Danimarca", "Estonia", "Finlandia", "Francia", "Germania",
              "Grecia", "Ungheria", "Irlanda", "Italia", "Lettonia", "Lituania", "Lussemburgo", "Malta", "Paesi Bassi", "Polonia",
              "Portogallo", "Repubblica Ceca", "Romania", "Slovacchia", "Slovenia", "Spagna", "Svezia")
rownames(dataset) = countries

D_Index = seq(1,60, by=2) # DEFINISCO GLI INDICI DELLE COLONNE PER OGNI VARIABILE
V_Index = seq(2,60, by=2) # D sta per Death, V per Value

D_dataset = dataset[, D_Index]
V_dataset = dataset[, V_Index]

years = seq(1990,2019)
colnames(D_dataset) = years
colnames(V_dataset) = years

# CALCOLO MINIMO, MASSIMO E CAMPO DI VARIAZIONE PER LE DUE VARIABILI

D_min = min(D_dataset)
V_min = min(V_dataset)

D_max = max(D_dataset)
V_max = max(V_dataset)

D_cdv = D_max - D_min
V_cdv = V_max - V_min

resume = rbind(c(D_min,D_max,D_cdv),c(V_min,V_max,V_cdv))
colnames(resume) = c("MIN", "MAX", "CdV")
rownames(resume) = c("DPM","VSL")
View(resume)

# DEFINISCO LE CLASSI
labels_classi = c("Very Low", "Low", "Medium", "High", "Very High")
classi_D = c(D_min-1, 167.8986, 276.0472, 348.1958, 492.3444, D_max+1)
classi_V = c(V_min-1, 2.5008, 4.1486, 5.7946, 7.4442, V_max+1)


# DEFINISCO LA DIVISIONE IN CLASSI PER I 5 INTERVALLI TEMPORALI (Variabile DPM)

#devo modificare in ogni cut right=false

intervallo = cbind(D_dataset[,1:6])
avg_intervallo = apply(intervallo, 1, mean)
classi_D_1995 = cut(avg_intervallo, breaks=classi_D, labels=labels_classi)

intervallo = cbind(D_dataset[,7:12])
avg_intervallo = apply(intervallo, 1, mean)
classi_D_2001 = cut(avg_intervallo, breaks=classi_D, labels=labels_classi)

intervallo = cbind(D_dataset[,13:18])
avg_intervallo = apply(intervallo, 1, mean)
classi_D_2007 = cut(avg_intervallo, breaks=classi_D, labels=labels_classi)

intervallo = cbind(D_dataset[,19:24])
avg_intervallo = apply(intervallo, 1, mean)
classi_D_2013 = cut(avg_intervallo, breaks=classi_D, labels=labels_classi)

intervallo = cbind(D_dataset[,25:30])
avg_intervallo = apply(intervallo, 1, mean)
classi_D_2019 = cut(avg_intervallo, breaks=classi_D, labels=labels_classi)



# DEFINISCO LA DIVISIONE IN CLASSI PER I 5 INTERVALLI TEMPORALI (Variabile VSL)

intervallo = cbind(V_dataset[,1:6])
avg_intervallo = apply(intervallo, 1, mean)
classi_V_1995 = cut(avg_intervallo, breaks=classi_V, labels=labels_classi)

intervallo = cbind(V_dataset[,7:12])
avg_intervallo = apply(intervallo, 1, mean)
classi_V_2001 = cut(avg_intervallo, breaks=classi_V, labels=labels_classi)

intervallo = cbind(V_dataset[,13:18])
avg_intervallo = apply(intervallo, 1, mean)
classi_V_2007 = cut(avg_intervallo, breaks=classi_V, labels=labels_classi)

intervallo = cbind(V_dataset[,19:24])
avg_intervallo = apply(intervallo, 1, mean)
classi_V_2013 = cut(avg_intervallo, breaks=classi_V, labels=labels_classi)

intervallo = cbind(V_dataset[,25:30])
avg_intervallo = apply(intervallo, 1, mean)
classi_V_2019 = cut(avg_intervallo, breaks=classi_V, labels=labels_classi)

# DEFINISCO I GRAFICI DI FREQUENZA PER OGNI VARIABILE E CLASSE

barplot(table(classi_D_1995), col=2:3, main="DPM: Frequenze Assolute",
        sub="Intervallo 1990-1995", ylim=c(0,16), space=0.2)
barplot(table(classi_D_2001),col=4:5, main="DPM: Frequenze Assolute",
        sub="Intervallo 1996-2001", ylim=c(0,16), space=1)
barplot(table(classi_D_2007),col=6:7, main="DPM: Frequenze Assolute",
        sub="Intervallo 2002-2007", ylim=c(0,16), space=1)
barplot(table(classi_D_2013),col=2:3, main="DPM: Frequenze Assolute",
        sub="Intervallo 2008-2013", ylim=c(0,16), space=1)
barplot(table(classi_D_2019),col=4:5, main="DPM: Frequenze Assolute",
        sub="Intervallo 2014-2019", ylim=c(0,16), space=1)

