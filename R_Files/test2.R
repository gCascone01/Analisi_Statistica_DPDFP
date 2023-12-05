install.packages('readxl')
library(readxl)

#---- CARICO I DATASET ----

pathGitProject_Gio = "C:/Users/user/Desktop/Magistrale/Statistica e Analisi dei Dati/SAD_Project"
setwd(pathGitProject_Gio)

#pathGitProject_Ciro = "C:/Users/UTENTE/git/SAD_Project"
#setwd(pathGitProject_Ciro)

mydata = read_xlsx('./Datasets/European-Country/Complete_Dataset.xlsx', 1)
data = as.matrix(mydata)

mydata15 = read_xlsx('./Datasets/European-Country/less15.xlsx', 1)
data15 = as.matrix(mydata15)

mydataComp = read_xlsx('./Datasets/European-Country/15-64.xlsx', 1)
dataComp = as.matrix(mydataComp)

mydata64 = read_xlsx('./Datasets/European-Country/64.xlsx', 1)
data64 = as.matrix(mydata64)

#---- DAI DATASET COSTRUISCO LE MATRICI ----

# Anni: dal '95 al 2005 - Manca Cipro (Quando lo aggiungiamo, nrow=27)

# CREO MATRICI CON I DATI

dataset = matrix(data[,-1],nrow=26)
dataset = matrix( as.double(dataset) , nrow=26)

dataset15 = matrix(data15[,-1],nrow=26)
dataset15 = matrix( as.double(dataset15) , nrow=26)

datasetComp = matrix(dataComp[,-1],nrow=26)
datasetComp = matrix( as.double(datasetComp) , nrow=26)

dataset64 = matrix(data64[,-1],nrow=26)
dataset64 = matrix( as.double(dataset64) , nrow=26)

# RIMUOVO ANNI CHE NON MI INTERESSANO

dataset=dataset[,-(1:10)]
dataset=dataset[,-(43:50)]

dataset15=dataset15[,-(1:10)]
dataset15=dataset15[,-(43:50)]

datasetComp=datasetComp[,-(1:10)]
datasetComp=datasetComp[,-(43:50)]

dataset64=dataset64[,-(1:10)]
dataset64=dataset64[,-(43:50)]

# ASSEGNO NOMI ALLE RIGHE

countries = c("Austria", "Belgio", "Bulgaria", "Croazia", "Danimarca", "Estonia", "Finlandia", "Francia", "Germania",
              "Grecia", "Ungheria", "Irlanda", "Italia", "Lettonia", "Lituania", "Lussemburgo", "Malta", "Paesi Bassi", "Polonia",
              "Portogallo", "Repubblica Ceca", "Romania", "Slovacchia", "Slovenia", "Spagna", "Svezia")
rownames(dataset) = countries
rownames(dataset15) = countries
rownames(datasetComp) = countries
rownames(dataset64) = countries

# DEFINISCO LE COLONNE PER CREARE DUE DATASET SEPARATI, UNO PER DPM E UNO PER VSL

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
V_min = min(V_dataset)

D_max = max(D_dataset)
V_max = max(V_dataset)

D_mean = mean(D_dataset)
V_mean = mean(V_dataset)

D_median = median(D_dataset)
V_median = median(V_dataset)

# D_summary = summary(D_dataset)
# V_summary = summary(V_dataset)

resumeCentralità = rbind(c(D_min,D_max,D_mean,D_median),c(V_min,V_max,V_mean,V_median))
colnames(resumeCentralità) = c("MIN", "MAX", "MEAN", "MEDIAN")
rownames(resumeCentralità) = c("DPM","VSL")
View(resumeCentralità)

# Campo di Variazione: sensibile ai soli valori estremi, non tenendo conto del numero di osservaz
D_cdv = D_max - D_min
V_cdv = V_max - V_min

# Differenza interquartilica: meno sensibile a val estremi
D_diffInterq = unname(quantile(sort(D_dataset), probs = 0.75)) - unname(quantile(sort(D_dataset), probs = 0.25))
V_diffInterq = unname(quantile(sort(V_dataset), probs = 0.75)) - unname(quantile(sort(V_dataset), probs = 0.25))

# Varianza per tutto dataset
D_var = var(as.vector(D_dataset))
V_var = var(as.vector(V_dataset))

# Varianza per colonne
# D_var = var(D_dataset)
# V_var = var(V_dataset)

# Varianza per righe
D_var_righe = apply(D_dataset, 1, var)
V_var_righe = apply(V_dataset, 1, var)

# Varianza per colonne
D_var_colonne = apply(D_dataset, 2, var)
V_var_colonne = apply(V_dataset, 2, var)

# Deviazione Standard totale: valore differisce dalla media aritmetica dei valori, in media quadratica di: x
D_sd = sd(D_dataset)
V_sd = sd(V_dataset)

resumeDispersione = rbind(c(D_var,D_sd),c(V_var,V_sd))
colnames(resumeDispersione) = c("VAR", "SD")
rownames(resumeDispersione) = c("DPM","VSL")
View(resumeDispersione)

# CV

cv <- function (data){
  sd(data)/abs(mean(data))
}

cv(D_dataset["Italia",])
cv(D_dataset)

cv(V_dataset["Italia",])
cv(V_dataset)

#---- QUARTILI ----
D_quantile = quantile(sort(D_dataset))
V_quantile = quantile(sort(V_dataset))

boxplot(D_quantile, main="Boxplot DPM", xlab="DPM")
boxplot(V_quantile, main="Boxplot VSL", xlab="VSL")

#---- FdDC - Funzione di Distribuzione Empirica Continua ITALIA ----
min_D_It <- min(D_dataset["Italia",])
max_D_It <- max(D_dataset["Italia",])
classi_D_IT = c(min_D_It, min_D_It+(max_D_It-min_D_It)/5, min_D_It+2*(max_D_It-min_D_It)/5, 
                min_D_It+3*(max_D_It-min_D_It)/5, min_D_It+4*(max_D_It-min_D_It)/5, max_D_It)

freqrel_IT <- table(D_dataset["Italia",])/length(D_dataset["Italia",])

freqrel_D_IT <- table(cut(D_dataset["Italia",], breaks = classi_D_IT, right=FALSE))/length(D_dataset["Italia",])

Fcum_IT <- cumsum(freqrel_D_IT)

Fcum_IT[5] = Fcum_IT[5] + freqrel_IT[length(freqrel_D_IT)]

plot(Fcum_IT, type="b")

#---- FdDC - Funzione di Distribuzione Empirica Continua UNIONE EUROPEA ----
min_D_EU <- min(D_dataset)
max_D_EU <- max(D_dataset)
classi_D_EU = c(min_D_EU, min_D_EU+(max_D_EU-min_D_EU)/5, min_D_EU+2*(max_D_EU-min_D_EU)/5, 
                min_D_EU+3*(max_D_EU-min_D_EU)/5, min_D_EU+4*(max_D_EU-min_D_EU)/5, max_D_EU)

freqrel_EU <- table(D_dataset)/length(D_dataset)

freqrel_D_EU <- table(cut(D_dataset, breaks = classi_D_EU, right=FALSE))/length(D_dataset)

Fcum_EU <- cumsum(freqrel_D_EU)

Fcum_EU[5] = Fcum_EU[5] + freqrel_EU[length(freqrel_D_EU)]

plot(Fcum_EU, type="b")

#---- fare FdDC per VSL ----



#---- DEFINISCO SERIE TEMPORALI, SEPARATAMENTE ITALIA ED EUROPA ----

#DPM
serieD_IT = ts(D_dataset["Italia",])
serieD_EU = ts(apply(D_dataset,2,mean))
serieD = data.frame(serieD_IT, serieD_EU)
serieD = ts(serieD, start = 1995, frequency = 1)
plot.ts(serieD, plot.type = "single", col = c("red", "black"), xlab="", ylab="DPM",
        main="Valori DPM dal 1995 al 2015", type="b", pch=20, bty="l")
grid()
legend("topright",c("Italia","Media Europea"),pch=c(20,20),col=c("red","black"))

#VSL
serieV_IT = ts(V_dataset["Italia",])
serieV_EU = ts(apply(V_dataset,2,mean))
serieV = data.frame(serieV_IT, serieV_EU)
serieV = ts(serieV, start = 1995, frequency = 1)
plot.ts(serieV, plot.type = "single", col = c("red", "black"), xlab="", ylab="DPM",
        main="Valori VSL dal 1995 al 2015", type="b", pch=20, bty="l")
grid()
legend("bottomright",c("Italia","Media Europea"),pch=c(20,20),col=c("red","black"))

#---- DEFINISCO LE CLASSI ----

#CREO LE SUDDIVISIONI
labels_classi = c("Very Low", "Low", "Medium", "High", "Very High")
classi_D = c(D_min, D_min+(D_max-D_min)/5, D_min+2*(D_max-D_min)/5, 
             D_min+3*(D_max-D_min)/5, D_min+4*(D_max-D_min)/5, D_max)
classi_V = c(V_min, V_min+(V_max-V_min)/5, V_min+2*(V_max-V_min)/5, 
             V_min+3*(V_max-V_min)/5, V_min+4*(V_max-V_min)/5, V_max)

#---- GRAFICO A BARRE SOVRAPPOSTE ----
install.packages("ggplot2")
library(ggplot2)

# DPL

#1995-2004
anni=1995:2004
classi=rep(c("<15",">15 & <64",">64"),10)
DPM=cbind(apply(D_dataset15[,1:10],2,mean),
            apply(D_datasetComp[,1:10],2,mean),apply(D_dataset64[,1:10],2,mean))
DPM=c(t(DPM))
df=data.frame(anni, classi, DPM)
ggplot(df, aes(fill=classi, y=DPM, x=anni)) + 
  geom_bar(position="dodge", stat="identity", width=0.8,
           alpha=0.7, colour="black") + 
  ggtitle("Grafico delle Frequenze","DPM per Classi di Età (1995 - 2004)") + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"))

#2005-2014
anni=2005:2014
classi=rep(c("<15",">15 & <64",">64"),10)
DPM=cbind(apply(D_dataset15[,11:20],2,mean),
          apply(D_datasetComp[,11:20],2,mean),apply(D_dataset64[,11:20],2,mean))
DPM=c(t(DPM))
df=data.frame(anni, classi, DPM)
ggplot(df, aes(fill=classi, y=DPM, x=anni)) + 
  geom_bar(position="dodge", stat="identity", width=0.8,
           alpha=0.7, colour="black") + 
  ggtitle("Grafico delle Frequenze","DPM per Classi di Età (2005 - 2014)") + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"))

# VSL

#1995-2004
anni=1995:2004
classi=rep(c("<15",">15 & <64",">64"),10)
VSL=cbind(apply(V_dataset15[,1:10],2,mean),
          apply(V_datasetComp[,1:10],2,mean),apply(V_dataset64[,1:10],2,mean))
VSL=c(t(VSL))
df=data.frame(anni, classi, VSL)
ggplot(df, aes(fill=classi, y=VSL, x=anni)) + 
  geom_bar(position="dodge", stat="identity", width=0.8,
           alpha=0.7, colour="black") + 
  ggtitle("Grafico delle Frequenze","VSL per Classi di Età (1995 - 2004)") + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  scale_fill_manual(values = c("<15" = "lightgreen", ">15 & <64" = "orange", ">64" = "red"))

#2005-2014
anni=2005:2014
classi=rep(c("<15",">15 & <64",">64"),10)
VSL=cbind(apply(V_dataset15[,1:10],2,mean),
          apply(V_datasetComp[,1:10],2,mean),apply(V_dataset64[,1:10],2,mean))
VSL=c(t(VSL))
df=data.frame(anni, classi, VSL)
ggplot(df, aes(fill=classi, y=VSL, x=anni)) + 
  geom_bar(position="dodge", stat="identity", width=0.8,
           alpha=0.7, colour="black") + 
  ggtitle("Grafico delle Frequenze","VSL per Classi di Età (2005 - 2014)") + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"))
