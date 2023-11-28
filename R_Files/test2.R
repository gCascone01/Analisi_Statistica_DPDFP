#install.packages('xlsx')
#library(xlsx)
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

# IDEA: RIMOSSI ANNI PRIMA DEL 95 E DOPO IL 2014

# CREO MATRICI CON I DATI

dataset = matrix(data[,-1],nrow=26) # nrow = 27 quando aggiungiamo Cipro
dataset = matrix( as.double(dataset) , nrow=26) # idem

dataset15 = matrix(data[,-1],nrow=26) # nrow = 27 quando aggiungiamo Cipro
dataset15 = matrix( as.double(dataset) , nrow=26) # idem

datasetComp = matrix(data[,-1],nrow=26) # nrow = 27 quando aggiungiamo Cipro
datasetComp = matrix( as.double(dataset) , nrow=26) # idem

dataset64 = matrix(data[,-1],nrow=26) # nrow = 27 quando aggiungiamo Cipro
dataset64 = matrix( as.double(dataset) , nrow=26) # idem

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

#---- CALCOLO MINIMO, MASSIMO E CAMPO DI VARIAZIONE ----

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

#PER OGNI INTERVALLO (A STEP DI 5 ANNI) CREO LE CLASSI
classi_D_1995 = cut(D_dataset[,1], breaks=classi_D, labels=labels_classi)
classi_D_2000 = cut(D_dataset[,6], breaks=classi_D, labels=labels_classi)
classi_D_2005 = cut(D_dataset[,11], breaks=classi_D, labels=labels_classi)
#classi_D_2005 già lo abbiamo per il secondo slot temporale
classi_D_2010 = cut(D_dataset[,16], breaks=classi_D, labels=labels_classi)
classi_D_2015 = cut(D_dataset[,21], breaks=classi_D, labels=labels_classi)

classi_V_1995 = cut(V_dataset[,1], breaks=classi_V, labels=labels_classi)
classi_V_2000 = cut(V_dataset[,6], breaks=classi_V, labels=labels_classi)
classi_V_2005 = cut(V_dataset[,11], breaks=classi_V, labels=labels_classi)
#classi_V_2005 già lo abbiamo per il secondo slot temporale
classi_V_2010 = cut(V_dataset[,16], breaks=classi_V, labels=labels_classi)
classi_V_2015 = cut(V_dataset[,21], breaks=classi_V, labels=labels_classi)

#---- DISEGNO I BARPLOT (GRAFICI DI FREQUENZA) ----

#getX la uso semplicemente per definire le coordinate del pallino "Italia" (vedi il plot sottostante)
getX = function(ds, i, classi){
  if(cut(ds["Italia",i], breaks=classi, labels=labels_classi) == "Very Low") return(1)
  else if(cut(ds["Italia",i], breaks=classi, labels=labels_classi) == "Low") return(2.5)
  else if(cut(ds["Italia",i], breaks=classi, labels=labels_classi) == "Medium") return(4)
  else if(cut(ds["Italia",i], breaks=classi, labels=labels_classi) == "High") return(5.5)
  else if(cut(ds["Italia",i], breaks=classi, labels=labels_classi) == "Very High") return(7)
}

#DPM
barplot(table(classi_D_1995), col=c("darkgreen", "lightgreen", "yellow", "orange", "red"), main="DPM: Frequenze Assolute",
        sub="Anno 1995", ylim=c(0,16), space=0.3, width=1.2)
points(getX(D_dataset,1,classi_D),0.5,col="blue",pch=19, cex=1.5)
legend("topright", pch=19, col="blue", legend="Italia")

barplot(table(classi_D_2000), col=c("darkgreen", "lightgreen", "yellow", "orange", "red"), main="DPM: Frequenze Assolute",
        sub="Anno 2000", ylim=c(0,16), space=0.3, width=1.2)
points(getX(D_dataset,6,classi_D),0.5,col="blue",pch=19, cex=1.5)
legend("topright", pch=19, col="blue", legend="Italia")

barplot(table(classi_D_2005), col=c("darkgreen", "lightgreen", "yellow", "orange", "red"), main="DPM: Frequenze Assolute",
        sub="Anno 2005", ylim=c(0,16), space=0.3, width=1.2)
points(getX(D_dataset,11,classi_D),0.5,col="blue",pch=19, cex=1.5)
legend("topright", pch=19, col="blue", legend="Italia")

barplot(table(classi_D_2010), col=c("darkgreen", "lightgreen", "yellow", "orange", "red"), main="DPM: Frequenze Assolute",
        sub="Anno 2010", ylim=c(0,16), space=0.3, width=1.2)
points(getX(D_dataset,16,classi_D),0.5,col="blue",pch=19, cex=1.5)
legend("topright", pch=19, col="blue", legend="Italia")

barplot(table(classi_D_2015), col=c("darkgreen", "lightgreen", "yellow", "orange", "red"), main="DPM: Frequenze Assolute",
        sub="Anno 2015", ylim=c(0,16), space=0.3, width=1.2)
points(getX(D_dataset,21,classi_D),0.5,col="blue",pch=19, cex=1.5)
legend("topright", pch=19, col="blue", legend="Italia")

#VSL
barplot(table(classi_V_1995), col=c("darkgreen", "lightgreen", "yellow", "orange", "red"), main="VSL: Frequenze Assolute",
        sub="Anno 1995", ylim=c(0,16), space=0.3, width=1.2)
points(getX(V_dataset,1,classi_V),0.5,col="blue",pch=19, cex=1.5)
legend("topright", pch=19, col="blue", legend="Italia")

barplot(table(classi_V_2000), col=c("darkgreen", "lightgreen", "yellow", "orange", "red"), main="VSL: Frequenze Assolute",
        sub="Anno 2000", ylim=c(0,16), space=0.3, width=1.2)
points(getX(V_dataset,6,classi_V),0.5,col="blue",pch=19, cex=1.5)
legend("topright", pch=19, col="blue", legend="Italia")

barplot(table(classi_V_2005), col=c("darkgreen", "lightgreen", "yellow", "orange", "red"), main="VSL: Frequenze Assolute",
        sub="Anno 2005", ylim=c(0,16), space=0.3, width=1.2)
points(getX(V_dataset,11,classi_V),0.5,col="blue",pch=19, cex=1.5)
legend("topright", pch=19, col="blue", legend="Italia")

barplot(table(classi_V_2010), col=c("darkgreen", "lightgreen", "yellow", "orange", "red"), main="VSL: Frequenze Assolute",
        sub="Anno 2010", ylim=c(0,16), space=0.3, width=1.2)
points(getX(V_dataset,16,classi_V),0.5,col="blue",pch=19, cex=1.5)
legend("topright", pch=19, col="blue", legend="Italia")

barplot(table(classi_V_2015), col=c("darkgreen", "lightgreen", "yellow", "orange", "red"), main="VSL: Frequenze Assolute",
        sub="Anno 2015", ylim=c(0,16), space=0.3, width=1.2)
points(getX(V_dataset,21,classi_V),0.5,col="blue",pch=19, cex=1.5)
legend("topright", pch=19, col="blue", legend="Italia")

# PROCEDIMENTO UTILE, TROVARE UNA SUDDIVISIONE!!!
install.packages("ggplot2")
library(ggplot2)

#Per il DPM

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

#labs(title = "MAIN TITLE", x = "X-AXIS TITLE", y = "Y-AXIS TITLE")

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

#Per il VSL

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
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"))

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
