install.packages('readxl')
library(readxl)

#---- CARICO I DATASET ----

pathGitProject_Gio = "C:/Users/user/Desktop/Magistrale/Statistica e Analisi dei Dati/SAD_Project"
setwd(pathGitProject_Gio)

#pathGitProject_Ciro = "C:/Users/UTENTE/git/SAD_Project"
#setwd(pathGitProject_Ciro)

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

# Varianza per colonne (Forse ha più senso farlo per colonne, ragioniamoci bene)
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

cv <- function (data){
  sd(data)/abs(mean(data))
}

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

# Funzione che crea un grafico con i boxplot di 5 anni consecutivi
f_quantili = function (x){
  D_quantile1 = quantile(sort(D_dataset[,x]))
  D_quantile2 = quantile(sort(D_dataset[,x+1]))
  D_quantile3 = quantile(sort(D_dataset[,x+2]))
  D_quantile4 = quantile(sort(D_dataset[,x+3]))
  D_quantile5 = quantile(sort(D_dataset[,x+4]))
  
  V_quantile1 = quantile(sort(V_dataset[,x+1]))
  V_quantile2 = quantile(sort(V_dataset[,x+2]))
  V_quantile3 = quantile(sort(V_dataset[,x+3]))
  V_quantile4 = quantile(sort(V_dataset[,x+4]))
  V_quantile5 = quantile(sort(V_dataset[,x+5]))
  
  boxplot(D_quantile1,D_quantile2,D_quantile3,D_quantile4,D_quantile5,
          main="Boxplot DPM", xlab="DPM", names=c(1994+x):(1998+x), col=rainbow(5))
  boxplot(V_quantile1,V_quantile2,V_quantile3,V_quantile4,V_quantile5,
          main="Boxplot VSL", xlab="VSL", names=c(1994+x):(1998+x), col=rainbow(5))
}

f_quantili(1)
f_quantili(6)
f_quantili(11)
f_quantili(16)

#---- FdDC - Funzione di Distribuzione Empirica Continua ITALIA ----

f_FdDC = function(x){
  
  #DPM
  
  min_D_It <- min(D_dataset[x,])
  max_D_It <- max(D_dataset[x,])
  classi_D_IT = c(min_D_It, min_D_It+(max_D_It-min_D_It)/5, min_D_It+2*(max_D_It-min_D_It)/5, 
                  min_D_It+3*(max_D_It-min_D_It)/5, min_D_It+4*(max_D_It-min_D_It)/5, max_D_It)
  
  freqrel_IT <- table(D_dataset[x,])/length(D_dataset[x,])
  
  freqrel_D_IT <- table(cut(D_dataset[x,], breaks = classi_D_IT, right=FALSE))/length(D_dataset[x,])
  
  Fcum_IT <- cumsum(freqrel_D_IT)
  
  Fcum_IT[5] = Fcum_IT[5] + freqrel_IT[length(freqrel_D_IT)]
  
  plot(Fcum_IT, type="b", main=paste("FdDC - DPM",x, sep = " "))
  
  #VSL
  min_D_It <- min(V_dataset[x,])
  max_D_It <- max(V_dataset[x,])
  classi_D_IT = c(min_D_It, min_D_It+(max_D_It-min_D_It)/5, min_D_It+2*(max_D_It-min_D_It)/5, 
                  min_D_It+3*(max_D_It-min_D_It)/5, min_D_It+4*(max_D_It-min_D_It)/5, max_D_It)
  
  freqrel_IT <- table(V_dataset[x,])/length(V_dataset[x,])
  
  freqrel_D_IT <- table(cut(V_dataset[x,], breaks = classi_D_IT, right=FALSE))/length(V_dataset[x,])
  
  Fcum_IT <- cumsum(freqrel_D_IT)
  
  Fcum_IT[5] = Fcum_IT[5] + freqrel_IT[length(freqrel_D_IT)]
  
  plot(Fcum_IT, type="b", main=paste("FdDC - VSL",x, sep = " "))
}

f_FdDC("Italia")
f_FdDC("Germania")
f_FdDC("Francia")
f_FdDC("Spagna")
f_FdDC("Paesi Bassi")

#---- DEFINISCO SERIE TEMPORALI, SEPARATAMENTE ITALIA ED EUROPA ----

#DPM
serieD_IT = ts(D_dataset["Italia",])
serieD_GE = ts(D_dataset["Germania",])
serieD_FR = ts(D_dataset["Francia",])
serieD_PB = ts(D_dataset["Paesi Bassi",])
serieD_SP = ts(D_dataset["Spagna",])
serieD = data.frame(serieD_IT, serieD_GE,serieD_FR,serieD_PB,serieD_SP)
serieD = ts(serieD, start = 1995, frequency = 1)
plot.ts(serieD, plot.type = "single", col = rainbow(5), xlab="", ylab="DPM",
        main="Valori DPM dal 1995 al 2015", type="b", pch=20, bty="l")
grid()
legend("topright",c("Italia","Germania","Francia","Paesi Bassi","Spagna"),pch=c(20,20),col=rainbow(5))

#VSL
serieV_IT = ts(V_dataset["Italia",])
serieV_GE = ts(V_dataset["Germania",])
serieV_FR = ts(V_dataset["Francia",])
serieV_PB = ts(V_dataset["Paesi Bassi",])
serieV_SP = ts(V_dataset["Spagna",])
serieV = data.frame(serieV_IT, serieV_GE,serieV_FR,serieV_PB,serieV_SP)
serieV = ts(serieV, start = 1995, frequency = 1)
plot.ts(serieV, plot.type = "single", col = rainbow(5), xlab="", ylab="VSL",
        main="Valori VSL dal 1995 al 2015", type="b", pch=20, bty="l")
grid()
legend("topleft",c("Italia","Germania","Francia","Paesi Bassi","Spagna"),pch=c(20,20),col=rainbow(5))

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

#1995-2004 ITALIA
anni=1995:2014
classi=rep(c("15 -","15 + & 64 -","64+"),20)
DPM=cbind(D_dataset15["Italia",1:20], D_datasetComp["Italia",1:20], D_dataset64["Italia",1:20])
DPM=c(t(DPM))
df=data.frame(anni, classi, DPM)
ggplot(df, aes(fill=classi, y=DPM, x=anni)) + 
  geom_bar(position="dodge", stat="identity", width=0.8,
           alpha=0.7, colour="black") + 
  ggtitle("Grafico delle Frequenze","DPM per Classi di Età (1995 - 2004) - ITALIA") + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"))

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

