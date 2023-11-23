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

# DA QUI IN POI NON E' AGGIORNATO!!!

#CALCOLO ALCUNE MEDIE

#MEDIA ARITMETICA
avg = mean(dataset)
avg

#MEDIA ARITMETICA CON TRIM=0.05
avg = mean(dataset,0.05)
avg

#MEDIA ANNUALE
avg_per_year = apply(dataset, 2, function(x) mean(x))
avg_per_year_matrix = cbind(avg_per_year)
colnames(avg_per_year_matrix) = c( "AVERAGE")
rownames(avg_per_year_matrix) = years
plot(avg_per_year_matrix, type='o', xlab='Anno', ylab='DPCFP', main="Media annuale dei DPCFP",
     bty='l', col='red', xaxt = "n")
axis(1, at = seq(1,30,by=5), labels = seq(1990,2019,by=5), las = 2)

#QUANTILI
quantili=quantile(dataset)
quantili_matrix = cbind(quantili)
colnames(quantili_matrix) = c( "QUANTILE")
View(quantili_matrix)

boxplot=boxplot(as.vector(dataset), col="green", outlier.color="red")
outliers=boxplot$out
outliers
points(rep(1, length(outliers)), outliers, col = "red", pch = 19)   

#
View(dataset)
max