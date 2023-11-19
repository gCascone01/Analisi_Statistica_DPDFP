install.packages('xlsx')
library(xlsx)

#Ottengo i dati dal foglio excel con i dati assoluti e li inserisco in una matrice.
mydata <- read.xlsx('C:/Users/user/Desktop/Magistrale/Statistica e Analisi dei Dati/Progetto/Datasets/Both/All_Both.xlsx', 1)
data = as.matrix(mydata)

#Dalla matrice, definisco due vettori: uno per i nomi dei paesi, uno per gli anni.
countries = data[-1,1]
years = data[1,-1]

#Creo la versione finale della matrice con i valori assoluti, assegnando i nomi a righe e colonne.
dataset = matrix(data[-1,-1],nrow=54)
tmp = strtoi(dataset)
dataset = matrix( tmp , nrow=54)
rownames(dataset) = countries
colnames(dataset) = years

#DEFINISCO POI DUE TABELLE, UNA PER I MINIMI E UNA PER I MASSIMI

#TROVO I MINIMI
min = min( dataset )
positions = which( dataset == min )

#QUINDI TROVO GLI INDICI DEI MINIMI INDIVIDUATI
rows_index = positions %% length(countries)
cols_index = round( (positions) / length(countries) )

#CREO LA TABELLA DEI MINIMI
min_matrix = cbind(dataset[positions], countries[rows_index], years[cols_index])
colnames(min_matrix) = c( "VALUE", "COUNTRY", "YEAR")
rownames(min_matrix) = 1:length(positions)
min_matrix

#FACCIO LO STESSO PER I MASSIMI
max = max( dataset )
positions = which( dataset == max )

rows_index = positions %% length(countries)
cols_index = round( (positions) / length(countries) )

max_matrix = cbind(dataset[positions], countries[rows_index], years[cols_index])
colnames(max_matrix) = c( "VALUE", "COUNTRY", "YEAR")
rownames(max_matrix) = 1:length(positions)
max_matrix

#CALCOLO IL CAMPO DI VARIAZIONE
campo_di_variazione = max - min
campo_di_variazione

#IL DATASET RISULTA ESSERE POCO IDONEO. OPERIAMO SUL DATASET CHE TIENE CONTO DELLA DENSITA'

mydata = read.xlsx('C:/Users/user/Desktop/Magistrale/Statistica e Analisi dei Dati/Progetto/Datasets/Density/All_Both_Density.xlsx', 1)
data = as.matrix(mydata)

dataset = matrix(data[-1,-1],nrow=54)
tmp = as.double(dataset)
dataset = matrix( tmp , nrow=54)
rownames(dataset) = countries
colnames(dataset) = years

min = min( dataset )
positions = which( dataset == min )

rows_index = positions %% length(countries)
cols_index = round( (positions) / length(countries) )

min_matrix = cbind(dataset[positions], countries[rows_index], years[cols_index])
colnames(min_matrix) = c( "VALUE", "COUNTRY", "YEAR")
rownames(min_matrix) = 1:length(positions)
min_matrix

max = max( dataset )
positions = which( dataset == max )

rows_index = positions %% length(countries)
cols_index = round( (positions) / length(countries) )

max_matrix = cbind(dataset[positions], countries[rows_index], years[cols_index])
colnames(max_matrix) = c( "VALUE", "COUNTRY", "YEAR")
rownames(max_matrix) = 1:length(positions)
max_matrix

campo_di_variazione = max - min
campo_di_variazione

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
