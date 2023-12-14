#Funzione che calcola il CV
cv <- function (data){
  sd(data)/abs(mean(data))
}

# Funzione che crea 2 grafici (DPM e VSL) con i boxplot di 5 anni consecutivi
f_quantili = function (year, D_data, V_data, name){
  boxplot(sort(D_data[,year]),sort(D_data[,year+1]),sort(D_data[,year+2]),
          sort(D_data[,year+3]),sort(D_data[,year+4]),
          main=paste("Boxplot DPM ", name), xlab=paste("DPM", name), names=c(1994+year):(1998+year), col=rainbow(5))
  boxplot(sort(V_data[,year]),sort(V_data[,year+1]),sort(V_data[,year+2]),
          sort(V_data[,year+3]),sort(V_data[,year+4]),
          main=paste("Boxplot VSL", name), xlab=paste("VSL", name), names=c(1994+year):(1998+year), col=rainbow(5))
}

#Funzione che calcola la FdDC di un Paese country
f_FdDC = function(country){
  #DPM
  min_D <- min(D_dataset[country,])
  max_D <- max(D_dataset[country,])
  classi_D = c(min_D, min_D+(max_D-min_D)/5, min_D+2*(max_D-min_D)/5, 
                  min_D+3*(max_D-min_D)/5, min_D+4*(max_D-min_D)/5, max_D)
  
  freqrel <- table(D_dataset[country,])/length(D_dataset[country,])
  freqrel_D <- table(cut(D_dataset[country,], breaks = classi_D, right=FALSE))/length(D_dataset[country,])
  
  Fcum <- cumsum(freqrel_D)
  Fcum[5] = Fcum[5] + freqrel[length(freqrel_D)]
  
  plot(Fcum, type="b", main=paste("FdDC - DPM",country, sep = " "))
  
  #VSL
  min_D <- min(V_dataset[country,])
  max_D <- max(V_dataset[country,])
  classi_D = c(min_D, min_D+(max_D-min_D)/5, min_D+2*(max_D-min_D)/5, 
                  min_D+3*(max_D-min_D)/5, min_D+4*(max_D-min_D)/5, max_D)
  
  freqrel <- table(V_dataset[country,])/length(V_dataset[country,])
  freqrel_D <- table(cut(V_dataset[country,], breaks = classi_D, right=FALSE))/length(V_dataset[country,])
  
  Fcum <- cumsum(freqrel_D)
  Fcum[5] = Fcum[5] + freqrel[length(freqrel_D)]
  
  plot(Fcum, type="b", main=paste("FdDC - VSL",country, sep = " "))
}

#Funzione per rappresentazione delle serie temporali
f_timeSeries = function(data,tipo){
  colori = c("antiquewhite1","antiquewhite4","aquamarine","aquamarine4","azure3",
             "azure4","hotpink","black","blue","blueviolet","brown","burlywood",
             "burlywood4","cadetblue3","chartreuse","chartreuse4","chocolate",
             "chocolate1","chocolate4","cornflowerblue","cyan","cyan3","darkblue",
             "darkgoldenrod1","darkmagenta","darksalmon","red")
  #DPM
  list_ts <- list()
  
  for (i in 1:nrow(data)){
    list_ts[[i]] <- ts(data[i,])
  }
  
  df <- do.call(cbind, list_ts)
  
  
  ts = ts(df,start=1995, frequency=1)
  par(mar=c(5,4,4,15),xpd=TRUE)
  plot.ts(ts, plot.type = "single", col = colori, xlab="", ylab=tipo,
          main=paste(tipo, " Serie Storica: 1995-2015"), type="l", bty="l")
  grid()
  legend("topright",inset=c(-0.38,0),c(countries[1:14]), pch=c(20,20),col=colori[1:15])
  legend("topright",inset=c(-0.83,0),c(countries[15:nrow(data)]), pch=c(20,20),col=colori[15:27])
}

#Funzione per i barplot di DPM
#Per VSL non ha senso farlo, i valori sono uguali per ogni fascia di età
f_barre_sovrapp_per_eta = function(country, data_15, dataComp, data64, tipo){
  anni=rep(c(1995:2018),each=3)
  classi=rep(c("15 -","15 + & 64 -","64+"),24)
  
  values=c(t(cbind(data_15[country,], dataComp[country,], data64[country,])))
  df=data.frame(anni, classi, values)
  ggplot(df, aes(fill=classi, y=values, x=anni)) + 
    geom_bar(position="dodge", stat="identity", width=0.8,
             alpha=0.7, colour="black") + 
    ggtitle(paste(tipo," per Classi di Età (1995 - 2015) - ",country, sep="")) + 
    theme(panel.background = element_blank(), axis.line = element_line(colour = "black"))
}