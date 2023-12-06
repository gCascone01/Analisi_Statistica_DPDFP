#Funzione che calcola il CV
cv <- function (data){
  sd(data)/abs(mean(data))
}

# Funzione che crea 2 grafici (DPM e VSL) con i boxplot di 5 anni consecutivi
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

#Funzione che calcola la FdDC di un Paese x
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

#Funzione per rappresentazione delle serie temporali
f_timeSeries = function(x){
  #DPM
  ts = ts(data.frame(ts(D_dataset[x,]), ts(D_dataset[x+1,]),ts(D_dataset[x+2,]),
                     ts(D_dataset[x+3,]),ts(D_dataset[x+4,])),start=1995, frequency=1)
  plot.ts(ts, plot.type = "single", col = rainbow(5), xlab="", ylab="DPM",
          main="Serie Storica DPM: 1995-2015", type="b", pch=20, bty="l")
  grid()
  legend("topright",c(countries[x:(x+4)]),pch=c(20,20),col=rainbow(5))
  
  #VSL
  ts = ts(data.frame(ts(V_dataset[x,]), ts(V_dataset[x+1,]),ts(V_dataset[x+2,]),
                     ts(V_dataset[x+3,]),ts(V_dataset[x+4,])),start=1995, frequency=1)
  plot.ts(ts, plot.type = "single", col = rainbow(5), xlab="", ylab="VSL",
          main="Serie Storica VSL: 1995-2015", type="b", pch=20, bty="l")
  grid()
  legend("topleft",c(countries[x:(x+4)]),pch=c(20,20),col=rainbow(5))
}

#Funzione per i barplot di DPM
#Per VSL non ha senso farlo, i valori sono uguali per ogni fascia di età
f_barre = function(x){
  anni=rep(c(1995:2015),each=3)
  classi=rep(c("15 -","15 + & 64 -","64+"),21)
  
  values=c(t(cbind(D_dataset15[x,], D_datasetComp[x,], D_dataset64[x,])))
  df=data.frame(anni, classi, values)
  ggplot(df, aes(fill=classi, y=values, x=anni)) + 
    geom_bar(position="dodge", stat="identity", width=0.8,
             alpha=0.7, colour="black") + 
    ggtitle("Grafico delle Frequenze",paste("DPM per Classi di Età (1995 - 2004) - ",x, sep="")) + 
    theme(panel.background = element_blank(), axis.line = element_line(colour = "black"))
}