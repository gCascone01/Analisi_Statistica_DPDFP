#Funzione che calcola il CV
cv <- function (data){
  sd(data)/abs(mean(data))
}

# Funzione che crea 2 grafici (DPM e VSL) con i boxplot di 5 anni consecutivi
f_quantili = function (year, data, name){ # name = titolo del grafico
  if(year < 21){
    boxplot(sort(data[,year]),sort(data[,year+1]),sort(data[,year+2]),
            sort(data[,year+3]),sort(data[,year+4]),
            main=paste("Boxplot ", name), xlab=name, names=c(1994+year):(1998+year), col=rainbow(5))
  }
  else{
    boxplot(sort(data[,year]),sort(data[,year+1]),sort(data[,year+2]),
            sort(data[,year+3]),
            main=paste("Boxplot ", name), xlab=name, names=c(1994+year):(1997+year), col=rainbow(5))
  }
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
          main=paste(tipo, " Serie Storica: 1995-2018"), type="l", bty="l")
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
    ggtitle(paste(tipo," per Classi di Età (1995 - 2018) - ",country, sep="")) + 
    theme(panel.background = element_blank(), axis.line = element_line(colour = "black"))
}

f_regressione_lineare = function(d1,d2,paese,offset){
  c_cor=cor(d1,d2)
  
  model <- lm(d2~d1)
  
  if(summary(model)$r.squared > offset && (c_cor<(-0.85) || c_cor>0.85)) {
    plot(d1,d2, col="red", main =paste("Scatterplot e Curva Stimata",paese,sep=" - "))
    abline(model, col="blue")
    stime <- fitted (model)
    segments (d1, stime, d1, d2, col="magenta")
    
    summary <- summary(model)
    # print(paese)
    # print(summary$r.squared)
    # print(c_cor)
    
    residui <- resid(model)
    plot (d2, residui, main = paste("Diagramma dei residui",paese,sep=" - "),
          xlab = "VSL" , ylab ="Residui " , pch =9 , col =" red " )
    abline ( h =0 , col =" blue " , lty =2)
    # print(residui)
    
    return(list(c_cor=c_cor, r_squared=summary$r.squared, summary=summary, resid = residui))
  } else {
    return(paese)
  }
}

best_model_function <- function(d1, d2, paese) {
  
  # Definizione modelli di regressione studiati
  models <- list(
    lin = lm(d2 ~ d1),
    quad = lm(d2 ~ d1 + I(d1^2)),
    exp = lm(d2 ~ I(exp(d1))),
    semilog = lm(I(log(d2)) ~ d1),
    log = lm(I(log(d2)) ~ I(log(d1)))
  )
  
  best_model <- NULL
  best_r_squared <- -Inf
  
  temp_r_squared <- numeric(length(models))
  
  c_cor <- cor(d1, d2)
  
  for (model_name in names(models)) {
    model <- models[[model_name]]
    
    r_squared <- summary(model)$r.squared
    
    temp_r_squared[which(names(models) == model_name)] <- r_squared
    
    if (r_squared > best_r_squared) {
      best_r_squared <- r_squared
      best_model <- model
      best_model_name <- model_name
    }
  }
  
  
  # Plot dei dati e della curva stimata
  plot(d1, d2, col="red", main=paste("Scatterplot e Curva Stimata -", paese, "(", best_model_name, ")", "-", r_squared, sep=" "))
  switch(
    best_model_name,
    "lin" = abline(best_model, col="blue"),
    "quad" = curve(best_model$coefficients[[1]] + best_model$coefficients[[2]] * x + best_model$coefficients[[3]] * x^2, add=TRUE, col="blue"),
    "exp" = curve(best_model$coefficients[[1]] * exp(best_model$coefficients[[2]] * x), add=TRUE, col="blue"),
    "semilog" = curve(exp(best_model$coefficients[[1]] + best_model$coefficients[[2]] * x), add=TRUE, col="blue"),
    "log" = curve(exp(best_model$coefficients[[1]] + best_model$coefficients[[2]] * log(x)), add=TRUE, col="blue")
  )
  
  switch(
    best_model_name,
    "lin" = segments(d1, fitted(best_model), d1, d2, col="magenta"),
    "quad" = segments(d1, best_model$coefficients[[1]] + best_model$coefficients[[2]] * d1 + best_model$coefficients[[3]] * (d1)^2, d1, d2, col="magenta"),
    "exp" = segments(d1, best_model$coefficients[[1]] + best_model$coefficients[[2]] * exp (d1) , d1, d2, col="magenta"),
    "semilog" = segments(d1, exp(best_model$coefficients[[1]] + best_model$coefficients[[2]] * d1), d1, d2, col="magenta"),
    "log" = stime <- segments(d1, best_model$coefficients[[1]]*((d1)^ best_model$coefficients[[2]] ), d1, d2, col="magenta")
  )
  
  # Plot dei residui
  residui <- resid(best_model)
  plot(d2, residui, main=paste("Diagramma dei residui -", paese), xlab="Valori previsti", ylab="Residui", pch=19, col="red")
  abline(h=0, col="blue", lty=2)
  
  # Restituzione dei risultati
  return(list(
    model = best_model_name,
    r_squared = best_r_squared,
    # summary = summary(best_model),
    resid = residui,
    summary = data.frame(best_model=best_model_name, c_cor=c_cor, lin = temp_r_squared[1], quad = temp_r_squared[2], exp = temp_r_squared[3], semilog = temp_r_squared[4], log = temp_r_squared[5])
  ))
}

# Funzione per clustering gerarchico
hierarchClustering <- function(hls, n_clust, data, metodo, trHI, variabile) {
  taglio_hls <- cutree(hls, k = n_clust, h = NULL)
  
  table_hls <- table(taglio_hls)
  taglio_list_hls <- list(taglio_hls)
  
  # Calcolo misure di non omogeneità statistiche
  agvar_hls <- aggregate(data, taglio_list_hls, var)[, -1]
  
  trH_values <- numeric(n_clust)
  for(i in 1:n_clust) {
    trH_values[i] <- (table_hls[[i]] - 1) * sum(agvar_hls[i, ])
    if(is.na(trH_values[i])) trH_values[i] <- 0
  }
  
  trH_within <- sum(trH_values)
  trH_between <- trHI - trH_within
  
  
  plot(hls, hang=-1, xlab=paste(variabile,"- Metodo gerarchico agglomerativo"), sub=metodo)
  axis(side=4, at=round(c(0, hls$height), 2))
  
  rect.hclust(hls, k=n_clust, border=rainbow(n_clust))
  
  return(list(trH_within = trH_within, trH_between = trH_between))
}


# Funzione per clustering NON gerarchico
kMeansClustering <- function(data, n_clust, n_start = 5, iter_max, variabile) {
  km <- kmeans(data, centers = n_clust, nstart = n_start, iter.max = iter_max)
  
  # Calcola misure di non omogeneità
  trH_within <- sum(km$withinss)
  trH_between <- km$betweenss
  
  plot(data, col = km$cluster, xlab = variabile, ylab = variabile)
  points(km$centers, col = 1:n_clust, pch = 8, cex = 2)
  
  return(list(trH_within = trH_within, trH_between = trH_between, clusters = km$cluster))
}