#Daire Carroll, University of Gothenburg 2025, carrolldaire@gmail.com
#Species accumulaiton curves for the manuscript currently titled "Shifting harbor seal (Phoca vitulina) diet may reflect ecosystem changes in Skagerrak"

#rm(list=ls())
#graphics.off()  

setwd("C:/Users/daire/Desktop/Projects/Otioliths")
my.data = read.csv("ForResamp.csv", sep = ",", header = TRUE)
head(my.data)
attach(my.data)

cleanup_grid = theme(panel.border = element_blank(),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     axis.line = element_line(color = "black"),)

cleanup_text = theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
                     axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = 0.8, vjust = 0.4, face = "plain"),  
                     axis.title.x = element_text(color = "black", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
                     axis.title.y = element_text(color = "black", size = 20, angle = 90, hjust = .5, vjust = .5, face = "plain")) #clean up for ggplots			

problems <- c(
  "Cephalopoda", "Haddock/Pollack/Saithe", "Unidentified otoliths", "Unidentified Gadidae", "empty"
)

indexes = function(data){
  
  data = subset(data, is.na(species) == FALSE)
  
  Sp = unique(data$species)
  Total = length(Sp)
  
  N = nrow(data)
  n = c()
  
  for(i  in 1:length(Sp)){
    
    cho = Sp[i]
    n[i] = length(which(data$species == cho))
    
  }
  
  Simpson = sum(n*(n-1)) / (N*(N-1))
  Shannon = -sum((n/N)*log(n/N))
  
  return(list("Total" = Total, "Simpson" = Simpson, "Shannon" = Shannon))
  
} #determine richness and diversity index for a sample

indexes(my.data)

resamp = function(data,se,N){
  
  vals = c("Sample_Size","Mean_Total", "Mean_Simpson", "Mean_Shannon","SE_Total", "SE_Simpson", "SE_Shannon")
  
  samples = unique(data[,1])
  
  for(i in 1:length(se)){
    
    cho = se[i]
    store = c(NA,NA,NA)
    
    for(j in 1:N){
      
      sam = sample(samples,cho,replace = FALSE)
      #new.data = data[which(is.element(data[,1],sam)),]
      
      new.data = data[which(is.element(data[,1],sam[1])),]
      
      for(k in 2:length(sam)){
        
        new.data = rbind(new.data,
                        data[which(is.element(data[,1],sam[k])),]
                         )
        
      }
      
      new.data$species[is.element(new.data$species,problems)] <- NA 
      
      ind = indexes(new.data)
      store = rbind(store,ind)
    
    }
    
    store = data.frame(store[2:nrow(store),])
    
    TAv = mean(as.numeric(store[,1]))
    TSE = sd(as.numeric(store[,1]))/sqrt(N)
    SiAv = mean(as.numeric(store[,2]))
    SiSE = sd(as.numeric(store[,2]))/sqrt(N)
    ShAv = mean(as.numeric(store[,3]))
    ShSE = sd(as.numeric(store[,3]))/sqrt(N)
    
    vals = rbind(vals,c(cho,TAv,SiAv,ShAv,TSE,SiSE,ShSE))
    print(c(cho,TAv,SiAv,ShAv))
  }
  
  vals = data.frame(vals)
  names(vals) = vals[1,]
  vals = vals[2:nrow(vals),]
  return(data.frame(lapply(vals,as.numeric)))
  
} #data and N times to be resampled at each of a sequence se

samples = resamp(my.data,c(10:73),100)

P1 <- ggplot(samples  , aes(x = Sample_Size)) +	
  labs(x="Number of samples", y=expression("Species richness")) +
  cleanup_grid + cleanup_text    +
  geom_ribbon(aes(ymin= Mean_Total - SE_Total, ymax = Mean_Total + SE_Total), fill = "black", alpha = 0.3)+
  geom_line(aes(y = Mean_Total, group = 1), size = 1.5, alpha = 1)

P2<- ggplot(samples  , aes(x = Sample_Size)) +	
  labs(x="Number of samples", y=expression("Shannon index")) +
  cleanup_grid + cleanup_text    +
  geom_ribbon(aes(ymin= Mean_Shannon - SE_Shannon, ymax = Mean_Shannon + SE_Shannon), fill = "black", alpha = 0.3)+
  geom_line(aes(y = Mean_Shannon, group = 1), size = 1.5, alpha = 1)#+

P1
P2
