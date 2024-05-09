#Daire Carroll 2024. Gothenburg university Department of Biological and Environmental Science, carrolldaire@gmail.com
#Script to resample a dataset of scat samples to determine the influence of sample sice on richness and diversity (Shannon index)

setwd("") #insert location of Example_Data.csv file
my.data = read.csv("Example_Data.csv", sep = ",", header = TRUE)
head(my.data)
attach(my.data)

indexes = function(data){
  
  Sp = unique(data$species)
  Total = length(Sp)
  
  N = nrow(data)
  n = c()
  
  for(i  in 1:length(Sp)){
    
    cho = Sp[i]
    n[i] = length(which(data$species == cho))
    
  }
  
  Shannon = -sum((n/N)*log(n/N))
  
  return(list("Total" = Total, "Shannon" = Shannon))
  
} 

resamp = function(data,se,N){
  
  vals = c("Sample_Size","Mean_Total", "Mean_Shannon","SE_Total", "SE_Shannon")
  
  samples = unique(data[,1])
  
  for(i in 1:length(se)){
    
    cho = se[i]
    store = c(NA,NA,NA)
    
    for(j in 1:N){
      
      sam = sample(samples,cho,replace = TRUE)
      #new.data = data[which(is.element(data[,1],sam)),]
      
      new.data = data[which(is.element(data[,1],sam[1])),]
      
      for(k in 2:length(sam)){
        
        new.data = rbind(new.data,
                        data[which(is.element(data[,1],sam[k])),]
                         )
        
      }
      
      ind = indexes(new.data)
      store = rbind(store,ind)
    
    }
    
    store = data.frame(store[2:nrow(store),])
    
    TAv = mean(as.numeric(store[,1]))
    TSE = sd(as.numeric(store[,1]))/sqrt(N)
    ShAv = mean(as.numeric(store[,3]))
    ShSE = sd(as.numeric(store[,3]))/sqrt(N)
    
    vals = rbind(vals,c(cho,TAv,ShAv,TSE,ShSE))
    print(c(cho,TAv,ShAv))
  }
  
  vals = data.frame(vals)
  names(vals) = vals[1,]
  vals = vals[2:nrow(vals),]
  return(data.frame(lapply(vals,as.numeric)))
  
} #data and N times to be resampled at each of a sequence se

samples = resamp(my.data,c(10:300),100)

plot(samples$Mean_Total~samples$Sample_Size)
plot(samples$Mean_Shannon~samples$Sample_Size)
