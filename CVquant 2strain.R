# Multi plate final data analysis version
#2 Strain, v4 assay version

library(readr)
library(viridis)
library(ggpubr)
library(RColorBrewer)
library(ggplot2)
library(ggpubr)

Usedir <- dirname(rstudioapi::getSourceEditorContext()$path)



#Create index of all files in input folder
Files <- dir(paste0(Usedir,"/input/"), pattern = ".txt")

#Labels for strains
Strainlist <- c("O4","O12","O4","O12","O4","O12")



#Convert to long frame data format:
Biggerdata <- data.frame()
for(j in 1:length(Files)){ 
  Background <- as.double()
  suppressMessages(Data <- data.frame(read_table2(paste0(Usedir,"/input/",Files[j]),
                                                  col_names = FALSE, locale = locale(decimal_mark = ",", grouping_mark = "."))) )
  #Spectramax plate layout conversion:
  if(dim(Data)[2]>13){
    
    Data2 <- data.frame(matrix(c(colMeans(Data)[!is.na((colMeans(Data)))] ),nrow=12, ncol= 8) )
    Data <- data.frame(t(Data2), X13 = 570)
    
    
  }
  #Remove column specifying wavelength
  Data$X13 <- NULL
  
  #Save background mean
  Background <- mean(mean(Data$X2[2:7]),mean(Data$X11[2:7]))
  
  #Remove background coloumns from dataset
  Data <- Data[,-c(1,2,11,12)]
  
  #Remove empty columns 
  Data <- Data[-c(1,8),]
  
  #Subtract background
  Data <- Data - Background
  
  Bigdata <- data.frame()
for(i in 1:dim(Data)[2]){
  
  # Bigdata <- rbind(Bigdata, cbind(Data[,i], (ceiling((2+(i/3)))%%3)+1, 1+((2+i)%%3), seq(1,8) ) )
  Bigdata <- rbind(Bigdata, cbind(Data[,i],  (ceiling(1+i/4)%%2)+j+(j-1)   , 1+((3+i)%%4), seq(1,6) ) )
  
}
  Biggerdata <- rbind(Biggerdata, cbind(Bigdata,j))
  # Bigdata <- data.frame()
  
}
colnames(Biggerdata) <- c("Abs","Strain","Biorep","Techrep","Plate")

Biggerdata$Strain <- as.factor(Biggerdata$Strain)
Biggerdata$Biorep <- as.factor(Biggerdata$Biorep)
Biggerdata$Techrep <- as.factor(Biggerdata$Techrep)
Biggerdata$Plate <- as.factor(Biggerdata$Plate)



Biggerdata$Strain <- factor((Biggerdata$Strain), levels = levels(as.factor(Biggerdata$Strain)), labels = Strainlist )

#Average technical replicates
Biggestdata <- data.frame()
for(i in 1:length(levels(Biggerdata$Plate))){
  Temp1 <- subset(Biggerdata, Biggerdata$Plate == levels(Biggerdata$Plate)[i] )
  
  for(j in 1:length(unique(Temp1$Strain))){
    Temp2 <- subset(Temp1, Temp1$Strain == ( unique(Temp1$Strain)[j] ) ) 
    for( z in 1:length(levels(Temp2$Biorep))){
      Temp3 <- subset(Temp2, Temp2$Biorep == levels(Temp2$Biorep)[z] )
      
      Biggestdata <- rbind(Biggestdata, c(mean(Temp3$Abs),
                                          as.character(Temp3$Strain)[1],
                                          Temp3$Biorep[1],
                                          Temp3$Plate[1]

      ) )
      
    }
    
  }
  
  
  
  
}

colnames(Biggestdata) <- c("Abs","Strain","Biorep","Plate")

Biggestdata$Abs <- as.numeric(Biggestdata$Abs)
Biggestdata$Strain <- as.factor(Biggestdata$Strain)
Biggestdata$Biorep <- as.factor(Biggestdata$Biorep)
Biggestdata$Plate <- as.factor(Biggestdata$Plate)



ggplot(Biggestdata, aes(x=(Strain), y = Abs )) +
  geom_boxplot() +
  geom_jitter(aes(colour = Strain))



# ggsave(paste0(Usedir, "/Output/CVplot.png") ,plot = Plot, limitsize = FALSE, device = png())

