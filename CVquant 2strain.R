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


#CFU data
#Manual input
#Ensure strains etc
#Format for each row is as follow: CFU1, CFU2, CFU3, CFU4, Strain, Plate#
#It is important this format is followed as this is used to link CFU data to abs data. 
CFU <- data.frame(rbind( c(8,8,8,5,"O4",1), 
                         c(4,5,4,7,"O12",1),
                         c(4,4,6,6,"O4",2),
                         c(4,5,5,4,"O12",2),
                         c(5,8,10,3,"O4",3),
                         c(6,13,9,5,"O12",3)))
colnames(CFU) <- c("x1","x2","x3","x4","Strain","Plate")


#Convert to long frame data format:
Biggerdata <- data.frame()
for(j in 1:length(Files)){ 
  Background <- as.double()
  Data <- data.frame(read_table2(paste0(Usedir,"/input/",Files[j]),
                                 col_names = FALSE, locale = locale(decimal_mark = ",", grouping_mark = ".")))
  
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



Biggerdata$CFU <- 0
Biggerdata$CFU2 <- 0
for(i in 1:dim(CFU)[1]){
  
  temp <- CFU[i,]
  
  Biggerdata$CFU[Biggerdata$Strain==temp$Strain & Biggerdata$Plate == temp$Plate] <- rep( (as.numeric(temp[1:4])), 1 , each = 6) 
  Biggerdata$CFU2[Biggerdata$Strain==temp$Strain & Biggerdata$Plate == temp$Plate] <- mean((as.numeric(temp[1:4])))
  
  
}





#Normalize absorption for CFU

Biggerdata$Absnorm <- Biggerdata$Abs/Biggerdata$CFU
Biggerdata$Absnorm2 <- Biggerdata$Abs/Biggerdata$CFU2


Plot <- ggplot(Biggerdata, aes(x=Strain, y = (Absnorm2))) +
  geom_boxplot() 
  # stat_compare_means(label = "p.signif",comparisons = combn(unique(Strainlist),2,simplify = FALSE), method = "t.test") 
  # geom_jitter(aes(color=Biorep))
  # facet_wrap(~Plate) +
  # scale_color_brewer(palette = "Dark2")

ggsave(paste0(Usedir, "/Output/CVplot.png") ,plot = Plot, limitsize = FALSE, device = png())

