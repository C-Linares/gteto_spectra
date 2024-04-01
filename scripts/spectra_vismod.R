
# R Script: **Gran teton Lights visual model** (color: blue)

#---------------------------------------------------------#

# Author: **Carlos**
# Date: **3/29/2024**
# Purpose: **load the spectral data for the signify light from grand teton and measure run a model for the perception of two or more insects and bat(if available) species**


# libraries
# install.packages("pavo") # if neede.

library(pavo) #for spectral data 


# import

pectra<- getspec("data", 
              ext="IRR",
              subdir = T,
              subdir.names = F)

#remove neg values 
negvals<-pectra[pectra<0]
pectra.fix<-procspec(pectra,fixneg = "zero")

# subset 

red<-subset(pectra.fix,"_red_") # all measure for the red light 
red2<-subset(pectra.fix, "_[0-9]+_red_m\\d$",) # just irradiance measures for red light

white<-subset(pectra.fix,"_white_") # all white light measurements 
white2<-subset(pectra.fix,"_[0-9]+_red_m\\d$")


# aggregate by measurement. 

meanreds<-aggspec(red2,by=3,FUN = mean)
plot(meanreds,type = "o",col="red",)
mtext("phillips red 10-100 intensity (watts/m^2)",side=2, outer = T)

# summary(meanreds, subset=T) colorimetic variables

#model

meanred.flux<-irrad2flux(meanreds) # we need to transform to flux   μmol.s−1.m−2

vismodel(meanreds, )

#chiroptera lambada max

chiro<-sensmodel(c(360,430,500,550))
plot(chiro, ylab="absorbance")

vismo_chiro<-vismodel(meanred.flux, 
                      visual = chiro, relative = F,
                      qcatch = "Qi")




vm2
plot(vm2)
barplot(as.matrix(vismo_chiro[1:4]))

# visual form the models
for (i in 1:11) {
  par(mar = c(2, 2, 2, 2))
  plot(meanreds,
       select = i + 1)
  par(mar = c(4.1, 2.5, 2.5, 2))
  barplot(as.matrix(vismo_chiro),
          yaxt = "n",
          col = "black")
}


# canid model

vismo_canis<-vismodel(meanred.flux, 
                      visual = "canis", relative = F,
                      qcatch = "Qi")
barplot(as.matrix(vismo_canis))
