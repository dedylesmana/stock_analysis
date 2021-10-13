#Regresi data panel
#Panggil Packages dan Input data
install.packages("devtools")
library(devtools)
install_github("Displayr/flipPlots")
devtools::install_github("Displayr/flipTransformations")
require(devtools)
install_github("Displayr/flipPlots")
install.packages("flipPlots")
library(flipPlots)
library(dplyr)
library(data.table)
library(dtplyr)

otomotifdatapanel=read.csv(file.choose(),sep=";")
View(otomotifdatapanel)
str(otomotifdatapanel)
otomotifdatapanel$Tahun=as.factor(otomotifdatapanel$Tahun)
names(otomotifdatapanel)
head(otomotifdatapanel,48)
summary(otomotifdatapanel)

#Install dan panggil packages
install.packages('plm')
library(lmtest)
install.packages('randtests')
library(randtests)
library(ggplot2)
library(plm)
library(readxl)

# PLOT DATA
qplot(data = otomotifdatapanel,HargaSaham ,fill = KodeEmiten,HargaSaham  = 30)
qplot(data = otomotifdatapanel,HargaSaham ,fill = Tahun,HargaSaham = 30)
ggplot (otomotifdatapanel, aes (x=ROA, y=HargaSaham ) )  +      # Buat plot ggplot2 dengan benar 
  geom_point (aes(size=KodeEmiten, col=Tahun))
ggplot(otomotifdatapanel,aes(x=SukuBunga,y=HargaSaham)) +
  geom_point(aes(size=KodeEmiten,col=Tahun))

  
