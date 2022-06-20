##Script for the statistical analysis and the data management done 
#within the project Soil Salinity
#
#1st: soil analysis
#2nd: plant analysis
#3rd: correlation
#4th: accessory code for species repartition

#Loading packages #
library(tidyverse)
library(readxl)
library(agricolae)
library(rstatix)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(magrittr)
library(tibble)
library(vegan)

#Loading Data ## one doc per sheet ### the sheet is the document "DataAnalysis" from the drive
soil_20 <- read_excel("~/Césure/Cours JRL/SoilSalinityAnalysis/DataAnalysis2.xlsx", sheet="Soil 0-20",na = "")
View(soil_20)
soil_40 <- read_excel("~/Césure/Cours JRL/SoilSalinityAnalysis/DataAnalysis2.xlsx", sheet="Soil 20-40")
View(soil_40)
plantchar <- read_excel("~/Césure/Cours JRL/SoilSalinityAnalysis/DataAnalysis2.xlsx", sheet="PlantCharac")
View(plantcharac)


##Soil analysis
mydata<-soil_40  #analysis with the data about the layer 20-40 cm, apply the same with the other sheets
head(mydata)
str(mydata)

summary(mydata)
#summary(mydata$soil_moisture, na.rm=T) 
#summary(mydata$omc, na.rm=T)
#summary(mydata$ph, na.rm=T)
#summary(mydata$conductivie, na.rm=T)
#summary(mydata$ca, na.rm=T)
#summary(mydata$na, na.rm=T)

#statistical summary
#turn each parameter into a numeric one
mydata$soil_moisture <- as.numeric(mydata$soil_moisture)
mydata$omc <- as.numeric(mydata$omc)
mydata$ph <- as.numeric(mydata$ph)

#mean and IQR 
mydata %>% group_by(plot) %>%
  get_summary_stats(soil_moisture,type = "median_iqr")
mydata %>% group_by(plot) %>%
  get_summary_stats(omc,type = "median_iqr")
mydata %>% group_by(plot) %>%
  get_summary_stats(ph,type = "median_iqr")
mydata %>% group_by(plot) %>%
  get_summary_stats(conductivity,type = "median_iqr") 
mydata %>% group_by(plot) %>%
  get_summary_stats(ca,type = "median_iqr")
mydata %>% group_by(plot) %>%
  get_summary_stats(na,type = "median_iqr")
mydata %>% group_by(plot) %>%
  get_summary_stats(k,type = "median_iqr")


#Test of normality and homogeneity
#Test of normality with Shapiro (p-value > 0.05 = normal)
shapiro.test(mydata$soil_moisture) 
shapiro.test(mydata$omc) 
shapiro.test(mydata$ph)
shapiro.test(mydata$conductivity)
shapiro.test(mydata$ca) 
shapiro.test(mydata$na) 
shapiro.test(mydata$k) 

#Test for homogeneity with Bartlett (p-value > 0.05 = normal)
bartlett.test(mydata$soil_moisture~mydata$plot)
bartlett.test(mydata$omc~mydata$plot)
bartlett.test(mydata$ph~mydata$plot)
bartlett.test(mydata$conductivity~mydata$plot)
bartlett.test(mydata$ca~mydata$plot)
bartlett.test(mydata$na~mydata$plot)
bartlett.test(mydata$k~mydata$plot)

#we obtained parametric and non-parametric data...
#kruskal Wallis test is suitable for both (p-value <0.05 = significant difference)

kruskal.test(mydata$soil_moisture~mydata$plot)
kruskal.test(mydata$omc~mydata$plot) #p-value >0.05
kruskal.test(mydata$ph~mydata$plot)
kruskal.test(mydata$conductivity~mydata$plot)
kruskal.test(mydata$ca~mydata$plot)
kruskal.test(mydata$na~mydata$plot)
kruskal.test(mydata$k~mydata$plot)
kruskal.test(mydata$em38_h~mydata$plot)

#group with a test: Dunn test (when 'ns' obtained in a table: the result are not significantly different)
View(dunn_test(soil_moisture ~ plot, data = mydata))
View(dunn_test(omc ~ plot, data = mydata))
View(dunn_test(ph ~ plot, data = mydata))
View(dunn_test(conductivity ~ plot, data = mydata))
View(dunn_test(ca ~ plot, data = mydata))
View(dunn_test(na ~ plot, data = mydata))
View(dunn_test(k ~ plot, data = mydata))

#for the plant analysis:
plantchar %>% group_by(plot) %>%
  get_summary_stats(spec_cover,type = "median_iqr")


#Kruskal wallis
kruskal.test(plantchar$nb_species~mydata$plot)
kruskal.test(plantchar$height~mydata$plot)
kruskal.test(plantchar$littler~mydata$plot)
kruskal.test(plantchar$spec_cover~mydata$plot)
kruskal.test(plantchar$dry_biomass~mydata$plot)

#Dunn test
View(dunn_test(nb_species ~ plot, data = plantchar))
View(dunn_test(height ~ plot, data = plantchar))
View(dunn_test(littler ~ plot, data = plantchar))
View(dunn_test(spec_cover ~ plot, data = plantchar))
View(dunn_test(dry_biomass ~ plot, data = plantchar))


###data visualisation
#correlation for the suitable parameters

## linear correlation abandonment date x soil salinity (na+ content)
ggscatter(mydata, x = "date_abandon", y = "na", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "date", ylab = "Na content")

## correlation parameters indicating the salinity, to check
ggscatter(mydata, x = "na", y = "conductivity", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Na (ppm)", ylab = "Conductivity (mS/cm)")

## linear correlation abandonment date x plant characteristics 
ggscatter(plantchar, x = "date_abandon", y = "nb_species", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Date of Abandonment", ylab = "Number of Species")
ggscatter(plantchar, x = "date_abandon", y = "spec_cover", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "date", ylab = "species cover")
ggscatter(plantchar, x = "date_abandon", y = "litter", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "date", ylab = "litter cover")
ggscatter(plantchar, x = "date_abandon", y = "dry_biomass", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "date", ylab = "dry biomass")
ggscatter(mydata, x = "date_abandon", y = "na", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "date", ylab = "Na content")

### correlation tests
cor.test(plantchar$nb_species, plantchar$date_abandon)
cor.test(plantchar$spec_cover, plantchar$date_abandon)
cor.test(plantchar$height, plantchar$date_abandon)
cor.test(plantchar$litter, plantchar$date_abandon)
cor.test(plantchar$dry_biomass, plantchar$date_abandon)



## correlate plant and soil properties
#use the sheet merging both soil and plants data
mydata <- read_excel("~/Césure/Cours JRL/SoilSalinityAnalysis/DataAnalysis2.xlsx", sheet="LinearReg",na = "")

ggscatter(mydata, x = "na", y = "nb_species", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Na+", ylab = "species")
ggscatter(mydata, x = "na", y = "nb_species", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Na+ (ppm)", ylab = "Number of species")
ggscatter(mydata, x = "ca", y = "nb_species", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Ca2+ (ppm)", ylab = "Number of species")
ggscatter(mydata, x = "conductivity", y = "nb_species", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "conductivity", ylab = "Number of species")
ggscatter(mydata, x = "omc", y = "height", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "omc", ylab = "height")
ggscatter(mydata, x = "nb_species", y = "conductivity", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "species", ylab = "conductivity")
ggscatter(mydata, x = "nb_species", y = "soil_moisture", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "species", ylab = "moisture")





###species repartition by plot
mydata <- read_excel("~/Césure/Cours JRL/SoilSalinityAnalysis/DataAnalysisSpecies.xlsx", sheet="Plot2")
#in this file there is one sheet per plot
#the code allows to get a figure by plot
#to change the plot, change the selected plot in line 210, 
#and it's name on the graph in line 259

#everything numeric
for (j in 2 : length (mydata)) {
  mydata[[j]] <- as.numeric(mydata[[j]])
}
View(mydata)
summarise(mydata)

###manipulation data : have them in a good format
str(mydata) #see class of data
mydata <- select(mydata, -Plot, -Replicate) #remove plot and replicate
mydata<-summarise_all(mydata,mean) #check the appearence of data and get the mean
mydata<-t(mydata) 

#create a new tab before rowname
mydata2 <- data.frame(plante=row.names(mydata),
                      speciescover = mydata[,1])
mydata2 <- arrange(mydata2,desc(speciescover))
mydata2$plante<-fct_inorder(mydata$plante)  #order differently to have beautiful data by plot

#attribute dominance rate
mydata2$dom[1] <-mydata2$speciescover[1]
for (j in 2 : length (mydata2$speciescover)) {
  mydata2$dom[j] <- sum (mydata2$dom[j-1]+mydata2$speciescover[j])
  
}
mydata2$dominance[1]=1 # 
for (j in 2 : length (mydata2$speciescover)) {     
  if (mydata2$dom[j-1] <= 80){
    mydata2$dominance[j] = 1
  } else {
    mydata2$dominance[j]=0
  }
}
mydata2$plante <-fct_inorder(mydata2$plante)#inverse l'ordre des niveaux, ce qui compte pourle ggplot ce sera le taux de couverture
paged_table(mydata2)

#plot
p<-ggplot(data=mydata2, aes(x= plante, y=speciescover)) +  
  geom_bar(stat="identity")+
  scale_colour_gradient(high="red", low="blue")+
  theme(axis.text.x = element_text(face="italic", color="#121111", 
                                   size=6, angle=90),
        axis.text.y = element_text(face="bold", color="#121111", 
                                   size=6, angle=90))+
  ggtitle(paste("Species abundance in the plot P2", sep =" ") ) +
  xlab("Species") + ylab("Rate of coverage")+
  theme(plot.title = element_text(color="blue",size=12,hjust = 0.5))+
  theme(legend.position = "none")
plot(p)

