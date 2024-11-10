#EDA and Final Project Analysis
#Bethel, Emily, and Shannon


###Part 1###

#read in csv file and tidying dataset
data<-read.csv('~/Downloads/COVID 19 confirmed - COVID 19 confirmed-2.csv')
data<-data[c(1:31), c(1:8)]

#code each climate zone as numeric, ordered from coldest to hottest
data$Climate.Zone<-factor(data$Climate.Zone, ordered=TRUE, levels=c('tundra', 'mid-latitude desert', 'mid-latitude steppe', 'highland tropical, dry winter', 'humid continental, dry winter', 'humid continental', 'humid subtropical, dry winter', 'humid subtropical', 'tropical'))
levels(data$Climate.Zone)
data$Climate.Zone<-as.numeric(data$Climate.Zone)

#decomma pop. density and make it numeric
install.packages('eeptools')
library(eeptools)
data$Population.Density<-decomma(data$Population.Density)
data$Population.Density<-as.numeric(data$Population.Density)

#make peak day numeric
data$Peak.Day<-as.numeric(data$Peak.Day)

#make pairwise plot matrix
pairs(data[4:8],
      main="Relationships between Variables in COVID-19 Dataset",
      pch=19)

#get summary 
summary(data)

#correlation matrix
cor(data[4:8])

unique(data$Province.State)
unique(data$Lat)

#linear regression model 

##filtered data
outliers<-which(data$Cases.per.day.per.100.000 > 0.05)
filtered_data<-data[-outliers, ]

model<-lm(Cases.per.day.per.100.000 ~ Province.GDP..2020., data=filtered_data)
summary(model)

##diagnostic plots
par(mfrow=c(2, 2))
plot(model)


##plot linear regression in ggplot2
library(tidyverse)
library(broom)
lm_summary<-tidy(model)


ggplot(filtered_data, aes(x = Province.GDP..2020., y = Cases.per.day.per.100.000)) +
  geom_point(shape = 20, size = 3, color = "purple") +
  geom_smooth(method = "lm", color = "orange", linetype = "dashed", size = 1.5) +
  geom_vline(xintercept = mean(filtered_data$Province.GDP..2020.), color="#50c878")+
  geom_hline(yintercept = mean(filtered_data$Cases.per.day.per.100.000), color="#50c878")+
  annotate("text", x = 1450, y = 0.02, label = paste("y =", round(lm_summary$estimate[2], 7), "x +", round(lm_summary$estimate[1], 3)), size = 5) +
  annotate("text", x = 1450, y = 0.018, label = paste("R^2 =", round(summary(model)$r.squared, 3)), size = 4, color = "black") +
  annotate("text", x = 1450, y = 0.016, label = "p = 0.47", size = 4, color = "black") +
  labs(y = "Average COVID Cases per day per 100,000 People", x = "Province GDP in 2020 (USD, in billions)") +
  ylim(0, 0.025)+
  xlim(0, 1700)+
  ggtitle("Linear Regression of Relationship between Province GDP \nand Average COVID-19 Cases per Capita in 2020")+
  theme_light(base_size = 12)+
  theme(plot.title=element_text(size=14, face="bold", hjust=0.5, vjust=0.3))

