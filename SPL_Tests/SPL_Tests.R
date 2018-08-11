#Set working directory 
setwd("/Users/grzegorzantkiewicz/Documents/R_Working")


#Function testing the normal distribution of the data

if (any(grepl("tseries", installed.packages())) == FALSE){
  install.packages("tseries")
  YES
}
library("tseries")


nor = function(x) {
  
  jarque.bera.test(complete.cases(x))
  
}

#Testing the normality of data for the world, Europe and Africa

nor(final.df$death_under_5_per_birth)


#Function testing the significance of correlation 
cor = function(x,y){
  cor.test(x, y)
}
#Test on significance of correlation in all countries 

cor(final.df$GDP_PPP_2010,
    final.df$death_under_5_per_birth)

#Test on significance of correlation in Europe

cor(final.df_Europe$GDP_PPP_2010, 
    final.df_Europe$death_under_5_per_birth)

#Test on significance of correlation in Africa

cor(final.df_Africa$GDP_PPP_2010,
    final.df_Africa$death_under_5_per_birth)


#Function testing the heteroskedasticity
if (any(grepl("lmtest", installed.packages())) == FALSE){
  install.packages("lmtest")
}
library("lmtest")

het = function(x, y) {
  modell = lm(y~x)
  gqtest(modell)
  
}

#Test on heteroskedasticity in the world

het(final.df$GDP_PPP_2010, final.df$death_under_5_per_birth)

#Test on heteroskedasticity in Europe

het(final.df_Europe$GDP_PPP_2010, final.df_Europe$death_under_5_per_birth)

#Test on heteroskedasticity in Africa 

het(final.df_Africa$GDP_PPP_2010, final.df_Africa$death_under_5_per_birth)
