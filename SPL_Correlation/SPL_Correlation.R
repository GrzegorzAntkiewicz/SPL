setwd("/Users/grzegorzantkiewicz/Documents/R_Working")


#Install the package ggplot2


if (any(grepl("ggplot2", installed.packages())) == FALSE){
  install.packages("ggplot2")
}

library("ggplot2")

library("RColorBrewer")

final.df_Africa = subset(final.df, region_who == "AFRO")
final.df_Europe = subset(final.df, region_who == "EURO")


#Correlation between GDP per Capita and Death 
#under 5 per birth worldwide and moving average
d1 = ggplot(final.df,
            aes(x=final.df$GDP_PPP_2010,
                y=final.df$death_under_5_per_birth)) + 
  geom_point(aes(color=final.df$region_who))  + 
  geom_smooth() +
  labs(x="GDP per Capita in constant 2011 US-Dollar (PPP)",
       y ="Death under 5 per birth (in percent)",
       title = "Total Child Mortality and Income, 2010",
       col = "WHO region") 
?ggplot


d1

#Correlation between GDP per Capita and Death
#under 5 per birth in Europe and Africa


d2 = ggplot(final.df_Europe, aes(x=final.df_Europe$GDP_PPP_2010,
                                 y=final.df_Europe$death_under_5_per_birth)) +
  geom_point(color="#33CCFF") +
  theme(plot.background = element_blank()) +
  ylim(0, 7.5) + 
  xlim(0, 125000)+
  geom_smooth(method=lm) +
  labs( x="GDP per Capita in US Dollar (PPP)",
        y ="Death under 5 per birth (in %)",
        title = "Total Child Mortality and Income in Europe, 2010")
d2
?xlim

d2ver2 = d2 +
  ylim(0, 7.5) + 
  xlim(0, 90000)
d2ver2

d3 = ggplot(final.df_Africa, aes(x=final.df_Africa$GDP_PPP_2010,
                                 y=final.df_Africa$death_under_5_per_birth)) +
  geom_point(color="#FF3300") +
  geom_smooth(method=lm) +
  ylim(0, 22) +
  xlim(0, 40000) +
  labs(x="GDP per Capita in US Dollar (PPP)",
       y ="Death under 5 per birth (in %)",
       title = "Total Child Mortality and Income in Africa, 2010")

d3


#Arranging 2 graphs on the same page
if (any(grepl("gridExtra", installed.packages())) == FALSE){
  install.packages("gridExtra")
}

library("gridExtra")
grid.arrange(d2, d3, ncol = 2)
