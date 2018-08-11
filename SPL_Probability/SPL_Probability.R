
#Set working directory 
setwd("/Users/grzegorzantkiewicz/Documents/R_Working")


#Probability of dying before being 5

final.df$death_probability =
  (final.df$death_under_5_total/final.df$birth_total)

d5 = ggplot(final.df, aes(x=final.df$region_who,
                          y=final.df$death_probability,
                          color = final.df$region_who)) + 
  geom_boxplot() + 
  labs(x = "WHO region",
       y = "Probability of dying in the first 5 years",
       title="Probability of dying in the first 5 years in every WHO region") +
  coord_flip() + 
  theme(legend.position = "none")

d5

if (any(grepl("rworldmap", installed.packages())) == FALSE){
  install.packages("rworldmap")
}
library("rworldmap")


mapped_data <- joinCountryData2Map(final.df, joinCode = "ISO3", 
                                   nameJoinColumn = "Country.Code")

par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
mapCountryData(mapped_data,
               nameColumnToPlot = "death_probability",
               mapTitle = "Probability of death under 5",
               missingCountryCol = "#FFFFFF", 
               oceanCol = "#CCFFFF")
