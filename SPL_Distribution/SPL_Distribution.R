setwd("/Users/grzegorzantkiewicz/Documents/R_Working")

density1 = ggplot(final.df_Europe,
                  aes(GDP_PPP_2010,
                  fill = as.factor(final.df_Europe$region_who)))+
  geom_density(fill = "red", alpha = .3) +
  labs(x = "GDP per Capita in Europe in US-Dollar (PPP)",
       y = "Distriburion of Data",
       title = "Distribution of GDP per Capita in Europe")
density1

region = as.factor(final.df_Africa$region_who)

density2 = ggplot(final.df_Africa,
                  aes(GDP_PPP_2010,
                      fill = as.factor(final.df_Africa$region_who))) +
  geom_density(fill = "yellow", alpha = .3) +
  labs(x = "GDP per Capita in Africa in US-Dollar (PPP)",
       y = "Distriburion of Data",
       title = "Distribution of GDP per Capita in Africa")

density2
grid.arrange(density1, density2 , ncol = 2)

#Box-plot 



boxp1 = ggplot(final.df, aes(x = final.df$region_who,
                             y = final.df$GDP_PPP_2010,
                             color = final.df$region_who)) +
  geom_boxplot() +
  labs(y="GDP per Capita in US-Dollar (PPP)",
       x = "WHO region" ,
       title = "GDP by WHO regions") +
  theme(legend.position = "none")+
  coord_flip()

boxp1
