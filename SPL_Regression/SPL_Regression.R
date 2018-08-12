# Set the working directory

setwd("C:/Users/Privat/SPL/Code/R_Working")

#The stargazer package is needed to produce nice LaTex tables

if (any(grepl("stargazer", installed.packages())) == FALSE){
  install.packages("stargazer")
  }

library(stargazer)

#List off all WHO regions

who.regions = final.df$region_who
who.regions = unique(who.regions)

#Create regional dummies for all six WHO regions

final.df$region_dummy_EMRO = ifelse(final.df$region_who == "EMRO",1,0)
final.df$region_dummy_EURO = ifelse(final.df$region_who == "EURO",1,0)
final.df$region_dummy_AFRO = ifelse(final.df$region_who == "AFRO",1,0)
final.df$region_dummy_AMRO = ifelse(final.df$region_who == "AMRO",1,0)
final.df$region_dummy_WPRO = ifelse(final.df$region_who == "WPRO",1,0)
final.df$region_dummy_SEARO = ifelse(final.df$region_who == "SEARO",1,0)

#Generate natural log of GDP with sapply

final.df$ln_GDP_PPP_2010 = sapply(final.df$GDP_PPP_2010, log)

#Estimate linear model with logaritmic child mortality as the dependend variable

lreg1 = lm(final.df$death_under_5_per_birth~final.df$ln_GDP_PPP_2010)

#Add the gini coefficient and the birthrate as control 
#variables

lreg2 = lm(final.df$death_under_5_per_birth
           ~final.df$ln_GDP_PPP_2010
           +final.df$gini_disp
           +final.df$birthrate_2010)

#Control for region dummies
          
lreg3 = lm(final.df$death_under_5_per_birth
           ~final.df$ln_GDP_PPP_2010
           +final.df$region_dummy_AFRO
           +final.df$region_dummy_AMRO
           +final.df$region_dummy_EMRO
           +final.df$region_dummy_EURO
           +final.df$region_dummy_SEARO
           +final.df$region_dummy_WPRO)

#Controlled for the gini coefficient and the birthrate,
#As well as for the region dummies

lreg4 = lm(final.df$death_under_5_per_birth
            ~final.df$ln_GDP_PPP_2010
            +final.df$gini_disp
            +final.df$birthrate_2010
            +final.df$region_dummy_AFRO
            +final.df$region_dummy_AMRO
            +final.df$region_dummy_EMRO
            +final.df$region_dummy_EURO
            +final.df$region_dummy_SEARO
            +final.df$region_dummy_WPRO)

#Create Latex table

stargazer(lreg1, lreg2, lreg3, lreg4,
          title="Child Mortality and Income", 
          out = "C:/Users/Privat/SPL/Code/R_Working/reg_table.tex",
          out.header = TRUE,
          align=TRUE, dep.var.labels=c("Children dead under 5(in percent)"),
          covariate.labels=c("log(GDP in 2010)",
                             "Gini Coefficient in 2010",
                             "Birthrate in 2010",
                             "WHO Region AFRO",
                             "WHO Region AMRO",
                             "WHO Region EMRO",
                             "WHO Region EURO",
                             "WHO Region SEARO",
                             "WHO Region WPRO"),
          omit.stat=c("LL","ser","f"), no.space=TRUE)
