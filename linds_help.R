library(dplyr)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

dat <- read.csv ( "C:/projects/ahold/lindsey_data.csv", stringsAsFactors = F)


dat$silent <- ifelse (dat$Vocal_vs_Silent=="Silent",1,0 )

dat <- filter ( dat,  trim(Coded_Race_Final)!="")

summary ( glm ( silent ~ Coded_Race_Final, data=dat, family=binomial()))

