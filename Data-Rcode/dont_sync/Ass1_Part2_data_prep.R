# in case you want to clean your workspace
rm(list=ls())

setwd("/Users/sergej/Google Drive/QASS_Master/Network-Analysis-Week2/Assignment/Assignment1")
exports<-read.csv("BTDIXE_I3_07032016122249434.csv", header = TRUE, sep = ",")
library(data.table)
setDT(exports)
#Gross Exports in thousand US Dollar 2005 for 146 exporting countries
exports.2005.total<-dplyr::select(exports, X...COU, PAR, IND, Value)
names(exports.2005.total)[1]<-c("COU")
exports.2005.total<-exports.2005.total %>% dplyr::filter(., IND=="01T99" )
exports.2005.total$IND<-NULL

exports.2005.manufacturing<-dplyr::select(exports, X...COU, PAR, IND, Value)
names(exports.2005.manufacturing)[1]<-c("COU")
exports.2005.manufacturing<-exports.2005.manufacturing %>% filter(.,  IND=="15T37" )
exports.2005.manufacturing$IND<-NULL
exports.2005.manufacturing$Value<-ifelse(exports.2005.manufacturing$Value>0,1, 0)

#create symmetric country coverage
common.cntry<-intersect(as.character(unique(exports.2005.manufacturing$COU)), as.character(exports.2005.manufacturing$PAR))
exports.2005.manufacturing<-exports.2005.manufacturing %>% filter(.,PAR %in% common.cntry)
exports.2005.manufacturing<-exports.2005.manufacturing %>% filter(.,COU %in% common.cntry)

#import GDP 2005 (later not be used) and information about continent and classification of country income group 
setwd("/Users/sergej/Google Drive/Send_liza/Working_Data")
library(foreign)
library(DT)
library(dplyr)
gdp_classification<-read.dta("gdp_country.dta")
#gdp_cntry.dta file contains gdp classification, continent data and gdp 2005 from World Bank data base. I created it for my economics thesis 
gdp_classification<-as.data.table(gdp_classification)
gdp_class_par<-rename(gdp_classification, PAR=COU, inc_group_par=inc_group, continent_par=continent,GDP_2005_PAR=GDP_2005)
sum(is.na(exports.2005.total$Value)==TRUE)
gdp_class_par<-as.data.table(gdp_class_par)
exports.2005.manufacturing<-inner_join(exports.2005.manufacturing, gdp_class_par)
exports.2005.manufacturing<-inner_join(exports.2005.manufacturing,gdp_classification)
exports.2005.manufacturing<-rename(exports.2005.manufacturing,continent_COU=continent,inc_group_COU=inc_group,GDP_2005_COU=GDP_2005)
#create igraph df 
exports.2005.manufacturing.actors<-exports.2005.manufacturing[,c(1,6,7)]
exports.2005.manufacturing.actors<-exports.2005.manufacturing.actors[!duplicated(exports.2005.manufacturing.actors),]
exports.2005.manufacturing$value<-(exports.2005.manufacturing$GDP_2005_PAR+exports.2005.manufacturing$GDP_2005_COU)/2
setwd("/Users/sergej/blog")
save(exports.2005.manufacturing, file="edgelist_weights.Rdata")
save(exports.2005.manufacturing.actors, file="verticies_additional.Rdata")