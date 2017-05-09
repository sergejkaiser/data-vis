library(dplyr)
require(ggthemes)
library(haven)

ggpie <- function (dat, by, totals) {
  ggplot(dat, aes_string(x=factor(1), y=totals, fill=by)) +
    geom_bar(stat='identity', color='black') +
    scale_fill_brewer() +
    guides(fill=guide_legend(override.aes=list(colour=NA))) + # removes black borders from legend
    coord_polar(theta='y') +
    theme(axis.ticks=element_blank(),
          axis.text.y=element_blank(),
          axis.text.x=element_text(colour='black'),
          axis.title=element_blank(),
          text = element_text(size = 12),
          panel.background = element_rect(fill = "white"))+
    scale_y_continuous(breaks=cumsum(dat[[totals]]) - dat[[totals]] /4, labels=dat[[by]]) 
}

setwd("/Users/sergej/blog")
load('trade_assignment.Rdata')
number.employed<-data.ls[['number.employed']]
value.added<-data.ls[['value.added']]
number.firms <- data.ls[['number.firms']]

number.employed$SIZE_EMP<- as.factor(number.employed$SIZE_EMP)
number.firms$SIZE_EMP<- as.factor(number.firms$SIZE_EMP)
value.added$SIZE_EMP<- as.factor(value.added$SIZE_EMP)

#shorten description
value.added$SIZE_EMP <-revalue(value.added$SIZE_EMP, c("From 0 to 9 persons employed"="0 to 9"))
number.firms$SIZE_EMP <- revalue(number.firms$SIZE_EMP,c("From 0 to 9 persons employed"="0 to 9") ) 
number.employed$SIZE_EMP <- revalue(number.employed$SIZE_EMP,c("From 0 to 9 persons employed"="0 to 9") )

value.added$SIZE_EMP <-revalue(value.added$SIZE_EMP, c("From 10 to 19 persons employed"="10 to 19"))
number.firms$SIZE_EMP <- revalue(number.firms$SIZE_EMP,c("From 10 to 19 persons employed"="10 to 19") ) 
number.employed$SIZE_EMP <- revalue(number.employed$SIZE_EMP,c("From 10 to 19 persons employed"="10 to 19") )

value.added$SIZE_EMP <-revalue(value.added$SIZE_EMP, c("From 20 to 49 persons employed"="20 to 49"))
number.firms$SIZE_EMP <- revalue(number.firms$SIZE_EMP,c("From 20 to 49 persons employed"="20 to 49") ) 
number.employed$SIZE_EMP <- revalue(number.employed$SIZE_EMP,c("From 20 to 49 persons employed"="20 to 49") )

value.added$SIZE_EMP <-revalue(value.added$SIZE_EMP, c("From 50 to 249 persons employed"="50 to 249"))
number.firms$SIZE_EMP <- revalue(number.firms$SIZE_EMP,c("From 50 to 249 persons employed"="50 to 249") ) 
number.employed$SIZE_EMP <- revalue(number.employed$SIZE_EMP,c("From 50 to 249 persons employed"="50 to 249") )

value.added$SIZE_EMP <-revalue(value.added$SIZE_EMP, c("250 persons employed or more"=">250"))
number.firms$SIZE_EMP <- revalue(number.firms$SIZE_EMP,c("250 persons employed or more"=">250") ) 
number.employed$SIZE_EMP <- revalue(number.employed$SIZE_EMP,c("250 persons employed or more"=">250") )

#rename columns
colnames(number.firms)[colnames(number.firms)=="Value"] <- "number.firms"
colnames(number.employed)[colnames(number.employed)=="Value"] <- "number.employed"
colnames(value.added)[colnames(value.added)=="Value"] <- "value.added"

#remove space in numeric strings
number.employed$number.employed<-gsub(" ","",number.employed$number.employed)
number.firms$number.firms<-gsub(" ","",number.firms$number.firms)
value.added$value.added<-gsub(" ","",value.added$value.added)

#convert numerical strings to numerical values
value.added$value.added<-as.numeric(value.added$value.added)
number.employed$number.employed<- as.numeric(number.employed$number.employed)
number.firms$number.firms <- as.numeric(number.firms$number.firms)


number.employed$SIZE_EMP<- as.character(number.employed$SIZE_EMP)
value.added$SIZE_EMP<- as.character(value.added$SIZE_EMP)

#Reference Country and prepare exclusing of Category Total
Country<- "Belgium"
exclude.category<-"Total"

value.added.per.worker <- merge(value.added,number.employed,by=c("SIZE_EMP","GEO","NACE_R2","TIME"))
#Get rid of some unnecessary columns
value.added.per.worker$Flag.and.Footnotes<-NULL
value.added.per.worker$INDIC_SB.y<-NULL
value.added.per.worker$INDIC_SB.x<-NULL
value.added.per.worker$value.added.per.worker <-(value.added.per.worker$value.added*10^3)/ value.added.per.worker$number.employed

#reduce data to only include estonia
value.added.per.worker.short<-subset(value.added.per.worker, SIZE_EMP %nin% exclude.category)
value.added.per.worker.short<-subset(value.added.per.worker.short, GEO %in% Country)

value.added.per.worker.short$SIZE_EMP<- as.factor(value.added.per.worker.short$SIZE_EMP)


#same here
number.employed.short<-subset(number.employed, GEO %in% Country)
number.employed.short<-number.employed.short%>% mutate(rel.number.employed = number.employed/number.employed[SIZE_EMP == "Total"])
number.employed.short<-subset(number.employed.short, SIZE_EMP %nin% exclude.category)

number.employed.short$SIZE_EMP<- as.factor(number.employed.short$SIZE_EMP)

#reorder factor variable such that it is orderd from smalles to highest value
print(levels(number.employed.short$SIZE_EMP)) 
number.employed.short$SIZE_EMP<-factor(number.employed.short$SIZE_EMP,levels(number.employed.short$SIZE_EMP)[c(2:5,1)])
number.employed.short$rel.number.employed<-number.employed.short$rel.number.employed*100
#plot
tikz(file = "number_employed_short.tex")
ggplot(number.employed.short, aes(x=SIZE_EMP, y=(rel.number.employed), fill=GEO)) +
    geom_bar(stat="identity", position="dodge") +
    scale_x_discrete("Size Class")+
  scale_y_continuous("Percentage") +
    ggtitle(expression(atop("Employment Distribution by Size Class")))+
  theme_economist()+
  scale_colour_economist() +   
  theme(axis.text.x=element_text(angle = -66, hjust = -.05),text = element_text(size = 12),legend.position="none")
dev.off()
#reduce to only estonia
number.firms.short<-subset(number.firms, GEO %in% Country)
number.firms.short<- number.firms.short%>% mutate(rel.number.firms = number.firms/number.firms[SIZE_EMP == "Total"])
number.firms.short<-subset(number.firms.short, SIZE_EMP %nin% exclude.category)

number.firms.short$SIZE_EMP<- as.factor(number.firms.short$SIZE_EMP)
print(levels(number.firms.short$SIZE_EMP))
number.firms.short$SIZE_EMP<-factor(number.firms.short$SIZE_EMP,levels(number.firms.short$SIZE_EMP)[c(2:5,1)])
number.firms.short$rel.number.firms<-number.firms.short$rel.number.firms*100
tikz(file="number_firms_short.tex")
ggplot(number.firms.short, aes(x=SIZE_EMP, y=rel.number.firms, fill=GEO)) +
  geom_bar(stat="identity", position="dodge") +
  scale_x_discrete("Size Class")+
  scale_y_continuous("Percentage")+
  theme_economist()+
  scale_colour_economist() + ggtitle(expression(atop("Number of Firms Distribution of firms by Size Class")))+
  theme(axis.text.x=element_text(angle = -66, hjust = -.05),text = element_text(size = 12),legend.position="none")
dev.off()
#print(levels(value.added.per.worker.short$SIZE_EMP))
#value.added.per.worker.short$SIZE_EMP<-factor(value.added.per.worker.short$SIZE_EMP,levels(value.added.per.worker.short$SIZE_EMP)[c(2:5,1)])

tikz(file="value_added_per_worker_short.tex")
ggplot(value.added.per.worker.short, aes(x=SIZE_EMP, y=value.added.per.worker, fill=GEO)) +
  geom_bar(stat="identity", position="dodge") +
  scale_x_discrete("Size Class")+
  scale_y_continuous("Thous of EUR per worker",limits=c(0, 30),breaks=c(0,15,30))+
  theme_economist()+
  scale_colour_economist() + ggtitle(expression(atop("Labour Productivity Distribution by Size Class")))+
  theme(axis.text.x=element_text(angle = -66, hjust = -.05),text = element_text(size = 12),legend.position="none")
dev.off()
#piechart main acitivity in manufacturing according to value added at factor cost
va.manuf <- read.csv("VA_Manufac_Data2.csv", dec = ".", sep = ",",na.strings=":",header=T, stringsAsFactors=FALSE)

colnames(va.manuf)[colnames(va.manuf)=="Value"] <- "value.added.manuf"
va.manuf$NACE_R2<- str_replace(va.manuf$NACE_R2,"Maunfacture of", "")
va.manuf$SIZE_EMP<- as.factor(va.manuf$SIZE_EMP)
va.manuf$GEO<- as.character(va.manuf$GEO)
va.manuf$NACE_R2<-as.factor(va.manuf$NACE_R2)
va.manuf$value.added.manuf<-gsub(" ","",va.manuf$value.added.manuf)
va.manuf$value.added.manuf <- as.numeric(va.manuf$value.added.manuf)
va.manuf$NACE_R2 <-gsub(" ",".",va.manuf$NACE_R2 )
va.manuf$NACE_R2 <-as.factor(va.manuf$NACE_R2)
va.manuf.short<-subset(va.manuf, GEO %in% Country)
va.manuf.short$GEO<-as.factor(va.manuf.short$GEO)

#relative values
va.manuf.short<- va.manuf.short%>% mutate(rel.share.va = value.added.manuf/value.added.manuf[NACE_R2 == "Manufacturing"])

#get rid of unnecessary stuff
va.manuf$Flag.and.Footnotes<-NULL
va.manuf.short$Flag.and.Footnotes<-NULL
va.manuf.short$NACE_R2<- as.character(va.manuf.short$NACE_R2)
#and define which category I will exclude
exclude<-"Manufacturing"
pie.df<-subset(va.manuf.short, NACE_R2 %nin% exclude)

pie.df$value.added<- as.numeric(pie.df$X2)

pie.df$NACE_R2 <- as.character(pie.df$NACE_R2)

#nicer descritption
pie.df$NACE_R2<-gsub("Manufacture.of.", "",pie.df$NACE_R2,perl=T,fixed=T)
#pie.df$NACE_R2<-gsub("and", "",pie.df$NACE_R2,perl=T,fixed=T)
pie.df$NACE_R2<-gsub(";.manufacture.of.articles.of.straw..plaiting.materials","",pie.df$NACE_R2,fixed=T)
pie.df$NACE_R2<-gsub(".", " ",pie.df$NACE_R2,perl=T,fixed=T)
pie.df$NACE_R2<-gsub("equipment", "equip",pie.df$NACE_R2,fixed=T)
pie.df$NACE_R2<-gsub(" pharmaceutical ", " pharma ",pie.df$NACE_R2,fixed=T)
pie.df$NACE_R2<-gsub("; manufacture of articles of straw and plaiting materials","",pie.df$NACE_R2,fixed=T)
pie.dfl$NACE_R2<-gsub("and pharma preparations", "and preparations",pie.df$NACE_R2,perl=T,fixed=T)
pie.df$NACE_R2<-gsub("except machinery and equip", "",pie.df$NACE_R2,fixed=T)
pie.df$NACE_R2<-gsub(", except machinery and equip","",pie.df$NACE_R2,fixed=T)
pie.df$NACE_R2<-gsub("recorded","",pie.df$NACE_R2,fixed=T)
pie.df$NACE_R2<-gsub(", except furniture","",pie.df$NACE_R2,fixed=T)
pie.df$NACE_R2<-gsub("and pharma preparations","and preparations",pie.df$NACE_R2,fixed=T)
pie.df$NACE_R2<-gsub("electrical equip","electrical equipment",pie.df$NACE_R2,fixed=T) 
pie.df$NACE_R2<-gsub("and equip","", pie.df$NACE_R2,fixed=T)
pie.df$NACE_R2<-capitalize(pie.df$NACE_R2)
#economist style plot
pie.df<-rename(pie.df, c('NACE_R2'="Manufacture"))

tikz(file = "final_value_added_industry_absolut.tex")
ggplot(pie.df, aes(x=Manufacture, y=value.added.manuf, fill=GEO)) +
  geom_bar(stat="identity", position="dodge") +
 
  scale_y_continuous("Mio. of EUR",limits=c(0, 350),breaks=c(0,115,230,345))+
  theme_economist()+
  scale_colour_economist() + ggtitle(expression(atop("Value Added Distribution by Activity")))+
  theme(axis.text.x=element_text(angle = -66, hjust = -.05),text = element_text(size = 12),legend.position="none")
dev.off()


tikz(file = "final_value_added_industry_relative.tex")
ggplot(pie.df, aes(x=Manufacture, y=rel.share.va, fill=GEO)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous("Percentage")+
  theme_economist()+
  scale_colour_economist() + ggtitle(expression(atop("Value Added Distribution by Industry")))+
  theme(axis.text.x=element_text(angle = -66, hjust = -.05),text = element_text(size = 12),legend.position="none")
dev.off()
#start pie chart
pie.with.total<- va.manuf.short[va.manuf.short$NACE_R2 %in% c("Manufacturing","Manufacture.of.fabricated.metal.products,.except.machinery.and.equipment","Manufacture.of.food.products","Manufacture.of.wood.and.of.products.of.wood.and.cork,.except.furniture;.manufacture.of.articles.of.straw.and.plaiting.materials"), ]
pie.with.three.big.ind<- va.manuf.short[va.manuf.short$NACE_R2 %in% c("Manufacture.of.fabricated.metal.products,.except.machinery.and.equipment","Manufacture.of.food.products","Manufacture.of.wood.and.of.products.of.wood.and.cork,.except.furniture;.manufacture.of.articles.of.straw.and.plaiting.materials"), ]

#new observation capturing the share of the rest of manufacturing
new <- data.frame(cbind(2011,"Estonia", "Rest of Manufacturing", "Value added at factor cost",pie.with.total$value.added.manuf[1]-sum(pie.with.total[2:4,5]),1-sum(pie.with.total[2:4,6]) ))
rowbind(pie.with.total,new)

library(plyr)
new<-rename(new, c("X1"="TIME","X2"="GEO","X3"="NACE_R2","X4"="INDIC_SB","X5"="value.added.manuf","X6"="rel.share.va"))

#lets put everything together
pie.chart.total<-rbind(pie.with.total,new)
pie.chart.total<-pie.chart.total[pie.chart.total$NACE_R2 %nin% c("Manufacturing"), ]
pie.chart.total$rel.share.va<-as.numeric(pie.chart.total$rel.share.va)
pie.chart.total$value.added.manuf<-as.numeric(pie.chart.total$value.added.manuf)

#nicer descriptions
pie.chart.total$NACE_R2<-gsub(".", " ",pie.chart.total$NACE_R2,perl=T,fixed=T)
pie.chart.total$NACE_R2<-gsub("equipment", "equip",pie.chart.total$NACE_R2,fixed=T)
pie.chart.total$NACE_R2<-gsub(" pharmaceutical ", " pharma ",pie.chart.total$NACE_R2,fixed=T)
pie.chart.total$NACE_R2<-gsub("; manufacture of articles of straw and plaiting materials","",pie.chart.total$NACE_R2,fixed=T)

blank_theme <- theme_minimal()+
theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )




#nicer description
pie.with.three.big.ind$NACE_R2<-gsub(".", " ",pie.with.three.big.ind$NACE_R2,fixed=T)
pie.with.three.big.ind$NACE_R2<-gsub("equipment", "equip",pie.with.three.big.ind$NACE_R2,fixed=T)
pie.with.three.big.ind$NACE_R2<-gsub(" pharmaceutical ", " pharma ",pie.with.three.big.ind$NACE_R2,fixed=T)
pie.with.three.big.ind$NACE_R2<-gsub("; manufacture of articles of straw and plaiting materials","",pie.with.three.big.ind$NACE_R2,fixed=T)
pie.with.three.big.ind$NACE_R2<-gsub("of wood and of products of wood and cork, except furniture","of wood products", pie.with.three.big.ind$NACE_R2)
pie.with.three.big.ind$NACE_R2<-gsub(" products, except machinery and equip","",pie.with.three.big.ind$NACE_R2)
pie.with.three.big.ind<-rename(pie.with.three.big.ind, c('NACE_R2'="Manufacture"))

pie.with.three.big.ind$Manufacture<-gsub("Manufacture of ","",pie.with.three.big.ind$Manufacture,fixed=T)


pie.with.three.big.ind$Manufacture<-capitalize(pie.with.three.big.ind[[3]])

pie.chart.total$NACE_R2<-gsub("of wood and of products of wood and cork, except furniture","of wood products", pie.chart.total$NACE_R2)
pie.chart.total$NACE_R2<-gsub(" products, except machinery and equip","",pie.chart.total$NACE_R2)
pie.chart.total<-rename(pie.chart.total, c('NACE_R2'="Manufacture"))
pie.chart.total$Manufacture<-gsub("Manufacture of ","",pie.chart.total$Manufacture,fixed=T)
pie.chart.total$Manufacture<-capitalize(pie.chart.total[[3]])
tikz(file = "pie1.tex")
ggpie(pie.chart.total,by="Manufacture",totals='value.added.manuf')+
  
  ggtitle("Three main activites manufacturing") +
  theme(axis.ticks.margin=unit(0,"lines"),
        plot.margin=rep(unit(0, "lines"),5))
dev.off()

tikz(file = "pie2.tex")
ggpie(pie.with.three.big.ind,by='Manufacture',totals='value.added.manuf')+
  
  ggtitle("Three main activites manufacturing in comparism") +
  theme(axis.ticks.margin=unit(0,"lines"),
        plot.margin=rep(unit(0, "lines"),5))
dev.off()
write.dta(va.manuf,"vale_added_manuf_complete.dta")
va_expr<-read.csv("TIVA_OECD_WTO.csv",dec = ".", sep = ";",header=T, stringsAsFactors=FALSE)
unique(va_expr$Industry)
exclude.service.other<- c("Agriculture, hunting, forestry and fishing", "Mining and quarrying" ,"Construction", "Wholesale and retail trade; Hotels and restaurants",  
"Transport and storage, post and telecommunication" ,"Financial intermediation","Business services"                                   
,"Other services")  
va_expr<- va_expr[va_expr$Industry %nin% exclude.service.other, ]
va_expr$Industry<-as.factor(va_expr$Industry)
va_expr$Country<-as.factor(va_expr$Country)
va_expr$Value<-gsub(",",".",va_expr$Value)
va_expr$Value<- as.numeric(va_expr$Value)
tikz(file = "va_exp.tex")
ggplot(va_expr, aes(x=Industry, y=Value, fill=Country)) +
  geom_bar(stat="identity", position="dodge") +
  scale_x_discrete("Manufacture")+
  scale_y_continuous("Mio US Dollar")+
  theme_economist()+
  scale_colour_economist() + ggtitle(expression(atop("Value Added Exports by Activity")))+
  theme(axis.text.x=element_text(angle = -66, hjust = -.05),text = element_text(size = 12),legend.position="none")
dev.off()
