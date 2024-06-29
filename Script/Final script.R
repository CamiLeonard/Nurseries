library(ggplot2)
library(ggpubr)
library(FSA)
library(glmmTMB)
library(MASS)
library(stringr)
library(lme4)
library(readxl)
library(dplyr)
library(mgcv)
library(car)
library(tidyverse)
library(plyr)
library(fmsb)

#Environmental
##light data
data3<-read_xlsx("Data/NurseryPAR.xlsx")
data7<-spread(data3,Depth,PAR)
data7$timestamp<-as.POSIXct(data7$TimeDate,format="%Y-%m-%d %H:%M:%OS")
colnames(data7)[4]<-'Lagoon'
colnames(data7)[5]<-'Fore reef'
max<-aggregate(PAR~Depth+Date,data3,max)
means<-aggregate(PAR~Depth,data=max,mean)

data4<-data3[-3,] %>% filter(Time >= "09:00:00" & Time <= "15:00:00")

data5<-spread(data4,Depth,PAR)
data6<-data5[-c(1:3),]
mean(data6$'7.5')
sd(data6$'7.5')
mean(data6$'13')
sd(data6$'13')
data6$attenuation<-(1-(data6$'13'/data6$'7.5'))*100
mean(data6$attenuation)
sd(data6$attenuation)

light<-ggplot(data7,aes(x=timestamp))+ylab("PAR (µmol/(m².s))")+xlab("")+
  geom_line(aes(y = Lagoon, color = "Lagoon 7 m"))+
  geom_line(aes(y = data7$'Fore reef', color = "Fore reef 13 m"))+
  scale_x_datetime(date_labels = "%Y-%m-%d",date_breaks="1 day")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_color_manual(name = "Habitat", values = c("Lagoon 7 m" = "#549159", "Fore reef 13 m" = "#915454"))
light
ggsave("Output/NurseryLight.svg",light,width = 6, height = 3)

##stats
wilcox.test(data7$'Fore reef',data7$Lagoon,paired=TRUE)
colnames(data7)[5]<-"Forereef"
lightday<-ddply(data7,c("Date"),summarise,maxL=max(Lagoon),
                maxF=max(Forereef))
lightday<-lightday[-12,]
mean(lightday$maxL)
sd(lightday$maxL)
mean(lightday$maxF)
sd(lightday$maxF)

### Temp data
hoboe2B<-read.csv("Data/hoboe2b13.7.csv",header=F,sep=",")
hoboe2B$timestamp<-as.POSIXct(hoboe2B$V2,format="%m/%d/%Y %I:%M:%OS %p")
hoboe2B$timestamp<-hoboe2B$timestamp+47
colnames(hoboe2B)[3]<-"Forereef"
hobolagon21<-read.csv("Data/hobolagon20210610.csv",header=F,sep=",")
hobolagon21$timestamp<-as.POSIXct(hobolagon21$V2,format="%m/%d/%Y %I:%M:%OS %p")
colnames(hobolagon21)[3]<-"Lagoon"

total<-inner_join(hoboe2B,hobolagon21,by="timestamp")
total<-total[-(1:2),]

ggplot(total,aes(x=timestamp))+
  geom_line(aes(y = Lagoon), color="#5BD9D9")+
  geom_line(aes(y = Forereef), color = "#3D3D99") + 
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line("black") ,
        legend.position = "none",axis.title.x = element_blank())+
  ylab("Temperature °C")
ylim(25,31)

##spiderplot for july-november 2021
total$date<-as.Date(total$timestamp)
tempday<-ddply(total,c("date"),summarise,meanL=mean(Lagoon),maxL=max(Lagoon),minL=min(Lagoon),
               meanF=mean(Forereef),maxF=max(Forereef),minF=min(Forereef))
tempday$rangeL<-tempday$maxL-tempday$minL
tempday$rangeF<-tempday$maxF-tempday$minF
df<-data.frame(row.names=c("Lagoon", "Forereef"))
df$Maximum<-c(max(total$Lagoon),max(total$Forereef))
df$Minimum<-c(min(total$Lagoon),min(total$Forereef))
df$"Max.daily.mean"<-c(max(tempday$meanL),max(tempday$meanF)) 
df$"Min.daily.mean"<-c(min(tempday$meanL),min(tempday$meanF))
df$"Mean.daily.range"<-c(mean(tempday$rangeL),mean(tempday$rangeF))
df$"Max.daily.range"<-c(max(tempday$rangeL),max(tempday$rangeF))
df$"Min.daily.range"<-c(min(tempday$rangeL),min(tempday$rangeF))

max_min <- data.frame(
  Maximum=c(29,27),
  Minimum=c(27,25),
  "Max.daily.mean"=c(28,26),
  "Min.daily.mean"=c(28,26),
  "Mean.daily.range"=c(2,0),
  "Max.daily.range"=c(3,1),
  "Min.daily.range"=c(2,0))
rownames(max_min) <- c("Max", "Min")

df2<-rbind(max_min,df)

create_beautiful_radarchart <- function(data, color = c("#549159","#915454"), 
                                        vlabels = c( "Maximum \n (27-29°C)",
                                                     "Minimum \n (25-27°C)",
                                                     "Max daily mean \n (26-28°C)",
                                                     "Min daily mean \n (26-28°C)",
                                                     "Mean daily range \n (0-2°C)",
                                                     "Max daily range \n (1-3°C)",
                                                     "Min daily range \n (0-2°C)"), vlcex = 0.7,
                                        caxislabels = c(0,0.5,1,1.5,'2°C'), title = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}


op <- par(mar = c(1, 1, 1, 1))

spider<-create_beautiful_radarchart(df2)
ggsave("Output/spider.svg",spider,width = 3, height = 3)

##stats
wilcox.test(total$Forereef,total$Lagoon,paired=TRUE)
wilcox.test(tempday$meanL,tempday$meanF,paired=TRUE)
wilcox.test(tempday$maxL,tempday$maxF,paired=TRUE)
wilcox.test(tempday$minL,tempday$minF,paired=TRUE)
wilcox.test(tempday$rangeL,tempday$rangeF,paired=TRUE)
tempday$maxdiff<-tempday$maxL-tempday$maxF
mean(tempday$maxdiff)
sd(tempday$maxdiff)
tempday$mindiff<-tempday$minF-tempday$minL
mean(tempday$mindiff)
sd(tempday$mindiff)
mean(tempday$rangeL)
sd(tempday$rangeL)
mean(tempday$rangeF)
sd(tempday$rangeF)

##mortality
10*100/352
##Lost
57*100/352

##maturity
size<-read_excel("Data/size.xlsx")
maturity<-size %>% filter(mature>1)
maturity$eggs<-0
maturity$eggs[maturity$mature!="empty"]<-1
maturity$size_of_origin<-as.numeric(maturity$size_of_origin)
kruskal.test(maturity$eggs,maturity$site)
kruskal.test(maturity$size_of_origin,maturity$site)
hist(maturity$size_of_origin)
shapiro.test(maturity$size_of_origin)
ggplot(maturity, aes(x=site,y=size_of_origin)) +
  geom_boxplot()+
  ylab("Size of colony")+
  stat_compare_means(method='kruskal.test')

mod1<-glmer(eggs~species*site+species*size_of_origin+(1|genotype),data=maturity,family = binomial)
plot(mod1)
Anova(mod1)
mod1<-glm(eggs~size_of_origin,data=maturity,family = binomial)
predict(mod1,data.frame(size_of_origin=50),type="response")
predict(mod1,data.frame(size_of_origin=200),type="response")

fig2b<-maturity %>% ggplot(aes(size_of_origin,eggs))+
  labs(tag="B")+
  geom_point(alpha=0.2)+
  geom_smooth(method="glm",method.args=list(family="binomial"),color="#f58e5b")+
  xlab(expression(paste("Fragment surface in cm"^{2})))+
  ylab('Probability to be mature')+
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"))+
  scale_y_continuous(limits = c(0, 1), breaks = c(0,1))
fig2b

model<-maturity %>% filter(species=="A. hyacinthus")
mod1<-glm(eggs~size_of_origin,data=model,family = binomial)
predict(mod1,data.frame(size_of_origin=100),type="response")
model<-maturity %>% filter(species=="A. nasuta")
mod1<-glm(eggs~size_of_origin,data=model,family = binomial)
predict(mod1,data.frame(size_of_origin=100),type="response")
model<-maturity %>% filter(species=="A. retusa")
mod1<-glm(eggs~size_of_origin,data=model,family = binomial)
predict(mod1,data.frame(size_of_origin=100),type="response")
model<-maturity %>% filter(species=="A. striata")
mod1<-glm(eggs~size_of_origin,data=model,family = binomial)
predict(mod1,data.frame(size_of_origin=100),type="response")
model<-maturity %>% filter(species=="A. globiceps")
mod1<-glm(eggs~size_of_origin,data=model,family = binomial)
predict(mod1,data.frame(size_of_origin=100),type="response")

pairwise.wilcox.test(maturity$eggs,maturity$species)
aggregate(size_of_origin~eggs,maturity,mean)
aggregate(eggs~mature,maturity,length)

maturity2<-ddply(maturity,c("species","site"),summarise,mature=sum(eggs)*100/length(eggs),immature=(length(eggs)-sum(eggs))*100/length(eggs))
maturity3<-gather(maturity2,"Maturity","sum",mature,immature)
maturity3$species<-factor(maturity3$species,c("A. globiceps","A. striata","A. hyacinthus","A. nasuta","A. retusa"))
maturity3$site<-factor(maturity3$site,c("Lagoon","Fore reef"))

fig2a<-ggplot(data=maturity3, aes(fill=Maturity, y=sum, x=species)) + 
  labs(tag="A")+
  geom_bar(stat="identity")+
  coord_flip()+
  facet_grid(cols=vars(site))+
  ylab("cover percentage")+
  labs(fill="Maturity", x=NULL,y="% of fragments")+
  theme(panel.background = element_rect(fill='white'),
        axis.text.y = element_text(face = "italic"),
        legend.position="top")+
  scale_fill_manual(values=c("#ada4a0","#f58e5b"))
fig2<-ggarrange(fig2a,fig2b)
fig2
ggsave("Output/Fig2.svg",fig2,width = 7, height = 3)

timing<-maturity %>% filter(spawning>1)
timing$month<-11
timing$month[timing$spawning=="September"]<-9
timing$month[timing$spawning=="October"]<-10
timing$month[timing$spawning=="December"]<-12
timing2<-ddply(timing,c("species","site","genotype"),summarise,mean=mean(month))

mod1<-lmer(mean~species*site+(1|genotype),data=timing2)
plot(mod1)
Anova(mod1)

ggplot(timing2, aes(x=site,y=mean)) +
  geom_boxplot()+
  facet_grid(cols=vars(species))+
  stat_compare_means(method='kruskal.test')

percent<-ddply(maturity3,c("species.x"),summarise,Dec=sum(December)*100/sum(total),
               Nov=sum(November)*100/sum(total),
               Oct=sum(October)*100/sum(total),
               Sep=sum(September)*100/sum(total))

##split spawning
timing2<-ddply(timing,c("species","genotype","spawning"),summarise,sum=sum(eggs))
timing3<-spread(timing2,spawning,sum)
timing3[is.na(timing3)]<-0
timing3$sum<-rowSums(timing3[,3:6])
split<-timing3 %>% filter(sum>1)
split<-split[c(2,3,5,8,9,11,12,13,15,16,20),]
length(unique(timing$genotype))
11*100/37

#Growth
mean(size$gf*365/12)
shapiro.test(size$gf)
hist(size$gf)
mod2<-glmmTMB(gf~(1|genotype)+species*site*time+size_of_origin,data=size)
Anova(mod2)
size$predicted<-fitted.values(mod2)

size$site<-factor(size$site,c("Lagoon","Fore reef"))
site<-ggplot(size,aes(y=gf*365,x=site,fill=site))+
  geom_boxplot()+
  facet_grid(~species)+
  geom_signif(comparisons=list(c("Fore reef","Lagoon")),map_signif_level = TRUE)+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        strip.text.x = element_text(face = "italic"),
        legend.position =  "none")+
  scale_fill_manual(values=c("#549159","#915454"))+
  ylab("Yearly surface increase %")+
  ylim(0,700)
site
ggsave("Output/Sitebysp2.svg",site,width = 8, height = 3)

facet<-ggplot(size,aes(y=predicted,x=size_of_origin,color=site))+
  geom_point(size=1)+
  geom_smooth(method='lm',linewidth=0.8)+
  facet_grid(species~time)+
  theme_bw()+
  theme(strip.text.y = element_text(face = "italic"))+
  ylab("Predicted growth in % per day")+
  scale_color_manual(values=c("#549159","#915454"))+
  xlab(expression(paste("Initial size in cm"^{2})))
facet

ggsave("Output/Facetgrowth.svg",facet,width = 5, height = 5.5)
ggsave("Output/Facetgrowth.jpg",facet,width = 5, height = 5.5)

##Physiology
final<-read_xlsx("Data/Physio.xlsx")
final$site<-factor(final$site,c("Lagoon","Fore reef"))

p2<-ggplot(final,aes(y=sym_cm2/1000000,x=site,fill=site))+
  geom_boxplot()+
  geom_signif(comparisons=list(c("Fore reef","Lagoon")),map_signif_level = TRUE)+
  ylab(expression(paste("Symbiont density (10 "^{6} ," per cm"^{2},")")))+
  theme_bw()+
  theme(legend.position="none",axis.title.x = element_blank())+
  scale_fill_manual(values=c("#549159","#915454"))+
  facet_grid(cols=vars(sym))+
  ylim(0,2.5)
p2

final$chla<-factor(final$chla,labels=(expression(paste("Chlorophyll " ,italic("a")))))
p3<-ggplot(final,aes(y=chla.cell*1000000,x=site,fill=site))+
  geom_boxplot()+geom_signif(comparisons=list(c("Fore reef","Lagoon")),map_signif_level = TRUE)+
  ylab(expression(paste("Chlorophyll ",italic("a "),"(pg per symbiont cell)")))+
  theme_bw()+
  theme(legend.position="none",axis.title.x = element_blank())+
  scale_fill_manual(values=c("#549159","#915454"))+
  facet_grid(cols=vars(chla),labeller = label_parsed)+
  ylim(3,11)
p3

final$chlc<-factor(final$chlc,labels=(expression(paste("Chlorophyll " ,italic(c[2])))))
p4<-ggplot(final,aes(y=chlc2.cell*1000000,x=site,fill=site))+
  geom_boxplot()+geom_signif(comparisons=list(c("Fore reef","Lagoon")),map_signif_level = TRUE)+
  ylab(expression(paste("Chlorophyll ",italic(c[2])," (pg per symbiont cell)")))+
  theme_bw()+
  theme(legend.position="none",axis.title.x = element_blank())+
  scale_fill_manual(values=c("#549159","#915454"))+
  facet_grid(cols=vars(chlc),labeller = label_parsed)+
  ylim(0,2)
p4
physio<-ggarrange(p2,p3,p4,ncol=3,labels = c("A", "B","C"))
physio
ggsave("Output/Physio.svg",physio,width = 7, height = 3)
ggsave("Output/Physio.pdf",physio,width = 7, height = 3)
ggsave("Output/Physio.jpeg",physio,width = 7, height = 3)

table<-read_xlsx("Data/Hyagrowthphysio.xlsx")
hist(table$Growth)
shapiro.test(table$Growth)
mod1<-lmer(Growth~Symbiont_density*Chlorophyll_a_per_cell*Chlorophyll_c2_per_cell*Site+Original_size+(1|genotype),data=table)
summary(mod1)
Anova(mod1)
anova<- as.data.frame(Anova(mod1)) 
anova1<-cbind("Explanatory"=rownames(anova),anova)
anova1<-anova1 %>% mutate(Explanatory = str_replace_all(Explanatory,":", " * "))
anova1$`Pr(>Chisq)`<-ifelse(anova1$`Pr(>Chisq)`<0.001,"<0.001",round(anova1$`Pr(>Chisq)`,digits=3))
anova1$Chisq<-round(anova1$Chisq,digits=2)
anova1 %>% write_xlsx("Output/growthanova.xlsx")
table$predicted<-fitted.values(mod1)
qplot(fitted.values(mod1),residuals(mod1))+geom_smooth(method='lm')
##no pattern in residuals vs fitted, that means that there is a real linear relationship between growth 

table$Site<-factor(table$Site,c("Lagoon","Fore reef"))
figgrowth<-ggplot(table,aes(y=predicted,x=Symbiont_density))+geom_point()+
  geom_smooth(method='lm',aes(color=Site))+
  facet_wrap(~Site)+
  theme_bw()+ theme(legend.position="none")+
  scale_color_manual(values=c("#549159","#915454"))+
  ylab("Predicted growth in % per day")+
  xlab(expression(paste("Symbiont density per cm"^{2})))
figgrowth

ggsave("Output/Predicted growth by symbiont.pdf",figgrowth,width = 5, height = 3)
ggsave("Output/Predicted growth by symbiont.svg",figgrowth,width = 5, height = 3)
ggsave("Output/Predicted growth by symbiont.jpg",figgrowth,width = 5, height = 3)

