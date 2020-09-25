library(ggrepel)

library(gghighlight)

library(fmsb)

library(reshape2)

library(colorspace)

library(purrr)
library(ggpubr)

library(forcats)

library(dplyr)

library(plotly)

library(stringr)

library(leaflet)

library(ggmap)

library(caTools)
library(plotrix)

#Reading Data into R studio#
Sample_Data <-read.csv("C:/Users/S.Puppala/Desktop/Data.csv",header = TRUE)

#summarize the data#
head(Sample_Data,row=6)
str(Sample_Data)
summary(Sample_Data)

#Data preprocessing#
#deleting all the unwanted columns#
Sample_Data<-(subset(Sample_Data,,-c(Loaned.From,Club.Logo,Photo,Flag,Real.Face,Jersey.Number,ID,Release.Clause,Value)))
#fromating  wage column#
#create a function to extract digits of value,wageand recall#
extract <- function(x){
  regexp <- "[[:digit:]]+"
  str_extract(x, regexp)
}
#split characters in all above columns#
Sample_Data$wageLast <- sapply(strsplit(as.character(Sample_Data$Wage), ""), tail, 1)

#replace with numerics using functions#
temp1 <- sapply(Sample_Data$Wage, extract)

Sample_Data$Wage <- as.numeric(temp1)
Sample_Data$Wage <- ifelse(Sample_Data$wageLast== "M", Sample_Data$Wage * 1000000, Sample_Data$Wage * 1000)

#creating BMI column#

#creating function h to replace ' by . in height#

h <-function(y) {gsub("'\\s*", ".", y)}

# converting height and weight to numeric#

Sample_Data$Weight<-as.numeric(sapply(Sample_Data$Weight,extract))
Sample_Data$Height<-as.numeric(sapply(Sample_Data$Height,h ))

# formuale for bmi and storing it in new dimension#

Sample_Data$bmi<- ((Sample_Data$Weight*(0.453))/(((Sample_Data$Height*(0.3))^2)))

#deleting null values and extra columns#
sum(is.na(Sample_Data))
Sample_Data<-(na.omit(Sample_Data))
Sample_Data$wageLast<-NULL
#correcting errors in body type column#
summary(Sample_Data$Body.Type)
Sample_Data$Body.Type[Sample_Data$Body.Type=="Messi"] <-"Lean"
Sample_Data$Body.Type[Sample_Data$Body.Type=="C. Ronaldo"] <-"Normal"
Sample_Data$Body.Type[Sample_Data$Body.Type=="Akinfenwa"] <-"Stocky"
Sample_Data$Body.Type[Sample_Data$Body.Type=="Courtois"] <-"Lean"
Sample_Data$Body.Type[Sample_Data$Body.Type=="Neymar"] <-"Normal"
Sample_Data$Body.Type[Sample_Data$Body.Type=="Shaqiri"] <-"Stocky"
Sample_Data$Body.Type[Sample_Data$Body.Type=="PLAYER_BODY_TYPE_25"] <-"Null"
Sample_Data$Body.Type[na.omit(Sample_Data$Body.Type)]





#body type visulization#
#creating table by different body typeclasses#

body=as.data.frame(sort(table(Sample_Data$Body.Type)))
colnames(body)<-c("body type","No:of Players")
body=body[-c(1:8),]
#creating percentages table of body types#
body <-body%>%
  arrange(desc(body$`body type`)) %>%
  mutate(prop = round(body$`No:of Players`*100/sum(body$`No:of Players`),),
         lab.ypos = cumsum(prop) - 0.5*prop)
pct=paste(body$`body type`,body$prop)
#pie chart with plotrix package#
pie3D(body$`No:of Players`,labels=pct,explode=0.1,
      main="Pie Chart of body types ")


#barplot oftop 10 countries by number of players#
country=as.data.frame(sort(table(Sample_Data$Nationality)))
colnames(country)<-c("nation","No:of Players")
country<-as.data.frame(tail(country,n=10))

#using ggpubr pacakage#
ggplot(country, aes(x =country$nation, y = country$`No:of Players`)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label =country$`No:of Players`), vjust = -0.3) + 
  theme_pubclean()
#distribution of age#
plot(Sample_Data$Age,Sample_Data$International.Reputation,, pch=16, type="h", main="scatterplot of age vs reputation")
ggplot(aes(Sample_Data$Age,Sample_Data$International.Reputation),data=Sample_Data,color="blue") +
  geom_line()
#clubs with most valuble players#
clubwage<-as.data.frame(Sample_Data[c("Club","Wage")])
clubwage$Club<-as.factor(clubwage$Club)
clubwage<-clubwage %>%
  group_by(Club) %>% summarise(Total = round(sum(Wage)))
clubwage<-clubwage[-1,]
clubwage<-clubwage[order(-clubwage$Total),]
clubwage<-head(clubwage,n=10)
clubwage$Total<-as.numeric(clubwage$Total)

ggplot(clubwage, aes(x=Club, y=Total)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Ordered Bar Chart", 
       subtitle="top 10 costly teams", 
       caption="source: mpg") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))





#Scatterplot of Age  by reputation#
plot(Sample_Data$Age~Sample_Data$International.Reputation,
     xlab="Repuataion",
     ylab = "Age",
     main="reputation by Age",
     pch=20,cex=2,
     col ="blue")

#clubs by players reputation#
Sample_Data$club<-as.factor(Sample_Data$Club)

 club_repu<- as.data.frame(Sample_Data[c("Club","International.Reputation")])

 club_repu$Club<-as.factor(club_repu$Club)
 club_repu<- club_repu%>%
                 group_by(Club)%>%
                 summarise(total=sum(International.Reputation))
 club_repu<-club_repu[-1,]
 club_repu<-club_repu[order(-club_repu$total),]
 club_repu<-head(club_repu,n=10)
 ggplot(club_repu, aes(y =total,
                      x = Club,
                      group =Club)) +
   geom_text(aes(label =total),vjust = -0.3)+
   geom_bar(stat = "identity", position = "dodge", aes(fill = Club))

#prefered foot vs sprintSpeed#
prefered<- data.frame(subset(Sample_Data,,c(Preferred.Foot,SprintSpeed)))
prefered$Preferred.Foot<-as.factor(prefered$Preferred.Foot)
prefered<-prefered%>%
  group_by(Preferred.Foot)%>%summarise(avg=mean(SprintSpeed),n=length(Preferred.Foot))
info=(paste(round(prefered$avg,2),", ",N=prefered$n))
ggplot(prefered, aes(y =avg,
               x = Preferred.Foot,
               group =Preferred.Foot)) +
  geom_text(aes(label =info),vjust = -0.3)+
  geom_bar(stat = "identity", position = "dodge", aes(fill = Preferred.Foot))
#Stastical Tests on(can left foot players sprint fast than Right)
prefered<- data.frame(subset(Sample_Data,,c(Preferred.Foot,SprintSpeed)))
test<-(t.test(SprintSpeed ~ Preferred.Foot, data = prefered,
       alternative = c("greater"), var.equal = TRUE))


