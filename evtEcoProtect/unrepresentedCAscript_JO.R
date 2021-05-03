getwd()

library(tidyverse)

#Loading CA data
evt_ca <- read.csv("./EVT_Percentages/US200EVT_CA.csv")
tnc_ca <- read.csv("./EVT_Percentages/US200EVT_CA_TNC.csv")

#I want to pull the EVTs which exist in CA but not in TNC_CA into their own table

#First let's add the PERCENT column that mw did.
evt_ca<-evt_ca%>%
  rename(COUNT_CA=COUNT)%>%
  arrange(desc(COUNT_CA))%>%
  mutate(PERCENT_CA=round(COUNT_CA/sum(COUNT_CA)*100,4),
         RANK_CA=1:pull(tally(evt_ca)))
tnc_ca<-tnc_ca%>%
  rename(COUNT_TNC=COUNT)%>%
  arrange(desc(COUNT_TNC))%>%
  mutate(PERCENT_TNC=round(COUNT_TNC/sum(COUNT_TNC)*100,4),
         RANK_TNC=1:pull(tally(tnc_ca)))
#aaand join
ca <- left_join(evt_ca, tnc_ca) %>%
  replace(is.na(.), 0)

#So to get the EVTs which are not owned by TNC, we want the rows in which the COUNT_TNC = 0
ca_notowned<-ca%>%
  filter(COUNT_TNC==0)%>%
  select(-COUNT_TNC,-PERCENT_TNC,-RANK_TNC)

#First select the top ten unrepresented EVTs

ca_tennotowned<-slice(ca_notowned,1:10)

#Now group by EVT_PHYS
phys_ca_notowned<-ca_notowned %>% group_by(EVT_PHYS)%>%
  summarise(PHYS_PERCENT_CA = sum(PERCENT_CA))%>%
  arrange(desc(PHYS_PERCENT_CA))

#graph time
library(ggplot2)

ggplot(phys_ca_notowned, aes(x=EVT_PHYS,y=PHYS_PERCENT_CA))+
  geom_bar(stat='identity', position='dodge')+
  xlab("EVT Phys Type")+
  ylab("Percent of CA Area")+
  ggtitle("Percent Areas of EVT PHYS Types in CA not owned by TNC")+
  theme_bw()

#quesiton for myles- how do I not make the x axis sorted alphabetically? I want it sorted by rank probably

top2bot <- df %>%
  arrange(desc(diff)) %>%
  mutate(EVT_NAME = factor(EVT_NAME, levels = EVT_NAME))

#Remove agricultural and developed PHYS categories from pys_ca_notowned

phys_ca_notowned<-phys_ca_notowned%>%
  filter(EVT_PHYS!='Agricultural' & EVT_PHYS!='Developed' & EVT_PHYS!='Snow-Ice')

#Sort top 10 not owned EVTs by percent instead of alphabetically
orderca_tennotowned<-ca_tennotowned%>%
  arrange(desc(PERCENT_CA))%>%
  mutate(EVT_NAME = factor(EVT_NAME, levels=EVT_NAME))
  

#I want to replace the "sparsely vegetated" character in the orderca_tennotowned table with "SparselyVegetated" 
#So I can manually enter the colors I want for the plot
library(stringr)
EVT_PHYSNS<-str_replace(orderca_tennotowned$EVT_PHYS," ","")

orderca_tennotowned$EVT_PHYSNS<-EVT_PHYSNS

orderca_tennotowned=subset(orderca_tennotowned, select=-EVT_PHYS)

#Now manually enter colors for plot 
ggplot(orderca_tennotowned, aes(x=EVT_NAME, y=PERCENT_CA, fill=EVT_PHYSNS))+
  geom_bar(stat='identity',position='dodge')+
  coord_flip()+
  labs(x="EVT",y="Percent of CA",title="Top 10 EVT's in CA not owned by TNC")+
  scale_fill_manual(values=c(Conifer="olivedrab4", Riparian="navy", Grassland="navajowhite1", Shrubland="orange2", Hardwood= "orange4", SparselyVegetated="olivedrab1"))
