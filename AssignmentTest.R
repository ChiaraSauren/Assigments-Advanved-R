## Aufgabe 2
library(dplyr)
library(knitr)
library(stringr)
library(here)
library(stringi)
library(janitor)
library(magrittr)
library(lubridate)
library(tibble)
library(kableExtra)
library(ggplot2)
library(zoo)
library(stargazer)
library(modelr)
library(here)
library(tidyr)
library(tidyverse)

#a)
load(here("rent_advertisements.RData"))
immowelt

head(immowelt)
names(immowelt)

#überblick
immowelt %>%
  knitr::kable(booktabs = TRUE, linesep = "",
               escape = TRUE, caption = 'Immowelt'
  ) |>
  kableExtra::kable_styling(font_size = 10,
                            latex_options = c("striped", "hold_position")) %>%
  kableExtra::row_spec(0, bold = TRUE)

# Stellen Sie anschließend sicher, dass alle Variablen in einer geeigneten Klasse gespeichert werden
char_v<-c("title","building_year")

immowelt%<>%
  mutate(across(where(is.character) &! any_of(char_v),
                as.factor))

#b) Kalkulieren cold_rent und warm_rent per square meter
immowelt%<>%
  mutate(cold_rent_qm=cold_rent/square_meter) %>%
  mutate(warm_rent_qm=warm_rent/square_meter)

#c) Tabelle mit 5 zipcode mit den meisten Anzeigen für jede Stadt
immowelt%>%
  filter(city=="essen")%>% 
  group_by(zipcode)%>%
  summarise(count=n())%>%
  arrange(desc(count))%>%
  slice(1:5)

immowelt%>%
  filter(city=="bochum")%>% 
  group_by(zipcode)%>%
  summarise(count=n())%>%
  arrange(desc(count))%>%
  slice(1:5)

#d)

immowelt %>%
  mutate(zipcode=factor(zipcode, c("45141","45147","45279","45326","45355",
                                   "44793","44795","44866","44809","44801"))) %>%
  ggplot(aes(x=zipcode, y=warm_rent, color=city))+
  geom_boxplot()+
  labs(title = 'Boxplot for the ten district with the most ads for essen and bochum')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))


#e)
immowelt%>%
  ggplot(aes(x=square_meter, y=cold_rent_qm, color=city))+
  geom_point()+
  geom_smooth(se=FALSE)

immowelt%>%
  ggplot(aes(x=square_meter, y=warm_rent_qm, color=city))+
  geom_point()+
  geom_smooth(se=FALSE)

#f)
immowelt%>%
  ggplot(aes(x=cold_rent,y=warm_rent, color=city))+
  geom_boxplot()+
  labs(title = "Comparing absolut Warm Rent vs. Cold Rent for Bochum and Essen",
       x="Cold rent",y="Warm rent" )


immowelt%>%
  ggplot(aes(x=cold_rent_qm, y=warm_rent_qm, color=city))+
  geom_boxplot()+
  labs(title = "Comparing Warm Rent vs. Cold Rent per square meter for Bochum and Essen",
       x="Cold rent", y="Warm rent")

#g)
# da war ein Außreiser, warmmiete bei 75 was kein sinn ergeben hat. Wusste nicht wie
#man die Zeile löscht, also habe ich den Eintrag zu 750 gemacht. Es war die Zeile 219 :)
immowelt$warm_rent[219]<-750

immowelt%>%
  select(heating_cost_included, square_meter, warm_rent)%>%
  mutate(ship_nebenkosten=heating_cost_included/warm_rent*100)%>%
  ggplot(aes(x=square_meter, y=ship_nebenkosten))+
  geom_point()+
  labs(title = " Share of Nebenkosten of the warm rent",
       x="Apprtment Size", y="Shape in %")

#h)
#(i)
immowelt%>%
  select(efficiency_class,energy_demand, city)%>%
  mutate(efficiency_class=factor(efficiency_class,c(
    "A","A+","B","C","D","E",
    "F","G","H")))%>%
  drop_na()%>%
  ggplot(aes(efficiency_class, energy_demand, color=city))+
  geom_boxplot()


##efficiency classes differ in terms of their energy demand  

#(ii)
immowelt%>%
  select(efficiency_class, city)%>%
  drop_na()%>%
  mutate(efficiency_class=factor(efficiency_class,c(
    "A","A+","B","C","D","E",
    "F","G","H"))) %>%
  ggplot(aes(x=efficiency_class))+
  geom_bar(stat = 'count', width = 0.5, fill = '#004c93')+
  labs(title = "Amount of adds among the efficiency classes",
       x="classes", "Amount")

## (iii+vi)
cor<-immowelt %>%
  mutate(across(where(is.character), as.numeric))%>%
  mutate(across(where(is.factor), as.numeric))%>%
  select_if(is.numeric) %>%
  cor()

cor%>%
  as_tibble()%>%
  mutate(var1=colnames(cor))%>%
  pivot_longer(-var1, names_to = "var2", values_to = "value")%>%
  ggplot(aes(var1, var2, fill=value))+
  geom_tile()

#there is no correlation between effiency_calss and building_year 
# and beetween efficiency_class and cold_rent
# but normaly there schould be. This depends here because of type of data and Lots of NA


# i)
reg1<-lm(warm_rent~efficiency_class+rooms+building_year+square_meter+service_charges+city,
         data = immowelt)
summary(reg1)  


reg2<-lm(warm_rent~efficiency_class+rooms+building_year+square_meter+service_charges+zipcode,
         data = immowelt)
summary(reg2)


reg3<-lm(warm_rent~efficiency_class+rooms+building_year+square_meter+service_charges+zipcode+city,
         data = immowelt)
summary(reg3)

#because maybe both cities can have the same zipcode. If we use both in one regression, we 
# maybe calculate it as double?^^ 











### 4.

## a)

$ git rm byeGit.txt  # removing and staging removal of byeGit.txt

$ git add HelloGit.txt  # staging for commit

$ git status  # make sure HelloGit.txt and byeGit.txt are staged

$ git commit -m "Fixes to file"    # HelloGit.txt committed

$ git add assignment_1.sqlite3     # track files for staging
$ git add rent_advertisment.RData  

$ git commit -m "data added"   # committed files


## b)

$ git add -p HelloGit.txt ## break down file into hunks, Git will prompt you 
                          ## with a choice which hunks to stage for next commit
$ git commit -m "Part 1"

$ git add HelloGit.txt  ## stage rest of commitment

$ git commit -m "Part2" ## commit rest of commitment
  