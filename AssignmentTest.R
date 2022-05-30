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

### heating_cost_excluded Spalte rausschmeißen, dort sind nur NAs
immowelt%<>%
  select(-heating_cost_excluded)

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


####NA raus
immowelt_clean<-na.omit(immowelt)

# Stellen Sie anschließend sicher, dass alle Variablen in einer geeigneten Klasse gespeichert werden
char_v<-c("title","building_year")

immowelt_clean%<>%
  mutate(across(where(is.character) &! any_of(char_v),
                as.factor))

immowelt_clean%<>%
  mutate(building_year=as.numeric(building_year))

#b) Kalkulieren cold_rent und warm_rent per square meter
immowelt_clean%<>%
  mutate(cold_rent_qm=cold_rent/square_meter) %>%
  mutate(warm_rent_qm=warm_rent/square_meter)

#c) Tabelle mit 5 zipcode mit den meisten Anzeigen für jede Stadt
immowelt_clean%>%
  filter(city=="essen")%>% 
  group_by(zipcode)%>%
  summarise(count=n())%>%
  arrange(desc(count))%>%
  slice(1:5)

immowelt_clean%>%
  filter(city=="bochum")%>% 
  group_by(zipcode)%>%
  summarise(count=n())%>%
  arrange(desc(count))%>%
  slice(1:5)

#d)
immowelt_clean %>%
  mutate(zipcode=factor(zipcode, c("45141","45147","45279","45326","45355",
                                   "44793","44795","44866","44809","44801"))) %>%
  drop_na() %>%
  ggplot(aes(x=zipcode, y=warm_rent, color=city))+
  geom_boxplot()+
  labs(title = 'Boxplot for the ten district with the most ads for essen and bochum',
       x="\n Zipcode", y="\n warm rent")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))


#e)
immowelt_clean%>%
  ggplot(aes(x=square_meter, y=cold_rent_qm, color=city))+
  geom_point()+
  geom_smooth(se=FALSE)+
  labs(title = "Relatonship between square meter and warm rent per square meter",
       x="square meter", y="cold rent per qm")

immowelt_clean%>%
  ggplot(aes(x=square_meter, y=warm_rent_qm, color=city))+
  geom_point()+
  geom_smooth(se=FALSE)+
  labs(title = "Relatonship between square meter and cold rent per square meter",
       x="square meter", y="warm rent per qm")

#f)
immowelt_clean%>%
  ggplot(aes(x=cold_rent,y=warm_rent, color=city))+
  geom_boxplot()+
  labs(title = "Comparing absolut Warm Rent vs. Cold Rent for Bochum and Essen",
       x="Cold rent",y="Warm rent" )


immowelt_clean%>%
  ggplot(aes(x=cold_rent_qm, y=warm_rent_qm, color=city))+
  geom_boxplot()+
  labs(title = "Comparing Warm Rent vs. Cold Rent per square meter for Bochum and Essen",
       x="Cold rent per qm", y="Warm rent per qm")

#g)
immowelt_clean%>%
  select(heating_cost_included, square_meter, warm_rent)%>%
  mutate(ship_nebenkosten=heating_cost_included/warm_rent*100)%>%
  ggplot(aes(x=square_meter, y=ship_nebenkosten))+
  geom_point()+
  labs(title = " Share of Nebenkosten of the warm rent",
       x="Apprtment Size in square meter", y="Shape in %")

#h)
#(i)
immowelt_clean%>%
  select(efficiency_class,energy_demand, city)%>%
  mutate(efficiency_class=factor(efficiency_class,c(
    "A","A+","B","C","D","E",
    "F","G","H")))%>%
  drop_na()%>%
  ggplot(aes(efficiency_class, energy_demand, color=city))+
  geom_boxplot()

##efficiency classes differ in terms of their energy demand  

#(ii)
immowelt_clean%>%
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
cor<-immowelt_clean %>%
  mutate(across(where(is.character), as.numeric))%>%
  mutate(across(where(is.factor), as.numeric))%>%
  select_if(is.numeric) %>%
  select(-title) %>%
  cor()

cor%>%
  as_tibble()%>%
  mutate(var1=colnames(cor))%>%
  pivot_longer(-var1, names_to = "var2", values_to = "value")%>%
  ggplot(aes(var1, var2, fill=value))+
  geom_tile()

cor[11,10]
# correlation between effiency_class and building_year= -0.3325452

cor[11,3]
#correlation between effiency_class and cold_rent= -0.14201658

#### oder mit nur den drei Variablen
cor<-immowelt_clean%>%
  select(efficiency_class, building_year, cold_rent)%>%
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


# i)
reg1<-lm(warm_rent~efficiency_class+rooms+building_year+square_meter+service_charges+city,
         data = immowelt_clean)
summary(reg1)  


reg2<-lm(warm_rent~efficiency_class+rooms+building_year+square_meter+service_charges+zipcode,
         data = immowelt_clean)
summary(reg2)


reg3<-lm(warm_rent~efficiency_class+rooms+building_year+square_meter+service_charges+zipcode+city,
         data = immowelt_clean)
summary(reg3)

#because maybe both cities can have the same zipcode. If we use both in one regression, we 
# maybe calculate it as double?^^ 

#oder mithilfe cross-validierung
set.seed(123)
train<-immowelt_clean%>%
  slice_sample(prop = 0.8)

test<-immowelt_clean%>%
  anti_join(train)

mod_city<-lm(warm_rent~efficiency_class+rooms+building_year+square_meter+service_charges+city,
             data = train)
summary(mod_city) 

mod_zipcode<-lm(warm_rent~efficiency_class+rooms+building_year+square_meter+service_charges+zipcode,
                data = train)

summary(mod_zipcode)

#For both models, compute the RMSE on the ‘test‘ dataset.
test%>%
  rmse(mod_city,.)

test%>%
  rmse(mod_zipcode,.)

# The prediciton with using zipcode is better.

mod_full<-lm(warm_rent~efficiency_class+rooms+building_year+square_meter+service_charges+
               zipcode+city, data = train)

test%>%
  rmse(mod_full,.)

summary(mod_full)
summary(mod_city)
summary(mod_zipcode)
# bei mod_full haben wir möglicherweise zu großen effekt,
# weil wir schon einzeln städte angucken und die Postleizahl ebenfalls die städte indirekt trennen.
# Doppelter Effekt ?











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
  