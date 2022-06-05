library(RSQLite)
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
library(tidyr)
library(tidyverse)
library(rvest)
library(purrr)

### Exercise 1)
### First scrape the data for **Essen** on page 1 of immowelt: 

url_essen_1 <- ("https://www.immowelt.de/liste/essen/wohnungen/mieten?sort=relevanz")

html_essen_1 <- read_html(url_essen_1)

## a) Now scrape the URL for the first page (20 objects):

get_url_essen_1 <- function(html_essen_1){
  html_essen_1 %>%
    html_elements(".noProject-eaed4") %>%
    html_attr("href")
}
url.e1<- get_url_essen_1(html_essen_1)
url.e1

# b) Now scrape the title of the first 20 advertisements: 

get_title_essen_1 <- function(html_essen_1){
  html_essen_1 %>%
    html_elements("h2") %>%
    html_text() %>%
    str_trim()
}

title.e1<-get_title_essen_1(html_essen_1) 
title.e1

# c) Scraping the area of the city

get_city_essen_1 <- function(html_essen_1){
  html_essen_1 %>%
    html_elements(".IconFact-e8a23:nth-child(1) span") %>%
    html_text() %>%
    str_replace_all(".*\\,", "") %>%
    str_trim()
  
}
city.e1<- get_city_essen_1(html_essen_1) 
city.e1

# d) Scraping Zip Codes

html_test <- read_html("https://www.immowelt.de/expose/25wua5q")

## nur der Befehl innerhalb der Funktion funktioniert

get_zip_essen_1 <- function(html_essen_1){
  html_test %>%
    html_elements("#exposeAddress div") %>%
    html_text()
  
}
zip.e1 <- get_zip_essen_1(html_essen_1)
zip.e1

# e) cold rent 

get_cold_rent_essen_1 <- function(html_essen_1){
  html_essen_1 %>%
    html_elements(".KeyFacts-efbce div:nth-child(1)") %>%
    html_text() %>%
    str_trim()
  
} 

cold_rent.e1 <- get_cold_rent_essen_1(html_essen_1)
cold_rent.e1

# f) square meters

get_square_meters_essen_1 <- function(html_essen_1){
  html_essen_1 %>%
    html_elements(".KeyFacts-efbce div:nth-child(2)") %>%
    html_text() %>%
    str_trim()
  
}

sq_mt.e1 <- get_square_meters_essen_1(html_essen_1)
sq_mt.e1

# g) rooms

get_rooms_essen_1 <- function(html_essen_1){
  html_essen_1 %>%
    html_elements(".KeyFacts-efbce div:nth-child(3)") %>%
    html_text() %>%
    str_trim() 
}

rooms.e1 <- get_rooms_essen_1(html_essen_1)
rooms.e1

### Now scraping data for Essen page 2 of immowelt 

url_essen_2 <- ("https://www.immowelt.de/liste/essen/wohnungen/mieten?d=true&sd=DESC&sf=RELEVANCE&sp=2")

html_essen_2 <- read_html(url_essen_2)
html_essen_2

## a) Scraping the URL for the second page (20 objects)

get_url_essen_2 <- function(html_essen_2){
  html_essen_2 %>%
    html_elements(".noProject-eaed4") %>%
    html_attr("href")
}

url.e2 <-get_url_essen_2(html_essen_2)
url.e2

# b) Now Scraping the title of the remaining 20 advertisements 

get_title_essen_2 <- function(html_essen_2){
  html_essen_2 %>%
    html_elements("h2") %>%
    html_text() %>%
    str_trim()
}

title.e2 <- get_title_essen_2(html_essen_2) 
title.e2

# c) Scraping the area of the city

get_city_essen_2 <- function(html_essen_2){
  html_essen_2 %>%
    html_elements(".IconFact-e8a23:nth-child(1) span") %>%
    html_text() %>%
    str_replace_all(".*\\,", "") %>%
    str_trim()
}

city.e2 <- get_city_essen_2(html_essen_2) 
city.e2


# e) cold rent 

get_cold_rent_essen_2 <- function(html_essen_2){
  html_essen_2 %>%
    html_elements(".KeyFacts-efbce div:nth-child(1)") %>%
    html_text() %>%
    str_trim()
} 

cold_rent.e2 <-get_cold_rent_essen_2(html_essen_2)
cold_rent.e2

# f) square meters

get_square_meters_essen_2 <- function(html_essen_2){
  html_essen_2 %>%
    html_elements(".KeyFacts-efbce div:nth-child(2)") %>%
    html_text() %>%
    str_trim()
}

sq_mt.e2 <-get_square_meters_essen_2(html_essen_2)
sq_mt.e2

# g) rooms

get_rooms_essen_2 <- function(html_essen_2){
  html_essen_2 %>%
    html_elements(".KeyFacts-efbce div:nth-child(3)") %>%
    html_text() %>%
    str_trim() 
}

rooms.e2 <- get_rooms_essen_2(html_essen_2)
rooms.e2


### First scraping data for Bochum page 1 of immowelt 

url_bochum_1 <- ("https://www.immowelt.de/liste/bochum/wohnungen/mieten?sort=relevanz")

html_bochum_1 <- read_html(url_bochum_1)
html_bochum_1

## a) Scraping the URL for the first page (20 objects)

get_url_bochum_1 <- function(html_bochum_1){
  html_bochum_1 %>%
    html_elements(".noProject-eaed4") %>%
    html_attr("href")
}

url.b1 <-get_url_bochum_1(html_bochum_1)
url.b1

# b) Now Scraping the title of the first 20 advertisements 

get_title_bochum_1 <- function(html_bochum_1){
  html_bochum_1 %>%
    html_elements("h2") %>%
    html_text() %>%
    str_trim()
}

title.b1 <- get_title_bochum_1(html_bochum_1) 
title.b1

# c) Scraping the area of the city

get_city_bochum_1 <- function(html_bochum_1){
  html_bochum_1 %>%
    html_elements(".IconFact-e8a23:nth-child(1) span") %>%
    html_text() %>%
    str_replace_all(".*\\,", "") %>%
    str_trim()
}

city.b1 <-get_city_bochum_1(html_bochum_1) 
city.b1

# e) cold rent 

get_cold_rent_bochum_1 <- function(html_bochum_1){
  html_bochum_1 %>%
    html_elements(".KeyFacts-efbce div:nth-child(1)") %>%
    html_text() %>%
    str_trim()
} 

cold_rent.b1 <-get_cold_rent_bochum_1(html_bochum_1)
cold_rent.b1

# f) square meters

get_square_meters_bochum_1 <- function(html_bochum_1){
  html_bochum_1 %>%
    html_elements(".KeyFacts-efbce div:nth-child(2)") %>%
    html_text() %>%
    str_trim()
}

sq_mt.b1 <-get_square_meters_bochum_1(html_bochum_1)
sq_mt.b1

# g) rooms

get_rooms_bochum_1 <- function(html_bochum_1){
  html_bochum_1 %>%
    html_elements(".KeyFacts-efbce div:nth-child(3)") %>%
    html_text() %>%
    str_trim() 
}

rooms.b1 <-get_rooms_bochum_1(html_bochum_1)
rooms.b1

### Now scraping data for Bochum page 2 of immowelt 

url_bochum_2 <- 
  ("https://www.immowelt.de/liste/bochum/wohnungen/mieten?d=true&sd=DESC&sf=RELEVANCE&sp=2")

html_bochum_2 <- read_html(url_bochum_2)

## a) Scraping the URL for the second page (20 objects)

get_url_bochum_2 <- function(html_bochum_2){
  html_bochum_2 %>%
    html_elements(".noProject-eaed4") %>%
    html_attr("href")
}

url.b2 <- get_url_bochum_2(html_bochum_2)
url.b2

# b) Now Scraping the title of the remaining 20 advertisements 

get_title_bochum_2 <- function(html_bochum_2){
  html_bochum_2 %>%
    html_elements("h2") %>%
    html_text() %>%
    str_trim()
}
title.b2 <-get_title_bochum_2(html_bochum_2) 

# c) Scraping the area of the city

get_city_bochum_2 <- function(html_bochum_2){
  html_bochum_2 %>%
    html_elements(".IconFact-e8a23:nth-child(1) span") %>%
    html_text() %>%
    str_replace_all(".*\\,", "") %>%
    str_trim()
}
city.b2 <-get_city_bochum_2(html_bochum_2) 


# e) cold rent 

get_cold_rent_bochum_2 <- function(html_bochum_2){
  html_bochum_2 %>%
    html_elements(".KeyFacts-efbce div:nth-child(1)") %>%
    html_text() %>%
    str_trim()
} 
cold_rent.b2 <-get_cold_rent_bochum_2(html_bochum_2)

# f) square meters

get_square_meters_bochum_2 <- function(html_bochum_2){
  html_bochum_2 %>%
    html_elements(".KeyFacts-efbce div:nth-child(2)") %>%
    html_text() %>%
    str_trim()
}
sq_mt.b2 <-get_square_meters_bochum_2(html_bochum_2)

# g) rooms

get_rooms_bochum_2 <- function(html_bochum_2){
  html_bochum_2 %>%
    html_elements(".KeyFacts-efbce div:nth-child(3)") %>%
    html_text() %>%
    str_trim() 
}
rooms.b2 <-get_rooms_bochum_2(html_bochum_2)


# join data


# Essen
url_essen <-rbind(url.e1, url.e2)
rooms_essen<- rbind(rooms.e1, rooms.e2)
title_essen <-rbind(title.e1, title.e2)
city_essen <- rbind(city.e1, city.e2)
cold_rent_essen <- rbind(cold_rent.e1, cold_rent.e2)
sqr_mt_essen <-rbind(sq_mt.e1, sq_mt.e2)
rooms_essen <-rbind(rooms.e1, rooms.e2)

# Bochum

url_bochum <- rbind(url.b1, url.b2)
title_bochum <-rbind(title.b1, title.b2)
city_bochum <- rbind(city.b1, city.b2)
cold_rent_bochum <- rbind(cold_rent.b1, cold_rent.b2)
sqr_mt_bochum <- rbind(sq_mt.b1, sq_mt.b2)
rooms_bochum <- rbind(rooms.b1, rooms.b2)


# creating tibble


webscrape_data <- tibble( url_bochum, title_bochum, city_bochum, 
                          cold_rent_bochum, sqr_mt_bochum, rooms_bochum,
                          url_essen, title_essen, city_essen, 
                          cold_rent_essen, sqr_mt_essen, rooms_essen )

head(webscrape_data)
webscrape_data$cold_rent_bochum

write.csv2(webscrape_data, "Webscrape_Data.csv")

## Extract all data as Tibble
# Tibble Essen Page 1
extract_page1_data_essen <- function(url){
  html <- read_html(url) 
  tibble(city_essen = get_city_essen_1(html),
         cold_rent_essen = get_cold_rent_essen_1(html),
         rooms_essen = get_rooms_essen_1(html), 
         square_meter_essen = get_square_meters_essen_1(html), 
         title_essen = get_title_essen_1(html),
         url_essen=get_url_essen_1(html))
  
}

extract_page1_data_essen("https://www.immowelt.de/liste/essen/wohnungen/mieten?sort=relevanz")
city.e1

#Tibble essen Page 2
extract_page2_data_essen <- function(url){
  html <- read_html(url) 
  tibble(city_essen = get_city_essen_2(html),
         cold_rent_essen = get_cold_rent_essen_2(html),
         rooms_essen = get_rooms_essen_2(html), 
         square_meter_essen = get_square_meters_essen_2(html), 
         title_essen = get_title_essen_2(html),
         url_essen=get_url_essen_2(html))
  
}

extract_page2_data_essen("https://www.immowelt.de/liste/essen/wohnungen/mieten?d=true&sd=DESC&sf=RELEVANCE&sp=2")


# Tibble Bochum Page 1
extract_page1_data_bochum <- function(url){
  html <- read_html(url) 
  tibble(city_bochum = get_city_bochum_1(html),
         cold_rent_bochum = get_cold_rent_bochum_1(html),
         rooms_bochum = get_rooms_bochum_1(html), 
         square_meter_bochum = get_square_meters_bochum_1(html), 
         title_bochum = get_title_bochum_1(html),
         url_bochum=get_url_bochum_1(html))
  
}

extract_page1_data_bochum("https://www.immowelt.de/liste/bochum/wohnungen/mieten?sort=relevanz")

# tibble Bochum page 2
extract_page2_data_bochum <- function(url){
  html <- read_html(url) 
  tibble(city_bochum = get_city_bochum_2(html),
         cold_rent_bochum = get_cold_rent_bochum_2(html),
         rooms_bochum = get_rooms_bochum_2(html), 
         square_meter_bochum = get_square_meters_bochum_2(html), 
         title_bochum = get_title_bochum_2(html),
         url_bochum=get_url_bochum_2(html))
  
}

extract_page2_data_bochum("https://www.immowelt.de/liste/bochum/wohnungen/mieten?d=true&sd=DESC&sf=RELEVANCE&sp=2")


## Exercise Number 2)


#a) load the immowelt date in R
load(here::here("rent_advertisements.RData"))


## omit column "heating_cost_excluded" as it contains no data (NAs) 
immowelt %<>%
  select( - heating_cost_excluded)

head(immowelt)
names(immowelt) 

#overview of the data
immowelt %>%
  knitr::kable(booktabs = TRUE, linesep = "",
               escape = TRUE, caption = 'Immowelt'
  ) |>
  kableExtra::kable_styling(font_size = 10,
                            latex_options = c("striped", "hold_position")) %>%
  kableExtra::row_spec(0, bold = TRUE)


#### clean all non-available data (NA)
immowelt_clean <- na.omit(immowelt)
immowelt_clean

# Ensure that all variables of the dataset are stored using a proper class
char_v <- c("title","building_year")

immowelt_clean %<>%
  mutate(across(where(is.character) &! any_of(char_v),
                as.factor))

immowelt_clean %<>%
  mutate(building_year = as.numeric(building_year))


#b) calculate the cold_rent und warm_rent per square meter

immowelt_clean %<>%
  mutate(cold_rent_qm = cold_rent / square_meter) %>%
  mutate(warm_rent_qm = warm_rent / square_meter)

immowelt_clean$cold_rent_qm
immowelt_clean$warm_rent_qm


#c) Create one table with the five districts with the most ads for each city

ads_essen <- immowelt_clean %>%
  filter(city =="essen") %>% 
  group_by(zipcode) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice(1:5)

ads_bochum <- immowelt_clean%>%
  filter(city=="bochum")%>% 
  group_by(zipcode)%>%
  summarise(count=n())%>%
  arrange(desc(count))%>%
  slice(1:5)

full_join(ads_bochum,ads_essen)  # one table for each city


#d)
immowelt_clean %>%
  mutate(zipcode = factor(zipcode, c("45141","45147","45279","45326","45355",
                                   "44793","44795","44866","44809","44801"))) %>%
  drop_na() %>%
  ggplot(aes(x=zipcode, y=warm_rent, color=city)) +
  geom_boxplot() +
  labs(title = 'Boxplot for the ten district with the most ads for essen and bochum',
       x ="\n Zipcode", y ="\n warm rent")+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


#e)

immowelt_clean %>%
  ggplot(aes(x = square_meter, y = cold_rent_qm, color = city)) +
  geom_point() +
  geom_smooth(se=FALSE, method = "lm") +
  labs(title = "Relatonship between square meter and warm rent per square meter",
       x ="square meter", y ="cold rent per qm")

immowelt_clean %>%
  ggplot(aes(x = square_meter, y = warm_rent_qm, color = city)) +
  geom_point() +
  geom_smooth(se=FALSE, method = "lm") +
  labs(title = "Relatonship between square meter and warm rent per square meter",
       x="square meter", y="warm rent per qm")

#f)
immowelt_clean%>%
  ggplot(aes(x=cold_rent, y=warm_rent, color=city))+
  geom_boxplot()+
  labs(title = "Comparing absolut warm Rent vs. cold Rent for Bochum and Essen",
       x="Cold rent",y="Warm rent" )


immowelt_clean%>%
  ggplot(aes(x=cold_rent_qm, y=warm_rent_qm, color=city))+
  geom_boxplot()+
  labs(title = "Comparing warm Rent vs. cold Rent per square meter for Bochum and Essen",
       x ="Cold rent per qm", y = "Warm rent per qm")

#g)

immowelt_clean %>%
  select(heating_cost_included, square_meter, warm_rent) %>%
  mutate(share_nebenkosten = (heating_cost_included+immowelt_clean$service_charges)
         /warm_rent*100) %>%
  ggplot(aes(x=square_meter, y=share_nebenkosten))+
  geom_point()+
  labs(title = " Share of Nebenkosten of the warm rent",
       x="Appartment size in square meter", y="Share in %")


#h)
#(i)
immowelt_clean %>%
  select(efficiency_class,energy_demand, city)%>%
  mutate(efficiency_class = factor(efficiency_class,c(
    "A","A+","B","C","D","E",
    "F","G","H"))) %>%
  drop_na() %>%
  ggplot(aes(efficiency_class, energy_demand, color = city))+
  geom_point()

##efficiency classes differ in terms of their energy demand. Low energy classes
## seem to correlate with high energy demand. High energy classes seem to
## correlate with low energy demands.

#(ii)
immowelt_clean %>%
  select(efficiency_class, city)%>%
  drop_na()%>%
  mutate(efficiency_class=factor(efficiency_class,c(
    "A","A+","B","C","D","E",
    "F","G","H"))) %>%
  ggplot(aes(x=efficiency_class))+
  geom_bar(stat = 'count', width = 0.5, fill = '#004c93')+
  labs(title = "Amount of adds among the efficiency classes",
       x=" energy class", "amount", y = "ad count")

## most ads are distributed at energy class "D"

## (iii+vi)
cor<-immowelt_clean %>%
  mutate(across(where(is.character), as.numeric))%>%
  mutate(across(where(is.factor), as.numeric))%>%
  select_if(is.numeric) %>%
  select(-title) %>%
  cor()


  
cor%>%
  as_tibble() %>%
  mutate(var1 = colnames(cor))%>%
  pivot_longer(-var1, names_to = "var2", values_to = "value")%>%
  ggplot(aes(var1, var2, fill=value))+
  geom_tile()
cor
cor[11,10]
# correlation between effiency_class and building_year= -0.3325452

cor[11,3]
#correlation between effiency_class and cold_rent= -0.14201658

## in both cases, theres a slight negative correlation. The whole dataset
## is included in this calculation.

####In the following the variables "building year", "cold rent" and "efficiency
##class" will be considered for calculating the correlation.

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
cor
cor[1,2] ## correlation between building year and efficiency class is -0.14201
cor[1,3]## correlation between cold rent and efficiency class is -0.3325.

# the results do not differ.

## a slight negative correlation between the efficiency class and the building
## year could be explained by improved building structure and heat efficient
## heating systems. It´s logical to assume that with decreasing building year, 
## the efficiency class will rise to a better grading.
##The same argumentation can be applied to the correlation between the efficiency
## class and the cold rent. A well build and innovative house will generally
## cost a higher cold rent which results in better insulation and hence a lower 
## (= better) efficiency class.

# i)
reg1<-lm(warm_rent~efficiency_class+rooms+building_year+square_meter+service_charges+city,
         data = immowelt_clean)
summary(reg1)  

## (Intercept)        -618.6383   942.9045  -0.656   0.5130 


reg2<-lm(warm_rent~efficiency_class+rooms+building_year+square_meter+service_charges+zipcode,
         data = immowelt_clean)
summary(reg2)

## (Intercept)        4691.89866 1780.57309   2.635  0.00952 ** 


reg3<-lm(warm_rent~efficiency_class+rooms+building_year+square_meter+service_charges+zipcode+city,
         data = immowelt_clean)
summary(reg3)



#If both zipcode and city are used in the regression analysis, it might result
# in multicollinearity, which will lead to possibly reduced accuracy of the predictions.

##To determine which model is more fitting, cross validation is used in the
## following

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

# The prediciton with using as variable zipcode is better fit.

mod_full<-lm(warm_rent~efficiency_class+rooms+building_year+square_meter+service_charges+
               zipcode+city, data = train)

test%>%
  rmse(mod_full,.)

summary(mod_full)
summary(mod_city)
summary(mod_zipcode)

## for mod_full has to much impact, a possible explanation could be 
## multicollinearity due to using zipcode and city in one regression


## Number 3

library(dplyr)
library(DBI)
library(RSQLite)

# a) function that uses a standard ggplot2 theme
theme_favourite <- function(){
  theme_classic() %+replace%
    theme(
      text = element_text(family = "serif", size = 16),
      panel.spacing = unit(2, "cm"),
      panel.grid.major = element_line(colour = "blue"),
      panel.grid.minor = element_line(color = "blue")
      
    )
}
   
 
# b)
ggscatt <- function(data,x,y){
  ggplot(data, aes(x = x, y=y)) +
    geom_point() +
    theme_favourite()+
    labs(title = "A scatter plot") +
    facet_grid()
}

ggscatt(data = mtcars, mtcars$mpg , mtcars$hp)   


## c) relative connection to the database

connection <- DBI::dbConnect(
  drv = RSQLite::SQLite(),
  dbname = here::here("assignment_1.sqlite3"),
)

# show the list of table in database
DBI::dbListTables(connection)


# d)  create an object which is a reference to a table in the database
metro<-tbl(connection, "metro")
head(metro)

# actually pulls the data into R.
metro_R<-metro %>% 
  collect(n=Inf)
head(metro_R)


# e) Ensure, that all variables are stored using a proper class,
# convert date_time as a time vector
metro_R%<>%
  mutate(date_time=as_datetime(date_time))
tail(metro_R) #works good.


# f) create a new variable weekday that represents the weekdays
metro_R %<>%
  mutate(weekday=weekdays(date_time))
  

#g 

ggscatt(data = metro_R, metro_R$traffic_volume, metro_R$temp)
ggscatt(data = metro_R, metro_R$traffic_volume, metro_R$rain_1h)
ggscatt(data = metro_R, metro_R$traffic_volume, metro_R$snow_1h)
ggscatt(data = metro_R, metro_R$traffic_volume, metro_R$clouds_all)




# h) create a boxplot with weekday on x-asix, traffic_volume on y-axis
metro_R %>%
  mutate(weekday=factor(weekday, c("Montag","Dienstag","Mittwoch","Donnerstag","Freitag",
                                   "Samstag","Sonntag"))) %>%
  ggplot(aes(x=weekday, y=traffic_volume))+
  geom_boxplot(color="steelblue")+
  labs(title = "Boxplot of traffic volume and Weekday", 
       x="Weekday", y="traffic Volume")
  


#i) add out modify dataframe to the database as metro_2
dbWriteTable(connection,
                  name="metro_2",
                  value=metro_R)
             

dbListTables(connection)

## Test
metro_2<-tbl(connection, "metro_2")
head(metro_2)

# actually pulls the data into R.
metro_R_2<-metro_2 %>% 
  collect(n=Inf)
head(metro_R_2)
# works :)

#von datenbank disconecten
dbDisconnect(connection)



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


