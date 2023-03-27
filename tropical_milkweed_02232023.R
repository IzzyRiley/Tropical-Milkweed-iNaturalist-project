### Tropical Milkweed Analysis ###
# 2/23/2022

# Load in libraries
#install.packages("dplyr")
#install.packages("ggplot")
#install.packages("lubridate")

# for any new packages
library(dplyr)
library(ggplot2)
library(lubridate)

##### Read in Data and Rbind #####
# Read in data
d.cali <- read.csv("d.cali.01192023.csv")
d.texa <- read.csv("d.texa.01242023.csv")
d.flor <- read.csv("d.flor.01202023.csv")

# merge all three state files into one large file
d.all <- rbind(d.cali, d.texa, d.flor)

# data cleaning here
# removing entries that aren't resolved to the county level
d.all <- subset(d.all, d.all$place_county_name != "")

# look at what's in this column
d.all$observed_on

# fix data file names - change d.cord to d.all
# formatting date columns
d.all$observed_on <- as.Date(d.all$observed_on, "%Y-%m-%d")
# check date format before running this

d.all$year <- year(d.all$observed_on)
d.all$month <- month(d.all$observed_on)

#Limiting Data to the First 8 Months of the Year
d.all <- subset(d.all, d.all$year < 2023)

# adding a new column yes or no tropical milkweed
d.all$tropical <- "no"

d.all$tropical[d.all$scientific_name == "Asclepias curassavica"] <- "yes"


# adding a new column with city name
d.all$metro <- NA

bay_counties <- c("Alameda", "Contra Costa", "Marin", "Napa", "San Francisco", "San Mateo", "Santa Clara", "Solano", "Sonoma")
d.all$metro[d.all$place_county_name %in% bay_counties & d.all$place_state_name == "California"] <- "San Francisco Metro"

lametro_counties <- c("Los Angeles", "Orange", "San Bernardino", "Riverside", "Ventura")
d.all$metro[d.all$place_county_name %in% lametro_counties & d.all$place_state_name == "California"] <- "LA Metro"

d.all$metro[d.all$place_county_name == "San Diego" & d.all$place_state_name == "California"] <- "San Diego Metro"

austinmetro_counties <- c("Travis", "Hays", "Williamson", "Bastrop", "Caldwell")
d.all$metro[d.all$place_county_name %in% austinmetro_counties & d.all$place_state_name == "Texas"] <- "Austin Metro"

sanantoniometro_counties <- c("Kendall", "Comal", "Guadalupe", "Bandera", "Bexar", "Wilson", "Medina", "Atascosa")
d.all$metro[d.all$place_county_name %in% sanantoniometro_counties & d.all$place_state_name == "Texas"] <- "San Antonio Metro"

houstonmetro_counties <- c("Montgomery", "Liberty", "Chambers", "Harris", "Galveston", "Brazoria", "Fort Bend", "Austin", "Waller")
d.all$metro[d.all$place_county_name %in% houstonmetro_counties & d.all$place_state_name == "Texas"] <- "Houston Metro"

DFWmetro_counties <- c("Wise", "Denton", "Collin", "Hunt", "Rockwall", "Kaufman", "Ellis", "Dallas", "Tarrant", "Johnson", "Parker")
d.all$metro[d.all$place_county_name %in% DFWmetro_counties & d.all$place_state_name == "Texas"] <- "DFW Metro"

orlandometro_counties <- c("Lake", "Seminole", "Orange", "Osceola")
d.all$metro[d.all$place_county_name %in% orlandometro_counties & d.all$place_state_name == "Florida"] <- "Orlando Metro"

tampametro_counties <- c("Hillsborough", "Pinellas", "Pasco", "Hernando")
d.all$metro[d.all$place_county_name %in% tampametro_counties & d.all$place_state_name == "Florida"] <- "Tampa Metro"

miamimetro_counties <- c("Miami-Dade", "Broward", "Palm Beach")
d.all$metro[d.all$place_county_name %in% miamimetro_counties & d.all$place_state_name == "Florida"] <- "Miami Metro"

# then subset out for the three CA cities - don't use state column
# use the metro column

CA_metros <- c("San Francisco Metro", "LA Metro", "San Diego Metro")
d.CA <- d.all[d.all$metro %in% CA_metros,]

TX_metros <- c("Austin Metro", "San Antonio Metro", "Houston Metro", "DFW Metro")
d.TX <- d.all[d.all$metro %in% TX_metros,]

FL_metros <- c("Orlando Metro", "Tampa Metro", "Miami Metro")
d.FL <- d.all[d.all$metro %in% FL_metros,]

table(d.all$metro)
# We still need to create state-specific datasets - see example for d.CA


# Plotting boxplots for each state
d.state <- d.all

d.state <- d.state %>% group_by(year, place_state_name)%>% filter(n()>150)

d.summary.state <- d.state %>%
  group_by(place_state_name, year, tropical) %>%
  summarize(count=n()) %>%
  mutate(proportion=count/sum(count))

d.summary.state <- subset(d.summary.state, d.summary.state$tropical == "yes")

ggplot(data=d.summary.state, aes(place_state_name, proportion))+
  geom_boxplot()


# Plotting boxplots for each metro area in California
d.CA.box <- d.CA

d.CA.box <- d.CA.box %>% group_by(year, metro)%>% filter(n()>150)

d.CA.box.summary <- d.CA.box %>%
  group_by(metro, year, tropical) %>%
  summarize(count=n()) %>%
  mutate(proportion=count/sum(count))

d.CA.box.summary <- subset(d.CA.box.summary, d.CA.box.summary$tropical == "yes")

ggplot(data=d.CA.box.summary, aes(metro, proportion))+
  geom_boxplot()

# Plotting boxplots for each metro area in Texas
d.TX.box <- d.TX

d.TX.box <- d.TX.box %>% group_by(year, metro)%>% filter(n()>150)

d.TX.box.summary <- d.TX.box %>%
  group_by(metro, year, tropical) %>%
  summarize(count=n()) %>%
  mutate(proportion=count/sum(count))

d.TX.box.summary <- subset(d.TX.box.summary, d.TX.box.summary$tropical == "yes")

ggplot(data=d.TX.box.summary, aes(metro, proportion))+
  geom_boxplot()

# Plotting boxplots for each metro area in Florida
d.FL.box <- d.FL

d.FL.box <- d.FL.box %>% group_by(year, metro)%>% filter(n()>150)

d.FL.box.summary <- d.FL.box %>%
  group_by(metro, year, tropical) %>%
  summarize(count=n()) %>%
  mutate(proportion=count/sum(count))

d.FL.box.summary <- subset(d.FL.box.summary, d.FL.box.summary$tropical == "yes")

ggplot(data=d.FL.box.summary, aes(metro, proportion))+
  geom_boxplot()

# exploratory data visualization of d.all
nrow(d.all)

hist(d.all$year)

table(d.all$place_county_name)

table(d.all$scientific_name)

table(d.all$place_state_name)

table(d.all$place_state_name, d.all$year)


##### Subsetting Metro Area Data ######
# pick counties for each metro area and save a new file with just those counties

#CALIFORNIA
#BAY AREA
# subset out all counties in the Bay Area
d.SF.metro <- subset(d.all, d.all$metro == "San Francisco Metro")

table(d.SF.metro$place_county_name)

table(d.SF.metro$scientific_name)
# reformatting milkweed species names to be ready for milkweed proportion graph
d.SF.metro$scientific_name[d.SF.metro$scientific_name == "Asclepias californica greenei"]<- "Asclepias californica"
#did cutoff for other as 50
SF.other <- c("Asclepias eriocarpa", "Asclepias incarnata", "Asclepias solanoana", "Asclepias syriaca", "Asclepias tuberosa" )
d.SF.metro$scientific_name[d.SF.metro$scientific_name %in% SF.other]<- "Other"

#GREATER LA AREA
d.LA.metro <- subset(d.all, d.all$metro == "LA Metro")

table(d.LA.metro$place_county_name, d.LA.metro$place_state_name)

table(d.LA.metro$scientific_name)
# reformatting milkweed species names to be ready for milkweed proportion graph
d.LA.metro$scientific_name[d.LA.metro$scientific_name == "Asclepias albicans × subulata"]<- "Asclepias albicans"
d.LA.metro$scientific_name[d.LA.metro$scientific_name == "Asclepias californica californica"]<- "Asclepias californica"
#did cutoff for other as 100
LA.other <- c("Asclepias asperula", "Asclepias asperula asperula", "Asclepias incarnata", "Asclepias nyctaginifolia", "Asclepias speciosa", "Asclepias tuberosa", "Asclepias vestita")
d.LA.metro$scientific_name[d.LA.metro$scientific_name %in% LA.other]<- "Other"

#SAN DIEGO 
# NOTE: has 2435 observations so I think we can treat it as its own
d.SD.county <- subset(d.all, d.all$place_county_name == "San Diego" & d.all$place_state_name == "California")
table(d.SD.county$place_county_name)

table(d.SD.county$scientific_name)
# reformatting milkweed species names to be ready for milkweed proportion graph
d.SD.county$scientific_name[d.SD.county$scientific_name == "Asclepias californica californica"]<- "Asclepias californica"
#did cutoff for other as 50
#SD.other <- c("Asclepias erosa", "Asclepias speciosa")
#d.SD.county$scientific_name[d.SD.county$scientific_name %in% SD.other]<- "Other"


#TEXAS
#GREATER AUSTIN AREA
d.Austin.metro <- subset(d.all, d.all$metro == "Austin Metro")
table(d.Austin.metro$place_county_name)

table(d.Austin.metro$scientific_name)
# reformatting milkweed species names to be ready for milkweed proportion graph
d.Austin.metro$scientific_name[d.Austin.metro$scientific_name == "Asclepias tuberosa interior"]<- "Asclepias tuberosa"
d.Austin.metro$scientific_name[d.Austin.metro$scientific_name == "Asclepias asperula capricornu"]<- "Asclepias asperula"
#did cutoff for other as 100
# we only have 17 tropical milkweed observations overall
Austin.other <- c("Asclepias amplexicaulis", "Asclepias arenaria", "Asclepias linearis", "Asclepias syriaca")
d.Austin.metro$scientific_name[d.Austin.metro$scientific_name %in% Austin.other]<- "Other"


#GREATER SAN ANTONIO AREA
d.SanAntonio.metro <- subset(d.all, d.all$metro == "San Antonio Metro")
table(d.SanAntonio.metro$place_county_name)

table(d.SanAntonio.metro$scientific_name)
# reformatting milkweed species names to be ready for milkweed proportion graph
d.SanAntonio.metro$scientific_name[d.SanAntonio.metro$scientific_name == "Asclepias asperula capricornu"]<- "Asclepias asperula"

#did cutoff for other as 50
SanAntonio.other <- c("Asclepias emoryi", "Asclepias engelmanniana", "Asclepias linearis", "Asclepias tuberosa", "Asclepias tuberosa interior", "Asclepias tuberosa tuberosa", "Asclepias verticillata")
d.SanAntonio.metro$scientific_name[d.SanAntonio.metro$scientific_name %in% SanAntonio.other]<- "Other"

#GREATER HOUSTON AREA
d.Houston.metro <- subset(d.all, d.all$metro == "Houston Metro")
table(d.Houston.metro$place_county_name)

table(d.Houston.metro$scientific_name)
# reformatting milkweed species names to be ready for milkweed proportion graph
d.Houston.metro$scientific_name[d.Houston.metro$scientific_name == "Asclepias tuberosa interior"]<- "Asclepias tuberosa"
#did cutoff for other as 100
#we only have 86 tropical milkweed overall
Houston.other <- c("Asclepias amplexicaulis", "Asclepias asperula capricornu", "Asclepias lanceolata", "Asclepias obovata", "Asclepias syriaca", "Asclepias variegata", "Asclepias viridiflora")
d.Houston.metro$scientific_name[d.Houston.metro$scientific_name %in% Houston.other]<- "Other"

#DALLAS/FORT WORTH
d.DFW.metro <- subset(d.all, d.all$metro == "DFW Metro")
table(d.DFW.metro$place_county_name)

table(d.DFW.metro$scientific_name)
# reformatting milkweed species names to be ready for milkweed proportion graph
#SHOULD I CALL THIS ASPERULA OR VIRIDIS
d.DFW.metro$scientific_name[d.DFW.metro$scientific_name == "Asclepias asperula × viridis"]<- "Asclepias asperula"
d.DFW.metro$scientific_name[d.DFW.metro$scientific_name == "Asclepias viridiflora × viridis"]<- "Asclepias viridiflora"
d.DFW.metro$scientific_name[d.DFW.metro$scientific_name == "Asclepias tuberosa interior"]<- "Asclepias tuberosa"
d.DFW.metro$scientific_name[d.DFW.metro$scientific_name == "Asclepias asperula capricornu"]<- "Asclepias asperula"
#did cutoff for other as 100
#we only have 9 tropical milkweed observations overall
DFW.other <- c("Asclepias amplexicaulis", "Asclepias engelmanniana", "Asclepias speciosa", "Asclepias syriaca", "Asclepias texana", "Asclepias linearis")
d.DFW.metro$scientific_name[d.DFW.metro$scientific_name %in% DFW.other]<- "Other"
table(d.DFW.metro$scientific_name)

#FLORIDA
#GREATER ORLANDO AREA
d.Orlando.metro <- subset(d.all, d.all$metro == "Orlando Metro")
table(d.Orlando.metro$place_county_name, d.Orlando.metro$place_state_name)

table(d.Orlando.metro$scientific_name)
# reformatting milkweed species names to be ready for milkweed proportion graph
d.Orlando.metro$scientific_name[d.Orlando.metro$scientific_name == "Asclepias tuberosa rolfsii"]<- "Asclepias tuberosa"
d.Orlando.metro$scientific_name[d.Orlando.metro$scientific_name == "Asclepias tuberosa tuberosa"]<- "Asclepias tuberosa"
d.Orlando.metro$scientific_name[d.Orlando.metro$scientific_name == "Asclepias incarnata incarnata"]<- "Asclepias incarnata"

#did cutoff for other as 100
Orlando.other <- c("Asclepias connivens", "Asclepias curtissii", "Asclepias longifolia", "Asclepias tomentosa", "Asclepias verticillata", "Asclepias viridis", "Asclepias incarnata", "Asclepias lanceolata")
d.Orlando.metro$scientific_name[d.Orlando.metro$scientific_name %in% Orlando.other]<- "Other"

#GREATER TAMPA AREA
d.Tampa.metro <- subset(d.all, d.all$metro == "Tampa Metro")
table(d.Tampa.metro$place_county_name)

table(d.Tampa.metro$scientific_name)
# reformatting milkweed species names to be ready for milkweed proportion graph
d.Tampa.metro$scientific_name[d.Tampa.metro$scientific_name == "Asclepias tuberosa rolfsii"]<- "Asclepias tuberosa"
d.Tampa.metro$scientific_name[d.Tampa.metro$scientific_name == "Asclepias tuberosa tuberosa"]<- "Asclepias tuberosa"
#did cutoff for other as 50
Tampa.other <- c("Asclepias amplexicaulis", "Asclepias connivens", "Asclepias curtissii", "Asclepias feayi", "Asclepias incarnata", "Asclepias lanceolata", "Asclepias longifolia", "Asclepias tomentosa", "Asclepias viridis")
d.Tampa.metro$scientific_name[d.Tampa.metro$scientific_name %in% Tampa.other]<- "Other"

#GREATER MIAMI AREA
d.Miami.metro <- subset(d.all, d.all$metro == "Miami Metro")
table(d.Miami.metro$place_county_name)

table(d.Miami.metro$scientific_name)
# reformatting milkweed species names to be ready for milkweed proportion graph
d.Miami.metro$scientific_name[d.Miami.metro$scientific_name == "Asclepias tuberosa rolfsii"]<- "Asclepias tuberosa"
#did cutoff for other as 50
Miami.other <- c("Asclepias", "Asclepias curtissii", "Asclepias incarnata incarnata", "Asclepias longifolia", "Asclepias pedicellata", "Asclepias verticillata", "Asclepias viridis")
d.Miami.metro$scientific_name[d.Miami.metro$scientific_name %in% Miami.other]<- "Other"



### looking at the proportion of observations in each area that were currasavica

##CALIFORNIA
#BAY AREA
# checking how many observations for each year
d.SF.metro %>%
  group_by(year) %>%
  summarize(count=n())

# removing repeated observations by the same observer in one day
table(table(d.SF.metro$user_login, d.SF.metro$observed_on))

table(table(d.SF.metro$user_login, d.SF.metro$year))

# only keeping the first of each date/species/username combination
d.SF.metro <- distinct(d.SF.metro, user_login,observed_on,scientific_name, .keep_all= TRUE)

# same as above but only allowing one obs/month
d.SF.metro <- distinct(d.SF.metro, user_login,month,year,scientific_name, .keep_all= TRUE)

# same but only allowing one obs/year
#d.SF.metro <- distinct(d.SF.metro, user_login,year,scientific_name, .keep_all= TRUE)

# we've decided to try a cutoff at 150 observations
d.SF.metro <- d.SF.metro %>% group_by(year)%>% filter(n()>150)

# make summary table
d.SF.summary.all <- d.SF.metro %>%
  group_by(year, scientific_name) %>%
  summarize(count=n()) %>%
  mutate(proportion=count/sum(count))

# subset currasavica data only out of summary table
d.SF.summary <- subset(d.SF.summary.all, d.SF.summary.all$scientific_name == "Asclepias curassavica")

#GREATER LA AREA
# checking how many observations for each year
d.LA.metro %>%
  group_by(year) %>%
  summarize(count=n())

# removing repeated observations by the same observer in one day
table(table(d.LA.metro$user_login, d.LA.metro$observed_on))

table(table(d.LA.metro$user_login, d.LA.metro$year))
# only keeping the first of each date/species/username combination
d.LA.metro <- distinct(d.LA.metro, user_login,observed_on,scientific_name, .keep_all= TRUE)

# same as above but only allowing one obs/month
d.LA.metro <- distinct(d.LA.metro, user_login,month,year,scientific_name, .keep_all= TRUE)

# same but only allowing one obs/year
#d.LA.metro <- distinct(d.LA.metro, user_login,year,scientific_name, .keep_all= TRUE)

# we've decided to try a cutoff at 150 observations
d.LA.metro <- d.LA.metro %>% group_by(year)%>% filter(n()>150)

#Make summary table
d.LA.summary.all <- d.LA.metro %>%
  group_by(year, scientific_name) %>%
  summarize(count=n()) %>%
  mutate(proportion=count/sum(count))
# New change as of 10/27 - saving this table with all species as d.LA.summary.all, while d.LA.summary only has currasavica observations

#subset currasavica data only out of summary table
d.LA.summary <- subset(d.LA.summary.all, d.LA.summary.all$scientific_name == "Asclepias curassavica")

#Trying to look at things by month
d.LA.month.summary <- d.LA.metro %>%
  group_by(month, scientific_name) %>%
  summarize(count=n()) %>%
  mutate(proportion=count/sum(count))
# subset currasavica data only out of summary table
d.LA.month.summary <- subset(d.LA.month.summary, d.LA.month.summary$scientific_name == "Asclepias curassavica")

#SAN DIEGO 
# checking how many observations for each year
d.SD.county %>%
  group_by(year) %>%
  summarize(count=n())

# only keeping the first of each date/species/username combination
d.SD.county <- distinct(d.SD.county, user_login,observed_on,scientific_name, .keep_all= TRUE)

# same as above but only allowing one obs/month
d.SD.county <- distinct(d.SD.county, user_login,month,year,scientific_name, .keep_all= TRUE)

# we've decided to try a cutoff at 150 observations
d.SD.county <- d.SD.county %>% group_by(year)%>% filter(n()>150)

# removing repeated observations by the same observer in one day
table(table(d.SD.county$user_login, d.SD.county$observed_on))
# only keeping the first of each date/species/username combination
d.SD.county <- distinct(d.SD.county, user_login,observed_on,scientific_name, .keep_all= TRUE)

#Make summary table
d.SD.summary.all <- d.SD.county %>%
  group_by(year, scientific_name) %>%
  summarize(count=n()) %>%
  mutate(proportion=count/sum(count))
# subset currasavica data only out of summary table
d.SD.summary <- subset(d.SD.summary.all, d.SD.summary.all$scientific_name == "Asclepias curassavica")


#TEXAS
#GREATER AUSTIN AREA
# checking how many observations for each year
d.Austin.metro %>%
  group_by(year) %>%
  summarize(count=n())

# only keeping the first of each date/species/username combination
d.Austin.metro <- distinct(d.Austin.metro, user_login,observed_on,scientific_name, .keep_all= TRUE)

# same as above but only allowing one obs/month
d.Austin.metro <- distinct(d.Austin.metro, user_login,month,year,scientific_name, .keep_all= TRUE)

# we've decided to try a cutoff at 150 observations
d.Austin.metro <- d.Austin.metro %>% group_by(year)%>% filter(n()>150)

# removing repeated observations by the same observer in one day
table(table(d.Austin.metro$user_login, d.Austin.metro$observed_on))
# only keeping the first of each date/species/username combination
d.Austin.metro <- distinct(d.Austin.metro, user_login,observed_on,scientific_name, .keep_all= TRUE)

#Make summary table
d.Austin.summary.all <- d.Austin.metro %>%
  group_by(year, scientific_name) %>%
  summarize(count=n()) %>%
  mutate(proportion=count/sum(count))
# subset currasavica data only out of summary table
d.Austin.summary <- subset(d.Austin.summary.all, d.Austin.summary.all$scientific_name == "Asclepias curassavica")

#GREATER SAN ANTONIO AREA
# checking how many observations for each year
d.SanAntonio.metro %>%
  group_by(year) %>%
  summarize(count=n())

# only keeping the first of each date/species/username combination
d.SanAntonio.metro <- distinct(d.SanAntonio.metro, user_login,observed_on,scientific_name, .keep_all= TRUE)

# same as above but only allowing one obs/month
d.SanAntonio.metro <- distinct(d.SanAntonio.metro, user_login,month,year,scientific_name, .keep_all= TRUE)

# we've decided to try a cutoff at 150 observations
d.SanAntonio.metro <- d.SanAntonio.metro %>% group_by(year)%>% filter(n()>150)

# removing repeated observations by the same observer in one day
table(table(d.SanAntonio.metro$user_login, d.SanAntonio.metro$observed_on))
# only keeping the first of each date/species/username combination
d.SanAntonio.metro <- distinct(d.SanAntonio.metro, user_login,observed_on,scientific_name, .keep_all= TRUE)

#Make summary table
d.SanAntonio.summary.all <- d.SanAntonio.metro %>%
  group_by(year, scientific_name) %>%
  summarize(count=n()) %>%
  mutate(proportion=count/sum(count))
# subset currasavica data only out of summary table
d.SanAntonio.summary <- subset(d.SanAntonio.summary.all, d.SanAntonio.summary.all$scientific_name == "Asclepias curassavica")

#GREATER HOUSTON AREA
# checking how many observations for each year
d.Houston.metro %>%
  group_by(year) %>%
  summarize(count=n())

# only keeping the first of each date/species/username combination
d.Houston.metro <- distinct(d.Houston.metro, user_login,observed_on,scientific_name, .keep_all= TRUE)

# same as above but only allowing one obs/month
d.Houston.metro <- distinct(d.Houston.metro, user_login,month,year,scientific_name, .keep_all= TRUE)

# we've decided to try a cutoff at 150 observations
d.Houston.metro <- d.Houston.metro %>% group_by(year)%>% filter(n()>150)

# removing repeated observations by the same observer in one day
table(table(d.Houston.metro$user_login, d.Houston.metro$observed_on))
# only keeping the first of each date/species/username combination
d.Houston.metro <- distinct(d.Houston.metro, user_login,observed_on,scientific_name, .keep_all= TRUE)

#Make summary table
d.Houston.summary.all <- d.Houston.metro %>%
  group_by(year, scientific_name) %>%
  summarize(count=n()) %>%
  mutate(proportion=count/sum(count))
# subset currasavica data only out of summary table
d.Houston.summary <- subset(d.Houston.summary.all, d.Houston.summary.all$scientific_name == "Asclepias curassavica")

#DALLAS/FORT WORTH
# checking how many observations for each year
d.DFW.metro %>%
  group_by(year) %>%
  summarize(count=n())

# only keeping the first of each date/species/username combination
d.DFW.metro <- distinct(d.DFW.metro, user_login,observed_on,scientific_name, .keep_all= TRUE)

# same as above but only allowing one obs/month
d.DFW.metro <- distinct(d.DFW.metro, user_login,month,year,scientific_name, .keep_all= TRUE)


# removing repeated observations by the same observer in one day
table(table(d.DFW.metro$user_login, d.DFW.metro$observed_on))

table(table(d.DFW.metro$user_login, d.DFW.metro$year))

# we've decided to try a cutoff at 150 observations
d.DFW.metro <- d.DFW.metro %>% group_by(year)%>% filter(n()>150)

#Make summary table
d.DFW.summary.all <- d.DFW.metro %>%
  group_by(year, scientific_name) %>%
  summarize(count=n()) %>%
  mutate(proportion=count/sum(count))
# subset currasavica data only out of summary table
d.DFW.summary <- subset(d.DFW.summary.all, d.DFW.summary.all$scientific_name == "Asclepias curassavica")


#FLORIDA
#GREATER ORLANDO AREA
# checking how many observations for each year
d.Orlando.metro %>%
  group_by(year) %>%
  summarize(count=n())

# only keeping the first of each date/species/username combination
d.Orlando.metro <- distinct(d.Orlando.metro, user_login,observed_on,scientific_name, .keep_all= TRUE)

# same as above but only allowing one obs/month
d.Orlando.metro <- distinct(d.Orlando.metro, user_login,month,year,scientific_name, .keep_all= TRUE)

# we've decided to try a cutoff at 150 observations
d.Orlando.metro <- d.Orlando.metro %>% group_by(year)%>% filter(n()>150)

# removing repeated observations by the same observer in one day
table(table(d.Orlando.metro$user_login, d.Orlando.metro$observed_on))
# only keeping the first of each date/species/username combination
d.Orlando.metro <- distinct(d.Orlando.metro, user_login,observed_on,scientific_name, .keep_all= TRUE)

#Make summary table
d.Orlando.summary.all <- d.Orlando.metro %>%
  group_by(year, scientific_name) %>%
  summarize(count=n()) %>%
  mutate(proportion=count/sum(count))
# subset currasavica data only out of summary table
d.Orlando.summary <- subset(d.Orlando.summary.all, d.Orlando.summary.all$scientific_name == "Asclepias curassavica")

#GREATER TAMPA AREA
# checking how many observations for each year
d.Tampa.metro %>%
  group_by(year) %>%
  summarize(count=n())

# only keeping the first of each date/species/username combination
d.Tampa.metro <- distinct(d.Tampa.metro, user_login,observed_on,scientific_name, .keep_all= TRUE)

# same as above but only allowing one obs/month
d.Tampa.metro <- distinct(d.Tampa.metro, user_login,month,year,scientific_name, .keep_all= TRUE)

# we've decided to try a cutoff at 150 observations
d.Tampa.metro <- d.Tampa.metro %>% group_by(year)%>% filter(n()>150)

# removing repeated observations by the same observer in one day
table(table(d.Tampa.metro$user_login, d.Tampa.metro$observed_on))
# only keeping the first of each date/species/username combination
d.Tampa.metro <- distinct(d.Tampa.metro, user_login,observed_on,scientific_name, .keep_all= TRUE)

#Make summary table
d.Tampa.summary.all <- d.Tampa.metro %>%
  group_by(year, scientific_name) %>%
  summarize(count=n()) %>%
  mutate(proportion=count/sum(count))
# subset currasavica data only out of summary table
d.Tampa.summary <- subset(d.Tampa.summary.all, d.Tampa.summary.all$scientific_name == "Asclepias curassavica")


#GREATER MIAMI AREA
# checking how many observations for each year
d.Miami.metro %>%
  group_by(year) %>%
  summarize(count=n())

# only keeping the first of each date/species/username combination
d.Miami.metro <- distinct(d.Miami.metro, user_login,observed_on,scientific_name, .keep_all= TRUE)

# same as above but only allowing one obs/month
d.Miami.metro <- distinct(d.Miami.metro, user_login,month,year,scientific_name, .keep_all= TRUE)

# we've decided to try a cutoff at 60 observations
d.Miami.metro <- d.Miami.metro %>% group_by(year)%>% filter(n()>60)

# removing repeated observations by the same observer in one day
table(table(d.Miami.metro$user_login, d.Miami.metro$observed_on))
# only keeping the first of each date/species/username combination
d.Miami.metro <- distinct(d.Miami.metro, user_login,observed_on,scientific_name, .keep_all= TRUE)

#Make summary table
d.Miami.summary.all <- d.Miami.metro %>%
  group_by(year, scientific_name) %>%
  summarize(count=n()) %>%
  mutate(proportion=count/sum(count))
# subset currasavica data only out of summary table
d.Miami.summary <- subset(d.Miami.summary.all, d.Miami.summary.all$scientific_name == "Asclepias curassavica")


### Visualization of County Trends Overtime ####
##CALIFORNIA
#BAY AREA
# making a quick graph of SF bay area data
d.SF.summary$year <- as.factor(d.SF.summary$year)
ggplot(d.SF.summary, aes(year, proportion, group=scientific_name, color=scientific_name))+
  geom_point()+
  geom_line() +
  ggtitle("SF Metro Area")+
  #ylim(0,1)+
  scale_color_manual(values = c("#FF0000"))

#GREATER LA AREA
d.LA.summary$year <- as.factor(d.LA.summary$year)
ggplot(d.LA.summary, aes(year, proportion, group=scientific_name, color=scientific_name))+
  geom_point()+
  geom_line()+
  ggtitle("LA Metro Area")+
  #ylim(0,1)+
  scale_color_manual(values = c("#FF0000"))

#Monthly - but it only sorts by month in general not year
d.LA.month.summary$month <- as.factor(d.LA.month.summary$month)
ggplot(d.LA.month.summary, aes(month, proportion, group=scientific_name, color=scientific_name))+
  geom_point()+
  geom_line()+
  ggtitle("LA Metro Area")+
  #ylim(0,1)+
  scale_color_manual(values = c("#FF0000"))

#SAN DIEGO 
d.SD.summary$year <- as.factor(d.SD.summary$year)
ggplot(d.SD.summary, aes(year, proportion, group=scientific_name, color=scientific_name))+
  geom_point()+
  geom_line()+
  ggtitle("San Diego County")+
  #ylim(0,1) +
  scale_color_manual(values = c("#FF0000"))

#TEXAS
#GREATER AUSTIN AREA
d.Austin.summary$year <- as.factor(d.Austin.summary$year)
ggplot(d.Austin.summary, aes(year, proportion, group=scientific_name, color=scientific_name))+
  geom_point()+
  geom_line()+
  ggtitle("Austin Metro Area")+
  #ylim(0,1)+
  scale_color_manual(values = c("#FF0000"))

#GREATER SAN ANTONIO AREA
d.SanAntonio.summary$year <- as.factor(d.SanAntonio.summary$year)
ggplot(d.SanAntonio.summary, aes(year, proportion, group=scientific_name, color=scientific_name))+
  geom_point()+
  geom_line()+
  ggtitle("San Antonio Metro Area")+
  #ylim(0,1) +
  scale_color_manual(values = c("#FF0000"))

#GREATER HOUSTON AREA
d.Houston.summary$year <- as.factor(d.Houston.summary$year)
ggplot(d.Houston.summary, aes(year, proportion, group=scientific_name, color=scientific_name))+
  geom_point()+
  geom_line()+
  ggtitle("Houston Metro Area")+
  #ylim(0,1)+
  scale_color_manual(values = c("#FF0000"))

#DALLAS/FORT WORTH
d.DFW.summary$year <- as.factor(d.DFW.summary$year)
ggplot(d.DFW.summary, aes(year, proportion, group=scientific_name, color=scientific_name))+
  geom_point()+
  geom_line()+
  ggtitle("Dallas/Fort Worth Metro Area") +
  scale_color_manual(values = c("#FF0000"))

#FLORIDA
#GREATER ORLANDO AREA
d.Orlando.summary$year <- as.factor(d.Orlando.summary$year)
ggplot(d.Orlando.summary, aes(year, proportion, group=scientific_name, color=scientific_name))+
  geom_point()+
  geom_line()+
  ggtitle("Orlando Metro Area")+
  #ylim(0,1) +
  scale_color_manual(values = c("#FF0000"))

#GREATER TAMPA AREA
d.Tampa.summary$year <- as.factor(d.Tampa.summary$year)
ggplot(d.Tampa.summary, aes(year, proportion, group=scientific_name, color=scientific_name))+
  geom_point()+
  geom_line()+
  ggtitle("Tampa Metro Area")+
  #ylim(0,1) +
  scale_color_manual(values = c("#FF0000"))

#GREATER MIAMI AREA
d.Miami.summary$year <- as.factor(d.Miami.summary$year)
ggplot(d.Miami.summary, aes(year, proportion, group=scientific_name, color=scientific_name))+
  geom_point()+
  geom_line()+
  ggtitle("Miami Metro Area")+
  #ylim(0,1) +
  scale_color_manual(values = c("#FF0000"))


####Species Visualizations####
#BAY AREA
table(d.SF.metro$year)

# graphing stacked bar chart
# for more info on r bar plots 
#https://r-graph-gallery.com/48-grouped-barplot-with-ggplot2
#ggplot(d.SF.summary.all, aes(fill=scientific_name, y=proportion, x=year)) + 
  #geom_bar(position="fill", stat="identity") +
  #ggtitle("SF Metro Area")

#graphing with lines for each species
ggplot(data=d.SF.summary.all, aes(x=year, y=proportion, group=scientific_name, color=scientific_name)) +
  geom_point()+
  geom_line() +
  ggtitle("SF Metro Area") +
  scale_color_manual(values = c("#ADFF2F", "#32CD32", "#FF0000", "#2E8B57", "#006400", "#9ACD32", "#66CDAA", "#008080"))

#LA METRO
table(d.LA.metro$year)

# graphing stacked bar chart
# for more info on r bar plots 
#https://r-graph-gallery.com/48-grouped-barplot-with-ggplot2
#ggplot(d.LA.summary.all, aes(fill=scientific_name, y=proportion, x=year)) + 
  #geom_bar(position="fill", stat="identity") +
  #ggtitle("LA Metro Area")


#graphing with lines for each species
ggplot(data=d.LA.summary.all, aes(x=year, y=proportion, group=scientific_name, color=scientific_name)) +
  geom_point()+
  geom_line() +
  ggtitle("LA Metro Area")+
  scale_color_manual(values = c("#ADFF2F", "#32CD32", "#FF0000", "#2E8B57", "#006400", "#9ACD32", "#66CDAA", "#008080",  "#8FBC8B", "#5F9EA0"))


#SAN DIEGO COUNTY
table(d.SD.county$year)
# it looks like we want to start visualizing at 2016, the first year with > 100 obs
d.SD.summary.all <- subset(d.SD.summary.all, d.SD.summary.all$year >= 2016)

# graphing stacked bar chart
# for more info on r bar plots 
#https://r-graph-gallery.com/48-grouped-barplot-with-ggplot2
#ggplot(d.SD.summary.all, aes(fill=scientific_name, y=proportion, x=year)) + 
  #geom_bar(position="fill", stat="identity") +
  #ggtitle("SD County")

#graphing with lines for each species
ggplot(data=d.SD.summary.all, aes(x=year, y=proportion, group=scientific_name, color=scientific_name)) +
  geom_point()+
  geom_line() +
  ggtitle("SD Metro Area") +
  scale_color_manual(values = c("#ADFF2F", "#32CD32", "#FF0000", "#2E8B57", "#006400", "#9ACD32", "#66CDAA"))

#AUSTIN METRO
table(d.Austin.metro$year)
# it looks like we want to start visualizing at 2016, the first year with > 100 obs
d.Austin.summary.all <- subset(d.Austin.summary.all, d.Austin.summary.all$year >= 2016)

# graphing stacked bar chart
# for more info on r bar plots 
#https://r-graph-gallery.com/48-grouped-barplot-with-ggplot2
#ggplot(d.Austin.summary.all, aes(fill=scientific_name, y=proportion, x=year)) + 
  #geom_bar(position="fill", stat="identity") +
  #ggtitle("Austin Metro Area")

#graphing with lines for each species
ggplot(data=d.Austin.summary.all, aes(x=year, y=proportion, group=scientific_name, color=scientific_name)) +
  geom_point()+
  geom_line() +
  ggtitle("Austin Metro Area") +
  scale_color_manual(values = c("#ADFF2F", "#FF0000", "#32CD32", "#2E8B57", "#006400", "#9ACD32", "#66CDAA", "#008080", "#8FBC8B", "#5F9EA0"))

#SAN ANTONIO
table(d.SanAntonio.metro$year)
# it looks like we want to start visualizing at 2016, the first year with > 100 obs
d.SanAntonio.summary.all <- subset(d.SanAntonio.summary.all, d.SanAntonio.summary.all$year >= 2016)

# graphing stacked bar chart
# for more info on r bar plots 
#https://r-graph-gallery.com/48-grouped-barplot-with-ggplot2
#ggplot(d.SanAntonio.summary.all, aes(fill=scientific_name, y=proportion, x=year)) + 
  #geom_bar(position="fill", stat="identity") +
  #ggtitle("San Antonio Metro Area")

#graphing with lines for each species
ggplot(data=d.SanAntonio.summary.all, aes(x=year, y=proportion, group=scientific_name, color=scientific_name)) +
  geom_point()+
  geom_line() +
  ggtitle("San Antonio Metro Area") +
  scale_color_manual(values = c("#ADFF2F", "#FF0000", "#32CD32", "#2E8B57", "#006400", "#9ACD32", "#66CDAA", "#008080"))

#HOUSTON METRO
table(d.Houston.metro$year)
# it looks like we want to start visualizing at 2016, the first year with > 100 obs
d.Houston.summary.all <- subset(d.Houston.summary.all, d.Houston.summary.all$year >= 2016)

# graphing stacked bar chart
# for more info on r bar plots 
#https://r-graph-gallery.com/48-grouped-barplot-with-ggplot2
#ggplot(d.Houston.summary.all, aes(fill=scientific_name, y=proportion, x=year)) + 
  #geom_bar(position="fill", stat="identity") +
  #ggtitle("Houston Metro Area")

#graphing with lines for each species
ggplot(data=d.Houston.summary.all, aes(x=year, y=proportion, group=scientific_name, color=scientific_name)) +
  geom_point()+
  geom_line() +
  ggtitle("Houston Metro Area") +
  scale_color_manual(values = c("#FF0000", "#ADFF2F", "#32CD32",  "#2E8B57", "#006400", "#9ACD32", "#66CDAA", "#008080", "#8FBC8B", "#5F9EA0", "#87CEEB", "#00FFFF"))

#DFW METRO
table(d.DFW.metro$year)
# it looks like we want to start visualizing at 2015, the first year with > 100 obs
d.DFW.summary.all <- subset(d.DFW.summary.all, d.DFW.summary.all$year >= 2015)

# graphing stacked bar chart
# for more info on r bar plots 
#https://r-graph-gallery.com/48-grouped-barplot-with-ggplot2
#ggplot(d.DFW.summary.all, aes(fill=scientific_name, y=proportion, x=year)) + 
  #geom_bar(position="fill", stat="identity") +
  #ggtitle("Dallas/Fort Worth Metro Area")

#graphing with lines for each species
ggplot(data=d.DFW.summary.all, aes(x=year, y=proportion, group=scientific_name, color=scientific_name)) +
  geom_point()+
  geom_line() +
  ggtitle("Dallas/Fort Worth Metro Area") +
  scale_color_manual(values = c( "#32CD32", "#FF0000", "#2E8B57", "#006400", "#9ACD32", "#66CDAA", "#008080", "#8FBC8B", "#5F9EA0", "#87CEEB", "#00FFFF", "#aaff00"))


#ORLANDO METRO
table(d.Orlando.metro$year)
# it looks like we want to start visualizing at 2016, the first year with > 100 obs
d.Orlando.summary.all <- subset(d.Orlando.summary.all, d.Orlando.summary.all$year >= 2019)

# graphing stacked bar chart
# for more info on r bar plots 
#https://r-graph-gallery.com/48-grouped-barplot-with-ggplot2
#ggplot(d.Orlando.summary.all, aes(fill=scientific_name, y=proportion, x=year)) + 
  #geom_bar(position="fill", stat="identity") +
  #ggtitle("Orlando Metro Area")

#graphing with lines for each species
ggplot(data=d.Orlando.summary.all, aes(x=year, y=proportion, group=scientific_name, color=scientific_name)) +
  geom_point()+
  geom_line() +
  ggtitle("Orlando Metro") +
  scale_color_manual(values = c("#FF0000", "#2E8B57", "#006400", "#9ACD32", "#66CDAA", "#008080", "#8FBC8B", "#5F9EA0", "#87CEEB", "#00FFFF"))


#TAMPA METRO
table(d.Tampa.metro$year)
# it looks like we want to start visualizing at 2016, the first year with > 100 obs
d.Tampa.summary.all <- subset(d.Tampa.summary.all, d.Tampa.summary.all$year >= 2016)
d.Tampa.summary.all$year <- factor(d.Tampa.summary.all$year)
# graphing stacked bar chart
# for more info on r bar plots 
#https://r-graph-gallery.com/48-grouped-barplot-with-ggplot2
#ggplot(d.Tampa.summary.all, aes(fill=scientific_name, y=proportion, x=year)) + 
  #geom_bar(position="fill", stat="identity") +
  #ggtitle("Tampa Metro Area")

#graphing with lines for each species
ggplot(data=d.Tampa.summary.all, aes(x=year, y=proportion, group=scientific_name, color=scientific_name)) +
  geom_point()+
  geom_line() +
  ggtitle("Tampa Metro") +
  scale_color_manual(values = c("#FF0000", "#2E8B57", "#006400", "#9ACD32", "#66CDAA", "#008080", "#8FBC8B", "#5F9EA0", "#87CEEB", "#00FFFF"))


#SPECIES BY SPECIES PROPORTIONS
table(d.Miami.metro$year)
# it looks like we want to start visualizing at 2016, the first year with > 100 obs
d.Miami.summary.all <- subset(d.Miami.summary.all, d.Miami.summary.all$year >= 2016)

# graphing stacked bar chart
# for more info on r bar plots 
#https://r-graph-gallery.com/48-grouped-barplot-with-ggplot2
#ggplot(d.Miami.summary.all, aes(fill=scientific_name, y=proportion, x=year)) + 
  #geom_bar(position="fill", stat="identity") +
  #ggtitle("Miami Metro Area")

#graphing with lines for each species
ggplot(data=d.Miami.summary.all, aes(x=year, y=proportion, group=scientific_name, color=scientific_name)) +
  geom_point()+
  geom_line() +
  ggtitle("Miami Metro Area") +
  scale_color_manual(values = c("#FF0000", "#2E8B57", "#006400", "#9ACD32"))


# for more info see
# https://r-graph-gallery.com/line-chart-several-groups-ggplot2.html

# Link with information about ggplot colors: https://www.datanovia.com/en/blog/ggplot-colors-best-tricks-you-will-love/


### adding notes of further things to add to the code

# graph multiple cities of data on one graph

# some observations are in gardens and some are in wild areas

#ask about statistics
#How can I make this more robust --> is there some form of an extension


### Statistics ####

# Start with proportion tests to see which states and which cities within a state have more tropical milkweed

# Example here: https://rpkgs.datanovia.com/rstatix/reference/prop_test.html

#### OVERALL STATE COMPARISONS ####
# making a table with proportion yes or no tropical milkweed in each state
state_prop <- table(d.all$place_state_name, d.all$tropical)
# testing signficance 
pairwise.prop.test(state_prop, p.adjust.method = "bonferroni")

#CALIFORNIA COMPARISONS
CA_prop <- table(d.all$place_state_name, d.all$tropical)
# For this test - we actually need to have a dataset of only the three CA metros, rather than starting with d.all. See Line 90 and example below:
CA_prop <- table(d.CA$metro, d.CA$tropical)
pairwise.prop.test(CA_prop, p.adjust.method = "bonferroni")
# all CA metros are significantly different from each other. 

#TEXAS COMPARISONS
TX_prop <- table(d.all$place_state_name, d.all$tropical)
# For this test - we actually need to have a dataset of only the three CA metros, rather than starting with d.all. See Line 90 and example below:
TX_prop <- table(d.TX$metro, d.TX$tropical)

pairwise.prop.test(TX_prop, p.adjust.method = "bonferroni")
# all CA metros are significantly different from each other

#FLORIDA COMPARISONS
FL_prop <- table(d.all$place_state_name, d.all$tropical)
# For this test - we actually need to have a dataset of only the three CA metros, rather than starting with d.all. See Line 90 and example below:
FL_prop <- table(d.FL$metro, d.FL$tropical)
pairwise.prop.test(FL_prop, p.adjust.method = "bonferroni")
# all CA metros are significantly different from each other


####CALIFORNIA STATISTICAL ANALYSIS####
#BAY AREA
d.SF.summary$year <- as.numeric(as.character(d.SF.summary$year))

#Pairwise Prop Test
# testing proportions across year within each metro area
SFmetro_prop <- table(d.SF.metro$year, d.SF.metro$tropical)
pairwise.prop.test(SFmetro_prop, p.adjust.method = "bonferroni")
# we don't see a clear difference over time in SF proportion tropical

#Linear Model
plot(d.SF.summary$year, d.SF.summary$proportion)
# running a linear model to see if there is a clear trend of increase or decrease over time 

summary(lm(data=d.SF.summary, proportion~year))

#LA METRO
#Pairwise Prop Test
# testing proportions across year within each metro area
LAmetro_prop <- table(d.LA.metro$year, d.LA.metro$tropical)
pairwise.prop.test(LAmetro_prop, p.adjust.method = "bonferroni")
# we don't see a clear difference over time in SF proportion tropical

#Linear Model
plot(d.LA.summary$year, d.LA.summary$proportion)
# running a linear model to see if there is a clear trend of increase or decrease over time 
d.LA.summary$year <- as.numeric(d.LA.summary$year)
summary(lm(data=d.LA.summary, proportion~year))

#SD METRO
#Pairwise Prop Test
# testing proportions across year within each metro area
SDmetro_prop <- table(d.SD.county$year, d.SD.county$tropical)
pairwise.prop.test(SDmetro_prop, p.adjust.method = "bonferroni")
# we don't see a clear difference over time in SF proportion tropical

#Linear Model
plot(d.SD.summary$year, d.SD.summary$proportion)
# running a linear model to see if there is a clear trend of increase or decrease over time 
d.SD.summary$year <- as.numeric(d.SD.summary$year)
summary(lm(data=d.SD.summary, proportion~year))

####TEXAS STATISTICAL ANALYSIS####
#AUSTIN METRO
#Pairwise Prop Test
# testing proportions across year within each metro area
Austinmetro_prop <- table(d.Austin.metro$year, d.Austin.metro$tropical)
pairwise.prop.test(Austinmetro_prop, p.adjust.method = "bonferroni")
# we don't see a clear difference over time in SF proportion tropical

#Linear Model
plot(d.Austin.summary$year, d.Austin.summary$proportion)
# running a linear model to see if there is a clear trend of increase or decrease over time 
d.Austin.summary$year <- as.numeric(d.Austin.summary$year)
summary(lm(data=d.Austin.summary, proportion~year))

#SAN ANTONIO METRO
#Pairwise Prop Test
# testing proportions across year within each metro area
SanAntoniometro_prop <- table(d.SanAntonio.metro$year, d.SanAntonio.metro$tropical)
pairwise.prop.test(SanAntoniometro_prop, p.adjust.method = "bonferroni")
# we don't see a clear difference over time in SF proportion tropical

#Linear Model
plot(d.SanAntonio.summary$year, d.SanAntonio.summary$proportion)
# running a linear model to see if there is a clear trend of increase or decrease over time 
d.SanAntonio.summary$year <- as.numeric(d.SanAntonio.summary$year)
summary(lm(data=d.SanAntonio.summary, proportion~year))

#HOUSTON METRO
#Pairwise Prop Test
# testing proportions across year within each metro area
Houstonmetro_prop <- table(d.Houston.metro$year, d.Houston.metro$tropical)
pairwise.prop.test(Houstonmetro_prop, p.adjust.method = "bonferroni")
# we don't see a clear difference over time in SF proportion tropical

#DFW METRO
#Pairwise Prop Test
# testing proportions across year within each metro area
DFWmetro_prop <- table(d.DFW.metro$year, d.DFW.metro$tropical)
pairwise.prop.test(DFWmetro_prop, p.adjust.method = "bonferroni")
# we don't see a clear difference over time in SF proportion tropical

#Linear Model
plot(d.DFW.summary$year, d.DFW.summary$proportion)
# running a linear model to see if there is a clear trend of increase or decrease over time 
d.DFW.summary$year <- as.numeric(d.DFW.summary$year)
summary(lm(data=d.DFW.summary, proportion~year))

####FLORIDA STATISTICAL ANALYSIS####
#ORLANDO METRO
#Pairwise Prop Test
# testing proportions across year within each metro area
Orlandometro_prop <- table(d.Orlando.metro$year, d.Orlando.metro$tropical)
pairwise.prop.test(Orlandometro_prop, p.adjust.method = "bonferroni")
# we don't see a clear difference over time in SF proportion tropical

#Linear Model
plot(d.Orlando.summary$year, d.Orlando.summary$proportion)
# running a linear model to see if there is a clear trend of increase or decrease over time 
d.Orlando.summary$year <- as.numeric(d.Orlando.summary$year)
summary(lm(data=d.Orlando.summary, proportion~year))

#TAMPA METRO
#Pairwise Prop Test
# testing proportions across year within each metro area
Tampametro_prop <- table(d.Tampa.metro$year, d.Tampa.metro$tropical)
pairwise.prop.test(Tampametro_prop, p.adjust.method = "bonferroni")
# we don't see a clear difference over time in SF proportion tropical

#Linear Model
plot(d.Tampa.summary$year, d.Tampa.summary$proportion)
# running a linear model to see if there is a clear trend of increase or decrease over time 
d.Tampa.summary$year <- as.numeric(d.Tampa.summary$year)
summary(lm(data=d.Tampa.summary, proportion~year))

#MIAMI METRO
#Pairwise Prop Test
# testing proportions across year within each metro area
Miamimetro_prop <- table(d.Miami.metro$year, d.Miami.metro$tropical)
pairwise.prop.test(Miamimetro_prop, p.adjust.method = "bonferroni")
# we don't see a clear difference over time in SF proportion tropical

#Linear Model
plot(d.Miami.summary$year, d.Miami.summary$proportion)
# running a linear model to see if there is a clear trend of increase or decrease over time 
d.Miami.summary$year <- as.numeric(d.Miami.summary$year)
summary(lm(data=d.Miami.summary, proportion~year))


d.DFW.metro.tropical = subset(d.DFW.metro, d.DFW.metro$scientific_name == "Asclepias curassavica")

