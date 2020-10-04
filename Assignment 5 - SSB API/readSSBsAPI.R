rm(list=ls())

library(data.table)
library(lubridate)
library(tidyverse)
library(PxWebApiData)

county <- ApiData("http://data.ssb.no/api/v0/dataset/95274.json?lang=no",
                  getDataByGET = TRUE)

whole_country <- ApiData("http://data.ssb.no/api/v0/dataset/95276.json?lang=no",
                         getDataByGET = TRUE)

# use first list, rowbind both data
dframe <- bind_rows(county[[1]], whole_country[[1]])


# new names, could have used dplyr::rename()
names(dframe) <- c("region", "date", "variable", "value")

# split date
dframe <- dframe %>% separate(date, 
                              into = c("year", "month"), 
                              sep = "M")

# make a new proper date variable
dframe <- dframe %>%  mutate(date = ymd(paste(year, month, 1)))

dframe <- dframe %>% mutate(variable_rec = dplyr::recode(variable,
                                                      "Utleigde rom"="rentedrooms",
                                                      "Pris per rom (kr)"="roomprice",
                                                      "Kapasitetsutnytting av rom (prosent)"="roomcap",
                                                      "Kapasitetsutnytting av senger (prosent)"="bedcap",
                                                      "Losjiomsetning (1 000 kr)"="revenue",
                                                      "Losjiomsetning per tilgjengeleg rom (kr)"="revperroom",
                                                      "Losjiomsetning, hittil i Ã¥r (1 000 kr)"="revsofar",
                                                      "Losjiomsetning per tilgjengeleg rom, hittil i Ã¥r (kr)"="revroomsofar",
                                                      "Pris per rom hittil i Ã¥r (kr)"="roompricesofar",
                                                      "Kapasitetsutnytting av rom hittil i Ã¥r (prosent)"="roomcapsofar",
                                                      "Kapasitetsutnytting av senger, hittil i Ã¥r (prosent)"="bedcapsofar"))
# get all unique values for variable_rec
dframe %>% select(variable_rec) %>% unique()
with(dframe, table(variable, variable_rec))


# get all region names
dframe %>% select(region) %>% unique()

# recode region name
dframe <- dframe %>% mutate(region_rec = dplyr::recode(region,
                                                    "Heile landet"="whole_country",
                                                    "Viken"="viken",
                                                    "Innlandet"="innlandet",
                                                    "Vestfold og Telemark" = "vestfold_og_telemark",
                                                    "Agder" = "agder",
                                                    "Rogaland" = "rogaland",
                                                    "Vestland" = "vestland",
                                                    "Møre og Romsdal" = "more_og_romsdal",
                                                    "Trøndelag - Trööndelage" = "trondelag",
                                                    "Nordland" = "nordland",
                                                    "Troms og Finnmark - Romsa ja Finnmárku" = "troms_and_finnmark",
                                                    "Svalbard" = "svalbard"))

# plot room utilization by county in 2020
dframe %>% 
  filter(variable_rec == "roomcap") %>% 
  ggplot(aes(x=month, y=value, group=region_rec)) +
  geom_line(aes(color=region_rec)) + 
  labs(title = "Room Capacity Utilization by County in 2020",
       x = "Month", y = "Room Capacity Utilized (%)") +
  theme_minimal()