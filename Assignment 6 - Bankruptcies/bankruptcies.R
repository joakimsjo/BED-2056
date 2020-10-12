library(rvest)
library(purrr)
library(tidyverse)
library(lubridate)

# Fetch data
generate_url <- function (from, to) {
  return  (sprintf("https://w2.brreg.no/kunngjoring/kombisok.jsp?datoFra=%s&datoTil=%s&id_region=0&id_niva1=51&id_niva2=-+-+-&id_bransje1=0", format(from, "%d.%m.%Y"), format(to, "%d.%m.%Y")   
 ))
}

# Add method to extract nodes from data
get_nodes_from_html <- function(data, node_name) {
  return (data %>%
            html_nodes(xpath = '//table') %>%
            html_nodes(node_name))
}

# create intervals for fetching data
start_date <- "01.01.2019"
end_date <- "01.10.2020"

intervals_from <- seq.Date(from=as.Date(start_date, format="%d.%m.%Y"), to=as.Date(end_date, format="%d.%m.%Y"), by="month" ) %>% as.list
intervals_to <- map(intervals_from[-1], function(x) { return(x - 1) })
intervals_from <- intervals_from[-length(intervals_from)]

# generate url's to fetch data from
urls <- map2(intervals_from, intervals_to, generate_url) 

# functions to select, format and clean data
brreg_select_table = function(data) {
  return (data %>%
    html_nodes(xpath = '//table') %>%
    html_nodes("table"))
}

brreg_table_to_df <- function(data) {
    temp <- html_table(data, trim=TRUE) %>%
    .[[1]] %>%
    .[-c(1,2,3,4,5),] %>%
    .[-c(1,3,5,7,9)]
    colnames(temp) <- c('company_name', 'org_nr','date', 'case')
    
    return (temp)
}

clean_brreg_table <- function(data) {
  return (data %>%
    mutate(county = ifelse(grepl("[A-Za-z]", org_nr), org_nr, NA)) %>%
    fill(county) %>%
    filter(nchar(org_nr)==11, case=='Konkursåpning', county != 'Utenlands') %>%
    mutate(date=dmy(date), 
         year = as.factor(year(date)), 
         month = month(date, label=TRUE))
  )
}

scrape_brreg_data = compose(clean_brreg_table, brreg_table_to_df,  brreg_select_table, read_html)

commencements_df <- map(urls, scrape_brreg_data) %>% do.call('rbind', .)

# create plot

plot <- function() {
  commencements_df %>%
  select(case, year, month, county) %>%
  group_by(county, year, month) %>%
  count(case) %>%
  group_by(county, year) %>%
  mutate(count= cumsum(n)) %>%
  ungroup() %>%
  ggplot(aes(x=month, y=count, group=year)) +
  geom_line(aes(color=year), size=1.5, group=1) +
  facet_wrap(~county) +
  ggtitle("Number of Bankruptcies in Norway per County in 2019 and 2020") +
  xlab("Month") + 
  ylab("Cumulative Number of Bankruptcies")
}
  

