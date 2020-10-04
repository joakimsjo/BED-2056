library(rvest)
library(tibble)
library(readr)
library(stringr)
library(purrr)

# Fetch data
timeplan_data <- read_html("http://timeplan.uit.no/emne_timeplan.php?sem=20h&module%5B%5D=BED-2056-1&View=list")

# Add method to extract nodes from data
get_nodes_from_html <- function(data, node_name) {
  return (data %>%
            html_nodes(node_name))
}


# Method to extract date and time from table row
extract_date_and_time_from_row <- function(row) {
    nodes <- html_nodes(row, "td")

    date <- nodes[[1]] %>% html_text()  %>% str_replace(., "Mandag", "") %>% parse_character()
    time <- nodes[[2]] %>% html_text()  %>% parse_character() %>% substr(., 1,5)
    
    return (paste(date, time, sep=" "))
}

rows <- get_nodes_from_html(timeplan_data, ".table-primary")
lecture_dates <- rows %>% map(extract_date_and_time_from_row) %>% map(parse_datetime, format="%d.%m.%Y  %H:%M")
df <- data.frame(date=I(lecture_dates))

head(df)
