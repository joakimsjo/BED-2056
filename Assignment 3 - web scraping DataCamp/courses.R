library(rvest)
library(tibble)

# Fetch data
r <- read_html("https://www.datacamp.com/courses/tech:r")
python <- read_html("https://www.datacamp.com/courses/tech:python")


# Add method to extract nodes from data as text
get_nodes_from_html <- function(data, node_name) {
  return (data %>%
            html_nodes(node_name) %>%
            html_text())
}

# Parse text to DataFrame, add tech column
r_course_titles <- get_nodes_from_html(r, ".course-block__title") %>% tibble(course_name = .) %>% add_column(tech="r")
python_course_titles <- get_nodes_from_html(python, ".course-block__title") %>% tibble(course_name = .) %>% add_column(tech="python")

# Combine tables
courses_df <- rbind(r_course_titles, python_course_titles)

# Print head
head(courses_df)
