library(readr)
library(gdata)
library(purrr)

# read_data
# args:
#     start: vector:(int,int,int)
#     end: vector:(int,int,int)
#     col_names: vector:(string)
#     filename: string
read_cdc_file <- function(start, end, col_names, filename) {
  data <- read_fwf(paste("assignment\ 7/",filename, sep=""),
           fwf_positions(start = start,
                         end = end,
                         col_names = col_names
           )
  ) %>% data.frame
  
  return (data)
}

# write_data
# args:
#   data: frame
#   filename: string
write_extracted_data <- function(data, filename) {
    out_file <- file(paste("assignment\ 7/extracted-", filename, sep=""), 'w')
    write.fwf(data.frame(data), file=out_file)
    close(out_file)
}

col_start = c(475,13,504, 23)
col_end = c(475,14,507,23)
col_names = c("sex","birth_month", "birth_weight", "day_of_birth")

read_cdc_cols = partial(read_cdc_file, col_start, col_end, col_names)

filenames = c("Nat2017PublicUS.c20180516.r20180808.txt","Nat2018PublicUS.c20190509.r20190717.txt","Nat2019PublicUS.c20200506.r20200915.txt")
map(filenames, ~ read_cdc_cols(.x) %>% write_extracted_data(., .x))
