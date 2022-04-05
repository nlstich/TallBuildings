# load required libraries -------------------------------------------------

library(tidyverse)
library(rvest)

# Get building data -------------------------------------------------------

# list of urls to wikipedia pages containing detail and rankings of tallest
# buildings in each city. Note: this could be done with a series of loops and
# additional scraps by taking the list of most populated cities in each state or
# across the country and appending it to a base url. However, at this time and
# for ease of readability, each city is called individually.

nyc_url <- "https://en.wikipedia.org/wiki/List_of_tallest_buildings_in_New_York_City"
la_url <- "https://en.wikipedia.org/wiki/List_of_tallest_buildings_in_Los_Angeles"
chicago_url <- "https://en.wikipedia.org/wiki/List_of_tallest_buildings_in_Chicago"
houston_url <- "https://en.wikipedia.org/wiki/List_of_tallest_buildings_in_Houston"
philly_url <- "https://en.wikipedia.org/wiki/List_of_tallest_buildings_in_Philadelphia"
phoenix_url <- "https://en.wikipedia.org/wiki/List_of_tallest_buildings_in_Phoenix"
sanantonio_url <- "https://en.wikipedia.org/wiki/List_of_tallest_buildings_in_San_Antonio"
sandiego_url <- "https://en.wikipedia.org/wiki/List_of_tallest_buildings_in_San_Diego"
dallas_url <- "https://en.wikipedia.org/wiki/List_of_tallest_buildings_in_Dallas"
sanjose_url <- "https://en.wikipedia.org/wiki/List_of_tallest_buildings_in_San_Jose,_California"
sanfransisco_url <- "https://en.wikipedia.org/wiki/List_of_tallest_buildings_in_San_Francisco"
seattle_url <- "https://en.wikipedia.org/wiki/List_of_tallest_buildings_in_Seattle"
miami_url <- "https://en.wikipedia.org/wiki/List_of_tallest_buildings_in_Miami"
ohio_url <- "https://en.wikipedia.org/wiki/List_of_tallest_buildings_in_Ohio"


# this function scrapes each url to find the ranking table. Some pages have additional tables.
buildings_wiki <- function(url_in){
  
  # read url
  url <- read_html(url_in)
  
  # return list of tables on each page
  lists <- url %>% 
    html_nodes(".wikitable") %>% 
    html_table()
  
  # iterate through the list of tables to find the one with containing rankings
  for( item in lists){
    item_names <- colnames(item)
    if("Rank" %in% colnames(item)){
      look_here <- as.tibble(item)
      colnames(look_here) <- str_remove_all(colnames(look_here),"\\[.+")
      return(look_here)
    }
  }
}

# call function for each url. Note - you could combine and clean at this step,
# but if you needed to change either the scrape function (building_url) or the
# clean function (initial_data_cleaner) below, I believe having both in the same
# function would make it more difficult to update either function.

nyc_df <- buildings_wiki(nyc_url)
la_df <- buildings_wiki(la_url)
chicago_df <- buildings_wiki(chicago_url)
houston_df <- buildings_wiki(houston_url)
philly_df <- buildings_wiki(philly_url)
phoenix_df <- buildings_wiki(phoenix_url)
sanantonio_df <- buildings_wiki(sanantonio_url)

# drop urls as they are no longer needed
rm(list = grep("url",ls(),value = T))

# Clean data --------------------------------------------------------------

# this function is used in initial clean up of the scrape data

initial_data_cleaner <- function(df_in){
  # select columns
  new_df_out <- df_in %>% select(rank = 1, name = 2, height = 4, floors = 5, year = 6, address = 7, coordinates = 8) %>% 
    # trim citations
    mutate(floors = floors %>% str_remove_all("\\[.+")) %>% 
    mutate(height = height %>% str_remove_all("\\[.+")) %>% 
    
    # trim other coordinates column - drop punctuation and extra text
    mutate(coordinates = coordinates %>% str_remove_all("\\(.+")) %>% 
    mutate(coordinates = coordinates %>% gsub(".*}","",.)) %>% 
    
    # filter out rows with citations and other non-uniform data 
    filter(str_detect(year, '(1|2)\\d{3}')) %>% 
    filter(str_length(name) > 0) %>% 
    filter(str_length(coordinates) > 0) %>% 
    
    # separate height column into measurements in feet and meters
    separate(height, c("height_ft", "height_m"), sep = " ", remove = T) %>% 
    # remove any leftover punctuation in height columns
    mutate(height_m = height_m %>% str_remove_all("[:punct:]")) %>% 
    mutate(height_ft = height_ft %>% str_remove_all("[:punct:]")) %>% 
    
    # convert numeric columns to integers
    mutate_at(c("height_ft","height_m","floors","year"), function(x){x %>% as.integer()}) %>% 
    # add index column to preserve initial rank column and preserve sort order later
    mutate(city_index = row_number())
  
  # clean the first row of coordinates - could be done with a mutate where statement
  
  return(new_df_out)
}

nyc_buildings <- initial_data_cleaner(nyc_df)
