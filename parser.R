#install.packages("rvest")
#install.packages("tidyverse")
install.packages("htmltools")
library(rvest, tidyverse, htmltools)

#Barbora

barbora_articles <- read_html("https://barbora.lv/")
# find list of categories
categories <- barbora_articles %>% html_elements('.b-categories-list') %>% html_elements("a") %>% html_text2()
link_to_categories <- barbora_articles %>% html_elements('.b-categories-list') %>% html_elements("a") %>% html_attr("href")

# for each category create a list of sub-category
main_category <- list()

for (category in 1:length(categories)) {
  main_category[[categories[category]]] <- read_html(paste("https://barbora.lv", link_to_categories[category], sep = "")) %>%
    html_elements(".b-single-category--child") %>% html_text2()
  
  
  #parse 
}

