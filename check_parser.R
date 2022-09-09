







# library(readr)
library(magrittr)
setwd(stringr::str_sub(
  as.character(rstudioapi::getActiveDocumentContext()$path),
  end = -15
))
getwd()
library(tidyverse)
library(htmltools)
library(rvest)

# read receipts
check_file <-
  read_file("chek.txt") %>% {
    unlist(str_split(., "\r\n"))
  } %>% {
    .[!. %in% c("", " ")]
  }

#important stuff
address <-
  check_file[check_file %>% str_detect("Veikals \"Maxima\"") %>% which(T) +
               1]


date_and_time <-
  check_file[check_file %>% str_detect("LAIKS") %>% which(T)] %>%
  str_remove("LAIKS") %>% str_trim("both") %>%
  as.POSIXct(format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

check_id <-
  check_file[check_file %>% str_detect("[0-9999]{1,}\\/[0-99999]{1,}") %>% which(T)]

#convert check_file to data
filter_reg <- "\\s[0-999]{1,},[0-99]{1,}\\sX|Atlaide:"
receipt <-
  data.frame(name = check_file[seq(
    check_file %>% str_detect(check_id) %>% which(T) + 1,
    check_file %>% str_detect("===") %>% which(T) %>% {
      .[1]
    } - 1,
    by = 1
  )]) %>%
  mutate(name = ifelse(
    !str_detect(name, filter_reg) &
      !str_detect(lead(name), filter_reg),
    paste0(name, " ", lead(name)),
    name
  )) %>%
  filter(!lag(
    !str_detect(name, filter_reg) &
      !str_detect(lead(name), filter_reg),
    default = FALSE
  )) %>%
  mutate(name = ifelse(!is.na(str_extract(
    lead(name), regex('Atlaide:')
  )),
  paste0(name, " ", lead(name)), name)) %>%
  filter(!row_number() %in% (str_which(name, regex('^Atlaide:')))) %>%
  mutate(lagged = str_replace_all(lead(name), ",", ".")) %>%
  filter(row_number() %% 2 != 0) %>%
  rowwise() %>%   #!important
  mutate(category = str_extract(name, regex("^(?:(?!(?:\\S*[A-Z]){2}).)+")) %>% str_trim("both")) %>%
  mutate(
    gross_price = as.numeric(str_extract_all(
      lagged, pattern =  regex('[0-999]{1,}.[0-999]{1,}|[0-999]')
    ) %>% unlist() %>% .[1]),
    amount = as.numeric(str_extract_all(
      lagged, pattern =  regex('[0-999]{1,}.[0-999]{1,}|[0-999]')
    ) %>% unlist() %>% .[2]),
    #info = str_extract_all(lagged, pattern =  regex('gab|kg')) %>% unlist() %>% .[1],
    discount = as.numeric(str_extract(
      lagged, pattern =  regex('-[0-999]{1,}.[0-999]{1,}')
    ) %>% .[1])
  ) %>%
  mutate(net_price = gross_price * amount + ifelse(is.na(discount), 0, discount)) %>%
  select(-lagged) %>%
  ungroup() %>%
  group_by(category, name, gross_price) %>%
  summarise(
    amount = sum(amount),
    discount = sum(discount),
    net_price = sum(net_price)
  ) %>%
  ungroup() %>%
  select(category, name, gross_price, amount, discount, net_price) %>%
  ungroup() %>%
  mutate(date = date_and_time,
         address = address,
         check_id = check_id)

#convert category to link-wise string and return values from barbora.lv
parse_barbora <- receipt %>%
  mutate(filtered_name = str_to_lower(name) %>% str_replace_all(
    c(
      "ā" = "a",
      "č" = "c",
      "ē" = "e",
      "ģ" = "g",
      "ī" = "i",
      "ķ" = "k",
      "ļ" = "l",
      "ņ" = "n",
      "š" = "s",
      "ū" = "u",
      "ž" = "z",
      "\\." = "",
      "\\s" = "-",
      "\\," = "-",
      ".(?<=[0-999]{1}l\b)" = "X"
    )
  )) %>%
  mutate(
    filtered_name = str_replace_all(
      filtered_name,
      c(
        ".(?<=[0-999]{1}l\\b)" = "-l",
        ".(?<=[0-999]{1}g\\b)" = "-g"
      )
    ),
    category_barbora = NA,
    img_barbora = NA
  ) %>%
  select(name, filtered_name, category_barbora, img_barbora)

for (i in 1:nrow(parse_barbora)) {
  # parse categories
  category <- tryCatch({
    read_html(paste0(
      "https://barbora.lv/produkti/",
      parse_barbora$filtered_name[i]
    )) %>%
      html_elements(".breadcrumb") %>%
      html_text2() %>%
      str_remove("Mājas lapa\n")
    
    
  }, error = function(e) {
    return(NA)
  })
  # parse image
  parse_barbora$category_barbora[i] <- category
  rm(category)
}


# move all categories to receipt data
receipt <- bind_cols(receipt, parse_barbora[, 3]) %>%
  rowwise() %>%
  mutate(
    sub_cat1 = unlist(str_split(category_barbora, "\n"))[1] %>% {
      ifelse(is.na(.), category, .)
    },
    sub_cat2 = unlist(str_split(category_barbora, "\n"))[2] %>% {
      ifelse(is.na(.), category, .)
    },
    sub_cat3 = unlist(str_split(category_barbora, "\n"))[3] %>% {
      ifelse(is.na(.), category, .)
    }
  ) %>%
  select(-category_barbora)

# read existing data and store

if (file.exists("maxima.csv")) {
  total_data <- read.csv("maxima.csv", fileEncoding = "UTF-8") %>%
    mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  
  if (!check_id %in% unique(total_data$check_id)) {
    total_data %<>% bind_rows(receipt)
    
  } else {
    print("receipt has already been added")
  }
  
  
  write.csv(
    x = total_data,
    file = "maxima.csv",
    fileEncoding = "UTF-8",
    na = "",
    row.names = FALSE
  )
  
  #summary by category:
  total_data %>% group_by(category) %>%
    summarise(total_by_category = sum(net_price),
              amount = sum(amount)) %>% print(n = 10000)
  
} else {
  print("First time loading receipt, previous data is missing")
  
  write.csv(
    x = receipt,
    file = "maxima.csv",
    fileEncoding = "UTF-8",
    na = "",
    row.names = FALSE
  )
  
}