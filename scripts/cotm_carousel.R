library(curl)
library(jsonlite)
library(dplyr)
library(stringr)
library(glue)

# read in cotm data
cotm_raw<-read.csv('data/cotm.csv')
cotm_filtered<-cotm_raw %>% 
    # filter unwanted (e.g. unpublished charts)
    filter(carousel_include == 1 & !is.na(vis_url)) %>%
    mutate(vis_url = str_remove(vis_url, 'https://www.ons.gov.uk'))

# use curl to avoid SSL issues
json_txt <- curl_fetch_memory(
  "https://raw.githubusercontent.com/ONSdigital/get-ons-vis-screenshots/main/screenshot-filenames.json"
)$content

json_data <- fromJSON(rawToChar(json_txt))

screenshot_data<-
    data.frame(
        vis_url = names(json_data),
        img_id = as.integer(unname(json_data))
    ) %>%
    mutate(img_url = paste0('https://raw.githubusercontent.com/ONSdigital/get-ons-vis-screenshots/main/screenshots/',
                            img_id,
                            ".png"))


carousel_data <- cotm_filtered %>%
  left_join(screenshot_data, by = c("vis_url")) %>%
  # filter out charts missing from screenshot tool for some reason
  filter(!is.na(img_url))

items <- glue::glue(
  ":::: {{.carousel-item image=\"{carousel_data$img_url}\" height=\"400px\" caption=\"{carousel_data$release_title}\"}}\n::::"
)

carousel_qmd <- paste0(
  "::::: {.carousel .dark .framed indicators=\"true\" controls=\"true\"}\n\n",
  paste(items, collapse = "\n\n"),
  "\n\n:::::"
)


writeLines(carousel_qmd, "_includes/carousel.qmd")
