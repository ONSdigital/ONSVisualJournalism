library(curl)
library(jsonlite)
library(dplyr)
library(stringr)
library(glue)
library(tidyr)
library(lubridate)

# clean_dates function - to tidy the dates from articles-and-dvcs.json

clean_dates <- function(x) {
  # Extract the first date-like pattern
  date_str <- str_extract(
    x,
    "\\d{4}-\\d{2}-\\d{2}|\\d{2}/\\d{2}/\\d{4}"
  )
  
  # Parse multiple possible formats
  parsed_date <- parse_date_time(
    date_str,
    orders = c("ymd", "dmy")
  )
  
  parsed_date
}



# read in cotm data

cotm_raw<-read.csv('data/cotm.csv')


# reformat the dates
cotm_raw$cotm_month <- format(
  my(cotm_raw$cotm_month),
  "%B %Y"
)


cotm_filtered<-cotm_raw %>% 
  # filter unwanted (e.g. unpublished charts)
  filter(carousel_include == 1 & !is.na(vis_url)) %>%
  mutate(vis_url_short = str_remove(vis_url, 'https://www.ons.gov.uk')) %>%
  select(-vis_url, -local_url)

rm(cotm_raw)


# get the filenames of the screenshots
# use curl to avoid SSL issues
json_txt <- curl_fetch_memory(
  "https://raw.githubusercontent.com/ONSdigital/get-ons-vis-screenshots/main/screenshot-filenames.json"
)$content

json_data <- fromJSON(rawToChar(json_txt))

screenshot_data<-
  data.frame(
    vis_url_short = names(json_data),
    img_id = as.integer(unname(json_data))
  ) %>%
  mutate(img_url = paste0('https://raw.githubusercontent.com/ONSdigital/get-ons-vis-screenshots/main/screenshots/',
                          img_id,
                          ".png"))

rm(json_data, json_txt)



# get articles and dvcs data - to use release name, date and URL

url <- "https://raw.githubusercontent.com/ONSdigital/get-ons-vis-screenshots/main/articles-and-dvcs.json"


tmp <- tempfile(fileext = ".json")
curl_download(url, tmp)

data <- fromJSON(tmp, flatten = TRUE)


df_long <- unnest_longer(
  data,
  vis_urls,
  keep_empty = TRUE
) %>%
  mutate(parsed_date = clean_dates(release_date)) %>%
  select(-release_date)



releases_w_vis <- df_long %>%
  filter(
    !is.na(vis_urls),
    grepl("index\\.html$", vis_urls)
  ) %>%
  rename(release_date = parsed_date)

rm(data, df_long)

# join the three together

carousel_data_1 <- cotm_filtered %>%
  left_join(screenshot_data, by = c("vis_url_short")) %>%
  # filter out charts missing from screenshot tool for some reason
  filter(!is.na(img_url)) 


carousel_data <- carousel_data_1 %>%
  left_join(
    releases_w_vis %>%
      select(vis_urls, release_date, title, doc_uri),
    by = c("vis_url_short" = "vis_urls")
  ) %>%
  mutate(caption = glue("{cotm_month} - {title} - {release_date}"))

write.csv(carousel_data, "data/cotm_carousel_data.csv")

rm(carousel_data_1)

items <- glue::glue(
  ":::: {{.carousel-item image=\"{carousel_data$img_url}\" height=\"400px\" caption=\"{carousel_data$caption}\"}}\n::::"
)

carousel_qmd <- paste0(
  "::::: {.carousel .dark .framed indicators=\"true\" controls=\"true\"}\n\n",
  paste(items, collapse = "\n\n"),
  "\n\n:::::"
)

writeLines(carousel_qmd, "_includes/carousel.qmd")
