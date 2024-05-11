library(rvest)
library(xml2)

# Function to get a random Wikipedia article
get_random_wiki_article <- function() {
  url <- "https://en.wikipedia.org/wiki/Special:Random"
  response <- read_html(url)
  random_article_url <- response %>% html_node('link[rel="canonical"]') %>% html_attr('href')
  return(random_article_url)
}

# Function to extract information from the article page
get_article_info <- function(article_url) {
  info_url <- paste0(article_url, "?action=info")
  response <- read_html(info_url)
  
  page_info <- list()
  page_info[["Name"]] <- article_url
  
  # Extract size info
  size_info <- response %>%
    html_nodes(xpath = '//*[@id="mw-pageinfo-length"]/td[2]') %>%
    html_text()
  if (length(size_info) > 0) {
    page_info[["Size"]] <- size_info[1] %>%
      stringr::str_trim()
  } else {
    page_info[["Size"]] <- NA
  }
  
  # Extract date info
  date_info <- response %>%
    html_nodes(xpath = '//*[@id="mw-pageinfo-firsttime"]/td[2]/a') %>%
    html_text()
  if (length(date_info) > 0) {
    page_info[["Date"]] <- date_info[1] %>%
      stringr::str_trim()
  } else {
    page_info[["Date"]] <- NA
  }
  
  return(page_info)
}

# Main function
main <- function() {
  article_info_list <- list()
  
  for (i in 1:1000) {
    random_article_url <- get_random_wiki_article()
    article_info <- get_article_info(random_article_url)
    article_info_list[[i]] <- article_info
  }
  
  article_info_df <- do.call(rbind, lapply(article_info_list, as.data.frame))
  
  write.csv(article_info_df, "Info.csv", row.names = FALSE)
}

# Run the main function
main()