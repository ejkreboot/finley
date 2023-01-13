library(dotenv)
library(httr)
library(jsonlite)
library(shiny)

library(dotenv)
load_dot_env(file = ".env")

token <- Sys.getenv("YNAB_TOKEN")

get <- function(endpoint, last_knowledge=NULL) {
  url <- paste0("https://api.youneedabudget.com/v1/", endpoint)
  if(!is.null(last_knowledge)) {
    url <- paste0(url, "?last_knowledge_of_server=", last_knowledge)
  }
  r  <- GET(url,
            add_headers(Authorization = paste("Bearer", token)))
  content(r)
}

put <- function(endpoint, body) {
  url <- paste0("https://api.youneedabudget.com/v1/", endpoint)
  r  <- PUT(url,
            body = body,
            add_headers('Content-Type' = "application/json", 'Authorization' = paste("Bearer", token)))
  if(status_code(r) == 200) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

to_dataframe <- function(x) {
  json <- toJSON(x)
  dat <- fromJSON(json, flatten=TRUE)
  dat <- apply(dat, 1, unlist)
  as.data.frame(t(dat))
}


