library(purrr)
library(uuid)
library(jsonlite)

EXCLUDE_CATEGORIES <- c(
  "Starting Balance"
)

#' Get transactions for current month
#'
#' @param budget the uuid, name, or index of the desired budget
#' @param last_knowledge optional "last knowledge" value to fetch only new info
#' @return vector of categories
#' 
get_current_transactions <- function(budget, last_knowledge=NULL) {
  first <- first_of_the_month()
  
  id <- as.UUID(budget)
  categories <- NULL
  
  if(is.na(id)) {
    id <-  get_budget_id(budget)
  }
  
  url <- paste0("budgets/", id, "/transactions?since_date=", first)
  if(!is.null(last_knowledge)) {
    url <- paste0(url, "&last_knowledge_of_server=", last_knowledge)
  }
  dat <- get(url)$data$transactions
  names <- names(dat[[1]])
  dat <- as.data.frame(t(sapply(dat, rbind)))
  colnames(dat) <- names
  dat$amount <- as.numeric(dat$amount)
  return(dat)
}

#' Put a transaction (to modify)
#'
#' @param budget the uuid, name, or index of the desired budget
#' @param transaction the transaction data
#' @return http response
#' 
put_transaction <- function(budget, transaction) {
  id <- as.UUID(budget)
  
  if(is.na(id)) {
    id <-  get_budget_id(budget)
  }

  url <- paste0("budgets/", id, "/transactions/", transaction$id)
  put(url, toJSON(list(transaction = sapply(transaction, unlist)), 
                  auto_unbox = TRUE))
}
