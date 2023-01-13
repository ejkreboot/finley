library(purrr)
library(uuid)

#' Get accounts and balances
#'
#' @param n Either a numeric index or name of budget.
#' @return a dataframe containig account details.
#' 
get_account_info <- function(budget, last_knowledge=NULL) {
  first <- first_of_the_month()
  
  id <- as.UUID(budget)
  if(is.na(id)) {
    id <-  get_budget_id(budget)
  }
  
  url <- paste0("budgets/", id, "/accounts")
  dat <- get(url, last_knowledge)
  dat <- to_dataframe(dat$data$accounts)
  dat <- apply(dat, 2, unlist)
  dat <- as.data.frame(dat)
  dat$balance <- as.numeric(dat$balance)
  dat$cleared_balance <- as.numeric(dat$cleared_balance)
  dat$uncleared_balance <- as.numeric(dat$uncleared_balance)
  dat
}
