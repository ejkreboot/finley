library(purrr)
library(uuid)

#' Get current month's data
#'
#' @param budget the uuid, name, or index of the desired budget
#' @param last_knowledge optional "last knowledge" value to fetch only new info
#' @return vector of categories
#' 
get_current_month <- function(budget, last_knowledge=NULL) {
  id <- as.UUID(budget)
  categories <- NULL
  
  if(is.na(id)) {
    id <-  get_budget_id(budget)
  }
  dat <- get(paste0("budgets/", id, "/months/current"), 
                last_knowledge)$data$month
  return(dat)
}

#' Get current month's total income
#'
#' This is a convenience wrapper for get_current_month
#' @param budget the uuid, name, or index of the desired budget
#' @param last_knowledge optional "last knowledge" value to fetch only new info
#' @return vector of categories
#' 
get_current_month_income<- function(...) {
  return(get_current_month(...)$income)
}

#' Get current month's total activity
#'
#' This is a convenience wrapper for get_current_month
#' @param budget the uuid, name, or index of the desired budget
#' @param last_knowledge optional "last knowledge" value to fetch only new info
#' @return vector of categories
#' 
get_current_month_activity<- function(...) {
  return(get_current_month(...)$activity)
}

#' Get current month's activities by category
#'
#' This is a convenience wrapper for get_current_month
#' @param budget the uuid, name, or index of the desired budget
#' @param last_knowledge optional "last knowledge" value to fetch only new info
#' @return vector of categories
#' 
get_current_month_activities<- function(...) {
  dat <- get_current_month(...)$categories;
  activities <- map(dat, function(x) { c(x$activity, x$budgeted) })
  activities <- sapply(activities, rbind)
  colnames(activities) <- unlist(map(dat, function(x) { x$name}))
  return(activities)
}