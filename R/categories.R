library(purrr)
library(uuid)
library(foreach)
library(dotenv)

load_dot_env(file = ".env")
CATEGORIES = NULL;

if(nchar(Sys.getenv("YNAB_CATEGORIES"))) {
  cats <- gsub(", ", ",", Sys.getenv("YNAB_CATEGORIES"))
  cats <- gsub("^ ", "", cats)
  CATEGORIES = strsplit(cats, ",")[[1]]
}

.flatten <- function(r) {
  r <- r %>% replace(.=="NULL", NA)
  if(is.list(r)) {
    return(.flatten(unlist(r)))
  }
  else {
    return(r)
  }
}

#' Get categories for specified budget
#'
#' @param budget the uuid, name, or index of the desired budget
#' @param last_knowledge optional "last knowledge" value to fetch only new info
#' @return vector of categories
#' 
get_categories <- function(budget, last_knowledge=NULL) {
  id <- as.UUID(budget)

  if(is.na(id)) {
    id <-  get_budget_id(budget)
  }
  
  groups <- get(paste0("budgets/", id, "/categories"), 
                last_knowledge)$data$category_groups
  
  categories <- foreach( g = groups,
                         .errorhandling = "remove", 
                         .combine = rbind ) %do% {
    r <- NULL
    if(length(g$categories)) {
      r <- map(g$categories, .flatten)
      r <- reduce(r, rbind)
    }
    if(is.null(r)) {
      stop()
    }
    r
  }
  group_info <- foreach(g = groups, .combine = rbind, .final = as.data.frame) %do% {
    r <- c(g$id, g$name)
    names(r) <- c("category_group_id", "category_name")
    r
  }
  categories <- as.data.frame(categories)
  r <- inner_join(categories, group_info, by=c("category_group_id"))
  if(!is.null(CATEGORIES)) {
    ix <- which(r$category_name %in% CATEGORIES)
    r <- r[ix,]
  }
  r  
}

#' Get category ids for specified budget
#' 
#' This is a convenience wrapper for get_categories
#'
#' @param budget the uuid, name, or index of the desired budget
#' @param last_knowledge optional "last knowledge" value to fetch only new info
#' @return vector of category ids
#' 
get_category_ids <- function(...) {
  cats <- get_categories(...);
  cats$id
}

#' Get category names for specified budget
#' 
#' This is a convenience wrapper for get_categories
#'
#' @param budget the uuid, name, or index of the desired budget
#' @param last_knowledge optional "last knowledge" value to fetch only new info
#' @return vector of category names
#' 
get_category_names <- function(...) {
  cats <- get_categories(...)
  cats$name
}

#' Get category balances for specified budget
#' 
#' This is a convenience wrapper for get_categories
#'
#' @param budget the uuid, name, or index of the desired budget
#' @param last_knowledge optional "last knowledge" value to fetch only new info
#' @return vector of activities
#' 
get_category_balances <- function(...) {
  cats <- get_categories(...);
  unlist(map(cats, function(x) { x$balance}))
}


#' Get category activities for specified budget
#' 
#' This is a convenience wrapper for get_categories
#'
#' @param budget the uuid, name, or index of the desired budget
#' @param last_knowledge optional "last knowledge" value to fetch only new info
#' @return vector of activities
#' 
get_category_activities <- function(...) {
  cats <- get_categories(...);
  acts <- unlist(map(cats, function(x) { x$activity})) / 1000
  names(acts) <- get_category_names(...)
  acts
}

#' Get category activities for specified budget
#' 
#' This is a convenience wrapper for get_categories
#'
#' @param budget the uuid, name, or index of the desired budget
#' @param last_knowledge optional "last knowledge" value to fetch only new info
#' @return vector of activities
#' 
get_category_goals <- function(...) {
  cats <- get_categories(...);
  goals <- unlist(map(cats, function(x) { x$goal_target})) / 1000
  names(goals) <- get_category_names(...)
  goals
}

  