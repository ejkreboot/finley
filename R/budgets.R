#' Get budget name by index or name
#'
#' @param n Either a numeric index or name of budget
#' @return id (uuid format) of matching budget or NULL if not found
#' 
get_budget_id <- function(n) {
  res <- get("budgets")$data$budgets
  id <- NULL
  if(is.numeric(n)) {
    tryCatch({
      id <- res[[n]]$id
    }, error = function(e) {
      id <- NULL
    })
  } else if(is.character(n)){
    for(b in res) {
      if(b$name == n)
        id = b$id
    }
  }
  return(id)
}

#' Get vector of budget names
#'
#' @return vector of names
#' 
get_budget_names <- function() {
  res <- get("budgets")
  names <- NULL
  for(b in res$data$budgets) {
    names = c(names, b$name)
  }
  names
}

