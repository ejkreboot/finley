#
# convenience functions for working with dates and ISO format
#

current_date <- function() {
  Sys.Date();
}

day_of_the_month <- function() {
  as.numeric(format.Date(current_date(), format="%e"))
}

first_of_the_month <- function() {
  today <- day_of_the_month();
  current_date() - today + 1;
}
