transactionUI <- function(id, tr,
                          choices) {
  ns <- NS(id)

  tags$tr(
    tags$td(class="col1", gsub("^.*?-", "", tr$date)),
    tags$td(tr$payee_name),
    tags$td(class="money", paste0("$ ", format(-tr$amount/1000, nsmall=2))),
    tags$td(
      selectInput(ns("category"), selectize = FALSE, 
             label = NULL,
             multiple = FALSE,
             selected = unlist(tr$category_name),
             choices = choices)
    )
  ) 
}

transactionServer <- function(id, budget, transaction, categories) {
  tr <- transaction; # get transaction into local scope.
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$category, {
        if(input$category != unlist(tr$category_name)) {
          ix <- which(tr == "NULL")
          if(length(ix)) {
            tr[ix] <- ""
          }
          id <-  categories %>% 
                  filter(name == input$category) %>%    
                  select("id") %>% 
                  flatten_chr()

          tr$category_id = id
          tr$approved <- TRUE
          tr <- tr %>% dplyr::select(-category_name, -flag_color)
          res <- put_transaction(budget, tr)
          if(!res) {
            warning("Failed to update transaction category.")
          }
        }
      })
    }
  )
}
