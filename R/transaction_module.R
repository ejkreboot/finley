transactionUI <- function(id, tr,
                          choices) {
  ns <- NS(id)

  tags$tr(class = "transaction",
    tags$td(class="col1", gsub("^.*?-", "", tr$date)),
    tags$td(tr$payee_name),
    tags$td(class="money", paste0("$ ", format(-tr$amount, nsmall=2))),
    tags$td(
      selectInput(ns("category"), selectize = FALSE, 
             label = NULL,
             multiple = FALSE,
             selected = unlist(tr$category_name),
             choices = choices)
    ),
    tags$td(
      actionButton(ns("approve"), class = "approve_transaction", "Approve"),
    )
  ) 
}

transactionServer <- function(id, budget, transaction, categories) {
  tr <- transaction; # get transaction into local scope.
  approved <- FALSE
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$approve, {
        if(!approved) {
          id <-  categories %>% 
            filter(name == input$category) %>%    
            select("id")
          tr$category_id = id
          tr$approved <- TRUE
          tr$amount <- tr$amount * 1000
          tr <- tr %>% dplyr::select(-category_name, -flag_color, -subtransactions)
          res <- put_transaction(budget, tr)
          if(!res) {
            warning("Failed to update transaction category.")
          } else {
            approved <- TRUE
          }
        }
      })
      observeEvent(input$category, {
        if(input$category != unlist(tr$category_name)) {
          ix <- which(tr == "NULL")
          if(length(ix)) {
            tr[ix] <- ""
          }
          id <-  categories %>% 
                  filter(name == input$category) %>%    
                  select("id")

          tr$category_id = id
          tr$approved <- TRUE
          tr$amount <- tr$amount * 1000
          tr <- tr %>% dplyr::select(-category_name, -flag_color, -subtransactions)
          res <- put_transaction(budget, tr)
          if(!res) {
            warning("Failed to update transaction category.")
          }
        }
      })
    }
  )
}
