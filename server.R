library(dplyr)
library(shiny)
library(shinythemes)
library(ggplot2)
library(ggfx)
library(ggdark)
library(DT)
library(rnab) # remotes::install_github("https://github.com/ejkreboot/rnab")

library(dotenv)
load_dot_env(file = ".env")

BUDGET <- as.numeric(Sys.getenv("YNAB_BUDGET"))
GOAL <- as.numeric(Sys.getenv("YNAB_GOAL"))
REFRESH <- 1000 * 60 * 60 * 2 # 2 hours
options(scipen = 999)

finley_theme <- 
  dark_theme_bw() + 
  theme(axis.title = element_text(face="bold"), 
        plot.title = element_text(face="bold", size=12),
        panel.grid = element_line(color = "dodgerblue", size=0.1),
        axis.text.x = element_text(vjust = 0.5, hjust=0.5))


server <- function(input, output, session) {
    trans <- get_current_transactions(BUDGET)
    cats <- get_categories(BUDGET)
    options = c(cats$name, "Uncategorized")
    ix <- which(trans$category_name %in% c("Credit Card Payments", 
                                           "Internal Master Category", 
                                           "Inflow: Ready to Assign"))
    if(length(ix) > 0) {
      trans <- trans[-ix,]
    }
    ix <- which(trans$approved == FALSE)
    ts <- NULL
    if(length(ix) > 0) {
      trans <- trans[ix,]
      for(i in 1:nrow(trans)) {
        tr <- trans[i,]
        insertUI("#transactions_table", 
                 where = "beforeEnd", 
                 ui=transactionUI(tr$id, tr, options))
        ts <- c(ts, transactionServer(tr$id, BUDGET, tr, cats))
      }
    }

    observeEvent(input$import, {
      if(input$import > 0) {
        new_count = update_transactions(BUDGET);
        output$import_message <- renderUI({
          paste(new_count, "transaction(s) imported.")
        })
      }
    })
    
    output$accountsPlot <- renderPlot({
      invalidateLater(REFRESH, session)
      dat <- get_account_info(BUDGET)
      dat <- rbind(c("", "NET", "", "", "", "", sum(dat$balance), rep("", 4)), dat)
      dat$balance <- as.numeric(dat$balance) # ??
      
      ggplot(dat, aes(x=name, y=balance)) +
        with_outer_glow(geom_bar(stat="identity", 
                                 width=0.3, 
                                 fill="dodgerblue"),
                        colour="dodgerblue", sigma=10, expand=8) + 
        ggtitle("ACCOUNT BALANCES") +
        xlab("") + 
        ylab("Balance") + 
        finley_theme  
    })
    
    output$totalExpensesPlot <- renderPlot({
      invalidateLater(REFRESH, session)
      dat <- get_current_transactions(BUDGET);
      ix <- which(grepl("Inflow", dat$category_name))
      if(length(ix) > 0) {
        dat <- dat[-ix,]
      }
      total <- sum(-dat$amount)
      dat <- data.frame(class = c("Spent", "Budgeted"), amount=c(total, GOAL-total))
      ggplot(dat, aes(x="", y=amount, fill=class)) +
        with_blur(geom_bar(stat="identity", width=0.35), sigma = 20) + 
        geom_bar(stat="identity", width=0.3) +
        xlab("") + 
        ylab("Balance") + 
        ggplot2::scale_fill_manual(values=c("dodgerblue", "#ff2c00")) +
        ggtitle("SPENDING") +
        finley_theme      
    })
    
    output$categoriesTable <- renderDataTable({
      invalidateLater(REFRESH, session)
      dat <- get_categories(BUDGET)
      ix <- which(dat$category_name %in% c("Credit Card Payments", 
                                             "Internal Master Category", 
                                             "Inflow: Ready to Assign"))
      if(length(ix) > 0) {
        dat <- dat[-ix,]
      }
      dat <- dat[ , c(3,7,8,15)]
      colnames(dat) <- c("Category", "Allocated", "Spent", "Budget")
      dat$net <- (dat$Budget + dat$Spent) / dat$Budget
      
      d <- DT::datatable(dat, 
                         rownames = FALSE,
                         options = list( 
                           columnDefs = list(
                             list(targets = c(1:4), className = "dt-center"),
                             list(targets = c("net"), visible = FALSE)
                           ),
                           paging = TRUE,
                           searching = FALSE,
                           pageLength = 20
                         )
      )
      
      d %>% formatStyle(
        'Spent', 'net',
        color = styleInterval(c(-0.01, 0.1), c("#f75036", "#f7ba36", "dodgerblue"))
      ) %>% formatCurrency(columns = 2:5)
    })
}
