library(dplyr)
library(shiny)
library(shinythemes)
library(ggplot2)
library(ggfx)
library(ggdark)
library(DT)

library(dotenv)
load_dot_env(file = ".env")
BUDGET <- as.numeric(Sys.getenv("YNAB_BUDGET"))
GOAL <- 7000
REFRESH <- 1000 * 60 * 60 * 2 # 2 hours
options(scipen = 999)

finley_theme <- 
  dark_theme_bw() + 
  theme(axis.title = element_text(face="bold"), 
        plot.title = element_text(face="bold", size=12),
        panel.grid = element_line(color = "dodgerblue", size=0.1),
        # axis.text.x = element_blank())
        axis.text.x = element_text(vjust = 0.5, hjust=0.5))

server <- function(input, output, session) {
  
    trans <- get_current_transactions(BUDGET)
    cats <- get_categories(BUDGET)
    options = c(cats$name, "Uncategorized")
    ix <- which(trans$category_name == "Inflow: Ready to Assign")
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

    output$accountsPlot <- renderPlot({
      invalidateLater(REFRESH, session)
      dat <- get_account_info(BUDGET);
      dat$balance <- dat$balance/1000
      dat <- rbind(c("", "NET", "", "", "", sum(dat$balance), rep("", 6)), dat)
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
      total <- sum(dat$amount)/1000
      dat <- data.frame(class = c("Spent", "Budgeted"), amount=c(total, GOAL))
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
      dat <- dat[ , c(3,7,8,12)]
      dat[ , 2] <- format(as.numeric(dat[ , 2]) / 1000, nsmall=2)
      dat[ , 3] <- format(as.numeric(dat[ , 3]) / 1000, nsmall=2)
      dat
    }, options = list(
      columnDefs = list(list(className = "dt-center", targets = 2:4)),
      paging = TRUE,
      searching = FALSE,
      pageLength = 20
    ))
}
