library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)

ui <- dashboardPage(
    
    dashboardHeader(title = "Anne-So's Monthly Challenge", titleWidth = "100%"),
    dashboardSidebar(disable = T),
    dashboardBody(
        tabsetPanel(
            tabPanel(title = "Challenge entry",
                     uiOutput("ChallengeTab")
                     ),
            tabPanel(title = "Admin",
                     textInput("newName", "Enter new participant name"),
                     actionButton("newEnter", "Validate"),
                     actionButton("reset", "Reset database")
                     )
        )
    )
)

server <- function(input, output) {
    output$ChallengeTab <- renderUI({
        input$newEnter
        load(file = "database.RData")
        fluidPage(
            selectInput("name", "Please select your name", choices = colnames(db[-1]), multiple = F),
            actionButton("validate", "Challenge Done!"),
            tags$br(), tags$br(), tags$br(),
            plotOutput("challengePlot")
        )
    })
    
    observeEvent(input$validate, {
        load(file = "database.RData")
        db[which(db$Date == today()), input$name] <- 1
        save(db, file = "database.RData")
    })
    
    observeEvent(input$newEnter, {
        load(file = "database.RData")
        db <- db %>% mutate(New = 0)
        colnames(db)[which(colnames(db) == "New")] <- input$newName
        save(db, file = "database.RData")
    })
    
    output$challengePlot <- renderPlot({
        input$validate
        input$reset
        load(file = "database.RData")
        db$Total <- apply(db[, -1], 1, sum) 
        ggplot(db) + geom_line(aes(x = Date, y = Total), colour = "blue") +
            scale_y_continuous(limits = c(0, 10), breaks = c(1:10)) +
            scale_x_date(limits = c("2020-10-01", "2020-10-31") %>% ymd)
    })
    
    observeEvent(input$reset, {
        db <- tibble(Date = c("2020-10-24" %>% ymd, "2020-10-24" %>% ymd + seq(0:67)), 
                     `Anne-So` = 0,
                     Felipe = 0,
                     `Bérénice` = 0)
        save(db, file = "database.RData")
    })
}

shinyApp(ui = ui, server = server)
