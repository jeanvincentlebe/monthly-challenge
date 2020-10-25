library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(rdrop2)

## Drop Box path
filePath <- "/database.rdata"

ui <- dashboardPage(
    dashboardHeader(title = "Anne-So's Monthly Challenge", titleWidth = "100%"),
    dashboardSidebar(disable = T),
    dashboardBody(
        tabsetPanel(
            tabPanel(title = "Challenge entry",
                     uiOutput("ChallengeTab")
            ),
            tabPanel(title = "Other days",
                     uiOutput("OtherDays")
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
    # Tab Challenge Entry
    output$ChallengeTab <- renderUI({
        input$newEnter
        drop_download(filePath, overwrite = T)
        load(file = "database.rdata")
        fluidPage(
            selectInput("name", "Please select your name", choices = colnames(db[-1]), multiple = F, selected = ""),
            actionButton("validate", "Challenge Done!"),
            dateInput("from", "Plot start date", value = today() %>% floor_date(unit = "month")),
            dateInput("to", "Plot end date",  value = today() %>% ceiling_date(unit = "month")),
            plotOutput("indivPlot"),
            tags$br(),
            plotOutput("challengePlot")
        )
    })
    
    observeEvent(input$validate, {
        drop_download(filePath, overwrite = T)
        load(file = "database.rdata")
        db[which(db$Date == today()), input$name] <- 1
        save(db, file = "database.RData")
        drop_upload("database.rdata")
    })
    
    output$challengePlot <- renderPlot({
        input$validate
        input$iValidate
        input$reset
        drop_download(filePath, overwrite = T)
        load(file = "database.rdata")
        db$Total <- apply(db[, -1], 1, sum) 
        ggplot(db) + geom_line(aes(x = Date, y = Total), colour = "blue") +
            scale_y_continuous(limits = c(0, 10), breaks = c(1:10)) +
            scale_x_date(limits = c(input$from, input$to))
    })
    
    output$indivPlot <- renderPlot({
        input$validate
        input$iValidate
        input$reset
        drop_download(filePath, overwrite = T)
        load(file = "database.rdata")
        db2 <- db %>% pivot_longer(2:ncol(db), names_to = "Name", values_to = "Done")
        db2$Done <- db2$Done %>% as.character
        db2$Done[db2$Done == "1"] <- "V"
        db2$Done[db2$Done == "0"] <- "X"
        ggplot(db2) + geom_label(aes(x = Date, y = Name, label = Done, colour = Done)) +
            scale_x_date(limits = c(input$from, input$to)) +
            scale_color_manual(values = c("green", "red"), guide = F)
    })
    
    # Tab Other Days
    output$OtherDays <- renderUI({
        drop_download(filePath, overwrite = T)
        load(file = "database.rdata")
        fluidPage(
            dateInput("iDate", "Please select date to validate"),
            selectInput("iName", "Please select your name", choices = colnames(db[-1]), multiple = F, selected = ""),
            actionButton("iValidate", "Challenge Done!")
        )
    })
    
    observeEvent(input$iValidate, {
        drop_download(filePath, overwrite = T)
        load(file = "database.rdata")
        db[which(db$Date == input$iDate), input$iName] <- 1
        save(db, file = "database.RData")
        drop_upload("database.rdata")
    })
    
    # Admin Tab
    observeEvent(input$newEnter, {
        drop_download(filePath, overwrite = T)
        load(file = "database.rdata")
        if(!input$newName %in% colnames(db)){
            db <- db %>% mutate(New = 0)
            colnames(db)[which(colnames(db) == "New")] <- input$newName
        }
        save(db, file = "database.RData")
        drop_upload("database.rdata")
    })
    
    observeEvent(input$reset, {
        db <- tibble(Date = c("2020-10-24" %>% ymd, "2020-10-24" %>% ymd + seq(0:67)), 
                     `Anne-So` = 0,
                     Felipe = 0,
                     `Bérénice` = 0)
        save(db, file = "database.RData")
        drop_upload("database.rdata")
    })
}

shinyApp(ui = ui, server = server)
