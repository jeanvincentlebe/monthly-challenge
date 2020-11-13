library(shiny)
library(shinydashboard)

library(ggplot2)
library(dplyr)
library(tidyr)
library(magrittr)

library(lubridate)
library(rdrop2)

## Drop Box path
filePath <- "/database.rdata"
token <- readRDS("droptoken.rds")
drop_download(filePath, overwrite = T, dtoken = token)
load(file = "database.rdata")

ui <- dashboardPage(skin = "purple", 
                    dashboardHeader(title = "Monthly Challenge", titleWidth = "100%"),
                    dashboardSidebar(disable = T),
                    dashboardBody(
                        tags$head(tags$style(type = "text/css", "a{color: #55509B;}")), #800080
                        tags$head(tags$style(".content-wrapper, .right-side {background-color: #D6DDFF;}")),
                        tabsetPanel(
                            tabPanel(title = "Challenge entry",
                                     # tags$br(), tags$br(),
                                     tags$h4("10 MINUTE KILLER AB WORKOUT", style = "color: #55509B"),
                                     tags$a(
                                         tags$img(src = "pic.jpg", width="70%", height="70%"),
                                         href = "https://youtu.be/O-NKhBarAcc"
                                     ),
                                     tags$br(), tags$br(),
                                     uiOutput("ChallengeTab")
                            ),
                            tabPanel(title = "Other days",
                                     uiOutput("OtherDays")
                            ),
                            tabPanel(title = "Admin",
                                     helpText(paste0("Server Time: ", now())),
                                     textInput("newName", "Enter new participant name"),
                                     actionButton("newEnter", "Validate"),
                                     # actionButton("reset", "Reset database"),
                                     downloadButton("downloadData", "Download csv table")
                            )
                        )
                    )
)

server <- function(input, output) {
    # Tab Challenge Entry
    output$ChallengeTab <- renderUI({
        fluidPage(
            selectInput("name", "Please select your name", choices = colnames(db[-1]) %>% sort, multiple = F, selected = ""),
            actionButton("Validate", "Challenge Done!"),
            tags$br(), tags$br(), tags$br(),
            plotOutput("indivPlot"),
            tags$br(),
            plotOutput("challengePlot", height = "200px"),
            tags$br(),
            flowLayout(
                dateInput("from", "Plots start date", value = today() %>% floor_date(unit = "month"), weekstart = 1),
                dateInput("to", "Plots end date",  value = (today() %>% ceiling_date(unit = "month") - 1), weekstart = 1)
            )
        )
    })
    
    observeEvent(input$Validate, {
        drop_download(filePath, overwrite = T, dtoken = token)
        load(file = "database.rdata")
        db[which(db$Date == today()), input$name] <- 1
        save(db, file = "database.rdata")
        drop_upload("database.rdata", dtoken = token)
        db <<- db
    })
    
    output$indivPlot <- renderPlot({
        input$Validate
        input$iValidate
        input$newEnter
        input$reset

        db2 <- db[db$Date >= input$from & db$Date <= input$to,] %>% 
            pivot_longer(2:ncol(db), names_to = "Name", values_to = "Done")
        db2$Name <- db2$Name %>% factor(levels = db2$Name %>% unique %>% sort %>% rev)
        db2$Done <- db2$Done %>% as.character
        db2$Done[db2$Done == "1"] <- "V"
        # db2$Done[db2$Done == "0"] <- NA
        db2 <- db2[db2$Done == "V",]
        ggplot(db2) + geom_label(aes(x = Date, y = Name, label = Done, colour = Done)) +
            scale_x_date(limits = c(input$from, input$to)) +
            scale_color_manual(values = c("green", "red"), guide = F) +
            geom_vline(xintercept = today(), colour = "#55509B") +
            labs(y = NULL)
    })
    
    output$challengePlot <- renderPlot({
        input$Validate
        input$iValidate
        input$newEnter
        input$reset
        db2 <- db[db$Date >= input$from & db$Date <= input$to,]
        db2$Total <- apply(db2[, -1], 1, sum)
        MaxLim <- max(db2$Total)
        db2$Total[db2$Total == 0] <- NA
        ggplot(db2) + geom_point(aes(x = Date, y = Total), colour = "#55509B", shape = 8) +
            scale_y_continuous(limits = c(1, MaxLim), breaks = c(1:MaxLim)) +
            scale_x_date(limits = c(input$from, input$to)) +
            theme(panel.grid.minor = element_blank())
    })
    
    # Tab Other Days
    output$OtherDays <- renderUI({
        fluidPage(
            selectInput("iName", "Please select your name", choices = colnames(db[-1]) %>% sort, multiple = F, selected = ""),
            dateInput("iDate", "Please select date to validate", weekstart = 1),
            actionButton("iValidate", "Challenge Done!")
        )
    })
    
    observeEvent(input$iValidate, {
        drop_download(filePath, overwrite = T, dtoken = token)
        load(file = "database.rdata")
        db[which(db$Date == input$iDate), input$iName] <- 1
        save(db, file = "database.rdata")
        drop_upload("database.rdata", dtoken = token)
        db <<- db
    })
    
    # Admin Tab
    observeEvent(input$newEnter, {
        drop_download(filePath, overwrite = T, dtoken = token)
        load(file = "database.rdata")
        if(!input$newName %in% colnames(db)){
            db <- db %>% mutate(New = 0)
            colnames(db)[which(colnames(db) == "New")] <- input$newName
        }
        save(db, file = "database.rdata")
        drop_upload("database.rdata", dtoken = token)
        db <<- db
    })
    
    output$downloadData <- downloadHandler(
        filename = "Challenge.csv",
        content = function(file) {
            drop_download(filePath, overwrite = T, dtoken = token)
            load(file = "database.rdata")
            write.csv2(db, file, row.names = FALSE)
        }
    )
    
    observeEvent(input$reset, {
        db <- tibble(Date = c("2020-10-24" %>% ymd, "2020-10-24" %>% ymd + seq(0:67)), 
                     `Ann'So` = 0,
                     `Bérénice` = 0,
                     `Bruno` = 0,
                     `Céline` = 0,
                     Felipe = 0,
                     Franciele = 0,
                     `Géraldine` = 0,
                     Greg = 0,
                     JLO = 0,
                     `Karen Mc` = 0,
                     Maxie = 0,
                     `Mélanie` = 0,
                     Patrick = 0,
                     Rose = 0,
                     Sarah = 0,
                     `Sarah Salvi` = 0,
                     Virg = 0
        )
        save(db, file = "database.rdata")
        drop_upload("database.rdata", dtoken = token)
        db <<- db
    })
}

shinyApp(ui = ui, server = server)
