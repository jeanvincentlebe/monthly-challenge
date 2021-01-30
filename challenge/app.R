library(shiny)
library(shinydashboard)

library(ggplot2)
library(dplyr)
library(tidyr)
library(magrittr)

library(lubridate)
library(rdrop2)

## Drop Box path
# db and database.rdata are 2020 database
# db21 and database21.rdata are 2021 database
filePath <- "/database.rdata"
filePath21 <- "/database21.rdata"

token <- readRDS("droptoken.rds")
drop_download(filePath21, overwrite = T, dtoken = token)
drop_download(filePath, overwrite = T, dtoken = token)

load(file = "database21.rdata")
load(file = "database.rdata")

ui <- dashboardPage(
    dashboardHeader(title = "Monthly Challenge", titleWidth = "100%"),
    dashboardSidebar(disable = T),
    dashboardBody(
        tabsetPanel(
            tabPanel(title = "Current Month",
                     uiOutput("ChallengeTab")
            ),
            tabPanel(title = "Other days",
                     uiOutput("OtherDays")
            ),
            tabPanel(title = "Admin",
                     helpText(paste0("Server Time: ", now())),
                     textInput("newName", "Enter new participant name"),
                     actionButton("newEnter", "Validate"),
                     downloadButton("downloadData", "Download csv table")
            ),
            
            tabPanel(title = "Past Months",
                     uiOutput("PastTab")
            )
        )
    )
)

server <- function(input, output) {
    # Tab Challenge Entry
    ## ------ Active tab -------------------------------------------------------------------------------
    output$ChallengeTab <- renderUI({
        fluidPage(
            selectInput("name", "Please select your name", choices = colnames(db21[-1]) %>% sort, multiple = F, selected = ""),
            actionButton("Validate", "Challenge Done!"),
            tags$br(), tags$br(), tags$br(),
            plotOutput("indivPlot"),
            tags$br(),
            plotOutput("challengePlot", height = "200px"),
            tags$br()
        )
    })
    
    observeEvent(input$Validate, {
        drop_download(filePath21, overwrite = T, dtoken = token)
        load(file = "database21.rdata")
        db21[which(db21$Date == today()), input$name] <- 1
        save(db21, file = "database21.rdata")
        drop_upload("database21.rdata", dtoken = token)
        db21 <<- db21
    })
    
    output$indivPlot <- renderPlot({
        input$Validate
        input$iValidate
        input$newEnter
        
        from <- floor_date(today(), "month")
        to <- ceiling_date(today(), "month") - 1
        db2 <- db21[db21$Date >= from & db21$Date <= to,] %>% 
            pivot_longer(2:ncol(db21), names_to = "Name", values_to = "Done")
        db2$Name <- db2$Name %>% factor(levels = db2$Name %>% unique %>% sort %>% rev)
        db2$Done <- db2$Done %>% as.character
        db2$Done[db2$Done == "1"] <- "V"
        db2 <- db2[db2$Done == "V",]
        ggplot(db2) + geom_label(aes(x = Date, y = Name, label = Done, colour = Done)) +
            scale_x_date(limits = c(from, to)) +
            scale_color_manual(values = c("green", "red"), guide = F) +
            geom_vline(xintercept = today(), colour = "blue") +
            labs(y = NULL)
    })
    
    output$challengePlot <- renderPlot({
        input$Validate
        input$iValidate
        input$newEnter
        
        from <- floor_date(today(), "month")
        to <- ceiling_date(today(), "month") - 1
        db2 <- db21[db21$Date >= from & db21$Date <= to,]
        db2$Total <- apply(db2[, -1], 1, sum)
        MaxLim <- ifelse(length(db2$Total) > 0, max(db2$Total), 0)
        db2$Total[db2$Total == 0] <- NA
        ggplot(db2) + geom_point(aes(x = Date, y = Total), colour = "blue", shape = 8) +
            scale_y_continuous(limits = c(1, MaxLim), breaks = c(1:MaxLim)) +
            scale_x_date(limits = c(from, to)) +
            theme(panel.grid.minor = element_blank())
    })
    
    # Tab Other Days --------------------------------------------------------------------------------
    output$OtherDays <- renderUI({
        fluidPage(
            selectInput("iName", "Please select your name", choices = colnames(db21[-1]) %>% sort, multiple = F, selected = ""),
            dateInput("iDate", "Please select date to validate", weekstart = 1),
            actionButton("iValidate", "Challenge Done!")
        )
    })
    
    observeEvent(input$iValidate, {
        drop_download(filePath21, overwrite = T, dtoken = token)
        load(file = "database21.rdata")
        db21[which(db21$Date == input$iDate), input$iName] <- 1
        save(db21, file = "database21.rdata")
        drop_upload("database21.rdata", dtoken = token)
        db21 <<- db21
    })
    
    # Admin Tab ------------------------------------------------------------------------------------
    observeEvent(input$newEnter, {
        drop_download(filePath21, overwrite = T, dtoken = token)
        load(file = "database21.rdata")
        if(!input$newName %in% colnames(db21)){
            db21 <- db21 %>% mutate(New = 0)
            colnames(db21)[which(colnames(db21) == "New")] <- input$newName
        }
        save(db21, file = "database21.rdata")
        drop_upload("database21.rdata", dtoken = token)
        db21 <<- db21
    })
    
    output$downloadData <- downloadHandler(
        filename = "Challenge.csv",
        content = function(file) {
            drop_download(filePath21, overwrite = T, dtoken = token)
            load(file = "database21.rdata")
            write.csv2(bind_rows(db, db21), file, row.names = FALSE)
        }
    )
    
    # Past Months tab ----------------------------------------------------------------------------
    output$PastTab <- renderUI({
        fluidPage(
            selectInput("past_m", "Select past month to display", 
                        choices = c("Nov 20", "Dec 20", "Jan 21"), 
                        selected = "Nov 20"),
            plotOutput("PastIndivPlot"),
            tags$br(),
            plotOutput("PastChallengePlot", height = "200px")
        )
    })
    
    output$PastIndivPlot <- renderPlot({
        if(input$past_m == "Nov 20") per <- c("20201101" %>% ymd(), "20201130" %>% ymd())
        if(input$past_m == "Dec 20") per <- c("20201201" %>% ymd(), "20201231" %>% ymd())
        if(input$past_m == "Jan 21") per <- c("20210101" %>% ymd(), "20210131" %>% ymd())
        
        db2 <- db[db$Date >= per[1] & db$Date <= per[2],]
        if(!input$past_m %in% c("Nov 20", "Dec 20")) db2 <- db21[db21$Date >= per[1] & db21$Date <= per[2],]
        
        db2 <- db2 %>% pivot_longer(2:ncol(db2), names_to = "Name", values_to = "Done")
        db2$Name <- db2$Name %>% factor(levels = db2$Name %>% unique %>% sort %>% rev)
        db2$Done <- db2$Done %>% as.character
        db2$Done[db2$Done == "1"] <- "V"
        db2 <- db2[db2$Done == "V",]
        ggplot(db2) + geom_label(aes(x = Date, y = Name, label = Done, colour = Done)) +
            scale_x_date(limits = per) +
            scale_color_manual(values = c("green", "red"), guide = F) +
            labs(y = NULL)
    })
    
    output$PastChallengePlot <- renderPlot({
        coul <- "blue"
        if(input$past_m == "Nov 20") per <- c("20201101" %>% ymd(), "20201130" %>% ymd())
        if(input$past_m == "Nov 20") coul <- "#55509B"
        if(input$past_m == "Dec 20") per <- c("20201201" %>% ymd(), "20201231" %>% ymd())
        if(input$past_m == "Dec 20") coul <- "#941E1E"
        if(input$past_m == "Jan 21") per <- c("20210101" %>% ymd(), "20210131" %>% ymd())
        db2 <- db[db$Date >= per[1] & db$Date <= per[2],]
        if(!input$past_m %in% c("Nov 20", "Dec 20")) db2 <- db21[db21$Date >= per[1] & db21$Date <= per[2],]
        db2$Total <- apply(db2[, -1], 1, sum)
        MaxLim <- max(db2$Total)
        db2$Total[db2$Total == 0] <- NA
        ggplot(db2) + geom_point(aes(x = Date, y = Total), colour = coul, shape = 8) +
            scale_y_continuous(limits = c(1, MaxLim), breaks = c(1:MaxLim)) +
            scale_x_date(limits = per) +
            theme(panel.grid.minor = element_blank())
    })
}

shinyApp(ui = ui, server = server)
