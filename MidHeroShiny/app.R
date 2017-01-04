#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(DT)

midData <- readRDS("midData.rds")
heroNames <- read.csv2("heronames2.csv")
heroNames <- select(heroNames, hero_name, URL)
withURL <- merge(midData, heroNames, by.x = "heroName2", by.y = "hero_name")
withURL$oppositeHero <- paste('<img src="', withURL$URL, '"></img> ', withURL$heroName2)

midDataClean <- select(withURL, heroName1, oppositeHero, XPDiff, goldDiff, winRate)
colnames(midDataClean) <- c("heroName1", "Opposing Hero", "XP Difference", "Gold Difference", "Win rate")


choices <- sort(as.character(unique(midDataClean$heroName1)))

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
  includeCSS("styles.css"),
  
   # Application title
   titlePanel("Mid Hero stats, based on YASP data dump (patch 6.85)"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("select", label = h3("Select Hero"), 
                    choices, selected = 1)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(h3("Mid stats:"),
                p("Shows the difference in stats between the hero selected, and his mid opponents, sampled at
                10 minutes into the game."),
                p("Data is based off the YASP data dump, and uses 30.000 games from December 2015 (patch 6.85), only 
                  hero combinations with 20 or more games are shown"),
                p("Unfortunately there has not been another data dump from YASP since December 2015,
                  so the numbers here are not up to date. I know YASP plans on more incremental changes in the future
                  (a huge dump is being prepared for August), so hopefully this tool can be kept updated when/if that 
                  starts happening reguarly."),
        dataTableOutput('table')
      )
   )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
    output$table <- renderDataTable(
      datatable(subset(midDataClean, midDataClean$heroName1 == input$select)[,-1], 
                options = list (pageLength = 50), escape = FALSE) %>% formatStyle(
        'XP Difference',
        color = styleInterval(c(-100,100), c('red', 'orange','green')),
        background = styleColorBar(midDataClean$`XP Difference`, '#202020'),
        backgroundSize = '95% 80%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center') %>% formatStyle(
        'Gold Difference',
        color = styleInterval(c(-100,100), c('red', 'orange','green')),
        background = styleColorBar(midDataClean$`Gold Difference`, '#202020'),
        backgroundSize = '100% 80%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center') %>% formatStyle(
        'Win rate',
        color = styleInterval(c(0.48,0.52), c('red', 'orange','green')),
        background = styleColorBar(midDataClean$`Win rate`, '#202020'),
        backgroundSize = '100% 80%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center')
        )
})

# Run the application 
shinyApp(ui = ui, server = server)

