
# Libraries

library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(stringr)
library(gt)
library(forcats)
library(plotly)
library(ggthemes)
library(DT)

# Read in data from rds file 

plays <- read_rds("bway_data/plays_musicals.rds")

# Define UI for application that draws a histogram
ui <- navbarPage(title = "It's a Hit: Tony Awards for Best Play and Best Musical", 
                 inverse = T, selected = "About",
                 
                 tabPanel("About", 
                 h3("Tony Awards"),
                 p("The Tony Awards, which recognize excellence in live Broadway theatre, are annually nominated and awarded to 
                   Broadway shows across several categories, including Best Play and Best Musical. The awards in these categories 
                   began in 1948 and 1949, respectively, and are presented by the American Theatre Wing and The Broadway League."),
                 
                 h3("Broadway Data"),
                 p("The Broadway League, the national trade association for the Broadway industry, publishes data gathered 
                   from Broadway shows, including the opening and closing dates for Broadway shows, in the Internet Broadway 
                   Database. Broadway shows typically perform 8 shows per week, so running length can be calculated between 
                   opening and closing dates. Running length is used as a proxy for the popularity of plays and musicals."),
                 h3("Sources"),
                 a("Internet Broadway Database (IBDB)", href="https://www.ibdb.com/"), p(),
                 a("Tony Awards for Best Musical", href="https://en.wikipedia.org/wiki/Tony_Award_for_Best_Musical"), p(),
                 a("Tony Awards for Best Play", href="https://en.wikipedia.org/wiki/Tony_Award_for_Best_Play"),
                 h3("About"),
                 p("Madeleine Snow is a senior at Harvard College studying Psychology and Theater, Dance & Media."),
                 a("github.com/madeleinesnow", href="https://github.com/madeleinesnow")),
                 
                 
                 tabPanel("Broadway Runs",  
                          
                          fluidPage(
                              
                            h3("Running Length of Tony Award-Nominated Plays and Musicals",
                               
                               br(),
                               
                               plotlyOutput("comparePlot"),
                               
                               br(),
                               
                               h5("Tony-nominated and Tony-winning Broadway musicals run, on average, 
                                  for about 800 performances more than Tony-nominated and Tony-winning Broadway plays."),
                               
                               br(),
                               
                               plotlyOutput("winnerPlot"),
                               
                               br(),
                               
                               h5("Tony-winning Broadway plays and musicals run, on average, 
                                  longer than Tony-nominated Broadway plays and musicals. Tony-winning
                                  musicals run, on average, for 1200 more performances than Tony-nominated
                                  musicals, nearly triple the run of nominated but non-winning musicals. Tony-winning
                                  plays run, on average, for 250 more performances than Tony-nominated plays,
                                  nearly double the run of nominated but non-winning plays.")))))
                          
  
           
server <- function(input, output) {
  

  output$comparePlot <- renderPlotly({

    compare <- ggplotly(plays_run %>% 
      group_by(Category) %>% 
      summarise(Average_Run = mean(Performances)) %>% 
    
      ggplot(aes(x = Category, y = Average_Run, fill = Category)) +
    geom_bar(stat = "identity", show.legend=FALSE))
      
    hide_legend(ggplotly(compare)) %>% config(displayModeBar = FALSE)
    
  })
  
  
  output$winnerPlot <- renderPlotly({
    
    winner <- ggplotly(plays_run %>% 
      group_by(Category, Winner) %>% 
      summarise(Average_Run = mean(Performances)) %>% 
    
      ggplot(aes(x = Winner, y = Average_Run, fill = Winner)) +
      geom_bar(stat = "identity", show.legend=FALSE) +
      facet_grid(~ Category))
    
    hide_legend(ggplotly(winner)) %>% config(displayModeBar = FALSE)
    
  })
    
 
}


# Run the application 
shinyApp(ui = ui, server = server)
