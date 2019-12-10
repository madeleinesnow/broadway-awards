
# Load neccessary dependencies

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

plays_musicals <- read_rds("bway_data/plays_musicals.rds")

# Define UI for application, use the shinytheme "sandstone" for professional appearance 

ui <- fluidPage(theme = shinytheme("sandstone"),
 
  # Add line breaks for aesthetic purposes
  
    br(),
  
  # Set the first division of the navbar to be Broadway Runs - a section that will analyze number
  # of performances, a proxy to measure popularity, of Broadway plays and musicals that were nominated for 
  # and/or won Tony Awards
  
  navbarPage("It's a Hit: Tony Awards for Best Play and Best Musical", tabPanel("Broadway Runs",  
        
            # My first question when playing with this data was about popularity of plays versus musicals. Are
            # Broadway plays really more popular than musicals?
                                                                                                              
             tabsetPanel(
               tabPanel("Plays vs. Musicals",
                                                 
                  h4("Number of Performances of Plays versus Musicals"),
                  h5("Distribution of Runs of Tony Award-Nominated and Award-Winning Shows"),
                                                    
                    br(),
                  
                  sidebarPanel(
                    
                    # These histogram showing distribution of number of performances can be faceted according
                    # to category, namely plays versus musicals, which I anticipated would show significant
                    # differences and that indeed led to different distributions when varied.
                    
                    radioButtons("Category",
                                 "Category:",
                                 unique(plays_musicals$Category)),
                    
                    # From these visualizations, some trends can be generalized
                    
                    h6("The distribution of number of performances for Tony-nominated and Tony-winning Broadway musicals 
                       is more skewed right than number of performances for Broadway plays, illustrating that Broadway musicals tend
                       to have longer runs and more performances.")
                               
                                ),
                    
                  mainPanel(
                    
                    # This plot is made from the plotly package for crisper, sharper looking graphics than ggplot2
                                                    
                    plotlyOutput("histPlot"),
                  
                    br()),
                  
                    br(),
                  
                    # I wanted to show clear differences by displaying the difference of means between categories,
                    # so I included a bar plot for easy visualization of side-by-side comparison and written 
                    # generalization of trends.
                  
                    h4("Average Number of Performances of Plays versus Musicals"),
                  h5("Tony-nominated musicals run, on average, for about 800 performances more than Tony-nominated plays."),
                  
                  br(),
                  
                  # This is the plotly plot for the bar graph showing differences of average runs by category
                  
                  plotlyOutput("comparePlot"),
                                                    
                    br()
                     ),
                                                 
                 tabPanel("Nomination vs. Winning",
                                                          
                   h4("Average Run of Tony Award-Nominated versus Award-Winning Shows"),
                                                          
                   plotlyOutput("winnerPlot"),
                                                          
                   br(),
                                                          
                   h6("Tony-winning Broadway plays and musicals run, on average, 
                   longer than Tony-nominated Broadway plays and musicals. Tony-winning
                   musicals run, on average, for 1200 more performances than Tony-nominated
                   musicals, nearly triple the run of nominated but non-winning musicals. Tony-winning
                   plays run, on average, for 250 more performances than Tony-nominated plays,
                   nearly double the run of nominated but non-winning plays.")),
  
               tabPanel("Trends by Decade",
                        
                        h4("Runs of Tony-Nominated and Tony-Winning Shows by Decade"),
                        h5("Average Number of Performances of Plays versus Musicals"),
                        
                        
                        sidebarPanel(
                          radioButtons("Category",
                                       "Category:",
                                       unique(plays_musicals$Category)),
                         
                           strong("Tony-Nominated and Tony-Winning Musicals in the 1960s and 1970s"),
                          
                          h6("Tony-Nominated and Tony-Winning Plays which opened in the 1960s and 1970s had the longest
                      average run of plays across the decades of the award category history, with an average of about 
                      400 performances. This trend can be attributed to the cultural upheaval in the United States. 
                      The era of movements including the civil rights movement, student movement, anti-Vietnam War 
                      movement, gay rights movement, and environmental movement contributed to the subject of socially
                      conscious drama."),
                          
                          strong("Tony-Nominated and Tony-Winning Musicals in the 1980s"),
                          
                          h6("Tony Award-Nominated and Award-Winning Musicals which opened in the 1980s had the longest
                      average run of musicals across the decades of the award category history, with an average of over
                      1,300 performances. This trend can be attributed to the commercialization of musicals intended
                      to be large-scale spectacles, known as 'mega-musicals.' Notable megamusicals
                      in the 1980s include Cats (1981), Les Miserables (1985), and The Phantom of the Opera (1986).")         
                          
                          ),
                        
                        mainPanel(
                          
                          plotlyOutput("decadePlot"),
                          
                          br()
                        
                        )),
                  
              tabPanel("Trends by Year",
                   
                  h4("Runs of Plays and Musicals by Year"),
                  h5("Number of Performances of Tony-Nominated and Tony-Winning Plays and Musicals"),
                  
                  plotlyOutput("yearPlot"),
                         
                   h6("Explanation"),
                  
                  br()
                  
                  ))
             )),
    
    
    tabPanel("About", 
             mainPanel(
                 h3("Tony Awards"),
                 p("The Tony Awards, which recognize excellence in live Broadway theatre, are annually nominated and awarded to 
                   Broadway shows across several categories, including Best Play and Best Musical. The awards in these categories 
                   began in 1948 and 1949, respectively, and are presented by the American Theatre Wing and The Broadway League."),
                 
                 h3("Broadway Data"),
                 p("The Broadway League, the national trade association for the Broadway industry, publishes data gathered 
                   from Broadway shows, including the opening and closing dates for Broadway shows, in the Internet Broadway 
                   Database. Broadway shows typically perform 8 shows per week, so running length can be calculated between 
                   opening and closing dates. Running length is used as a proxy for the popularity of plays and musicals."),
                 h3("Data Sources"),
                 a("Internet Broadway Database (IBDB)", href="https://www.ibdb.com/"), p(),
                 a("Tony Awards for Best Musical", href="https://en.wikipedia.org/wiki/Tony_Award_for_Best_Musical"), p(),
                 a("Tony Awards for Best Play", href="https://en.wikipedia.org/wiki/Tony_Award_for_Best_Play"),
                 h3("About Me: Madeleine Snow"),
                 p("I am a senior at Harvard College studying Psychology and Theater, Dance & Media. I enjoy using R to 
                 reveal insights in data all around us."),
                 p("Please contact me at msnow@college.harvard.edu or connect with me on LinkedIn", 
                   a("HERE.", href="https://linkedin.com/in/madeleinesnow/")),
                 h3("Source Code"),
                h5("The source code for this Shiny App can be found at my GitHub", 
                   a("HERE.", href="https://github.com/madeleinesnow/broadway-awards"))
         ))
  )
                 
                          
  
           
server <- function(input, output) {
  
  
  output$histPlot <- renderPlotly({
    
    hist <- ggplotly(plays_musicals %>% 
                       filter(Category == input$Category) %>% 
                          group_by(Category) %>% 
                          
                          ggplot(aes(x = Performances, fill = Category)) +
                          geom_histogram() +
                          facet_grid(~ Category) +
                          labs(x = "Number of Performances", y = "Count"))
    
    hide_legend(ggplotly(hist)) %>% config(displayModeBar = FALSE) %>% style(hoverinfo = "skip")

    
  })

  
  output$comparePlot <- renderPlotly({

    compare <- ggplotly(plays_musicals %>% 
      group_by(Category) %>% 
      summarise(Average_Run = mean(Performances)) %>% 
    
      ggplot(aes(x = Category, y = Average_Run, fill = Category)) +
    geom_bar(stat = "identity", show.legend=FALSE) +
      labs(y = "Average Run (Number of Performances)"))
      
    hide_legend(ggplotly(compare)) %>% config(displayModeBar = FALSE)
    
  })
  
  
  output$winnerPlot <- renderPlotly({
    
    winner <- ggplotly(plays_musicals %>% 
      group_by(Category, Winner) %>% 
      summarise(Average_Run = mean(Performances)) %>% 
    
      ggplot(aes(x = Winner, y = Average_Run, fill = Winner)) +
      geom_bar(stat = "identity", show.legend=FALSE) +
      facet_grid(~ Category) +
      labs(y = "Average Run (Number of Performances)"))
    
    hide_legend(ggplotly(winner)) %>% config(displayModeBar = FALSE)
    
  })
   
  output$decadePlot <- renderPlotly({
    
    decade <- ggplotly(plays_musicals %>% 
       filter(Category == input$Category) %>% 
       filter(Decade != "1940s") %>% 
       group_by(Decade, Category) %>% 
       summarise(Average_Run = mean(Performances)) %>% 
  
       ggplot(aes(x = Decade, y = Average_Run, fill = Decade)) +
       geom_bar(stat = "identity", show.legend=FALSE) +
       facet_grid(~ Category) +
       labs(x = "", y = "Average Run (Number of Performances)"))
    
    hide_legend(ggplotly(decade)) %>% config(displayModeBar = FALSE)
    
  })
  
  output$yearPlot <- renderPlotly({
    
    year <- ggplotly(plays_musicals %>% 
                         group_by(Year, Category) %>% 
                       
                         ggplot(aes(x = Year, y = Performances, fill = Category, size = Performances)) +
                         geom_point() +
                         scale_x_discrete(breaks = c(1950, 1960, 1970, 1980, 1990, 2000, 2010)))
    
    hide_legend(ggplotly(year)) %>% config(displayModeBar = FALSE)
    
  })
 
}


# Run the application 
shinyApp(ui = ui, server = server)
