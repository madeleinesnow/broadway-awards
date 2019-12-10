
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
plays_musicals$Category <- as.factor(plays_musicals$Category)

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
                                                 
                  h3("Number of Performances of Plays versus Musicals"),
                  strong("Distribution of Runs of Tony Award-Nominated and Award-Winning Shows"),
                                                    
                    br(),
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
                    # generalization of trends. This plot confirmed that Tony-nominated musicals run, on average, 
                    # longer than plays.
                  
                    strong("Average Number of Performances of Plays versus Musicals"),
                    h6("Tony-nominated musicals run, on average, for about 800 performances more than Tony-nominated plays."),
                  
                    br(),
                  
                  # This is the plotly plot for the bar graph showing differences of average runs by category
                  
                  plotlyOutput("comparePlot"),
                  
                  br()
                     
                    ),
               
                  # I was also curious about the relationship between popularity of shows which were nominated for (but did not win)
                  # the Tony Award versus that of shows which actually won the award. It is intuitive that shows which win the 
                  # awards are more popular and run longer. Making these bar plot comparisons was an attempt to confirm or deny this
                  # intuition.
                                                 
                 tabPanel("Nomination vs. Winning",
                                                          
                   h3("Average Run of Tony Award-Nominated versus Award-Winning Shows"),
                   
                   br(),
                   
                   sidebarPanel(
                     
                     radioButtons("type1",
                                  "Category:",
                                  levels(plays_musicals$Category)),
                    
                     # Visualizations for both plays and musicals to show that the pattern of Tony-winning shows running
                     # for more performances, on average, than Tony-nominated shows holds for both categories
                      
                     h6("Tony-winning Broadway plays and musicals run, on average, 
                   longer than Tony-nominated Broadway plays and musicals. Tony-winning
                   musicals run, on average, for 1200 more performances than Tony-nominated
                   musicals, nearly triple the run of nominated but non-winning musicals. Tony-winning
                   plays run, on average, for 250 more performances than Tony-nominated plays,
                   nearly double the run of nominated but non-winning plays.")
                     
                                ),
                   
                   mainPanel(
                                                          
                   plotlyOutput("winnerPlot")
                   
                   # I had winning show points represented with the color gold and nominated/non-winning shows
                   # represented by the color silver. As I suspected, the winner (versus nominee) status of both plays
                   # and musicals might influence this correlation.
                   
                   )),
               
               # Create a tab for visualizations regarding runs of plays and musicals over time, starting with breakdowns 
               # by decade. I chose to visualize the average run time of performances based on the decade in which they opened.
               
               # These graphs reflected historical trends of plays and musicals. Plays which opened in the 1960s and 1970s 
               # had the highest number of average performances in those decades and reflected trends of socially 
               # conscious theatre. Musicals which opened in the 1980s had the highest number of average 
               # performances in those decades and reflected trends of commercialized 'megamusicals.'
  
               tabPanel("Trends by Decade",
                        
                        h3("Runs of Tony-Nominated and Tony-Winning Shows by Decade"),
                        strong("Average Number of Performances of Plays versus Musicals"),
                        
                        br(),
                        br(),
                        
                        sidebarPanel(
                          radioButtons("type2",
                                       "Category:",
                                       levels(plays_musicals$Category)),
                         
                           strong("Plays in the 1960s and 1970s"),
                          
                          h6("Tony-Nominated and Tony-Winning Plays which opened in the 1960s and 1970s had the longest
                      average run of plays across the decades of the award category history, with an average of about 
                      400 performances. This trend can be attributed to the cultural upheaval in the United States. 
                      The era of movements including the civil rights movement, student movement, anti-Vietnam War 
                      movement, gay rights movement, and environmental movement contributed to the subject of socially
                      conscious drama."),
                          
                          strong("Musicals in the 1980s"),
                          
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
              
              # Create a tab for visualizations of regarding runs of plays and musicals over time. I chose a 
              # scatter plot to visualize this data. The data indicates that ...
                   
              tabPanel("Trends by Year",
                   
                  h3("Runs of Plays and Musicals by Year"),
                  strong("Number of Performances of Tony-Nominated and Tony-Winning Plays and Musicals"),
                   
                  plotlyOutput("yearPlot"),
                  
                  h6("Explanation"),
                  
                  br()
                  
                  )
             )),
    
    
    tabPanel("About", 
             mainPanel(
               
               # Ensure that the minimum relevant background is provided to understand what is being presented
               
                 h3("Tony Awards"),
                 p("The Tony Awards, which recognize excellence in live Broadway theatre, are annually nominated and awarded to 
                   Broadway shows across several categories, including Best Play and Best Musical. The awards in these categories 
                   began in 1948 and 1949, respectively, and are presented by the American Theatre Wing and The Broadway League."),
                 
               # Provide information about industry and project data sources
                 
                 h3("Broadway Data"),
                 p("The Broadway League, the national trade association for the Broadway industry, publishes data gathered 
                   from Broadway shows, including the opening and closing dates for Broadway shows, in the Internet Broadway 
                   Database. Broadway shows typically perform 8 shows per week, so running length can be calculated between 
                   opening and closing dates. Running length is used as a proxy for the popularity of plays and musicals."),
                 
                 h3("Data Sources"),
                 a("Internet Broadway Database (IBDB)", href="https://www.ibdb.com/"), p(),
                 a("Tony Awards for Best Musical", href="https://en.wikipedia.org/wiki/Tony_Award_for_Best_Musical"), p(),
                 a("Tony Awards for Best Play", href="https://en.wikipedia.org/wiki/Tony_Award_for_Best_Play"),
                 
              # Include information about the app author so that anyone impressed by my Shiny App can contact me to offer me a job!

                 h3("About Me: Madeleine Snow"),
                 p("I am a senior at Harvard College studying Psychology and Theater, Dance & Media. I enjoy using R to 
                 reveal insights in data all around us."),
                 p("Please contact me at msnow@college.harvard.edu or connect with me on LinkedIn", 
                   a("HERE.", href="https://linkedin.com/in/madeleinesnow/")),
                 
              # Include a link to the Source Code for reproducibility and credibility  
              
                h3("Source Code"),
                h5("The source code for this Shiny App can be found at my GitHub", 
                   a("HERE.", href="https://github.com/madeleinesnow/broadway-awards"))
         ))
  ))
                 
                          
  
           
server <- function(input, output) {
  
  
  output$histPlot <- renderPlotly({
    
    hist <- ggplotly(plays_musicals %>% 
                       filter(Category == input$Category) %>% 
                          group_by(Category) %>% 
                          
                          ggplot(aes(x = Performances, fill = Category)) +
                          geom_histogram() +
                          labs(x = "Number of Performances", y = "Count") +
                       scale_fill_manual(values = c("plum3")))
    
    hide_legend(ggplotly(hist)) %>% config(displayModeBar = FALSE) %>% style(hoverinfo = "skip")

    
  })

  
  output$comparePlot <- renderPlotly({

    compare <- ggplotly(plays_musicals %>%
      group_by(Category) %>% 
      summarise(Average_Run = mean(Performances)) %>% 
    
      ggplot(aes(x = Category, y = Average_Run, fill = Category)) +
    geom_bar(stat = "identity", show.legend=FALSE) +
      labs(y = "Average Run (Number of Performances)") +
      scale_fill_manual(values = c("violetred3", "royalblue3")))
      
    hide_legend(ggplotly(compare)) %>% config(displayModeBar = FALSE)
    
  })
  
  
  output$winnerPlot <- renderPlotly({
    
    winner <- ggplotly(plays_musicals %>%
      filter(Category == input$type1) %>%                 
      group_by(Category, Winner) %>% 
      summarise(Average_Run = mean(Performances)) %>% 
    
      ggplot(aes(x = Winner, y = Average_Run, fill = Winner)) +
      geom_bar(stat = "identity", show.legend=FALSE) +
      facet_grid(~ Category) +
      labs(y = "Average Run (Number of Performances)") +
      scale_fill_manual(values = c("lightsteelblue3", "goldenrod3")))
      
    
    hide_legend(ggplotly(winner)) %>% config(displayModeBar = FALSE)
    
  })
   
  output$decadePlot <- renderPlotly({
    
    decade <- ggplotly(plays_musicals %>% 
       filter(Category == input$type2) %>% 
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
                         scale_x_discrete(breaks = c(1950, 1960, 1970, 1980, 1990, 2000, 2010)) +
                       scale_fill_manual(values = c("violetred3", "royalblue3")))
    
    hide_legend(ggplotly(year)) %>% config(displayModeBar = FALSE)
    
  })
 
}


# Run the application 
shinyApp(ui = ui, server = server)
