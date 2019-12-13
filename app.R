
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

# Read in data from rds files

plays_musicals <- read_rds("bway_data/plays_musicals.rds")
plays_musicals$Category <- as.factor(plays_musicals$Category)
plays_musicals$Winner <- as.factor(plays_musicals$Winner)

plays <- read_rds("bway_data/plays.rds")
plays$Category <- as.factor(plays$Category)
plays$Winner <- as.factor(plays$Winner)

musicals <- read_rds("bway_data/musicals.rds")
musicals$Category <- as.factor(musicals$Category)
musicals$Winner <- as.factor(musicals$Winner)

# Define UI for application, use the shinytheme "sandstone" for professional appearance 

ui <- fluidPage(theme = shinytheme("sandstone"),
 
  # Add line breaks for aesthetic purposes
  
    br(),
  
  # Set the first division of the navbar to be Broadway Runs - a section that will compare popularity of plays versus 
  # musicals in terms of performances, my proxy to measure popularity, for shows which were nominated for 
  # and/or won Tony Awards
  
  navbarPage("It's a Hit: Tony Awards for Best Play and Best Musical", tabPanel("Plays vs. Musicals",  
        
            # My first question when playing with this data was about popularity of plays versus musicals over time. Are
            # Broadway musicals really more popular than plays?
            
            # Create a tab for visualizations of runs of plays and musicals over time. I chose a 
            # scatter plot to visualize this data. The data indicates consistent trends in longer runs for Tony-nominated/
            # Tony-winning musicals than Tony-nominated/Tony-winning plays. However, it is interesting to note the length 
            # of popular musicals' runs which has a positive trend from the 1950s to the 1980s but decreases in run over time.
                                                                                                              
             tabsetPanel(
               tabPanel("Trends by Year",
                        
                        h3("Runs of Plays and Musicals by Year"),
                        strong("Number of Performances of Tony-Nominated and Tony-Winning Plays and Musicals"),
                        
                        # This plot is made from the plotly package for crisper, sharper looking graphics than ggplot2. 
                        # This plotly plot is the scatterplot of Tony-nominated/winning plays vs musicals over time
                        
                        plotlyOutput("yearPlot"),
                        
                        h6("In every year since 1955, the longest-running Tony-nominated or Tony-winning show of the season was a musical. 
                  In some years, however, plays up for these awards ran longer than some other musicals which opened that year. While 
                  plays had few outlying shows with relatively long runs within the category, musicals
                  had increasing numbers of outliers with long runs from the 1940s to the 1980s. This trend reversed in the
                  1990s, showing decreasing frequency of outliers with long runs of musicals."),
                        
                        br()
                        
               ),
               
               
               # This visualization prompted me to examine these trend by decade. I chose to visualize the average run time of 
               # plays versus musicals based on the decade in which they opened using a bar graph for clear comparisons between decades.
               
               # These graphs reflected historical trends of plays and musicals. Plays which opened in the 1960s and 1970s 
               # had the highest number of average performances in those decades and reflected trends of socially 
               # conscious theatre. Musicals which opened in the 1980s had the highest number of average 
               # performances in those decades and reflected trends of commercialized 'megamusicals.'
               
               tabPanel("Trends by Decade",
                        
                        h3("Runs of Tony-Nominated and Tony-Winning Shows by Decade"),
                        strong("Average Number of Performances of Plays versus Musicals"),
                        
                        br(),
                        br(),
                        
                        # Viewers can interact with the app, choosing to view decade trends in plays or musicals based on the category they click
                        
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
                          
                          # This is the plotly plot comparing the average length of runs by decade between the categories of plays and musicals 
                          
                          plotlyOutput("decadePlot"),
                          
                          br()
                          
                        )),
               
               # Final tab comparing Tony-nominated/Tony-winning plays versus musicals. I chose to show the distribution of 
               # runs between the categories of plays and musicals using a histogram to show frequency. I also chose to use a bar graph
               # for simple visual comparison of the length of runs between these categories of show.
               
               tabPanel("Summary",
                        
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
                          
                          # This is the plotly plot for the histogram showing differences between distribution of run lengths by category
                          
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
                        
               ))),
               
          
         # My next question when playing with this data was about popularity of shows which were nominated for versus actually 
         # won Tony Awards over time. Are shows which were nominated for and won Tony Awards really more popular than 
         # nominated, non-winning ones?
        
         # Create a tab for visualizations of Tony winners versus nominees over time. As with my plays versus musical comparison, I chose a 
         # scatter plot to visualize this data. The data indicates overall trends in longer runs for Tony-winning than
         # Tony-winning shows, but there are several years in which a Tony-nominated show ran longer than Tony-winning shows
         # which opened that same year. Interestingly, all of those popular, Tony-nominated shows are musicals.
            
          tabPanel("Award Winners vs. Nominees",  
                  
          tabsetPanel(
            tabPanel("Trends by Year",
                     
                     h3("Runs of Nominated and Winning Shows by Year"),
                     strong("Number of Performances of Tony-Nominated and Tony-Winning Plays and Musicals"),
                     
                     # This plotly plot is the scatterplot of Tony-nominated vs Tony-winning shows over time

                     plotlyOutput("yearwinnerPlot"),
                     
                     h6("In most years, the longest-running show won the Tony, but there are several years in 
                     which a Tony-nominated show ran significantly longer than the Tony-winning shows that season:
                     1972 (Grease), 1991 (Miss Saigon), 1994 (Beauty and the Beast), 2002 (Mamma Mia!), 
                     2004 (Wicked), 2007 (Mary Poppins), 2009 (Rock of Ages), and 2014 (Aladdin). Musicals were the 
                     sole category of the longest-running show, then, in every year in which Tony-nominated shows 
                     outran Tony-winning shows."),
                     
                     br()
                     
            ),
            
            tabPanel("Trends by Decade",
                     
                     h3("Runs of Plays versus Musicals by Decade"),
                     strong("Average Number of Performances of Tony-Nominated and Tony-Winning Shows by Category"),
                     
                     br(),
                     br(),
                     
                     sidebarPanel(
                       radioButtons("type4",
                                    "Ranking:",
                                    levels(plays_musicals$Winner)),
                       
                       strong("Winners in the 1980s"),
                       
                       h6("Tony-Winning shows which opened in the 1980s had the longest average run of plays and musicals 
                          across the decades of the awards' history, with an average of about 
                          2,200 performances. The decades prior to the 80s show an increase across decades in the average
                          number of performances of Tony-Winning shows, while the decades after the 80s show a decrease in
                          average number of performances."),
                       
                       strong("Nominees in the 2000s"),
                       
                       h6("Tony-Nominated shows which opened in the 2000s had the longest average run of plays and musicals 
                          across the decades of the awards' history, with an average of about 
                          575 performances. This trend can be attributed to the rise of Disney and jukebox musicals, which were popular and 
                          commercially successful but were not the strongest candidates in those year to win the Tony Award.")         
                       
                     ),
                     
                     mainPanel(
                       
                       plotlyOutput("decadewinnerPlot"),
                       
                       br()
                       
                     )),
            
            
            # I was curious about the overall relationship between popularity of shows which were nominated for (but did not win)
            # the Tony Award versus that of shows which actually won the award. It is intuitive that shows which win the 
            # awards are more popular and run longer. Making these bar plot comparisons was an attempt to confirm or deny this
            # intuition.
            
            tabPanel("Summary",
                     
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
                       
                     ))
            
            
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
  
  output$decadewinnerPlot <- renderPlotly({
    
    decadewinner <- ggplotly(plays_musicals %>% 
                         filter(Winner == input$type4) %>% 
                         filter(Decade != "1940s") %>% 
                         group_by(Decade, Winner) %>% 
                         summarise(Average_Run = mean(Performances)) %>% 
                         
                         ggplot(aes(x = Decade, y = Average_Run, fill = Decade)) +
                         geom_bar(stat = "identity", show.legend=FALSE) +
                         facet_grid(~ Winner) +
                         labs(x = "", y = "Average Run (Number of Performances)"))
    
    hide_legend(ggplotly(decadewinner)) %>% config(displayModeBar = FALSE)
    
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
  
  output$yearwinnerPlot <- renderPlotly({
    
    yearwinner <- ggplotly(plays_musicals %>% 
                       group_by(Year, Winner) %>% 
                       
                       ggplot(aes(x = Year, y = Performances, fill = Winner, size = Performances)) +
                       geom_point() +
                       scale_x_discrete(breaks = c(1950, 1960, 1970, 1980, 1990, 2000, 2010)) +
                       scale_fill_manual(values = c("lightsteelblue3", "goldenrod3")))
    
    hide_legend(ggplotly(yearwinner)) %>% config(displayModeBar = FALSE)
 
})
  
}


# Run the application 
shinyApp(ui = ui, server = server)
