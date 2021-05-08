#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(tidycensus)
source("data.R")
library(shinythemes)


# This is my first ever tab on my shiny app.
# I wanted to start off my shinyapp by displaying an interactive chart


ui <- navbarPage(
    "Hitter Evaluations",
    tabPanel("Introduction",
             fluidPage(fluidRow(
                 column(3,
                        titlePanel("Velo Distribution"),
                        p("This graph displays a posterior distrubution for Average
                          MLB exit velocity. Exit Velocity is a measure for how fast
                          a ball comes off a hitter's bat in a game. As you can see from
                          the posterior distribution, the average exit velo is around 90
                          miles per hour. This means that if watched one random hit 
                          from the MLB season, there is around a 3% chance that the ball
                          will be hit near 90 miles per hour off the bat.
                          
                          I created this distribution using the function stan_glm. 
                          Since I had no predictor variables, I had a formula argument 
                          of avg_hit_speed ~ 1. I used data from Baseball Savant.")),
                 
                 
                 # I chose to make the plot a lot wider in order to fully display the chart.
                 # I did not want to just focus on the explanation itself.
                 
                 column(9,
                        imageOutput("mph_distribution"))))),
    # I now have a tab that was supposed to be my introduction tab.
    # Although, I did not end up liking the tab itself.
    # I had trouble understanding the meaning of the histogram.
    
    tabPanel("Constitutional",
             fluidPage(theme = shinytheme("darkly"),
                       fluidRow(
                           column(7,
                                  titlePanel("The Basics"),
                                  plotOutput("plot_1"),
                                  p(" ")),
                           
                           
                           # Displaying the image was a pocess in it of itself. It took me a while
                           # to display it without the console running forever.
                           
                           column(5,
                                  titlePanel("Velo Correlation"),
                                  p("This graph displays the correlation between
                                    exit velocity and distance traveled of the ball. 
                                    As you can see, there is a positve correlation
                                    between the two. Because the game of baseball
                                    is relying more and more on power, exit velocity
                                    is becoming a key stat for hitting long home runs"))))),
    
    tabPanel("Interaction",
             titlePanel("Choose your character"),
             
    
# I honestly don't know what a sidebar layout or a sidebar Panel is.

             sidebarLayout(
                 sidebarPanel(
                     
                     
# I received inspiration/help from Diego Martinez github for this graph.
# This turned out to be a lot less code than I expected.
                     
                     selectInput("name",label = strong("Players"),
                                 choices = unique(stats_5$full_name),
                                 selected = "Adam Eaton",
                                 multiple = TRUE)),

                 
# Now I am showing the plot and referring down to the server itself.
# I was confused to see what br() was from Diego's code.
                 
                 mainPanel(
                     plotOutput("linePlot"),
                     br(),
                     br(),
                     h4(" ")))),


# I start a new tab to begin the focus of my argument.
# I began fluidRow here to help organize my code.

    tabPanel("Bad Guys",
             fluidPage(fluidRow(
                 column(5,
                        titlePanel("The Worst Barrel Ratios"),
                        plotOutput("barrel_plot"),
                        p("The graph above is showing you top 10 worst barrel ratios
                          in the league. Barrel Ratio is a self created stat which is 
                          a measure of the percentage of hits a player has that are
                          hard hit balls. Because these players have bad barrel ratios,
                          a small percentage of their hits were balls that they hit hard.
                          Their batting average may be high, but they were lucky most of 
                          the time that they got a hit. This is intriguing to an MLB 
                          general manager when determing the salary that a certain hitter 
                          should be paid.")),
                 
                 
# I chose to make the salary plot longer to make the names more visible

                 column(7,
                        titlePanel("The Worst Players and their Salaries"),
                        plotOutput("salary_bad_plot"),
                        p("The graph above is showing you what the players with 
                          the top 10 worst barrel ratios are getting paid. As you can 
                          see, some of the players are getting paid about what they should,
                          but some of the players are getting paid too much. We will
                          summarize this chart in the Evaluations tab."))))),


# I then made a tab for players with higher ratios
# It followed a very similar format to that of the proceeding tab.

    tabPanel("Good Guys",
             fluidPage(fluidRow(
                 column(5,
                        titlePanel("The Best Barrel Ratios"),
                        plotOutput("barrel_plot_good"),
                        p("The graph above is showing you top 10 best barrel ratios
                          in the league. As mentioned previously, barrel ratio is a self
                          created stat which is 
                          a measure of the percentage of hits a player has that are
                          hard hit balls. Because these players have good barrel ratios,
                          a large percentage of their hits were balls that they hit hard.
                          There batting average may be low, but they hit the ball
                          hard most of 
                          the time that they got a hit. This is also intriguing to an MLB 
                          general manager when determing the salary that a certain hitter 
                          should be paid. ")),


# I initially had a theme from a package that I found online.
# I ended up changing the theme, because it did not fit the theme of the shinyapp

                 column(7,
                        titlePanel("The Best Players and their Salaries"),
                        plotOutput("salary_good_plot"),
                        p("The graph above is showing you what the players with 
                          the top 10 best barrel ratios are getting paid. As you can 
                          see, some of the players are getting paid about what they should,
                          but some of the players are getting paid too little. We will
                          summarize this chart in the Evaluations tab. "))))),


# I then created a tab to display the players that I found as overvalued and
# undervalued. This was a confusing process for me, because I had to find
# png images that were very similar in size. # I also don't know why the 
# height on each of the images are distorted. 
# I only chose 3 players that had the best salary to barrel ratio ratios for 
# thier particular extreme.

    tabPanel("Evaluations",
             fluidPage(fluidRow(
                 column(6,
                        plotOutput("barrel_bad_salary"),
                        p("We created the graph above by using the same players from
                          the worst barrel ratios graph. The graph shows the 
                          ratio between their poor barrel ratios and their 
                          salaries. We called this their Barrel-to-Salary Ratios.
                          If a player had a low barrel ratio but a high salary, then
                          they would have a low Barrel-to-Salary Ratio. As you can
                          see, Kolten Wong, Johnathan Villar, and Hanser Alberto 
                          have the worst Barrel-to-Salary Ratios. This means they
                          are getting paid too much for the small amount of hard
                          hit balls that they are hitting."))
                 ,
                 column(6,
                        plotOutput("barrel_good_salary"),
                        p("We created the graph above by using the same players from
                          the best barrel ratios graph. The graph shows the 
                          ratio between their great barrel ratios and their 
                          salaries. We called this their Barrel-to-Salary Ratios.
                          If a player had a high barrel ratio but a low salary, then
                          they would have a high Barrel-to-Salary Ratio. As you can
                          see, Keston Hiura, Pete Alonso, and Brandon Lowe 
                          have the best Barrel-to-Salary Ratios. This means they
                          are getting paid too little for the large amount of hard
                          hit balls that they are hitting."))))),


# I then created a tab that displayed a line chart of the top 10 players.
# I wanted to show how they would progress over the years.

    tabPanel("Progression",
             fluidPage(fluidRow(
                 column(3,
                        titlePanel("The Best and their Path"),
                        p("This graph displays the top 10 players and their
                          barrel ratios throughout the years. As you can see
                          there are players who have been around for a long time,
                          Nick Castellanos, and we have players who have only been
                          around for a year, Fernando Tatis Jr. In fact, there is
                          only a small point for Tatis on this chart, because he
                          dubuted in 2021.")),
                 

# I chose to make the plot a lot wider in order to fully display the chart.
# I did not want to just focus on the explanation itself.

                 column(9,
                        plotOutput("yearly_barrel"))))),





# I then created a discussion tab that discusses the modeling choices I made.
# I ended up saving all of my discussion for last in the code.

    tabPanel("Discussion",
             titlePanel("Why I did this"),
             p("I wanted to see which players get lucky, and which players
               get unlucky. A hitter can control whether he hits a ball hard or 
               not based on his own actions. On the other hand, a hitter cannot
               control whether he hits a ball hard right at fielder. I wanted to
               display this discrepancy by creating the variable barrel ratio. 
               Barrel ratio measures that percentage of balls a player hits hard 
               compared to the players batting average. If a player hit a lot of
               balls hard, but did not get many hits, he would have a higher barrel
               ratio. Therfore that player would be undervalued. On the other hand,
               if a player had a lot of hits, but did not hit many of them hard, he
               would have a lower barrel ratio. Therefore that player would be overvalued.")),


# I then created an about panel to describe my inspiration for this project.

    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("I am going to try to get data from baseball-reference.com. I hope
               to find some good info there about players' hard hit ball percentage. 
               There is also another website that I need to talk to Diego about. It 
               is another baseball website that is not widely known, but it has a lot of
               good data. I need to confirm the name shortly.Diego has referenced this
               website in the Harvard Sport Analytics Club. I think I want to focus on 
               the hard hit ball percentage on both of these websites. I want to find the
               ratio between that percentage and the players batting average. I believe
               that baseball-reference.com will get me pretty far on this, but I will 
               also gather more in depth data from the source recommended by Diego."),
             h3("About Me"),
             p("My name is Christopher Snopek and I study Data Science! 
             You can reach me at csnopek@college.harvard.edu."),
             p("This project was a long process. I started off with only players batting
               average and the number of hard hit balls that they had. I got 
               most of my data from", a("Baseball Savant", 
                                            href="https://baseballsavant.mlb.com/savant-player/pete-alonso-624413?stats=statcast-r-hitting-mlb"), 
               "I had individually look up 
               the salaries of certain players. I found the salaries through", a("US Today",
                                                                                 href="https://databases.usatoday.com/mlb-salaries/"), "and",
               a("Baseball Reference",
                 href="https://www.baseball-reference.com/about/salary.shtml"),
              ". I then piped my way until
               I found 10 of the best and 10 of the worst barrel ratios, a self made
               variable to measure quality of at bats. I then used this to see if the
               players were undervalued or overvalued based on their salaries. I ended up
               with a fair conclusion by displaying a couple of players that were overvalued
               and a couple of players that were undervalued.")))


# I first had to set up these libraries to help with the interaction
# portion of the server.
# I had to load the library direct labels to help with the interaction 
# portion of the server.

server <- function(input, output) {
    library(ggplot2)
    library(tidyverse)
    library(ggthemes)
    library(directlabels)
    
    
# This was the first image that I actually displayed. It took me a while
# to first display the image itself.

    output$mph_distribution <- renderImage({list(src = "new_baseball_velocity.png",
                                                 width = 600,
                                                 height = 500,
                                                 alt = "MPH Distribution")


# This was the image of Keston Hiura with not height or width argument.

    }, deleteFile = FALSE)
 
    
# now we are on to all of the plots.
# This part of the code was pretty easy for me to understand.
# The left side was the thing that I called in the shinyapp, and the 
# right side was the thing that I call from the data.R file.
    
    output$plot_1 <- renderPlot(plot_1)
    output$barrel_plot <- renderPlot(barrel_plot)
    output$salary_bad_plot <- renderPlot(salary_bad_plot)
    output$salary_good_plot <- renderPlot(salary_good_plot)
    output$barrel_plot_good <- renderPlot(barrel_plot_good)
    output$yearly_barrel <- renderPlot(yearly_barrel)
    output$barrel_bad_salary <- renderPlot(barrel_bad_salary)
    output$barrel_good_salary <- renderPlot(barrel_good_salary)
    
# This is where I start to get some help from Diego Martinez's code.
# I did not change much from his graph, but I plan on doing so before May 9.

    subset<-reactive({stats_5%>% filter(full_name %in%input$name)})
    
    
    output$linePlot <- renderPlot({
        
        
# draws line plot for player chosen
        
        ggplot(subset(), aes(x=subset()$year, y=subset()$exit_velocity_avg, color = subset()$full_name))+
            
            
#smoothed trend line of performance  
            
            geom_smooth(formula = y~x+x^2, se = FALSE, size = 1.5)+
            
            
# actual datapoints and Exit Velocity from players career. 
            
            geom_line(alpha = 0.20)+
            geom_point(alpha = 0.5)+
            
        
# ranges of Exit Velocity and Year in the dataset
            
            scale_y_continuous(breaks = seq(80,100,1), limits = c(80,100)) + 
            scale_x_continuous(breaks = seq(2015,2021,1), limits = c(2015,2021)) +
            
            
            labs(x="Year", y = "Average Exit Velocity", color = "Players")+
           
             
# for aesthetic purpose and including axis labels 
            
            theme_fivethirtyeight()+
            theme(axis.title = element_text(colour = "black" ))
    }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
