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
                        p("Explanation")),
                 
                 
# I chose to make the salary plot longer to make the names more visible

                 column(7,
                        titlePanel("The Worst Players and their Salaries"),
                        plotOutput("salary_bad_plot"),
                        p("Explanation"))))),


# I then made a tab for players with higher ratios
# It followed a very similar format to that of the proceeding tab.

    tabPanel("Good Guys",
             fluidPage(fluidRow(
                 column(5,
                        titlePanel("The Best Barrel Ratios"),
                        plotOutput("barrel_plot_good"),
                        p(" ")),


# I initially had a theme from a package that I found online.
# I ended up changing the theme, because it did not fit the theme of the shinyapp

                 column(7,
                        titlePanel("The Best Players and their Salaries"),
                        plotOutput("salary_good_plot"),
                        p(" "))))),


# I then created a tab to display the players that I found as overvalued and
# undervalued. This was a confusing process for me, because I had to find
# png images that were very similar in size. # I also don't know why the 
# height on each of the images are distorted. 
# I only chose 3 players that had the best salary to barrel ratio ratios for 
# thier particular extreme.

    tabPanel("Evaluations",
             fluidPage(fluidRow(
                 column(6,
                        titlePanel("Undervalued Players"),
                        p("Keston Hiura"),
                        imageOutput("kestonhiura"),
                        p("Evan White"),
                        imageOutput("evanwhite"),
                        p("Brandon Lowe"),
                        imageOutput("blowe")
                 ),
                 column(6,
                        titlePanel("Overvalued Players"),
                        p("Johnathan Villar"),
                        imageOutput("villar"),
                        p("David Fletcher"),
                        imageOutput("dfletcher"),
                        p("Kolten Wong"),
                        imageOutput("kwong"))))),


# I then created a tab that displayed a line chart of the top 10 players.
# I wanted to show how they would progress over the years.

    tabPanel("Progression",
             fluidPage(fluidRow(
                 column(3,
                        titlePanel("The Best and their Path"),
                        p("This graph displays the top 10 players and their
                          barrel ratios throughout the years.")),
                 

# I chose to make the plot a lot wider in order to fully display the chart.
# I did not want to just focus on the explanation itself.

                 column(9,
                        plotOutput("yearly_barrel"))))),


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
                                  titlePanel("Classy Distribution"),
                                  imageOutput("mph_distribution"),
                                  p(" "))))),


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
             p("I have not processed the data for my project into my raw data folder. 
               I have made progress on the project though. I know with confidence, what 
               I want my topic to be and how I want to analyze that topic. I have also done
               research on different graphs that involve the topic of hard hit baseballs to
               get an idea of how I should represent that data. I feel good about my idea, because
               I feel like hard hit baseballs are often overlooked by organizations. I did find
               a csv that I could use during the milestone. I am happy that I found a csv that
               should not be too difficult to process. I still need to do more research on how
               to read a csv into a shinyapp. I looked over the csv file to see which
               columns will become useful to me. I found the data on baseball savant. I feel
               that I have made good progress with my project, because I great idea of what
               I want to accomplish with my data. I also know where my data is coming from, which 
               is a nice feeling to have. I want to see if some MLB players are overlooked, because
               they have a low batting average but a high hard hit ball percentage. I found that
               the more you hit the ball hard, the better chance one has to get on base. I am passionate
               about this topic. I have a good feel for the progress that I have made thus far."),
             p(tags$a(href = "https://github.com/christophersnopek/shiny_app_1.git"))))


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

    output$mph_distribution <- renderImage({list(src = "baseball_velocity.png",
                                                 width = 800,
                                                 height = 500,
                                                 alt = "MPH Distribution")


# This was the image of Keston Hiura with not height or width argument.

    }, deleteFile = FALSE)
    output$kestonhiura <- renderImage({list(src = "khiura.png",
                                                 alt = "MPH Distribution")
        
    
# This is the same process as the Keston Hiura process.

    }, deleteFile = FALSE)
    output$evanwhite <- renderImage({list(src = "ewhite.png",
                                                 alt = "MPH Distribution")
        

# I did the same process for all of the baseball player imaging.
# It did take some time to format.
# Although, I still need to figure out the height distortion.
        
    }, deleteFile = FALSE)
    output$villar <- renderImage({list(src = "jvillar.png",
                                                 alt = "MPH Distribution")
    }, deleteFile = FALSE)
    output$blowe <- renderImage({list(src = "blowe.png",
                                       alt = "MPH Distribution")
    }, deleteFile = FALSE)
    output$dfletcher <- renderImage({list(src = "dfletcher.png",
                                       alt = "MPH Distribution")
    }, deleteFile = FALSE)
    output$kwong <- renderImage({list(src = "kwong.png",
                                       alt = "MPH Distribution")
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

# shinyApp(
#     ui = fluidPage(
#         shinythemes::themeSelector(),  # <--- Add this somewhere in the UI
#         sidebarPanel(
#             textInput("txt", "Text input:", "text here"),
#             sliderInput("slider", "Slider input:", 1, 100, 30),
#             actionButton("action", "Button"),
#             actionButton("action2", "Button2", class = "btn-primary")
#         ),
#         mainPanel(
#             tabsetPanel(
#                 tabPanel("Tab 1"),
#                 tabPanel("Tab 2")
#             )
#         )
#     ),
#     server = function(input, output) {}
# )

# Run the application 
shinyApp(ui = ui, server = server)
