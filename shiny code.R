#install.packages("shiny")

library(shiny)
library(shinydashboard)
library(shinythemes)
library(plotly)
library(shinycssloaders)
library(tidyverse)
library(scales)
library(knitr)
library(kableExtra)
library(ggfortify)
library(plotly)
library(dplyr)
library(FNN)
library(ggplot2)
library(shinybusy)
library(stargazer)
library(stats)
source("make_data.R")


# rmdfiles <- c("C:/Users/Rental/Documents/R/495 Models/RaptorModelsApp.rmd")
# sapply(rmdfiles, knit, quiet = T)

# ui <- fluidPage("10 Most Simliar Pernonslities - NBA",
#                  # tabPanel("Graphic",fluidPage(theme = shinytheme("flatly")),
#                           # tags$head(
#                           #   tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
#                           # pageWithSidebar(
#                           #   headerPanel('Apply filters'),
#                              sidebarPanel(width = 4,
#                                          selectInput(inputId = 'player', label = 'Choose a player:',choices = unique(data$player.name), selected = "Lebron James"))
#                                         # selectInput(inputId = 'player1', label = 'Choose a player:',choices = paste("-", data$Player), selected = "Lebron James")
#                                         
#                                          #,
# 
#                                         # checkboxGroupInput(inputId = "position",
#                                         #                     label = 'Position:', choices = c("PG" = "PG",
#                                         #                                                      "SG" = "SG", "SF" = "SF",
#                                         #                                                      "PF" = "PF", "C" = "C"),
#                                         #                     selected = c("PG"="PG"),inline=TRUE),
#                                         # #Just seeing if they interact... can delete
#                                         # numericInput("num", "Choose a number:", 5, min = 0, max= 1000),
#                                                                                 # submitButton("Update")
#                                         
#                             
#                             mainPanel(
#                               column(7, plotlyOutput("plot1", width = 800, height=700),
#                                      p("To visualize the graph of the player, click the icon at side of names 
#              in the graphic legend. It is worth noting that graphics will be overlapped.",
#                                        style = "font-size:25px")
#                                      #Check
#                                      #column(12, plotOutput("plot2", width = 500, height = 500))
#                               )
#                              )
#                           # ))
#                  #,
#                  #tabPanel("About")
# )

ui <- navbarPage("Comparison and Analysis of NBA Personalities from Twitter", theme = shinytheme("cerulean"),
                 

                  tabPanel("About", 
                           verbatimTextOutput("About"),
                           fluidPage(
                          mainPanel(width = 8,
                            tags$b("A little info about the data:", style = "font-size:30px")),
                          tags$br(),
                          tags$br(),
                          tags$br(),
                          tags$b("Players are in if they...", style= "font-size:18px"),
                          br(),
                          tags$ul(
                            tags$li(" Entered the NBA between 2003-2020", style = "fontsize:15px"),
                            tags$li(" Had a twitter account with at least 175 tweets for accuracy", style = "fontsize:15px"),
                            tags$li(" Tweeted in a language recognized by IBM Personality Insights (English, Spanish)", style = "fontsize:15px" )
                          ),
                          
                          hr(),
                          p("The Personality scores were calculated using IBM Personality Insights. They are the BIG 5 or OCEAN traits. Big Five personality characteristics represent the most widely used model
           for generally describing how a person engages with the world. The model includes five primary dimensions:", style = "fontsize:15px"),
                          tags$ul(
                            tags$li(tags$b("Agreeableness")," is a person's tendency to be compassionate and cooperative toward others.", style = "fontsize:15px"),
                            tags$li(tags$b("Conscientiousness")," is a person's tendency to act in an organized or thoughtful way.", style = "fontsize:15px"),
                            tags$li(tags$b("Extraversion"), " is a person's tendency to seek stimulation in the company of others.", style = "fontsize:15px"),
                            tags$li(tags$b("Emotional Range")," also referred to as Neuroticism or Natural reactions, is the extent to which a person's emotions are sensitive to the person's environment.", style = "fontsize:15px"),
                            tags$li(tags$b("Openness")," is the extent to which a person is open to experiencing different activities.", style = "fontsize:15px")),
                  
                          
                          fluidRow(column(width = 1, img(src='steph.png', width = 200)), 
                                   column(offset = 1,width = 1, img(src='donny.png', width = 300)),
                                   column(offset = 1, width = 1, img(src='lebron1.png', width = 310)), 
                                   column(offset = 1, width = 1, img(src = 'dame3.png', width = 300)))
                           )

                 ),
                
                  tabPanel("Graphic",fluidPage(
                   tabsetPanel(
                     tabPanel("Bar Chart",
                              
                              mainPanel(width = 10,
                                        tags$br(),
                                        tags$b("Explore NBA Personality", style = "font-size:25px"),
                                        tags$br(),
                                        tags$hr(),
                                        fluidRow(
                                          column(2,selectInput("player.bar1",
                                                    "Choose a Player",
                                                    choices = barplotdata$Player.x,
                                                    selected = "Chris Paul")
                                                 ),
                                                 
                                        column(5,selectInput("player.bar2",
                                                    "Choose a Player to Compare",
                                                    choice = barplotdata$Player.x,
                                                    selected = "Average")
                                               )
                                        )
                                        
                                        
                                ,plotOutput("barplot1")  %>% withSpinner(color="#0dc5c1")),
                              
                              # sidebarPanel(width = 2,
                              #              selectInput("player.bar1",
                              #                          "Choose a player",
                              #                          choices = barplotdata$Player.x,
                              #                          selected = "Chris Paul"),
                              #              selectInput("player.bar2",
                              #                          "Choose another player",
                              #                          choice = barplotdata$Player.x,
                              #                          selected = "Average",
                              #              )
                              # )
                     ),
                     
                     
                     tabPanel("Radar Plot",
                        
                   # sidebarPanel(width = 2,
                   #              p("Values measured as percentile within the NBA.", style = "fontsize:17px"),
                   #              br(),
                   #              p("Highlight a point to find out what percentile the player is in 
                   #                  for that personality trait.", style = "fontsize:17px"), 
                   #              br(),
                   #              p(" r: 86.0 means that player is in the 86th percentile for that 
                   #                  attribute.", style = "fontsize:17px"),
                   #              br(),
                   #              tags$b("Select a player from the list on the right to see the selected player's closest personality comps.",
                   #                     style = "fontsize:1517"),
                   #              br(),
                   #              br(),
                   #              p("A value at the center of the circle indicates 0th percentile and values at the edge of
                   #                       the circle indicate the 100th percentile.", style = "fontsize:17px")
                   # ),
                   
                   mainPanel(width = 10,  
                             tags$br(),
                              tags$b("Explore NBA Personality", style = "font-size:25px"),
                       tags$hr(),
                       
                       
                      
                        column(2, 
                        selectInput("player",
                                   "Choose a Player",
                                   choices = (nov.18$Player.x),
                                   selected = "Zion Williamson"
                                   )),
                        
                        
                       # p("The graph of the selected NBA player will appear below. To see the value of a characteristic,
                       #   hover over it with your mouse and the 'r:' will show you what percentile the player is in for that 
                       #   personality trait.",
                       #   style = "font-size:15px"),
                       # tags$p("The 10 players with the personalities most similar to the selected player appear on the right,
                       #        click on one to overlay their personality scores.", style = "font-size:15px"),
                       tags$br(),
                       tags$br(),
                       
                       plotlyOutput("plot1", width = 900, height=550) %>% withSpinner(color="#0dc5c1")
                                            
                     )
                   )

                   )
                 
                 )
                 
                 ),
                  
                 tabPanel("Data",
                          tabPanel("Personality  Data",
                                   tags$hr(),
                                   DT::dataTableOutput("table") %>% withSpinner(color="#0dc5c1")
                          )
                 ),

               
                 tabPanel("Analysis",
                          mainPanel(offset = 2, width = 8,
                                    tags$b("Introduction"),
                                    tags$br(),
                              p("To see what kind of influence player personality has on on-court production I run linear models. 
                              I chose dependent variables as outcomes based on fivethirtyeight's 
                              These statistics are derived using tracking data and are used to evaluate a player's on-court impact 
                              (https://fivethirtyeight.com/features/how-our-raptor-metric-works/).
                              Because personality scores were only taken for a player's entire career and not for each individual season, 
                              I modify the outcome variables to be the career sum and career average for RAPTOR.", style = "fontsize:15px"),
                          tags$br(),
                          
                          p("I first check to see if the assumptions of normality can be met for these variables. 
                            Here I show a Histogram and Normal Q-Q plot for average predator", style = "fontsize:15px"),
                          
                            fluidRow(splitLayout(cellWidths = c("50%", "50%"), plotOutput("hist1"),
                                         plotOutput("qqnorm1"))),
                             #fluidRow(splitLayout(cellWidths = c("50%", "50%"), plotOutput("hist3"),
                              #                    plotOutput("hist4"))),
                            
                            tags$b("About the data"),
                            p("Players were considered in the analysis if they played the NBA between the years of 2014-2019 
                            (these are the years for which fivethirtyeight has tracking data and reports data). 
                            Personality scores were recorded only for players with twitter accounts who had more than
                            175 tweets with a maximum of 1600 tweets from each player. Players tweeting in languages not recognized by IBM were excluded. 
                            To ensure an accurate outcome variable, only seasons in which a player recorded a minimum of 1000 possessions were considered.
                              Below are the model summaries for Average Raptor the 6-year span.", style = "fontsize:15px"),
                          
                          tags$br(),  
                          tags$b("Model Results"),
                          tags$br(),
                          
                          #verbatimTextOutput("modelresult") %>% withSpinner(color = "#0dc5c1")
                          verbatimTextOutput("modelsummary1"), 
                          #verbatimTextOutput("modelsummary1"),
                          br(),
                          
                          tags$b("Key finding: Conscientiousness is statistically significant with a positive effect", style = "fontsize:15px"),
                          br(),
                          br(),
                          p("Players who are more Consienctious tend to be better basketball players. Other outcome variables were tested for robustness,
                            these all yeilded very similar results.", style = "fontsize:15px"),
                        
                          tags$br(),
                          tags$br(),
                          
                          fluidRow(column(offset = 4,8, tags$b("Scatterplot of Conscientiousness and Averge Raptor"))),
                          fluidRow(column(offset = 2,8,plotOutput("scatterplot1") %>% withSpinner(color="#0dc5c1")))
                          
                          )
                        
                          
                          ),
                 # tabPanel("practice table",
                 #          mainPanel(
                 #          DT::dataTableOutput("practicetable") %>% withSpinner(color="#0dc5c1"))
                 # ),
                
                          
            
                 
                 tabPanel("Further Study",
                          mainPanel(
                            tags$p("Other potential uses for this data set:", style = "fontsize:15px"),
                            tags$ul(
                              tags$li("Instead of quality of play could style of play be predicted? This could include pace, hustle stats, shot selection, 4th quarter 'clutch' stats, or playoff stats.", style = "fontsize:15px"),
                              tags$li("What is the sum of the personalities of the whole team? Is there an effect?", style = "fontsize:15px"),
                              tags$li("Are personality traits predictive of successful relationships? (Effective pick and roll partners, strong defensive lineups, player/coach etc)", style = "fontsize:15px" ),
                              tags$hr(),
                              tags$p("For more information, contact Nate Hawkins at natehawk2@gmail.com")
                            
                            ),
                
                          )
                 )
                 
)




server <-  function(input, output, session) {
  
  library(shiny)
  library(shinydashboard)
  library(shinythemes)
  library(plotly)
  library(shinycssloaders)
  library(tidyverse)
  library(scales)
  library(knitr)
  library(kableExtra)
  library(ggfortify)
  library(plotly)
  library(dplyr)
  library(FNN)
  library(ggplot2)
  library(stargazer)
  
  #Barchart
  
  #Get player of interest
  bar1 <- reactive({
    barplotdata %>% filter(Player.x == input$player.bar1) %>% select(`Openness Percentile`, `Conscientiousness Percentile`, 
                                                               `Extroversion Percentile`, `Agreeableness Percentile`,
                                                               `Emotional Range Percentile`)
  })
  
  #vectors of their data
  vector1 <- reactive({
    c(bar1()$`Openness Percentile`, bar1()$`Conscientiousness Percentile`, bar1()$`Extroversion Percentile`,
               bar1()$`Agreeableness Percentile`, bar1()$`Emotional Range Percentile`)
  })
  
  #Player to compare
  bar2 <- reactive({
    barplotdata %>% filter(Player.x == input$player.bar2) %>% select(`Openness Percentile`, `Conscientiousness Percentile`, 
                                                               `Extroversion Percentile`, `Agreeableness Percentile`,
                                                               `Emotional Range Percentile`)
  })
  
  vector2 <- reactive({
    c(bar2()$`Openness Percentile`, bar2()$`Conscientiousness Percentile`, bar2()$`Extroversion Percentile`,
               bar2()$`Agreeableness Percentile`, bar2()$`Emotional Range Percentile`)
  })
  
  personality.vec <- reactive({
    c("Openness", "Conscientiousness", "Extroversion", "Agreeableness", "Emotional Range")
  })

  bar3 <- reactive({
    data.frame(personality.vec(), as.data.frame(vector1()), as.data.frame(vector2()))
  })

  
  #gather
   bar4 <-   reactive({
    gather(bar3(), personality.vec.., key = player, vector1..:vector2..)
   })

   output$bar4table <- DT::renderDataTable({
     DT::datatable(finalgraph.2())
   })
   

  finalgraph <- reactive({
    cbind(bar4(), personality.vec())
  })
  
  finalgraph.1 <- reactive({
    finalgraph() %>% mutate(score = personality.vec..)
  })
  
  finalgraph.2 <- reactive({
    finalgraph.1() %>% mutate(trait = `personality.vec()`)
  })
  

  output$barplot1 <- renderPlot(ggplot(data = finalgraph.2(), aes(x= trait, y=as.numeric(score), fill = player)) + 
    geom_col(position = "dodge", width = .5) +
      labs(x="Personality Trait", y = "NBA Percentile Score") +
    scale_fill_discrete(labels = c(input$player.bar1, input$player.bar2)) +
      theme(legend.title = element_blank()) +
      theme(text = element_text(size=18)) +
      scale_y_continuous(limits = c(0,100))) 
    
  
  
  
  #make personality vector and dataframe
  

  
  # #Gets everyone but the player I want
  selectedData1 <- reactive({
    nov.18 %>%
      filter(nov.18$Player.x != input$player)
  })
  
  
  #Takes the correct filters from selectedData1
  selectedData2 <- reactive({
    selectedData1() %>%
      select(4,7,33:37,31)
             #& selectedData1()$status %in% input$status)
   
    
  })
  
  #Gets the player I want with the correct columns
  selectedData3 <- reactive({
    nov.18 %>%
      select(4,7,33:37,31) %>%
      filter(nov.18$Player.x == input$player)
    
  })
  
  #Rbind my player on top
  selectedData4 <- reactive({
    rbind(selectedData3(),selectedData2())
    
  })
  
  #Get columns I'm interested in comparing
  selectedData5 <- reactive({
    selectedData4() %>%
      select(3:7)
  })
  
  #Find the top 10 matches
  selectedData6 <- reactive({
    as.numeric(knnx.index(selectedData5(), selectedData5()[1, , drop=FALSE], k=11))
  })
  
  #Make those top ten matches a dataframe
  selectedData7 <- reactive({
    selectedData4()[selectedData6(),]
  })
  
  #Take the columns of interest from the matches dataframe
  selectedData8 <- reactive({
    selectedData7() %>%
      select(3:7)
  })
  
  
  # Combine the selected variables into a new data frame
  output$plot1 <- renderPlotly({
    
    #Make sure the filters weren't too aggressive
    validate(
      need(dim(selectedData2())[1]>=10, "Sorry, no ten similar players were found. 
           Please change the input filters."
      )
    )
    
    plot_ly(
      type = 'scatterpolar',
      mode = "closest",
      fill = 'toself'
      
    ) %>%
      add_trace(
        #Gets length of each column for first match
        r = as.matrix(selectedData8()[1,]),
        #Sets direction names
        theta = c("NBA Openness Percentile", "NBA Conscientiousness Percentile", "NBA Extroversion Percentile", 
                  "NBA Agreeableness Percentile", "NBA Emotional Range Percentile"),
        showlegend = TRUE,
        mode = "markers",
        
        #Put the name of the player on the side
        name = selectedData7()[1,1]
      ) %>% layout(
          polar = list(
            radialaxis = list(
              visible = T,
              range = c(0,100)
            )
          ),

          showlegend=TRUE
        
      )
    
  })
  
  #Data Table
  output$practicetable <- DT::renderDataTable({
    DT::datatable(nov.18 %>% select(Player.x))
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(nov.19 %>% select(2,57:61,66,67) %>% arrange(desc(`Average Raptor`)), filter = "top")
  })
  
  
  
  nov.20 <- read.csv("NBA Ten Closest Player Personalities/nov.19.csv")
  raptor <- read.csv("NBA Ten Closest Player Personalities/modern_raptor.csv")
  raptor <- raptor %>% filter(poss > 1000)
  raptor <- raptor %>% group_by(Player.x) %>% mutate(sum_war = sum(war_total))
  raptor <- raptor %>% group_by(Player.x) %>% mutate(avg_war = mean(war_total))
  raptor <- raptor %>% group_by(Player.x) %>% mutate(sum_raptor = sum(raptor_total))
  raptor <- raptor %>% group_by(Player.x) %>% mutate(avg_raptor = mean(raptor_total))
  raptor <- raptor %>% group_by(Player.x) %>% mutate(sum_predator = sum(predator_total))
  raptor <- raptor %>% group_by(Player.x) %>% mutate(avg_predator = mean(predator_total))
  merge.raptor <- merge(raptor, nov.20, by.x = "Player.x")
  slice.raptor <- merge.raptor %>% group_by(Player.x) %>%  filter(row_number() == 1)
  
  #Models for raptor
  sum_raptor.glm <- glm(data = slice.raptor, formula = (sum_raptor)^(1/3) ~ Openness.percentile + Conscientiousness.percentile + 
                          Extraversion.percentile + Agreeableness.percentile + EmotionalRange.percentile)
  summary(sum_raptor.glm)
  
  avg_raptor.glm <- glm(data = slice.raptor, formula = avg_raptor ~ Openness.percentile + Conscientiousness.percentile + 
                          Extraversion.percentile + Agreeableness.percentile + EmotionalRange.percentile)
  summary(avg_raptor.glm)
  
  

  #####################
  #Models for Predator#
  #####################
  
  sum_predator.glm <- glm(data = slice.raptor, formula = (sum_predator+25)^(1/3) ~ Openness.percentile + Conscientiousness.percentile + 
                            Extraversion.percentile + Agreeableness.percentile + EmotionalRange.percentile)

  avg_predator.glm <- glm(data = slice.raptor, formula = avg_predator ~ Openness.percentile + Conscientiousness.percentile + 
                            Extraversion.percentile + Agreeableness.percentile + EmotionalRange.percentile)

  output$hist1 <- renderPlot(hist(slice.raptor$avg_raptor, breaks = 10, main = "Histogram of Average Raptor",
       xlab = "Average Raptor"))
  
  output$hist2 <- renderPlot(hist(slice.raptor$avg_predator, breaks = 10, main = "Histogram of Average Predator",
       xlab = " Average Predator"))
  
  # output$modelresult <- renderPrint(stargazer(avg_raptor.glm, avg_predator.glm, 
  #                                             sum_raptor.glm, sum_predator.glm, report = "vc*p" , type = "text",
  #                                             column.labels = c("Average Raptor", "Average Predator",
  #                                                               "Total Raptor", "Total Predator"),
  #                                             dep.var.labels = c("",""),
  #                                             title = "Linear Model Results",
  #                                             dep.var.labels.include = FALSE,
  #                                             font.size = "scriptsize"))
  
  output$modelsummary1 <- renderPrint({
    
    summary(avg_raptor.glm)
    
  })
  
  output$modelsummary2 <- renderPrint({
    
    summary(sum_raptor.glm)
    
  })
  
  
   output$hist3 <- renderPlot(hist((slice.raptor$sum_raptor+4)^(1/3), breaks = 10, xlab = c("Total Raptor ^ (1/3)"),
                                   main = "Histogram of Total Raptor (Cube Root Transformation)"))
  # 
   output$hist4 <- renderPlot(hist((slice.raptor$sum_predator+4)^(1/3), breaks = 10, xlab = "Total Predator ^ (1/3)",
                                   main = "Histogram of Total Predator (Cube Root Transformation)"))
  # 
  
  output$qqnorm1 <- renderPlot(qqnorm(raptor$avg_raptor, main = "Normal Q-Q Plot for Average Raptor"))
  output$qqnorm2 <- renderPlot(qqnorm((raptor$sum_raptor)^(1/3), main = "Normal Q-Q Plot for Total Raptor Cubed"))
  
  output$scatterplot1 <- renderPlot(ggplot(slice.raptor, aes(x=Conscientiousness.Percentile, y = avg_raptor))+
     geom_point() +
     geom_smooth(method='lm',formula=y~x) +
    labs(x="Conscientiousness", y = "Average Raptor") +
    theme(text = element_text(size=15)))
    
    
  
}



shinyApp(ui = ui, server = server)




# 



#Get player of interest
# bar1 <-   nov.18 %>% filter(Player.x == "Chris Paul") %>% select(`Openness Percentile`, `Conscientiousness Percentile`, 
#                                                               `Extroversion Percentile`, `Agreeableness Percentile`,
#                                                               `Emotional Range Percentile`)
# 
# 
# #vectors of their data
# vector1 <-   c(bar1$`Openness Percentile`, bar1$`Conscientiousness Percentile`, bar1$`Extroversion Percentile`,
#     bar1$`Agreeableness Percentile`, bar1$`Emotional Range Percentile`)
# 
# 
# #Player to compare
# bar2 <-   nov.18 %>% filter(Player.x == "Kyrie Irving") %>% select(`Openness Percentile`, `Conscientiousness Percentile`, 
#                                                               `Extroversion Percentile`, `Agreeableness Percentile`,
#                                                               `Emotional Range Percentile`)
# 
# 
# vector2 <-   c(bar2$`Openness Percentile`, bar2$`Conscientiousness Percentile`, bar2$`Extroversion Percentile`,
#     bar2$`Agreeableness Percentile`, bar2$`Emotional Range Percentile`)
# 
# 
# personality.vec <-   c("Openness", "Conscientiousness", "Extroversion", "Agreeableness", "Emotional Range")
#   
# 
# 
# bar3 <-   data.frame(personality.vec, vector1, vector2)
# 
# 
# #gather
# bar4 <-   gather(bar3, yes, vector1:vector2)
# 
# 
# 
# finalgraph <-   cbind(bar4, personality.vec)
# finalgraph



####################################
## How to do the whole comparison ##
####################################

# selectedData1 <- reactive({
#   nov.18 %>%
#     filter(nov.18$Player.x != input$player)
# })
# 
# 
# #Takes the correct filters from selectedData1
# selectedData2 <- reactive({
#   selectedData1() %>%
#     select(4,7,33:37,31) %>%
#     filter(selectedData1()$Pos %in% input$position )
#   #& selectedData1()$status %in% input$status)
#   
#   
# })
# 
# #Gets the player I want with the correct columns
# selectedData3 <- reactive({
#   nov.18 %>%
#     select(4,7,33:37,31) %>%
#     filter(nov.18$Player.x == input$player)
#   
# })
# 
# #Rbind my player on top
# selectedData4 <- reactive({
#   rbind(selectedData3(),selectedData2())
#   
# })
# 
# #Get columns I'm interested in comparing
# selectedData5 <- reactive({
#   selectedData4() %>%
#     select(3:7)
# })
# 
# #Find the top 10 matches
# selectedData6 <- reactive({
#   as.numeric(knnx.index(selectedData5(), selectedData5()[1, , drop=FALSE], k=11))
# })
# 
# #Make those top ten matches a dataframe
# selectedData7 <- reactive({
#   selectedData4()[selectedData6(),]
# })
# 
# #Take the columns of interest from the matches dataframe
# selectedData8 <- reactive({
#   selectedData7() %>%
#     select(3:7)
# })
# 
# 
# # Combine the selected variables into a new data frame
# output$plot1 <- renderPlotly({
#   
#   #Make sure the filters weren't too aggressive
#   validate(
#     need(dim(selectedData2())[1]>=10, "Sorry, no ten similar players were found. 
#            Please change the input filters."
#     )
#   )
#   
#   plot_ly(
#     type = 'scatterpolar',
#     mode = "closest",
#     fill = 'toself'
#     
#   ) %>%
#     add_trace(
#       #Gets length of each column for first match
#       r = as.matrix(selectedData8()[1,]),
#       #Sets direction names
#       theta = c("NBA Openness Percentile", "NBA Conscientiousness Percentile", "NBA Extroversion Percentile", 
#                 "NBA Agreeableness Percentile", "NBA Emotional Range Percentile"),
#       showlegend = TRUE,
#       mode = "markers",
#       
#       #Put the name of the player on the side
#       name = selectedData7()[1,1]
#     ) %>%
#     add_trace(
#       r = as.matrix(selectedData8()[2,]), 
#       theta = c("NBA Openness Percentile", "NBA Conscientiousness Percentile", "NBA Extroversion Percentile", 
#                 "NBA Agreeableness Percentile", "NBA Emotional Range Percentile"),
#       showlegend = TRUE,
#       mode = "markers",
#       visible="legendonly",
#       name = selectedData7()[2,1]
#       
#     ) %>%
#     add_trace(
#       r = as.matrix(selectedData8()[3,]),
#       theta = c("NBA Openness Percentile", "NBA Conscientiousness Percentile", 
#                 "NBA Extroversion Percentile", "NBA Agreeableness Percentile", "NBA Emotional Range Percentile"),
#       showlegend = TRUE,
#       mode = "markers",
#       visible="legendonly",
#       name = selectedData7()[3,1]
#     ) %>%
#     add_trace(
#       r = as.matrix(selectedData8()[4,]),
#       theta = c("NBA Openness Percentile", "NBA Conscientiousness Percentile", 
#                 "NBA Extroversion Percentile", "NBA Agreeableness Percentile", "NBA Emotional Range Percentile"),
#       showlegend = TRUE,
#       mode = "markers",
#       visible="legendonly",
#       name = selectedData7()[4,1]
#     ) %>%
#     add_trace(
#       r = as.matrix(selectedData8()[5,]),
#       theta = c("NBA Openness Percentile", "NBA Conscientiousness Percentile", 
#                 "NBA Extroversion Percentile", "NBA Agreeableness Percentile", "NBA Emotional Range Percentile"),
#       showlegend = TRUE,
#       mode = "markers",
#       visible="legendonly",
#       name = selectedData7()[5,1]
#     ) %>%
#     add_trace(
#       r = as.matrix(selectedData8()[6,]),
#       theta = c("NBA Openness Percentile", "NBA Conscientiousness Percentile", 
#                 "NBA Extroversion Percentile", "NBA Agreeableness Percentile"
#                 , "NBA Emotional Range Percentile"),
#       showlegend = TRUE,
#       mode = "markers",
#       visible="legendonly",
#       name = selectedData7()[6,1]
#     ) %>%
#     add_trace(
#       r = as.matrix(selectedData8()[7,]),
#       theta = c("NBA Openness Percentile", "NBA Conscientiousness Percentile", 
#                 "NBA Extroversion Percentile", "NBA Agreeableness Percentile", "NBA Emotional Range Percentile"),
#       showlegend = TRUE,
#       mode = "markers",
#       visible="legendonly",
#       name = selectedData7()[7,1]
#     ) %>%
#     add_trace(
#       r = as.matrix(selectedData8()[8,]),
#       theta = c("NBA Openness Percentile", "NBA Conscientiousness Percentile", 
#                 "NBA Extroversion Percentile", "NBA Agreeableness Percentile", "NBA Emotional Range Percentile"),
#       showlegend = TRUE,
#       mode = "markers",
#       visible="legendonly",
#       name = selectedData7()[8,1]
#     ) %>%
#     add_trace(
#       r = as.matrix(selectedData8()[9,]),
#       theta = c("NBA Openness Percentile", "NBA Conscientiousness Percentile", "NBA Extroversion Percentile", "NBA Agreeableness Percentile", "NBA Emotional Range Percentile"),
#       showlegend = TRUE,
#       mode = "markers",
#       visible="legendonly",
#       name = selectedData7()[9,1]
#     ) %>%
#     add_trace(
#       r = as.matrix(selectedData8()[10,]),
#       theta = c("NBA Openness Percentile", "NBA Conscientiousness Percentile", "NBA Extroversion Percentile", "NBA Agreeableness Percentile", "NBA Emotional Range Percentile"),
#       showlegend = TRUE,
#       mode = "markers",
#       visible="legendonly",
#       name = selectedData7()[10,1]
#     ) %>%
#     add_trace(
#       r = as.matrix(selectedData8()[11,]),
#       theta = c("NBA Openness Percentile", "NBA Conscientiousness Percentile", "NBA Extroversion Percentile", "NBA Agreeableness Percentile", "NBA Emotional Range Percentile"),
#       showlegend = TRUE,
#       mode = "markers",
#       visible="legendonly",
#       name = selectedData7()[11,1]
#     ) %>%
#     layout(
#       polar = list(
#         radialaxis = list(
#           visible = T,
#           range = c(0,100)
#         )
#       ),
#       
#       showlegend=TRUE
#       
#       
#     )
#   
# })
