library(shinydashboard)
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(V8)
library(png)
library(ggplot2)

ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Matching distributions",
                                    tags$li(class = "dropdown",
                                            tags$a(href= "https://shinyapps.science.psu.edu/",
                                                   icon("home",lib = "font-awesome"))),
                                    tags$li(class = "dropdown",
                                            actionLink("info",icon("info"),class = "myClass"))
                    ),
                    dashboardSidebar(
                      sidebarMenu(
                        id = "tabs",
                        menuItem("Overview", tabName = "overview",icon = icon("dashboard")),
                        menuItem("Challenge", tabName = "matchingdist",icon = icon("cogs"))
                      )
                    ),
                    dashboardBody(
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "Feature.css"),
                        tags$style(HTML(
                          '.popover-title{
                          color:black;
                          font-size:18px;
                          background-color: white;
                          }'
                        ))),
                      
                      tabItems( 
                        
                        #Overview Tab
                        tabItem(tabName = "overview",
                                tags$a(href='http://stat.psu.edu/',   tags$img(src='logo.png', align = "left", width = 180)),
                                br(),
                                br(),
                                br(),
                                h3(strong("About:")),
                                h4("In this App, you will gain practice at associating context with different probability distributions. "),
                                br(),
                                h3(strong("Instructions:")),
                                h4(tags$li("You'll start this game with a little man on the top of a tree,  and you are trying to prevent his fall to the ground.  If you provide a wrong answer, he falls to a lower branch and eventually to the ground. If you get 10 questions correct before he falls to the ground, you have won the game and saved the little man!")),
                                h4(tags$li("Please select which probability distribution(s) you 'd like to work on and hit the “filter” button.")),
                                h4(tags$li("Read the given text and choose a distribution from the dropdown menu. Make sure you understand the scenario.")),
                                h4(tags$li("If you need some extra help, click the 'hint' button (shown as a question mark symbol).")),
                                h4(tags$li("After you select the distribution, click 'Submit' to check your answer. ")),
                                h4(tags$li("Once you click 'Submit', you cannot revise your answer. You can only click 'Next Question' to move on to your next challenge. ")),
                               
                                
                                div(style = "text-align:center",
                                    actionButton("go", "G O !", icon("bolt"), size = "medium",style = 'color: #fff; background-color: #337ab7; border-color: #2e6da4',class="circle grow")),
                                br(),
                                h3(strong("Acknowledgements:")),
                                h4("This app was developed and coded by Zhiliang Zhang and futher updated by Yiyang Wang and Yuqing Lei.")
                                ),
                        
                        # Challenge Tab
                        tabItem(tabName = "matchingdist",
                                fluidRow(
                                  column(width=6, h4("Please select the distributions you'd like to use in this app")),
                                  br(),br(),br(),
                                  column(
                                    width = 2,
                                    dropdownButton(
                                      label = "Discrete distributions",circle = FALSE, status = "default", width='12px',
                                      fluidRow(
                                        column(
                                          width = 3,
                                          actionButton("selectAllD","Select All",size="small"),
                                          checkboxGroupInput(inputId = "discretelist", label = NULL, choices = c("Bernoulli", "Binomial", "Discrete Uniform", "Poisson", "Geometric", "Negative Binomial"))
                                        )
                                      )
                                    ),
                                    verbatimTextOutput(outputId = "res1")
                                  ),
                                  
                                  column(
                                    width = 2,
                                    dropdownButton(
                                      label = "Continuous distributions",circle = FALSE, status = "default", width = '100%',
                                      #tags$label("Pick which continuous distribution(s) to use in the app:"),
                                      fluidRow(
                                        column(
                                          width = 2,
                                          actionButton("selectAllC","Select All", size="small"),
                                          checkboxGroupInput(inputId = "continuouslist", label = NULL, choices =  c("Continuous Uniform", "Gamma", "Exponential", "Normal"), width = '100%')
                                        )
                                      )
                                    ),
                                    verbatimTextOutput(outputId = "res2")
                                  ),
                                  column(2, offset=1,
                                         bsButton('filter', "Filter", size= "large", style="warning",disabled =FALSE))),
                                
                                titlePanel("Matching the text with the distribution"),
                                sidebarLayout(
                                  sidebarPanel(
                                    wellPanel(style = "background-color: #EAF2F8",
                                              uiOutput("question"),
                                              tags$style(type='text/css', '#question {font-weight:bold;font-size: 25px;background-color: #EAF2F8;color: black;}','.well { padding: 12px; margin-bottom: 15px; max-width: 1000px; }')
                                    ),
                                    
                                    wellPanel(style = "background-color: #EAF2F8",
                                              
                                              fluidRow(
                                                column(10,
                                                       h4("Identify the distribution of given text:",
                                                          tags$li(style="display: inline-block;", circleButton("hint",icon = icon("question"), status = "myClass",size = "xs"))
                                                       ))),
                                              
                                              fluidRow(
                                                tags$style(type='text/css', ".selectize-dropdown-content {max-height: 500px; }"),
                                                column(8, 
                                                       uiOutput('answerbox'), 
                                                       selectInput('answer',"",c('Select Distribution'), width='100%')),
                                                br(),
                                                column(1, uiOutput('mark')),
                                                column(3,
                                                       bsButton('submit', "Submit", size= "large", style="warning",disabled =FALSE)),
                                                br(),br(),br(),
                                                
                                                column(4,
                                                       bsButton('nextq', "Next Question", size ="large", style="success",disabled=TRUE)),
                                                column(4,
                                                       bsButton('restart', "Restart the game", size = "large", style= "warning", disabled=FALSE)),
                                                br(), br(), br())),
                                    wellPanel(style = "background-color: #EAF2F8",
                                              fluidRow(
                                                column(width=12, 
                                                       uiOutput('feedback'))
                                              )),
                                    
                                    
                                    br(),
                                    br(),
                                    br(),
                                    
                                    tags$head(tags$style(HTML("#result {font-size: 17px;background-color:#EAF2F8}"))),
                                    
                                    width = 6),
                                  mainPanel(
                                    
                                    width = 6,
                                    
                                    fluidRow(
                                      uiOutput("correct", align = 'center')
                                    ),
                                    fluidRow(
                                      uiOutput("distPlot", align = 'center')
                                    ),
                                    br(),
                                    br(),
                                    br()
                                  ),
                                  position ="left"
                                )))))