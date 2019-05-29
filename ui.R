library(shinydashboard)
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)

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
                          background-color: #337ab7
                          }'
                        ))
                        
                        ),
                      
                      tabItems( 
                        
                        #Overview Tab
                        tabItem(tabName = "overview",
                                tags$a(href='http://stat.psu.edu/',   tags$img(src='logo.png', align = "left", width = 180)),
                                br(),
                                br(),
                                br(),
                                h3(strong("About:")),
                                h4("In this App, you will explore the difference between different distributions. You need to choose the best distribution that fits the given
                                   scenario."),
                                br(),
                                h3(strong("Instructions:")),
                                h4(tags$li("You'll start this game with nothing on the gallows, if you provide a wrong answer, a part of the body will be drawn, and if the whole little man is completely drawn, then you have lost this game.")),
                                h4(tags$li("Read the given text before you make your choice. Make sure you understand the scenario text provided.")),
                                h4(tags$li("If you need some extra help, click the 'hint'.")),
                                h4(tags$li("After you select the distribution, click 'Submit' to check your answer.")),
                                h4(tags$li("Once you click 'Submit', you cannot revise your answer. You can only click 'Next Question' to move on your challenge.")),
                                
                                div(style = "text-align:center",
                                    bsButton("go", "G O !", icon("bolt"), size = "medium",style = "primary")),
                                br(),
                                h3(strong("Acknowledgements:")),
                                h4("This app was developed and coded by Zhiliang Zhang.")
                                ),
                        
                        
                        # Challenge Tab
                        
                        tabItem(tabName = "matchingdist",
                                # div(style="display: inline-block;vertical-align:top;",
                                #     tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 19))
                                # ),
                                # div(style="display: inline-block;vertical-align:top;",
                                #     circleButton("hint",icon = icon("info"), status = "myClass",size = "xs")
                                # ),
                                titlePanel("Matching the text with the distribution"),
                                sidebarLayout(
                                  sidebarPanel(
                                    wellPanel(style = "background-color: #EAF2F8",
                                              
                                              uiOutput("question"),
                                              tags$style(type='text/css', '#question {font-weight:bold;font-size: 20px;background-color: #EAF2F8;color: black;}','.well { padding: 12px; margin-bottom: 15px; max-width: 1000px; }')
                                              
                                    ),
                                    
                                    # div(style="display: inline-block;vertical-align:top;",
                                    #     circleButton("hint",icon = icon("question",class = "glyphicon glyphicon-question-sign"), size = "xs")
                                    # ),
                                    
                                    #br(),
                                    
                                    ###Change the icon color###
                                    
                                    #tags$head(tags$style(HTML("#hint {font-weight:bold;font-size:19px;color:blue}"))),
                                    
                                    #br(),
                                    #div(style="display: inline-block;vertical-align:top;",
                                    #    tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 19))
                                    #),
                                    #div(style="display: inline-block;vertical-align:top;",
                                    #    circleButton("inshint",icon = icon("info"), status = "myClass",size = "xs")
                                    #),
                                    #div(style="display: inline-block;vertical-align:top;",
                                    #    circleButton("hint",icon = icon("question"), status = "myClass",size = "xs")
                                    #),
                                    
                                    #wellPanel(style = "background-color:#EAF2F8",
                                     #         fluidRow(
                                     #           uiOutput("result")
                                     #         )),
                                    
                                    fluidRow(
                                      h3("Identify the distribution of given text:")
                                    ),
                                    
                                    div(style="display: inline-block;vertical-align:top;",
                                        circleButton("hint",icon = icon("question"), status = "myClass",size = "xs")
                                    ),
                                    
                                    fluidRow(uiOutput('answerbox'),selectInput('answer',"",c('Select Distribution','Bernoulli','Binomial','Continuous Uniform','Discrete Uniform','Exponential','Gamma','Geometric','Negative Binomial',
                                                                                             'Normal','Poisson'), width='100%'),
                                             uiOutput('mark')),
                                    
                                    
                                    br(),
                                    br(),
                                    br(),
                                    
                                    tags$head(tags$style(HTML("#result {font-size: 17px;background-color:#EAF2F8}"))),
                                    
                                    
                                  width = 6),
                                  mainPanel(
                                    br(),
                                    width = 6,
                                    
                                    fluidRow(
                                      uiOutput("correct", align = 'center')
                                    ),
                                    
                                    br(),
                                    br(),
                                    
                                    fluidRow(
                                             uiOutput("distPlot", align = 'center')
                                    ),
                                    br(),
                                    br(),
                                    br(),
                                    
                                    fluidRow(
                                      column(3, offset=2,
                                             bsButton('nextq', "Next Question", size ="large", style="success",disabled=TRUE)),
                                      column(3,
                                             bsButton('submit', "Submit", size= "large", style ="warning", disabled =FALSE)))
                                    # bsPopover("distPlot", " ","Choose different measure of association for each numeric value, then click Submit to check your answer", place="left")
                                    
                                    
                                  ),
                                  position ="left"
                                  
                                )
                                
                                
                                ####Previous Layout####
                                
                                # wellPanel(
                                #   
                                #   fluidRow(
                                #     h3("Identify the measure association of the following numeric values: ")
                                #   ),
                                # 
                                #   column(3,
                                #         selectInput('first',uiOutput('box1'),c('Relative Risk', 'Risk', 'Odds Ratio', "Probability")),
                                #         uiOutput('mark1')),
                                #   column(3, 
                                #          selectInput('second',uiOutput('box2'),c('Relative Risk', 'Risk', 'Odds Ratio', "Probability")),
                                #          uiOutput('mark2')),
                                #   column(3,
                                #          selectInput('third',uiOutput('box3'),c('Relative Risk', 'Risk', 'Odds Ratio', "Probability")),
                                #          uiOutput('mark3') ),
                                #   column(3,
                                #          selectInput('fourth',uiOutput('box4'),c('Relative Risk', 'Risk', 'Odds Ratio', "Probability")),
                                #          uiOutput('mark4')),
                                #   # fluidRow(
                                #   #   column(4, offset= 7,
                                #   #    verbatimTextOutput("result")     
                                #   #   )
                                #   # ),
                                #     
                                # fluidRow(
                                # column(3, offset=7,
                                #          verbatimTextOutput("result"))),
                                # 
                                # plotOutput("distPlot",width = "50%"),
                                # 
                                # 
                                # column(2, offset=6,
                                #        bsButton('nextq', "Next Question", size ="large", style="success",disabled=TRUE)),
                                # column(2, offset = 6,
                                #        bsButton('submit', "Submit", size= "large", style ="warning", disabled =FALSE)),
                                # 
                                # tags$head(tags$style(HTML("#result {font-size: 18px;background-color:white}"))),
                                # 
                                # bsPopover("disPlot", " ","Choose different measure of association for each numeric value, then click Submit to check your answer", place="right"),
                                # 
                                # br(),
                                # br(),
                                # br(),
                                # br(),
                                # br()
                                # 
                                # 
                                # 
                                # )
                                
                                
                        )
                        
                      )
                      
                        )
                    
                        )




