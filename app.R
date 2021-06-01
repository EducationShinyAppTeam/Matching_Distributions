library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(shinydashboard)
library(boastUtils)

APP_TITLE <<- "Matching Distributions"
MAX_TRIES <- 4
WIN_STATE <- 10
GAME_OVER <- FALSE

# source("global.R")
numberRow <- numeric()
hint <- c()
correct_answer <- c()
bank <- read.csv("distributionG.csv")
bank <- data.frame(lapply(bank, as.character), stringsAsFactors = FALSE)

# Define UI for App ----
ui <- list(
  ## Create the app page ----
  dashboardPage(
    skin = "blue",
    ### Create the app header ----
    dashboardHeader(
      title = "Matching Distributions",
      titleWidth = 300,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Matching_Distributions")
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/',
               icon("home")
        )
      )
    ),
    
    ### Create the sidebar/left navigation menu ----
    dashboardSidebar(sidebarMenu(
      id = "pages",
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
      menuItem("Challenge", tabName = "challenge", icon = icon("cogs")), 
      menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )),
    
    ### Create the content ----
    dashboardBody(
      withMathJax(),
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "Feature.css"),
        tags$style(
          HTML(
            ".popover-title{
                            color:black;
                            font-size:18px;
                            background-color: white;
                            }"
          )
        )
      ),
      
      #### Set up the Overview Page ----
      tabItems(
        # Overview Tab
        tabItem(
          tabName = "overview",
          h3("Matching Distributions"),
          p(
            "In this App, you will gain practice at associating context with 
            different probability distributions. "),
          br(),
          h3("Instructions:"),
          tags$ul(
            tags$li(
              "You'll start this game with a little man on the top of a tree, 
              and you are trying to prevent his fall to the ground.  If you 
              provide a wrong answer, he falls to a lower branch and eventually 
              to the ground. If you get 10 questions correct before he falls to 
              the ground, you have won the game and saved the little man!"
            ),
            tags$li(
              "Please select which probability distribution(s) you'd like to 
              work on and hit the “filter” button."
            ),
            tags$li(
              "Read the given text and choose a distribution from the dropdown 
              menu. Make sure you understand the scenario."
            ),
            tags$li(
              "If you need some extra help, click the 'hint' button (shown as a 
              question mark symbol)."
            ),
            tags$li(
              "After you select the distribution, click 'Submit' to check your 
              answer. "
            ),
            tags$li(
              "Once you click 'Submit', you cannot revise your answer. You can 
              only click 'Next Question' to move on to your next challenge. "
            ), 
            tags$li(
              "Before checking out the challenge page, be sure to review your 
              knowledge on the prerequisites page!"
            )
          ),
          
          ##### Go Button--location will depend on your goals ----
          div(
            style = "text-align:center",
            actionButton(
              inputId = "go",
              label = "GO!",
              icon("book"),
              size = "large"
            )
          ),
          br(),
          h3("Acknowledgements:"),
          p(
            "This app was developed and coded by Zhiliang Zhang and futher 
            updated by Yiyang Wang, Yuqing Lei and Shravani Samala in 2021."
          )
        ),
  
        #### Set up the Prerequisites Page ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          h2("Prerequisites"),
          p("In order to get the most out of this app, please review the
            following:"),
          tags$ul(
            tags$li("Learn the difference between discrete distributions and 
                    continuous distributions."),
            tags$li("Learn what types of situations each probability distribution
                    is used for."),
            tags$li(
              "Review each distribution's parameters, pmf and/or cdf, mean, 
                    variance, and moment-generating function in the ",
              tags$a(
                href = "https://psu-eberly.shinyapps.io/probability_applications/",
                "Probability Applications", class = "bodylinks"), "app's prerequisite page!")
          ),
          
          br(), 
          br(), 
          tags$b("Discrete Distributions: ", 
                 style = "font-size:20px;"), "Data can only take on certain values 
                                              in a discrete set such as integers.",
          br(), 
          br(), 
          
          fluidRow(
            box(
              title = strong("Bernoulli"),
              status = "primary",
              width = 6, 
              collapsible = TRUE,
              collapsed = TRUE,
              tags$li(tags$b("Model: "), "Values can only be 0 or 1."), 
              tags$li(tags$b("Example: "), "Number of “heads” in one toss of a coin.")
            ),
            
            box(
              title = strong("Binomial"),
              status = "primary",
              width = 6, 
              collapsible = TRUE,
              collapsed = TRUE,
              tags$li(tags$b("Model: "), "Number of successes in a fixed number 
                                          of independent trials."), 
              tags$li(tags$b("Example: "), "Number of “heads” in ten tosses of 
                                            a coin.")
            ), 
          ), 
          
          fluidRow(
            box(
              title = strong("Discrete Uniform"),
              status = "primary",
              width = 6, 
              collapsible = TRUE,
              collapsed = TRUE,
              tags$li(tags$b("Model: "), "Equally likely individual outcomes."), 
              tags$li(tags$b("Example: "), "The number that comes up when a die is rolled.")
            ), 
            
            box(
              title = strong("Geometric"),
              status = "primary",
              width = 6, 
              collapsible = TRUE,
              collapsed = TRUE,
              tags$li(tags$b("Model: "), "Number of independent trials until first success."), 
              tags$li(tags$b("Example: "), "Number of tosses until you get a “heads”.")
            ), 
          ),
            
          fluidRow(  
            box(
              title = strong("Hypergeometric"),
              status = "primary",
              width = 6, 
              collapsible = TRUE,
              collapsed = TRUE,
              tags$li(tags$b("Model: "), "Randomly select ", 
                      tags$em("n"), "items without replacement from ",
                      tags$em("N"), "items. There are ", tags$em("m"), 
                      "of type 1 items and ", tags$em("N-m"), 
                      "of type 2 items"),
              tags$li(tags$b("Example: "), "A bowl contains 30 red marbles and 
                      60 black marbles. You randomly choose 15 marbles from the 
                      bowl. What is the probability that the number of blue 
                      marbles chose in the 15 marble sample is X.")
            ), 
            
            box(
              title = strong("Negative Binomial"),
              status = "primary",
              width = 6, 
              collapsible = TRUE,
              collapsed = TRUE,
              tags$li(tags$b("Model: "), "Number of independent trials until the kth success."), 
              tags$li(tags$b("Example: "), "Number of tosses until you get ten “heads”.")
            ), 
          ),
          
          fluidRow(
            box(
              title = strong("Poisson"),
              status = "primary",
              width = 6, 
              collapsible = TRUE,
              collapsed = TRUE,
              tags$li(tags$b("Model: "), "Count of rare events in a fixed time or space."), 
              tags$li(tags$b("Example: "), "Number of fatal scuba diving accidents in 
                                            Australia next year.")
            ), 
          ),
          
          br(), 
          br(), 
          tags$b("Continuous Distributions: ", 
                 style = "font-size:20px;"), "Can take on values from a continuim.",
          br(), 
          br(), 
          
          fluidRow(
            box(
              title = strong("Beta"),
              status = "primary",
              width = 6, 
              collapsible = TRUE,
              collapsed = TRUE,
              tags$li(tags$b("Model: "), "Specific rank out of fixed number of independent 
                                          standard uniforms."), 
              tags$li(tags$b("Example: "), "The median of five trials for the proportion 
                                            of the way around a circle that a spinner lands.")
            ), 
            
            box(
              title = strong("Continuous Uniform"),
              status = "primary",
              width = 6, 
              collapsible = TRUE,
              collapsed = TRUE,
              tags$li(tags$b("Model: "), "Probability spread evenly over a range."), 
              tags$li(tags$b("Example: "), "The proportion of the way around a 
                                            circle for the angle that a spinner lands.")
            ), 
          ), 
          
          fluidRow(
            box(
              title = strong("Exponential"),
              status = "primary",
              width = 6, 
              collapsible = TRUE,
              collapsed = TRUE,
              tags$li(tags$b("Model: "), "Time to next independent rare event."), 
              tags$li(tags$b("Example: "), "How long until the next fatal scuba 
                                            diving accident in Australia.")
            ), 
            
            box(
              title = strong("Gamma"),
              status = "primary",
              width = 6, 
              collapsible = TRUE,
              collapsed = TRUE,
              tags$li(tags$b("Model: "), "Time until kth rare independent event."), 
              tags$li(tags$b("Example: "), "How long until there are ten fata 
                                            scuba diving accidents in Australia.")
            ), 
          ),
          
          fluidRow(
            box(
              title = strong("Normal"),
              status = "primary",
              width = 6, 
              collapsible = TRUE,
              collapsed = TRUE,
              tags$li(tags$b("Model: "), "Sums or averages from a large sample."), 
              tags$li(tags$b("Example: "), "Average income for a random sample 
                                            of 1000 people.")
            ), 
          ),
          
          ##### Go Button--location will depend on your goals ----
          div(
            style = "text-align:center",
            actionButton(
              inputId = "go2",
              label = "GO!",
              icon("cogs"),
              size = "large"
            )
          ),
        ),
        
        #### Challenge Tab
        tabItem(
          tabName = "challenge",
          fluidRow(
            column(
              width = 6,
              p("Please select the distributions you'd like to use in this app 
                and click Filter")
            ),
            br(),
            br(),
            br(),
            column(
              width = 2,
              dropdownButton(
                label = "Discrete distributions",
                circle = FALSE,
                status = "default",
                width = "100%",
                tags$div(
                  actionButton(
                    inputId = "selectAllD", 
                    label = "Unselect",
                    size ="small"
                  ),
                  checkboxGroupInput(
                    inputId = "discretelist",
                    label = NULL,
                    choices = c(
                      "Bernoulli",
                      "Binomial",
                      "Discrete Uniform",
                      "Poisson",
                      "Geometric",
                      "Negative Binomial",
                      "Hypergeometric"
                    ),
                    selected = c(
                      "Bernoulli",
                      "Binomial",
                      "Discrete Uniform",
                      "Poisson",
                      "Geometric",
                      "Negative Binomial", 
                      "Hypergeometric"
                    )
                  )
                )
              ),
              verbatimTextOutput(outputId = "res1")
            ),
  
            column(
              width = 2,
              dropdownButton(
                label = "Continuous distributions",
                circle = FALSE,
                status = "default",
                width = "100%",
                # tags$label("Pick which continuous distribution(s) to use in the app:"),
                tags$div(
                  actionButton("selectAllC", "Unselect",
                    size =
                      "small"
                  ),
                  checkboxGroupInput(
                    inputId = "continuouslist",
                    label = NULL,
                    choices = c("Continuous Uniform", 
                                "Gamma", 
                                "Exponential", 
                                "Normal", 
                                "Beta"),
                    selected = c("Continuous Uniform", 
                                 "Gamma", 
                                 "Exponential", 
                                 "Normal", 
                                 "Beta"),
                  )
                )
              ),
              verbatimTextOutput(outputId = "res2")
            ),
            column(
              2,
              offset = 1,
              bsButton(
                inputId = "filter",
                label = "Filter",
                size = "large",
                style = "warning",
                disabled = FALSE
              )
            )
          ),
  
          titlePanel("Matching the text with the distribution"),
          sidebarLayout(
            sidebarPanel(
              wellPanel(
                style = "background-color: #EAF2F8",
                withMathJax(uiOutput("question")),
                tags$style(
                  type = "text/css",
                  "#question {font-size: 125%;background-color: #EAF2F8;color: black;}",
                  ".well { padding: 12px; margin-bottom: 15px; max-width: 1000px; }"
                )
              ),
  
              wellPanel(
                style = "background-color: #EAF2F8",
  
                fluidRow(column(
                  10,
                  p(
                    "Identify the distribution of given text:",
                    tags$li(
                      style = "display: inline-block;",
                      bsButton(
                        inputId = "hint",
                        label = "hint", 
                        icon = icon("question"),
                        size = "large", 
                        disabled = TRUE
                      )
                    )
                  )
                )),
  
                fluidRow(
                  tags$style(type = "text/css", ".selectize-dropdown-content {max-height: 500px; }"),
                  column(
                    8,
                    uiOutput("answerbox"),
                    selectInput(
                      inputId = "answer", 
                      choices = "", 
                      c("Select Distribution"),
                      width = "100%"
                    )
                  ),
                  br(),
                  column(1, uiOutput("mark")),
                  column(
                    3,
                    bsButton(
                      inputId = "submit",
                      label = "Submit",
                      size = "large",
                      style = "warning",
                      disabled = FALSE
                    )
                  ),
                  br(),
                  br(),
                  br(),
  
                  column(
                    4,
                    bsButton(
                      inputId = "nextq",
                      label = "Next Question",
                      size = "large",
                      style = "success",
                      disabled = TRUE
                    )
                  ),
                  column(
                    4,
                    bsButton(
                      inputId = "restart",
                      label = "Restart the game",
                      size = "large",
                      style = "warning",
                      disabled = FALSE
                    )
                  ),
                  br(),
                  br(),
                  br(),
                  uiOutput("test1"),
                  uiOutput("test2")
                )
              ),
              wellPanel(
                style = "background-color: #EAF2F8",
                fluidRow(column(
                  width = 12,
                  uiOutput("feedback")
                ))
              ),
  
  
              br(),
              br(),
              br(),
  
              tags$head(tags$style(
                HTML("#result {font-size: 17px;background-color:#EAF2F8}")
              )),
              width = 6
            ),
            mainPanel(
              width = 6,
  
              fluidRow(uiOutput("correct", align = "center")),
              fluidRow(uiOutput("distPlot", align = "center")),
              br(),
              br(),
              br()
            ),
            position = "left"
          ), 
        ), 
        
        #### Set up the References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
            "Attali, D. (2020). shinyjs: Easily Improve the User Experience of
              Your Shiny Apps in Seconds. R package version 1.1. Available from
              https://CRAN.R-project.org/package=shinyjs"
          ),
          p(
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
            (v0.61). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Carey, R. and Hatfield, N. (2020). boastUtils: BOAST Utilities. 
            R package version 0.1.6.3. Available from 
            https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Chang, W. and Borges Ribeiro, B. (2018). shinydashboard: 
            Create Dashboards with 'Shiny'. R package version 0.7.1. Available 
            from https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J. 
            (2020). shiny: Web Application Framework for R. R package version 
            1.5.0. Available from https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Introduction to STAT 414.” PennState Eberly College of Science, 
            online.stat.psu.edu/stat414/lesson/introduction-stat-414." 
          ),
          p(
            class = "hangingindent",
            "Perrier, V., Meyer, F., and Granjon, D. (2020). shinyWidgets: 
            Custom Inputs Widgets for Shiny. R package version 0.5.3. Available 
            from https://CRAN.R-project.org/package=shinyWidgets"
          ),
        
        
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()  
          )
        )
      )
    )
  )


# Define server logic ----
server <- function(session, input, output) {

  # Learning Locker Statement Generation
  .generateStatement <- function(session, verb = NA, object = NA, description = NA, value = NA) {
    if (is.na(object)) {
      object <- paste0("#shiny-tab-", session$input$pages)
    } else {
      object <- paste0("#", object)
    }

    stmt <- list(
      verb = verb,
      object = list(
        id = paste0(boastUtils::getCurrentAddress(session), object),
        name = paste0(APP_TITLE),
        description = description
      )
    )

    if (!is.na(value)) {
      stmt$result <- list(
        response = paste(value)
      )
    }

    statement <- rlocker::createStatement(stmt)
    response <- rlocker::store(session, statement)

    return(response)
  }

  .generateAnsweredStatement <- 
    function(session, 
             verb = NA, 
             object = NA, 
             description = NA, 
             interactionType = NA, 
             response = NA, 
             success = NA, 
             completion = FALSE) {
    statement <- rlocker::createStatement(list(
      verb = verb,
      object = list(
        id = paste0(getCurrentAddress(session), "#", object),
        name = paste0(APP_TITLE),
        description = paste0("Identify the distribution of given text: ", description),
        interactionType = interactionType
      ),
      result = list(
        success = success,
        response = response,
        completion = completion
      )
    ))

    return(rlocker::store(session, statement))
  }

  observeEvent(input$info, {
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "In this app the user is challenged to identify 
      the probability model for various contextual situations.",
      type = "info"
    )
  })

  ## Go button overview to prerequisites----
  observeEvent(input$go,{
    updateTabItems(
      session = session,
      inputId = "pages",
      selected = "prerequisites")
  })
  
  ## Go button prerequisites to challenge----
  observeEvent(input$go2,{
    updateTabItems(
      session = session,
      inputId = "pages",
      selected = "challenge")
  })

  
  observeEvent(input$submit, {
    updateButton(
      session = session, 
      inputId = "nextq", 
      disabled = FALSE)
    updateButton(
      session = session, 
      inputId = "hint", 
      disabled = FALSE)
  })

  observeEvent(input$submit, {
    updateButton(
      session = session, 
      inputId = "submit", 
      disabled = TRUE)
    updateButton(
      session = session, 
      inputId = "hint", 
      disabled = TRUE)
    
  })

  observeEvent(input$nextq, {
    updateButton(
      session = session, 
      inputId = "submit", 
      disabled = FALSE)
    updateButton(
      session = session, 
      inputId = "nextq", 
      disabled = TRUE)
    updateButton(
      session = session, 
      inputId = "hint", 
      disabled = FALSE)
    
  })

  observeEvent(input$restart, {
    updateButton(
      session = session, 
      inputId = "submit", 
      disabled = TRUE)
    updateButton(
      session = session, 
      inputId = "restart", 
      disabled = FALSE)
    updateButton(
      session = session, 
      inputId = "filter", 
      disabled = FALSE)
    updateButton(
      session = session, 
      inputId = "hint", 
      disabled = TRUE)

    GAME_OVER <<- FALSE
    .generateStatement(
      session = session, 
      object = "restart", 
      verb = "interacted", 
      description = "Game has been restarted.")

    output$question <- renderUI({
      withMathJax()
      return(NULL)
      output$test1 <- renderUI({
        withMathJax()
      })
      output$test2 <- renderUI({
        withMathJax()
      })
    })

    output$feedback <- renderUI({
      withMathJax()
      return(NULL)
      output$test1 <- renderUI({
        withMathJax()
      })
      output$test2 <- renderUI({
        withMathJax()
      })
    })
    observeEvent(input$hint, {
      p("Please select the distribution")
    })

    updateCheckboxGroupInput(
      session = session, 
      inputId = "discretelist", 
      label = NULL, 
      choices = c(
        "Bernoulli", 
        "Binomial", 
        "Discrete Uniform", 
        "Poisson", 
        "Geometric", 
        "Negative Binomial"), 
      selected = c(
        "Bernoulli", 
        "Binomial", 
        "Discrete Uniform", 
        "Poisson", 
        "Geometric", 
        "Negative Binomial"))
    
    updateCheckboxGroupInput(
      session = session, 
      inputId = "continuouslist", 
      label = NULL, 
      choices = c(
        "Continuous Uniform", 
        "Gamma", 
        "Exponential", 
        "Normal", 
        "Beta"), 
      selected = c(
        "Continuous Uniform", 
        "Gamma", 
        "Exponential", 
        "Normal", 
        "Beta"))

    updateButton(
      session = session, 
      inputId = "selectAllD", 
      label = "Unselect")
    
    updateButton(
      session = session, 
      inputId = "selectAllC", 
      label = "Unselect")
    
    updateCheckboxGroupInput(
      session, 
      inputId = "discretelist", 
      label = NULL, 
      choices = c(
        "Bernoulli", 
        "Binomial", 
        "Discrete Uniform", 
        "Poisson", 
        "Geometric", 
        "Negative Binomial"), 
      selected = c(
        "Bernoulli", 
        "Binomial", 
        "Discrete Uniform", 
        "Poisson", 
        "Geometric", 
        "Negative Binomial"))
    
    updateCheckboxGroupInput(
      session = session, 
      inputId = "continuouslist", 
      label = NULL, 
      choices = c(
        "Continuous Uniform", 
        "Gamma", 
        "Exponential", 
        "Normal", 
        "Beta"), 
      selected = c(
        "Continuous Uniform", 
        "Gamma", 
        "Exponential", 
        "Normal", 
        "Beta"))

    updateSelectInput(
      session = session, 
      inputId = "answer", 
      choices = "", 
      c("Select Distribution"))

    output$mark <- renderUI({
      img(src = NULL, width = 30)
    })
    
    id <<- 0
    numberRow <<- numeric()
    value[["mistake"]] <<- 0
    # print(value[["mistake"]])
    value$correct <<- 0
  })

  output$result <- renderUI({
    withMathJax()
    p("Choose the distribution from the list to match the given text, then click
      'Submit' to check your answer.")
  })

  value <- reactiveValues(index = 1, mistake = 0, correct = 0)
  correct_answer <- as.matrix(bank[1:91, 4])
  hint <- as.matrix(bank[1:91, 6])
  index_list <- reactiveValues(list = sample(2:91, 89, replace = FALSE))
  feedbacks <- as.matrix(bank[1:91, 7])

  observeEvent(input$go, {
    value$index <- 1
    value$answerbox <- value$index
  })

  output$correct <- renderUI({
    withMathJax()
    h3("Number of correct answers:", "", value$correct)
  })

  ####### select AllDiscrete#####
  observeEvent(input$selectAllD, {
    if (input$selectAllD == 0) {
      updateButton(
        session = session, 
        inputId = "selectAllD", 
        label = "Unselect")
      updateCheckboxGroupInput(
        session, 
        "discretelist", 
        choices = c(
          "Bernoulli", 
          "Binomial", 
          "Discrete Uniform", 
          "Poisson", 
          "Geometric", 
          "Negative Binomial"), 
        selected = c(
          "Bernoulli", 
          "Binomial", 
          "Discrete Uniform", 
          "Poisson", 
          "Geometric", 
          "Negative Binomial"))
    }
    else if (input$selectAllD %% 2 == 1) {
      updateButton(
        session = session, 
        inputId = "selectAllD", 
        label = "Select All")
      updateCheckboxGroupInput(
        session = session, inputId = "discretelist", 
        choices = c(
          "Bernoulli", 
          "Binomial", 
          "Discrete Uniform", 
          "Poisson", 
          "Geometric", 
          "Negative Binomial"))
    }
    else {
      updateButton(
        session = session, 
        inputId = "selectAllD", 
        label = "Unselect")
      updateCheckboxGroupInput(
        session = session, 
        inputId = "discretelist", 
        choices = c(
          "Bernoulli", 
          "Binomial", 
          "Discrete Uniform", 
          "Poisson", 
          "Geometric", 
          "Negative Binomial"), 
        selected = c(
          "Bernoulli", 
          "Binomial", 
          "Discrete Uniform", 
          "Poisson", 
          "Geometric", 
          "Negative Binomial"))
    }
  })
  ####### select AllContinuous#####
  observeEvent(input$selectAllC, {
    if (input$selectAllC == 0) {
      updateButton(
        session = session,  
        inputId = "selectAllC", 
        label = "Unselect")
      updateCheckboxGroupInput(
        session = session, 
        inputId = "continuouslist",
        choices = c(
          "Continuous Uniform", 
          "Gamma", 
          "Exponential", 
          "Normal", 
          "Beta"), 
        selected = c(
          "Continuous Uniform", 
          "Gamma", 
          "Exponential", 
          "Normal", 
          "Beta"))
    }
    else if (input$selectAllC %% 2 == 1) {
      updateButton(
        session = session, 
        inputId = "selectAllC", 
        label = "Select All")
      updateCheckboxGroupInput(
        session = session, 
        inputId = "continuouslist", 
        choices = c(
          "Continuous Uniform", 
          "Gamma", 
          "Exponential", 
          "Normal", 
          "Beta"))
    }
    else {
      updateButton(
        session = session, 
        inputId = "selectAllC", 
        label = "Unselect")
      updateCheckboxGroupInput(
        session = session, 
        inputId = "continuouslist", 
        choices = c(
          "Continuous Uniform", 
          "Gamma", 
          "Exponential", 
          "Normal", 
          "Beta"), 
        selected = c(
          "Continuous Uniform", 
          "Gamma", 
          "Exponential", 
          "Normal", 
          "Beta"))
    }
  })

  ######## Mixture of Dropdown and Checkbox########

  # TO DO: revise this code so that it is NOT hard coded, but rather makes use 
  #columns in the data frame.
  observeEvent(input$filter, {
    updateButton(
      session = session, 
      inputId = "hint", 
      disabled = FALSE
    )
    discretechosen <- input$discretelist
    continuouschosen <- input$continuouslist
    distributionchosen <<- c(discretechosen, continuouschosen)

    .generateStatement(session, object = "filter", verb = "interacted", 
          description = "Please select the distributions you'd like to use 
          in this app and click Filter", value = paste(distributionchosen, 
          sep = ", ", collapse = ", "))

    numberRow <- numeric()
    # numberRow <- dplyr::filter(bank, bank$distribution %in% distributionchosen)
    if ("Bernoulli" %in% distributionchosen) {
      numberRow <- c(numberRow, 1:6)
    }
    if ("Beta" %in% distributionchosen) {
      numberRow <- c(numberRow, 7:10)
    }
    if ("Binomial" %in% distributionchosen) {
      numberRow <- c(numberRow, 11:20)
    }
    if ("Continuous Uniform" %in% distributionchosen) {
      numberRow <- c(numberRow, 21:29)
    }
    if ("Discrete Uniform" %in% distributionchosen) {
      numberRow <- c(numberRow, 30:34)
    }
    if ("Exponential" %in% distributionchosen) {
      numberRow <- c(numberRow, 35:42)
    }
    if ("Gamma" %in% distributionchosen) {
      numberRow <- c(numberRow, 43:49)
    }
    if ("Geometric" %in% distributionchosen) {
      numberRow <- c(numberRow, 50:56)
    }
    if ("Hypergeometric" %in% distributionchosen) {
      numberRow <- c(numberRow, 57:60)
    }
    if ("Negative Binomial" %in% distributionchosen) {
      numberRow <- c(numberRow, 61:68)
    }
    if ("Normal" %in% distributionchosen) {
      numberRow <- c(numberRow, 69:80)
    }
    if ("Poisson" %in% distributionchosen) {
      numberRow <- c(numberRow, 81:90)
    }
    numberRow <<- numberRow
    updateButton(
      session = session, 
      inputId = "submit", 
      disabled = FALSE)
    # print(numberRow)
    
    ### Select questions from edited databank###
    output$question <- renderUI({
      withMathJax()
      id <<- sample(numberRow, 1, replace = FALSE, prob = NULL)
      updateSelectInput(
        session = session, 
        inputId = "answer",
        label = NULL, 
        choices = c("Select distribution", distributionchosen),
        selected = NULL
      )
      output$mark <- renderUI({
        img(src = NULL, width = 30)
      })
      # print(id)
      numberRow <<- numberRow[!numberRow %in% id]
      # print(numberRow)
      return(p(bank[id, 3]))
    })
    output$test1 <- renderUI({
      withMathJax()
    })
    output$test2 <- renderUI({
      withMathJax()
    })
  })

  ### PRINT HINT ####
  observeEvent(input$hint, {
    sendSweetAlert(
      session = session,
      title = "Hint:",
      closeOnClickOutside = TRUE,
      p(bank[id, 6])
    )
    .generateStatement(
      session = session, 
      object = "hint", 
      verb = "interacted", 
      description = "Hint", 
      value = bank[id, 6])
    })

  ### SUBMIT BUTTON###
  observeEvent(input$submit, {
    output$mark <- renderUI({
      if (!is.null(input$answer)) {
        correct_answer <<- bank[id, 4]
        success <- input$answer == correct_answer
        if (success) {
          img(src = "check.png", width = 30)
        } else {
          img(src = "cross.png", width = 30)
        }
      }
    })
  })

  ### NEXT QUESTION BUTTON###
  observeEvent(input$nextq, {
    if (length(numberRow) == 1) {
      sendSweetAlert(
        session = session,
        title = "Warning:",
        # type = "error",
        closeOnClickOutside = TRUE,
        p("We have run out of questions. Please restart.")
      )
      updateButton(
        session = session, 
        inputId = "submit",
        disabled = TRUE)
      updateButton(
        session = session, 
        inputId = "nextq", 
        disabled = TRUE)
      updateButton(
        session = session, 
        inputId = "hint", 
        disabled = TRUE)
      
    }
    else {
      id <<- sample(numberRow, 1, replace = FALSE, prob = NULL)
      # print(id)
      hint <- bank[id, 6]
      numberRow <<- numberRow[!numberRow %in% id]
      # print(numberRow)
      output$question <- renderUI({
        withMathJax()
        output$test1 <- renderUI({
          withMathJax()
        })
        output$test2 <- renderUI({
          withMathJax()
        })
        return(p(bank[id, 3]))
      })
      
      updateButton(
        session = session,
        inputId = "submit", 
        disabled = FALSE)
      
      updateSelectInput(
        session = session, 
        inputId = "answer",
        label = NULL, 
        choices = c("Select distribution", distributionchosen),
        selected = NULL
      )
      output$mark <- renderUI({
        img(src = NULL, width = 30)
      })
      ### FEEDBACK###
      output$feedback <- renderUI({
        return(NULL)
      })
    }
  })

  observeEvent(input$submit, {
    if (value[["mistake"]] == MAX_TRIES) {
      updateButton(
        session = session, 
        inputId = "nextq", 
        disabled = TRUE)
      output$result <- renderUI({
        h3("You have lost this Game. You need to click 'restart' button to start 
           this game from the beginning.")
      })
    }
  })

  ########### Counting Correct answers############
  observeEvent(input$submit, {
    if (!is.null(input$answer)) {
      correct_answer <<- bank[id, 4]
      success <- input$answer == correct_answer
      WIN <- FALSE

      if (success) {
        value$correct <<- value$correct + 1
        WIN <- value$correct == WIN_STATE
        if (WIN) {
          GAME_OVER <<- TRUE
          sendSweetAlert(
            session = session,
            title = "Success:",
            type = "success",
            closeOnClickOutside = TRUE,
            h4("Congrats! You Win! Please click Restart to start over.")
          )

          updateButton(
            session = session, 
            inputId = "submit", 
            disabled = TRUE)
          updateButton(
            session = session, 
            inputId = "nextq", 
            disabled = TRUE)
          updateButton(
            session = session, 
            inputId = "restart", 
            disabled = FALSE)
          updateButton(
            session = session, 
            inputId = "hint", 
            disabled = TRUE)
          
        }
      } else {
        value[["mistake"]] <<- value[["mistake"]] + 1

        if (value[["mistake"]] == MAX_TRIES) {
          GAME_OVER <<- TRUE
          WIN <- FALSE

          updateButton(
            session = session, 
            inputId = "submit", 
            disabled = TRUE)
          updateButton(
            session = session, 
            inputId = "nextq", 
            disabled = TRUE)
          updateButton(
            session = session, 
            inputId = "restart",
            disabled = FALSE)
          updateButton(
            session = session,
            inputId = "filter", 
            disabled = TRUE)
          updateButton(
            session = session,
            inputId = "hint", 
            disabled = TRUE)
          

          sendSweetAlert(
            session = session,
            title = "Lost:",
            type = "error",
            closeOnClickOutside = TRUE,
            h4("You  have lost. Please click Restart to start over.")
          )
        }
      }

      .generateAnsweredStatement(
        session,
        object = "submit",
        verb = "answered",
        description = bank[id, 3],
        response = input$answer,
        interactionType = "choice",
        success = success,
        completion = GAME_OVER
      )

      if (GAME_OVER) {
        if (WIN) {
          .generateStatement(
            session = session, 
            object = "game", 
            verb = "completed", 
            description = "Player has won the game.")
        } else {
          .generateStatement(session = session, 
                             object = "game", 
                             verb = "completed", 
                             description = "Player has lost the game.")
        }
      }


      ### FEEDBACK###
      output$feedback <- renderUI({
        correct_answer <<- bank[id, 4]
        if (input$answer == correct_answer) {
          p("CORRECT!", br(), bank[id, 7])
        }
        else {
          p(strong("Answer:"), br(), bank[id, 4], 
            br(), strong("Explanation:"), br(), bank[id, 7])
        }
        # withMathJax()
        # h4(strong('Feedback',br(),bank[id,7]))
      })

      output$result <- renderUI({
        h3("Congratulations! You got this one correct. Click 'Next Question' to
           move on your challenge.")
      })
    }
  })

  ##### Draw the Hangman Game#####

  output$distPlot <- renderUI({
    if (value[["mistake"]] == 0) {
      img(src = "Cell01.jpg", alt = "Man resting on top branch.")
    }

    else if (value[["mistake"]] == 1) {
      img(src = "Cell02.jpg", alt = "Man has fallen down one branch. Two 
                                    more branches left!")
    }

    else if (value[["mistake"]] == 2) {
      img(src = "Cell03.jpg", alt = "Man has fallen down one branch. One 
                                    more branches left!")
    }

    else if (value[["mistake"]] == 3) {
      img(src = "Cell04.jpg", alt = "Man is standing on the last branch.")
    }

    else if (value[["mistake"]] == 4) {
      img(src = "Cell05.jpg", alt = "Man has fallen to the ground.")
    }
    else if (value[["mistake"]] == 5) {
      img(src = "GAMEOVER.png", alt = "Game Over!")
    }
  })
}

boastApp(server = server, ui = ui)
