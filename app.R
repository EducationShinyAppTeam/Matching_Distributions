# Load libraries ----
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(shinydashboard)
library(boastUtils)
library(dplyr)

# Define Global Constants ----
MAX_TRIES <- 4
WIN_STATE <- 10
GAME_OVER <- FALSE

# Load and shuffle question bank ----
rawBank <- read.csv(file = "questionBank.csv", header = TRUE)

# Define lists of discrete and continuous distributions ----
discDists <- rawBank %>%
  dplyr::filter(type == "discrete") %>%
  dplyr::select(distribution) %>%
  dplyr::distinct(distribution) %>%
  dplyr::arrange(distribution)

contDists <- rawBank %>%
  dplyr::filter(type == "continuous") %>%
  dplyr::select(distribution) %>%
  dplyr::distinct(distribution) %>%
  dplyr::arrange(distribution)

# Investigate these constants ----

numberRow <- numeric()
hint <- c()
correct_answer <- c()

# Define UI for App ----
ui <- list(
  dashboardPage(
    skin = "blue",
    ## Header ----
    dashboardHeader(
      title = "Matching Distributions",
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Matching_Distributions")
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/', icon("home"))
      )
    ),
    ## Sidebar ----
    dashboardSidebar(
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Game", tabName = "game", icon = icon("gamepad")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ## Body ----
    dashboardBody(
      ### Overview Page ----
      tabItems(
        tabItem(
          tabName = "overview",
          h1("Matching Distributions"),
          p("In this App, you will gain practice at associating context with
            different probability distributions."),
          br(),
          h2("Instructions"),
          tags$ul(
            tags$li(
              "You will start this game with a little man on the top of a tree,
              and you are trying to prevent his fall to the ground.  If you
              provide a wrong answer, he falls to a lower branch and eventually
              to the ground. If you get 10 questions correct before he falls to
              the ground, you have won the game and saved the little man!"
            ),
            tags$li(
              "Please select which probability distribution(s) you would like to
              work on and hit the 'filter button."
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
              "Before checking out the game page, be sure to review your
              knowledge on the prerequisites page!"
            )
          ),
          div(
            style = "text-align:center;",
            bsButton(
              inputId = "go",
              label = "GO!",
              icon("book"),
              size = "large"
            )
          ),
          br(),
          h3("Acknowledgements"),
          p(
            "This app was developed and coded by Zhiliang Zhang and futher
            updated by Yiyang Wang, Yuqing Lei and Shravani Samala in 2021.",
            br(),
            br(),
            br(),
            "Cite this app as:",
            br(),
            boastUtils::citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 10/8/2021 by NJH.")
          )
        ),
        ### Prerequisites Page ----
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
            tags$li("Review each distribution's parameters, pmf and/or cdf, mean,
                    variance, and moment-generating function in the ",
                    tags$a(
                      href = "https://psu-eberly.shinyapps.io/probability_applications/",
                      "Probability Applications",
                      class = "bodylinks"
                    ),
                    "app's prerequisite page!")
          ),
          h3("Discrete Distributions"),
          p("For Discrete Distributions, data can only take on certain values in
            a discrete set such as the whole numbers or integers."),
          fluidRow(
            box(
              title = "Bernoulli",
              status = "primary",
              width = 6,
              collapsible = TRUE,
              collapsed = TRUE,
              p(tags$strong("Model: "), "any situation where we can think of as 
                entailing a 'success' (typically coded as 1) and 'failure' 
                (everything that isn't a 'success', typically coded as 0).",
                br(),
                tags$strong("Example: "), "getting a 'heads' when flipping a coin."
              )
            ),
            box(
              title = "Binomial",
              status = "primary",
              width = 6,
              collapsible = TRUE,
              collapsed = TRUE,
              p(tags$strong("Model: "), "The number of 'successes' we see in a
                fixed number of independent Bernoulli trials.",
                br(),
                tags$strong("Example: "), "how many 'heads' we get when we flip
                a coin 10 times."
              )
            )
          ),
          fluidRow(
            box(
              title = "Discrete Uniform",
              status = "primary",
              width = 6,
              collapsible = TRUE,
              collapsed = TRUE,
              p(tags$strong("Model: "), "any situation where we believe that
                statistical fairness holds; that is, we anticipate that each
                individual outcome has the same probability of occurring.",
                br(),
                tags$strong("Example: "), "observing the top facing number when
                rolling a standard, six-sided, fair, die."
              )
            ),
            box(
              title = "Geometric",
              status = "primary",
              width = 6,
              collapsible = TRUE,
              collapsed = TRUE,
              p(tags$strong("Model: "), "any situation where we are tracking how
                many independent trials we conduct before we observe the first
                success.",
                br(),
                tags$strong("Example: "), "how many times you have to flip a coin
                until you first get 'heads'."
              )
            )
          ),
          fluidRow(
            box(
              title = "Hypergeometric",
              status = "primary",
              width = 6,
              collapsible = TRUE,
              collapsed = TRUE,
              p(tags$strong("Model: "), "any situation where we first imagine a 
                collection of ", tags$em("N"), " total items. In this collection
                there are ", tags$em("m"), " type 1 items and ", tags$em("N - m"),
                " type 2 items. We then imagine randomly selecting ", tags$em("n"),
                " items without replacement from the collection.",
                br(),
                tags$strong("Example: "), "a bowl contains 30 red marbles and 
                      60 black marbles. You randomly choose 15 marbles from the 
                      bowl. The number of black marbles in this sample follows a
                      hypergeometric distribution."
              )
            ), 
            box(
              title = "Negative Binomial",
              status = "primary",
              width = 6,
              collapsible = TRUE,
              collapsed = TRUE,
              p(tags$strong("Model: "), "any situation where we are tracking how
                many independent trials we conduct before we observe the ",
                tags$em("k"), "-th success.",
                br(),
                tags$strong("Example: "), "how many times you have to flip a coin
                until you get ten 'heads'."
              )
            )
          ),
          fluidRow(
            box(
              title = "Poisson",
              status = "primary",
              width = 6,
              collapsible = TRUE,
              collapsed = TRUE,
              p(tags$strong("Model: "), "any situation where we are interested in
                the how many times we observe a rare event (the event's count) in
                a fixed amount of time or space.",
                br(),
                tags$strong("Example: "), "the number of fatal scuba diving 
                accidents in Australia next year."
              )
            )
          ),
          h3("Continuous Distributions"),
          p("For Continuous Distributions, data can take on values from a 
            continuum."),
          fluidRow(
            box(
              title = "Beta",
              status = "primary",
              width = 6,
              collapsible = TRUE,
              collapsed = TRUE,
              p(tags$strong("Model: "), "the specific rank of fixed number of 
                independent standard uniforms.",
                br(),
                tags$strong("Example: "), "the median of five trials for the
                proportion of the way around a circle that a spinner lands."
              )
            ),
            box(
              title = "Continuous Uniform",
              status = "primary",
              width = 6,
              collapsible = TRUE,
              collapsed = TRUE,
              p(tags$strong("Model: "), "when we imagine the probability spread
                evenly (uniformly) over the continuum.",
                br(),
                tags$strong("Example: "), "the measure of the angle swept out by
                the spinner from a fixed reference point."
              )
            )
          ),
          fluidRow(
            box(
              title = "Exponential",
              status = "primary",
              width = 6,
              collapsible = TRUE,
              collapsed = TRUE,
              p(tags$strong("Model: "), "the time until the next independent and
                rare event.",
                br(),
                tags$strong("Example: "), "how long until the next fatal scuba
                diving accident occurs in Australia."
              )
            ),
            box(
              title = "Gamma",
              status = "primary",
              width = 6,
              collapsible = TRUE,
              collapsed = TRUE,
              p(tags$strong("Model: "), "the time until the ", tags$em("k"), "-th
                rare independent event occurs.",
                br(),
                tags$strong("Example: "), "how long until there have been ten
                fatal scuba diving accidents occur in Australia."
              )
            )
          ),
          fluidRow(
            box(
              title = "Normal",
              status = "primary",
              width = 6,
              collapsible = TRUE,
              collapsed = TRUE,
              p(tags$strong("Model: "), "sum or [arithmetic] mean from a large
                sample.",
                br(),
                tags$strong("Example: "), "the [arithmetic] mean income of a 
                random sample of 1000 people."
              )
            )
          ),
          div(
            style = "text-align:center;",
            bsButton(
              inputId = "go2",
              label = "GO!",
              icon("gamepad"),
              size = "large"
            )
          )
        ),
        ### Game Page ----
        tabItem(
          tabName = "game",
          withMathJax(),
          h2("Matching Distributions Game"),
          p("Identify the appropriate distribution based upon the context 
            provided. To win, you will need to correctly identify ", WIN_STATE, 
            " distributions before the man falls to the ground. Each time you 
            incorrectly guess, he will move down a branch; you make at most ",
            MAX_TRIES, " mistakes."),
          hr(),
          h3("First, Select Your Distributions"),
          p("Please select the distributions you'd like to use for this game and
            then click the 'Filter' button to begin."),
          fluidRow(
            column(
              width = 2,
              offset = 0,
              dropdownButton(
                inputId = "discreteDropdown",
                label = "Discrete distributions",
                size = "lg",
                circle = FALSE,
                status = "default",
                width = "100%",
                tags$div(
                  bsButton(
                    inputId = "selectAllD",
                    label = "Unselect",
                    size = "small"
                  ),
                  checkboxGroupInput(
                    inputId = "discreteList",
                    label = "Discrete distributions",
                    choices = discDists$distribution,
                    selected = discDists$distribution
                  )
                )
              )
            ),
            column(
              width = 2,
              offset = 1,
              dropdownButton(
                inputId = "continuousDropdown",
                label = "Continuous distributions",
                circle = FALSE,
                size = "lg",
                status = "default",
                width = "100%",
                tags$div(
                  bsButton(
                    inputId = "selectAllC",
                    label = "Unselect",
                    size = "small"
                  ),
                  checkboxGroupInput(
                    inputId = "continuousList",
                    label = "Continuous distributions",
                    choices = contDists$distribution,
                    selected = contDists$distribution
                  )
                )
              )
            ),
            column(
              width = 2,
              offset = 1,
              bsButton(
                inputId = "filter",
                label = "Filter",
                size = "large",
                style = "primary",
                disabled = FALSE
              )
            )
          ),
          hr(),
          h3("Play the Game"),
          fluidRow(
            column(
              width = 5,
              offset = 0,
              wellPanel(
                p("Your current context:"),
                uiOutput("question"),
                fluidRow(
                  column(
                    width = 3,
                    bsButton(
                      inputId = "hint",
                      label = "Hint",
                      icon = icon("question"),
                      size = "large",
                      type = "toggle",
                      disabled = TRUE
                    )
                  ),
                  column(
                    width = 9,
                    uiOutput("hintDisplay")
                  )
                ),
                selectInput(
                  inputId = "answer",
                  label = "Select the matching distribution",
                  choices = "", 
                  selected = "",
                  width = "100%"
                ),
                fluidRow(
                  column(
                    width = 3,
                    bsButton(
                      inputId = "submit",
                      label = "Submit",
                      size = "large",
                      style = "default",
                      disabled = TRUE
                    )
                  ),
                  column(
                    width = 1,
                    uiOutput("mark")
                  )
                ),
                uiOutput("feedback"),
                br(),
                fluidRow(
                  column(
                    width = 4,
                    bsButton(
                      inputId = "nextq",
                      label = "Next Question",
                      size = "large",
                      style = "success",
                      disabled = TRUE
                    )
                  ),
                  column(
                    width = 4,
                    offset = 1,
                    bsButton(
                      inputId = "restart",
                      label = "Restart the game",
                      size = "large",
                      style = "warning",
                      disabled = FALSE
                    )
                  )
                )
              )
            ),
            column(
              width = 7,
              offset = 0,
              uiOutput("correctCount", align = "center"),
              uiOutput("gameProgress", align = "center")
            )
          )
        ), 
        ### References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h1("References"),
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
          p(
            class = "hangingindent",
            "Wickham, H., François, R., Henry, L., and Müller, K. (2021). dplyr:
            A Grammar of Data Manipulation. R package version 1.0.7. Available
            from https://CRAN.R-project.org/package=dplyr"
          ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
)
   

# Define server logic ----
server <- function(session, input, output) {
  ## User variables ----
  chosenDists <- reactiveVal(NULL)
  filteredQuestions <- reactiveVal()
  rowNumber <- reactiveVal(0)
  score <- reactiveVal(0)
  mistakes <- reactiveVal(0)

  ## Info button ----
  observeEvent(
    eventExpr = input$info, 
    handlerExpr = {
      sendSweetAlert(
        session = session,
        title = "Instructions",
        text = "After selecting which distributions you want to practice and
        clicking Filter, carefully read through the provided context. Then select
        the distribution that best matches what's happening.",
      type = "info"
      )
    })

  ## Go button overview to prerequisites----
  observeEvent(
    eventExpr = input$go,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "prerequisites")
    })
      
  ## Go button prerequisites to challenge----
  observeEvent(
    eventExpr = input$go2,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "game")
    })

  ## Select all discrete ----
  observeEvent(
    eventExpr = input$selectAllD, 
    handlerExpr = {
      if (input$selectAllD == 0) {
        updateButton(
          session = session,
          inputId = "selectAllD",
          label = "Unselect"
        )
        updateCheckboxGroupInput(
          session,
          "discreteList",
          choices = discDists$distribution,
          selected = discDists$distribution
        )
      } else if (input$selectAllD %% 2 == 1) {
        updateButton(
          session = session,
          inputId = "selectAllD",
          label = "Select All")
        updateCheckboxGroupInput(
          session = session, 
          inputId = "discreteList",
          choices = discDists$distribution
        )
      } else {
        updateButton(
          session = session,
          inputId = "selectAllD",
          label = "Unselect")
        updateCheckboxGroupInput(
          session = session,
          inputId = "discreteList",
          choices = discDists$distribution,
          selected = discDists$distribution
        )
      }
    }
  )
  
  ## Select all continuous ----
  observeEvent(
    eventExpr = input$selectAllC, 
    handlerExpr = {
      if (input$selectAllC == 0) {
        updateButton(
          session = session,
          inputId = "selectAllC",
          label = "Unselect")
        updateCheckboxGroupInput(
          session = session,
          inputId = "continuousList",
          choices = contDists$distribution,
          selected = contDists$distribution
        )
      } else if (input$selectAllC %% 2 == 1) {
        updateButton(
          session = session,
          inputId = "selectAllC",
          label = "Select All")
        updateCheckboxGroupInput(
          session = session,
          inputId = "continuousList",
          choices = contDists$distribution
        )
      } else {
        updateButton(
          session = session,
          inputId = "selectAllC",
          label = "Unselect")
        updateCheckboxGroupInput(
          session = session,
          inputId = "continuousList",
          choices = contDists$distribution,
          selected = contDists$distribution
        )
      }
    })
  
  ## Filter button ----
  observeEvent(
    eventExpr = input$filter,
    handlerExpr = {
      ### Enable buttons: hint, submit ----
      updateButton(
        session = session,
        inputId = "hint",
        disabled = FALSE
      )
      updateButton(
        session = session,
        inputId = "submit",
        disabled = FALSE
      )

      ### Filter question bank ----
      chosenDists(c(input$discreteList, input$continuousList))

      #### Error check
      if (is.null(chosenDists())) {
        sendSweetAlert(
          session = session,
          title = "No Distributions Selected",
          text = "You've not selected any distributions to review. Please select
          at least one distribution.",
          type = "error"
        )
      } else {
        updateSelectInput(
          session = session,
          inputId = "answer",
          label = NULL,
          choices = c("Select a distribution", chosenDists()),
          selected = NULL
        )
        
        questionBank <- rawBank %>%
          dplyr::slice_sample(prop = 1)
        
        filteredQuestions(
          questionBank %>%
            dplyr::filter(distribution %in% chosenDists())
        )
        ### Do we really want this here?
        rowNumber(1)
        
        output$question <- renderUI({
          withMathJax(p(filteredQuestions()$context[rowNumber()]))
        })
      }
      
      stmt <- boastUtils::generateStatement(
        session,
        object = "filter",
        verb = "interacted",
        description = "Please select the distributions you'd like to use in this app and click Filter",
        response = paste(chosenDists(), sep = ", ", collapse = ", ")
      )
      
      boastUtils::storeStatement(session, stmt)
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )
  
  ## Submit button ----
  observeEvent(
    eventExpr = input$submit, 
    handlerExpr = {
      ### Check for valid response on submit ----
      if (is.null(input$answer) || input$answer == "Select a distribution") {
        sendSweetAlert(
          session = session,
          title = "Select an answer",
          text = "Be sure to select a distribution from the drop down menu
          before you click the submit button.",
          type = "error"
        )
      } else {
        #### Disable self and hint buttons ----
        updateButton(
          session = session,
          inputId = "hint",
          disabled = TRUE
        )
        updateButton(
          session = session,
          inputId = "submit",
          disabled = TRUE
        )
        #### Score choice ----
        success <- input$answer == filteredQuestions()$distribution[rowNumber()]
        if (success) {
          score(score() + 1)
          output$mark <- boastUtils::renderIcon(icon = "correct")
        } else {
          mistakes(mistakes() + 1)
          output$mark <- boastUtils::renderIcon(icon = "incorrect")
        }
        
        #### Display feedback ----
        output$feedback <- renderUI({
          withMathJax(p(filteredQuestions()$feedback[rowNumber()]))
        })
        
        stmt <- boastUtils::generateStatement(
          session,
          object = "submit",
          verb = "answered",
          description = filteredQuestions()$context[rowNumber()],
          response = input$answer,
          interactionType = "choice",
          success = success,
          completion = score() == WIN_STATE
        )
        
        boastUtils::storeStatement(session, stmt)
      }
      
      ### Check if game has been won or lost ----
      if (score() == WIN_STATE) {
        sendSweetAlert(
          session = session,
          title = "Congrats!",
          text = "You have won the game!",
          type = "success"
        )
        
        stmt <- boastUtils::generateStatement(
          session = session,
          object = "game",
          verb = "completed",
          description = "Player has won the game.")
        
        boastUtils::storeStatement(session, stmt)
      } else if (mistakes() == MAX_TRIES) {
        sendSweetAlert(
          session = session,
          title = "Try Again",
          text = "The man fell to the ground. Please try again.",
          type = "error"
        )
        
        stmt <- boastUtils::generateStatement(
          session = session,
          object = "game",
          verb = "completed",
          description = "Player has lost the game.")
        
        boastUtils::storeStatement(session, stmt)
      } else {
        #### Enable next question
        updateButton(
          session = session,
          inputId = "nextq",
          disabled = FALSE
        )
      }
    })
  
  ## Next question button ----
  observeEvent(
    eventExpr = input$nextq,
    handlerExpr = {
      ### Clear feedback displays ----
      output$mark <- renderIcon()
      output$hintDisplay <- NULL
      output$feedback <- NULL
      
      ### Reset answer selection ----
      updateSelectInput(
        session = session,
        inputId = "answer",
        selected = "Select a distribution"
      )
      
      ### Move through question bank ----
      if (rowNumber() >= nrow(filteredQuestions())) {
        sendSweetAlert(
          session = session,
          title = "Out of Questions",
          text = "Unfortunately, we've run out of questions. Please restart the
          game and/or include more distributions to practice with.",
          type = "error"
        )
      } else {
        rowNumber(rowNumber() + 1)
        
        #### Enable buttons ----
        updateButton(
          session = session,
          inputId = "hint",
          disabled = FALSE
        )
        updateButton(
          session = session,
          inputId = "submit",
          disabled = FALSE
        )
      }
      
      updateButton(
        session = session,
        inputId = "nextq",
        disabled = TRUE
      )
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )
  
  ## Restart button ----
  observeEvent(
    eventExpr = input$restart,
    handlerExpr = {
      ### Clear feedback displays ----
      output$mark <- renderIcon()
      output$hintDisplay <- NULL
      output$feedback <- NULL
      
      ### Reset answer selection ----
      updateSelectInput(
        session = session,
        inputId = "answer",
        choices = c("Select a distribution"),
        selected = "Select a distribution"
      )
      
      ### Disable buttons ----
      updateButton(
        session = session,
        inputId = "hint",
        disabled = TRUE
      )
      updateButton(
        session = session,
        inputId = "submit",
        disabled = TRUE
      )
      updateButton(
        session = session,
        inputId = "nextq",
        disabled = TRUE
      )
      
      ### Reset filters ----
      updateButton(
        session = session,
        inputId = "selectAllD",
        label = "Unselect"
      )
      updateButton(
        session = session,
        inputId = "selectAllC",
        label = "Unselect"
      )
      
      updateCheckboxGroupInput(
        session = session,
        inputId = "discreteList",
        selected = discDists$distribution
      )
      
      updateCheckboxGroupInput(
        session = session,
        inputId = "continuousList",
        selected = contDists$distribution
      )
      
      ### Clear user variables ----
      chosenDists(NULL)
      filteredQuestions(NULL)
      rowNumber(0)
      score(0)
      mistakes(0)
      
      stmt <- boastUtils::generateStatement(
        session = session,
        object = "restart",
        verb = "interacted",
        description = "Game has been restarted.")
      
      boastUtils::storeStatement(session, stmt)
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  ## Reactive display objects ----
  ### Display base question prompt ----
  output$question <- renderUI({
    if (rowNumber() == 0) {
      output$question <- renderUI({
        p("Select distributions to review and then press Filter.")
      })
    }
  })
  
  ### Display hint ----
  observeEvent(
    eventExpr = input$hint, 
    handlerExpr = {
      if (input$hint) {
        output$hintDisplay <- renderUI({
          hint <- filteredQuestions()$hint[rowNumber()]
          
          stmt <- boastUtils::generateStatement(
            session = session,
            object = "hint",
            verb = "interacted",
            description = "Hint",
            response = hint)
          
          boastUtils::storeStatement(session, stmt)
          
          withMathJax(p(hint))
        })
      } else {
        output$hintDisplay <- NULL
      }
    })
  
  ### Number correct display ----
  output$correctCount <- renderUI({
    p(
      class = "largerFont",
      "Number of correct answers: ", score()
    )
  })
  
  ### Progress plot ----
  output$gameProgress <- renderUI({
    if (mistakes() == 0) {
      img(src = "Cell01.jpg", alt = "Man resting on top branch.")
    }
    
    else if (mistakes() == 1) {
      img(src = "Cell02.jpg", alt = "Man has fallen down one branch. Two
                                    more branches left!")
    }
    
    else if (mistakes() == 2) {
      img(src = "Cell03.jpg", alt = "Man has fallen down one branch. One
                                    more branches left!")
    }
    
    else if (mistakes() == 3) {
      img(src = "Cell04.jpg", alt = "Man is standing on the last branch.")
    }
    
    else if (mistakes() == 4) {
      img(src = "Cell05.jpg", alt = "Man has fallen to the ground.")
    }
  })
  
  boastUtils::typesetMath(session = session)
}

boastApp(server = server, ui = ui)
