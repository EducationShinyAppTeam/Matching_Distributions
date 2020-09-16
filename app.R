library(shiny)
library(shinyBS)
library(shinyWidgets)
library(shinydashboard)
library(boastUtils)
library(dplyr)

## App Meta Data----------------------------------------------------------------
APP_TITLE <<- "Matching Distributions"
APP_DESCP  <<- paste(
  "This app provides a hangman-style game format for helping students review",
  "various probability distributions. Students examine a presented context and",
  "then categorize the conveyed long-run behavior as being a particular named",
  "distribution."
)
## End App Meta Data------------------------------------------------------------

# Global Constants and Functions ----
maxTries <- 4
winLimit <- 10

# Read in Questions ----
questionBank <- read.csv("questionBank.csv", stringsAsFactors = FALSE)

## Place all questions into a randomized but fixed order by distribution ----
set.seed(2020)
questionBank <- questionBank %>%
  dplyr::group_by(distribution) %>%
  dplyr::slice_sample(prop = 1)
set.seed(NULL)

# Define lists of Discrete and Continuous distributions in question questionBank ----
discDists <- questionBank %>%
  dplyr::filter(type == "discrete") %>%
  dplyr::select(distribution) %>%
  dplyr::distinct(distribution) %>%
  dplyr::arrange(distribution)

contDists <- questionBank %>%
  dplyr::filter(type == "continuous") %>%
  dplyr::select(distribution) %>%
  dplyr::distinct(distribution) %>%
  dplyr::arrange(distribution)

# Create the UI ----
ui <- list(
  dashboardPage(
    skin = "blue",
    ## Header ----
    dashboardHeader(
      title = "Matching Distributions",
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(class = "dropdown",
              tags$a(href = "https://shinyapps.science.psu.edu/",
                     icon("home")
              )
      )
    ),
    ## Sidebar ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "tabs",
        menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
        menuItem("Game", tabName = "matchingDist", icon = icon("gamepad")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::psu_eberly_logo("reversed")
      )
    ),
    ## Body ----
    dashboardBody(
      tabItems(
        # Overview Tab ----
        tabItem(
          tabName = "overview",
          h1("Matching Distributions Game"),
          p(
            "In this app, you will gain practice at associating contexts with
          different probability distributions."
          ),
          p(paste(
            "You'll start this game with a little man on the top of a tree, and you
          are trying to prevent his fall to the ground. If you provide a wrong
          answer, he falls to a lower branch and eventually to the ground. If you
          get", winLimit, "questions correct before he falls to the ground, you have won
          the game and saved the little man!")
          ),
          h2("Instructions"),
          tags$ol(
            tags$li(
              "Select which probability distribution(s) you'd like to work on from
            the Discrete and Continous distribution menus. (All are selected by
            default.)"
            ),
            tags$li("Click the 'Filter' button to process your selections and display
                  the first context to classify."
            ),
            tags$li(
              "Read the given text and choose a distribution from the dropdown menu.
            Make sure you understand the scenario."
            ),
            tags$li("Press the 'Submit' button to check your answer. Once you do,
                  you cannot revise your answer."),
            tags$li("If you need a hint, click the '? Hint' button."),
            tags$li("Press the 'Next Question' button to move on the next question.")
          ),
          div(
            style = "text-align:center",
            bsButton(
              inputId = "go",
              label = "GO!",
              icon = icon("bolt"),
              size = "large",
              style = "default"
            )
          ),
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This app was developed and coded by Zhiliang Zhang and futher updated
          by Yiyang Wang and Yuqing Lei in 2019 and Neil J. Hatfield in 2020.",
            br(),
            br(),
            br(),
            div(class = "updated", "Last Update: 8/25/2020 by NJH.")
          )
        ),
        # Game Tab ----
        tabItem(
          tabName = "matchingDist",
          withMathJax(),
          h2("Matching Contexts with Distributions"),
          p("Please select the distributions you'd like to use in this app and
          click the 'Filter' button."),
          ### Selection Row ----
          fluidRow(
            column(
              width = 3,
              dropdownButton(
                inputId = "discreteDrop",
                label = "Discrete distributions",
                circle = FALSE,
                status = "default",
                size = "lg",
                bsButton(
                  inputId = "selectAllD",
                  label = "Unselect all",
                  size = "medium"
                ),
                checkboxGroupInput(
                  inputId = "discreteList",
                  label = NULL,
                  choices = discDists$distribution,
                  selected = discDists$distribution
                )
              )
            ),
            column(
              width = 4,
              offset = 1,
              dropdownButton(
                inputId = "continousDrop",
                label = "Continuous distributions",
                circle = FALSE,
                status = "default",
                size = "lg",
                bsButton(
                  inputId = "selectAllC",
                  label = "Unselect all",
                  size = "medium"
                ),
                checkboxGroupInput(
                  inputId = "continuousList",
                  label = NULL,
                  choices = contDists$distribution,
                  selected = contDists$distribution
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
                style = "success",
                disabled = FALSE
              )
            )
          ),
          br(),
          ### Main display area ----
          fluidRow(
            column(
              width = 5,
              wellPanel(
                h3("Context"),
                uiOutput("context"),
                bsButton(
                  inputId = "hint",
                  label = "Hint",
                  icon = icon("question"),
                  size = "default",
                  disabled = TRUE,
                  style = "default"
                ),
                selectInput(
                  inputId = "answer",
                  label = "Select your answer",
                  choices = c("Select distributions and filter first")
                ),
                tagList(
                  uiOutput("mark"),
                  uiOutput("feedback"),
                ),
                bsButton(
                  inputId = "submit",
                  label = "Submit",
                  size = "large",
                  style = "warning",
                  disabled = TRUE
                ),
                br(),
                bsButton(
                  inputId = "nextQ",
                  label = "Next question",
                  size = "large",
                  style = "success",
                  disabled = TRUE
                ),
                br(),
                bsButton(
                  inputId = "restart",
                  label = "Restart the game",
                  size = "large",
                  style = "danger",
                  disabled = FALSE
                )
              )
            ),
            column(
              width = 7,
              uiOutput("correctCount", align = "center"),
              uiOutput("gameProgress", align = "center")
            )
          ),
          ## MathJax Re-triggers  ----
          uiOutput("math1"),
          uiOutput("math2")
        ),
        ## References Tab ----
        tabItem(
          tabName = "references",
          h2("References")
        )
      )
    )
  )
)

# Define the server ----
server <- function(session, input, output) {
  ## Learning Locker Statement Generation ----
  .generateStatement <- function(session, verb = NA, object = NA, description = NA, value = NA) {
    if (is.na(object)) {
      object <- paste0("#shiny-tab-", session$input$tabs)
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

  .generateAnsweredStatement <- function(session, verb = NA, object = NA, description = NA, interactionType = NA, response = NA, success = NA, completion = FALSE) {
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

  ## Initialize Variables ----
  score <- reactiveVal(0)
  mistakes <- reactiveVal(0)
  rowNum <- reactiveVal(0)
  gameOver <- FALSE
  #hint <- c()
  #correct_answer <- c()

  ## Define buttons ----
  #### Info button ----
  observeEvent(input$info, {
    sendSweetAlert(
      session = session,
      title = "Instructions",
      text = tags$ol(
        tags$li("Select which Discrete/Continuous distributions you want to
                review and press the Filter button."),
        tags$li("Read the context and select which distribution you believe
                   correctly describes the long-run behavior in the context.")
      ),
      type = "info"
    )
  })

  ### Go button ----
  observeEvent(input$go, {
    updateTabItems(
      session = session,
      inputId = "tabs",
      selected = "matchingDist")
    value$index <<- 1
    value$answerbox <<- value$index
  })

  ### Select/Unselect All-Discrete button----
  observeEvent(input$selectAllD, {
    if (input$selectAllD %% 2 == 0) {
      updateButton(
        session = session,
        inputId = "selectAllD",
        label = "Unselect all")
      updateCheckboxGroupInput(
        session = session,
        inputId = "discreteList",
        selected = discDists$distribution
      )
    }
    else {
      updateButton(
        session = session,
        inputId = "selectAllD",
        label = "Select all")
      updateCheckboxGroupInput(
        session = session,
        inputId = "discreteList",
        choices = discDists$distribution
      )
    }
  })

  ### Select/Unselect All-Conintuous button----
  observeEvent(input$selectAllC, {
    if (input$selectAllC %% 2  == 0) {
      updateButton(
        session = session,
        inputId = "selectAllC",
        label = "Unselect all")
      updateCheckboxGroupInput(
        session = session,
        inputId = "continuousList",
        selected = contDists$distribution
      )
    }
    else {
      updateButton(
        session = session,
        inputId = "selectAllC",
        label = "Select all")
      updateCheckboxGroupInput(
        session = session,
        inputId = "continuousList",
        choices = contDists$distribution
      )
    }
  })

  ### Filter button ----
  observeEvent(input$filter, {
    discreteChosen <- input$discreteList
    continouousChosen <- input$continuousList
    distributionChosen <- sort(c(discreteChosen, continouousChosen))

    #### Error Check-No Distributions Chosen ----
    if(is.null(distributionChosen)){
      sendSweetAlert(
        session = session,
        title = "ERROR",
        text = "You've not selected any distributions to review. Please select
        at least one distribution.",
        type = "error"
      )
    } else {
      .generateStatement(session, object = "filter", verb = "interacted", description = "Please select the distributions you'd like to use in this app and click Filter", value = paste(distributionChosen, sep = ", ", collapse = ", "))

      #### Filter Question Bank ----
      questionBank <- questionBank %>%
        filter(distribution %in% distributionChosen)
      numberRow <- nrow(questionBank)

      #### Arrange Question Bank ----
      if (length(distributionChosen) > 1 ){
        shufDist <- sample(
          x = questionBank$distribution,
          size = numberRow,
          replace = FALSE)
      }


      #### Update rowNum ----
      rowNum(1)

      #### Disable Filter button ----
      updateButton(
        session = session,
        inputId = "filter",
        disabled = TRUE
      )

      #### Update Various Buttons ----
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
      updateSelectInput(
        session = session,
        inputId = "answer",
        choices = c("Select distribution", distributionChosen)
      )
    }
  })

  ### Hint button ----
  observeEvent(input$hint, {
    sendSweetAlert(
      session = session,
      title = "Hint",
      text = p(questionBank[rowNum(), "hint"]),
      type = "info"
    )
    .generateStatement(session, object = "hint", verb = "interacted", description = "Hint", value = questionBank[rowNum(), "hint"])
  })

  ### Submit button ----
  observeEvent(input$submit, {
    updateButton(
      session = session,
      inputId = "nextQ",
      disabled = FALSE
    )
    updateButton(
      session = session,
      inputId = "submit",
      disabled = TRUE
    )
    if (!is.null(input$answer)) {
      if (input$answer == questionBank[rowNum(), "distribution"]) {
        score(score() + 1)
        output$mark <- renderUI({
          img(src = "check.png", width = 30, alt = "Correct")
        })
        output$feedback <- renderUI({
          p(paste("Correct;", questionBank[rowNum(), "feedback"]))
        })
      } else {
        mistakes(mistakes() + 1)
        output$mark <- renderUI({
          img(src = "cross.png", width = 30, alt = "Incorrect")
        })
        output$feedback <- renderUI({
          p(paste("Incorrect;", questionBank[rowNum(), "feedback"]))
        })
      }
    }
  })

  ### Next question button ----
  observeEvent(input$nextQ, {
    updateButton(
      session = session,
      inputId = "submit",
      disabled = FALSE)
    updateButton(
      session = session,
      inputId = "nextQ",
      disabled = TRUE)
    updateSelectInput(
      session = session,
      inputId = "answer",
      selected = "Select distribution"
    )

    if(rowNum() == numberRow && gameOver == FALSE){
      sendSweetAlert(
        session = session,
        title = "Out of Questions",
        type = "warning",
        text = paste("You have gone through all of the questions that we currently",
                     "have for the distributions you selected. Please restart",
                     "the game.")
      )
      updateButton(
        session = session,
        inputId = "submit",
        disabled = TRUE
      )
      updateButton(
        session = session,
        inputId = "nextQ",
        disabled = TRUE
      )
    } else {
      rowNum(rowNum() + 1)
      output$mark <- renderUI({
        img(src = NULL, width = 30)
      })
      output$feedback <- renderUI({
        return(NULL)
      })
      output$math1 <- renderUI(
        withMathJax()
      )
      output$math2 <- renderUI(
        withMathJax()
      )
    }
  })

  ### Restart button ----
  observeEvent(input$restart, {
    #### Update buttons and inputs ----
    updateButton(
      session = session,
      inputId = "submit",
      disabled = TRUE)
    updateButton(
      session = session,
      inputId = "hint",
      disabled = TRUE
    )
    updateCheckboxGroupInput(
      session = session,
      inputId = "discreteList",
      selected = discDists$distribution)
    updateCheckboxGroupInput(
      session = session,
      inputId = "continuousList",
      selected = contDists$distribution)
    updateButton(
      session = session,
      inputId = "selectAllD",
      label = "Unselect all")
    updateButton(
      session = session,
      inputId = "selectAllC",
      label = "Unselect all")
    updateSelectInput(
      session = session,
      inputId = "answer",
      choices = c("Select distributions and filter first"))
    updateButton(
      session = session,
      inputId = "filter",
      disabled = FALSE
    )

    #### Clear outputs ----
    output$question <- renderUI({
      return(NULL)
    })
    output$feedback <- renderUI({
      return(NULL)
    })
    output$mark <- renderUI({
      img(src = NULL, width = 30)
    })

    #### Reset variables ----
    ##### Bob, depreciated variables are marked, delete once you've found the alt
    score(0)
    mistakes(0)
    rowNum(0)
    gameOver <- FALSE
    id <- 0 ## Depreciated, use rowNum() for question presentation order
            ## Use questionBank[rowNum(), "problemID"] for universal question ID
    numberRow <- 0
    value[["mistake"]] <<- 0  ## Depreciated, use mistakes()
    value$correct <<- 0 ## Depreciated, use score()

    #### RLocker Statement ----
    .generateStatement(session, object = "restart", verb = "interacted", description = "Game has been restarted.")
  })

  ## Bob, I'm not sure the next five lines are yours or if they are leftovers from
  ## somewhere. My code does NOT use them. If they aren't yours, delete.
  ## If they are yours, update
  value <<- reactiveValues(index = 1, mistake = 0, correct = 0)
  correct_answer <- as.matrix(questionBank[1:91, 4])
  hint <- as.matrix(questionBank[1:91, 6])
  index_list <- reactiveValues(list = sample(2:91, 89, replace = FALSE))
  feedbacks <- as.matrix(questionBank[1:91, 7])

  ## Render Count of Correct ----
  output$correctCount <- renderUI({
    p("Number of correct answers: ", paste0(score(),";"),
      "Number of mistakes made: ", mistakes())
  })

  ## Render Context ----
  output$context <- renderUI({

    if(is.null(rowNum()) || rowNum() == 0){
      NULL
    } else {
      withMathJax(p(questionBank[rowNum(), "context"]))
    }
  })

  ## Game Over Messages ----
  observeEvent(mistakes(), {
    if(mistakes() == maxTries){
      gameOver <- TRUE
      sendSweetAlert(
        session = session,
        title = "Lost",
        type = "error",
        text = "You have lost. Please click Restart to start over and try again."
      )
      updateButton(
        session = session,
        inputId = "submit",
        disabled = TRUE
      )
      updateButton(
        session = session,
        inputId = "nextQ",
        disabled = TRUE
      )
    }
  })

  observeEvent(score(), {
    if(score() == winLimit){
      gameOver <- TRUE
      sendSweetAlert(
        session = session,
        title = "Winner!",
        type = "success",
        text = "You have won the game. Please click Restart to play again."
      )
      updateButton(
        session = session,
        inputId = "submit",
        disabled = TRUE
      )
      updateButton(
        session = session,
        inputId = "nextQ",
        disabled = TRUE
      )
    }
  })

  ## Bob, I removed the prior section that this was in; I'm not sure where you
  ## want to put these calls
      # .generateAnsweredStatement(
      #   session,
      #   object = "submit",
      #   verb = "answered",
      #   description = questionBank[id, "context"],
      #   response = input$answer,
      #   interactionType = "choice",
      #   success = success,
      #   completion = gameOver
      # )
      #
      # if (gameOver) {
      #   if (WIN) {
      #     .generateStatement(session, object = "game", verb = "completed", description = "Player has won the game.")
      #   } else {
      #     .generateStatement(session, object = "game", verb = "completed", description = "Player has lost the game.")
      #   }
      # }

  ## Display the pictures ----
  output$gameProgress <- renderUI({
    if (mistakes() == 0) {
      img(
        src = "Cell01.jpg",
        width = "100%",
        alt = "The man is on the top branch"
      )
    } else if (mistakes() == 1) {
      img(
        src = "Cell02.jpg",
        width = "100%",
        alt = "The man has fallen one branch"
      )
    } else if (mistakes() == 2) {
      img(
        src = "Cell03.jpg",
        width = "100%",
        alt = "The man has fallen another branch, only one remaining"
      )
    } else if (mistakes() == 3) {
      img(
        src = "Cell04.jpg",
        width = "100%",
        alt = "The man has fallen to the last branch"
      )
    } else if (mistakes() == 4) {
      img(
        src = "Cell05.jpg",
        width = "100%",
        alt = "The man has fallen to the ground"
      )
    }
  })
}

boastUtils::boastApp(server = server, ui = ui)
