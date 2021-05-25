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

numberRow <- numeric()
hint <- c()
correct_answer <- c()
bank <- read.csv("distributionG.csv")
bank <- data.frame(lapply(bank, as.character), stringsAsFactors = FALSE)

ui <- dashboardPage(
  skin = "blue",
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
  dashboardSidebar(sidebarMenu(
    id = "tabs",
    menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
    menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
    menuItem("Challenge", tabName = "matchingdist", icon = icon("cogs"))
    ),
    tags$div(
      class = "sidebar-logo",
      boastUtils::sidebarFooter()
    )),
  
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

    tabItems(
      # Overview Tab
      tabItem(
        tabName = "overview",
        br(),
        br(),
        br(),
        h3("Matching Distributions"),
        p(
          "In this App, you will gain practice at associating context with different probability distributions. "
        ),
        br(),
        h3("Instructions:"),
        tags$ul(
          tags$li(
            "You'll start this game with a little man on the top of a tree,  and you are trying to prevent his fall to the ground.  If you provide a wrong answer, he falls to a lower branch and eventually to the ground. If you get 10 questions correct before he falls to the ground, you have won the game and saved the little man!"
          ),
          tags$li(
            "Please select which probability distribution(s) you 'd like to work on and hit the “filter” button."
          ),
          tags$li(
            "Read the given text and choose a distribution from the dropdown menu. Make sure you understand the scenario."
          ),
          tags$li(
            "If you need some extra help, click the 'hint' button (shown as a question mark symbol)."
          ),
          tags$li(
            "After you select the distribution, click 'Submit' to check your answer. "
          ),
          tags$li(
            "Once you click 'Submit', you cannot revise your answer. You can only click 'Next Question' to move on to your next challenge. "
          )
        ),
        div(
          style = "text-align:center",
          actionButton(
            "go",
            "GO!",
            icon("bolt"),
            size = "large",
            style = "color: #fff; background-color: #337ab7; border-color: #2e6da4",
            class = "circle grow"
          )
        ),
        br(),
        h3("Acknowledgements:"),
        p(
          "This app was developed and coded by Zhiliang Zhang and futher updated by Yiyang Wang, Yuqing Lei and Shravani Samala in 2021."
        )
      ),

      # Challenge Tab
      tabItem(
        tabName = "matchingdist",
        fluidRow(
          column(
            width = 6,
            p("Please select the distributions you'd like to use in this app and click Filter")
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
                actionButton("selectAllD", "Unselect",
                  size =
                    "small"
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
                    "Negative Binomial"
                  ),
                  selected = c(
                    "Bernoulli",
                    "Binomial",
                    "Discrete Uniform",
                    "Poisson",
                    "Geometric",
                    "Negative Binomial"
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
                  choices = c("Continuous Uniform", "Gamma", "Exponential", "Normal", "Beta"),
                  selected = c("Continuous Uniform", "Gamma", "Exponential", "Normal", "Beta"),
                )
              )
            ),
            verbatimTextOutput(outputId = "res2")
          ),
          column(
            2,
            offset = 1,
            bsButton(
              "filter",
              "Filter",
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
                    circleButton(
                      "hint",
                      icon = icon("question"),
                      status = "myClass",
                      size = "xs"
                    )
                  )
                )
              )),

              fluidRow(
                tags$style(type = "text/css", ".selectize-dropdown-content {max-height: 500px; }"),
                column(
                  8,
                  uiOutput("answerbox"),
                  selectInput("answer", "", c("Select Distribution"),
                    width =
                      "100%"
                  )
                ),
                br(),
                column(1, uiOutput("mark")),
                column(
                  3,
                  bsButton(
                    "submit",
                    "Submit",
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
                    "nextq",
                    "Next Question",
                    size = "large",
                    style = "success",
                    disabled = TRUE
                  )
                ),
                column(
                  4,
                  bsButton(
                    "restart",
                    "Restart the game",
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
        )
      )
    )
  )
)

server <- function(session, input, output) {

  # Learning Locker Statement Generation
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

  observeEvent(input$info, {
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "In this app the user is challenged to identify the probability model for various contextual situations.",
      type = "info"
    )
  })

  observeEvent(input$go, {
    updateTabItems(session, "tabs", "matchingdist")
    updateButton(session, "submit", disabled = TRUE)
  })

  observeEvent(input$submit, {
    updateButton(session, "nextq", disabled = FALSE)
  })

  observeEvent(input$submit, {
    updateButton(session, "submit", disabled = TRUE)
  })

  observeEvent(input$nextq, {
    updateButton(session, "submit", disabled = FALSE)
    updateButton(session, "nextq", disabled = TRUE)
  })

  observeEvent(input$restart, {
    updateButton(session, "submit", disabled = TRUE)
    updateButton(session, "restart", disabled = FALSE)
    updateButton(session, "filter", disabled = FALSE)

    GAME_OVER <<- FALSE
    .generateStatement(session, object = "restart", verb = "interacted", description = "Game has been restarted.")

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

    updateCheckboxGroupInput(session, inputId = "discretelist", label = NULL, choices = c("Bernoulli", "Binomial", "Discrete Uniform", "Poisson", "Geometric", "Negative Binomial"), selected = c("Bernoulli", "Binomial", "Discrete Uniform", "Poisson", "Geometric", "Negative Binomial"))
    updateCheckboxGroupInput(session, inputId = "continuouslist", label = NULL, choices = c("Continuous Uniform", "Gamma", "Exponential", "Normal", "Beta"), selected = c("Continuous Uniform", "Gamma", "Exponential", "Normal", "Beta"))

    updateButton(session, "selectAllD", label = "Unselect")
    updateButton(session, "selectAllC", label = "Unselect")
    updateCheckboxGroupInput(session, inputId = "discretelist", label = NULL, choices = c("Bernoulli", "Binomial", "Discrete Uniform", "Poisson", "Geometric", "Negative Binomial"), selected = c("Bernoulli", "Binomial", "Discrete Uniform", "Poisson", "Geometric", "Negative Binomial"))
    updateCheckboxGroupInput(session, inputId = "continuouslist", label = NULL, choices = c("Continuous Uniform", "Gamma", "Exponential", "Normal", "Beta"), selected = c("Continuous Uniform", "Gamma", "Exponential", "Normal", "Beta"))

    updateSelectInput(session, "answer", "", c("Select Distribution"))

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
    p("Choose the distribution from the list to match the given text, then click 'Submit' to check your answer.")
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
      updateButton(session, "selectAllD", label = "Unselect")
      updateCheckboxGroupInput(session, "discretelist", choices = c("Bernoulli", "Binomial", "Discrete Uniform", "Poisson", "Geometric", "Negative Binomial"), selected = c("Bernoulli", "Binomial", "Discrete Uniform", "Poisson", "Geometric", "Negative Binomial"))
    }
    else if (input$selectAllD %% 2 == 1) {
      updateButton(session, "selectAllD", label = "Select All")
      updateCheckboxGroupInput(session, "discretelist", choices = c("Bernoulli", "Binomial", "Discrete Uniform", "Poisson", "Geometric", "Negative Binomial"))
    }
    else {
      updateButton(session, "selectAllD", label = "Unselect")
      updateCheckboxGroupInput(session, "discretelist", choices = c("Bernoulli", "Binomial", "Discrete Uniform", "Poisson", "Geometric", "Negative Binomial"), selected = c("Bernoulli", "Binomial", "Discrete Uniform", "Poisson", "Geometric", "Negative Binomial"))
    }
  })
  ####### select AllContinuous#####
  observeEvent(input$selectAllC, {
    if (input$selectAllC == 0) {
      updateButton(session, "selectAllC", label = "Unselect")
      updateCheckboxGroupInput(session, "continuouslist", choices = c("Continuous Uniform", "Gamma", "Exponential", "Normal", "Beta"), selected = c("Continuous Uniform", "Gamma", "Exponential", "Normal", "Beta"))
    }
    else if (input$selectAllC %% 2 == 1) {
      updateButton(session, "selectAllC", label = "Select All")
      updateCheckboxGroupInput(session, "continuouslist", choices = c("Continuous Uniform", "Gamma", "Exponential", "Normal", "Beta"))
    }
    else {
      updateButton(session, "selectAllC", label = "Unselect")
      updateCheckboxGroupInput(session, "continuouslist", choices = c("Continuous Uniform", "Gamma", "Exponential", "Normal", "Beta"), selected = c("Continuous Uniform", "Gamma", "Exponential", "Normal", "Beta"))
    }
  })

  ######## Mixture of Dropdown and Checkbox########

  # TO DO: revise this code so that it is NOT hard coded, but rather makes use columns in the data frame.
  observeEvent(input$filter, {
    discretechosen <- input$discretelist
    continuouschosen <- input$continuouslist
    distributionchosen <<- c(discretechosen, continuouschosen)

    .generateStatement(session, object = "filter", verb = "interacted", description = "Please select the distributions you'd like to use in this app and click Filter", value = paste(distributionchosen, sep = ", ", collapse = ", "))

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
    updateButton(session, "submit", disabled = FALSE)
    # print(numberRow)
    ### Select questions from edited databank###
    output$question <- renderUI({
      withMathJax()
      id <<- sample(numberRow, 1, replace = FALSE, prob = NULL)
      updateSelectInput(session, "answer",
        label = NULL, choices = c("Select distribution", distributionchosen),
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
    .generateStatement(session, object = "hint", verb = "interacted", description = "Hint", value = bank[id, 6])
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
      updateButton(session, "submit", disabled = TRUE)
      updateButton(session, "nextq", disabled = TRUE)
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
      updateButton(session, "submit", disabled = FALSE)
      updateSelectInput(session, "answer",
        label = NULL, choices = c("Select distribution", distributionchosen),
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
      updateButton(session, "nextq", disabled = TRUE)
      output$result <- renderUI({
        h3("You have lost this Game. You need to click 'restart' button to start this game from the beginning.")
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

          updateButton(session, "submit", disabled = TRUE)
          updateButton(session, "nextq", disabled = TRUE)
          updateButton(session, "restart", disabled = FALSE)
        }
      } else {
        value[["mistake"]] <<- value[["mistake"]] + 1

        if (value[["mistake"]] == MAX_TRIES) {
          GAME_OVER <<- TRUE
          WIN <- FALSE

          updateButton(session, "submit", disabled = TRUE)
          updateButton(session, "nextq", disabled = TRUE)
          updateButton(session, "restart", disabled = FALSE)
          updateButton(session, "filter", disabled = TRUE)

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
          .generateStatement(session, object = "game", verb = "completed", description = "Player has won the game.")
        } else {
          .generateStatement(session, object = "game", verb = "completed", description = "Player has lost the game.")
        }
      }


      ### FEEDBACK###
      output$feedback <- renderUI({
        correct_answer <<- bank[id, 4]
        if (input$answer == correct_answer) {
          p("CORRECT!", br(), bank[id, 7])
        }
        else {
          p(strong("Hint:"), br(), bank[id, 6])
        }
        # withMathJax()
        # h4(strong('Feedback',br(),bank[id,7]))
      })

      output$result <- renderUI({
        h3("Congratulations! You got this one correct. Click 'Next Question' to move on your challenge.")
      })
    }
  })

  ##### Draw the Hangman Game#####

  output$distPlot <- renderUI({
    if (value[["mistake"]] == 0) {
      img(src = "Cell01.jpg")
    }

    else if (value[["mistake"]] == 1) {
      img(src = "Cell02.jpg")
    }

    else if (value[["mistake"]] == 2) {
      img(src = "Cell03.jpg")
    }

    else if (value[["mistake"]] == 3) {
      img(src = "Cell04.jpg")
    }

    else if (value[["mistake"]] == 4) {
      img(src = "Cell05.jpg")
    }
    else if (value[["mistake"]] == 5) {
      img(src = "GAMEOVER.png")
    }
  })
}

boastApp(server = server, ui = ui)
