library(shiny)
library(shinyjs)
library(shinyBS)
library(plotrix)
library(shinyWidgets)
library(rlocker)

numberRow<-numeric()
hint<-c()
correct_answer<-c()
bank <- read.csv("distribution - distribution.csv")
bank <- data.frame(lapply(bank, as.character), stringsAsFactors = FALSE)

shinyServer(function(session, input, output) {
  
  #####add rlocker#####
  # Gets current page address from the current session
  getCurrentAddress <- function(session){
    return(paste0(
      session$clientData$url_protocol, "//",
      session$clientData$url_hostname,
      session$clientData$url_pathname, ":",
      session$clientData$url_port,
      session$clientData$url_search
    ))
  }
  
  # Initialize Learning Locker connection
  connection <- rlocker::connect(session, list(
    base_url = "https://learning-locker.stat.vmhost.psu.edu/",
    auth = "Basic ZDQ2OTNhZWZhN2Q0ODRhYTU4OTFmOTlhNWE1YzBkMjQxMjFmMGZiZjo4N2IwYzc3Mjc1MzU3MWZkMzc1ZDliY2YzOTNjMGZiNzcxOThiYWU2",
    agent = rlocker::createAgent()
  ))
  
  # Setup demo app and user.
  currentUser <- 
    connection$agent
  
  if(connection$status != 200){
    warning(paste(connection$status, "\nTry checking your auth token.")) 
  }
  
  ###end rlocker set up##
  
  observeEvent(input$go, {
    updateTabItems(session, "tabs", "matchingdist")
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
    
   observeEvent(input$restart,{
     
     updateButton(session, "submit", disabled = FALSE)
     updateButton(session,"restart",disabled =FALSE)
     updateButton(session, "filter", disabled = FALSE)
     
     output$question <- renderUI({
       return(NULL)
     })
     
     output$feedback <- renderUI({
       return(NULL)
     })
     observeEvent(input$hint,{
         h4("please select the distribution")
     })
     updateCheckboxGroupInput(session, inputId = "discretelist", label = NULL, choices = c("Bernoulli", "Binomial", "Discrete Uniform", "Poisson", "Geometric", "Negative Binomial"), selected = NULL)
     updateCheckboxGroupInput(session, inputId = "continuouslist", label = NULL, choices =  c("Continuous Uniform", "Gamma", "Exponential", "Normal"), selected = NULL)
     
     updateSelectInput(session,"answer","",c('Select Distribution'))
     
     output$mark <- renderUI({
       img(src = NULL,width = 30)
     })
     numberRow<<-numeric()
     value[["mistake"]] <<-0
     print(value[["mistake"]])
     value$correct <<- 0
   })
  
  output$result <- renderUI({
    
    h3("Choose the distribution from the list to match the given text, then click 'Submit' to check your answer.")
  })
  
  value <- reactiveValues(index =  1, mistake = 0,correct = 0)
  correct_answer <- as.matrix(bank[1:91,4])
  hint <- as.matrix(bank[1:91,6])
  index_list<-reactiveValues(list=sample(2:91,89,replace=FALSE))
  feedbacks<-as.matrix(bank[1:91,7])
  
  observeEvent(input$go,{
    value$index <- 1
    value$answerbox = value$index
  })
  
  output$correct <- renderUI({
    
    h3("Number of correct answers:" ,"", value$correct )
  })
  
  #######select AllDiscrete#####
  observeEvent(input$selectAllD,{
    if(input$selectAllD == 0) return(NULL) 
    else if (input$selectAllD%%2 == 0)
    {
      updateButton(session, "selectAllD", label="Select All")
      updateCheckboxGroupInput(session,"discretelist", choices=c("Bernoulli", "Binomial", "Discrete Uniform", "Poisson", "Geometric", "Negative Binomial"))
    }
    else
    {
      updateButton(session, "selectAllD", label="Unselect")
      updateCheckboxGroupInput(session,"discretelist",choices=c("Bernoulli", "Binomial", "Discrete Uniform", "Poisson", "Geometric", "Negative Binomial"),selected=c("Bernoulli", "Binomial", "Discrete Uniform", "Poisson", "Geometric", "Negative Binomial"))
      }
  })
  #######select AllContinuous#####
  observeEvent(input$selectAllC,{
    if(input$selectAllC == 0) return(NULL) 
    else if (input$selectAllC%%2 == 0)
    {
      updateButton(session, "selectAllC", label="Select All")
      updateCheckboxGroupInput(session,"continuouslist", choices=c("Continuous Uniform", "Gamma", "Exponential", "Normal"))
    }
    else
    {
      updateButton(session, "selectAllC", label="Unselect")
      updateCheckboxGroupInput(session,"continuouslist",choices=c("Continuous Uniform", "Gamma", "Exponential", "Normal"), selected=c("Continuous Uniform", "Gamma", "Exponential", "Normal"))
      }
  })
  
  ######## Mixture of Dropdown and Checkbox########
  observeEvent(input$filter,{
    discretechosen=input$discretelist
    continuouschosen=input$continuouslist
    distributionchosen <<- c(discretechosen, continuouschosen)
    if ("Bernoulli" %in% distributionchosen){
      numberRow <- c(numberRow, 1:6)
    }
    if ("Beta" %in% distributionchosen){
      numberRow <- c(numberRow, 7:10)
    }
    if ("Binomial" %in% distributionchosen){
      numberRow <- c(numberRow, 11:20)
    }
    if ("Continuous Uniform" %in% distributionchosen){
      numberRow <- c(numberRow, 21:29)
    }
    if ("Discrete Uniform" %in% distributionchosen){
      numberRow <- c(numberRow, 31:34)
    }
    if ("Exponential" %in% distributionchosen){
      numberRow <- c(numberRow, 35:42)
    }
    if ("Gamma" %in% distributionchosen){
      numberRow <- c(numberRow, 43:49)
    }
    if ("Geometric" %in% distributionchosen){
      numberRow <- c(numberRow, 50:56)
    }
    if ("Hypergeometric" %in% distributionchosen){
      numberRow <- c(numberRow, 57:60)
    }
    if ("Negative Binomial" %in% distributionchosen){
      numberRow <- c(numberRow, 61:68)
    }
    if ("Normal" %in% distributionchosen){
      numberRow <- c(numberRow, 69:80)
    }
    if ("Poisson" %in% distributionchosen){
      numberRow <- c(numberRow, 81:90)
    }
    if ("Weibull" %in% distributionchosen){
      numberRow <- c(numberRow, 91)
    }
    numberRow<<-numberRow
    print(numberRow)
    ###Select questions from edited databank###
    output$question <- renderUI({
        id <<-sample(numberRow, 1, replace = FALSE, prob = NULL)
        numberRow<<-numberRow[!numberRow %in% id]
        print(numberRow)
        updateSelectInput(session, "answer", label = NULL, choices = c("Select distribution",distributionchosen),
                            selected = NULL)
        output$mark <- renderUI({
          img(src = NULL,width = 30)
        })
        return(bank[id,3])
        })
    })
  
  ###PRINT HINT####
   observeEvent(input$hint,{
     sendSweetAlert(
       session = session,
       title = "Hint:",
       closeOnClickOutside = TRUE,
       h4(bank[id,6])
     )
   })
    
  ###SUBMIT BUTTON###
  
  
   observeEvent(input$submit,{ 
     correct_answer<<-bank[id,4]
      output$mark <- renderUI({
        if (!is.null(input$answer)){
          if (input$answer == correct_answer){
            img(src = "check.png",width = 30)
          }
          else{
            img(src = "cross.png",width = 30)}}})
      
      answer<-isolate(input$answer)

      statement <- rlocker::createStatement(
        list(
          verb = list(
            display = "answered"
          ),
          object = list(
            id = paste0(getCurrentAddress(session), "#", id),
            name =  paste('Question', id, list(distributionchosen)),
            description = bank[id, 3]
          ),
          result = list(
            success = any(answer == correct_answer),
            response = answer
          )
        )
      )
      
      # Store statement in locker and return status
      status <- rlocker::store(session, statement)
      
      print(statement) # remove me
      print(status) # remove me
      }
      )
   
   ###NEXT QUESTION BUTTON###
    observeEvent(input$nextq,{
      if (length(numberRow)==1){
        sendSweetAlert(
          session = session,
          title = "Error:",
          type = "error",
          closeOnClickOutside = TRUE,
          h4('We have run out of questions. Please restart it')
        )
        updateButton(session, "submit", disabled = TRUE)
        updateButton(session,"nextq",disabled =TRUE)
      }
      else{
      id <<-sample(numberRow, 1, replace = FALSE, prob = NULL)
      print(id)
      hint<-bank[id,6]
      numberRow<<-numberRow[!numberRow %in% id]
      print(numberRow)
      output$question <- renderUI({
        return(bank[id,3])
      })
      updateButton(session, "submit", disabled = FALSE)
      updateSelectInput(session, "answer", label = NULL, choices = c("Select distribution", distributionchosen),
                        selected = NULL)
         output$mark <- renderUI({
           img(src = NULL,width = 30)
         })
         ###FEEDBACK###
         output$feedback <- renderUI({
           return(NULL)})}
      })
  
    observeEvent(input$submit,{
          
      if(value[["mistake"]] == 4){
        
        updateButton(session, "nextq", disabled = TRUE)
        
        output$result <- renderUI({
          h3("You have lost this Game. You need to click 'restart' button to start this game from the beginning.")
        })
      }
    })
  
  ###########Counting Correct answers############
   observeEvent(input$submit,{
     if (!is.null(input$answer)){
       correct_answer<<-bank[id,4]
       if (input$answer == correct_answer){
          value$correct <<- value$correct + 1
          if (value$correct==10){
            sendSweetAlert(
              session = session,
              title = "Success:",
              type = "success",
              closeOnClickOutside = TRUE,
              h4('Congrats! You Win! Please click Restart to start over.')
            )
            updateButton(session, "submit", disabled = TRUE)
            updateButton(session, "nextq", disabled = TRUE)
            updateButton(session, "restart", disabled = FALSE)
          }
          }
       
       if(input$answer != correct_answer){
           value[["mistake"]] <<- value[["mistake"]]+1
           if (value[["mistake"]]==4){
             sendSweetAlert(
               session = session,
               title = "Lost:",
               type = "error",
               closeOnClickOutside = TRUE,
               h4('You lost. Please click Restart to start over')
             )
             updateButton(session, "submit", disabled = TRUE)
             updateButton(session, "nextq", disabled = TRUE)
             updateButton(session, "restart", disabled = FALSE)
           }
       }
       ###FEEDBACK###
       output$feedback <- renderUI({

         h4(strong('Feedback',br(),bank[id,7]))})
       
       output$result <- renderUI({
         h3("Congratulation! You got this one correct. Click 'Next Question' to move on your challenge")
       })
     }
     if(value$correct == 8){
       updateButton(session, "nextq", disabled = TRUE)
       updateButton(session, "restart", disabled = FALSE)
     }
   })
  
  ##### Draw the Hangman Game#####
  
  output$distPlot <- renderUI({
    
    if(value[["mistake"]] == 0){
      img(src = "Cell01.jpg")
    }
    
    else if(value[["mistake"]] ==1 ) {
      img(src = "Cell02.jpg")
    }
    
    else if(value[["mistake"]] == 2) {
      img(src="Cell03.jpg")
    }
    
    else if(value[["mistake"]] ==3 ) {
      img(src="Cell04.jpg")
    }
    
    else if(value[["mistake"]] ==4) {
      img(src= "Cell05.jpg")
    }
    else if(value[["mistake"]] ==5) {
      img(src= "GAMEOVER.png")
    }
  })
  })
