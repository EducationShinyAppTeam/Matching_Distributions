library(shiny)
library(shinyjs)
library(shinyBS)
library(plotrix)
library(shinyWidgets)
library(shinydashboard)
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(V8)
library(png)
library(ggplot2)

numberRow<-numeric()
hint<-c()
correct_answer<-c()
bank <- read.csv("distributionG.csv")
bank <- data.frame(lapply(bank, as.character), stringsAsFactors = FALSE)

shinyServer(function(session, input, output) {
  
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "Choose the probability you'd like to use, then answer the questions by choosing the correct distribution",
      type = "info"
    )
  })
  
  observeEvent(input$go, {
    updateTabItems(session, "tabs", "matchingdist")
    updateButton(session, 'submit', disabled=TRUE)
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
    
    updateButton(session, "submit", disabled = TRUE)
    updateButton(session,"restart",disabled =FALSE)
    updateButton(session, "filter", disabled = FALSE)
    
    output$question <- renderUI({
      withMathJax()
      return(NULL)
      output$test1 <- renderUI({withMathJax()})
      output$test2 <- renderUI({withMathJax()})
    })
    
    output$feedback <- renderUI({
      withMathJax()
      return(NULL)
      output$test1 <- renderUI({withMathJax()})
      output$test2 <- renderUI({withMathJax()})
    })
    observeEvent(input$hint,{
      p("Please select the distribution")
    })
    updateCheckboxGroupInput(session, inputId = "discretelist", label = NULL, choices = c("Bernoulli", "Binomial", "Discrete Uniform", "Poisson", "Geometric", "Negative Binomial"), selected = NULL)
    updateCheckboxGroupInput(session, inputId = "continuouslist", label = NULL, choices =  c("Continuous Uniform", "Gamma", "Exponential", "Normal","Beta"), selected = NULL)
    
    updateSelectInput(session,"answer","",c('Select Distribution'))
    
    output$mark <- renderUI({
      img(src = NULL,width = 30)
    })
    id<<-0
    numberRow<<-numeric()
    value[["mistake"]] <<-0
    print(value[["mistake"]])
    value$correct <<- 0
  })
  
  output$result <- renderUI({
    withMathJax()
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
    withMathJax()
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
      updateCheckboxGroupInput(session,"continuouslist", choices=c("Continuous Uniform", "Gamma", "Exponential", "Normal","Beta"))
    }
    else
    {
      updateButton(session, "selectAllC", label="Unselect")
      updateCheckboxGroupInput(session,"continuouslist",choices=c("Continuous Uniform", "Gamma", "Exponential", "Normal","Beta"), selected=c("Continuous Uniform", "Gamma", "Exponential", "Normal","Beta"))
    }
  })
  
  ######## Mixture of Dropdown and Checkbox########
  
  #TO DO: revise this code so that it is NOT hard coded, but rather makes use columns in the data frame.
  observeEvent(input$filter,{
    discretechosen=input$discretelist
    continuouschosen=input$continuouslist
    distributionchosen <<- c(discretechosen, continuouschosen)
    #numberRow <- dplyr::filter(bank, bank$distribution %in% distributionchosen)
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
      numberRow <- c(numberRow, 30:34)
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
    numberRow<<-numberRow
    updateButton(session, 'submit', disabled = FALSE)
    print(numberRow)
    ###Select questions from edited databank###
    output$question <- renderUI({
      withMathJax()
      id <<-sample(numberRow, 1, replace = FALSE, prob = NULL)
      updateSelectInput(session, "answer", label = NULL, choices = c("Select distribution",distributionchosen),
                        selected = NULL)
      output$mark <- renderUI({
        img(src = NULL,width = 30)
      })
      print(id)
      numberRow<<-numberRow[!numberRow %in% id]
      print(numberRow)
      return(h3(strong(bank[id,3])))
    })
    output$test1 <- renderUI({withMathJax()})
    output$test2 <- renderUI({withMathJax()})
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
    output$mark <- renderUI({
      correct_answer<<-bank[id,4]
      if (!is.null(input$answer)){
        if (input$answer == correct_answer){
          img(src = "check.png",width = 30)
        }
        else{
          img(src = "cross.png",width = 30)}}})})
  
  ###NEXT QUESTION BUTTON###
  observeEvent(input$nextq,{
    if (length(numberRow)==1){
      sendSweetAlert(
        session = session,
        title = "Warning:",
        # type = "error",
        closeOnClickOutside = TRUE,
        h4('We have run out of questions. Please restart.')
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
        withMathJax()
        return(h3(strong(bank[id,3])))
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
          updateButton(session, "submit", disabled = TRUE)
          updateButton(session, "nextq", disabled = TRUE)
          updateButton(session, "restart", disabled = FALSE)
          updateButton(session, "filter", disabled = TRUE)
          sendSweetAlert(
            session = session,
            title = "Lost:",
            type = "error",
            closeOnClickOutside = TRUE,
            h4('You lost. Please click Restart to start over')
          )
          
        }
      }
      ###FEEDBACK###
      output$feedback <- renderUI({

        correct_answer<<-bank[id,4]
        if (input$answer == correct_answer){
          h4(strong("CORRECT!",br(),bank[id,7]))}
        else{h4(strong("Hint:",br(),bank[id,6]))}
          

        # withMathJax()
        # h4(strong('Feedback',br(),bank[id,7]))
        })

      
      output$result <- renderUI({
        h3("Congratulation! You got this one correct. Click 'Next Question' to move on your challenge")
      })
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
