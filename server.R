library(shinydashboard)
library(shiny)
library(shinyjs)
library(shinyBS)
library(plotrix)
library(shinyWidgets)

bank <- read.csv("distribution.csv")
bank = data.frame(lapply(bank, as.character), stringsAsFactors = FALSE)


shinyServer(function(session, input, output) {
  
  
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
    updateSelectInput(session,"answer","",c('Select Distribution','Continuous Uniform','Binomial','Exponential','Discrete Uniform','Geometric','Negative Binomial',
                                            'Poisson','Gamma','Normal','Geometric','Bernoulli'))
    
    output$result <- renderUI({
      
      h3("Choose the distribution from the list to match the given text, then click 'Submit' to check your answer.")
    })
    
    index_list$list=index_list$list[-1]   
    value$index <- index_list$list[1]
  })
  
  observeEvent(input$restart,{
    
    updateButton(session, "submit", disabled = FALSE)
    updateButton(session,"restart",disable =TRUE)
    
    updateSelectInput(session,"answer","",c('Select Distribution','Continuous Uniform','Binomial','Exponential','Discrete Uniform','Geometric','Negative Binomial',
                                            'Poisson','Gamma','Normal','Geometric','Bernoulli'))
    
    index_list$list<-c(index_list$list,sample(1:48,48,replace=FALSE))
    value$index <- 49
    value$answerbox = value$index
    correct_answer <- as.matrix(bank[1:49,4])
    output$mark <- renderUI({
      img(src = NULL,width = 30)
    })
    output$result <- renderUI({
      h3("Choose different measure of associations for each numeric value, then click 'Submit' to check your answer.")
    })
    value$correct <- 0
    
  })
  
  observeEvent(input$reset,{
    updateButton(session, "submit", disabled = FALSE)
    updateButton(session,"reset",disable =TRUE)
    updateSelectInput(session,"answer","",c('Select Distribution','Continuous Uniform','Binomial','Exponential','Discrete Uniform','Geometric','Negative Binomial',
                                            'Poisson','Gamma','Normal','Geometric','Bernoulli'))
    
    index_list$list<-c(index_list$list,sample(1:48,48,replace=FALSE))
    value$index <- 49
    value$answerbox = value$index
    correct_answer <- as.matrix(bank[1:49,4])
    output$mark <- renderUI({
      img(src = NULL,width = 30)
    })
    
    output$result <- renderUI({
      h3("Choose different measure of associations for each numeric value, then click 'Submit' to check your answer.")
    })
  })
  
  output$result <- renderUI({
    
    h3("Choose the distribution from the list to match the given text, then click 'Submit' to check your answer.")
  })
  
  value <- reactiveValues(index =  1, mistake = 0,correct = 0)
  correct_answer <- as.matrix(bank[1:49,4])
  
  index_list<-reactiveValues(list=sample(2:49,48,replace=FALSE))
  
  
  
  observeEvent(input$go,{
    value$index <- 1
    value$answerbox = value$index
    correct_answer <- as.matrix(bank[1:49,4])
    
  })
  
  observeEvent(input$nextq,{
    index_list$list=index_list$list[-1]   
    value$index <- index_list$list[1]
    value$answerbox= value$index
    
    if(length(index_list$list) == 1){
      updateButton(session, "nextq", disabled = TRUE)
      updateButton(session,"submit", disabled = TRUE)
      updateButton(session, "reset",disabled = FALSE)
      output$result <- renderUI({
        
        h3("Please click RELOAD to reload questions from database so you can continue this game")
      })
    }
    output$mark <- renderUI({
      img(src = NULL,width = 30)
    })
    
  })
  
  output$correct <- renderUI({
    
    h3("Number of correct answers:" ,"", value$correct )
  })
  
  hint <- as.matrix(bank[1:49,6])
  
  
  ########SELECT HINT#######
  
  observeEvent(input$inshint,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "Choose the distribution in the dropdown menu that matches the variable X in the context described. Then hit 'Submit' to check your answer.",
      type = "info"
    )
  })
  observeEvent(input$hint,{
    sendSweetAlert(
      session = session,
      title = "Hints:",
      type = "info",
      if(value$index == 1){
        h4(bank[1,6])
      }
      else if(value$index == 2){
        h4(bank[2,6])
      }
      else if(value$index == 3){
        h4(bank[3,6])
      }
      else if(value$index == 4){
        h4(bank[4,6])
      }
      else if(value$index == 5){
        h4(bank[5,6])
      }
      else if(value$index == 6){
        h4(bank[6,6])
      }
      else if(value$index == 7){
        h4(bank[7,6])
      }
      else if(value$index == 8){
        h4(bank[8,6])
      }
      else if(value$index == 9){
        h4(bank[9,6])
      }
      else if(value$index == 10){
        h4(bank[10,6])
      }
      else if(value$index == 11){
        h4(bank[11,6])
      }
      else if(value$index == 12){
        h4(bank[12,6])
      }
      else if(value$index == 13){
        h4(bank[13,6])
      }
      else if(value$index == 14){
        h4(bank[14,6])
      }
      else if(value$index == 15){
        h4(bank[15,6])
      }
      else if(value$index == 16){
        h4(bank[16,6])
      }
      else if(value$index == 17){
        h4(bank[17,6])
      }
      else if(value$index == 18){
        h4(bank[18,6])
      }
      else if(value$index == 19){
        h4(bank[19,6])
      }
      else if(value$index == 20){
        h4(bank[20,6])
      }
      else if(value$index == 21){
        h4(bank[21,6])
      }
      else if(value$index == 22){
        h4(bank[22,6])
      }
      else if(value$index == 23){
        h4(bank[23,6])
      }
      else if(value$index == 24){
        h4(bank[24,6])
      }
      else if(value$index == 25){
        h4(bank[25,6])
      }
      else if(value$index == 26){
        h4(bank[26,6])
      }
      else if(value$index == 27){
        h4(bank[27,6])
      }
      else if(value$index == 28){
        h4(bank[28,6])
      }
      else if(value$index == 29){
        h4(bank[29,6])
      }
      else if(value$index == 30){
        h4(bank[30,6])
      }
      else if(value$index == 31){
        h4(bank[31,6])
      }
      else if(value$index == 32){
        h4(bank[32,6])
      }
      else if(value$index == 33){
        h4(bank[33,6])
      }
      else if(value$index == 34){
        h4(bank[34,6])
      }
      else if(value$index == 35){
        h4(bank[35,6])
      }
      else if(value$index == 36){
        h4(bank[36,6])
      }
      else if(value$index == 37){
        h4(bank[37,6])
      }
      else if(value$index == 38){
        h4(bank[38,6])
      }
      else if(value$index == 39){
        h4(bank[39,6])
      }
      else if(value$index == 40){
        h4(bank[40,6])
      }
      else if(value$index == 41){
        h4(bank[41,6])
      }
      else if(value$index == 42){
        h4(bank[42,6])
      }
      else if(value$index == 43){
        h4(bank[43,6])
      }
      else if(value$index == 44){
        h4(bank[44,6])
      }
      else if(value$index == 45){
        h4(bank[45,6])
      }
      else if(value$index == 46){
        h4(bank[46,6])
      }
      else if(value$index == 47){
        h4(bank[47,6])
      }
      else if(value$index == 48){
        h4(bank[48,6])
      }
      else if(value$index == 49){
        h4(bank[49,6])
      }
      
    )
    
  })
  
  ###Select questions from databank###
  output$question <- renderUI({
    if(value$index == 1){
      h3(bank[1,3])
    }
    else if(value$index == 2){
      h3(bank[2,3])
    }
    else if(value$index == 3){
      h3(bank[3,3])
    }
    else if(value$index == 4){
      h3(bank[4,3])
    }
    else if(value$index == 5){
      h3(bank[5,3])
    }
    else if(value$index == 6){
      h3(bank[6,3])
    }
    else if(value$index == 7){
      h3(bank[7,3])
    }
    else if(value$index == 8){
      h3(bank[8,3])
    }
    else if(value$index == 9){
      h3(bank[9,3])
    }
    else if(value$index == 10){
      h3(bank[10,3])
    }
    else if(value$index == 11){
      h3(bank[11,3])
    }
    else if(value$index == 12){
      h3(bank[12,3])
    }
    else if(value$index == 13){
      h3(bank[13,3])
    }
    else if(value$index == 14){
      h3(bank[14,3])
    }
    else if(value$index == 15){
      h3(bank[15,3])
    }
    else if(value$index == 16){
      h3(bank[16,3])
    }
    else if(value$index == 17){
      h3(bank[17,3])
    }
    else if(value$index == 18){
      h3(bank[18,3])
    }
    else if(value$index == 19){
      h3(bank[19,3])
    }
    else if(value$index == 20){
      h3(bank[20,3])
    }
    else if(value$index == 21){
      h3(bank[21,3])
    }
    else if(value$index == 22){
      h3(bank[22,3])
    }
    else if(value$index == 23){
      h3(bank[23,3])
    }
    else if(value$index == 24){
      h3(bank[24,3])
    }
    else if(value$index == 25){
      h3(bank[25,3])
    }
    else if(value$index == 26){
      h3(bank[26,3])
    }
    else if(value$index == 27){
      h3(bank[27,3])
    }
    else if(value$index == 28){
      h3(bank[28,3])
    }
    else if(value$index == 29){
      h3(bank[29,3])
    }
    else if(value$index == 30){
      h3(bank[30,3])
    }
    else if(value$index == 31){
      h3(bank[31,3])
    }
    else if(value$index == 32){
      h3(bank[32,3])
    }
    else if(value$index == 33){
      h3(bank[33,3])
    }
    else if(value$index == 34){
      h3(bank[34,3])
    }
    else if(value$index == 35){
      h3(bank[35,3])
    }
    else if(value$index == 36){
      h3(bank[36,3])
    }
    else if(value$index == 37){
      h3(bank[37,3])
    }
    else if(value$index == 38){
      h3(bank[38,3])
    }
    else if(value$index == 39){
      h3(bank[39,3])
    }
    else if(value$index == 40){
      h3(bank[40,3])
    }
    else if(value$index == 41){
      h3(bank[41,3])
    }
    else if(value$index == 42){
      h3(bank[42,3])
    }
    else if(value$index == 43){
      h3(bank[43,3])
    }
    else if(value$index == 44){
      h3(bank[44,3])
    }
    else if(value$index == 45){
      h3(bank[45,3])
    }
    else if(value$index == 46){
      h3(bank[46,3])
    }
    else if(value$index == 47){
      h3(bank[47,3])
    }
    else if(value$index == 48){
      h3(bank[48,3])
    }
    else if(value$index == 49){
      h3(bank[49,3])
    }
    
  })
  
  
  
  observeEvent(input$submit,{ 
    
    output$mark <- renderUI({
      if (!is.null(input$answer)){
        if (any(input$answer == correct_answer[value$answerbox,1])){
          img(src = "check.png",width = 30)
        }
        else{
          img(src = "cross.png",width = 30)
          
        }
      }
    })
    
  })
  
  ###########Counting Mistakes############  
  observeEvent(input$submit,{
    for(i in c(input$answer)){
      if(any(input$answer != correct_answer[value$answerbox,1]))
      {
        value[["mistake"]] <- value[["mistake"]]+1
        output$result <- renderUI({
          
          h3("Wrong answer, click 'Next Question' to move on your challenge")
        })
      }
      
    }
  })
  
  observeEvent(input$nextq,{
    if(value[["mistake"]] == 4){
      
      updateButton(session, "nextq", disabled = TRUE)
      value[["mistake"]] <- 0
      value$correct <- 0
      
      output$result <- renderUI({
        h3("You have lost this Game. You need to start this game from the beginning.")
        
      })
    }
  })
  
  ###########Counting Correct answers############
  observeEvent(input$submit,{
    if(any(input$answer == correct_answer[value$answerbox,1]))
    {
      value$correct <- value$correct + 1
      
      output$result <- renderUI({
        h3("Congratulation! You got this one correct. Click 'Next Question' to move on your challenge")
      })
    }
    
    if(value$correct == 10){
      output$result <- renderUI({
        h3("Well Done! You have completed this challenge!  You saved that poor little man!")
      })
      updateButton(session, "nextq", disabled = TRUE)
      updateButton(session, "restart", disabled = FALSE)
    }
  })
  
  
  ##### Draw the Hangman Game#####
  
  output$distPlot <- renderUI({
    
    if(value[["mistake"]] == 0){
      img(src = "hangman-1.jpg")
      
    }
    
    ## Head
    else if(value[["mistake"]] ==1 ) {
      img(src = "hangman-2.jpg")
    }
    
    ## Arms
    else if(value[["mistake"]] == 2) {
      img(src="hangman-3.jpg")
    }
    
    ## Body
    else if(value[["mistake"]] ==3 ) {
      img(src="hangman-4.jpg")
    }
    
    
    ## Legs
    else if(value[["mistake"]] ==4) {
      img(src= "hangman-5.jpg")
    }
    
  })
  
})



