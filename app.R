library(shiny)
library(rsconnect)
library(rhandsontable)
library(lpSolve)
library(ggplot2)
library(DT)

#User Inputs


ui <- fluidPage(
  titlePanel("99th FTS Optimal Class Size Model"),
  sidebarLayout(
    sidebarPanel(
    sliderInput(inputId = "old1", 
                label = "How many required flying events are currently backlogged in the pipeline?",
                value = 100, min = 0, max = 300),
    
    h2("Course Requirements"),
    rHandsontableOutput("table1"),
    h2("Programmed Class Sizes"),
    rHandsontableOutput("table3"),
    h2("Expected Succesful Sortie Capacity"),
    rHandsontableOutput("table2")
    ),
    mainPanel(
      dataTableOutput(outputId = "optimalClassSize"),
      plotOutput(outputId = "classSize"),
      plotOutput(outputId = "backlog")
    )
  )
)

server <- function(input,output){
  
    #Inputs
    periods <- 11
    classTypes <- 4
    
    # Start Dates
    # 21 Aug 18 = Class 1
    # 24 Sep 18
    # 25 Oct 18
    # 03 Dec 18
    # 15 Jan 19
    # 20 Feb 19
    # 25 Mar 19
    # 22 Apr 19
    # 28 May 19
    # 24 Jun 19
    # 23 Jul 19 = Class 11
    
    #Requirements Table
    df1 = data.frame(matrix(c(c(32,8,21,22),c(12,2,1,1),c(82,22,1,6),c(0,0,0,0)),nrow = 4,ncol = 4,byrow = T))
    colnames(df1) <- c("PIT","ICSO","CSO","Requal")
    rownames(df1) <- c("Succesful Sorties per Student","Max Class Size", "Total Graduates", "Allowable Grad Shortfall")
    values1 <- reactiveValues(data=df1)
    
    output$table1 <- renderRHandsontable({
      rhandsontable(values1$data, rowHeaderWidth = 200)
    })
    
    observeEvent(
      input$table1$changes$changes,
      {
        values1$data <- hot_to_r(input$table1)
      }
    )
    
    #Resources Table
    b11 <- round(runif(periods, min = 300, max = 350))
    b12 <- round(runif(periods, min = 300, max = 350))
    
    df2 = data.frame(matrix(c(b11,b12),ncol = 2, nrow = periods, byrow = F))
    colnames(df2) <- c("IPs","Jets")
    rownames(df2) <- paste0("Period ",seq(1,periods,1))
    values2 <- reactiveValues(data=df2)
  
    output$table2 <- renderRHandsontable({
      rhandsontable(values2$data, rowHeaderWidth = 90)
    })
    
    observeEvent(
      input$table2$changes$changes,
      {
        values2$data <- hot_to_r(input$table2)
      }
    )
    
    #Programmed Class Size Table
    df3 = data.frame(matrix(c(c(9,8,6,5,8,7,8,7,9,7,8),c(rep(2,periods)),c(rep(0,6)),1,c(rep(0,4)),c(0,0,0,1,1,0,1,1,1,0,1)),nrow = periods,ncol = 4,byrow = F))
    colnames(df3) <- c("PIT","ICSO","CSO","Requal")
    rownames(df3) <- paste0("Class ",seq(1,periods,1))
    values3 <- reactiveValues(data=df3)
    
    output$table3 <- renderRHandsontable({
      rhandsontable(values3$data, rowHeaderWidth = 90)
    })
    
    observeEvent(
      input$table3$changes$changes,
      {
        values3$data <- hot_to_r(input$table3)
      }
    )
    
    solutionData <- reactive({
      
      #IP Formulation
      c <- c(rep(0,periods*classTypes),rep(1,periods))
      
      A11 <- NULL
      
      for (i in 1:periods){
        a1x <- c(c(-1*rep(values1$data[1,1],i)),c(rep(0,periods-i)))
        A11 <- rbind(A11,a1x)
      }
      
      A12 <- NULL
      
      for (i in 1:periods){
        a1x <- c(c(-1*rep(values1$data[1,2],i)),c(rep(0,periods-i)))
        A12 <- rbind(A12,a1x)
      }
      
      A13 <- NULL
      
      for (i in 1:periods){
        a1x <- c(c(-1*rep(values1$data[1,3],i)),c(rep(0,periods-i)))
        A13 <- rbind(A13,a1x)
      }
      
      A14 <- NULL
      
      for (i in 1:periods){
        a1x <- c(c(-1*rep(values1$data[1,4],i)),c(rep(0,periods-i)))
        a1z <- rep(0,periods)
        a1z[i] <- 1
        a1 <- c(a1x,a1z)
        A14 <- rbind(A14,a1)
      }
      
      A1 <- cbind(A11,A12,A13,A14)
      
      A2 <- NULL
      
      for (i in 1:periods){
        a2x <- c(rep(0,classTypes*periods))
        a2z <- rep(0,periods)
        a2z[i] <- 1
        a2 <- c(a2x,a2z)
        A2 <- rbind(A2,a2)
      }
      
      A3 <- NULL
      
      for (i in 1:classTypes){
        if(i == 1){  
          a3 <- c(rep(1,periods),rep(0,classTypes*periods))
        }
        else{
          a3 <- c(rep(0,(i-1)*periods),rep(1,periods),rep(0,(classTypes-i+1)*periods))
          
        }
        A3 <- rbind(A3,a3)
      }
      
      A41 <- NULL
      
      for (i in 1:periods){
        a4x <- rep(0,classTypes*periods)
        a4x[i] <- -1
        a4z <- c(rep(0,periods))
        a4 <- c(a4x,a4z)
        A41 <- rbind(A41,a4)
      }
      
      A42 <- NULL
      
      for (i in (1+periods):(2*periods)){
        a4x <- rep(0,classTypes*periods)
        a4x[i] <- -1
        a4z <- c(rep(0,periods))
        a4 <- c(a4x,a4z)
        A42 <- rbind(A42,a4)
      }
      
      A43 <- NULL
      
      for (i in (1+2*periods):(3*periods)){
        a4x <- rep(0,classTypes*periods)
        a4x[i] <- -1
        a4z <- c(rep(0,periods))
        a4 <- c(a4x,a4z)
        A43 <- rbind(A43,a4)
      }
      
      A44 <- NULL
      
      for (i in (1+3*periods):(4*periods)){
        a4x <- rep(0,classTypes*periods)
        a4x[i] <- -1
        a4z <- c(rep(0,periods))
        a4 <- c(a4x,a4z)
        A44 <- rbind(A44,a4)
      }
      
      A4 <- rbind(A41,A42,A43,A44)
      
      b1 <- rep(0,periods)
      
      for(i in 1:periods){
        if (i == 1){
          b1[1] <- -1*min(values2$data[1,])+input$old1
        }
        else{
          b1[i] <- -1*min(values2$data[i,])+b1[i-1]
        }
      }
      
      b2 <- rep(0,periods)
      
      b3 <- values1$data[3,] - values1$data[4,]
      
      b4 <- c(-1*rep(values1$data[2,1],periods),-1*rep(values1$data[2,2],periods),-1*rep(values1$data[2,3],periods),-1*rep(values1$data[2,4],periods))
      
      A <- rbind(A1,A2,A3,A4)
      b <- c(b1,b2,b3,b4)
      sign <- rep(">=",length(b))
      
      solve <- lp (direction = "min", c, A, sign, b,
                   transpose.constraints = TRUE, presolve=0, compute.sens=0,
                   all.int=TRUE, all.bin=FALSE, scale = 196, dense.const, 
                   num.bin.solns=1, use.rw=FALSE)
      
      solutionOptimal <- cbind(1:periods,1:periods-1,1:periods,matrix(solve$solution, nrow = 11, ncol = 5,byrow = F),rep("Optimal Class Size",11))
      
      #Alternate Solutions
      #Max Class Size

      x <- c(rep(values1$data[2,1],periods),rep(values1$data[2,2],periods),rep(values1$data[2,3],periods),rep(values1$data[2,4],periods),rep(0,periods))
      x <- as.numeric(x)
      b <- as.numeric(b)
      bInf <- A %*% x
      bInf <- bInf - b
      
      backlogMaxClassSize <- unname(-1*bInf[1:periods,])
      backlogMaxClassSize[backlogMaxClassSize < 0] <- 0

      solutionMaxClassSize <- cbind(1:periods,1:periods-1,1:periods,matrix(x[1:44],nrow=periods,ncol=classTypes,byrow=F), backlogMaxClassSize,rep("Max Class Size",periods))

      #Programmed Class Size

      x <- c(values3$data[,1],values3$data[,2],values3$data[,3],values3$data[,4],rep(0,periods))
      x <- as.numeric(x)
      b <- as.numeric(b)
      bInf <- A %*% x
      bInf <- bInf - b
      
      backlogProgClassSize <- unname(-1*bInf[1:periods,])
      backlogProgClassSize[backlogProgClassSize < 0] <- 0

      solutionProgClassSize <- cbind(1:periods, 1:periods-1, 1:periods,matrix(x[1:44],nrow=periods,ncol=classTypes,byrow=F), backlogProgClassSize,rep("Programmed Class Size",periods))

      #Combined Solution Data
      solutions <- data.frame(rbind(solutionOptimal,solutionMaxClassSize,solutionProgClassSize))
      colnames(solutions) <- c("Period", "Time_Start", "Time_End", "PIT","ICSO","CSO","Requal","Expected_Backlog","Solution")

      for (i in 1:(ncol(solutions)-1)){
        solutions[,i] <- as.numeric(as.character(solutions[,i]))
      }
      
      solutions
    })
  
  
    output$optimalClassSize <- DT::renderDataTable({
      solutionData()
    })

    output$backlog <- renderPlot({
      backlogTime <- ggplot(data = solutionData(), aes(x = Time_End, y = Expected_Backlog, color = Solution))+
        geom_line()+
        xlim(as.character(1:11))+
        xlab("Time")+
        ylab("Expected Backlog")
      
      print(backlogTime)
    })

    output$classSize <- renderPlot({
        classSizeTime <- ggplot(data = solutionData(), aes(x = Time_Start, y = PIT, color = Solution))+
          geom_line()+
          ylim(0,max(values1$data[2,])+2)+
          xlab("Time")+
          ylab("Incoming PIT Class Size")+
          scale_x_discrete(limits = c(1:periods-1))
        
        print(classSizeTime)
      })
    
}

shinyApp(ui = ui, server = server)
