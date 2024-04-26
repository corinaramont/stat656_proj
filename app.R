# Load in Necessary Packages
library(shiny)
library(ggplot2)
library(plotly)
library(shinyjs)
library(readr)
library(glmnet)
library(gridExtra)
library(caret)
library(randomForest)
library(grid)


# Set up Layout for Application
ui <- fluidPage(
  #theme = bslib::bs_theme(bootswatch = "pulse"),
  shinyjs::useShinyjs(),
  tags$h1("STAT 656 Project"),
  #tags$a(tags$strong(tags$em("Source: Empirical Frequency Band Analysis of Nonstationary Time Series")), 
  #       href = "https://www.tandfonline.com/doi/full/10.1080/01621459.2019.1671199"),
  tags$p(tags$em("This project aims at seeing if Loan Status in the Prosper Loan dataset can be predicted. We will do this
                 through both Penalized Regression and Random Forest Models. The dataset consists of 55058 total observations and 66 features,
                 a number of which were only recorded after 2009. As such, you can fit a model on either pre 2009
                 data, or post 2009 data.")),
  tags$p(tags$strong("Authors: Dylan Ward, Megan Tung, Corina Ramont")),
  tags$hr(),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(condition = "input.tabselected==1",
                       selectInput("prInputYears", "Data Years", # Selects the type of Simulated Data to look at
                                    choices=c("Pre 2009", "Post 2009"), 
                                    selected="Post 2009"), 
                       radioButtons("prTrain", label = 'Do you want to utilize Tuned Parameters, or to select your own?',
                                    choices = c("Have Tuning Done to Select Parameters", 
                                                "Manually Select Parameters"),
                                                selected = "Have Tuning Done to Select Parameters"),
                       # Inputs for Simulated, Univariate Data
                       numericInput(inputId = "alpha", label = "Select the Alpha you wish to use *", 
                                    value = 0.5, step = 0.01, min=0, max=1), # Sets value for N
                       selectInput(inputId = "lambda", label = "Select the Lambda you wish to use **", 
                                    choices=seq(00, .001, length.out = 30), selected=0), # Sets value for N
                                        actionButton("go", label = "Run"), # Runs the Algorithm based on our selected parameters
                                        htmlOutput("Res"),
                                        htmlOutput("Res1")
                       
                       ),
      
      conditionalPanel(condition = "input.tabselected==2",
                       selectInput("rfInputYears", "Data Years", # Selects the type of Simulated Data to look at
                                   choices=c("Pre 2009", "Post 2009"), 
                                   selected="Post 2009"), 
                       radioButtons("rfTrain", label = 'Do you want to utilize Tuned Parameters, or to select your own?',
                                    choices = c("Have Tuning Done to Select Parameters", 
                                                "Manually Select Parameters"),
                                    selected = "Have Tuning Done to Select Parameters"),
                       selectInput(inputId = "ntree", label = "Choose the number of trees to grow", 
                                    choices = c(100,250,500,1000,2000,5000), selected = 1000), # Sets value for N
                       selectInput(inputId = "mtry", label = "Choose the number of variables randomly sampled at each split", 
                                    choices = c(3,5,7,9,11), selected = 7), # Sets value for K
                       
                       actionButton("go2", label = "Run") # Runs the Univariate Algorithm based on our selected parameters
      )
    ),
    mainPanel(
      tabsetPanel(type = "tabs", id = 'tabselected', selected = 1,
                  tabPanel("Penalized Regression", value = 1),
                  tabPanel("Random Forest", value=2)), # Conditionally displays either the Simulated or File Upload sides of the app
      conditionalPanel(condition = "input.tabselected==1", # This corresponds to the Penalized Regression Side of the app
                       
                                        plotOutput("Image_Plotb", height=400, width = 1000), # Plots time series
                                        plotOutput("Image_Plot", height=600, width=1000), # Plots Local periodogram
                                        plotOutput("SummaryStat", height=200, width=1000),
                                        plotOutput("ConfMat", height=600, width=1000),
                                        plotOutput("classStats", height=400, width=1000),
                                        hidden(downloadButton('downloadData','Download the Above Results')) , # Downloads the plots seen into a pdf
                                        br(),
                                        br()
                       
                                        
      ),
      conditionalPanel(condition = "input.tabselected==2", # This corresponds to the Random Forest side of the app
                       
                       # Outputs for Observed, Univariate Data
                       plotOutput("Image_Plota", height=400, width=1000), # Plots data
                       plotOutput("Image_Plot2", height=600, width=1000), # Plots local periodogram
                       plotOutput("Image_Plot3", height=600, width=1000),
                       plotOutput("Image_Plot4", height=200, width=1000),
                       plotOutput("Image_Plot5", height=600, width=1000),
                       plotOutput("Image_Plot6", height=400, width=1000),
                       hidden(downloadButton('downloadData1','Download the Above Results')), # Downloads the plots seen into a pdf
                       br(),
                       br()
                       
      )
    )
  )
  
)

# This houses all of the code that will produce the above plots and results 
server <- function(input,output, session) {
  output$Res <- renderText({
    paste(h6("* Values must be between 0 and 1"))
  })
  output$Res1 <- renderText({
    paste(h6("** Values were chosen to be 30 equally spaced values between 0 and 0.1"))
  })
  observeEvent(input$prTrain, {
    toggle("alpha")
    toggle("lambda")
  })
  observeEvent(input$rfTrain, {
      toggle("ntree")
      toggle("mtry")
  })
  
  ### Start Penalized Regression Block
  
  plot.listPR <- eventReactive(input$go, ignoreNULL = TRUE, {
    data <- read.csv("CleanedProsperData (1).csv")[,-1]
    data$ListingCreationDate <- as.character(data$ListingCreationDate)
    data$BorrowerState <- factor(data$BorrowerState)
    data$ListingKey <- NULL
    data$ListingNumber <- NULL
    data$CreditGrade <- factor(data$CreditGrade)
    data$ClosedDate <- NULL
    data$Occupation <- NULL
    data$EmploymentStatus <- factor(data$EmploymentStatus)
    data$GroupKey <- NULL
    data$DateCreditPulled <- NULL
    data$FirstRecordedCreditLine <- NULL
    data$IncomeRange <- factor(data$IncomeRange)
    data$LoanKey <- NULL
    data$LoanOriginationDate <- NULL
    data$LoanOriginationQuarter <- factor(data$LoanOriginationQuarter)
    data$MemberKey <- NULL
    data$LoanNumber <- NULL
    data$IsBorrowerHomeowner <- as.numeric(data$IsBorrowerHomeowner)
    data$CurrentlyInGroup <- as.numeric(data$CurrentlyInGroup)
    data$IncomeVerifiable <- as.numeric(data$IncomeVerifiable)
    
    currTime <- numeric(0)
    for(i in 1:length(data$ListingCreationDate)){
      curr <- data[i,]
      currTime[i] <- as.numeric(unlist(strsplit(curr$ListingCreationDate, "-")))[1] + (as.numeric(unlist(strsplit(curr$ListingCreationDate, "-")))[2] - 1)/12
    }
    data$ListingCreationDate <- NULL
    oldData <- data[currTime < 2009.5, ]
    newData <- data[currTime >= 2009.5, ]
    years <- input$prInputYears
    if(years == "Pre 2009"){
      data = oldData
      data$EstimatedEffectiveYield <- NULL
      data$EstimatedLoss <- NULL
      data$EstimatedReturn <- NULL
      data$ProsperRating..numeric. <- NULL
      data$ProsperRating..Alpha. <- NULL
      data$ProsperScore <- NULL
    } else {
      data = newData
      data$CreditGrade <- NULL
    }
    set.seed(656)
    data <- data[rowSums(is.na(data)) == 0, ]
    Y     = factor(data$LoanStatus)
    data$LoanStatus <- NULL
    trainIndex <- createDataPartition(Y, p=0.8)[[1]]
    xTrain = data[trainIndex, ]
    xTest  = data[-trainIndex, ]
    yTrain = Y[trainIndex]
    yTest  = Y[-trainIndex]
    classes <- numeric(0)
    for(i in 1:ncol(xTrain)){
         classes[i] <- class(data.frame(xTrain)[,i])
     }
    xTrain <- xTrain[,classes=="numeric"]
    xTest <- xTest[,classes=="numeric"]    
    training <- input$prTrain
    if(input$prTrain == "Have Tuning Done to Select Parameters"){
      shinyjs::show("Image_Plot")
      K            = 10
      trainControl = trainControl(method = "cv", number = K)
      tuneGrid     = expand.grid('alpha'=c(0,.25,.5,.75,1),'lambda' = seq(00, .001, length.out = 30))
      
      elasticOut = train(x = as.matrix(xTrain), y = yTrain,
                         method = "glmnet", 
                         trControl = trainControl, tuneGrid = tuneGrid)
      alpha = elasticOut$bestTune$alpha
      lambda = elasticOut$bestTune$lambda
      
    } else {
      hide("Image_Plot")
      alpha = as.numeric(input$alpha)
      lambda = as.numeric(input$lambda)
      elasticOut = 1
    }
    glmnetOut      = glmnet(x = as.matrix(xTrain), y = yTrain, 
                               alpha = alpha, family = 'multinomial', 
                               standardize = FALSE)
    
    vals <- predict(glmnetOut, as.matrix(xTest), type="response", 
                    s=lambda)
    
    maxVals <- apply(vals, 1, which.max)
    maxVals[which(maxVals == 1)] = "Chargedoff"
    maxVals[which(maxVals == 2)] = "Completed"
    maxVals[which(maxVals == 3)] = "Defaulted"
    check = "test"
    Prediction <- rep(c("Chargedoff","Completed", "Defaulted"), each=3)
    Reference <- rep(c("Chargedoff","Completed", "Defaulted"), 3)
    
    list(xTrain = xTrain, xTest = xTest, yTrain = yTrain, yTest = yTest,
         alpha = alpha, lambda = lambda, vals = vals, maxVals = maxVals, 
         elasticOut = elasticOut, glmnetOut = glmnetOut, check = check,
         Prediction = Prediction, Reference = Reference)
  })
  
  #### End Penalized Regression Block #####
  
  
  #### Start Penalized Regression Plots ####
  observeEvent(plot.listPR()[[11]], {
    output$Image_Plotb <- renderPlot({
        plot(plot.listPR()[[10]]) 
    })
    output$Image_Plot <- renderPlot({
      plot(plot.listPR()[[9]])
    })
    output$SummaryStat <- renderPlot({
      alpha <- as.numeric(plot.listPR()[[5]])
      lambda <- as.numeric(plot.listPR()[[6]])
      Metric = c("Alpha", "Lambda")
      vals <- c(alpha, lambda)
      res <- data.frame("Metric" = Metric, "Value" = vals)
      res1 <- tableGrob(res, rows = NULL)
      title <- textGrob(expression(bold("Selected Alpha and Lambda Values for Model Fitting")))
      blank9090 <- textGrob(""); blank0909 <- textGrob("")
      grid.arrange(blank9090,title, res1, blank0909, ncol = 1)
    })
    output$ConfMat <- renderPlot({
      
      Prediction <- plot.listPR()[[12]]
      Reference <- plot.listPR()[[13]]
      values <- table(plot.listPR()[[8]], plot.listPR()[[4]])[c(3,6,9,2,5,8,1,4,7)]
      res <- c("N", "N", "Y","N", "Y", "N", "Y", "N", "N")
      data<- data.frame(Prediction, Reference, values, res)
      
      ggplot(data =  data, mapping = aes(x = Reference, y =Prediction)) +
        geom_tile(aes(fill = res)) +
        geom_text(aes(label = sprintf("%1.0f", values)), vjust = 1) +
        scale_fill_manual(breaks = res, values=c("Y" = "green", "N" = "red")) +
         theme(legend.position = "none") + scale_y_discrete(expand=c(0,0), labels=c("Defaulted", "Completed", "Chargedoff")) + 
        scale_x_discrete(expand = c(0,0))
    })
    output$classStats <- renderPlot({
      acc <- mean(plot.listPR()[[8]] == plot.listPR()[[4]])
      values <- table(plot.listPR()[[8]], plot.listPR()[[4]])
      sens <- c(values[1] / colSums(values)[1],
                values[5] / colSums(values)[2],
                values[9] / colSums(values)[3])
      spec <- c(sum(diag(values)[-1]) / sum(sum(values[1,-1]), sum(diag(values)[-1])),
                sum(diag(values)[-2]) / sum(sum(values[2,-2]), sum(diag(values)[-2])),
                sum(diag(values)[-3]) / sum(sum(values[3,-3]), sum(diag(values)[-3])))
      sens <- round(sens, 5)
      spec <- round(spec, 5)
      data <- data.frame("Accuracy", round(acc,5))
      colnames(data) <- NULL
      dataGrob <- tableGrob(data, rows=NULL)
      title = textGrob(expression(bold("Class by Class Summary Statistics")))
      Metric = c("Class","Sensitivity", "Specificity")
      vals <- cbind(c("Chargedoff", "Completed", "Defaulted"),sens, spec)
      res <- data.frame(vals)
      colnames(res) <- Metric
      res1 <- tableGrob(res, rows = NULL)
      blank9090 <- textGrob(""); blank0909 <- textGrob("")
      grid.arrange(blank9090,dataGrob,title, res1, ncol = 1)
    })
  })
  
  #### End Penalized Regression Plots ####
  
  #### Start Random Forest Block ####
  
  plot.listRF <- eventReactive(input$go2, ignoreNULL = TRUE, {
    data <- read.csv("CleanedProsperData (1).csv")[,-1]
    data$ListingCreationDate <- as.character(data$ListingCreationDate)
    currTime <- numeric(0)
    for(i in 1:length(data$ListingCreationDate)){
      curr <- data[i,]
      currTime[i] <- as.numeric(unlist(strsplit(curr$ListingCreationDate, "-")))[1] + (as.numeric(unlist(strsplit(curr$ListingCreationDate, "-")))[2] - 1)/12
    }
    oldData <- data[currTime < 2009.5, ]
    newData <- data[currTime >= 2009.5, ]
    years <- input$rfInputYears
    if(years == "Pre 2009"){
      data = oldData
      data$EstimatedEffectiveYield <- NULL
      data$EstimatedLoss <- NULL
      data$EstimatedReturn <- NULL
      data$ProsperRating..numeric. <- NULL
      data$ProsperRating..Alpha. <- NULL
      data$ProsperScore <- NULL
    } else {
      data = newData
      data$CreditGrade <- NULL
    }
    set.seed(656)
    remain_data = data[rowSums(is.na(data)) == 0,]
    train_index = createDataPartition(remain_data$LoanStatus, p = .8, list = FALSE) %>% 
      as.vector(.)
    train_data = (remain_data[train_index,])[,-c(1,2)]
    test_data = (remain_data[-train_index,])[,-c(1,2)]
    if(input$rfTrain == "Have Tuning Done to Select Parameters"){
      shinyjs::show("Image_Plot2")
      shinyjs::show("Image_Plot3")
      train = "Yes"
      ntree = 5000
      mtry = floor(sqrt(ncol(train_data) - 1))
    } else {
      hide("Image_Plot2")
      hide("Image_Plot3")
      train = "No"
      ntree = as.numeric(input$ntree)
      mtry = as.numeric(input$mtry)
    }
    rf1 = randomForest(factor(LoanStatus) ~ ., data = train_data, 
                       sampsize = c('Chargedoff' = 300, 'Completed' = 300,
                                    'Defaulted' = 300), 
                       ntree = ntree, mtry=mtry)
    rf = rf1 # using 300 for all
    oob_data = data.frame(
      trees = rep(1:nrow(rf$err.rate), 4), 
      type = rep(c("OOB","Chargedoff","Completed","Defaulted"), 
                 each = nrow(rf$err.rate)),
      error = c(rf$err.rate[,"OOB"], rf$err.rate[,"Chargedoff"], 
                rf$err.rate[,"Completed"], rf$err.rate[,"Defaulted"]))
    if(input$rfTrain == "Have Tuning Done to Select Parameters"){
      tuning <- tuneRF(train_data[,-2], factor(train_data$LoanStatus), nTreeTry = 1000, 
                       stepFactor=1.5,improve=0.01,trace=FALSE, plot=FALSE,
                       sampsize = c('Chargedoff' = 300, 'Completed' = 300,
                                    'Defaulted' = 300))
      ntreeNew = which.min(oob_data$error[1:5000])
      mtryNew = unname(tuning[,1][which.min(tuning[,2])])
      rf1 = randomForest(factor(LoanStatus) ~ ., data = train_data, 
                         sampsize = c('Chargedoff' = 300, 'Completed' = 300,
                                      'Defaulted' = 300), 
                         ntree = ntreeNew, mtry=mtryNew)
    } else {
      tuning <- 1:100
      ntreeNew = ntree
      mtryNew = mtry
    }
    rf_pred = predict(rf1, test_data)
    yTest = factor(test_data$LoanStatus)
    Prediction <- rep(c("Chargedoff","Completed", "Defaulted"), each=3)
    Reference <- rep(c("Chargedoff","Completed", "Defaulted"), 3)
    
    list(data = oob_data, ntree = ntree, mtry = mtry, ntreeNew = ntreeNew, mtryNew = mtryNew,
         train = train, rf1 = rf1, test_data = test_data, tuning=tuning, rf_pred = rf_pred,
         yTest = yTest, prediction = Prediction, Reference = Reference)
  })
  
  #### End Random Forest Block ####
  
  #### Start Random Forest Plots ####
  
  observeEvent(plot.listRF()[[6]], {
    output$Image_Plota <- renderPlot({
      ggplot(data = plot.listRF()[[1]], aes(x = trees, y= error)) + 
        geom_line(aes(color = type)) + ggtitle("Sampling 300 of each class") + 
        theme(plot.title = element_text(hjust=0.5))
    })
    output$Image_Plot2 <- renderPlot({
      plot((plot.listRF()[[1]] %>% filter(type == "OOB"))$trees,
           (plot.listRF()[[1]] %>% filter(type == "OOB"))$error, type = "l", 
           xlab = "Number of trees built",
           ylab = "OOB error",
           main = "OOB error for the number of trees built")
      abline(v = as.numeric(plot.listRF()[[4]]),col = rgb(0.49, 0.81, 0.54))
    })
    output$Image_Plot3 <- renderPlot({
      plot(plot.listRF()[[9]], type="l", main = "OOB Error for various values of mtry")
      abline(v = as.numeric(plot.listRF()[[5]]),col = rgb(0.49, 0.81, 0.54))
    })
    output$Image_Plot4 <- renderPlot({
      ntree <- as.numeric(plot.listRF()[[4]])
      mtry <- as.numeric(plot.listRF()[[5]])
      Metric = c("ntree", "mtry")
      vals <- c(ntree, mtry)
      res <- data.frame("Metric" = Metric, "Value" = vals)
      res1 <- tableGrob(res, rows = NULL)
      if(plot.listRF()[[6]] == "Yes"){
        title <- textGrob(expression(bold("Selected ntree and mtry Values, which minimized OOB Error, for Model Fitting")))
      } else {
        title <- textGrob(expression(bold("Chosen ntree and mtry Values, for Model Fitting")))
        
      }
      blank9090 <- textGrob(""); blank0909 <- textGrob("")
      grid.arrange(blank9090,title, res1, blank0909, ncol = 1)
    })
    output$Image_Plot5 <- renderPlot({
      Prediction <- plot.listRF()[[12]]
      Reference <- plot.listRF()[[13]]
      values <- table(plot.listRF()[[10]], plot.listRF()[[11]])[c(3,6,9,2,5,8,1,4,7)]
      res <- c("N", "N", "Y","N", "Y", "N", "Y", "N", "N")
      data<- data.frame(Prediction, Reference, values, res)
      
      ggplot(data =  data, mapping = aes(x = Reference, y =Prediction)) +
        geom_tile(aes(fill = res)) +
        geom_text(aes(label = sprintf("%1.0f", values)), vjust = 1) +
        scale_fill_manual(breaks = res, values=c("Y" = "green", "N" = "red")) +
        theme(legend.position = "none") + scale_y_discrete(expand=c(0,0), labels=c("Defaulted", "Completed", "Chargedoff")) + 
        scale_x_discrete(expand = c(0,0))
    })
    output$Image_Plot6 <- renderPlot({
      acc <- mean(plot.listRF()[[10]] == plot.listRF()[[11]])
      values <- table(plot.listRF()[[10]], plot.listRF()[[11]])
      sens <- c(values[1] / colSums(values)[1],
                values[5] / colSums(values)[2],
                values[9] / colSums(values)[3])
      spec <- c(sum(diag(values)[-1]) / sum(sum(values[1,-1]), sum(diag(values)[-1])),
                sum(diag(values)[-2]) / sum(sum(values[2,-2]), sum(diag(values)[-2])),
                sum(diag(values)[-3]) / sum(sum(values[3,-3]), sum(diag(values)[-3])))
      sens <- round(sens, 5)
      spec <- round(spec, 5)
      data <- data.frame("Accuracy", round(acc,5))
      colnames(data) <- NULL
      dataGrob <- tableGrob(data, rows=NULL)
      title = textGrob(expression(bold("Class by Class Summary Statistics")))
      Metric = c("Class","Sensitivity", "Specificity")
      vals <- cbind(c("Chargedoff", "Completed", "Defaulted"),sens, spec)
      res <- data.frame(vals)
      colnames(res) <- Metric
      res1 <- tableGrob(res, rows = NULL)
      blank9090 <- textGrob(""); blank0909 <- textGrob("")
      grid.arrange(blank9090,dataGrob,title, res1, ncol = 1)
    })
  })
  
  #### End Random Forest  Plots ####
}

shinyApp(ui = ui, server = server)
