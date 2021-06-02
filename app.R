library(shiny)
library(shinydashboard)
library(png)
library(shinyBS)
library(V8)
library(shinyjs)
library(shinyalert)
#library(discrimARTs)
library(leaflet)
library(raster)
library(DT)
library(car)
library(rgdal)
library(shinyWidgets)
library(boastUtils)


#This app will be used to explore and play around with the assumptions and diagnostics of regression
GRID_SIZE <- 3
TILE_COUNT <- GRID_SIZE ^ 2

ui <- list(
  dashboardPage(
    skin = "black",
    dashboardHeader(title = "Regression Assumptions and Diagnostics",
                    titleWidth = 250,
# Creating header buttons ----
                    tags$li(
                      class = "dropdown",
                      actionLink("info",
                                 icon("info"))
                    ),
                    tags$li(
                      class = "dropdown",
                      tags$a(target = "_blank", 
                             icon("comments"),
                             href = "https://pennstate.qualtrics.com/jfe/form/SV_7TLIkFtJEJ7fEPz?appName=Assumptions"
                      )
                    ),
                    tags$li(
                      class = "dropdown",
                      tags$a(icon("home"),
                             href = 'https://shinyapps.science.psu.edu/'
                      )
                    )
    ),
    
    #adding prereq and game tabs ----
    dashboardSidebar(
      width = 220,
      sidebarMenu(id = "tabs",
                  menuItem(text = "Overview",
                           tabName = "instruction", 
                           icon = icon("dashboard")
                  ),
                  menuItem(text = "Prerequisites", 
                           tabName = "prereq", 
                           icon = icon("book")
                  ),
                  menuItem(text = "Explore",
                           tabName = "explore", 
                           icon = icon("wpexplorer")
                  ),
                  menuItem(text = "Game", 
                           tabName = "qqq", 
                           icon = icon("gamepad")
                  ),
                  menuItem(text = "References",
                           tabName = "refs",
                           icon = icon("leanpub")
                  )
      ),
      tags$div(class = "sidebar-logo", boastUtils::psu_eberly_logo("reversed"))
    ),
# Creating overview page ----
    dashboardBody(
      tabItems(
        tabItem(tabName = "instruction",
                h1("Regression Assumptions"),
                h2("About:"),
                h4("This app will allow you to explore how to read diagnostic plots
                while interacting with different transformations to help you better 
                understand the assumptions of regression."),
                br(),                     
                h2("Instructions"),
                h4(
                  tags$li("Each 'Mystery Model' on the exploration page is generated
                        with variables or their transformations being the response (Y) or 
                        the predictor variables (X1, X2).")
                ),
                h4(
                  tags$li("Watch how diagnostic plots change when you adjust the 
                        predictors and response variables using different transformations. 
                        Note that transforming the y variable will effect certain 
                        plots more, and transforming the x variable will effect 
                        other plots more.")
                ),
                h4(
                  tags$li('You also have the option to change the variances of each
                        term, and the sample size.')
                ),
                h4(
                  tags$li('The instructions in the activity provide some ideas for exploration.')
                ),
                h4(
                  tags$li("In the game, the object is to win at tic-tac-toe where 
                        you are playing X's.  Select a square, then answer the question.
                        If you get the question correct, an X goes in the square. 
                        If you get it wrong, an O goes in the square.")
                ),
                div(style = "text-align: center",
                    bsButton(inputId = "go", 
                             label = "Go to Prereqs!",
                             icon = icon("book"), 
                             style = "danger", 
                             size = "large", 
                             class = "circle grow")
                ),
                br(),
                h2("Acknowledgements:"),
                h4("This app was developed and coded by TJ McIntyre, with the help of Ryan Voyack and was updated by Lydia Bednarczyk.")
        ),
#Adding pre-requisites page ----
        tabItem(tabName = "prereq",
                h2("Prerequisites"),
                h4("In order to get the most out of this app, it is important to
                   understand background information about assumptions and diagnostic
                   plots in regression."),
                h4(
                  tags$li("Transforming the x values is appropriate when non-linearity 
                        is the only problem (i.e., the independence, normality, 
                        and equal variance conditions are met). Transforming the 
                        y values should be considered when non-normality and/or 
                        unequal variances are the problems with the model.")
                ),
                h4(
                  tags$li("The Fitted vs Residuals plot can be used to check the 
                        assumption of linearity (any location on the x axis, the 
                        average residual should be close to 0) and it can also be 
                        used to check the assumption of equal variances (at any 
                        location on the x axis, the variability of the residual 
                        should be similar).")
                ),
                h4(
                  tags$li("The Normal Q-Q plot can be used to check the assumption 
                        of normal errors: i.e. the majority of the points should 
                        be a straight line. Skewness can also be seen by this plot. 
                        See the ", 
                          a(href='https://psu-eberly.shinyapps.io/QQ_Plot/', 'Q-Q plot'),
                          " app for further exploration.")
                ),
                h4(
                  tags$li("The Scale-Location plot can be used to check the assumption 
                        of equal variances, at any location of the x axis, the upper 
                        bound of the residuals should be similar.")
                ),
                h4(
                  tags$li("The Cook's Distance plot shows the values of leverage, 
                        standardized residuals, and Cook's Distance of each data point
                        which can be used to determine high leverage points, outliers 
                        and influential points.")
                ),
                br(),
                div(style = "text-align: center",
                    bsButton(
                      inputId = "start",
                      label = "GO!",
                      icon = icon("bolt"),
                      style = "danger",
                      size = "large", 
                      class = "circle grow")
                )
        ),
        #Explore page ----
        tabItem(tabName = "explore",
                fluidRow(
                  h2("Transformations, Sample size, and Variances vs. Diagnostic plots"),
                  h4("Each model is generated with Y as the response variable and X1 and X2 being the predictor variables.")
                ),
                br(),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("model", 
                                "Select Mystery Model:", 
                                choices = c(
                                  'Model 1', 
                                  'Model 2', 
                                  'Model 3')
                    ),
                    sliderInput("n", 
                                "Sample Size:", 
                                min = 10,
                                max = 510, 
                                value = 50, 
                                step = 5),
                    bsPopover(id = "n",
                              title = "Sample Size Input", 
                              content = "Number of observations pulled from the normal distribution",
                              placement = "top", 
                              trigger = "click", 
                              options = NULL
                    ),
                    selectInput("x", 
                                "Transformation on X1:", 
                                choices = c(
                                  'logx', 
                                  'sqrtx',
                                  "none"), 
                                selected = "nonex"
                    ),
                    # bsPopover(id = "x",
                    #           title = "Transformation Hint", 
                    #           content = "Transform X when non-linearity is in question",
                    #           placement = "top", 
                    #           trigger = "click", 
                    #           options = NULL
                    #           ),
                    sliderInput("x1v", 
                                "x1 Variance:", 
                                min = 0, 
                                max = 20, 
                                value = 2, 
                                step = 1
                    ),
                    bsPopover(id = "x1v",
                              title = "Variance for X1", 
                              content = "Variance for X1 normal distribution",
                              placement = "top", 
                              trigger = "click", 
                              options = NULL
                    ),
                    selectInput("x2", 
                                "Transformation on X2:", 
                                choices = c(
                                  'logx2', 
                                  'sqrtx2', 
                                  "none"),
                                selected = "nonex2"
                    ),
                    # bsPopover(id = "x2",
                    #           title = "Transformation Hint", 
                    #           content = "Transform X when non-linearity is in question",
                    #           placement = "top", 
                    #           trigger = "click", 
                    #           options = NULL
                    #           ),
                    sliderInput("x2v", 
                                "x2 Variance:", 
                                min = 0, 
                                max = 20, 
                                value = 2, 
                                step = 1
                    ),
                    bsPopover(id = "x2v",
                              title = "Variance for X2", 
                              content = "Variance for X2 normal distribution",
                              placement = "top", 
                              trigger = "click", 
                              options = NULL),
                    selectInput('y', 
                                'Transformation on Y:', 
                                choices = c(
                                  'logy',
                                  'sqrty',
                                  "none"
                                ), 
                                selected = "logy"
                    ),
                    sliderInput("yv", 
                                "Y Variance:", 
                                min = 0, 
                                max = 20, 
                                value = 2, 
                                step = 1
                    ),
                    bsPopover(id = "yv",
                              title = "Variance for Y", 
                              content = "Variance for Y normal distribution",
                              placement = "top", 
                              trigger = "click", 
                              options = NULL
                    ),
                    bsPopover(id = "y",
                              title = "Transformation Hint", 
                              content = "Transform Y when non-normality or unequal variances are in question",
                              placement = "top", 
                              trigger = "click", 
                              options = NULL
                    ),
                    bsButton(inputId = "submitD", 
                             label = "Results for a New Sample", 
                             style = "danger",
                             icon = icon("retweet"), 
                             size = "large"
                    ),
                    br(),
                    br(),
                    bsButton(inputId = "begin",
                             label = "Go to game!",
                             icon = icon("bolt"),
                             style = "danger",
                             size = "large",
                             class = "circle grow")
                  ),
                  mainPanel(
                    plotOutput("plots"),
                    br(),
                    fluidRow(
                      h3("Adjust the inputs to complete the activity:")
                    ),
                    
                    wellPanel(
                      fluidRow(
                        uiOutput("challenges")
                      )
                    ),
                    fluidRow(
                      h3("Feedback:")
                    ),
                    wellPanel(
                      fluidRow(
                        uiOutput("answers")
                      )
                    ),
                    bsButton(inputId = "challenge", 
                             label = "New Activity", 
                             style = "danger",
                             disabled = FALSE
                    ),
                    bsButton(inputId = "answer", 
                             label = "View Feedback", 
                             style = "danger",
                             disabled = FALSE)
                  )
                )
        ),
        # Game page ----
        tabItem(tabName = "qqq",
                withMathJax(),
                useShinyalert(),
                h2("Regression Tic-Tac-Toe"),
                p(
                  "To play, click on any one of the buttons that have a question mark. A question will appear to the right with possible answers. If you answer correctly, you will take the square; if not, the computer will take the square. Try your best to win the game!"
                ),
                h3(uiOutput("player")),
                fluidRow(
                  div(
                    class = "col-sm-12 col-md-4",
                    h3("Game Board"),
                    br(),
                    uiOutput("gameBoard", class = "game-board")
                  ),
                  div(
                    class = "col-sm-12 col-md-8",
                    h3("Question"),
                    withMathJax(uiOutput("question")),
                    uiOutput("extraOutput"),
                    h3("Answer"),
                    uiOutput("answer"),
                    uiOutput("mark"),
                    uiOutput("feedback"),
                    bsButton(
                      inputId = "submit",
                      label = "Submit",
                      size = "large",
                      style = "default",
                      disabled = TRUE
                    ),
                    bsButton(
                      inputId = "reset",
                      label = "Reset Game",
                      color = "primary",
                      size = "large",
                      style = "default"
                    ),
                    br(),
                    #These two triggers help with MathJax re-rendering
                    uiOutput("trigger1"),
                    uiOutput("trigger2")
                  )
                )
                
                # fluidRow(
                #   h2("Tic-Tac-Toe"),
                #   h4("You will get an X for a correct answers and the computer will 
                #   get an O for a wrong answer.")
                # ),
                # fluidRow(
                #   column(4,
                #          leafletOutput('image'),
                #          br(),
                #          textOutput("warning"),
                #          textOutput("gameMessage")
                #   ),
                #   column(8,
                #          conditionalPanel("output.temp != 2",
                #                           conditionalPanel(
                #                             "input.image_click",
                #                             uiOutput("CurrentQuestion"),
                #                             uiOutput("CurrentQuestion.extra"),
                #                             br(),
                #                             br(),
                #                             br()
                #                           ),
                #                           textOutput("directions"),
                #                           br()
                #          )
                #   ),
                #   
                #   column(2,
                #          bsButton(
                #            inputId = 'submit', 
                #            label = 'Submit Answer', 
                #            style = 'danger')
                #   ),
                #   column(1,
                #          bsButton(
                #            inputId = "nextButton",
                #            label = "Next Question", 
                #            style = 'danger')
                #   )
                # ),
                # fluidRow(
                #   column(
                #     width = 12, 
                #     offset = 5,
                #     br(),
                #     bsButton(inputId = "reset", 
                #              label = "Start new game", 
                #              style = 'danger'
                #     )
                #   )
                # )
        ),
        tabItem(
          tabName = "refs",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
            "https://educationshinyappteam.github.io/Style_Guide/index.html#organization"
          ),
          p(
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny. (v0.61). [R package]. Available from https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Carey, R. (2019). boastUtils: BOAST Utilities. (v0.1.0). [R Package]. Available from https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Chang, W. and Borges Ribeio, B. (2018). shinydashboard: Create dashboards with 'Shiny'. (v0.7.1) [R Package]. Available from https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J. (2019). shiny: Web application framework for R. (v1.4.0) [R Package]. Available from https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Perrier, V., Meyer, F., Granjon, D. (2019). shinyWidgets: Custom inputs widgets for shiny. (v0.5.0) [R Package]. Available from https://CRAN.R-project.org/package=shinyWidgets"
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


#bankc for challenge bank

bankc <- read.csv("ChallengeOutput.csv")
bankc = data.frame(lapply(bankc, as.character), stringsAsFactors = FALSE)


# Server ----

server <- function(input, output,session) {
  
#Go buttons ---- 
  observeEvent(input$infoex,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = " Move the sliders to see their effect on the diagnostic plots.",
      type = "info"
    )
  })
  
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "Click on desired square, answer the question, then hit submit and go to next question.",
      type = "info"
    )
  })
  observeEvent(
    eventExpr = input$go, 
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "tabs",
        selected = "prereq")
    })
  
  observeEvent(
    eventExpr = input$start,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "tabs",
        selected = "explore")
    })
  
  observeEvent(
    eventExpr = input$begin,
    handlerExpr = {
      updateTabItems(
        session = session, 
        inputId = "tabs", 
        selected = "qqq")
    })
#Gray out buttons ----
  
  observeEvent(
    eventExpr = input$start, 
    handlerExpr = {
      updateButton(
        session = session, 
        inputId = "answers", 
        disabled = TRUE)
    })
  
  observeEvent(
    eventExpr = input$challenge, 
    handlerExpr = {
      updateButton(
        session = session, 
        inputId = "answers", 
        disabled = FALSE)
    })
  
  observeEvent(
    eventExpr = input$answer, 
    handlerExpr = {
      updateButton(
        session = session, 
        inputId = "answers", 
        disabled = TRUE)
    })
  
  observeEvent(
    eventExpr = input$begin, 
    handlerExpr = {
      updateButton(
        session = session, 
        inputId = "submit", 
        disabled = TRUE)
    })
  
  
  #############################plot outputs#################################
  observeEvent(input$go | input$submitD, {
    output$plots=
      renderPlot({
        
        if(input$model == "Model 1"){
          nonex<- rnorm(input$n,3,input$x1v)
          nonex2<- rnorm(input$n,3,input$x2v)
          e<- rnorm(input$n,0,.2**2)
          y<- (rnorm(input$n,3,input$yv))**2
          
          logx<- log(nonex)
          logx2<-log(nonex2)
          logy<- log(y)
          
          expx<- sqrt(nonex)
          expx2<- sqrt(nonex2)
          expy<- sqrt(y)
        } else if (input$model == "Model 2"){
          nonex<- rnorm(input$n,3,input$x1v)**2
          nonex2<- rnorm(input$n,3,input$x2v)**2
          e<- rnorm(input$n,0,.2**2)
          y<- (rnorm(input$n,3,input$yv))
          
          logx<- log(nonex)
          logx2<-log(nonex2)
          logy<- log(y)
          
          expx<- sqrt(nonex)
          expx2<- sqrt(nonex2)
          expy<- sqrt(y)
        }else{
          nonex<- rnorm(input$n,3,input$x1v)**2
          nonex2<- rnorm(input$n,3,input$x2v)**2
          e<- rnorm(input$n,0,.2**2)
          y<- (rnorm(input$n,3,input$yv))**2
          
          logx<- log(nonex)
          logx2<-log(nonex2)
          logy<- log(y)
          
          expx<- sqrt(nonex)
          expx2<- sqrt(nonex2)
          expy<- sqrt(y)
        }
        
        for (i in c(input$x)){ for(j in c(input$x2)) {for( k in  c(input$y)){
          if (any(i == "none")& any(j== "none")&any(k== "none")){
            par(mfrow=c(2,2))
            model= lm(y~nonex+nonex2+e)
            plot(model)
          } 
          else if (any(i == "none")& any(j== "logx2")&any(k== "none")) {
            par(mfrow=c(2,2))
            model2= lm(y~nonex+logx2+e)
            plot(model2)
          }
          
          else if(any(i == "none")& any(j== "sqrtx2")&any(k== "none")){
            par(mfrow=c(2,2))
            model3= lm(y~nonex+expx2+e)
            plot(model3)
          }
          
          else if(any(i == "logx")& any(j== "none")&any(k== "none")){
            par(mfrow=c(2,2))
            model4= lm(y~logx+nonex2+e)
            plot(model4)
          }
          
          else if(any(i == "logx")& any(j== "logx2")&any(k== "none")){
            par(mfrow=c(2,2))
            model5= lm(y~logx+logx2+e)
            plot(model5)
          }
          
          else if(any(i == "logx")& any(j== "sqrtx2")&any(k== "none")){
            par(mfrow=c(2,2))
            model6= lm(y~logx+expx2+e)
            plot(model6)
          }
          
          else if(any(i == "sqrtx")& any(j== "none")&any(k== "none")){
            par(mfrow=c(2,2))
            model7= lm(y~expx+nonex2+e)
            plot(model7)
          }
          
          else if(any(i == "sqrtx")& any(j== "logx2")&any(k== "none")){
            par(mfrow=c(2,2))
            model8= lm(y~expx+logx2+e)
            plot(model8)
          }
          
          else if(any(i == "sqrtx")& any(j== "sqrtx2")&any(k== "none")){
            par(mfrow=c(2,2))
            model9= lm(y~expx+expx2+e)
            plot(model9)
          }
          
          else if(any(i == "none")& any(j== "nonex2")&any(k== "logy")){
            par(mfrow=c(2,2))
            model10= lm(logy~nonex+nonex2+e)
            plot(model10)
          }
          
          else if(any(i == "none")& any(j== "logx2")&any(k== "logy")){
            par(mfrow=c(2,2))
            model11= lm(logy~nonex+logx2+e)
            plot(model11)
          }
          
          else if(any(i == "none")& any(j== "sqrtx2")&any(k== "logy")){
            par(mfrow=c(2,2))
            model12= lm(logy~nonex+expx2+e)
            plot(model12)
          }
          
          else if(any(i == "logx")& any(j== "nonex2")&any(k== "logy")){
            par(mfrow=c(2,2))
            model13= lm(logy~logx+nonex2+e)
            plot(model13)
          }
          
          else if(any(i == "logx")& any(j== "logx2")&any(k== "logy")){
            par(mfrow=c(2,2))
            model14= lm(logy~logx+logx2+e)
            plot(model14)
          }
          
          else if(any(i == "logx")& any(j== "sqrtx2")&any(k== "logy")){
            par(mfrow=c(2,2))
            model15= lm(logy~logx+expx2+e)
            plot(model15)
          }
          
          else if(any(i == "sqrtx")& any(j== "nonex2")&any(k== "logy")){
            par(mfrow=c(2,2))
            model16= lm(logy~expx+nonex2+e)
            plot(model16)
          }
          
          else if(any(i == "sqrtx")& any(j== "logx2")&any(k== "logy")){
            par(mfrow=c(2,2))
            model17= lm(logy~expx+logx2+e)
            plot(model17)
          }
          
          else if(any(i == "sqrtx")& any(j== "sqrtx2")&any(k== "logy")){
            par(mfrow=c(2,2))
            model18= lm(logy~expx+expx2+e)
            plot(model18)
          }
          
          else if(any(i == "none")& any(j== "nonex2")&any(k== "sqrty")){
            par(mfrow=c(2,2))
            model19= lm(expy~nonex+nonex2+e)
            plot(model19)
          }
          
          else if(any(i == "none")& any(j== "logx2")&any(k== "sqrty")){
            par(mfrow=c(2,2))
            model20= lm(expy~nonex+logx2+e)
            plot(model20)
          }
          
          else if(any(i == "none")& any(j== "sqrtx2")&any(k== "sqrty")){
            par(mfrow=c(2,2))
            model21= lm(expy~nonex+expx2+e)
            plot(model21)
          }
          
          else if(any(i == "logx")& any(j== "none")&any(k== "sqrty")){
            par(mfrow=c(2,2))
            model22= lm(expy~logx+nonex2+e)
            plot(model22)
          }
          
          else if(any(i == "logx")& any(j== "logx2")&any(k== "sqrty")){
            par(mfrow=c(2,2))
            model23= lm(expy~logx+logx2+e)
            plot(model23)
          }
          
          else if(any(i == "logx")& any(j== "sqrtx2")&any(k== "sqrty")){
            par(mfrow=c(2,2))
            model24= lm(expy~logx+expx2+e)
            plot(model24)
          }
          
          else if(any(i == "sqrtx")& any(j== "none")&any(k== "sqrty")){
            par(mfrow=c(2,2))
            model25= lm(expy~expx+nonex2+e)
            plot(model25)
          }
          
          else if(any(i == "sqrtx")& any(j== "logx2")&any(k== "sqrty")){
            par(mfrow=c(2,2))
            model26= lm(expy~expx+logx2+e)
            plot(model26)
          }
          
          else {
            par(mfrow=c(2,2))
            model27= lm(expy~expx+expx2+e)
            plot(model27)
          }
        }}} 
      })
    
    
    ####end of observeeventsubmit     
  })
  
  index <- reactiveValues(index=7)
  
  ######################### This section is to output a new activity ############################
  
  observeEvent(input$challenge | input$go, {
    index$index <- sample(1:20,1, replace=FALSE, prob=NULL)
    
    output$challenges <- renderUI ({
      if (index$index == 1){
        h4(bankc[1,2])
      } 
      else if (index$index == 2){
        h4(bankc[2,2])
      }
      else if (index$index == 3){
        h4(bankc[3,2])
      }
      else if (index$index == 4){
        h4(bankc[4,2])
      }
      else if (index$index == 5){
        h4(bankc[5,2])
      }
      else if (index$index == 6){
        h4(bankc[6,2])
      }
      else if (index$index == 7){
        h4(bankc[7,2])
      }
      else if (index$index == 8){
        h4(bankc[8,2])
      }
      else if (index$index == 9){
        h4(bankc[9,2])
      }
      else if (index$index == 10){
        h4(bankc[10,2])
      }
      else if (index$index == 11){
        h4(bankc[11,2])
      }
      else if (index$index == 12){
        h4(bankc[12,2])
      }
      else if (index$index == 13){
        h4(bankc[13,2])
      }
      else if (index$index == 14){
        h4(bankc[14,2])
      }
      else if (index$index == 15){
        h4(bankc[15,2])
      }
      else if (index$index == 16){
        h4(bankc[16,2])
      }
      else if (index$index == 17){
        h4(bankc[17,2])
      }
      else if (index$index == 18){
        h4(bankc[18,2])
      }
      else if (index$index == 19){
        h4(bankc[19,2])
      }
      else if (index$index == 20){
        h4(bankc[20,2])
      }
      
    }
    )
    
  })
  
  ########################## Output for answer box when nothing is in the box #############################
  
  observeEvent(
    eventExpr = input$challenge,
    handlerExpr = {
      output$answers <- renderText("Please hit the view feedback button for feedback")
    }) 
  
  observeEvent(
    eventExpr = input$go,
    handlerExpr = {
      output$answers <- renderText("Please hit the view feedback button for feedback")
    })  
  observeEvent(
    eventExpr = input$start,
    handlerExpr = {
      output$answers <- renderText("Please hit the view feedback button for feedback")
    }) 
  
  ###################### output of the answers ################################
  
  observeEvent(
    eventExpr = input$answer,
    handlerExpr = {
      output$answers <- renderUI ({
        if (index$index == 1){
          h4(bankc[1,3])
        } 
        else if (index$index == 2){
          h4(bankc[2,3])
        }
        else if (index$index == 3){
          h4(bankc[3,3])
        }
        else if (index$index == 4){
          h4(bankc[4,3])
        }
        else if (index$index == 5){
          h4(bankc[5,3])
        }
        else if (index$index == 6){
          h4(bankc[6,3])
        }
        else if (index$index == 7){
          h4(bankc[7,3])
        }
        else if (index$index == 8){
          h4(bankc[8,3])
        }
        else if (index$index == 9){
          h4(bankc[9,3])
        }
        else if (index$index == 10){
          h4(bankc[10,3])
        }
        else if (index$index == 11){
          h4(bankc[11,3])
        }
        else if (index$index == 12){
          h4(bankc[12,3])
        }
        else if (index$index == 13){
          h4(bankc[13,3])
        }
        else if (index$index == 14){
          h4(bankc[14,3])
        }
        else if (index$index == 15){
          h4(bankc[15,3])
        }
        else if (index$index == 16){
          h4(bankc[16,3])
        }
        else if (index$index == 17){
          h4(bankc[17,3])
        }
        else if (index$index == 18){
          h4(bankc[18,3])
        }
        else if (index$index == 19){
          h4(bankc[19,3])
        }
        else if (index$index == 20){
          h4(bankc[20,3])
        } 
        
      }
      )
    }
  )
  
  #Tic tac toe ----
  
  # Variables
  activeBtn <- NA
  activeQuestion <- NA
  player <- NA
  opponent <- NA
  scoreMatrix <-
    matrix(
      data = rep.int(0, times = TILE_COUNT),
      nrow = GRID_SIZE,
      ncol = GRID_SIZE
    )
  gameProgress <- FALSE
  
  # Helper Functions
  .tileCoordinates <- function(tile = NULL, index = NULL) {
    row <- -1
    col <- -1
    
    # if: button tile is given, derive from id
    # else: derive from index
    if (!is.null(tile)) {
      # grid-[row]-[col]
      tile <- strsplit(tile, "-")[[1]]
      tile <- tile[-1] # remove oxo
      
      row <- strtoi(tile[1])
      col <- strtoi(tile[2])
    } else {
      row <- (index - 1) %/% GRID_SIZE + 1
      col <- index - (GRID_SIZE * (row - 1))
    }
    
    coordinates <- list("row" = row,
                        "col" = col)
    
    return(coordinates)
  }
  
  .tileIndex <- function(tile) {
    coords <- .tileCoordinates(tile)
    
    index = GRID_SIZE * (coords$row - 1) + coords$col
    
    return(index)
  }
  
  .btnReset <- function(index) {
    coords <- .tileCoordinates(index = index)
    id <- paste0("grid-", coords$row, "-", coords$col)
    updateButton(
      session = session,
      inputId = id,
      label = "?",
      disabled = FALSE
    )
  }
  
  .score <- function(score, tile, value) {
    i <- .tileCoordinates(tile)
    
    score[i$row, i$col] <- value
    
    return(score)
  }
  
  .gameCheck <- function(mat) {
    rows <- rowSums(mat)
    cols <- colSums(mat)
    
    if (GRID_SIZE > 1) {
      mainD <- sum(diag(mat))
      rotated <- apply(t(mat), 2, rev)
      offD <- sum(diag(rotated))
      
      if (GRID_SIZE %in% rows ||
          GRID_SIZE %in% cols ||
          mainD == GRID_SIZE || offD == GRID_SIZE) {
        return("win")
      } else if (-GRID_SIZE %in% rows ||
                 -GRID_SIZE %in% cols == 1 ||
                 mainD == -GRID_SIZE || offD == -GRID_SIZE) {
        return("lose")
      } else if (any(mat == 0)) {
        return("continue")
      } else {
        return("draw")
      }
    } else {
      ifelse(rows == 1 && rows != 0, return("win"), return("lose"))
    }
  }
  
  .boardBtn <- function(tile) {
    index <- .tileIndex(tile)
    activeQuestion <<- gameSet[index, "id"]
    
    output$question <- renderUI({
      withMathJax()
      return(gameSet[index, "question"])
    })
    
    output$answer <- .ansFunc(index, gameSet)
    
    if (gameSet[index, "extraOutput"] != "") {
      output$extraOutput <- renderText({
        gameSet[index, "extraOutput"]
      })
    } else {
      output$extraOutput <- NULL
    }
    
    #Retrigger MathJax processing
    output$trigger1 <- renderUI({
      withMathJax()
    })
    output$trigger2 <- renderUI({
      withMathJax()
    })
    
    #Enable Submit Button
    updateButton(session = session,
                 inputId = "submit",
                 disabled = FALSE)
  }
  
  .ansFunc <- function(index, df) {
    if (df[index, "format"] == "numeric") {
      renderUI({
        numericInput(inputId = "ans",
                     label = df[index, "label"],
                     value = 0)
      })
    } else if (df[index, "format"] == "two") {
      renderUI({
        radioGroupButtons(
          inputId = "ans",
          choices = list(df[index, "A"],
                         df[index, "B"]),
          selected = character(0),
          checkIcon = list(
            yes = icon("check-square"),
            no = icon("square-o")
          ),
          status = "textGame",
          direction = "horizontal",
          individual = TRUE
        )
      })
    } else if (df[index, "format"] == "three") {
      renderUI({
        radioGroupButtons(
          inputId = "ans",
          choices = list(df[index, "A"],
                         df[index, "B"],
                         df[index, "C"]),
          selected = character(0),
          checkIcon = list(
            yes = icon("check-square"),
            no = icon("square-o")
          ),
          status = "textGame",
          direction = "vertical"
        )
      })
    } else {
      renderUI({
        radioGroupButtons(
          inputId = "ans",
          choices = list(df[index, "A"],
                         df[index, "B"],
                         df[index, "C"],
                         df[index, "D"]),
          selected = character(0),
          checkIcon = list(
            yes = icon("check-square"),
            no = icon("square-o")
          ),
          status = "textGame",
          direction = "vertical"
        )
      })
    }
  }
  
  .gameReset <- function() {
    lapply(1:TILE_COUNT, .btnReset)
    qSelected <<-
      sample(seq_len(nrow(questionBank)), size = TILE_COUNT, replace = FALSE)
    gameSet <<- questionBank[qSelected,]
    
    output$question <-
      renderUI({
        return("Click a button on the game board to get started on your new game.")
      })
    output$answer <- renderUI({
      ""
    })
    output$extraOutput <- renderUI({
      ""
    })
    scoreMatrix <<-
      matrix(
        data = rep.int(0, times = TILE_COUNT),
        nrow = GRID_SIZE,
        ncol = GRID_SIZE
      )
    gameProgress <- FALSE
    activeBtn <- NA
    
    updateButton(session = session,
                 inputId = "submit",
                 disabled = TRUE)
  }
  
  #Feedback ----
 # observeEvent(
 #   eventExpr = input$submit,
 #   handlerExpr = {
 #     output$mark <- renderUI({
 #       index <- .tileIndex(activeBtn)
 #         correct_answer <- questionBank[index, 16]
 #         success <- input$answer == correct_answer
 #         if (success) {
 #           renderIcon(
 #             icon = "correct",
 #             width = "4"
 #           )
 #         }
 #         else {
 #           renderIcon(
 #             icon = "incorrect",
 #             width = 4
 #           )
 #         }
 #       }
 # )
 #   }
 # )
 #    
  ## BEGIN App Specific xAPI Wrappers ----
  .generateStatement <- function(session, verb = NA, object = NA, description = NA) {
    if(is.na(object)){
      object <- paste0("#shiny-tab-", session$input$tabs)
    }
    
    stmt <- boastUtils::generateStatement(
      session,
      verb = verb,
      object = object,
      description = description
    )
    
    response <- boastUtils::storeStatement(session, stmt)
    
    return(response)
  }
  
  .generateAnsweredStatement <- function(session, verb = NA, object = NA, description = NA, interactionType = NA, response = NA, success = NA, completion = FALSE) {
    
    stmt <- boastUtils::generateStatement(
      session,
      verb = verb,
      object = object,
      description = paste0("Question ", activeQuestion, ": ", description),
      interactionType = interactionType,
      success = success,
      response = response,
      completion = completion,
      extensions = list(
        ref = "https://educationshinyappteam.github.io/BOAST/xapi/result/extensions/scoreMatrix",
        value = paste(as.data.frame(scoreMatrix), collapse = ", ")
      )
    )
    
    response <- boastUtils::storeStatement(session, stmt)
    
    return(response)
  }
  ## END App Specific xAPI Wrappers ----
  
  # Define navigation buttons
  observeEvent(
    eventExpr = input$go1, 
    handlerExpr = {
    updateTabItems(session, "tabs", "qqq")
  })
  
  # Read in data and generate the first subset ----
  questionBank <-
    read.csv("bank.csv",
             stringsAsFactors = FALSE,
             as.is = TRUE)
  qSelected <-
    sample(seq_len(nrow(questionBank)), size = TILE_COUNT, replace = FALSE)
  gameSet <- questionBank[qSelected,]
  
  # Program the Reset Button
  observeEvent(input$reset, {
    .generateStatement(session, object = "reset", verb = "interacted", description = "Game board has been reset.")
    .gameReset()
  })
  
  # Render Game Board / Attach Observers
  output$gameBoard <- renderUI({
    board <- list()
    index <- 1
    
    sapply(1:GRID_SIZE, function(row) {
      sapply(1:GRID_SIZE, function(column) {
        id <- paste0("grid-", row, "-", column)
        
        board[[index]] <<- tags$li(
          actionButton(
            inputId = paste0("grid-", row, "-", column),
            label = "?",
            color = "primary",
            style = "bordered",
            class = "grid-fill"
          ),
          class = "grid-tile"
        )
        
        observeEvent(session$input[[id]], {
          activeBtn <<- id
          .boardBtn(id)
          .generateStatement(session, object = activeBtn, verb = "interacted", description = paste0("Tile ", activeBtn, " selected. Rendering question: ", activeQuestion, "."))
        })
        
        index <<- index + 1
      })
    })
    
    tags$ol(board, class = paste(
      "grid-board",
      "grid-fill",
      paste0("grid-", GRID_SIZE, "x", GRID_SIZE)
    ))
  })
  
  # Program Submit Button
  observeEvent(input$submit, {
    index <- .tileIndex(activeBtn)
    answer <- ""
    
    if (gameSet[index, "format"] == "numeric") {
      answer <- gameSet[index, "answer"]
    } else {
      answer <- gameSet[index, gameSet[index, "answer"]]
    }
    
    success <- input$ans == answer
    
    if (is.null(success) || length(success) == 0) {
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "Please select an answer before pressing Submit.",
        type = "error"
      )
    } else if (success) {
      updateButton(
        session = session,
        inputId = activeBtn,
        label = player,
        disabled = TRUE
      )
      scoreMatrix <<- .score(scoreMatrix, activeBtn, 1)
      
    } else {
      updateButton(
        session = session,
        inputId = activeBtn,
        label = opponent,
        disabled = TRUE
      )
      scoreMatrix <<- .score(scoreMatrix, activeBtn,-1)
      
    }
    
    # Check for game over states
    .gameState <- .gameCheck(scoreMatrix)
    completion <- ifelse(.gameState == "continue", FALSE, TRUE)
    interactionType <- ifelse(gameSet[index,]$format == "numeric", "numeric", "choice")
    
    .generateAnsweredStatement(
      session,
      object = activeBtn,
      verb = "answered",
      description = gameSet[index,]$question,
      response = input$ans,
      interactionType = interactionType,
      success = success,
      completion = completion
    )
    
    if (.gameState == "win") {
      .generateStatement(session, object = "qqq", verb = "completed", description = "Player has won the game.")
      confirmSweetAlert(
        session = session,
        inputId = "endGame",
        title = "You Win!",
        text = "You've filled either a row, a column, or a main diagonal. Start over and play a new game.",
        btn_labels = "Start Over"
      )
    } else if (.gameState == "lose") {
      .generateStatement(session, object = "qqq", verb = "completed", description = "Player has lost the game.")
      confirmSweetAlert(
        session = session,
        inputId = "endGame",
        title = "You lose :(",
        text = "Take a moment to review the concepts and then try again.",
        btn_labels = "Start Over"
      )
    } else if (.gameState == "draw") {
      .generateStatement(session, object = "game", verb = "completed", description = "Game has ended in a draw.")
      confirmSweetAlert(
        session = session,
        inputId = "endGame",
        title = "Draw!",
        text = "Take a moment to review the concepts and then try again.",
        btn_labels = "Start Over"
      )
    }
    if (is.null(success) || length(success) == 0) {
      updateButton(
        session = session,
        inputId = "submit",
        disabled = FALSE
      )
    } else{
      updateButton(
        session = session,
        inputId = "submit",
        disabled = TRUE
      )
    }
  })
  
  observeEvent(
    eventExpr = input$submit,
    handlerExpr = {
      index <- .tileIndex(activeBtn)
      answer <- ""
      
      if (gameSet[index, "format"] == "numeric") {
        answer <- gameSet[index, "answer"]
      } 
      else {
        answer <- gameSet[index, gameSet[index, "answer"]]
      }
      
      success <- input$ans == answer
      if (success) {
        output$mark <- renderIcon(
          icon = "correct",
          width = 100
        )
      }
      else {
        output$mark <- renderIcon(
          icon = "incorrect",
          width = 100
        )
        output$feedback <- renderUI(
          paste("Your answer is incorrect. The correct answer is", answer, ".")
        )
      }
    },
    ignoreNULL = TRUE
  )
  
  observeEvent(input$tabs, {
    if (input$tabs == "qqq") {
      if (!gameProgress) {
        shinyalert(
          title = "Player Select",
          text = "Select whether you want to play as O or X.",
          showConfirmButton = TRUE,
          confirmButtonText = "Play as X",
          showCancelButton = TRUE,
          cancelButtonText = "Play as O"
        )
        gameProgress <<- TRUE
      }
    }
    .generateStatement(session, verb = "experienced", description = paste0("Navigated to ", input$tabs, " tab."))
  }, ignoreInit = TRUE)
  
  observeEvent(input$endGame, {
    .generateStatement(session, object = "endGame", verb = "interacted", description = paste("Game has been reset."))
    .gameReset()
  })
  
  observeEvent(input$shinyalert, {
    if (input$shinyalert == TRUE) {
      player <<- "X"
      opponent <<- "O"
    }
    if (input$shinyalert == FALSE) {
      player <<- "O"
      opponent <<- "X"
    }
    
    .generateStatement(session, object = "shinyalert", verb = "interacted", description = paste0("User has selected player: ", player))
    
    output$player <- renderUI({
      return(paste0("You are playing as ", player, "."))
    })
  })
  
  # #### question bank ####
  # bank <- read.csv('bank.csv', stringsAsFactors = FALSE)
  # bank <- bank[,c(1,2,4:7,9,16)]
  # bank[17:18,2] <- gsub('11',"\\\\",gsub("([\\\\])",'1',bank[17:18,2]))
  # Qs <- nrow(bank)
  # 
  # 
  # ######## MY SERVER CODE ##########
  # 
  # 
  # X.icon <- makeIcon(iconUrl = 'X.PNG', iconWidth= 90)
  # O.icon <- makeIcon(iconUrl= 'O.PNG',iconWidth= 105)
  # value <- matrix(rep(-3,9),3,3)
  # values <- list(value) # 
  # container <- c() # contains right or wrong answers after submit button is pressed 
  # XsAndOs <- list()
  # resolved <- c(rep(FALSE, Qs))
  # 
  # #lists <- reactiveValues(container <- c(),values <- list(),value <- NULL)
  # 
  # sr1.1=Polygon(cbind(c(0.5,0.5,3.5,3.5), c(2.5,1.5,1.5,2.5))) # inner, middle horizontal lines
  # sr1.2=Polygon(cbind(c(0.5,0.5,3.5,3.5), c(2.5,3.5,3.5,2.5))) # inner, horizontal lines
  # sr1.3=Polygon(cbind(c(0.5,0.5,3.5,3.5), c(0.5,1.5,1.5,0.5))) # inner, horizontal lines
  # sr2=Polygon(cbind(c(1.5,1.5,2.5,2.5), c(3.5,0.5,0.5,3.5)))
  # sr3=Polygon(cbind(c(0.5,0.5,0.5,3.5), c(0.52,3.47,0.5,0.5))) #outer 
  # sr4=Polygon(cbind(c(3.5,3.5,0.5,3.5), c(0.52,3.47,3.5,3.5))) #square
  # srs1.1=Polygons(list(sr1.1), 's1.1')
  # srs1.2=Polygons(list(sr1.2), 's1.2')
  # srs1.3=Polygons(list(sr1.3), 's1.3')
  # srs2=Polygons(list(sr2), 's2')
  # srs3=Polygons(list(sr3), 's3')
  # srs4=Polygons(list(sr4), 's4')
  # #srs1.2,srs1.3
  # spp = SpatialPolygons(list(srs1.1,srs2,srs3,srs4), 1:4)
  # #spp = SpatialPolygons(list(srs3,srs4), 1:2)
  # 
  # r <- raster(xmn = 0.5, xmx = 3.5, ymn = 0.5, ymx = 3.5, nrows = 3, ncols = 3)
  # values(r) <- matrix(1:9, nrow(r), ncol(r), byrow = TRUE)
  # crs(r) <- CRS("+init=epsg:4326")
  # new.board <- leaflet(options = leafletOptions(zoomControl = FALSE, doubleClickZoom = FALSE, minZoom = 7, maxZoom = 7)) %>%  addPolygons(data=spp) %>% addRasterImage(r, colors="Set3")
  # # %>% addTiles()
  # line <- function(){
  #   #recall, in the value matrix, zero's represent O's, or wrong answers
  #   for(i in 1:3){
  #     total.1 <- 0 ; total.2 <- 0
  #     for(j in 1:3){
  #       total.1 <- total.1 + value[i, j]
  #       total.2 <- total.2 + value[j, i]
  #     }
  #     if(total.1==0 | total.2==0 | total.1==3 | total.2==3){
  #       break
  #     }
  #   }
  #   total.3 <- value[1, 1] + value[2, 2] + value[3, 3]
  #   total.4 <- value[1, 3] + value[2, 2] + value[3, 1]
  #   
  #   #if the game has been won:
  #   if(total.1==0 | total.2==0 | total.3==0 | total.4==0 | total.1==3 | total.2==3 | total.3==3 | total.4==3){
  #     #place.na[!is.na(place.na)] <<- NA
  #     if(total.1==0 | total.2==0 | total.3==0 | total.4==0){
  #       warning('LOSE')
  #       return("You lost try again!")  #title(sub=list("You Are a Loser !", col="darkblue", font=2, cex=2.5), line=2)
  #     }else{
  #       warning('WIN')
  #       return("You Win! Game Over!")  #title(sub=list("You Win ! Game Over !", col="red", font=2, cex=2.5), line=2)
  #     }
  #   }
  #   
  #   
  #   
  #   
  #   #if the previous is true (if the game is over) then this will fire through as well
  #   #this will also fire through if the board is full, regardless of whether the previous if() has executed
  #   #i dont know why these two statements should both be here
  #   
  #   if(length(which(value!=-3))==9){
  #     if(total.1==0 | total.2==0 | total.3==0 | total.4==0 | total.1==3 | total.2==3 | total.3==3 | total.4==3){
  #       #if(total.1==0 | total.2==0 | total.3==0 | total.4==0){
  #       #  title(sub=list("You Are a Loser !", col="darkblue", font=2, cex=2.5), line=2)
  #       #}else{
  #       #  title(sub=list("You Win ! Game Over !", col="orange", font=2, cex=2.5), line=2)
  #       #}
  #     }else{
  #       warning('RESTART')
  #       return("Draw ! Please try again !")  #title(sub=list("Draw ! Please try again !", col="blue", font=2, cex=2.5), line=2)
  #     }
  #   }
  #   return(NULL)
  # }
  # 
  # 
  # 
  # v <- reactiveValues(doPlot = FALSE)
  # observeEvent(input$image_click, priority = 7, {
  #   validate(need(is.null(game()), label='Game is over'))
  #   
  #   #input$image_click will return coordinates when clicked, not a logical value
  #   if(!is.null(input$image_click)){
  #     v$doPlot <- TRUE
  #   }
  # })
  # 
  # #object that contains the coordinates returned from clicking the image
  # coords <- eventReactive(input$image_click, {
  #   validate(need(is.null(game()), label='Game is over'))
  #   
  #   validate(need(!is.null(input$image_click), 'need to click image'))
  #   input$image_click
  # })
  # 
  # 
  # #### keep track of clicks on A: the plot, B: the submit button, C: the next button ####
  # 
  # 
  # clicks <- reactiveValues(A=0,B=0,C=0)
  # observeEvent(input$image_click, priority = 6, {
  #   validate(need(is.null(game()), label='Game is over'))
  #   mouse.at <- coords()
  #   output$warning <- renderText('')
  #   #we dont return an error for out of bounds clicks
  #   if(!(mouse.at[[1]] > 3.5 | mouse.at[[1]] < 0.5 | mouse.at[[2]] > 3.5 | mouse.at[[2]] < 0.5)){
  #     if(!is.null(input$image_click) & clicks$A==clicks$B){
  #       #we allow the user to switch his tic tac toe selection (before a corresponding answer) here
  #       clicks$A <- (clicks$A+1)
  #       warning(clicks$A, "A")
  #     }
  #   }else{
  #     if(clicks$A == clicks$B){
  #       output$warning <- renderText('Please click a valid square')
  #     }
  #   }
  # })
  # observeEvent(input$submit, priority = 6, {
  #   
  #   #make sure the player selected an answer when he pressed submit
  #   num <- as.character(numbers$question[length(numbers$question)])
  #   #     this has been hard coded to correspond with the number of questions in the question bank
  #   ans <- if(num==1){input$'1'}else if(num==2){input$'2'}else if(num==3){input$'3'}else if(num==4){input$'4'}else if(num==5){input$'5'}else if(num==6){input$'6'}else if(num==7){input$'7'}else if(num==8){input$'8'}else if(num==9){input$'9'}else if(num==10){input$'10'}else if(num==11){input$'11'}else if(num==12){input$'12'}else if(num==13){input$'13'}else if(num==14){input$'14'}else if(num==15){input$'15'}else if(num==16){input$'16'}else if(num==17){input$'17'}else if(num==18){input$'18'}else if(num==19){input$'19'}else if(num==20){input$'20'}else if(num==21){input$'21'}else if(num==22){input$'22'}else if(num==23){input$'23'}else if(num==24){input$'24'}else if(num==25){input$'25'}else if(num==26){input$'26'}else if(num==27){input$'27'}else if(num==28){input$'28'}
  #   validate(need((ans %in% c("A", "B", "C", "D", seq(0:1000))), label='please select one of the multiple choice responses'))
  #   warning(ans, 'tit')
  #   if(!is.null(input$submit)){clicks$B <- (clicks$B+1)}
  #   warning(clicks$B, 'b')
  # })
  # observeEvent(input$nextButton, priority = 6, {
  #   if(!is.null(input$nextButton)){clicks$C <- (clicks$C+1)}
  #   warning(clicks$C, 'c')
  # })
  # 
  # 
  # #called in the next observer below 
  # ID <- reactive({
  #   validate(need(is.null(game()), label='Game is over'))
  #   
  #   validate(
  #     need(!is.null(input$image_click), 'click image') # because this will always come first
  #   )
  #   if(clicks$A>=clicks$B){
  #     if(clicks$A>clicks$B){
  #       "choice"
  #     }else if(clicks$A==clicks$B){
  #       #if the first few clicks are not valid squares, we must make sure theres no errors in the next observe handler below
  #       if(clicks$A==0){
  #         "choice"
  #       }else{
  #         "answer"
  #       }
  #     }
  #   }
  # })
  # 
  # observe({
  #   warning(XsAndOs)
  # })
  # 
  # game <- reactiveVal(NULL)
  # 
  # #contains the board as it changes throughout the game
  # #will store markers in XsAndOs list
  # #as this event fires, only one marker will be added to the XsAndOs list each time
  # #the first if statement will store an empty marker, when a box is chosen. Then,
  # #then the second will overwrite it with an X or an O, when a question is answered
  # 
  # observeEvent({input$submit
  #   input$image_click}, priority = 0,{
  #     validate(need(is.null(game()), label='Game is over'))
  #     
  #     mouse.at <- coords()
  #     ID <- ID()
  #     
  #     #if the user enters an invalid click after he already entered his choice on the board, it will not affect the outcome of an answer submission
  #     
  #     if(clicks$A > clicks$B | clicks$A == 0){
  #       validate(
  #         need(!(mouse.at[[1]] > 3.5 | mouse.at[[1]] < 0.5 | mouse.at[[2]] > 3.5 | mouse.at[[2]] < 0.5), label='please enter a valid click')
  #       )
  #     }else if(clicks$A == clicks$B & ID == "answer" & XsAndOs[[ifelse(length(XsAndOs)==0,'',length(XsAndOs))]][3] %in% c(0,1) ){
  #       #or, if the user enters an invalid click before entering a valid click, it will not crash the app
  #       if(length(XsAndOs) == clicks$A){
  #         validate(
  #           need(!(mouse.at[[1]] > 3.5 | mouse.at[[1]] < 0.5 | mouse.at[[2]] > 3.5 | mouse.at[[2]] < 0.5), label='please enter a valid click')
  #         )
  #       }
  #     }
  #     
  #     
  #     warning(ID)
  #     
  #     #statement that checks the ID return from either of the input triggering events. choice or answer
  #     
  #     if(ID=="choice"){
  #       mouse.at[[1]] <- round(mouse.at[[1]])
  #       mouse.at[[2]] <- round(mouse.at[[2]])
  #       x<<-mouse.at[[1]]
  #       y<<-mouse.at[[2]]
  #       
  #       #     should this be length(XsAndOs) ?  6/14/18
  #       
  #       XsAndOs[[(length(container)+1)]] <<- c(mouse.at[[2]],(mouse.at[[1]]-.3),NULL)
  #       leafletProxy("image", session) %>% addMarkers(lng=XsAndOs[[length(XsAndOs)]][1], lat=XsAndOs[[length(XsAndOs)]][2], layerId = "dummy")
  #       
  #     }else if(ID=="answer"){
  #       value <<- values[[length(values)]]
  #       if(container[length(container)]==0){
  #         value[x, y] <<- 0
  #       }else if(container[length(container)]==1){
  #         value[x, y] <<- 1
  #       }
  #       values[[1]] <<- matrix(-3,3,3)
  #       values[[(length(values)+1)]] <<- value
  #       temp <<- which((values[[length(values)]] - values[[ (length(values)-1) ]])!=0)
  #       values[[length(values)]][temp] <<- container[length(container)]
  #       
  #       #plotting part, this will overwrite what previously passed through the first if statement in this observe handler
  #       
  #       if(temp<4){
  #         XsAndOs[[(length(XsAndOs))]][3] <<- ifelse(matrix(values[[length(values)]],3,3)[[temp]]==0, 0, 1)
  #       }else if(temp<7){ 
  #         XsAndOs[[(length(XsAndOs))]][3] <<- ifelse(matrix(values[[length(values)]],3,3)[[temp]]==0, 0, 1)
  #       }else if(temp<10){
  #         XsAndOs[[(length(XsAndOs))]][3] <<- ifelse(matrix(values[[length(values)]],3,3)[[temp]]==0, 0, 1)
  #       }else{
  #         stop("OH NO")
  #       }
  #       
  #       leafletProxy("image", session)  %>% removeMarker(layerId = "dummy")
  #       warning(XsAndOs[[length(XsAndOs)]][3])
  #       if(!(XsAndOs[[length(XsAndOs)]][3] %in% c(0,1))){stop('VERY BAD')}
  #       if(XsAndOs[[length(XsAndOs)]][3]==1){
  #         leafletProxy("image", session)  %>% addMarkers(y,x+.56,icon = X.icon)
  #       }else{
  #         leafletProxy("image", session)  %>% addMarkers(y,x+.56,icon = O.icon)
  #       }
  #       
  #       temp <- line()
  #       game(temp) #reactiveVal syntax
  #       message(temp)
  #     }
  #   }
  # )  
  # 
  # #### alerts user of status of game (if it is over) ####
  # #return from line() function
  # output$gameMessage <- renderText({
  #   validate(need(!is.null(game), label = 'Game is over'))
  #   
  #   game <- game()
  #   game
  # })
  # 
  # 
  # ####render image of tic tac toe board####
  # output$image <- renderLeaflet({
  #   new.board
  #   #  out = out %>% addMarkers(lng=temp[[1]],lat=temp[[2]], icon=tryCatch(ifelse(temp[[3]]==1, X.icon, O.icon), error=function(e)NULL))
  # })
  # 
  # #### go button ####
  # observeEvent(input$go, priority=1, {
  #   updateTabItems(session, "tabs", "qqq")
  # })
  # observe({
  #   validate(
  #     need(is.null(input$image_click), message='')
  #   )
  #   output$directions <- renderText({"Begin by selecting the square on the tic-tac-toe grid you would like."})
  # })
  # 
  # ####start over; new game####
  # observeEvent(input$reset, priority = 5,{
  #   if(!is.null(game())){
  #     game(NULL)
  #   }
  #   leafletProxy("image", session) %>% clearMarkers()
  #   
  #   value <<- matrix(rep(-3,9),3,3)
  #   values <<- list(value)
  #   container <<- c()
  #   XsAndOs <<- list()
  #   #need to use scoping operator because i didnt create the (above) as reactive objects
  #   clicks$A <- 0
  #   clicks$B <- 0
  #   clicks$C <- 0
  #   
  #   
  #   #create arbitrary cut off of 9 questions (i would say its reasonable) keep in mind the quesiton bank has 18 questions
  #   #if more than 9 questions have been answered, then we reset the question bank so that all questions can be drawn from
  #   
  #   if(length(which(answers() %in% c("correct","incorrect")))>9){
  #     numbers$question <- c()
  #   }else{
  #     #put the incorrectly answered questions back into play
  #     if("incorrect" %in% unique(answers())){
  #       if("correct" %in% unique(answers())){
  #         #numbers$question <- numbers$question[which((answers()=="correct"))] #we assign the taken questions to be only the correctly answered ones
  #         numbers$question <- which(answers()=="correct") #we assign the taken questions to be only the correctly answered ones
  #       }else{
  #         numbers$question <- c() #since no answers were correct, all questions are in play
  #       }
  #     }
  #     
  #     #get rid of most recent question (but if it was answered correctly, it still will not be included)
  #     numbers$question <- numbers$question[-length(numbers$question)]
  #   }
  #   
  #   #resample to get new random question for when the game is restarted (when image is clicked)
  #   
  #   space<-c(1:Qs)
  #   numbers$question[(length(numbers$question)+1)] <- sample(space[-tryCatch(if(numbers$question){numbers$question}, error=function(e) 29)], 1)
  #   warning(numbers$question)
  #   
  #   updateButton(session, 'nextButton',  disabled = TRUE, style= 'danger')
  #   output$directions <- renderText({"Begin by selecting the square on the plot you would like"})
  # })
  # 
  # #temporary place holders, will be used as a logical pass to the conditional panel containing the renderUI output
  # 
  # observeEvent(input$image_click, {
  #   validate(need(is.null(game()), label='Game is over'))
  #   
  #   output$temp <- renderText({'1'})
  # })
  # observeEvent(input$reset, priority = 8, {
  #   output$temp <- renderText({'2'})
  # })
  # 
  # 
  # #### enact game over mode ####
  # observeEvent(input$submit, priority = -1, {
  #   validate(need(!is.null(game()), label='Game is over'))
  #   
  #   #make sure the player selected an answer when he pressed submit
  #   
  #   num <- as.character(numbers$question[length(numbers$question)])
  #   
  #   #     this has been hard coded to correspond with the number of questions in the question bank
  #   
  #   ans <- if(num==1){input$'1'}else if(num==2){input$'2'}else if(num==3){input$'3'}else if(num==4){input$'4'}else if(num==5){input$'5'}else if(num==6){input$'6'}else if(num==7){input$'7'}else if(num==8){input$'8'}else if(num==9){input$'9'}else if(num==10){input$'10'}else if(num==11){input$'11'}else if(num==12){input$'12'}else if(num==13){input$'13'}else if(num==14){input$'14'}else if(num==15){input$'15'}else if(num==16){input$'16'}else if(num==17){input$'17'}else if(num==18){input$'18'}else if(num==19){input$'19'}else if(num==20){input$'20'}else if(num==21){input$'21'}else if(num==22){input$'22'}else if(num==23){input$'23'}else if(num==24){input$'24'}else if(num==25){input$'25'}else if(num==26){input$'26'}else if(num==27){input$'27'}else if(num==28){input$'28'}
  #   validate(need((ans %in% c("A", "B", "C", "D", seq(0:1000))), label='please select one of the multiple choice responses'))
  #   
  #   updateButton(session, 'nextButton', 
  #                disabled = TRUE)
  #   updateButton(session, 'submit', 
  #                disabled = TRUE)
  #   output$directions <- renderText({"If you would like to play again, press 'Start new game'!"})
  # })
  # 
  # 
  # ######## QUESTIONS #########
  # 
  # 
  # 
  # 
  # #### random question ####
  # 
  # numbers <- reactiveValues(question = c())
  # observeEvent(input$image_click, once=TRUE, priority = 9, {
  #   validate(need(is.null(game()), label='Game is over'))
  #   
  #   numbers$question[1] <- sample(1:Qs, 1)
  #   updateButton(session, 'nextButton',  disabled = TRUE)
  #   output$directions <- renderText({"Now answer the question and press submit"})
  # })
  # observeEvent(input$nextButton, priority = 4, {
  #   space <- c(1:Qs)
  #   numbers$question[(length(numbers$question)+1)] <- sample(space[-tryCatch(numbers$question, error=function(e) 29)], 1)
  #   updateButton(session, 'nextButton',  disabled = TRUE)
  #   output$directions <- renderText({"Now select another square on the board"})
  #   if(clicks$A == (clicks$C + 1)){
  #     updateButton(session, 'submit',  disabled = FALSE)
  #     output$directions <- renderText({"Now answer the question and press submit"})
  #   }
  # })
  # 
  # 
  # ####output random question####
  # output$CurrentQuestion <- renderUI({
  #   warning(numbers$question)
  #   num <- as.character(numbers$question[length(numbers$question)])
  #   warning(num)
  #   temp <- NULL
  #   
  #   # THIS WAS NOT AS DYNAMIC
  #   #if(num %in% c(1,2)){
  #   #  numericInput(inputId = (num), bank[num, 2], min=0,max=100, val=0)
  #   #}else if(num %in% c(3,4,7,10:12)){ 
  #   #  radioButtons(inputId = (num), label=(bank[num, 2]), choiceNames=c(bank[num, 3], bank[num, 4]), choiceValues = c("A", "B"),  selected = character(0))
  #   #}else if(num %in% c(5,6,8,9,13:16)){
  #   #  radioButtons(inputId = (num), label=ifelse(is.null(temp), bank[num, 2], temp), choiceNames=c(bank[num, 3], bank[num, 4], bank[num, 5], bank[num, 6]), choiceValues = c("A", "B", "C", "D"), selected = character(0))
  #   #}else if(num %in% c(17,18)){
  #   #  withMathJax(
  #   #    h4(sprintf(
  #   #      bank[num, 2]
  #   #    ))
  #   #  )
  #   #  #temp <- ""
  #   # }
  #   
  #   
  #   
  #   if(num %in% c(17,18)){
  #     #this mathjax call is hard coded for specific questions in the CSV
  #     withMathJax(
  #       h4(sprintf(
  #         bank[num, 2]
  #       ))
  #     )
  #   }else if(!(FALSE %in% unique(as.vector(bank[num,3:6]=='')))){
  #     numericInput(inputId = (num), bank[num, 2], min=0,max=1000, val=0)
  #   }else if(!(FALSE %in% unique(as.vector(bank[num,5:6]=='')))){
  #     radioButtons(inputId = (num), label=(bank[num, 2]), choiceNames=c(bank[num, 3], bank[num, 4]), choiceValues = c("A", "B"),  selected = character(0))
  #   }else if(bank[num,6]==''){
  #     radioButtons(inputId = (num), label=(bank[num, 2]), choiceNames=c(bank[num, 3], bank[num, 4], bank[num, 5]), choiceValues = c("A", "B", "C"), selected = character(0))
  #   }else{
  #     radioButtons(inputId = (num), label=(bank[num, 2]), choiceNames=c(bank[num, 3], bank[num, 4], bank[num, 5], bank[num, 6]), choiceValues = c("A", "B", "C", "D"), selected = character(0))
  #   }
  # })
  # 
  # #hard coded observer for specific questions in the csv
  # output$CurrentQuestion.extra <- renderUI({
  #   num <- as.character(numbers$question[length(numbers$question)])
  #   if(num == 3){
  #     img(src="CIQ3.png",height = 150,width = 500,align = "middle")
  #   }else if(num == 4){
  #     img(src="CIQ4.png",height = 150,width = 400,align = "middle")
  #   }
  #   else if(num == 19){
  #     img(src="CIQ19.png",height = 150,width = 400,align = "middle")
  #   }
  #   else if(num == 20){
  #     img(src="CIQ20.png",height = 150,width = 400,align = "middle")
  #   }
  #   else if(num == 21){
  #     img(src="CIQ21.png",height = 150,width = 400,align = "middle")
  #   }
  #   else if(num == 22){
  #     img(src="CIQ22.png",height = 150,width = 400,align = "middle")
  #   }
  #   else if(num == 23){
  #     img(src="CIQ3.png",height = 150,width = 400,align = "middle")
  #   }
  #   else if(num == 24){
  #     img(src="CIQ19.png",height = 150,width = 400,align = "middle")
  #   }
  #   else if(num == 25){
  #     img(src="CIQ19.png",height = 150,width = 400,align = "middle")
  #   }
  #   else if(num == 26){
  #     img(src="CIQ22.png",height = 150,width = 400,align = "middle")
  #   }
  #   else if(num == 27){
  #     img(src="CIQ19.png",height = 150,width = 400,align = "middle")
  #   }
  #   else if(num == 28){
  #     img(src="CIQ22.png",height = 150,width = 400,align = "middle")
  #   }
  #   else if(num == 17){
  #     radioButtons(inputId = (num), label='', choiceNames=c(bank[num, 3], bank[num, 4], bank[num, 5]), choiceValues = c("A", "B", "C"), selected = character(0))
  #   }else if(num == 18){
  #     radioButtons(inputId = (num), label='', choiceNames=c(bank[num, 3], bank[num, 4], bank[num, 5], bank[num, 6]), choiceValues = c("A", "B", "C", "D"), selected = character(0))
  #   }
  # })  
  # 
  # 
  # ####logical flow of answering questions; some extra code to deal with button disabling sequence####
  # observeEvent(input$submit, {
  #   #make sure the player selected an answer when he pressed submit
  #   num <- as.character(numbers$question[length(numbers$question)])
  #   #     this has been hard coded to correspond with the number of questions in the question bank
  #   ans <- if(num==1){input$'1'}else if(num==2){input$'2'}else if(num==3){input$'3'}else if(num==4){input$'4'}else if(num==5){input$'5'}else if(num==6){input$'6'}else if(num==7){input$'7'}else if(num==8){input$'8'}else if(num==9){input$'9'}else if(num==10){input$'10'}else if(num==11){input$'11'}else if(num==12){input$'12'}else if(num==13){input$'13'}else if(num==14){input$'14'}else if(num==15){input$'15'}else if(num==16){input$'16'}else if(num==17){input$'17'}else if(num==18){input$'18'}else if(num==19){input$'19'}else if(num==20){input$'20'}else if(num==21){input$'21'}else if(num==22){input$'22'}else if(num==23){input$'23'}else if(num==24){input$'24'}else if(num==25){input$'25'}else if(num==26){input$'26'}else if(num==27){input$'27'}else if(num==28){input$'28'}
  #   validate(need((ans %in% c("A", "B", "C", "D", seq(0:1000))), label='please select one of the multiple choice responses'))
  #   
  #   updateButton(session, 'submit', disabled = TRUE)
  #   
  #   updateButton(session, 'nextButton',disabled = FALSE)
  #   
  #   output$directions <- renderText({"Now press the next button"})
  # })
  # observeEvent(input$image_click, {
  #   validate(need(is.null(game()), label='Game is over'))
  #   
  #   if(!(clicks$A > (clicks$C + 1))){
  #     updateButton(session, 'submit', disabled = FALSE)
  #     output$directions <- renderText({"Now answer the question and press submit"})
  #   }
  # })
  # 
  # 
  # ####checks answer####
  # answers <- reactiveVal(c(rep('', Qs)))
  # observeEvent(input$submit, {
  #   #make sure the player selected an answer when he pressed submit
  #   num <- as.character(numbers$question[length(numbers$question)])
  #   #     this has been hard coded to correspond with the number of questions in the question bank
  #   ans <- if(num==1){input$'1'}else if(num==2){input$'2'}else if(num==3){input$'3'}else if(num==4){input$'4'}else if(num==5){input$'5'}else if(num==6){input$'6'}else if(num==7){input$'7'}else if(num==8){input$'8'}else if(num==9){input$'9'}else if(num==10){input$'10'}else if(num==11){input$'11'}else if(num==12){input$'12'}else if(num==13){input$'13'}else if(num==14){input$'14'}else if(num==15){input$'15'}else if(num==16){input$'16'}else if(num==17){input$'17'}else if(num==18){input$'18'}else if(num==19){input$'19'}else if(num==20){input$'20'}else if(num==21){input$'21'}else if(num==22){input$'22'}else if(num==23){input$'23'}else if(num==24){input$'24'}else if(num==25){input$'25'}else if(num==26){input$'26'}else if(num==27){input$'27'}else if(num==28){input$'28'}
  #   validate(need((ans %in% c("A", "B", "C", "D", seq(0:1000))), label='please select one of the multiple choice responses'))
  #   
  #   temp <- answers()
  #   num <- as.character(numbers$question[length(numbers$question)])
  #   #this has been hard coded to correspond with the number of questions in the question bank
  #   ans <- if(num==1){input$'1'}else if(num==2){input$'2'}else if(num==3){input$'3'}else if(num==4){input$'4'}else if(num==5){input$'5'}else if(num==6){input$'6'}else if(num==7){input$'7'}else if(num==8){input$'8'}else if(num==9){input$'9'}else if(num==10){input$'10'}else if(num==11){input$'11'}else if(num==12){input$'12'}else if(num==13){input$'13'}else if(num==14){input$'14'}else if(num==15){input$'15'}else if(num==16){input$'16'}else if(num==17){input$'17'}else if(num==18){input$'18'}else if(num==19){input$'19'}else if(num==20){input$'20'}else if(num==21){input$'21'}else if(num==22){input$'22'}else if(num==23){input$'23'}else if(num==24){input$'24'}else if(num==25){input$'25'}else if(num==26){input$'26'}else if(num==27){input$'27'}else if(num==28){input$'28'}
  #   if(ans == bank[num, 8]){
  #     temp2 <- "correct"
  #   }else{
  #     temp2 <- "incorrect"
  #   }
  #   temp[num] <- temp2
  #   answers(temp)
  #   container[(length(container)+1)] <<- ifelse(temp[num] == "correct", 1, 0)
  # })
  
}
  boastUtils::boastApp(ui = ui, server = server)
  