library(shiny)
library(shinydashboard)
library(shinyBS)
library(raster)
library(shinyWidgets)
library(boastUtils)
#library(shinyjs)
library(shinyalert)

#This app will be used to explore and play around with the assumptions and 
#diagnostics of regression
GRID_SIZE <- 3
TILE_COUNT <- GRID_SIZE ^ 2
questionBank <-
  read.csv(
    "questionBank.csv",
    stringsAsFactors = FALSE,
    as.is = TRUE)
bankc <- read.csv("ChallengeOutput.csv")
bankc = data.frame(
  lapply(
    X = bankc,
    FUN = as.character),
  stringsAsFactors = FALSE)

ui <- list(
  dashboardPage(
    skin = "black",
    dashboardHeader(
      title = "Regr. Assumptions",
      titleWidth = 250,
      # Creating header buttons ----   
      tags$li(
        class = "dropdown",
        actionLink("info",
                   icon("info"))
      ),
      tags$li(
        class = "dropdown",
        tags$a(
          target = "_blank", 
          icon("comments"),
          href = "https://pennstate.qualtrics.com/jfe/form/SV_7TLIkFtJEJ7fEPz?appName=Assumptions"
        )
      ),
      tags$li(
        class = "dropdown",
        tags$a(
          icon("home"),
          href = 'https://shinyapps.science.psu.edu/'
        )
      )
    ),

    #adding pages to sidebar ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem(text = "Overview",
                 tabName = "instruction", 
                 icon = icon("dashboard")
        ),
        menuItem(text = "Prerequisites", 
                 tabName = "prereq", 
                 icon = icon("book")
        ),
        menuItem(text = "Challenge",
                 tabName = "explore", 
                 icon = icon("gears")
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
      tags$div(
        class = "sidebar-logo", 
        boastUtils::sidebarFooter()
        )
    ),
# Creating overview page ----
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "instruction",
          h1("Regression Assumptions and Diagnostics"),
          h2("About"),
          p("This app will allow you to explore how to read diagnostic plots
                while interacting with different transformations to help you better 
                understand the assumptions of regression."),
          br(),                     
          h2("Instructions"),
          tags$ul(
            p(
              tags$li("Each 'Mystery Model' on the challenge page is generated
                        with variables or their transformations being the response 
                        (Y) or the predictor variables (X1, X2).")
            ),
            p(
              tags$li("Watch how diagnostic plots change when you adjust the 
                        predictors and response variables using different transformations. 
                        Note that transforming the Y or X variables will effect 
                      plots in different ways. You also have the option to change 
                      the variances and sample size of each term.")
            ),
            p(
              tags$li('Read the challenge prompt and adjust the sliders to answer.')
            ),
            p(
              tags$li('Once you have an answer, check to see if you are correct.')
            ),
            p(
              tags$li('Complete as many challenges as you desire, and then move on 
                      to the tic-tac-toe game.')
            ),
            p(
              tags$li("In the game, the object is to win at tic-tac-toe where 
                        you are playing X's or O's.  Select a square, then answer the 
                        question. If you get the question correct, your selected 
                        X or O goes in the square. If you get it wrong, the other 
                      letter goes in the square.")
            )
          ),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "go",
              label = "Prerequisites",
              icon = icon("book"),
              style = "default",
              size = "large")
          ),
          br(),
          h2("Acknowledgements"),
          p("This app was developed and coded by TJ McIntyre, with the help 
                  of Ryan Voyack and was updated by Lydia Bednarczyk."),
          div(class = "updated", "Last Update: 7/9/2021 by LSB.")
        ),
#Adding pre-requisites page ----
        tabItem(
          tabName = "prereq",
          h2("Prerequisites"),
          p("In order to get the most out of this app, it is important to
                   understand background information about assumptions and diagnostic
                   plots in regression."),
          tags$ul(
            p(
              tags$li("Transforming the x values is appropriate when non-linearity 
                        is the only problem (i.e., the independence, normality, 
                        and equal variance conditions are met). Transforming the 
                        y values should be considered when non-normality and/or 
                        unequal variances are the problems with the model.")
            ),
            p(
              tags$li("The Fitted vs Residuals plot can be used to check the 
                        assumption of linearity (any location on the x axis, the 
                        average residual should be close to 0) and it can also be 
                        used to check the assumption of equal variances (at any 
                        location on the x axis, the variability of the residual 
                        should be similar).")
            ),
            p(
              tags$li("The Normal Q-Q plot can be used to check the assumption 
                        of normal errors: i.e. the majority of the points should 
                        be a straight line. Skewness can also be seen by this plot. 
                        See the ", 
                      a(href='https://psu-eberly.shinyapps.io/QQ_Plot/', 'Q-Q 
                            plot'), " app for further exploration.")
            ),
            p(
              tags$li("The Scale-Location plot can be used to check the assumption 
                        of equal variances, at any location of the x axis, the upper 
                        bound of the residuals should be similar.")
            ),
            p(
              tags$li("The Cook's Distance plot shows the values of leverage, 
                        standardized residuals, and Cook's Distance of each data point
                        which can be used to determine high leverage points, outliers 
                        and influential points.")
            )
          ),
          br(),
          div(
            style = "text-align: center;",
            bsButton(
                inputId = "start",
                label = "Explore!",
                icon = icon("bolt"),
                style = "default",
                size = "large")
          )
        ),
        #Challenge page ----
        tabItem(
          tabName = "explore",
          h2("Transformations, Sample Size, and Variances vs. Diagnostic Plots"),
          p("First, select a mystery model to analyze. Each model is generated 
          with Y as the response variable and X1 and X2 as the predictor variables. 
          Then, read the challenge question and adjust the sliders accordingly in 
          order to answer it. Once you have an answer, click the 'View Feedback' 
          button to see if you are correct. Once you complete a challenge, click 
          the 'New Challenge' button to receive a new challenge question. You can 
          also click the 'Results for a New Sample' button to analyze a different 
          sample of data from the mystery model selected. Once you have completed 
          your desired amount of challenges, click the 'Play' button to move onto 
          the tic-tac-toe game."),
          br(),
          fluidRow(
            column(
              width = 4,
              wellPanel(
                selectInput("model", 
                            "Select Mystery Model:", 
                            choices = c(
                              'Model 1', 
                              'Model 2', 
                              'Model 3'),
                            width = 350
                ),
                sliderInput(
                  inputId = "n",
                  label = "Sample Size:", 
                  min = 10,
                  max = 510, 
                  value = 50, 
                  step = 5),
                br(),
                br(),
                selectInput("x", 
                            "Transformation on X1:", 
                            choices = c(
                              'Log(x1)' = 'logx', 
                              'Square root of x1' = 'sqrtx',
                              "None" = 'nonex'), 
                            selected = "None",
                            width = 350
                ),    
                sliderInput(
                  inputId = "x1v", 
                  label = "x1 Variance:", 
                  min = 0, 
                  max = 20, 
                  value = 2, 
                  step = 1          
                ),
                br(),
                br(),
                selectInput(
                  inputId = "x2", 
                  label = "Transformation on X2:", 
                  choices = c(
                    'Log(x2)' = 'logx2', 
                    'Square root of x2' = 'sqrtx2', 
                    "None" = 'nonex2'),
                  selected = "None",
                  width = 350
                ), 
                sliderInput(
                  inputId = "x2v", 
                  label = "x2 Variance:", 
                  min = 0, 
                  max = 20, 
                  value = 2, 
                  step = 1          
                ),
                br(),
                br(),
                selectInput(
                  inputId = 'y', 
                  label = 'Transformation on Y:', 
                  choices = c(
                    'Log(y)' = 'logy',
                    'Square root of y' = 'sqrty',
                    "None" = 'noney'
                  ), 
                  selected = "Log(y)",
                  width = 350
                ),
                sliderInput(
                  inputId = "yv", 
                  label = "Y Variance:", 
                  min = 0, 
                  max = 20, 
                  value = 2, 
                  step = 1          
                ),
                bsButton(
                  inputId = "submitD", 
                  label = "Results for a New Sample", 
                  style = "default",
                  icon = icon("retweet"), 
                  size = "large"
                ),
                br(),
                br(),
                bsButton(
                  inputId = "begin",
                  label = "Play!",
                  icon = icon("bolt"),
                  style = "default",
                  size = "large")
              )
              ),
            column(
              width = 8,
              offset = 0,
              h3("Challenge:"),
              uiOutput("challenges"),
              plotOutput(
                "plots",
                width = "100%",
                height = "750px"),
              tags$script(HTML(
                "$(document).ready(function() {
                document.getElementById('plots').setAttribute('aria-label',
                `Collage of dynamic plots including Residuals vs. Fitted, Normal 
              Q-Q, Scale-Location, and Residuals vs. Leverage.`)
               })"
              )),
              br(),
              h3("Feedback:"),
              uiOutput("answers"),
              br(),
              bsButton(
                inputId = "answer", 
                label = "View Feedback", 
                style = "default",
                size = "large",
                disabled = FALSE),
              bsButton(
                inputId = "challenge", 
                label = "New Challenge", 
                style = "default",
                size = "large",
                disabled = FALSE
              )
            )
            ),
                ),

        # Game page ----
        tabItem(
          tabName = "qqq",
          withMathJax(),
          useShinyalert(),
          h2("Regression Tic-Tac-Toe"),
          p("To play, click on any one of the buttons that have a question
                  mark. A question will appear to the right with possible answers. 
                  If you answer correctly, you will take the square; if not, the 
                  computer will take the square. Try your best to win the game!"
          ),
          h3(
            uiOutput("player")
            ),
          fluidRow(
            div(
              class = "col-sm-12 col-md-4",
              h3("Game Board"),
              br(),
              uiOutput(
                "gameBoard", 
                class = "game-board")
            ),
            div(
              class = "col-sm-12 col-md-8",
              h3("Question"),
              withMathJax(
                uiOutput("question")),
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
                style = "danger"
              ),
              br(),
              #These two triggers help with MathJax re-rendering
              uiOutput("trigger1"),
              uiOutput("trigger2")
            )
          )
        ),
# Reference page ----
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
            "Attali, Dean and Edwards, Tristan. (2020). shinyalert: Easily Create 
            Pretty Popup Messages (Modals) in 'Shiny'. (v2.0.0) [R Package]. Available
            from https://CRAN.R-project.org/package=shinyalert"
          ),
          p(
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny. 
            (v0.61). [R package]. Available from https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Carey, R. (2019). boastUtils: BOAST Utilities. (v0.1.0). [R Package]. 
            Available from https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Chang, W. and Borges Ribeio, B. (2018). shinydashboard: Create dashboards 
            with 'Shiny'. (v0.7.1) [R Package]. 
            Available from https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J. (2019). 
            shiny: Web application framework for R. (v1.4.0) [R Package]. 
            Available from https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Hijmans, Robert J. (2021). raster: Geographic Data Analysis and Modeling. 
            (v3.4-10) [R Package]. Available from https://CRAN.R-project.org/package=raster"
          ),
          p(
            class = "hangingindent",
            "Perrier, V., Meyer, F., Granjon, D. (2019). shinyWidgets: Custom inputs 
            widgets for shiny. (v0.5.0) [R Package]. Available from 
            https://CRAN.R-project.org/package=shinyWidgets"
          ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  ))

# Server ----

server <- function(input, output, session) {
  
#Go buttons ---- 
  observeEvent(
    eventExpr = input$infoex,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        title = "Instructions:",
        text = " Move the sliders to see their effect on the diagnostic plots.",
        type = "info"
      )
  })
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        title = "Instructions:",
        text = "Click on desired square, answer the question, then hit submit and 
      go to next question.",
        type = "info"
      )
  })
  observeEvent(
    eventExpr = input$go, 
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "prereq")
    })
  observeEvent(
    eventExpr = input$start,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "explore")
    })
  observeEvent(
    eventExpr = input$begin,
    handlerExpr = {
      updateTabItems(
        session = session, 
        inputId = "pages", 
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
        if(input$model == "Model 1") {
          nonex <- rnorm(input$n,
                         mean = 1000,
                         sd = sqrt(input$x1v))
          nonex2 <- rnorm(input$n,
                          mean = 1000,
                          sd = sqrt(input$x2v))
          e <- rnorm(input$n,
                     mean = 0,
                     sd = .2**2)
          y <- (rnorm(input$n,
                      mean = 3,
                      sd = input$yv))**2
          
          logx <- log(nonex)
          logx2 <- log(nonex2)
          logy <- log(y)
    
          sqrtx <- sqrt(nonex)
          sqrtx2 <- sqrt(nonex2)
          sqrty <- sqrt(y)
        }
        else if (input$model == "Model 2") {
          nonex <- rnorm(input$n,
                         mean = 1000,
                         sd = sqrt(input$x1v))
          nonex2 <- rnorm(input$n,
                          mean = 1000,
                          sd = sqrt(input$x2v))
          e <- rnorm(input$n,
                     mean = 0,
                     sd = .2**2)
          y <- rnorm(input$n,
                     mean = 1000,
                     sd = input$yv)
          
          logx <- log(nonex)
          logx2 <-log(nonex2)
          logy <- log(y)
          
          sqrtx <- sqrt(nonex)
          sqrtx2 <- sqrt(nonex2)
          sqrty <- sqrt(y)
        }
        else {
          nonex <- rnorm(input$n,
                         mean = 1000,
                         sd = input$x1v)**2
          nonex2 <- rnorm(input$n,
                          mean = 1000,
                          sd = input$x2v)**2
          e <- rnorm(input$n,
                     mean = 0,
                     sd = .2**2)
          y <- (rnorm(input$n,
                      mean = 1000,
                      sd = input$yv))**2
          
          logx <- log(nonex)
          logx2 <- log(nonex2)
          logy <- log(y)
          
          sqrtx <- sqrt(nonex)
          sqrtx2 <- sqrt(nonex2)
          sqrty <- sqrt(y)
        }
        
        ###Work section ------

        if (input$x == 'none') {
          modelx = nonex
        }
        else if (input$x == 'logx') {
          modelx = logx
        }
        else {
          modelx = sqrtx
        }
        
        if (input$x2 == 'none') {
          modelx2 = nonex2
        }
        else if (input$x == 'logx2') {
          modelx2 = logx2
        }
        else {
          modelx2 = sqrtx2
        }
        
        if (input$y == 'none') {
          modely = noney
        }
        else if (input$x == 'logy') {
          modely = logy
        }
        else {
          modely = sqrty
        }
        
        par(
          mfrow = c(2,2)
        )
        
        model = lm(modely ~ modelx + modelx2 )
        plot(model)
        
        
            
        # 
        # for (i in c(input$x)) {
        #   for(j in c(input$x2)) {
        #     for( k in  c(input$y)) {
        #   if (any(i == "none") & any(j == "none") & any(k == "none")) {
        #     par(
        #       mfrow = c(2,2)
        #       )
        #     model = lm(y~nonex+nonex2+e)
        #     plot(model)
        #   }
        #   else if (any(i == "none") & any(j == "logx2") & any(k == "none")) {
        #     par(
        #       mfrow = c(2,2)
        #       )
        #     model2 = lm(y~nonex+logx2+e)
        #     plot(model2)
        #   }
        #   else if(any(i == "none") & any(j == "sqrtx2") & any(k == "none")) {
        #     par(
        #       mfrow = c(2,2)
        #       )
        #     model3 = lm(y~nonex+sqrtx2+e)
        #     plot(model3)
        #   }
        #   else if(any(i == "logx") & any(j == "none") & any(k == "none")) {
        #     par(
        #       mfrow = c(2,2)
        #       )
        #     model4 = lm(y~logx+nonex2+e)
        #     plot(model4)
        #   }
        #   else if(any(i == "logx") & any(j == "logx2") & any(k == "none")) {
        #     par(
        #       mfrow = c(2,2)
        #       )
        #     model5 = lm(y~logx+logx2+e)
        #     plot(model5)
        #   }
        #   else if(any(i == "logx") & any(j == "sqrtx2") & any(k == "none")) {
        #     par(
        #       mfrow = c(2,2)
        #       )
        #     model6 = lm(y~logx+sqrtx2+e)
        #     plot(model6)
        #   }
        #   else if(any(i == "sqrtx") & any(j == "none") & any(k == "none")) {
        #     par(
        #       mfrow = c(2,2)
        #       )
        #     model7 = lm(y~sqrtx+nonex2+e)
        #     plot(model7)
        #   }
        #   else if(any(i == "sqrtx") & any(j == "logx2") & any(k == "none")) {
        #     par(
        #       mfrow = c(2,2)
        #       )
        #     model8 = lm(y~sqrtx+logx2+e)
        #     plot(model8)
        #   }
        #   else if(any(i == "sqrtx") & any(j == "sqrtx2") & any(k == "none")) {
        #     par(
        #       mfrow = c(2,2)
        #       )
        #     model9 = lm(y~sqrtx+sqrtx2+e)
        #     plot(model9)
        #   }
        #   else if(any(i == "none") & any(j == "nonex2") & any(k == "logy")) {
        #     par(
        #       mfrow = c(2,2)
        #       )
        #     model10 = lm(logy~nonex+nonex2+e)
        #     plot(model10)
        #   }
        #   else if(any(i == "none") & any(j == "logx2") & any(k == "logy")) {
        #     par(
        #       mfrow = c(2,2)
        #       )
        #     model11 = lm(logy~nonex+logx2+e)
        #     plot(model11)
        #   }
        #   else if(any(i == "none") & any(j == "sqrtx2") & any(k == "logy")) {
        #     par(
        #       mfrow = c(2,2)
        #       )
        #     model12 = lm(logy~nonex+sqrtx2+e)
        #     plot(model12)
        #   }
        #   else if(any(i == "logx") & any(j == "nonex2") & any(k == "logy")) {
        #     par(
        #       mfrow = c(2,2)
        #       )
        #     model13 = lm(logy~logx+nonex2+e)
        #     plot(model13)
        #   }
        #   else if(any(i == "logx") & any(j == "logx2") & any(k == "logy")) {
        #     par(
        #       mfrow = c(2,2)
        #       )
        #     model14 = lm(logy~logx+logx2+e)
        #     plot(model14)
        #   }
        #   else if(any(i == "logx") & any(j == "sqrtx2") & any(k == "logy")) {
        #     par(
        #       mfrow = c(2,2)
        #       )
        #     model15 = lm(logy~logx+sqrtx2+e)
        #     plot(model15)
        #   }
        #   else if(any(i == "sqrtx") & any(j == "nonex2") & any(k == "logy")) {
        #     par(
        #       mfrow = c(2,2)
        #       )
        #     model16 = lm(logy~sqrtx+nonex2+e)
        #     plot(model16)
        #   }
        #   else if(any(i == "sqrtx") & any(j == "logx2") & any(k == "logy")) {
        #     par(
        #       mfrow = c(2,2)
        #       )
        #     model17 = lm(logy~sqrtx+logx2+e)
        #     plot(model17)
        #   }
        #   else if(any(i == "sqrtx") & any(j == "sqrtx2") & any(k== "logy")) {
        #     par(
        #       mfrow = c(2,2)
        #       )
        #     model18 = lm(logy~sqrtx+sqrtx2+e)
        #     plot(model18)
        #   }
        #   else if(any(i == "none") & any(j == "nonex2") & any(k == "sqrty")) {
        #     par(
        #       mfrow = c(2,2)
        #       )
        #     model19 = lm(sqrty~nonex+nonex2+e)
        #     plot(model19)
        #   }
        #   else if(any(i == "none") & any(j == "logx2") & any(k == "sqrty")) {
        #     par(
        #       mfrow = c(2,2)
        #       )
        #     model20 = lm(sqrty~nonex+logx2+e)
        #     plot(model20)
        #   }
        #   else if(any(i == "none") & any(j == "sqrtx2") & any(k == "sqrty")) {
        #     par(
        #       mfrow = c(2,2)
        #       )
        #     model21 = lm(sqrty~nonex+sqrtx2+e)
        #     plot(model21)
        #   }
        #   else if(any(i == "logx") & any(j == "none") & any(k == "sqrty")) {
        #     par(
        #       mfrow = c(2,2)
        #       )
        #     model22 = lm(sqrty~logx+nonex2+e)
        #     plot(model22)
        #   }
        #   else if(any(i == "logx") & any(j == "logx2") & any(k== "sqrty")) {
        #     par(
        #       mfrow = c(2,2)
        #       )
        #     model23 = lm(sqrty~logx+logx2+e)
        #     plot(model23)
        #   }
        #   else if(any(i == "logx") & any(j == "sqrtx2") & any(k == "sqrty")) {
        #     par(
        #       mfrow = c(2,2)
        #       )
        #     model24 = lm(sqrty~logx+sqrtx2+e)
        #     plot(model24)
        #   }
        #   else if(any(i == "sqrtx") & any(j == "none") & any(k== "sqrty")) {
        #     par(
        #       mfrow = c(2,2)
        #       )
        #     model25 = lm(sqrty~sqrtx+nonex2+e)
        #     plot(model25)
        #   }
        #   else if(any(i == "sqrtx") & any(j == "logx2") & any(k == "sqrty")) {
        #     par(
        #       mfrow = c(2,2)
        #       )
        #     model26 = lm(sqrty~sqrtx+logx2+e)
        #     plot(model26)
        #   }
        #   else {
        #     par(
        #       mfrow = c(2,2)
        #       )
        #     model27 = lm(sqrty~sqrtx+sqrtx2+e)
        #     plot(model27)
        #   }
        #     }
        #   }
        #   }
      })
    
    #end of observeeventsubmit     
  })
  
  index <- reactiveValues(index = 7)
  
  #Outputting a new activity ----
  
  observeEvent(input$challenge | input$go, {
    if(input$model == "Model 1") {
      index$index <- sample(
        x = c(1:7,18:20),
        size = 1, 
        replace = FALSE, 
        prob = NULL
      )
    }
    else if(input$model == "Model 2") {
      index$index <- sample(
        x = c(8:13,18:20),
        size = 1, 
        replace = FALSE, 
        prob = NULL
      )
    }
    else {
      index$index <- sample(
        x = 14:20,
        size = 1, 
        replace = FALSE, 
        prob = NULL
      )
    }
    
    output$challenges <- renderUI ({
      h4(bankc[index$index, 2])
    }
    )
  })
  
  #Output for answer box when nothing is in the box ----
  observeEvent(
    eventExpr = input$challenge,
    handlerExpr = {
      output$answers <- 
        renderText("Please hit the view feedback button for feedback")
    }
    ) 
  observeEvent(
    eventExpr = input$go,
    handlerExpr = {
      output$answers <- 
        renderText("Please hit the view feedback button for feedback")
    }
    )  
  observeEvent(
    eventExpr = input$start,
    handlerExpr = {
      output$answers <- 
        renderText("Please hit the view feedback button for feedback")
    }
    ) 

  #output of the answers ----
  observeEvent(
    eventExpr = input$answer,
    handlerExpr = {
      output$answers <- renderUI ({
        p(bankc[index$index, 3])
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
      data = rep.int(0, 
                     times = TILE_COUNT),
      nrow = GRID_SIZE,
      ncol = GRID_SIZE
    )
  gameProgress <- FALSE
  
  # Helper Functions
  .tileCoordinates <- function(tile = NULL, 
                               index = NULL) {
    row <- -1
    col <- -1
    # if: button tile is given, derive from id
    # else: derive from index
    if (!is.null(tile)) {
      # grid-[row]-[col]
      tile <- strsplit(x = tile, 
                       split = "-")[[1]]
      tile <- tile[-1] # remove oxo
      row <- strtoi(
        x = tile[1])
      col <- strtoi(
        x = tile[2])
    } 
    else {
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
    id <- paste0(
      "grid-", 
      coords$row, 
      sep = "-", 
      coords$col)
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
      rotated <- apply(X = t(mat), 
                       MARGIN = 2,
                       FUN = rev)
      offD <- sum(diag(rotated))
      if (GRID_SIZE %in% rows ||
          GRID_SIZE %in% cols ||
          mainD == GRID_SIZE || offD == GRID_SIZE) {
        return("win")
      } 
      else if (-GRID_SIZE %in% rows ||
                 -GRID_SIZE %in% cols == 1 ||
                 mainD == -GRID_SIZE || offD == -GRID_SIZE) {
        return("lose")
      } 
      else if (any(mat == 0)) {
        return("continue")
      } 
      else {
        return("draw")
      }
    } 
    else {
      ifelse(
        test = rows == 1 && rows != 0, 
        yes = return("win"), 
        no = return("lose")
      )
     }
  }
  
  .boardBtn <- function(tile) {
    index <- .tileIndex(tile)
    activeQuestion <<- gameSet[index, "id"]
    output$question <- renderUI({
      withMathJax()
      return(gameSet[index, "question"])
    }
    )
    output$answer <- .ansFunc(index, gameSet)
    
    if (gameSet[index, "extraOutput"] != "") {
      output$extraOutput <- renderText({
              gameSet[index, "extraOutput"]
      })
    } 
    else {
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
    updateButton(
      session = session,
      inputId = "submit",
      disabled = FALSE)
  }
  
  .ansFunc <- function(index, df) {
    if (df[index, "format"] == "numeric") {
      renderUI({
        numericInput(
          inputId = "ans",
          label = df[index, "label"],
          value = 0)
      })
    } 
    else if (df[index, "format"] == "two") {
      renderUI({
        radioGroupButtons(
          inputId = "ans",
          choices = list(df[index, "A"],
                         df[index, "B"]
                         ),
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
    } 
    else if (df[index, "format"] == "three") {
      renderUI({
        radioGroupButtons(
          inputId = "ans",
          choices = list(df[index, "A"],
                         df[index, "B"],
                         df[index, "C"]
                         ),
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
    else {
      renderUI({
        radioGroupButtons(
          inputId = "ans",
          choices = list(df[index, "A"],
                         df[index, "B"],
                         df[index, "C"],
                         df[index, "D"]
                         ),
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
    lapply(
      X = 1:TILE_COUNT, 
      FUN = .btnReset)
    qSelected <<-
      sample(
        seq_len(nrow(questionBank)), 
        size = TILE_COUNT, 
        replace = FALSE)
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
        data = rep.int(0, 
                       times = TILE_COUNT),
        nrow = GRID_SIZE,
        ncol = GRID_SIZE
      )
    gameProgress <- FALSE
    activeBtn <- NA
    
    updateButton(
      session = session,
      inputId = "submit",
      disabled = TRUE)
  }
 
  ## BEGIN App Specific xAPI Wrappers ----
  .generateStatement <- function(session, 
                                 verb = NA, 
                                 object = NA, 
                                 description = NA) {
    if(is.na(object)){
      object <- paste0("#shiny-tab-", 
                       session$input$pages)
    }
    
    stmt <- boastUtils::generateStatement(
      session,
      verb = verb,
      object = object,
      description = description
    )
    
    response <- boastUtils::storeStatement(
      session = session, 
      statement = stmt)
    return(response)
  }
  
  .generateAnsweredStatement <- function(
    session, 
    verb = NA, 
    object = NA, 
    description = NA, 
    interactionType = NA, 
    response = NA, 
    success = NA, 
    completion = FALSE) {
    stmt <- boastUtils::generateStatement(
      session,
      verb = verb,
      object = object,
      description = paste0(
        "Question ", 
        activeQuestion, 
        sep = ": ", 
        description),
      interactionType = interactionType,
      success = success,
      response = response,
      completion = completion,
      extensions = list(
        ref = "https://educationshinyappteam.github.io/BOAST/xapi/result/extensions/scoreMatrix",
        value = paste(as.data.frame(scoreMatrix), 
                      collapse = ", ")
      )
    )
    
    response <- boastUtils::storeStatement(
      session = session, 
      statement = stmt)
    return(response)
  }
  ## END App Specific xAPI Wrappers ----
  
  # Define navigation buttons
  observeEvent(
    eventExpr = input$go1, 
    handlerExpr = {
    updateTabItems(
      session = session, 
      inputId = "pages", 
      selected = "qqq")
  })
  
  # Read in data and generate the first subset ----
  qSelected <-
    sample(
      x = seq_len(nrow(questionBank)), 
      size = TILE_COUNT, 
      replace = FALSE)
  gameSet <- questionBank[qSelected,]
  
  # Program the Reset Button
  observeEvent(
    eventExpr = input$reset, 
    handlerExpr = {
      .generateStatement(session, 
                         object = "reset", 
                         verb = "interacted", 
                         description = "Game board has been reset.")
      .gameReset() 
  })
  
  # Render Game Board / Attach Observers
  output$gameBoard <- renderUI({
    board <- list()
    index <- 1
    sapply(
      X = 1:GRID_SIZE, 
      FUN = function(row) {
        sapply(X = 1:GRID_SIZE, 
               FUN = function(column) {
                 id <- paste0("grid-", 
                              row, 
                              sep = "-", 
                              column)
                 board[[index]] <<- tags$li(
                   actionButton(
                     inputId = paste0("grid-", 
                                      row, 
                                      sep = "-", 
                                      column),
                     label = "?",
                     color = "primary",
                     style = "bordered",
                     class = "grid-fill"
                   ),
                   class = "grid-tile"
                 )
       observeEvent(
         eventExpr = session$input[[id]], 
         handlerExpr = {
           activeBtn <<- id
           .boardBtn(id)
           .generateStatement(session, 
                              object = activeBtn, 
                              verb = "interacted", 
                              description = paste0("Tile ", 
                                                   activeBtn, 
                                                   " selected. Rendering 
                                                               question: ", 
                                                   activeQuestion, 
                                                   ".")
           )
           output$mark <- renderUI(NULL)
           output$feedback <- renderUI(NULL)
         }
         )
        index <<- index + 1
      })
    })
    
    tags$ol(
      board,
      class = paste(
        "grid-board",
        "grid-fill",
        paste0("grid-", 
               GRID_SIZE, 
               "x", 
               GRID_SIZE)
      )) 
   })
  
  # Program Submit Button ----
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
      
      if (is.null(success) || length(success) == 0) {
        sendSweetAlert(
          session = session,
          title = "Error",
          text = "Please select an answer before pressing Submit.",
          type = "error"
        )
      } 
      else if (success) {
        updateButton(
          session = session,
          inputId = activeBtn,
          label = player,
          disabled = TRUE
        )
        scoreMatrix <<- .score(scoreMatrix, activeBtn, 1)
        
        output$mark <- renderIcon(
          icon = "correct",
          width = 50
        )
        output$feedback <- renderUI(
          paste("Your answer is correct!")
        )
      } 
      else {
        updateButton(
          session = session,
          inputId = activeBtn,
          label = opponent,
          disabled = TRUE
        )
        scoreMatrix <<- .score(scoreMatrix, 
                               activeBtn,
                               -1)
        output$mark <- renderIcon(
          icon = "incorrect",
          width = 50
        )
        output$feedback <- renderUI(
          paste("Your answer is incorrect. The correct answer is", 
                answer, 
                ".")
        )
      }
      
      # Check for game over states
      .gameState <- .gameCheck(scoreMatrix)
      completion <- ifelse(
        test = .gameState == "continue", 
        yes = FALSE, 
        no = TRUE)
      interactionType <- ifelse(
        test = gameSet[index,]$format == "numeric", 
        yes = "numeric", 
        no = "choice")
      
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
        .generateStatement(
          session, 
          object = "qqq", 
          verb = "completed", 
          description = "Player has won the game.")
        confirmSweetAlert(
          session = session,
          inputId = "endGame",
          title = "You Win!",
          text = "You've filled either a row, a column, or a main 
                     diagonal. Start over and play a new game.",
          btn_labels = "Start Over"
        )
        output$mark <- renderUI(NULL)
        output$feedback <- renderUI(NULL)
      } 
      else if (.gameState == "lose") {
        .generateStatement(
          session, 
          object = "qqq", 
          verb = "completed", 
          description = "Player has lost the game.")                 
        confirmSweetAlert(
          session = session,
          inputId = "endGame",
          title = "You lose :(",
          text = "Take a moment to review the concepts and then try 
                     again.",
          btn_labels = "Start Over"
        )
        output$mark <- renderUI(NULL)
        output$feedback <- renderUI(NULL)
      } 
      else if (.gameState == "draw") {
        .generateStatement(
          session, 
          object = "game", 
          verb = "completed", 
          description = "Game has ended in a draw.")
        confirmSweetAlert(
          session = session,
          inputId = "endGame",
          title = "Draw!",
          text = "Take a moment to review the concepts and then try 
                     again.",
          btn_labels = "Start Over"
        )
        output$mark <- renderUI(NULL)
        output$feedback <- renderUI(NULL)
      }
      if (is.null(success) || length(success) == 0) {
        updateButton(
          session = session,
          inputId = "submit",
          disabled = FALSE
        )
      } 
      else{
        updateButton(
          session = session,
          inputId = "submit",
          disabled = TRUE
        )
      }
      disabled = TRUE
    })
                 
  
  observeEvent(
    eventExpr = input$pages, 
    handlerExpr = {
      if (input$pages == "qqq") {
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
      .generateStatement(session, 
                         verb = "experienced", 
                         description = paste0("Navigated to ", 
                                              input$pages, 
                                              " tab.")
      )
    }, 
    ignoreInit = TRUE)
                 
  observeEvent(
    eventExpr = input$endGame, 
    handlerExpr = {
      .generateStatement(session, 
                         object = "endGame", 
                         verb = "interacted", 
                         description = paste("Game has been reset.")
      )
      .gameReset()
    })
            
  observeEvent(
    eventExpr = input$shinyalert, 
    handlerExpr = {
      if (input$shinyalert == TRUE) {
        player <<- "X"
        opponent <<- "O"
      }
      if (input$shinyalert == FALSE) {
        player <<- "O"
        opponent <<- "X"
      }
      .generateStatement(session, 
                         object = "shinyalert", 
                         verb = "interacted", 
                         description = paste0("User has selected player: ", 
                                              player)
      )
      
      output$player <- renderUI({
        return(paste0("You are playing as ", 
                      player, 
                      "."))
      })
    })
}
  boastUtils::boastApp(ui = ui, server = server)
  
  # as.name(input$y) 
  