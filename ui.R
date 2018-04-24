library(shiny, warn.conflicts = FALSE)
library(shinyjs, warn.conflicts = FALSE)
library(shinythemes)
library(googleAuthR)
library(googleID)
library(openxlsx)
library(DBI)
library(RMySQL)

options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/userinfo.email",
                                        "https://www.googleapis.com/auth/userinfo.profile"))
options("googleAuthR.webapp.client_id" = "291733423133-4uaa6fihbd35ols5q080pik6too8c9t0.apps.googleusercontent.com")
options("googleAuthR.webapp.client_secret" = "pjPbp1jxrOwp3qB1afnllXLN")


shinyUI(navbarPage("DSRT", collapsible = !0, 
                   theme = shinytheme("spacelab"), useShinyjs(), shinytoastr::useToastr(),
                   
                   tabPanel("FIMM version"),
                   tabPanel("Statistics"),
                   # tabPanel("login"),
                   
                   #tabPanel(actionButton("login","Log in", class = "getstartedbutton2 btn-primary btn-md", icon = icon("forward"))),
                   
                   
                   
                   tags$head(
                     HTML('<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/normalize/7.0.0/normalize.min.css" />'),
                     HTML('<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Lora" />'),
                     tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),tags$script(src = "plotsurface.js")
                     #tags$script(src = "plotlygl3d.js"), 
                   ),
                   fluidRow(tags$div(id="particles-js", style="z-index:-1000")),
                   
                   # Show a plot of the generated distribution
                   br(),
                   
                   fluidRow(
                     column(6, offset = 3, HTML("<img src = 'main.png' class = 'mainimg' ></img>"))#,
                     #column(2, HTML('<div id="htmlwidget38014a1c13a0881fc26b"></div>'))
                   ),  fluidRow(column(8, offset = 2, hr())),  
                   
                   fluidRow(column(8, offset = 2, HTML('<p class = "maintext"><b>DSRT</b></p> <p class = "maintext2">Drug Sensitivity and Resistance Testing pipeline. </p>')
                   )),  
                   
                   #fluidRow(column(2, offset = 5, actionButton("inpid","Get started",width = "100%", style = "color:white; background-color:green;"))),  br(),
                   
                   
                   
                   br(), br(), 
                   
                   fluidRow(
                     column(2, offset = 4, actionButton("getstarted","Get started", style = "width:50%; border-radius:15px;position:absolute;right:10px;padding:14px;border-color: #e9ecef;", class = "getstartedbutton2 btn-primary btn-md", icon = icon("forward"))),
                     column(2, offset = 0, actionButton("getstarted2","Learn more", style = "width:50%; border-radius:15px;position:absolute;left:10px;padding:14px;border-color: #e9ecef;);", class = "getstartedbutton2"))
                   ), 
                   
                   
                   tags$div(
                     HTML("<img src = 'footer.png' class = 'fixFoot' ></img>"),
                     #HTML("<img src = 'browsersupport.png' class = 'browserTop' ></img>"),
                     HTML('<script type="text/javascript" id = "feedbacksc"> window._urq=window._urq||[];_urq.push(["initSite","b3f010a9-182f-47c2-ac6f-7316a1e52b1d"]);(function(){var a=document.createElement("script");a.type="text/javascript";a.async=!0;a.src="https:"==document.location.protocol?"https://cdn.userreport.com/userreport.js":"http://cdn.userreport.com/userreport.js";var b=document.getElementsByTagName("script")[0];b.parentNode.insertBefore(a,b)})();</script>')
                   )
                   
                   
                   
                   #tags$script(src="particles.min.js"), tags$script(src="app.js")
                   
                   #HTML('<canvas class="background"></canvas>')
                   #actionButton("getstarted","Get strated", width = "130px", class = "getstartedbutton")
)
)
