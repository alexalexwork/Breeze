library(shiny)
library(shinyjs)
library(googleAuthR)
library(googleID)
library(openxlsx)
library(RMySQL)
library(shinytoastr)
library(gtools)
library(plyr)

shinyServer(function(input, output, session) {
  
  shinyjs::runjs("$(\"[data-value='FIMM version']\").click(function(){ Shiny.onInputChange(\"onlog\",\"onlog\");})");
  
  annot <<- NULL; registered <<- 0;
  observe({
    # browser();
   # data=read.xlsx("emails.xlsx")
    # data2<<-read.xlsx("ready.xlsx")
    mydb = dbConnect(MySQL(),user='breezeuser',password='Breezeshinyatlas123!!!',dbname='breezedata', host='127.0.0.1')
    data <- dbGetQuery(mydb, "SELECT * FROM OurEmails")
    # data2 <<- dbGetQuery(mydb, "SELECT * FROM Controls")
    dbDisconnect(mydb)
    
    if(!is.null(googleAuthR:::authReturnCode(session))){
      app_url <- googleAuthR:::gar_shiny_getUrl(session)
      access_token <- googleAuthR:::gar_shiny_getToken(googleAuthR:::authReturnCode(session),
                                                       app_url)
      Authentication$set("public", "app_url", app_url,
                         overwrite = TRUE)
      Authentication$set("public", "shiny", TRUE, overwrite = TRUE)
      
      name = with_shiny(get_user_info, shiny_access_token = access_token)$email$value
      # browser();
      if(name %in% as.character(data$emails)){
        shinytoastr::toastr_success(paste0("Hi ", name, " You registered successfully"));  registered <<- !0;
      } else {
        shinytoastr::toastr_error("You have not been successfully registered");  registered <<- !1;
      }
    }
  })
  
  ## Authentication
  output$try1 <- renderUI({
    
    
    shiny::a("Login via Google", 
             href = googleAuthR:::gar_shiny_getAuthUrl(googleAuthR:::gar_shiny_getUrl(session), 
                                                       access_type = "online", approval_prompt = "auto"), 
             class = "btn btn-primary", role = "button")
  })
  
  
  
  observeEvent(input$onlog,{
    showModal(modalDialog(
      title = "App Name",
      windowTitle = "Browser window title",
      tabPanel("Tab 1",
               useShinyjs(),
               sidebarLayout(
                 sidebarPanel(
                   p("Welcome!"),
                   shiny::uiOutput("try1")
                 ),
                 mainPanel(
                   textOutput("display_username")
                 )
               )
      ),
      tabPanel("Tab 2",
               p("Layout for tab 2")
      )
      
    ))
    
    
    
    # data=read.xlsx("emails.xlsx")
    # mydb = dbConnect(MySQL(),user='breezeuser',password='Breezeshinyatlas123!!!',dbname='breezedata', host='127.0.0.1')
    # data <- RMySQL::dbSendQuery(mydb, "SELECT * FROM OurEmails;")
    # data = fetch(data, n=-1)
    # dbDisconnect(mydb)
    
  })
  
  
  
  
  
  
  observeEvent(input$getstarted, {
    if(registered == !0){
      mydb = dbConnect(MySQL(),user='breezeuser',password='Breezeshinyatlas123!!!',dbname='breezedata', host='127.0.0.1')
      # data <- dbGetQuery(mydb, "SELECT * FROM OurEmails")
      data2 <<- dbGetQuery(mydb, "SELECT * FROM Controls")
      dbDisconnect(mydb)
       # data2<<-read.xlsx("ready.xlsx");
      showModal(modalDialog(
        title = "DSRT",
        fluidRow(
          column(7, fileInput("fileinp", "Experimental info file (*.xlsx)", multiple = F, accept = NULL, width = NULL)),
          column(7, fileInput("fileinp2", "Raw data (*.zip)", multiple = F, accept = NULL, width = NULL)),
          column(5, selectInput("fileType", "DSRT set", choices = c("FO4B","FO5A","FO4A2","FO4A1","FA2A","FA1A","FA2B","FS1A","FS2A","FM1B","FM1A","FO3D","FO3C","FO3AB1","FO2AB1","FO2Baq","FO2AA1","FIMM_01B","FIMM_01E","GSK","FE1A","FE2A","FE3A","FA2C","FA2Cee","FECCA","COMBO"))),
          column(5, selectInput("fileType2", "Controls", choices = c("None","FO4B_CTG","FO4B_CTxG","FO5A_CTxG","FO5A_CTG")))
        ),
        #tags$p(tags$b("OR"), "download example data"),
        #downloadButton(outputId = "loadExData", label = "Example data", class = "butEx"), 
        easyClose = !0,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("start", "Start", class = "btn-success")
        )
      ))
      
      
    } else {
      
      showModal(modalDialog(
        title = "DSRT",
        fluidRow(
          column(7, fileInput("fileinp", "Experimental info file (*.xlsx)", multiple = F, accept = NULL, width = NULL)),
          column(5, selectInput("fileType777", "Choose readout:", choices = c("Inhibition","Viability"))),
          # column(7, fileInput("fileinp2", "Raw data (*.zip)", multiple = F, accept = NULL, width = NULL)),
          column(5, downloadButton(outputId = "loadExData", label = "Example data", class = "butEx")),
          column(7, fileInput("fileinp345", "  Controls (*.xlsx)", multiple = F, accept = NULL, width = NULL))
          # column(5, selectInput("fileType", "DSRT set", choices = c("FO4B","FO5A","FO4A2","FO4A1","FA2A","FA1A","FA2B","FS1A","FS2A","FM1B","FM1A","FO3D","FO3C","FO3AB1","FO2AB1","FO2Baq","FO2AA1","FIMM_01B","FIMM_01E","GSK","FE1A","FE2A","FE3A","FA2C","FA2Cee","FECCA","COMBO")))
        ),
        #tags$p(tags$b("OR"), "download example data"),
        #downloadButton(outputId = "loadExData", label = "Example data", class = "butEx"),
        easyClose = !0,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("start", "Start", class = "btn-success")
        )
      ))
    }
  })
  
  observeEvent(input$start, {
    
    showModal(modalDialog(
      title = "DECREASE2", size = "l", easyClose = !1,
      tags$form(action="./Results/HTMLreport/qc.html",method="get",target="_blank",
                actionButton("results_", "Check results", type="submit")),
      uiOutput("plots"),
      plotOutput(outputId = "distPlot")
    )); 
    disable("results_");
    # set dialog width
    shinyjs::runjs('var width_ = $(window).width(); $(".modal-lg").css("width", width_ = $(window).width()*.9+"px"); ');
    
    withProgress(message = 'Starting Quality Control checking',
                 detail = 'This may take a while...', value = 0, {
                   incProgress(1/3);
                   raw_data_path <<- input$fileinp2$datapath; experiment_info_path <<- input$fileinp$datapath; HTML_QC <<- !1;
                   if(registered == !0){ 
                   annoframe <<- openxlsx::read.xlsx(paste0("./datasets/FIMMOncologyAnnotations/Annotations_", isolate(input$fileType), "_2.xlsx"));
                   xpo<<-isolate(input$fileType2)
                   aaannoframe <<- data2[,c("ID","DRUG_NAME",xpo)]
                   }
                   else {
                     maika=input$fileinp345$datapath
                     pup=is.null(maika)
                     if(pup == !0){ 
                       # xpo <<- colnames(data2)[3];
                       # aaannoframe <<- data2[,c("ID","DRUG_NAME",xpo)]
                       xpo="None"
                       }
                     else {
                       data2 <<- openxlsx::read.xlsx(maika);
                     # data2 <<- openxlsx::read.xlsx(input$fileinp345$name);
                     xpo <<- colnames(data2)[3];
                     aaannoframe <<- data2[,c("ID","DRUG_NAME",xpo)]}
                   };
                   # xpo<<-isolate(input$fileType2)
                   # saveRDS(data2,"data2.rds")
                   # saveRDS(data,"data.rds")
                   # aaannoframe <<- data2[,c("ID","DRUG_NAME",xpo)]
                   # aaannoframe=fileinp345[]
                   drug_annot_tbl_full <<- openxlsx::read.xlsx("./datasets/Drug_annotations_table/FO5A annotations.xlsx");
                   DSS_typ <<- 2; HTMLreport <<- !1; 
                   sDSS <<- ifelse(xpo != "None" && !is.null(xpo), !0, !1);
  
                   #raw_data_path <<- input$fileinp2$name;
                   #browser();
                   # source aditional functions
                   source("QC_PDF_HTML.R")
                   
                   incProgress(1/3, message = "Fitting dose-response curves");
                   source("dss.R")
                   source("curve_Fitting.R")
                   
                   
                 
                   incProgress(1/3, message = "Finalizing results");
                   HTMLreport <<- !0; barplot_ <<- !0; headerwater <<- "headerwater.txt"; heatmap_ <<- !0; 
                   headerpath <<- "headerheatdss.txt"; tailerpath <<- "tailerheatdss.txt";
                   source("DSS_PDF_HTML.R");
                   
                   #file.copy("./Results", "./www", recursive=!0); 
                   zip(zipfile = "./www/results.zip", files = "./www/Results/");
                   
                   enable("results_");
                   
                 })
    
    # observeEvent(input$start, {
    #   cpoxpo3 <<- cpoxpo
    #   # cpoxpo3=read.xlsx(paste0(dirCur,"/www/Results/Curve_fits/", screen,"cpoxpo.xlsx"))
    #   mydb = dbConnect(MySQL(),user='breezeuser',password='Breezeshinyatlas123!!!',dbname='breezedata', host='127.0.0.1')
    #   # cpoxproab = smartbind(cpoxpro77,cpoxpo3)
    #   dbWriteTable(mydb, name='Screensssss', value=cpoxpo3,overwrite = F,append=T)
    #   # data69 <- dbGetQuery(mydb, "SELECT * FROM Screensss")
    #   dbDisconnect(mydb)
    #   # write.xlsx(cpoxpo3,"data69.xlsx")
    #   
    # })
    
    #output$distPlot <- renderPlot({
    #  
    #  x    <- 1:10
    #  
    #  hist(x, col = "#75AADB", border = "white",
    #       xlab = "Waiting time to next eruption (in mins)",
    #       main = "Histogram of waiting times")
    #  
    #})
    
  })
  
 
  
  
  observeEvent(input$fileinp, {
    
    # Check that data object exists and is data frame.
    if (!is.null(input$fileinp$name)) {
      if (tools::file_ext(input$fileinp$name) %in% c("xlsx", "txt", "csv")) {
        
        # source file load functions
        source("getData.R")
        
        tryCatch({
          
          if(!is.null(input$fileType)){
            if(input$fileinp == "Tabular" && tools::file_ext(input$fileinp$name) %in% c("txt", "csv", "xlsx"))
            {
              if (tools::file_ext(input$fileinp$name) == 'xlsx') annot <<- openxlsx::read.xlsx(input$fileinp$datapath)
              else if (tools::file_ext(input$fileinp$name) %in% c("txt", "csv")) annot <<- read.table(file = input$fileinp$datapath, header = T, sep=",", row.names = NULL, fill = T)
              
              # take care of NA's and empty rows/cols       
              annot <<- data.frame(lapply(annot, as.character), stringsAsFactors=F)
              annot <<- annot[!apply(is.na(annot) | annot == "", 1, all),] # rows with all NA
              annot <<- annot[,!apply(is.na(annot) | annot == "", 2, all)] # cols with all NA
              annot$Conc1 = as.numeric(as.character(annot$Conc1)); annot$Conc2 = as.numeric(as.character(annot$Conc2)); annot$Response = as.numeric(as.character(annot$Response));
            } else if (input$fileType == "Matrix" && tools::file_ext(input$fileinp$name) %in% c("txt", "csv", ".xlsx")) {
              
              if (ext == 'xlsx') annot <<- openxlsx::read.xlsx(input$annotfile$datapath, colNames = F)
              else if (ext %in% c("txt", "csv")) annot <<- read.table(file = input$annotfile$datapath, header = F, sep =",",  row.names = NULL, fill = T)
              
              # take care of NA's and empty rows/cols       
              annot <<- data.frame(lapply(annot, as.character), stringsAsFactors=F)
              annot <<- annot[!apply(is.na(annot) | annot == "", 1, all),] # rows with all NA
              annot <<- annot[,!apply(is.na(annot) | annot == "", 2, all)] # cols with all NA
              D1 = sum(grepl("Drug1:", annot[,1])); D2= sum(grepl("Drug2:", annot[,1])); ConcUn = sum(grepl("ConcUnit:", annot[,1]))
            }
            
          } else {
            # annot <<- openxlsx::read.xlsx(input$annotfile$datapath, colNames = F);
            annot <<- openxlsx::read.xlsx(input$fileinp$datapath);
            if(registered == !1){ 
              ProductUpper <<- toupper(annot[,"ProductName"]); 
              annot$Content <<- "sample"; annot$Content[ProductUpper %in% c("CELLS","EMPTY","") | is.na(ProductUpper)] <<- "cells";
              annot$Content[ProductUpper=="POS"] <<- "pos"; annot$Content[ProductUpper=="NEG"] <<- "neg"
            }
            else {};
          }
          
          
          enable("start"); 
        })
        
      } else {
        warning("wrong file format")
      }
    }
  })
  
  # Upload example data
  output$loadExData <- downloadHandler(
    filename = function(){ paste0("ExampleData.xlsx") },
    content = function(file){ file.copy("ExampleData.xlsx", file)},
    contentType = NULL
  )
  
})
