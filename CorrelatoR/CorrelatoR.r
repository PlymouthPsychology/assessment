library(shiny)
library(tidyverse)
library(rio)
library(psych)





#### SHINY ----

ui<-fluidPage(
  titlePanel("CorrelatoR - produce a correlation marix from several questions"),
  sidebarLayout(
    sidebarPanel(
       htmlOutput("info1"),
       fileInput("file","Upload multiple Gradebook files containing question marks", multiple = T), 
      textOutput("overview"),
    ),
    mainPanel(
      uiOutput("tb")
   )
    
  )
)


server<-function(input,output) {
  
## input$file is a data frame and contains the details around the name, 
  # size and temp location of the files uploaded

  
  datamerge <- reactive({
    
    if(is.null(input$file))
      return()
    else 
    {
      nfiles = nrow(input$file) 
      csv = list()
      for (i in 1 : nfiles)
      {
        f = import(input$file[[i, 'datapath']])
        f<-f %>% select(`Email address`,Grade)
        f$file <- input$file[[i, 'name']]  # add filename to dataframe as variable
        csv[[i]] = f
        
      }
      # Merging the data files using rbind
      # assumption that all files have same columns
      
    do.call(rbind, csv) # rbind the datasets
      
    }
  })
  
  
  output$info1<-renderUI(HTML("Upload Gradebook files from different assignments at once.<br/>(You must have revealed identities first).<br/>It is a good idea to rename them first to e.g., Q1, Q2, Q3 etc to make the output more readable.<br/>"))
  
   
  output$overview<-renderText({
     if(is.null(input$file))
        return()
     else 
     {
      d<-datamerge()
      n<-unique(d$`Email address`)
      f<-unique(d$file)
      paste0(length(n)," unique Students found in ",length(f)," Gradebook files.")
     }
        
  })
  
 
  
  
  
  
  output$matrix<-renderPlot({
    if(is.null(input$file))
      return()
    else 
    {
      d<-datamerge()
      
      pairs.data<-d %>% select(`Email address`,Grade,file) %>% 
        filter(Grade>0) %>% 
        pivot_wider(names_from=file, values_from=Grade) %>% 
        select(-`Email address`)
      
      pairs.panels(pairs.data,
                   smooth = TRUE,      # If TRUE, draws loess smooths
                   scale = FALSE,      # If TRUE, scales the correlation text font
                   density = TRUE,     # If TRUE, adds density plots and histograms
                   ellipses = TRUE,    # If TRUE, draws ellipses
                   method = "pearson", # Correlation method (also "spearman" or "kendall")
                   pch = 20,           # pch symbol
                   lm = TRUE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
                   cor = TRUE,         # If TRUE, reports correlations
                   jiggle = TRUE,     # If TRUE, data points are jittered
                   factor = 2,         # Jittering factor
                   hist.col = 4,       # Histograms color
                   stars = TRUE,       # If TRUE, adds significance level with stars
                   ci = TRUE)    
      
      
    }
    
  })
  
  
   
  output$report <- downloadHandler(
     filename = "Mark Correlation Report.html",
     content = function(file) {
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        
        params <- list(
           Data = datamerge()
        )
        
        rmarkdown::render(input = tempReport, 
                          output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
     }
  )
  
  

#### MainPanel tabset renderUI code ----
# the following renderUI is used to dynamically generate the tabsets when the files have been loaded. 
# Until the file is loaded, app will not show the tabset.
  output$tb <- renderUI({
    if(is.null(input$file)) {return()}
    else
      tabsetPanel(
         tabPanel("Correlations", 
                  plotOutput("matrix"),
                  downloadButton("report", "Download report")
                 )
        )
  })
}


## Run the application 
shinyApp(ui = ui, server = server)
