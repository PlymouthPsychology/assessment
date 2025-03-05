library(shiny)
library(tidyverse)
library(rio)
library(psych)

library(bslib)
library(bsicons) # icons.getbootstrap.com
library(thematic)

thematic_shiny()  # passes bslib theme settings through to ggplot

# extend theme_minimal with grey to show up on black background
theme_ww <- theme_minimal() +
   theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.text = element_text(colour = "grey90"),
      axis.title = element_text(colour = "grey90"),
      legend.title = element_text(colour = "grey90"),
      legend.text = element_text(colour = "grey90")
   )

# help text if sidebar hidden
infostring <- "<----Please upload the files listed in the sidebar"

# html for making text very small
vsmall <- "<small><small><small>"
vsmalloff <- "</small></small></small>"

# info text for  file entry box
infotext <- HTML(paste0(
   vsmall,
   "Upload Gradebook files from different assignments at once.<br/>(You must have revealed identities first).<br/>It is a good idea to rename them first to e.g., Q1, Q2, Q3 etc to make the output more readable.<br/>",
   vsmalloff
))


#### SHINY ----

# ui<-fluidPage(
#   titlePanel("CorrelatoR - produce a correlation marix from several questions"),
#   sidebarLayout(
#     sidebarPanel(
#        htmlOutput("info1"),
#        fileInput("file","Upload multiple Gradebook files containing question marks", multiple = T), 
#       textOutput("overview"),
#     ),
#     mainPanel(
#       uiOutput("tb")
#    )
#     
#   )
# )

ui<-page_sidebar(
   # this theme mimics Westworld tablets to a limited extent
   theme = bs_theme(
      bootswatch = "darkly",
      primary = "#77597F",
      success = "#4CF4F4",
      secondary = "#CB0F0F",
      "table-color" = "#4CF4F4",
      base_font = font_google("Encode Sans Condensed"),
      font_weight_base = "font-weight-light",
      headings_font_weight = 300,
      font_scale = .9
   ),
   
   
   # Application title
   title = HTML(
      paste0('<img src="neglogo.png" width="auto" height="80"> <h2>Correlations between grades</h2>')
   ),
   
   # Sidebar
   sidebar = sidebar(
      class = "bg-primary",
      width = "33%", 
      
      fileInput("file",infotext, multiple = T), 
      textOutput("overview")
   ),
   
   # use layout for output
   layout_columns(
      card(
         card_header("Correlations between uploaded grade files", class = "text-success"),
        
         plotOutput("matrix")
      ),
      
      
      card(
         card_header("Download report to a PDF file", class =
                        "text-success"),
         downloadButton("report", "Download report")
      ),
      
      # two full width panes 
      col_widths = c(12, 12), # each row has 12 notional units
      row_heights = c(4, 1)    # these are just ratios
   )
   
) # end ui


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
  
  
  # output$info1<-renderUI(HTML("Upload Gradebook files from different assignments at once.<br/>(You must have revealed identities first).<br/>It is a good idea to rename them first to e.g., Q1, Q2, Q3 etc to make the output more readable.<br/>"))
  
   
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
     filename = "Mark Correlation Report.pdf",
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
  # output$tb <- renderUI({
  #   if(is.null(input$file)) {return()}
  #   else
  #     tabsetPanel(
  #        tabPanel("Correlations", 
  #                 plotOutput("matrix"),
  #                 downloadButton("report", "Download report")
  #                )
  #       )
  # })
}


## Run the application 
shinyApp(ui = ui, server = server)
