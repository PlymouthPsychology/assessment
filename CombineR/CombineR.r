library(shiny)
library(tidyverse)
library(rio)

WeightTable<-NULL
NewData<-NULL
Final<-NULL
CheckData<-NULL


#### SHINY ----

ui<-fluidPage(
  titlePanel("CombineR - produce a combined mark from several questions"),
  sidebarLayout(
    sidebarPanel(
       htmlOutput("info1"),
       fileInput("file","Upload multiple Gradebook files containing question marks", multiple = T), 
      textOutput("overview"),
      htmlOutput("info2"),
      fileInput("final","Upload single DLE Gradebook file to return combined marks"),
      textOutput("summary")
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
        f$file <- input$file[[i, 'name']]  # add filename to dataframe as variable
        f<-f %>% mutate(Marker=ifelse(is.na(Marker),f,Marker)) # if no marker named use filename 
        csv[[i]] = f
        
      }
      # Merging the data files using rbind
      # assumption that all files have same columns
      
    do.call(rbind, csv) # rbind the datasets
      
    }
  })
  
  
  output$info1<-renderUI(HTML("Upload all the Gradebook files at once.<br/>(You must have revealed identities first).<br/>It is a good idea to rename them first to e.g., Q1, Q2, Q3 etc to make the output more readable.<br/>"))
  
  output$info2<-renderUI(HTML("<br/><br/>Upload the Gradebook file to return the combined Grade in.<br/>(You must have revealed identities first).<br/><br/>"))
  
  
  output$overview<-renderText({
     if(is.null(input$file))
        return()
     else 
     {
      d<-datamerge()
      n<-unique(d$`Email address`)
      paste0(length(n)," unique Students found in Gradebook files.")
     }
        
  })
  
 
  
  output$summary<-renderText({
     if(is.null(input$final)){return()}
     else {
 
        Final<<-import(input$final$datapath)
         paste0("There are ",nrow(Final)," students in the file.")
     }
  })
  
  ## Display the merged data
  output$newdata <- renderTable({
    if(is.null(CheckData)){return()}
     else
    {return(CheckData)}
    
  })
  
  output$weightTable<-renderTable({
    f<-tibble(file=input$file$name)
    d<-datamerge()
    n<-d %>% filter(!is.na(Grade)) %>% 
      group_by(file) %>% 
      summarise(students=n(), M=mean(Grade, na.rm=T), SD=sd(Grade,na.rm=T))
    f<-left_join(f,n)
    
    
    if(is.null(input$weightValues))
      return(f)
    else 
    {
    weights<-unlist(strsplit(input$weightValues,split=","))
   
    if(length(weights)==nrow(f)){
    
    WeightTable<<-cbind(f,weights) %>% 
       mutate(weights=as.numeric(weights))
    n<-left_join(d,WeightTable) %>% 
       select(`Email address`,Grade,weights) %>% 
       mutate(wGrade=Grade*weights/100) %>% 
       filter(!is.na(wGrade)) %>% 
       group_by(`Email address`) %>% 
       summarise(newGrade=sum(wGrade))
    NewData<<-left_join(Final,n) %>% 
      mutate(Grade=newGrade) %>% 
      select(-newGrade) %>% 
      filter(!is.na(Grade))
    m<-d %>% filter(!is.na(Grade)) %>%
      select(`Email address`,Marker) %>% 
      rename(newMarker=Marker)
    NewData<<-left_join(NewData,m) %>% 
      mutate(Marker=newMarker) %>% 
      select(-newMarker)
    
    
    
    CheckData<<-d %>% 
       select(`Email address`,Grade,file) %>% 
       pivot_wider(names_from = file, values_from = Grade) %>% 
       left_join(.,NewData %>% select(`Email address`,Grade))
      
    return(WeightTable)
    }
    else{return(f)}
    }
  })
  
  
  ## DownloadHandler to download the merged dataset
  output$download <- downloadHandler("merged.csv",

    

    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {

      # Write to a file 
      write.csv(NewData, file, row.names = FALSE)
    }
  )
  
  ## DownloadHandler to download the check data
  output$check <- downloadHandler("CheckData.csv",
                                     
                                     
                                     
                                     # This function should write data to a file given to it by
                                     # the argument 'file'.
                                     content = function(file) {
                                        
                                        # Write to a file 
                                        write.csv(CheckData, file, row.names = FALSE)
                                     }
  )
  
  output$report <- downloadHandler(
     filename = "Mark Combining Report.html",
     content = function(file) {
        tempReport <- file.path(tempdir(), "CombiningMarks.Rmd")
        file.copy("CombiningMarks.Rmd", tempReport, overwrite = TRUE)
        
        params <- list(
           Data = CheckData
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
    if(is.null(input$file) ) {return()}
    else
      tabsetPanel(
        tabPanel("Weights",
                 textInput("weightValues","Weight (0 to 100) for each file",value=""),
                 tableOutput("weightTable")),
        tabPanel("Merged Dataset", 
                 downloadButton("download", "Download File to upload to DLE" ), 
                 downloadButton("check", "Download File with check data" ), 
                 downloadButton("report", "Download Report" ), 
                 tableOutput("newdata")
                 )
        )
  })
}


## Run the application 
shinyApp(ui = ui, server = server)
