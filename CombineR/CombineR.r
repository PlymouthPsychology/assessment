#### A shiny app that will combine several gradebooks from different assignments
#### into a single mark that can be uploaded to a 'combined grade' assignment
#### Each question can be weighted differently

#### libraries ----

library(shiny)
library(tidyverse)
library(rio)

#### initialize ----
# NB these will be globals made by <<- so persist outside the function making them
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

  
  datamerge <- reactive({   # combine all of the files dropped into input$file
                            # adding filename and using filename if no Marker named
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
  
  
  output$overview<-renderText({  # count number of students
     if(is.null(input$file))
        return()
     else 
     {
      d<-datamerge()
      n<-unique(d$`Email address`)
      paste0(length(n)," unique Students found in Gradebook files.")
     }
        
  })
  
 
  
  output$summary<-renderText({   # report students in output
     if(is.null(input$final)){return()}
     else {
 
        Final<<-import(input$final$datapath)  # make gradebook for returned grade a Global
         paste0("There are ",nrow(Final)," students in the file.")
     }
  })
  
  ## Display the merged data
  output$newdata <- renderTable({
    if(is.null(CheckData)){return()}
     else
    {return(CheckData)}
    
  })
  
  output$weightTable<-renderTable({   # display question stats and ask for weights
    f<-tibble(file=input$file$name)   # a list of the files provided
    d<-datamerge()
    n<-d %>% filter(!is.na(Grade)) %>%  # stats for each question file
      group_by(file) %>% 
      summarise(students=n(), M=mean(Grade, na.rm=T), SD=sd(Grade,na.rm=T))
    f<-left_join(f,n)                 # add stats to the list of files 
    
    
    if(is.null(input$weightValues))  # get a comma separated list of values
      return(f)  # if nothing entered show file stats
    else 
    {
    weights<-unlist(strsplit(input$weightValues,split=","))
   
    if(length(weights)==nrow(f)){
    
    WeightTable<<-cbind(f,weights) %>%       # add the weights to the list of files
       mutate(weights=as.numeric(weights))   # NB no checking here so gigo possible 
                                             # NB this is a Global <<-

    n<-left_join(d,WeightTable) %>%          # add the weights onto every line in the input data       
       select(`Email address`,Grade,weights) %>% 
       mutate(wGrade=Grade*weights/100) %>%  # compute weighted Grade
       filter(!is.na(wGrade)) %>%            # remove and NAs
       group_by(`Email address`) %>%         # and add them up for each student
       summarise(newGrade=sum(wGrade))       # should now have just email and combined grade
       
       
    NewData<<-left_join(Final,n) %>%         # NB Global made by adding combined grade onto output gradebook
      mutate(Grade=newGrade) %>%             # replace Grade with combined grade 
      select(-newGrade) %>%                  # remove combined grade
      filter(!is.na(Grade))
      
      
    m<-d %>% filter(!is.na(Grade)) %>%       # unsure if this works as intended
      select(`Email address`,Marker) %>% 
      rename(newMarker=Marker)               # find marker for each student
    NewData<<-left_join(NewData,m) %>%       # add it to return file
      mutate(Marker=newMarker) %>%           # replace empty Marker
      select(-newMarker)                     # remove added column
     
    
    
    CheckData<<-d %>%                        # make a Global file with workings for checking       
       select(`Email address`,Grade,file) %>%                  # get all the grades for a student
       pivot_wider(names_from = file, values_from = Grade) %>% # onto one line
       left_join(.,NewData %>% select(`Email address`,Grade))  # and add the combined Grade
      
    return(WeightTable)  # display the file stats with weights
    }
    else{return(f)}      # or just the file stats
    }
  })
  
  
  ## DownloadHandler to download the merged dataset
  output$download <- downloadHandler("merged.csv",

    # This function should write NewData to a merged.csv
    content = function(file) {
      write.csv(NewData, file, row.names = FALSE)
    }
  )
  
  ## DownloadHandler to download the check data
  output$check <- downloadHandler("CheckData.csv",
                                     
       # This function should write CheckData to Checkdata.csv
       content = function(file) {
          write.csv(CheckData, file, row.names = FALSE)
       }
  )
  
  output$report <- downloadHandler(   # pass data to the Markdown file for output
     filename = "Mark Combining Report.html",
     content = function(file) {  # make a copy of the outputfile locally on shiny server
        tempReport <- file.path(tempdir(), "CombiningMarks.Rmd")
        file.copy("CombiningMarks.Rmd", tempReport, overwrite = TRUE)
        
        params <- list(
           Data = CheckData   # send the Checkdata over
        )
        
        rmarkdown::render(input = tempReport,   # combine the data with the markdown
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


#### Run the application ----
shinyApp(ui = ui, server = server)
