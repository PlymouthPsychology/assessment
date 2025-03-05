library(shiny)
library(tidyverse)
library(rio)

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

# create a theme for the ui
westworld = bs_theme(
   bootswatch = "darkly",
   primary = "#0E7CF1",
   success = "#4CF4F4",
   secondary = "#0B7F7F",
   "table-color" = "#4CF4F4",
   base_font = font_google("Encode Sans Condensed"),
   font_weight_base = "font-weight-light",
   headings_font_weight = 300,
   font_scale = .9
)

# help text if sidebar hidden
infostring <- "<----Please upload the files listed in the sidebar"

# html for making text very small
vsmall <- "<small><small><small>"
vsmalloff <- "</small></small></small>"

# don't know what modcode is yet
modcode<-"Module"

# initialise global variables
WeightTable<<-NULL
NewData<<-NULL
CheckData<<-NULL


#### SHINY ----

# Define UI for application using bslib for styling
ui <- page_sidebar(
   # this theme mimics Westworld tablets to a limited extent
   theme=westworld,
   
   # Application title including logo
   title = HTML('<img src="neglogo.png" width="auto" height="80"> <h2>Combine a students assignments into a single grade</h2>'),
 
  # The sidebar with file entry boxes            
 sidebar=sidebar(
     class="bg-secondary",  # colour it
     width = "33%",
     
      htmlOutput("info1"),
       fileInput("file","Upload multiple Gradebook files containing question marks", multiple = T), 
      textOutput("overview"),
      htmlOutput("info2"),
      fileInput("final","Upload single DLE Gradebook file to return combined marks"),
      textOutput("summary")
    ),
 
 
 # use layout for output
 layout_columns(
    card(
       card_header("Weights for assignments", class = "text-success"),
       htmlOutput("bestNinfo"),
       textInput("bestN","How many marks to use?",value="1"),
       textInput("weightValues","Weight (0 to 100) for each file",value="")
    ),
    
    card(
       card_header("Summary", class = "text-success"),
       
       tableOutput("weightTable"),
       downloadButton("download", "Download File to upload to DLE" ), 
       downloadButton("check", "Download File with check data" ), 
       downloadButton("report", "Download Report" )
       
    ),
    
    

    
    # two  panes 
    col_widths = c(4,8)  # window is twelve equal units wide
    # each row has 12 notional units
    #row_heights = c(3, 1)    # these are just ratios
 )
    
)  # end ui



server<-function(input,output,session) {
  
observe({  # update weights to 100 per file added
   if(is.null(input$file)){NULL}
   else{
   n<-nrow(input$file)
   updateTextInput(inputId="weightValues",value=paste(c(rep("100",n)),collapse=","))
   }
})
   
   
  # read in all the files dropped into the input$file box
  datamerge <- reactive({
    
    if(is.null(input$file))
      return()  # if not files, do nothing
    else 
    {
      nfiles = nrow(input$file) 
      csv = list()  # initialise
      for (i in 1 : nfiles)
      {
        f = import(input$file[[i, 'datapath']])
        f$file <- input$file[[i, 'name']]  # add filename to dataframe as variable
        f<-f %>% mutate(Marker=ifelse(is.na(Marker),f$file,Marker)) # if no marker named use filename 
        csv[[i]] = f  # add the dataframe to the list
        
      }

      # Merging the data files using rbind
      # assumption that all files have same columns
      #
      
    do.call(rbind, csv) # rbind the datasets for return
      
    }
  })
  
 final<-reactive({
     if(is.null(input$final))
     {return()}
     else
     {
        final<-import(input$final$datapath)
     }
  })
  
# info text to appear above the data entry fields
    
  output$info1<-renderUI(HTML(paste0(
     vsmall,
     "Upload all the Gradebook files at once.<br/>(You must have revealed identities first).<br/>It is a good idea to rename them first to e.g., Q1, Q2, Q3 etc to make the output more readable.<br/>",
     vsmalloff)))
  
  output$info2<-renderUI(HTML(paste0(
     vsmall,
     "<br/><br/>Upload the Gradebook file to return the combined Grade in.<br/>(You must have revealed identities first).<br/><br/>",
     vsmalloff)))
  
  output$bestNinfo<-renderUI(HTML(paste0(
     vsmall,
     "<br/><br/>If a student submits more pieces of work than required, the best N can be included. Enter the N below. Leave at 1 if only one required.",
     vsmalloff)))
  
  # count N of students as feedback to user on files read
  output$overview<-renderText({
     if(is.null(input$file))
        return()  # if no file do nothing
     else 
     {
      d<-datamerge()  # read the data
      n<-unique(d$`Email address`)  # count the number fo different email addresses read
      paste0(length(n)," unique Students found in Gradebook files.")
     }
        
  })
  
 
  # count N of students in output file as feedback to user on file read
  output$summary<-renderText({
     if(is.null(input$final)){return()}  # if no file do nothing
     else {
         paste0("There are ",nrow(final())," students in the ouput file.")
     }
  })
  
  ## Display the merged data
  output$checkdataTable <- renderTable({
    # if(is.null(CheckData)){return()}
    #  else
    {return(CheckData)}
    
  })
  
  
  
  ## Debug
  output$datamerge <- renderTable ({
     return(datamerge())
  })  # show the data on screen
  
  # show a table for each input with N, M, SD
  output$weightTable <- renderTable({
     if (is.null(input$file) | is.null(input$final))
     {
        return()  # no files added yet
     }
     else
     {
     
     f <- tibble(file = input$file$name)  # names of the input files
     d <- datamerge()       # read the data
     n <- d %>% filter(!is.na(Grade)) %>%  # summarise data for each file
        group_by(file) %>%
        summarise(
           students = n(),
           M = mean(Grade, na.rm = T),
           SD = sd(Grade, na.rm = T)
        )
     f <- left_join(f, n)  # add summary onto filenames
     
     
     
     if (is.null(input$weightValues))
        {
        return(f)  # if no weights typed into box, return the table so far
     }
     else
     #
     {
        weights <- unlist(strsplit(input$weightValues, split = ","))  # read the weights entered, splitting at commas
        
        if (length(weights) == nrow(f)) {
           # when the right number have been entered....
           
           WeightTable <<- cbind(f, weights) %>% # turn WeightTable into a vector
              mutate(weights = as.numeric(weights))
           
           n <- left_join(d, WeightTable) %>%
              select(`Email address`, Grade, weights) %>%
              mutate(wGrade = Grade * weights / 100) %>%   # compute weighted grades
              filter(!is.na(wGrade)) %>%             # remove NAs
              group_by(`Email address`) %>%          # for each student
              arrange(desc(wGrade)) %>%              # sort wGrades from best to worst
              mutate(order = row_number()) %>%         # add 1..n
              filter(order <= input$bestN) %>%         # keep only the bestN rows
              summarise(newGrade = sum(wGrade))        # add them up
           
           
           NewData <<- left_join(final(), n) %>%    # add the bestN grades onto data
              mutate(Grade = newGrade) %>%        # replace Grade
              select(-newGrade) %>%
              filter(!is.na(Grade))
           
           m <- d %>% filter(!is.na(Grade)) %>%  # get columns ready to copy into output file
              select(`Email address`, Marker) %>%
              rename(newMarker = Marker)
           
           NewData <<- left_join(NewData, m) %>%   # copy into output file
              mutate(Marker = newMarker) %>%       # add the marker name
              select(-newMarker)
           
           
           # make a checkable file with all the grades found for each student
           CheckData <<- d %>%    # global variable
              select(`Email address`, Grade, file) %>%                     # find the data
              pivot_wider(names_from = file, values_from = Grade) %>%    # put a students grades in a row
              left_join(., NewData %>% select(`Email address`, Grade)) %>% # add the computed grade
              arrange(desc(Grade))                                      # best at the top to spot errors
           
           return(WeightTable)  # just give back what was typed in, all the action has been saved as globals
        }
        else{
           return(f)
        }   # if no weights entered, give the data without the weights
     }
     }
  })
  

  
  ## DownloadHandler to download the merged dataset
  output$download <- downloadHandler("merged.csv",
    content = function(file) {export(NewData, file)}
  )
  
  ## DownloadHandler to download the check data
  output$check <- downloadHandler("CheckData.csv",
                                  content = function(file) {export(CheckData, file)}
  )
  
  output$report <- downloadHandler(
     filename = "Mark Combining Report.docx",
     content = function(file) {
        tempReport <- file.path(tempdir(), "CombiningMarks.Rmd")
        file.copy("CombiningMarks.Rmd", tempReport, overwrite = TRUE)
        
        params <- list(
           Data = CheckData  # send the necessary data to the markdown file
        )
        # make a temporary copy of the .Rmd file and send it the paramter list
        rmarkdown::render(input = tempReport, 
                          output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
     }
  )
  
 
  
 }  # end server


## Run the application 
shinyApp(ui = ui, server = server)
