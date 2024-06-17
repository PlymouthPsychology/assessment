#
# This is a Shiny web application. 
# Jon May 2024 June





library(tidyverse)
library(rio)
library(numform)



#### Functions for moderation ----

testfile<-function(file){
   ifelse(is.null(file),"no",nrow(file))
}



#### SHINY ----

library(shiny)



ui <- fluidPage(
   sidebarLayout(
      sidebarPanel(
         htmlOutput("Intro"),
            fileInput("qfiles", "Choose the Gradebook Files to compare ", accept = ".csv", multiple = T),
         # htmlOutput("summary"),
         # htmlOutput("markInfo"),
         tableOutput("blockmeans")
                      
      ),
      mainPanel(
         
         # Output: Tabset  ----
         tabsetPanel(type = "tabs",
                     # tabPanel("Original", 
                     #          # textOutput("modcode"),
                     #          htmlOutput("summary")
                     #          ),
  
                     # tabPanel("Debug",
                     #          tableOutput("marks")
                     #          ),      
                     
                     tabPanel("Compare Marks",
                              
                              
                              numericInput("targetblock", "Enter Row ID of target Block",0),
                              
                              htmlOutput("comparisonText"),
                              tableOutput("comparisonTable")
                     ),
                     
                     tabPanel("Scatterplot",
                              plotOutput("scatter")      ),

                     tabPanel("Outputs",
                              # htmlOutput("output1"),
                                 radioButtons("anonymise", "Should student names be removed :",
                                           c("Yes",
                                             "No")),
                              downloadButton("checkable", "Download the checkable marks file"),
                              downloadButton("comparisonsfile", "Download the comparisons")

                     )
                  
                     )
         )
         
      )
   )


server <- function(input, output) {
 
    
    qfiles <- reactive({
       if(is.null(input$qfiles))
          return()
       else 
       {
          nfiles = nrow(input$qfiles) 
          csv = list()
          for (i in 1 : nfiles)
          {
             f = import(input$qfiles[[i, 'datapath']])
             f$file <- input$qfiles[[i, 'name']]  # add filename to dataframe as variable
             f$Block <- str_sub(f$file,8,15)
             
             
             if("Email address" %in% variable.names(f)){"TRUE"}else{f$`Email address`=""}
             # if("Marker" %in% variable.names(f)){TRUE}else{f$Marker="option""}
             # 
             if("Grade" %in% variable.names(f)){"TRUE"}else{f$Grade=""}
             
             f$option<-str_sub(f$file,27,-5) 
              #if(f$Marker==""){f$Marker=option}
             
             f<-f %>% select(file, Block, option, `Email address`, Grade) %>% filter(Grade>0) 
             
             csv[[i]] = f
             
             #keep needed columns
          }
          # Merging the data files using rbind

          data<-do.call(rbind, csv) # rbind the datasets
          
          # remove multiple submissions per assignment
          data<-data %>% group_by(`Email address`,Block)  %>%
             mutate(rowid=row_number(desc(Grade))) %>%
             filter(rowid==1) %>%
             ungroup() %>% 
             select(Block,option,`Email address`,Grade)

         return(data)
          
       }
    }) 
    

    
    # 
    # moderated<-reactive({
    #   if(is.null(input$scripts) | is.null(s4file))
    #     return()
    #   else{
    #  
    #     moderated<-left_join(allmarks(),details()) %>% 
    #       mutate(moderated=moderate(category,moderate,input$minGrade),
    #              percent=percent(moderated))
    #     # moderated should now have a row for each mark with columns :
    #     # file, SRN, Email, Grade, category, (from scripts, aas, s4file)
    #     # question, QName, QweightinSec, section, SweightinExam, moderate, (from details)
    #     # moderated, percent (computed here)
    #     
    #   } 
    # })
    
    
 
    nfiles<-reactive({   #  count number of files dropped into qfiles
         if(is.null(input$qfiles))
           return("no")
         else{
         q<-qfiles()
         return(length(unique(q$file)))     
          }
       })
    
    nstudents<-reactive({   #  count number of different students found in qfiles
       if(is.null(input$qfiles))
          return("no")
       else{
          q<-qfiles()
          return(length(unique(q$`Email address`)))     
       }
    })
    
    nmarks<-reactive({   #  count number of different students found in qfiles
       if(is.null(input$qfiles))
          return("no")
       else{
        
          return(nrow(qfiles()))  
       }
    })
   
    output$markInfo<-renderUI({
       if(is.null(input$qfiles))
          s<-"No files input"
       else{
          q<-qfiles()
          s<-paste0("M=",f_num(mean(q$Grade),2),"%, SD=",f_num(sd(q$Grade),2),"%." )  
       }
       HTML(paste("From these files, the grades have",s,"</br>"))
    })
    
    
    
    compare<-reactive({ #  find means on other modules for each option in the target block
      if(is.null(input$qfiles))
        return()
      else 
      {
       q<-qfiles()                     # read gradebooks
       blocks<-sort(unique(q$Block))   # blocks is the names of the assessments in the data eg 601A
       b<-input$targetblock            # index id of assessment to target
       
       
       if(b>0 & b<=length(blocks)){    # check valid index id
       target<-blocks[b]               # name of block to target
       
       #newblocks<-c(blocks[b],blocks[-b]) # make a new ordering of blocks with the target block first
       q$Block<-factor(q$Block, levels=c(blocks[b],blocks[-b])) # impose order on Blocks so target comes first
       
       subset2<-q %>% filter(Block==target)   # subset is just the target assessment
       options<-unique(subset2$option)        # options is the names of the options found in the target assessment
       
          means<-NULL
          
          for(i in 1:length(options)){                                # for each of the options in the target assessment
             
             subset3<-q %>% filter(option==options[i])                 # subset is just this option
             emails<-subset3$`Email address`                           # make a vector of the students who did this option
             
             subset4<- q %>% filter(`Email address` %in% emails)
             descriptives<-subset4 %>%                              # subset all of the data from those students
                group_by(Block) %>%                                   # and for each assessment
                summarise(Med=median(Grade),
                          M=f_num(mean(Grade),2),
                          SD=f_num(sd(Grade)),
                          N=n())                                  # find their stats
             
             if(length(emails)>=5){                                # only attempt regression if there are more than 4 students
               m<-lm(data=subset4, Grade ~ Block)          # for these students predict Grade in each assessment
               coeffs<-tibble(Block=c(blocks[b],blocks[-b]),
                              diff=f_num(m$coefficients,2),
                              p=f_num(summary(m)$coefficients[,4],3))   # get the diffs from target and p values
               }
               else{coeffs<-tibble(Block=c(blocks[b],blocks[-b]),
                                   diff=rep("--",length(blocks)),
                                   p=rep("--",length(blocks)) )
                                   }
               
               descriptives$option=options[i]                               # name the rows of the descriptives with this option
         
               
               descriptives=left_join(descriptives,coeffs)                  # add in the diffs and p values
               
             means<-rbind(means,descriptives)                               # append to the table we are building
             means<-means %>% select(option, Block, N, Med, M, SD, diff, p)      # sensible order for display
          
          }
          return(means)        # when all options done, return the table
       }
       else
       {return()           # if invalid index, do nothing
       }
      }
    })
    
   output$Intro<-renderUI(
      HTML("<h2>Marksheet comparison</h2>This app will combine students' marks from several DLE gradebooks.<br/> It will ask you for a target gradebook, group the students by marker, and compare those marks with the groups' marks in the other gradebooks.<br/><hr><br/>")
   )
 
  
   output$summary<-renderUI({
      if(is.null(input$qfiles))
         return()
      else 
      {
    HTML(paste0(
        "There are ",nstudents()," students with ",nmarks()," marks in the ",nfiles()," files provided.<br/>"
     ))}
   })
   
   output$comparisonText<-renderUI({
     if(is.null(input$qfiles))
       return()
     else 
     {
      q<-qfiles()
      blocks<-unique(q$Block)
      b<-input$targetblock
      if(b>0 & b<=length(blocks))
      {HTML(paste0("Comparison of ",blocks[b], " with other assignments.<br/>"))}
         else
     {HTML(paste0("Please choose a row id between 1 and ",length(blocks),"<br/>"))}
     } 
   })
   
   output$comparisonTable<-renderTable(compare())
   
  #  
   
   output$marks<-renderTable(qfiles())
   
   output$blockmeans<-renderTable({
      if(is.null(input$qfiles))
         return()
      else 
      {
      qfiles() %>% group_by(Block) %>% summarise(N=n(),M=mean(Grade),SD=sd(Grade)) %>% rowid_to_column()
      }
   })
   
   
  ## 
  # output$downloadR <- downloadHandler(     
  #    filename = "Exam Collation Report.html",
  #    content = function(file) {
  #       tempReport <- file.path(tempdir(), "report.Rmd")
  #       file.copy("report.Rmd", tempReport, overwrite = TRUE)
  #       
  #       params <- list(
  #          moderated = moderated(),
  #          categorical = categorical,
  #          srn.errors = srn.errors(),
  #          mark.errors=mark.errors(),
  #          nomark.errors=nomark.errors(),
  #          checkable=checkable(),
  #          minGrade=input$minGrade,
  #          anonymise=input$anonymise
  #       )
  #       
  #       rmarkdown::render(input = tempReport, 
  #                         output_file = file,
  #                         params = params,
  #                         envir = new.env(parent = globalenv())
  #       )
  #    }
  # )
  # 
  
   
  makecheckable<-reactive({
     q<-qfiles()                     # read gradebooks
     blocks<-sort(unique(q$Block))   # blocks is the names of the assessments in the data eg 601A
     b<-input$targetblock            # index id of assessment to target
     
     
     if(b>0 & b<=length(blocks)){    # check valid index id
        target<-blocks[b]               # name of block to target
        
        subset2<-q %>% filter(Block==target) %>% 
           select(Block, option, `Email address`) %>% 
           unite("option", Block:option) # subset is just the target assessment
        
        
        f<-qfiles()  %>% select(Block, `Email address`, Grade) %>% pivot_wider(names_from = "Block", values_from = "Grade")
        f<-left_join(f,subset2)
        
        
        
        return(f)
     }
     else
     {return()}
  })   
   
   
  output$checkable <- downloadHandler(
     filename = "Checkable.csv",
     content = function(file){
       
       if(is.null(input$qfiles))
         return()
       else 
       {
        
           f<-makecheckable()
          
           if(input$anonymise=="Yes"){f<-f %>% select(-`Email address`)}
           
           export(f, file)
         }
     } 
  )
       
     
  
 
  output$comparisonsfile <- downloadHandler(
     filename = "Comparisons.csv",
     content = function(file){
        f<-compare()
        export(f, file)
     }
  )
  # 
 
  output$scatter<- renderPlot({
     if(is.null(input$qfiles))
        return()
     else 
     {
        q<-makecheckable()
      
        xlabel<-colnames(q)[1+input$targetblock]
        colnames(q)[1+input$targetblock]="target"
        
    
        q<-q %>% pivot_longer(-c(`Email address`,option, target)) 
        
        q<-q %>% filter(!is.na(value)) %>% filter(!is.na(target))
        
        q %>% ggplot(aes(x=target, y=value, colour=option))+
           geom_jitter(width=.1, height=.1, size=.2)+
           geom_smooth(method="lm", se=F)+
           xlab(xlabel)+
           theme_minimal()+
           theme(legend.position = "bottom")+
           facet_grid( ~ name, #ncol=1,
                      #scales = "free_y",
                      labeller = label_value)
                      #strip.position = "left")
                      
        
     } 
  })
 
} # end server

# Run the application 
shinyApp(ui = ui, server = server)
