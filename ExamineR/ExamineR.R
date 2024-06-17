#
# This is a Shiny web application. 
# Jon May 2023 June

# Upload S4 enrolment report for module
# Upload one or more CSV files containing SRN, percent
# Upload one or more AA gradebook files downloaded from DLE
# Upload one or more empty Gradebook files for each Question from DLE
# Upload one gradebook file for combined marks
# specify Questions each file corresponds to
# specify Sections each question corresponds to
# specify weight of each question in its section
# specify weights of each section in exam
# 
# 




library(tidyverse)
library(rio)
library(zip)



# convert letters or pc to integers and classes
categorical=data.frame(c(0, 15 , 25, 38, 42, 45, 48, 52, 55, 58, 62, 65, 68, 77, 88, 100),
                       c("N","F-","F","F+","D-","D","D+","C-","C","C+","B-","B","B+","A-","A","A+"),
                       0:15, 
                       c("Zero", "Fail", "Fail", "Fail",
                         "3rd", "3rd", "3rd", 
                         "2ii", "2ii", "2ii", 
                         "2i", "2i", "2i", 
                         "1st", "1st", "1st"),
                       c("Zero", "Fail", "Fail", "Fail",
                         "Fail", "Fail", "Fail", 
                         "Pass", "Pass", "Pass", 
                         "Merit", "Merit", "Merit", 
                         "Dist", "Dist", "Dist")
                       )
colnames(categorical)<-c("percent","letter","category", "Class", "MClass")

categorical$Class<-ordered(categorical$Class, levels=c("1st","2i","2ii","3rd","Fail","Zero"))

categorical$MClass<-ordered(categorical$MClass, levels=c("Dist","Merit","Pass","Fail","Zero"))



# given a letter grade or percentage return the linear category value 0-15 or N/A
categorise<-function(x){
   y<-case_when(
      !is.na(as.integer(x)) ~  categorical$category[match(as.integer(x),categorical$percent)],
      is.na(as.integer(x))  ~  categorical$category[match(x,categorical$letter)]
   )
   return(y)
}

# given a category, return a percent
percent<-function(x){
   y <- categorical$percent[match(x,categorical$category)]
   return(y)
}


# Report F(df1,df2)=xxx, p=.xxx
frep<-function(f.obj){
   paste0("F(", f.obj$Df[1],", ",f.obj$Df[2],")=",f_num(f.obj$`F value`[1],3),", ",f_pval(f.obj$`Pr(>F)`[1],.001,3))
}


#### Functions for moderation ----

testfile<-function(file){
   ifelse(is.null(file),"no",nrow(file))
}


moderate<-function(Category, Moderate, minGrade){
   # moderate the marks by adding the adjustment to the Category
   # Marks cannot go above 15, 
   # marks cannot be moved below minGrade (4 or D- for UG, 7 or C- for MSc)
   # marks below minGrade are unchanged
  minGrade=as.integer(minGrade)
  moderated<-ifelse(Category + Moderate >15, 15, 
                    ifelse(Category < minGrade, Category,
                          ifelse(Category + Moderate < minGrade, minGrade, Category + Moderate)
                             )
      )
}   



#### SHINY ----

library(shiny)

ui <- fluidPage(
   sidebarLayout(
      sidebarPanel(
         htmlOutput("Intro"),
         fileInput("scripts", "Choose the CSV Files with SRN, letters from Scripts ", accept = ".csv", multiple = T), 
         fileInput("aas", "Choose the Gradebook Files from AAs ", accept = ".csv", multiple = T), 
         fileInput("qfiles", "Choose the empty Gradebook Files to return Question marks ", accept = ".csv", multiple = T), 
         fileInput("efile", "Choose the empty Gradebook File to return combined exam mark ", accept = ".csv", multiple = F), 
         fileInput("s4file", "Choose the S4 enrolment report file ", accept = ".csv", multiple = F), 
         radioButtons("minGrade", "Select type of Module:",
                      c("Undergraduate" = 4,
                        "Masters" = 7))
      ),
      mainPanel(
         
         # Output: Tabset  ----
         tabsetPanel(type = "tabs",
                     tabPanel("Original", 
                              textOutput("modcode"),
                              htmlOutput("summary")
                              ),
  
                           
                     tabPanel("Details",
                           tableOutput("details.table"),
                         #  actionButton("reset", "Reset these details"),
                           downloadButton("downloadDetails","Download these details"),
                           fileInput("details.file", "Upload an existing details file", accept = ".csv", multiple = F) 
                           ),
                     
                     
                     tabPanel("Check SRNs",
                              htmlOutput("srnInfo"),
                              tableOutput("srn.errors")
                              ),
                     
                     tabPanel("Check Marks",
                              htmlOutput("markInfo"),
                              tableOutput("mark.errors")
                     ),
                     
                     tabPanel("No Marks",
                              htmlOutput("nomarkInfo"),
                              tableOutput("nomark.errors")
                     ),
                     tabPanel("Duplicated Marks",
                              htmlOutput("dupInfo"),
                              tableOutput("duplicate.errors")
                     ),
                     
                     tabPanel("Outputs",
                              htmlOutput("output1"),
                              downloadButton("downloadQ","Download the zipped Question files"),
                              downloadButton("downloadE", "Download the combined Exam file"),
                              htmlOutput("output2"),
                              radioButtons("anonymise", "Should student names be removed from the Module Leader files:",
                                           c("Yes",
                                             "No")),
                              downloadButton("checkable", "Download the checkable marks file"),
                              downloadButton("downloadR", "Download the Module Leader's Report")
                              
                     ),
                     
                     # uncomment the following for debugging
                     
                     # tabPanel("csv",tableOutput("csv.table")),  # debugging
                     # tabPanel("aa",tableOutput("aa.table")),    # debugging
                     # tabPanel("Q",tableOutput("q.table")),      # debugging
                     # tabPanel("E",tableOutput("e.table")),      # debugging
                     # tabPanel("MOD",tableOutput("mod.table")),  # debugging

                                            
                     tabPanel("check",
                              tableOutput("check.table"))   
                     
                     
                     
                          
                     )
         )
         
      )
   )


server <- function(input, output) {
  
    scripts <- reactive({
        if(is.null(input$scripts))
         return()
      else 
      {
         nfiles = nrow(input$scripts) 
         csv = list()
         for (i in 1 : nfiles)
         {
            f = import(input$scripts[[i, 'datapath']])
            colnames(f)<-c("SRN","Grade")
            f <- f %>% select(SRN,Grade)
            f$file <- input$scripts[[i, 'name']]  # add filename to dataframe as variable
            csv[[i]] = f
            f <- f %>% select(SRN,Grade,file)
            
         }
         # Merging the data files using rbind
         # assumption that all files have two columns
         
         do.call(rbind, csv) # rbind the datasets
         
      }
   }) # end scriptdata
    
    aas <- reactive({
       if(is.null(input$aas))
          return()
       else 
       {
          nfiles = nrow(input$aas) 
          csv = list()
          for (i in 1 : nfiles)
          {
             f = import(input$aas[[i, 'datapath']])
             f<-f %>% select(`Email address`,Grade) 
             f$file <- input$aas[[i, 'name']]  # add filename to dataframe as variable
             csv[[i]] = f
             
          }
          # Merging the data files using rbind
          # assumption that all files have same columns
          
          data<-do.call(rbind, csv) # rbind the datasets
          colnames(data)<-c("Email","Grade","file")
          data<-data %>% filter(!is.na(Grade))
          return(data)
          
       }
    }) # end aadata
    
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
             csv[[i]] = f
             
          }
          # Merging the data files using rbind
          # assumption that all files have same columns
          
          
          data<-do.call(rbind, csv) # rbind the datasets
          
          
          colnames(data)<-c("Identifier"	,"Full name"	, "Email address",
            "Status","Marker","Grade","Maximum Grade",
            "Marking workflow state (Release grades and feedback)",
            "Grade can be changed",
            "Last modified (grade)","Feedback comments","file")
          
          return(data)
          
       }
    }) 
    
    efile<-reactive({
       if(is.null(input$efile))
          return()
       else 
       {
       file <- input$efile
       ext <- tools::file_ext(file$datapath)
       
       req(file)
       validate(need(ext == "csv", "Please upload a csv file"))
       
       data<-read.csv(file$datapath, header = TRUE)
       
       
       colnames(data)<-c("Identifier"	,"Full name"	, "Email address",
         "Status","Marker","Grade","Maximum Grade",
         "Marking workflow state (Release grades and feedback)",
         "Grade can be changed",
         "Last modified (grade)","Feedback comments")
       
       return(data)
       
       
       
       }
    })
    
    s4file<-reactive({
       if(is.null(input$s4file))
          return()
       else 
       {
       file <- input$s4file
       ext <- tools::file_ext(file$datapath)
       
       req(file)
       validate(need(ext == "csv", "Please upload a csv file"))
       
       data<-read.csv(file$datapath, header = TRUE)
       
       colnames(data)<-c("eng","all","SRN","s4Name","Email")
       
       data<-data %>% select(SRN,s4Name,Email)
       }
    })
   
    details<-reactive({
      if(!is.null(input$details.file)){   # details file supplied
        file <- input$details.file
        ext <- tools::file_ext(file$datapath)
        
        req(file)
        validate(need(ext == "csv", "Please upload a csv file"))
        
        data<-read.csv(file$datapath, header = TRUE)
        
      }
      else
      {
      
      # build default details file from scripts, aas and qfiles
      if(is.null(input$qfiles) & is.null(input$scripts))
        return()
      else 
      {
         # scripts
         nfiles = nrow(input$scripts) 
         csv = list()
         for (i in 1 : nfiles)
         {
           csv[[i]] =  input$scripts[[i, 'name']]  
          }
 
         sources<-do.call(rbind, csv) # rbind the datasets
         
         
         # aas
         if(!is.null(input$aas)){
           
         nfiles = nrow(input$aas) 
         csv = list()
         for (i in 1 : nfiles)
         {
           csv[[i]] =  input$aas[[i, 'name']]  
         }
         
         aas<-do.call(rbind, csv) # rbind the datasets
         
         sources<-rbind(sources,aas)
         
         }
         
         
         # qfiles
         nfiles = nrow(input$qfiles) 
         csv = list()
         for (i in 1 : nfiles)
         {
           csv[[i]] =  input$qfiles[[i, 'name']]  
         }
         
         qfiles<-do.call(rbind, csv) # rbind the datasets
         
         details<-tibble(file=sources, 
                         question=rep(qfiles, length.out=length(sources)),
                         QName=rep(1:length(qfiles),length.out=length(sources)),
                         QweightinSec=rep(100.0000/length(qfiles),length(sources)),
                         section=rep(1,length(sources)),
                         SweightinExam=rep(100.0000,length(sources)),
                         moderate=rep(0,length(sources))) %>% 
            mutate(QName=paste0("Q",QName))
      }
      } 
    })
    
    observeEvent(input$reset, {
       input$details.file<-NULL
       details()
    })
    
    allmarks<-reactive({
      if(is.null(input$scripts) | is.null(s4file))
        return()
      else{
        scripts=left_join(scripts(), s4file(), by="SRN") %>% select(file,SRN,Email,Grade)
        if(!is.null(input$aas)){
          aas<-left_join(aas(),s4file(),by="Email") %>% 
            select(file,SRN,Email,Grade)
          scripts=rbind(scripts,aas)
        }
        scripts<-scripts %>% mutate(category=categorise(Grade))
      } 
    })
    
    moderated<-reactive({
      if(is.null(input$scripts) | is.null(s4file))
        return()
      else{
     
        moderated<-left_join(allmarks(),details()) %>% 
          mutate(moderated=moderate(category,moderate,input$minGrade),
                 percent=percent(moderated))
        # moderated should now have a row for each mark with columns :
        # file, SRN, Email, Grade, category, (from scripts, aas, s4file)
        # question, QName, QweightinSec, section, SweightinExam, moderate, (from details)
        # moderated, percent (computed here)
        
      } 
    })
    
    srn.errors<-reactive({ # only works if CSV files and s4 report available
      if(is.null(input$s4file) | is.null(input$scripts))
        return()
      else 
      {
        srn.errors<-left_join(scripts(), s4file(), by="SRN") %>% filter(is.na(Email))
      }
    })
    
    mark.errors<-reactive({ # only works if CSV files and s4 available
      if(is.null(input$scripts) | is.null(s4file))
        return()
      else 
      {
        allmarks() %>% filter(is.na(category))
      }
    })
    
    nomark.errors<-reactive({ # only works if CSV files and s4 available
      if(is.null(input$scripts) | is.null(s4file))
        return()
      else 
      {
       nmarks.df<-allmarks() %>% group_by(SRN) %>% summarise(found=n())   
       fulltable<-pivot_wider(moderated() %>% 
                                 select(SRN,Email,file,percent),
                              names_from = file, values_from = percent) 
       fulltable<-left_join(fulltable,nmarks.df)
       modal.n<-median(fulltable$found) # assume median value is the right one
       fulltable<-fulltable %>% 
         mutate(error=ifelse(found==0,"No marks",
                       ifelse(found<modal.n,"Too few - Check SRNs", # not fatal
                             ifelse(found>modal.n,"Too many - MUST CORRECT", # fatal error
                                    "OK") 
                       )
         ))
       fulltable<-fulltable %>% filter(!error=="OK")
       
       return(fulltable)
      }
    })
    
    duplicate.errors<-reactive({ # only works if CSV files and s4 available
       if(is.null(input$scripts) | is.null(s4file))
          return()
       else 
       {
          allmarks() %>% group_by(SRN, file) %>% summarise(n=n()) %>% filter(n>1)
          }
       })
    
    checkable<-reactive({# only works if CSV files and s4 available
       if(is.null(input$scripts) | is.null(s4file))
          return()
       else 
       {
       # compute section marks
       sectionmarks<-moderated() %>% 
          mutate(weighted=percent * QweightinSec / 100) %>% 
          group_by(SRN,section,SweightinExam) %>% 
          summarise(secmark=round(sum(weighted),2))
       
       # compute exam mark
       exammarks<-sectionmarks %>% 
          mutate(weighted=secmark * SweightinExam / 100) %>% 
          group_by(SRN) %>% 
          summarise(exammark=round(sum(weighted),2))
       
       
       
       checkable.df<-moderated() %>% select(SRN,Email,file,percent) %>% 
          pivot_wider(names_from = file, values_from = percent)
       
       checkable.df<-  left_join(checkable.df,
                                 sectionmarks %>% pivot_wider(!SweightinExam, 
                                                              names_from = section,
                                                              names_prefix = "Section.",
                                                              values_from = secmark))
       checkable.df<-left_join(checkable.df,
                               exammarks)
       }
       
    })
    
 
    
   output$Intro<-renderUI(
      HTML("<h2>Exam mark collation</h2>This app will combine students' exam marks from several CSV files and AA gradebooks.<br/> It will validate the files and save the combined marks fro each question and the weighted overall grade for upload to the DLE. <br/><hr><br/>")
   )
 
   output$modcode<-renderText({
      MODCODE<<- input$efile %>% str_extract_all("(PSYC|CPSY)\\d+") %>% unlist %>% unique
      paste0("Processing marks for ",MODCODE)
   })
   
   output$summary<-renderUI({
    HTML(paste0(
        "There are ",testfile(scripts())," marks found from paper exam scripts.<br/>",
        "There are ",testfile(aas())," marks found from AA scripts.<br/>",
        "There are ",testfile(qfiles())," marks to be returned.<br/>",
        "There are ",testfile(efile())," students listed in the Module on the DLE<br/>",
        "There are ",testfile(s4file())," students enrolled in the Module on S4<br/>"
     ))
   })
   
   output$downloadDetails <- downloadHandler( "details.csv",
                                              
                                              content = function(file) {
                                                orig<-file()
                                                
                                                f <- details()
                                                
                                                write.csv(f, file, row.names = FALSE)
                                              }
   )

   
   output$srnInfo<-renderUI(
     HTML("<h2>Check these SRNs</h2>The CVS files have been compared against the S4 enrolment report, and the following SRNs were not recognised.<br/>Please correct the CSV files, and reupload them.<hr>The 'Missing marks' tab may help identify which students they might be.<br/>If there is no table below, then all SRNs have been found in the S4 file and there are no errors.<hr/>")
   )
   
   output$srn.errors<-renderTable(srn.errors())
   
   output$markInfo<-renderUI(
     HTML("<h2>Check these Marks</h2>These marks in the CSV files or AA files do not match values in the Categorical marking Scale.<br/>Please correct them and reupload.<br/>If none are shown, then all marks are acceptable.<hr/>")
   )
   
   output$mark.errors<-renderTable(mark.errors())
  
   output$nomarkInfo<-renderUI(
     HTML("<h2>Students with wrong number of marks.</h2>If they have no marks this is probably because they have EC.<br/> If they have too few marks, check incorrect SRNs or missing scripts.<br/>If they have TOO MANY marks, then the lowest mark should be deleted from the CSV or AA file.<hr/>")
   )
   
   output$nomark.errors<-renderTable(nomark.errors())
   
   output$dupInfo<-renderUI(
      HTML("<h2>Students with duplicate marks.</h2>These SRNs appear more than once in a question. Please check and delete errors before continuing.<hr/>")
   )
   
   output$output2<-renderUI(
      HTML("<h3>Files for Module Leader:</h3>")
   ) 
   
   output$output1<-renderUI(
      HTML("<h3>Files for upload to DLE</h3>")
   ) 
   output$duplicate.errors<-renderTable(duplicate.errors())  
   
   
   output$downloadE <- downloadHandler(
     filename = "Exam.csv",
     content = function(file){
    
       
       m<-moderated() %>% 
         select(Email,percent,QweightinSec,section,SweightinExam) %>% 
         mutate(percent=round(percent*(QweightinSec/100)*(SweightinExam/100),2))
       e<-m %>% group_by(Email) %>% summarise(ExamMark=sum(percent))
       f<-left_join(efile(),e ,by=c("Email address" = "Email")) %>% 
            mutate(Grade=ExamMark) %>% select(-ExamMark)
       write.csv(f, file, row.names = FALSE)
     }
   )
   
   output$downloadQ <- downloadHandler(
     filename = "Questions.zip",
     content = function(file){
       #go to a temp dir to avoid permission issues
       owd <- setwd(tempdir())
       on.exit(setwd(owd))
       files <- NULL;
       qfiles<-qfiles()
       moderated<-moderated()
       
       #loop through the sheets
       nfiles = nrow(input$qfiles) 
       for (i in 1 : nfiles)
       {
         fileName =  input$qfiles[[i, 'name']] 
         f<-qfiles %>% filter(file==fileName) %>% select(-file)
         m<-moderated%>% filter(question==fileName)%>% select(Email,percent)
         f<-left_join(f,m ,by=c("Email address" = "Email")) %>% 
           mutate(Grade=percent) %>% select(-percent)
         write.csv(f, fileName, row.names = FALSE)
         files <- c(fileName,files)
       }
       #create the zip file
       zip(file,files)
     }
   )
  # these for debugging
  output$csv.table<-renderTable(scripts())
  output$aa.table<-renderTable(aas())
  output$q.table<-renderTable(qfiles())
  output$e.table<-renderTable(efile())
  output$s4.table<-renderTable(s4file())
  output$mod.table<-renderTable(moderated())
 
  
   # 
  output$check.table<-renderTable(checkable())  
  
  output$details.table<-renderTable(details())
  

  output$downloadR <- downloadHandler(     
     filename = "Exam Collation Report.html",
     content = function(file) {
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        
        params <- list(
           moderated = moderated(),
           categorical = categorical,
           srn.errors = srn.errors(),
           mark.errors=mark.errors(),
           nomark.errors=nomark.errors(),
           checkable=checkable(),
           minGrade=input$minGrade,
           anonymise=input$anonymise
        )
        
        rmarkdown::render(input = tempReport, 
                          output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
     }
  )
 
  
  output$checkable <- downloadHandler(
     filename = "Checkable.csv",
     content = function(file){
       f<-checkable()
       if(input$anonymise=="Yes"){f<-f %>% select(-Email)}
       export(f, file)
     }
  )
  
 
 
} # end server

# Run the application 
shinyApp(ui = ui, server = server)
