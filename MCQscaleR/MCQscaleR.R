#
# This is a Shiny web application. 
# January 2025 Jon May

# MCQ score checker   Jon May January 2025
# ====================
#
# Faculty OCR software generates PDFs so this
# script reads student raw answerr data from the 'results breakdown' file
# and the 'correct answers' used from the 'correct answers' file to 
# extract the raw data for our own use
# This potentially allows us to check how scores would change if poor items
# were removed from the test.
#
# Tested on PSYC422 in January 2025 to confirm that it gives same
# answers as the Speedwell OCR.
#
# As the rejected items are answered at random removing them has little effect
# on rank order or candidates but would increase %age correct slightly
#


library(pdftools)   # to extract text from pdf
library(tidyverse)
library(psych)      # for descriptives
library(rio)  # file import export



# Conversion for grades to letters, linear categories 1-15, and classes
CATEGORICAL=tibble(Grade=c(0, 15 , 25, 38, 42, 45, 48, 52, 55, 58, 62, 65, 68, 77, 88, 100),
                   Category=0:15, 
                   Letter=c("Z","N-", "N", "N+", "D-", "D", "D+", "C-", "C", "C+", "B-", "B", "B+", "A-", "A", "A+"),
                   Class=c("Zero","Fail", "Fail", "Fail", "3rd", "3rd", "3rd", "2:II", "2:II", "2:II", "2:I", "2:I", "2:I", "1st", "1st", "1st"),
                   MClass= c("Zero","Fail", "Fail", "Fail", "Fail", "Fail", "Fail", "Pass", "Pass", "Pass", "Merit", "Merit", "Merit", "Dist", "Dist", "Dist")) 

CATEGORICAL$MClass<-ordered(CATEGORICAL$MClass, levels=c("Dist","Merit","Pass","Fail"))
   

#### Functions ----

#function to extract students' answers data from one page of PDF
parsepage<-function(p){
   srnpos<-str_locate(p, "Candidate Number: ")[1,2]
   srn<-str_sub(p,srnpos+1, srnpos+8)
   
   z<-str_length(p)
   x<-str_locate(p,"Response Correct\n\n")
   t<-str_sub(p,x[2], z)  # everything between that string and the end of the page
   
   # strip out spaces, returns, Yes and No, leaving just the letters chosen and item numbers
   t2<-str_remove_all(t," ")
   t3<-str_remove_all(t2,"Yes")
   t4<-str_remove_all(t3,"No")
   t5<-str_remove_all(t4,"\n")
   
   # remove all the item numbers
   t10<-str_remove_all(t5,"[1234567890]")
   
   # retain just the 99 letters
   t11<-str_sub(t10,1,99)
   
   return(tibble(SRN=srn,DATA=t11))
   
} #end parsepage

# given the text of a PDF file, read each students' data into a row of a tibble SRN, DATA
readanswers<-function(text){
   
   #### read PDFs of answers and scoring key
   
   # find the files in the working directory
   # breakdown<-list.files(pattern="results breakdown.pdf")
   # correctanswers<-list.files(pattern="results correct answers.pdf")
   # 
   
   #### read the file with a page for each student ----
   #pdf_file<-file.path("PSYC422 results","PSYC422 results breakdown.pdf")
   #text<-pdf_text(breakdown)
   
   # how many sheets are there
   students<-length(text)
   
   
   # initialise data
   data<-tibble(SRN=NULL,DATA=NULL) 
   
   #### loop  reading each page and adding it to data ----
   for (page  in 1:students){
      data<-rbind(data,parsepage(text[page]))
   }
   
   # remember that the answers are in order 1, 36, 71, 2, 37, 72...
   # 
   itemnames<-paste0("Q",c(1,36,71,
                           2,37,72,
                           3,38,73,
                           4,39,74,
                           5,40,75,
                           6,41,76,
                           7,42,77,
                           8,43,78,
                           9,44,79,
                           10,45,80,
                           11,46,81,
                           12,47,82,
                           13,48,83,
                           14,49,84,
                           15,50,85,
                           16,51,86,
                           17,52,87,
                           18,53,88,
                           19,54,89,
                           20,55,90,
                           21,56,91,
                           22,57,92,
                           23,58,93,
                           24,59,94,
                           25,60,95,
                           26,61,96,
                           27,62,97,
                           28,63,98,
                           29,64,99,
                           30,65,
                           31,66,
                           32,67,
                           33,68,
                           34,69,
                           35,70))
   
   # each answer has width 1
   items<-c(rep(1,99))
   #create a named list of items each width width 1
   nameditems<-set_names(items,itemnames)
   
   # used the named listr to separate the DATA string into 99 correctly named columns
   data<-separate_wider_position(data,DATA,nameditems) %>% pivot_longer(-SRN)
   
   return(data)
}  # end readanswers

# given the students' answers and the text of the correct answer key
# extract key, score answers and do stats
readkey<-function(text){   # data is students' answers, text is page from PDF file correct answers
   #### read the file with the correct answers ----
   #pdf_file<-file.path("PSYC422 results","PSYC422 results correct answers.pdf")
   #text<-pdf_text(correctanswers)
   
   
   t<-tibble(text)   # convert text to a tibble
   t2<-separate_wider_delim(t, text, delim="\n",names_sep="_", too_few="align_start") # put each line on page into new column
   t3<-pivot_longer(t2, text_10:text_52)  # make the page long 
   t4<-t3 %>% select(value) %>% filter(!value=="")  # remove blank lines
   t5<-t4 %>% mutate(value=str_replace(value," ","_"),value=str_replace_all(value," ","")) # make the first space between item and answers and underscore and remove the others
   t6<-separate_wider_delim(t5,value,"_",names=c("Q","Ans"),too_few = "align_start") # split off the item number Q from the answer key  Ans
   scoringkey<-t6 %>% mutate(Q=paste0("Q",Q),   # add a Q to the front of the item number 
                             key=str_sub(Ans,1,4),      # get the first four chars of the answer key
                             correct=case_when(         # decode the answer key
                                key=="1000" ~ "A",
                                key=="0100" ~ "B",
                                key=="0010" ~ "C",
                                key=="0001" ~ "D",
                                T ~ NA)
   ) %>% 
      filter(!is.na(correct)) %>%           # remove garbage lines from end of page
      select(Q,correct)      # just keep the  item number and the correct answer
   return(scoringkey)
} #end readkey
 

score<-function(data, scoringkey){ 
   # now we have an answer key to join onto a long version of the student answers
   
   scored<-left_join(data, scoringkey, join_by(name == Q)) %>%   # merge answers with key
      mutate(score=if_else(value==correct,1,0))                   # if answer chosen matches correct
   return(scored)
}   


grade<-function(aplus, pass, dist){
   points<-aplus-pass  # what is the gap between 42 and 100
   binwidth<-round(points/11,0)    # how wide are the 11 bins
   spare<-points-(11*binwidth)     # how many spare get added to A
   dist %>% group_by(SRN) %>% summarise(score=sum(score))
   dist<-dist %>% mutate(grade=case_when(
      score<pass ~ 38,   # if below pass, grade is 38
      score<pass + binwidth ~ 42,
      score<pass + 2*binwidth ~ 45,
      score<pass + 3*binwidth ~ 48,
      score<pass + 4*binwidth ~ 52,
      score<pass + 5*binwidth ~ 55,
      score<pass + 6*binwidth ~ 58,
      score<pass + 7*binwidth ~ 62,
      score<pass + 8*binwidth ~ 65,
      score<pass + 9*binwidth ~ 68,
      score<pass + 10*binwidth ~ 77,
      score<pass + 11*binwidth + spare ~ 88,
      
      TRUE ~ 100
   ))
   return(dist)
}

#### SHINY ----

library(shiny)

ui <- fluidPage(
   
      mainPanel(
         
         # Output: Tabset  ----
         tabsetPanel(type = "tabs",
                     tabPanel("Original", 
                              htmlOutput("Intro"),
                              fileInput("studentfile", "Choose 'results breakdown' PDF File", accept = ".pdf"),
                              fileInput("anskey", "Choose 'correct answers' PDF File", accept = ".pdf"),
                              fileInput("dlefile", "Upload DLE gradebook file to return marks", accept = ".csv"),
                              fileInput("s4file", "Upload S4 enrolment report to match SRNs to students", accept = ".csv"),
                              #textOutput("summary"), 
                              
                              textOutput("modcode")#,
                              #tableOutput("origClasses"),
                             ),
                     tabPanel("Scaling",
                              htmlOutput("Scaletext"),
                           
                              plotOutput("origDist"),
                              numericInput("aplus","Minimum for A+", value=80, width=150),
                              numericInput("pass","Minimum for D-", value=40, width=150),
                              plotOutput("gradeDist")
                              ),
                     tabPanel("Output",
                              downloadButton("downloadData", "Download graded marks"),
                              downloadButton("report", "Download report"),
                              downloadButton("dleData", "Download grades for upload to DLE"),
                              
            
                             )
                     )
         
              )
   )
#)

server <- function(input, output) {
   students<-reactive({
      file <- input$studentfile
      ext <- tools::file_ext(file$datapath)

      req(file)
      validate(need(ext == "pdf", "Please upload a pdf file"))

      text<-pdf_text(file$datapath)
      
      data<-readanswers(text)
      return(data)

   })
   
   answers<-reactive({
      file <- input$anskey
      ext <- tools::file_ext(file$datapath)
      
      req(file)
      validate(need(ext == "pdf", "Please upload a pdf file"))
      
      text<-pdf_text(file$datapath)
      
      data<-readkey(text)
      return(data)
      
   })
   
   gradebook<-reactive({
      file <- input$dlefile
      ext <- tools::file_ext(file$datapath)
      
      req(file)
      validate(need(ext == "csv", "Please upload a csv file"))
      
      data<-import(file$datapath)
      
      return(data)
      
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
         
         data<-import(file$datapath)
         
         colnames(data)<-c("eng","all","SRN","s4Name","Email")
         
         data<-data %>% select(SRN,s4Name,Email) %>% mutate(SRN=as.character(SRN))
      }
   })
   
   scored<-reactive({
      
      scoredMarks<<-score(students(),answers())
      
   })
   
   output$Intro<-renderUI(
      HTML("<h2>MCQ scoring</h2>This app will read the PDFs produced by Speedwell OCR and help you convert raw N Correct into Grades </br></br>Upload the Gradebook from the DLE, the results breakdown and the correct answer PDFs <br/>You will be shown the distribution of N Correct, and can identify the minimum score needed for A+ (100) and for D- (42).<br/> The marks in between will be equally divided between the other grades and you can adjust these bins.<br/>You can then download a CSV file (to upload to the DLE) and a Report.<br/><br/>")
   )
   
   output$Scaletext<-renderUI(
      HTML("<h2>MCQ scaling</h2>Having inspected the histogram, identify the thresholds for A+ and D- and see the resulting grades below.<br/>")
   )
 
 
   output$modcode<-renderText({
      if(is.null(input$dlefile)|is.null(input$studentfile)|is.null(input$anskey))
         return()
      else 
      {
      modcode<<- input$dlefile %>% str_extract_all("(PSYC|CPSY)\\d+") %>% unlist %>% unique
      
      scored<-scored()
      Nstudents<-length(unique(students()$SRN))
      paste0("Processing marks for ",modcode,". ",
             nrow(scored)," answers found for ",
             nrow(answers())," items from ",
             Nstudents," students.")
             }
   })
   
 
   
   

 output$origDist<-renderPlot({
    dist<-scored() %>% group_by(SRN) %>% summarise(score=sum(score))
    dist %>% ggplot(aes(x=score))+geom_bar()
 })

 
 output$gradeDist<-renderPlot({
    if(is.null(input$dlefile)|is.null(input$studentfile)|is.null(input$anskey))
       return()
    else 
    {   dist<-scored() %>% group_by(SRN) %>% summarise(score=sum(score))
       
    dist<-grade(input$aplus, input$pass, dist)    
    
    pct_format <- scales::percent_format(accuracy = .1)
    
    dist %>% ggplot(aes(x=grade))+
       geom_bar() +
       geom_text(
       aes(
          label = sprintf(
             '%d (%s)',
             ..count..,
             pct_format(..count.. / sum(..count..))
          )
       ),
       stat = 'count',
       nudge_y = 1,
       colour = 'royalblue',
       size = 3
    )
    
    # credit to Stackoverflow for the labelling
    # https://stackoverflow.com/questions/6455088/how-to-put-labels-over-geom-bar-in-r-with-ggplot2
    
    }
 })
 
 
 
 
  # Downloadable csv of scored dataset ----
  output$downloadData <- downloadHandler( 
     filename="checkable.csv",
    
     content = function(file) {
        if(is.null(input$dlefile)|is.null(input$studentfile)|is.null(input$anskey))
           return()
        else 
        {      
        
        
           dist<-scored() %>% group_by(SRN) %>% summarise(score=sum(score))
           
           dist<-grade(input$aplus, input$pass, dist)    
        
         #f <- scored() %>%  mutate(Marker=Marker.original) %>%  select(colnames(orig))
        
        export(dist, file)
        }
     }
  )
 
 # Downloadable csv of scored dataset for DLe----
 output$dleData <- downloadHandler( 
    filename="dle_upload.csv",
    
    content = function(file) {
       if(is.null(input$dlefile)|is.null(input$studentfile)|is.null(input$anskey))
          return()
       else 
       {      
          s4file<-s4file()
          gradebook<-gradebook()
          dist<-scored() %>% group_by(SRN) %>% summarise(score=sum(score))
          
          dist<-grade(input$aplus, input$pass, dist)   
          dist<-left_join(dist,s4file) %>% select(`Email address`=Email,newgrade=grade)
          
          gradebook<-left_join(gradebook,dist) %>% mutate(Grade=newgrade) %>% select(-newgrade)
          
          # colnames(gradebook)<-c("Identifier",	"Full name", "Email address" Status",	"Marker"	,"Grade",	"Maximum Grade",	"Marking workflow state (Release grades and feedback)",	"Grade can be changed",	"Last modified (submission)",	"Last modified (grade)","Feedback comments")
          # 
          
          export(gradebook, file)
       }
    }
 )
 
 
 output$report <- downloadHandler(     
    filename = "MCQ Scoring Report.html",
    content = function(file) {
       tempReport <- file.path(tempdir(), "report.Rmd")
       file.copy("report.Rmd", tempReport, overwrite = TRUE)
       
       params <- list(
          modcode = modcode,
          scored = scored(),
          aplus = input$aplus,
          pass = input$pass
          )
       
       rmarkdown::render(input = tempReport, 
                         output_file = file,
                         params = params,
                         envir = new.env(parent = globalenv())
       )
    }
 )
 
 
} # end server

# Run the application 
shinyApp(ui = ui, server = server)
