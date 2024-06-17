#
# This is a Shiny web application. 


library(tidyverse)
library(rio)
library(car)
library(emmeans)
library(numform)

#### modHelpers.R ----



# Conversion for grades to letters, linear categories 1-15, and classes
CATEGORICAL=tibble(Grade=c(0, 17 , 25, 38, 42, 45, 48, 52, 55, 58, 62, 65, 68, 77, 88, 100),
                   Category=0:15, 
                   Letter=c("Z","N-", "N", "N+", "D-", "D", "D+", "C-", "C", "C+", "B-", "B", "B+", "A-", "A", "A+"),
                   Class=c("Zero","Fail", "Fail", "Fail", "3rd", "3rd", "3rd", "2:II", "2:II", "2:II", "2:I", "2:I", "2:I", "1st", "1st", "1st"),
                   MClass= c("Zero","Fail", "Fail", "Fail", "Fail", "Fail", "Fail", "Pass", "Pass", "Pass", "Merit", "Merit", "Merit", "Dist", "Dist", "Dist")) 

CATEGORICAL$MClass<-ordered(CATEGORICAL$MClass, levels=c("Dist","Merit","Pass","Fail"))


# Report F(df1,df2)=xxx, p=.xxx
frep<-function(f.obj){
   paste0("F(", f.obj$Df[1],", ",f.obj$Df[2],")=",f_num(f.obj$`F value`[1],3),", ",f_pval(f.obj$`Pr(>F)`[1],.001,3))
}


#### Functions for moderation ----



processMarks<-function(original, minGrade){
   
   # get rid of empty lines and sort by the Participant ID
   marks <- original %>% filter(!is.na(Grade)) %>% arrange(Identifier)
   
   # remove hyphens and apostrophes from markers' names but keep original
   marks <- marks %>% 
      mutate(Marker.original=Marker,
             Marker=str_remove_all(Marker,"-"),
             Marker=str_remove_all(Marker,"'")
      )
   
   
   #  adds new columns with the category values and classes for each grade (see helpers)
   marks <- left_join(marks, CATEGORICAL)
   
   #  if this is a Masters module, replace Class with MClass
   if(minGrade=="7"){marks$Class=marks$MClass}
   
   # remove the zeroes 
   marks<-marks %>% filter(Category>0)
   
   
}  # end processMarks



checkMarks<-function(marks){
   
   # do a linear regression of category values
   model <- lm(Category ~ Marker, data=marks)
   originalanova<-Anova(model)
   
   # find out how far off the grand mean each marker is
   effects <- emmeans::contrast(emmeans::emmeans(model, ~Marker), method="eff") %>% 
      as.data.frame() %>% 
      mutate(Marker = str_replace(contrast, " effect", "")) %>% 
      select(Marker, estimate) %>% 
      # turn these into integers and reverse
      mutate(moderate=as.integer(-round(estimate))) %>% 
      select(Marker, estimate, moderate)
   
   return(effects)
} # end checkMarks

pairwiseMarkers<-function(marks){
   #  see if the markers differ using anova
   anova.test<-aov(data=marks, Category ~ Marker)
   
   # conduct pairwise comparisons
   t<-TukeyHSD(anova.test)
   
   d<- data.frame(t$Marker)
   d<-rownames_to_column(d, "comparison")
   colnames(d)<-c("comparison","diff","-CI","+CI","p")
   return(d)
   #
} # pairwiseMarkers

anovaMarks<-function(marks){
   model <- lm(Category ~ Marker, data=marks)
   return(Anova(model))
}

moderateMarks<-function(modmarks, minGrade){
   # moderate the marks by adding the adjustment to the Category
   # Marks cannot go above 15, 
   # marks cannot be moved below minGrade (4 or D- for UG, 7 or C- for MSc)
   # marks below minGrade are unchanged
  minGrade=as.integer(minGrade)
   moderated<-modmarks %>% 
      select(-Grade, -Class, -Letter, -MClass) %>% 
      mutate(Category=ifelse(Category + Moderate >15, 15, 
                             ifelse(Category < minGrade, Category,
                                    ifelse(Category + Moderate < minGrade, minGrade, Category + Moderate)
                             )
      )
      )
   
   #convert the categories back into marks
   moderated <- left_join(moderated, CATEGORICAL, by="Category")
   
   #  if this is a Masters module, replace Class with MClass
   if(minGrade=="7"){moderated$Class=moderated$MClass}
   
   return(moderated)
} # end moderateMarks



markersDensity <- function(marks){
   marks %>% ggplot(aes(x=Grade, group=Marker, colour=Marker))+
      geom_density() +
      xlim(0,100) +
      theme_classic()
   
}

distribution <- function(marks){
   marks %>% 
      ggplot(aes(x=Grade, fill=Class)) +
      geom_bar() +
      xlim(17,100) +
      scale_x_continuous(breaks=CATEGORICAL$Grade) +
      theme_classic()
}



#### SHINY ----

library(shiny)

ui <- fluidPage(
   sidebarLayout(
      sidebarPanel(
         htmlOutput("Intro"),
         fileInput("file1", "Choose DLE Gradebook File", accept = ".csv"),
         radioButtons("minGrade", "Select type of Module:",
                      c("Undergraduate" = 4,
                        "Masters" = 7)),
         textOutput("summary"), 
      ),
      mainPanel(
         
         # Output: Tabset  ----
         tabsetPanel(type = "tabs",
                     tabPanel("Original", 
                              textOutput("modcode"),
                              plotOutput("origDist"),
                                          tableOutput("origClasses"),
                                          plotOutput("origPlot"),
                                            tableOutput("markers"),
                                            textOutput("origAnova"),
                                            tableOutput("pairwise")),
                              
                           
                     tabPanel("Moderation",
                              textOutput("modInfo"), 
                              
                              textInput("modValues","Moderation values",value=""),
                              tableOutput("modTable")),
                     tabPanel("Moderated",
                              textOutput("modSummary"),
                              plotOutput("modPlot"),
                              tableOutput("moderated"),
                              tableOutput("mod.pairwise"),
                              tableOutput("modClasses"),
                              plotOutput("modDist")),
                     tabPanel("Output",
                              downloadButton("downloadData", "Download moderated marks"),
                               downloadButton("report", "Download report")
                              
            
                     )
         )
         
      )
   )
)

server <- function(input, output) {
   file<-reactive({
      file <- input$file1
      ext <- tools::file_ext(file$datapath)

      req(file)
      validate(need(ext == "csv", "Please upload a csv file"))

      data<-read.csv(file$datapath, header = TRUE)

   })
   
   moderated<-reactive({
      mods<-input$modValues
      marks<-processMarks(file(),input$minGrade)
     
      Moderate<-unlist(strsplit(mods,split=","))
      Marker<-marks %>% select(Marker) %>% unique() %>% unlist()
      m<-data.frame(cbind(Marker,Moderate))
      m<-m %>% mutate(Moderate=as.integer(Moderate))
      f<-left_join(marks,m) # add modvalue into file or marks by Marker
      moderatedMarks<<-moderateMarks(f,input$minGrade)
      
   })
   
   output$Intro<-renderUI(
      HTML("<h2>Statistical Moderation</h2>This app will conduct Statistical Moderation on a single Question marked by several Markers.<br/> Upload the Gradebook from the DLE (which must include Marker's names) and then apply moderation to each marker if necessary. <br/>You can then download a CSV file (to upload to the DLE) and a Report.<br/><hr><br/>")
   )
 
   output$origPlot<-renderPlot({markersDensity(processMarks(file(),input$minGrade))})
   
   output$modcode<-renderText({
      modcode<<- input$file1 %>% str_extract_all("(PSYC|CPSY)\\d+") %>% unlist %>% unique
      paste0("Processing marks for ",modcode)
   })
   
   output$summary<-renderText({
      f<-processMarks(file(),input$minGrade)
      
      meanmark<-mean(f$Grade)
      sdmark<-sd(f$Grade)
      markers<- f %>% group_by(Marker) %>% summarise(n=n())
      orig.summary<<-paste0(
         "There are ", nrow(f)," marks in this file, from ",nrow(markers),
         " markers. Overall, the mean mark is ",f_num(meanmark,2),
         " (SD=",f_num(sdmark,2),").")
   })
   
   
   
   
   output$markers<-renderTable({
      
         data<-processMarks(file(),input$minGrade) %>% select(Grade, Marker, Category) 
         summary<-data  %>% group_by(Marker) %>% summarise(n=n(), 
                                                           mean=mean(Grade), 
                                                           sd=sd(Grade), median=median(Grade) )
         effects<-checkMarks(data)
         orig.markers<<-left_join(summary,effects)
      
   })
   
   output$pairwise<-renderTable(
      orig.pairwise<<-pairwiseMarkers(processMarks(file(),input$minGrade)), digits=3
   )
   
   output$mod.pairwise<-renderTable(
      mod.pairwise<<-pairwiseMarkers(moderated()), digits=3
   )
   
   output$origAnova<-renderText({
     originalanova<-anovaMarks(processMarks(file(),input$minGrade))   
     origanova<<-paste0("The Grades were converted into linear categorical marks ranging from 1=F- (15%) to 15=A+ (100%), ",
           "and a oneway ANOVA comparing the markers gave ", frep(originalanova),
           ". In the table, 'estimate' is the difference of the marker's mean categorical mark from the overall mean, ",
           "'moderate' is the suggested moderation to try, though please only use this as a guide. ",
           "The table below shows pairwise comparisons between each markers' categorical marks.")
    
   })
   
   output$modInfo<-renderText({
      
      
         "To apply moderation, enter a series of integers separated by commas, e.g. 1,0,0,-2,0. This would moderate the first marker up a grade, and the fourth down 2 grades. No marks below 40 would be changed."
         
      
   })
   
  output$modTable<-renderTable({
     Moderate<-unlist(strsplit(input$modValues,split=","))
     Marker<-processMarks(file(),input$minGrade) %>% select(Marker) %>% unique() %>% unlist()
     modTable<<-cbind(Marker,Moderate)
  })
   
  
  output$moderated<-renderTable({
     
     f<-moderated() %>% select(Grade, Marker, Category) 
     summary<-f  %>% group_by(Marker) %>% summarise(n=n(), 
                                                       mean=mean(Grade), 
                                                       sd=sd(Grade), median=median(Grade) )

  })
  
  output$modSummary<-renderText({
     f<-moderated()
     meanmark<-mean(f$Grade)
     sdmark<-sd(f$Grade)
     modanova<-anovaMarks(f)   
     modSummary<<-paste0("Following moderation, a oneway ANOVA comparing the markers gave ", frep(modanova),
            ". The moderated Grades now have M=",f_num(meanmark,2),
            " (SD=",f_num(sdmark,2),").")
     
  })
  
  output$modPlot<-renderPlot({
 
     f<-moderated()
     markersDensity(f)
  })
  
  
 output$modClasses<-renderTable({
    f<-moderated()
    
    # count up the classes
    classes <- f %>% group_by(Class) %>% summarise(n=n())
    # and the proportion of each class
    modClasses <<- classes %>% mutate(prop=f_percent(100*n/sum(n)))
   
 })
 
 output$origClasses<-renderTable({
    f<-processMarks(file(),input$minGrade)
    
    # count up the classes
    classes <- f %>% group_by(Class) %>% summarise(n=n())
    # and the proportion of each class
    classes <- classes %>% mutate(prop=f_percent(100*n/sum(n)))
    
 })
 
  
 output$origDist<-renderPlot({
    f<-processMarks(file(),input$minGrade)
    distribution(f)
 })
 
 output$modDist<-renderPlot({
    f<-moderated()
    distribution(f)
 })
 
  # Downloadable csv of moderated dataset ----
  output$downloadData <- downloadHandler( "moderated.csv",
    
     content = function(file) {
        orig<-file()
        
        f <- moderated() %>%  mutate(Marker=Marker.original) %>%  select(colnames(orig))
        colnames(f)<-c("Identifier",	"Status",	"Marker"	,"Grade",	"Maximum Grade",	"Marking workflow state (Release grades and feedback)",	"Grade can be changed",	"Last modified (submission)",	"Last modified (grade)","Feedback comments")
 
        write.csv(f, file, row.names = FALSE)
     }
  )
 
 
 output$report <- downloadHandler(     
    filename = "Statistical Moderation Report.html",
    content = function(file) {
       tempReport <- file.path(tempdir(), "report.Rmd")
       file.copy("report.Rmd", tempReport, overwrite = TRUE)
       
       params <- list(
          modcode = modcode,
          marks = processMarks(file(),input$minGrade),
          summary = orig.summary,
          origanova = origanova,
          orig.markers = orig.markers,
          orig.pairwise = orig.pairwise,
          modTable = modTable,
          modSummary = modSummary,
          moderatedMarks = moderatedMarks,
          mod.pairwise = mod.pairwise
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
