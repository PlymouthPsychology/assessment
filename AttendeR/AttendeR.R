library(tidyverse)
library(rio)
library(numform)
library(shiny)
infostring<-paste("Please download the four files needed on the left and drop them into the right place.<br/><br/>",
                       "From the S4 page for the module:<br/>",
                       "<bold>Module Marks</bold> file:  click Marks, then Export to Excel the Module Marks panel.<br/>",
                       "<bold>Attendance</bold> report: from the Module Attendance Percentage Overview, click View Report, Export,  Details Only, and save as CSV.<br/> ",
                       "<bold>S4 enrolment</bold> report: click the value shown under Enrolments, then Enrolment Report, Export, Details Only, and save as CSV.<br/>",
                       "<br/><br/><bold>Panopto </bold>statistics report: From any Panopto recording for the module, navigate to the folder by clicking its name at the top of the page, click the bar chart icon at the upper right, select 'Any Time' at th etop right, then click User Completion in the lower left."
)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Effect of attendance on Module mark"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        fileInput("marksfile","Choose Module Marks file"),
        fileInput("s4file","Choose S4 enrolment report"),
        fileInput("attfile","Choose attendance report"),
        fileInput("panoptofile","Choose panopto statistics report"),
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Attendance", 
                             
                             htmlOutput("maininfo"),
                             plotOutput("mainplot")),
                    tabPanel("Panopto", 
                             
                             htmlOutput("paninfo"),
                             plotOutput("panplot")),
                    tabPanel("Both", 
                             
                             htmlOutput("bothinfo"))
        )
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

 
    
    s4<-reactive({
      if(is.null(input$s4file))
        return()
      else 
      {
        file <- input$s4file
        ext <- tools::file_ext(file$datapath)
        
        req(file)
        validate(need(ext == "csv", "Please upload a csv file"))
        
        data<-import(file$datapath)
        
        data<-data %>% select(`Student Reference Number (SRN)`,
                              `Student: Email`,
                              `Student: Preferred Firstname`,
                              `Student: Preferred Surname`,
                              `Student: Account Name`)
        colnames(data)<-c("SRN","Email","first","last","account")
        data<-data %>% 
          mutate(attname=paste(last,first,sep=", "))
      }
    })
    
    att<-reactive({
      if(is.null(input$attfile) & is.null(input$s4file))
        return()
      else 
      {
        file <- input$attfile
        ext <- tools::file_ext(file$datapath)
        
        req(file)
        validate(need(ext == "csv", "Please upload a csv file"))
        
        data<-read.csv(file$datapath, header = TRUE)
        
        colnames(data)<-c("tar","del","attname","module","attendance")
        data<-left_join(data,s4() %>% select(attname,SRN))

      }
    })
    
    panopto<-reactive({
      if(is.null(input$panoptofile) & is.null(input$s4file))
        return()
      else 
      {
        file <- input$panoptofile
        ext <- tools::file_ext(file$datapath)
        
        req(file)
        validate(need(ext == "csv", "Please upload a csv file"))
        
        data<-import(file$datapath)
        
        data<-data %>% 
          separate(Name,c("first","last"),sep=" ",extra="drop")
        
        data<-left_join(data,s4() %>% select(Email,SRN))
        
        data %>% 
          select(SRN,`Minutes Delivered`) %>% 
          group_by(SRN) %>% 
          summarise(panopto=sum(`Minutes Delivered`)) %>% 
          mutate(panopto=ifelse(is.na(panopto),0,panopto),
                 panZ=scale(panopto))
        
      }
    })
    
    marks<-reactive({
      if(is.null(input$marksfile) )
        return()
      else 
      {
        file <- input$marksfile
        ext <- tools::file_ext(file$datapath)
        
        req(file)
        validate(need(ext == "csv", "Please upload a csv file"))
        
        data<-import(file$datapath, fill = TRUE)
        
        data<-data %>% 
          select(SRN,Module) %>% 
          filter(!is.na(Module)) %>% 
          mutate(Module=as.numeric(str_remove(Module,"%")))
        
      }
    })
    
    students<-reactive({
      if(is.null(input$attfile) | 
         is.null(input$s4file) |
         is.null(input$marksfile)|
         is.null(input$panoptofile))
        return()
      else{
        students<-att() %>% 
          filter(attendance=="Attended") %>% 
          group_by(SRN) %>% 
          summarise(attendance=n()) 
        students<-full_join(students,panopto())
        students<-left_join(marks(),students)
      }
    })
    
    makePlot <- reactive({
      if(is.null(students())){
        return
      }
      else{
         students<-students()
         MaxLect=max(students$attendance, na.rm=T)
         students %>% filter(Module>0) %>% 
          ggplot(aes(x=attendance, y=Module, colour=panopto))+
          geom_jitter(height=0, width=.1)+
          geom_smooth(method = "lm", colour="darkred")+
          theme_minimal()+
          xlab(paste("Lectures attended out of",MaxLect))+
          ylab("Module Mark")+
          scale_x_continuous(breaks=seq(0,MaxLect,1), limits=c(0,MaxLect))+
          scale_y_continuous(breaks=seq(0,100,10), limits=c(0,100))+                     
          scale_colour_gradient(low="grey90",high="grey10")+
          theme(panel.grid.minor = element_blank())
      }
    })
    
    makePanPlot <- reactive({
      if(is.null(students())){
        return
      }
      else{
        students() %>% filter(Module>0) %>% filter(panopto>10) %>% 
          ggplot(aes(x=log10(panopto), y=Module, colour=attendance))+
          geom_point()+
          geom_smooth(method = "lm", colour="darkred")+
          theme_minimal()+
          xlab("Minutes watched on Panopto (Log10)")+
          ylab("Module Mark")+
          scale_y_continuous(breaks=seq(0,100,10), limits=c(0,100))+                     
          scale_colour_gradient(low="grey90",high="grey10")+
          theme(panel.grid.minor = element_blank())
      }
    })
    
    

  
    
    output$mainplot<-renderPlot(makePlot())
    
    output$panplot<-renderPlot(makePanPlot())
    
    
    output$maininfo<-renderUI({
      if(is.null(students())){
        
        HTML(infostring)
        
      }
    
      else
      {
         students<-students()
        Amodel<-lm(data=students %>% filter(Module>0),
                   Module ~ attendance)
        att.int<-Amodel$coefficients["(Intercept)"]
        att.co<-Amodel$coefficients["attendance"]
        r2<-summary(Amodel)$r.squared
        c<-c<-tibble(summary(Amodel)$coefficients)
        att.p<-unlist(c[2,1])[4]
        att.s<-ifelse(att.p<.05,"a significant","no")
        att.s<-paste0("There was ",att.s," effect of attendance on Module Mark, ",
                      f_pval(att.p,alpha=.001,digits=3))
        att.d<-students %>% summarise(med=median(attendance, na.rm=T), mean=mean(attendance, na.rm=T),
                                      ci=1.96*sd(attendance, na.rm=T)/sqrt(n()))

        HTML(paste0("<h3>Predicting Module Mark from Attendance alone</h3>",
           att.s,
          "<br/>The median number of lectures attended was ", att.d$med, 
                      " (Mean = ",f_num(att.d$mean,1),
                      ", 95%CI= ",f_num(att.d$mean-att.d$ci,1)," to ",f_num(att.d$mean+att.d$ci,1),").",
                      " With no lectures attended, mark would be ", f_num(att.int,2),"%.",
                      " For every lecture attended, marks changed by ", f_num(att.co,2),"%. ",
                      
                      "R-squared=",  f_num(r2,2))
        )
      }
    })
    
    output$paninfo<-renderUI({
      if(is.null(students())){
        
        HTML(infostring)
        
      }
      
      else
      { students<-students()
         Amodel<-lm(data=students %>% filter(Module>0),
                    Module ~ panopto)
         att.int<-Amodel$coefficients["(Intercept)"]
         att.co<-Amodel$coefficients["panopto"]
         r2<-summary(Amodel)$r.squared
         c<-c<-tibble(summary(Amodel)$coefficients)
         att.p<-unlist(c[2,1])[4]
         att.s<-ifelse(att.p<.05,"a significant","no")
         att.s<-paste0("There was ",att.s," effect of watching Panopto on Module Mark, ",
                       f_pval(att.p,alpha=.001,digits=3))
         att.d<-students %>% summarise(med=median(panopto, na.rm=T), mean=mean(panopto, na.rm=T),
                                       ci=1.96*sd(panopto, na.rm=T)/sqrt(n()))
         
         HTML(paste0("<h3>Predicting Module Mark from Panopto alone</h3>",att.s,
                     "<br/>The median hours of panopto watched was ", f_num(att.d$med/60,1), 
                     " (Mean = ",f_num(att.d$mean/60,1),
                     ", 95%CI= ",f_num((att.d$mean-att.d$ci)/60,1)," to ",f_num((att.d$mean+att.d$ci)/60,1),").",
                     " With no panopto watched, mark would be ", f_num(att.int,2),"%.",
                     " For every hour of panopto watched, marks changed by ", f_num(60*att.co,2),"%. ",
                     
                     "R-squared=",  f_num(r2,2))
              
        )
      }
    })


output$bothinfo<-renderUI({
   if(is.null(students())){
      HTML(infostring)
      
      
   }
   
   else
   {
    students<-students()
        both.cor<-cor.test(students$attendance, students$panopto)
      Bmodel<-lm(data=students %>% filter(Module>0),
                 Module ~ attendance + panopto)
      both.int<-Bmodel$coefficients["(Intercept)"]
      pan.co<-Bmodel$coefficients["panopto"]
      att.co<-Bmodel$coefficients["attendance"]
      r2<-summary(Bmodel)$r.squared
      c<-c<-tibble(summary(Bmodel)$coefficients)
      att.p<-unlist(c[2,1])[4]
      att.s<-ifelse(att.p<.05,"a significant","no")
      att.s<-paste0("There was ",att.s," effect of attendance on Module Mark, ",
                    f_pval(att.p,alpha=.001,digits=3))
      pan.p<-unlist(c[3,1])[4]
      pan.s<-ifelse(pan.p<.05,"a significant","no")
      pan.s<-paste0("There was ",pan.s," effect of watching Panopto on Module Mark, ",
                    f_pval(pan.p,alpha=.001,digits=3))
      
      HTML(paste0("<h3>Predicting Module Mark from Attendance and Panopto</h3>",
                  "<br/>Attendance and Panopto use correlated r=",
                  f_num(both.cor$estimate,2),", ",
                  f_pval(both.cor$p.value, alpha=.001, 3),".<br/><br/> ",
                  pan.s, "<br/>",att.s,
                  "<br/>With no Panopto watched, and no attendance, mark would be ",
                  f_num(both.int,2),"%.<br/>",
                  " For every hour of Panopto watched, marks changed by ",
                  f_num(60*pan.co,2),"%. ",
                  " <br/> For every lecture attended, marks changed by ",
                  f_num(att.co,2),"%.<br/> R-squared=",
                  f_num(r2,2))
      
      
      )
   }
})

}
# Run the application 
shinyApp(ui = ui, server = server)

