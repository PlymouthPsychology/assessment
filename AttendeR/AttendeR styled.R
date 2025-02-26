library(tidyverse)
library(rio)
library(numform)
library(shiny)


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

# don't know what modcode is yet
modcode<-"Module"

# info text for each file entry box
marksfileinfo <- HTML(paste0(
   vsmall,
   "Click Marks, then Export to Excel the Module Marks panel.",
   vsmalloff
))

s4fileinfo <- HTML(
   paste0(
      vsmall,
      "Click the value shown under Enrolments, then Enrolment Report, Export, Details Only, and save as CSV",
      vsmalloff
   )
)

attfileinfo <- HTML(
   paste0(
      vsmall,
      "From the Module Attendance Percentage Overview, click View Report, Export,  Details Only, and save as CSV",
      vsmalloff
   )
)

panoptofileinfo <- HTML(
   paste0(
      vsmall,
      "From any Panopto recording for the module, navigate to the folder by clicking its name at the top of the page, click the bar chart icon at the upper right, select 'Any Time' at the top right, then click User Completion in the lower left.",
      vsmalloff
   )
)



# Define UI for application using bslib for styling
ui <- page_sidebar(
   # this theme mimics Westworld tablets to a limited extent
   theme = bs_theme(
      bootswatch = "darkly",
      primary = "#0E7CF1",
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
      paste0('<img src="neglogo.png" width="auto" height="80"> <h2>Effect of attendance on Module mark</h2>')
   ),
   
   # Sidebar with logo
   sidebar = sidebar(
      class = "bg-secondary",
      width = "33%",
      
      fileInput("marksfile", marksfileinfo),
      fileInput("s4file", s4fileinfo),
      fileInput("attfile", attfileinfo),
      fileInput("panoptofile", panoptofileinfo),
      
   ),
   
   # use layout for output
   layout_columns(
      card(
         card_header("Predicting Mark from Attendance", class = "text-success"),
         htmlOutput("maininfo"),
         plotOutput("mainplot")
      ),
      
      card(
         card_header("Predicting Mark from Panopto", class = "text-success"),
         htmlOutput("paninfo"),
         plotOutput("panplot")
      ),
      
      card(
         card_header("Predicting Mark from both Attendance and Panopto use", class =
                        "text-success"),
         htmlOutput("bothinfo")
      ),
      
      # two equal panes above a small full length pane
      col_widths = c(6, 6, 12),
      # each row has 12 notional units
      row_heights = c(3, 1)    # these are just ratios
   )
)  # end ui




# Define server logic
server <- function(input, output) {
   # read the s4 enrollment file when dropped into slot
   s4 <- reactive({
      if (is.null(input$s4file))
         # only if file present
         return()
      else
      {
         file <- input$s4file
         ext <- tools::file_ext(file$datapath)
         
         req(file)
         validate(need(ext == "csv", "Please upload a csv file"))
         
         data <- import(file$datapath)
         
         data <- data %>% select(
            `Student Reference Number (SRN)`,
            `Student: Email`,
            `Student: Preferred Firstname`,
            `Student: Preferred Surname`,
            `Student: Account Name`
         )
         colnames(data) <- c("SRN", "Email", "first", "last", "account")
         data <- data %>%
            mutate(attname = paste(last, first, sep = ", "))
      }
   })   # end s4
   
   
   # read the attendance file when dropped into slot
   att <- reactive({
      if (is.null(input$attfile) &
          is.null(input$s4file))
         # only if files present
         return()
      else
      {
         file <- input$attfile
         ext <- tools::file_ext(file$datapath)
         
         req(file)
         validate(need(ext == "csv", "Please upload a csv file"))
         
         data <- read.csv(file$datapath, header = TRUE)
         
         colnames(data) <- c("tar", "del", "attname", "module", "attendance")
         data <- left_join(data, s4() %>% select(attname, SRN))   # add name and SRN to attendance data for later merging
         
      }
   })  # end att
   
   # read the panopto usage file when dropped into slot
   panopto <- reactive({
      if (is.null(input$panoptofile) &
          is.null(input$s4file))
         # only if files present
         return()
      else
      {
         file <- input$panoptofile
         ext <- tools::file_ext(file$datapath)
         
         req(file)
         validate(need(ext == "csv", "Please upload a csv file"))
         
         data <- import(file$datapath)
         
         data <- data %>%
            separate(Name,
                     c("first", "last"),
                     sep = " ",
                     extra = "drop")
         
         data <- left_join(data, s4() %>% select(Email, SRN)) # add emails and SRNs to the data
         
         data %>%
            select(SRN, `Minutes Delivered`) %>%
            group_by(SRN) %>%
            summarise(panopto = sum(`Minutes Delivered`)) %>%
            mutate(panopto = ifelse(is.na(panopto), 0, panopto), # NA = 0
                   panZ = scale(panopto))     # normalize M=0 SD=1
         
      }
   }) # end panopto
   
   # read the marks file when dropped into slot
   marks <- reactive({
      if (is.null(input$marksfile))
         # only if file present
         return()
      else
      {
         file <- input$marksfile
         ext <- tools::file_ext(file$datapath)
         
         req(file)
         validate(need(ext == "csv", "Please upload a csv file"))
         
         data <- import(file$datapath, fill = TRUE)
         
         data <- data %>%
            select(SRN, Module) %>%
            filter(!is.na(Module)) %>%
            mutate(Module = as.numeric(str_remove(Module, "%")))
         
         
      }
   })  #end marks
   
   # find Module code from Marks file
   modcode<-reactive({
      if (is.null(input$marksfile))
         # only if files present
         return()
      else {
         # data <- att() %>% mutate(module=str_sub(module,1,7)) %>%   # truncate teacing activity to module codes
         #    group_by(module) %>%    # for each module code in list
         #    summarise(n=n()) %>%    # count them
         #    arrange(desc(n))        # and arrange in descending order of frequency
         # modcode<-data$module[1]    # modcode of first row
         # 
         file<-input$marksfile
         modcode=str_extract(file$name, "PSYC[0-9]*")  # find module code from Marks filename
      }
   }) #end modcode
   
   
   # combine attendance, panopto and marks data
   students <- reactive({
      if (is.null(input$attfile) |
          is.null(input$s4file) |
          is.null(input$marksfile) |
          is.null(input$panoptofile))
         return()   # if any file missing do nothing
      else{
         students <- att() %>%
            filter(attendance == "Attended") %>%
            group_by(SRN) %>%
            summarise(attendance = n())
         students <- full_join(students, panopto())
         students <- left_join(marks(), students)
      }
   })  # end students
   
   # plot the module mark by attendance coloured by panotopo
   makePlot <- reactive({
      if (is.null(students())) {
         return
      }
      else{
         students <- students()
         MaxLect = max(students$attendance, na.rm = T)
         students %>% filter(Module > 0) %>%
            ggplot(aes(
               x = attendance,
               y = Module,
               colour = panopto
            )) +
            geom_jitter(height = 0, width = .1) +
            geom_smooth(method = "lm", colour = "deeppink") +
            xlab(paste("Lectures attended out of", MaxLect)) +
            ylab("Module Mark") +
            scale_x_continuous(breaks = seq(0, MaxLect, 1),
                               limits = c(0, MaxLect)) +
            scale_y_continuous(breaks = seq(0, 100, 10),
                               limits = c(0, 100)) +
            scale_colour_gradient(low = "#008080", high = "#00FFFF") +
            theme_ww
      }
   })  # end makeplot
   
   # plot the module mark by panoptp coloured by attendance
   makePanPlot <- reactive({
      if (is.null(students())) {
         return
      }
      else{
         students() %>% filter(Module > 0) %>% filter(panopto > 10) %>%
            ggplot(aes(
               x = log10(panopto),
               y = Module,
               colour = attendance
            )) +
            geom_point() +
            geom_smooth(method = "lm", colour = "deeppink") +
            theme_minimal() +
            xlab("Minutes watched on Panopto (Log10)") +
            ylab("Module Mark") +
            scale_y_continuous(breaks = seq(0, 100, 10),
                               limits = c(0, 100)) +
            scale_colour_gradient(low = "#008080", high = "#00FFFF") +
            theme_ww
      }
   }) # end makepanplot
   
   
   
   
   
   output$mainplot <- renderPlot(makePlot())
   
   output$panplot <- renderPlot(makePanPlot())
   
   # results text for effect of attendance on module mark
   output$maininfo <- renderUI({
      if (is.null(students())) {
         HTML(infostring)
         
      }
      
      else
      {
         students <- students()
         Amodel <- lm(data = students %>% filter(Module > 0), Module ~ attendance)
         att.int <- Amodel$coefficients["(Intercept)"]
         att.co <- Amodel$coefficients["attendance"]
         r2 <- summary(Amodel)$r.squared
         c <- c <- tibble(summary(Amodel)$coefficients)
         att.p <- unlist(c[2, 1])[4]
         att.s <- ifelse(att.p < .05, "a significant", "no")
         att.s <- paste0(
            "There was ",
            att.s,
            " effect of attendance on ",modcode()," Mark, ",
            f_pval(att.p, alpha = .001, digits = 3)
         )
         att.d <- students %>% summarise(
            med = median(attendance, na.rm = T),
            mean = mean(attendance, na.rm = T),
            ci = 1.96 * sd(attendance, na.rm =
                              T) / sqrt(n())
         )
         
         HTML(
            paste0(
               att.s,
               "<br/>The median number of lectures attended was ",
               att.d$med,
               " (Mean = ",
               f_num(att.d$mean, 1),
               ", 95%CI= ",
               f_num(att.d$mean - att.d$ci, 1),
               " to ",
               f_num(att.d$mean + att.d$ci, 1),
               ").",
               " With no lectures attended, mark would be ",
               f_num(att.int, 2),
               "%.",
               " For every lecture attended, marks changed by ",
               f_num(att.co, 2),
               "%. ",
               
               "R-squared=",
               f_num(r2, 2)
            )
         )
      }
   })  # end maininfo
   
   # results text for effect of panopto on module mark
   output$paninfo <- renderUI({
      if (is.null(students())) {
         HTML(infostring)
         
      }
      
      else
      {
         students <- students()
         Amodel <- lm(data = students %>% filter(Module > 0), Module ~ panopto)
         att.int <- Amodel$coefficients["(Intercept)"]
         att.co <- Amodel$coefficients["panopto"]
         r2 <- summary(Amodel)$r.squared
         c <- c <- tibble(summary(Amodel)$coefficients)
         att.p <- unlist(c[2, 1])[4]
         att.s <- ifelse(att.p < .05, "a significant", "no")
         att.s <- paste0(
            "There was ",
            att.s,
            " effect of watching Panopto on ",modcode()," Mark, ",
            f_pval(att.p, alpha = .001, digits = 3)
         )
         att.d <- students %>% summarise(
            med = median(panopto, na.rm = T),
            mean = mean(panopto, na.rm = T),
            ci = 1.96 * sd(panopto, na.rm = T) /
               sqrt(n())
         )
         
         HTML(
            paste0(
               att.s,
               "<br/>The median hours of panopto watched was ",
               f_num(att.d$med / 60, 1),
               " (Mean = ",
               f_num(att.d$mean / 60, 1),
               ", 95%CI= ",
               f_num((att.d$mean - att.d$ci) / 60, 1),
               " to ",
               f_num((att.d$mean + att.d$ci) / 60, 1),
               ").",
               " With no panopto watched, mark would be ",
               f_num(att.int, 2),
               "%.",
               " For every hour of panopto watched, marks changed by ",
               f_num(60 * att.co, 2),
               "%. ",
               
               "R-squared=",
               f_num(r2, 2)
            )
            
         )
      }
   }) # end paninfo
   
   
   # results text for effect of both attendance and panopto on module mark
   output$bothinfo <- renderUI({
      if (is.null(students())) {
         HTML(infostring)
         
         
      }
      
      else
      {
         students <- students() %>% mutate(panopto=panopto/60)
         both.cor <- cor.test(students$attendance, students$panopto)
         Bmodel <- lm(data = students %>% filter(Module > 0),
                      Module ~ attendance * panopto)
         both.int <- Bmodel$coefficients["(Intercept)"]
         pan.co <- Bmodel$coefficients["panopto"]
         att.co <- Bmodel$coefficients["attendance"]
         int.co <- Bmodel$coefficients["attendance:panopto"]
         r2 <- summary(Bmodel)$r.squared
         c <- c <- tibble(summary(Bmodel)$coefficients)
         att.p <- unlist(c[2, 1])[4]
         att.s <- ifelse(att.p < .05, "a significant", "no")
         att.s <- paste0(
            "There was ",
            att.s,
            " effect of attendance on ",modcode()," Mark, ",
            f_pval(att.p, alpha = .001, digits = 3),
            ". "
         )
         pan.p <- unlist(c[3, 1])[4]
         pan.s <- ifelse(pan.p < .05, "a significant", "no")
         pan.s <- paste0(
            "There was ",
            pan.s,
            " effect of watching Panopto on ",modcode()," Mark, ",
            f_pval(pan.p, alpha = .001, digits = 3),
            ". "
         )
            int.p <- unlist(c[4, 1])[4]
            int.s <- ifelse(int.p < .05, "a significant", "no")
            int.s <- paste0(
               "There was ",
               int.s,
               " interaction effect on ",modcode()," Mark, ",
               f_pval(int.p, alpha = .001, digits = 3),
               ". "   
         )
         
         HTML(
            paste0(
               "Attendance and Panopto use correlated r=",
               f_num(both.cor$estimate, 2),
               ", ",
               f_pval(both.cor$p.value, alpha = .001, 3),
               ".<br/>",
               pan.s,
               att.s,
               int.s,
               "With no Panopto watched, and no attendance, ",modcode()," mark would be ",
               f_num(both.int, 2),
               "%. ",
               "The regression equation is ",
               f_num(pan.co, 2),
               " x Panopto hours + ",
               f_num(att.co, 2),
               " x Lectures +",
               f_num(int.co, 2),
               " x Panopto x Lectures. ",
               # "For every hour of Panopto watched, marks changed by ",
               # f_num(pan.co, 2),
               # "%. ",
               # "For every lecture attended, marks changed by ",
               # f_num(att.co, 2),
               # "%. ",
               # "The interaction term adds ",
               # f_num(int.co, 2),
               # "% per hour or lecture to these values. ",
               "R-squared=",
               f_num(r2, 2)
            )
            
            
         )
      }
   })  # end both info
   
}  # end server
# Run the application
shinyApp(ui = ui, server = server)
