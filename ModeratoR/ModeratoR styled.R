library(tidyverse)
library(rio)
library(car)
library(emmeans)
library(numform)

library(bslib)
library(bsicons) # icons.getbootstrap.com
library(thematic)

library(shinyWidgets)  # for fancy ui components

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




# Conversion for grades to letters, linear categories 1-15, and classes
CATEGORICAL = tibble(
  Grade = c(0, 15 , 25, 38, 42, 45, 48, 52, 55, 58, 62, 65, 68, 77, 88, 100),
  Category = 0:15,
  Letter = c(
    "Z",
    "N-",
    "N",
    "N+",
    "D-",
    "D",
    "D+",
    "C-",
    "C",
    "C+",
    "B-",
    "B",
    "B+",
    "A-",
    "A",
    "A+"
  ),
  Class = c(
    "Zero",
    "Fail",
    "Fail",
    "Fail",
    "3rd",
    "3rd",
    "3rd",
    "2:II",
    "2:II",
    "2:II",
    "2:I",
    "2:I",
    "2:I",
    "1st",
    "1st",
    "1st"
  ),
  MClass = c(
    "Zero",
    "Fail",
    "Fail",
    "Fail",
    "Fail",
    "Fail",
    "Fail",
    "Pass",
    "Pass",
    "Pass",
    "Merit",
    "Merit",
    "Merit",
    "Dist",
    "Dist",
    "Dist"
  )
)

CATEGORICAL$MClass <- ordered(CATEGORICAL$MClass, levels = c("Dist", "Merit", "Pass", "Fail"))


# Report F(df1,df2)=xxx, p=.xxx
frep <- function(f.obj) {
  paste0(
    "F(",
    f.obj$Df[1],
    ", ",
    f.obj$Df[2],
    ")=",
    f_num(f.obj$`F value`[1], 3),
    ", ",
    f_pval(f.obj$`Pr(>F)`[1], .001, 3)
  )
}


#### Functions for moderation ----


processMarks <- function(original, masters, continuous) {
  # get rid of empty lines and sort by the Participant ID
  marks <- original %>% filter(!is.na(Grade)) %>% arrange(Identifier)
  
  # remove hyphens and apostrophes from markers' names but keep original
  marks <- marks %>%
    mutate(
      Marker.original = Marker,
      Marker = str_remove_all(Marker, "-"),
      Marker = str_remove_all(Marker, "'")
    )
  
  
  #  for all marks, copy Grade into Mark
  marks <- marks %>% mutate(Mark = Grade)
  if (continuous) {
    # for  Continuous marks,  round Grade down into a categorical value
    
    marks <- marks %>% mutate(
      Grade = case_when(
        Grade < 15 ~ 0,
        Grade < 25 ~ 15,
        Grade < 38 ~ 25,
        Grade < 42 ~ 38,
        Grade < 45 ~ 42,
        Grade < 48 ~ 45,
        Grade < 52 ~ 48,
        Grade < 55 ~ 52,
        Grade < 58 ~ 55,
        Grade < 62 ~ 58,
        Grade < 65 ~ 62,
        Grade < 68 ~ 65,
        Grade < 77 ~ 68,
        Grade < 88 ~ 77,
        Grade < 100 ~ 88,
        Grade == 100 ~ 100,
        TRUE ~ NA
      )
    )
  }
  #  adds new columns with the category values and classes for each grade (see helpers)
  marks <- left_join(marks, CATEGORICAL)
  
  
  
  #  if this is a Masters module, replace Class with MClass
  if (masters) {
    marks$Class = marks$MClass
  }
  
  # remove the zeroes
  marks <- marks %>% filter(Category > 0)
  
  
}  # end processMarks



checkMarks <- function(marks) {
  # do a linear regression of category values
  model <- lm(Category ~ Marker, data = marks)
  originalanova <- Anova(model)
  
  # find out how far off the grand mean each marker is
  effects <- emmeans::contrast(emmeans::emmeans(model, ~ Marker), method =
                                 "eff") %>%
    as.data.frame() %>%
    mutate(Marker = str_replace(contrast, " effect", "")) %>%
    select(Marker, estimate) %>%
    # turn these into integers and reverse
    mutate(moderate = as.integer(-round(estimate))) %>%
    select(Marker, estimate, moderate)
  
  return(effects)
} # end checkMarks

pairwiseMarkers <- function(marks) {
  #  see if the markers differ using anova
  anova.test <- aov(data = marks, Category ~ Marker)
  
  # conduct pairwise comparisons
  t <- TukeyHSD(anova.test)
  
  d <- data.frame(t$Marker)
  d <- rownames_to_column(d, "comparison")
  colnames(d) <- c("comparison", "diff", "-CI", "+CI", "p")
  return(d)
  #
} # pairwiseMarkers

anovaMarks <- function(marks) {
  # make an anova model to compare Markers' categorical grades
  model <- lm(Category ~ Marker, data = marks)
  return(Anova(model))
}

moderateMarks <- function(modmarks, masters) {
  # moderate the marks by adding the adjustment to the Category
  # Marks cannot go above 15 (A+),
  # marks cannot be moved below minGrade (4 or D- for UG, 7 or C- for MSc)
  # marks below minGrade (fails) are unchanged
  
  minGrade = ifelse(masters, 7, 4)
  moderated <- modmarks %>%
    select(-Grade, -Class, -Letter, -MClass) %>%
    mutate(Category = ifelse(
      Category + Moderate > 15,
      15,
      ifelse(
        Category < minGrade,
        Category,
        ifelse(Category + Moderate < minGrade, minGrade, Category + Moderate)
      )
    ))
  
  #convert the categories back into marks
  moderated <- left_join(moderated, CATEGORICAL, by = "Category")
  
  #  if this is a Masters module, replace Class with MClass
  if (masters) {
    moderated$Class = moderated$MClass
  }
  
  return(moderated)
} # end moderateMarks



markersDensity <- function(marks) {
  # a density plot coloured by marker
  marks %>% ggplot(aes(
    x = Grade,
    group = Marker,
    colour = Marker
  )) +
    geom_density() +
    xlim(0, 100) +
    theme_ww
  
}

distribution <- function(marks) {
  # a bar chart showing frequency of each grade (except zero)
  marks %>%
    ggplot(aes(x = Grade, fill = Class)) +
    geom_bar() +
    xlim(15, 100) +
    scale_x_continuous(breaks = CATEGORICAL$Grade) +
    guides(fill = "none") +
    theme_ww
}

modcode <- function(filename) {
  # find the module code stafrting PSYC or CPSY and some digits in the filename, it it is there
  return(filename %>% str_extract_all("(PSYC|CPSY)\\d+") %>% unlist %>% unique)
}

#### SHINY ----
# everything below here defines the layout and fiunciton of the app

library(shiny)


ui <- page_sidebar(
  # this theme mimics Westworld tablets to a limited extent
  theme = bs_theme(
    bootswatch = "darkly",
    primary = "#007777",
    success = "#4CF4F4",
    secondary = "#4C7F7F",
    "table-color" = "#4CF4F4",
    base_font = font_google("Encode Sans Condensed"),
    font_weight_base = "font-weight-light",
    headings_font_weight = 300,
    font_scale = .7
  ),
  
  
  # Application title
  title = HTML(
    paste0(
      '<img src="neglogo.png" width="auto" height="80"> <h2>Statistical Moderation</h2>'
    )
  ),
  
  # Sidebar
  sidebar = sidebar(
    class = "bg-primary",
    width = "25%",
    
    htmlOutput("Intro"),
    htmlOutput("filetext"),
    fileInput("file1", "Choose DLE Gradebook File", accept = ".csv"),
    htmlOutput("summary"),
    materialSwitch(
      inputId = "markType",
      label = "Continuous marks",
      status = "danger"
    ),
    
    htmlOutput("continfotext"),
    
    materialSwitch(
      inputId = "masters",
      label = "Masters module",
      status = "danger"
    ),
    
    htmlOutput("mastersinfotext")
    
    
  ),
  
  
  
  tabsetPanel(
    type = "tabs",
    tabPanel(
      "Original Marks",
      textOutput("modcode"),
      
      layout_columns(
        card(max_height = 350,
          card_header("Original grades"),
          plotOutput("origDist", height =
                       "100%")
        ),
        
        card(max_height = 350,
          card_header("Original distributions"),
          plotOutput("origPlot", height =
                       "100%")
        ),
        
        card(max_height = 350,
          card_header("Statistics"),
          tableOutput("markers"),
          textOutput("origAnova"),
          tableOutput("pairwise")
        ),
        
        card(max_height = 150,
          card_header("Moderate markers"),
          textOutput("modInfo"),
          textInput("modValues", "Moderation values", value =
                      "")
        ),
        
        card(max_height = 150,card_header("To be applied"), tableOutput("modTable")),
        
        col_widths = c(6, 6, 12, 6, 6),
        row_heights = c(1, 2, 1)
      )
    ),
    
    
    
    tabPanel(
      "Moderated Marks",
      
      layout_columns(
        card(max_height = 350,
          card_header("Moderated grades"),
          plotOutput("modDist", height =
                       "100%")
        ),
        
        card(max_height = 350,
          card_header("Moderated distributions"),
          plotOutput("modPlot", height =
                       "100%")
        ),
        
        card(max_height = 350,
          card_header("Statistics"),
          
          
          tableOutput("moderated"),
          textOutput("modSummary"),
          tableOutput("mod.pairwise")
        ),
        
        card(max_height = 150,card_header("Classifications"), tableOutput("modClasses")),
        
        card(max_height = 150,
          card_header("Files to download"),
          downloadButton("downloadData", "Download moderated marks"),
          downloadButton("report", "Download EE report")
        ),
        
        
        
        col_widths = c(6, 6, 12, 8, 4),
        row_heights = c(1, 2, 1)
      )
    ),
    
    
    
  )
  
) # end ui

server <- function(input, output, session) {
  
  file <- reactive({
    # this is a bare function which reads in the data file provided
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    data <- read.csv(file$datapath, header = TRUE)
    if (ncol(data) == 12) {
      data <- data[, c(1, 4:12)]
    }
    # need to remove columns 2 and 3 as identities have been revealed
    
    return(data)
    # data is now accessible by calling file()
  })
  
  moderated <- reactive({
    # apply moderation values to the original marks
    mods <- input$modValues
    marks <- processMarks(file(), input$masters, input$markType)
    
    Moderate <- unlist(strsplit(mods, split = ","))
    Marker <- marks %>% select(Marker) %>% unique() %>% unlist()
    m <- data.frame(cbind(Marker, Moderate))
    m <- m %>% mutate(Moderate = as.integer(Moderate))
    f <- left_join(marks, m) # add modvalue into file or marks by Marker
    
    moderatedMarks <<- moderateMarks(f, input$masters)
    # the <<- saves as global so it can be sent to markdown report
    
  })
  
  output$Intro <- renderUI(HTML(
    paste0(
      vsmall,
      "This app will conduct Statistical Moderation on a single Question marked by several Markers.",
      "<br/>You can then download a CSV file (to upload to the DLE) and a Report for the External Examiners.<hr>",
      vsmalloff
    )
  ))
  
  output$filetext <- renderUI(HTML(
    paste0(
      vsmall,
      "Upload the Gradebook from the DLE (which must include Marker's names) and then apply moderation to each marker if necessary.",
      vsmalloff
    )
  ))
  
  
  # info text for Continuous marks
  output$continfotext <- renderUI(HTML(
    paste0(
      vsmall,
      "(Although continuous marks are accepted as well as categorical marks, so you can produce graphs and statistics, they are rounded down to the categorical mark (i.e., 67.99 becomes 65 not 68 - so DO NOT upload the moderated output file.)",
      vsmalloff
    )
  ))
  
  # info text for masters modules
  output$mastersinfotext <- renderUI(HTML(
    paste0(
      vsmall,
      "(Masters modules have a pass mark of 50 instead of 40)",
      vsmalloff
    )
  ))
  
  output$origPlot <- renderPlot({
    markersDensity(processMarks(file(), input$masters, input$markType))
  })
  
  output$modcode <- renderText({
    paste0("Processing marks for ", modcode(input$file1))
  })
  
  output$summary <- renderText({
    f <- processMarks(file(), input$masters, input$markType)
    
    meanmark <- mean(f$Grade)
    sdmark <- sd(f$Grade)
    markers <- f %>% group_by(Marker) %>% summarise(n = n())
    updateTextInput(inputId = "modValues", value = paste(c(rep(
      "0", nrow(markers)
    )), collapse = ","))
    orig.summary <<- paste0(
      "There are ",
      nrow(f),
      " marks in this file, from ",
      nrow(markers),
      " markers. Overall, the mean mark is ",
      f_num(meanmark, 2),
      " (SD=",
      f_num(sdmark, 2),
      ")."
    )
    
  })
  
  
  
  
  output$markers <- renderTable({
    data <- processMarks(file(), input$masters, input$markType) %>% select(Grade, Marker, Category)
    summary <- data  %>% group_by(Marker) %>% summarise(
      n = n(),
      mean = mean(Grade),
      sd = sd(Grade),
      median = median(Grade)
    )
    effects <- checkMarks(data)
    orig.markers <<- left_join(summary, effects)
    
  })
  
  output$pairwise <- renderTable(orig.pairwise <<- pairwiseMarkers(processMarks(file(), input$masters, input$markType)), digits =
                                   3)
  
  output$mod.pairwise <- renderTable(mod.pairwise <<- pairwiseMarkers(moderated()), digits =
                                       3)
  
  output$origAnova <- renderText({
    originalanova <- anovaMarks(processMarks(file(), input$masters, input$markType))
    origanova <<- paste0(
      "The Grades were converted into linear categorical marks ranging from 1=F- (15%) to 15=A+ (100%), ",
      "and a oneway ANOVA comparing the markers gave ",
      frep(originalanova),
      ". In the table, 'estimate' is the difference of the marker's mean categorical mark from the overall mean, ",
      "'moderate' is the suggested moderation to try, though please only use this as a guide. ",
      "The table below shows pairwise comparisons between each markers' categorical marks."
    )
  })
  
  output$modInfo <- renderText({
    "To apply moderation, enter a series of integers separated by commas, e.g. 1,0,0,-2,0. This would moderate the first marker up a grade, and the fourth down 2 grades. No Fail marks would be changed."
  })
  
  output$modTable <- renderTable({
    Moderate <- unlist(strsplit(input$modValues, split = ","))
    Marker <- processMarks(file(), input$masters, input$markType) %>% select(Marker) %>% unique() %>% unlist()
    modTable <<- cbind(Marker, Moderate)
  })
  
  
  output$moderated <- renderTable({
    f <- moderated() %>% select(Grade, Marker, Category)
    mod.markers <<- f  %>% group_by(Marker) %>% summarise(
      n = n(),
      mean = mean(Grade),
      sd = sd(Grade),
      median = median(Grade)
    )
  })
  
  output$modSummary <- renderText({
    f <- moderated()
    meanmark <- mean(f$Grade)
    sdmark <- sd(f$Grade)
    modanova <- anovaMarks(f)
    modSummary <<- paste0(
      "Following moderation, a oneway ANOVA comparing the markers gave ",
      frep(modanova),
      ". The moderated Grades now have M=",
      f_num(meanmark, 2),
      " (SD=",
      f_num(sdmark, 2),
      ")."
    )
  })
  
  output$modPlot <- renderPlot({
    f <- moderated()
    markersDensity(f)
  })
  
  
  output$modClasses <- renderTable({
    # get the original class data
    classes <- processMarks(file(), input$masters, input$markType) %>%
      group_by(Class) %>%
      summarise(Orig = n()) %>%
      mutate(OrigP = f_percent(100 * Orig / sum(Orig)))
    # now get the moderated and count up the classes
    modclass <- moderated() %>%
      group_by(Class) %>%
      summarise(Mod = n()) %>%
      mutate(ModP = f_percent(100 * Mod / sum(Mod)))
    
    modClasses <- left_join(classes, modclass)
  })
  
  
  
  
  output$origDist <- renderPlot({
    f <- processMarks(file(), input$masters, input$markType)
    distribution(f)
  })
  
  output$modDist <- renderPlot({
    f <- moderated()
    distribution(f)
  })
  
  # Downloadable csv of moderated dataset ----
  output$downloadData <- downloadHandler(
    "moderated.csv",
    
    content = function(file) {
      orig <- file()
      
      f <- moderated() %>%  mutate(Marker = Marker.original) %>%  select(colnames(orig))
      colnames(f) <- c(
        "Identifier",
        "Status",
        "Marker"	,
        "Grade",
        "Maximum Grade",
        "Marking workflow state (Release grades and feedback)",
        "Grade can be changed",
        "Last modified (submission)",
        "Last modified (grade)",
        "Feedback comments"
      )
      
      write.csv(f, file, row.names = FALSE)
    }
  )
  
  
  output$report <- downloadHandler(
    filename = "Statistical Moderation Report.docx",
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(
        modcode = modcode(input$file1),
        marks = processMarks(file(), input$masters, input$markType),
        summary = orig.summary,
        origanova = origanova,
        orig.markers = orig.markers,
        orig.pairwise = orig.pairwise,
        modTable = modTable,
        modSummary = modSummary,
        moderatedMarks = moderatedMarks,
        mod.markers = mod.markers,
        mod.pairwise = mod.pairwise
      )
      
      rmarkdown::render(
        input = tempReport,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
} # end server

# Run the application
shinyApp(ui = ui, server = server)
