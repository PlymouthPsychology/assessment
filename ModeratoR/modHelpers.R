#### modHelpers.R ----



# Conversion for grades to letters, linear categories 0-15, and classes
CATEGORICAL=data.frame(c(17 , 25, 38, 42, 45, 48, 52, 55, 58, 62, 65, 68, 77, 88, 100),
                       1:15, 
                       c("N-", "N", "N+", "D-", "D", "D+", "C-", "C", "C+", "B-", "B", "B+", "A-", "A", "A+"),
                       c("Fail", "Fail", "Fail", "3rd", "3rd", "3rd", "2:II", "2:II", "2:II", "2:I", "2:I", "2:I", "1st", "1st", "1st")) %>% 
   set_names(c("Grade","Category", "Letter","Class"))


# define formatting functions
dp<-function(x,p){return(f_num(x, p))}

# report p to three dp or <.001
pval<-function(p){return(f_pval(p, alpha=.001, digits=3))}


# Report F(df1,df2)=xxx, p=.xxx
frep<-function(f.obj){
   s=paste0("F(", f.obj$Df[1],", ",f.obj$Df[2],")=",dp(f.obj$`F value`[1],3),", ",pval(f.obj$`Pr(>F)`[1]))
   return(s)
}


#### Code ----



processMarks<-function(original){
   
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
   
   # remove the zeroes and compute mean, SD two 2 decimal places
   marks <-marks %>% filter(Category>0)
   
   return(marks)
   
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
   #  see if the markers differ using anova
   model <- lm(Category ~ Marker, data=marks)
   return(Anova(model))
}

moderateMarks<-function(modmarks){
   # moderate the marks by adding the adjustment to the Category
   # Marks cannot go above 15, 
   # marks cannot be moved below 4 (D-)
   # Fails are unchanged
   moderated<-modmarks %>% 
      select(-Grade, -Class, -Letter) %>% 
      mutate(Category=ifelse(Category + Moderate >15, 15, 
                             ifelse(Category < 4, Category,
                                    ifelse(Category + Moderate < 4, 4, Category + Moderate)
                             )
      )
      )
   
   #convert the categories back into marks
   moderated <- left_join(moderated, CATEGORICAL, by="Category")
   return(moderated)
} # end moderateMarks



markersDensity <- function(marks){
   p<-marks %>% ggplot(aes(x=Grade, group=Marker, colour=Marker))+
      geom_density() +
      xlim(0,100) +
      theme_classic()
   return(p)
}
# 
# moderateddots.data<- moderated %>% 
#    group_by(Marker, Grade) %>% 
#    summarise(n=n())
# 
# moderateddots <- 
#    ggplot(data=moderateddots.data, aes(x=Grade, y=Marker))+
#    geom_point(shape=1, aes(size=as.factor(n)))+
#    geom_point(data=moddescriptives, aes(x=modmed, y=Marker), shape=18, colour="red")+
#    geom_point(data=moddescriptives, aes(x=modmean, y=Marker), shape=18, colour="blue")+
#    guides(size="none")+
#    xlim(17,100)+
#    scale_x_continuous(breaks=CATEGORICAL$Grade)+
#    xlab("Grade (red=median, blue=mean")+
#    theme_classic()
# 
# 
distribution <- function(marks){
   marks %>% 
      ggplot(aes(x=Grade, fill=Class)) +
      geom_bar() +
      xlim(17,100) +
      scale_x_continuous(breaks=CATEGORICAL$Grade) +
      theme_classic()
}
# 
# 
# 
# module_codes <- filenames %>% str_extract_all("(PSYC|CPSY)\\d+") %>% unlist %>% unique
# 
