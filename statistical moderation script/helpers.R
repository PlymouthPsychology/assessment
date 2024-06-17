# Code for Statistical Moderation, 
# to be read  into .Rmd file


library(tidyverse)
library(rio)
library(car)
library(emmeans)

# CONSTANTS AND HELPERS


# Conversion for grades to letters, linear categories 0-15, and classes
CATEGORICAL=data.frame(c(17 , 25, 38, 42, 45, 48, 52, 55, 58, 62, 65, 68, 77, 88, 100),
                       1:15, 
                       c("N-", "N", "N+", "D-", "D", "D+", "C-", "C", "C+", "B-", "B", "B+", "A-", "A", "A+"),
                       c("Fail", "Fail", "Fail", "3rd", "3rd", "3rd", "2:II", "2:II", "2:II", "2:I", "2:I", "2:I", "1st", "1st", "1st")) %>% 
   set_names(c("Grade","category", "letter","Class"))


# define formatting functions
dp<-function(x,p){return(format(round(as.numeric(x), p), nsmall = p))}

# report p to three dp or <.001
pval<-function(p){
   p=as.numeric(p)
   s=paste0("p=",format(round(p, 3), nsmall = 3))
   if(p<.001){s="p<.001"}
   return(s)}

# Report t(df1)=xxx, p=.xxx
trep<-function(t.obj){
   s=paste0("t(", dp(t.obj$parameter,1),")=",dp(t.obj$statistic,3),", ",pval(t.obj$p.value/2))
   return(s)
}
# Report F(df1,df2)=xxx, p=.xxx
frep<-function(f.obj){
   s=paste0("F(", f.obj$Df[1],", ",f.obj$Df[2],")=",dp(f.obj$`F value`[1],3),", ",pval(f.obj$`Pr(>F)`[1]))
   return(s)
}

#make functions to nicely report N and percent two ways
Npc<-function(num,tot){  # N (x%)
   paste0(num," (",round(100*num/tot, digits=0),"%)")
}

Npc2<-function(num,tot){ # (N, x%)
   paste0("(",num,", ",round(100*num/tot, digits=0),"%)")
}


build_adjustments <- function(stat_adjustments){
   
   rstudioapi::showDialog(title="Type of correction", message='Choose type of correction: "NONE" to skip moderation, "UP" to only moderate up, or "BOTH" to moderate up and down based on model estimates. Entering names (or parts of names, separated by spaces or commas) of markers, will adjust for only those markers.')
   mod_direction = rstudioapi::showPrompt(title="Correction", message="Type of correction:", default = "UP")
   
   if (mod_direction == "NONE"){
      #uncomment this to make no changes at all
      adjustments<- stat_adjustments %>%
         mutate(adjust=0)
      
   } else if (mod_direction == "UP") {
      #uncomment this to moderate only upwards using the model's estimates
      adjustments <- stat_adjustments %>%
         mutate(adjust=ifelse(adjust < 0, 0, adjust))
      
   } else if (mod_direction == "BOTH" | mod_direction == "B") {
      adjustments <- stat_adjustments
   } else {
      # TRY TO MATCH NAMES OF MARKERS
      namestring <- mod_direction %>% str_replace_all(",|\\s", "|")
      adjustments <- stat_adjustments %>%
         rowwise() %>% 
         mutate(adjust = ifelse(str_detect(tolower(Marker), namestring), adjust, 0)) %>% 
         ungroup()
   }
   return (adjustments)
}


# DO THE ADJUSTMENTS

# First tell me where the markers' files are
rstudioapi::showDialog(title="Select files", message='Select gradebooks to process')
path <- rstudioapi::selectDirectory(path = getwd())
full_path <- normalizePath(path)

# I will find their names...
files <- dir(path, pattern=".csv", full.names = T)
filenames <- dir(path, pattern=".csv", full.names = F)

# load marks preserving which file each row came from
original <- map_df(files, ~ read_csv(.x) %>% mutate(filename=.x))

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

# remove the zeroes and cnmpute mean, SD two 2 decimal places
nozeroes<-marks %>% filter(category>0)
original.mean<-dp(mean(nozeroes$Grade),2)
original.SD<-dp(sd(nozeroes$Grade),2)


#  make a table of descriptives from the Grades (not catgeories)
descriptives <- nozeroes %>% 
   group_by(Marker) %>% 
   summarise(mean=mean(Grade), sd=sd(Grade), n=n(), med=median(Grade))

# and see if the markers differ using anova
anova.test<-aov(data=nozeroes, category ~ Marker)

# conduct pairwise comparisons
pwc<-TukeyHSD(anova.test)
pwc.p<-cbind(pwc$Marker[,1],pwc$Marker[,4]) # keep diffs and p
pwc.p<-data.frame(pwc.p)
colnames(pwc.p)<-c("difference","p")
#retain any sig diffs
pwc.n <- pwc.p %>% filter(p<0.05)

# do a linear regression of category values
model <- lm(category ~ Marker, data=nozeroes)
originalanova<-Anova(model)

# find out how far off the grand mean each marker is
adjustments_ <- emmeans::contrast(emmeans::emmeans(model, ~Marker), method="eff") %>% 
   as.data.frame() %>% 
   mutate(Marker = str_replace(contrast, " effect", "")) %>% 
   select(Marker, estimate) %>% 
   # turn these into integers and reverse
   mutate(adjust=-round(estimate))


adjustments <- build_adjustments(adjustments_)


# add the adjustment into marks and remove the coefficient from the linear model
nozeroes <- left_join(nozeroes, adjustments) %>% select(-estimate)

# moderate the marks by adding the adjustment to the category
# Marks cannot go above 15, 
# marks cannot be moved below 4 (D-)
# Fails are unchanged
moderated<-nozeroes %>% 
   select(-Grade, -Class) %>% 
   mutate(category=ifelse(category + adjust >15, 15, 
                          ifelse(category < 4, category,
                                 ifelse(category + adjust < 4, 4, category + adjust)
                          )
   )
   )

#convert the categories back into marks
moderated <- left_join(moderated, CATEGORICAL, by="category")

# remove the zeroes again and compute mean, SD
nozeroes <- moderated %>% filter(Grade>0)
moderated.mean <- dp(mean(nozeroes$Grade),2)
moderated.SD <- dp(sd(nozeroes$Grade),2)

# count up the classes
classes <- nozeroes %>% group_by(Class) %>% summarise(n=n())
# and the proportion of each class
classes <- classes %>% mutate(prop=dp(n/sum(n),2))


# do another linear regression
mod.model <- lm(category ~ Marker, data=nozeroes)
moderated.anova <- Anova(mod.model)

#restore Marker's names
moderated <- moderated %>% 
   mutate(Marker=Marker.original) 

# restore the columns to the original set
moderated <- moderated %>% 
   select(colnames(original), -filename)


# make moderated descriptives
moddescriptives <- moderated %>% 
   filter(Grade>0) %>% 
   group_by(Marker) %>% 
   summarise(modmean=mean(Grade), modsd=sd(Grade), modn=n(), modmed=median(Grade))

descriptives <- left_join(descriptives, moddescriptives)


#do some plots

originadensity <- marks %>% 
   ggplot(aes(x=Grade, group=Marker, colour=Marker))+
   geom_density() +
   xlim(0,100) + 
   theme_classic()

moderateddensity <- moderated %>% 
   ggplot(aes(x=Grade, group=Marker, colour=Marker)) + 
   geom_density() + xlim(0,100) +
   theme_classic()


moderateddots.data<- moderated %>% 
   group_by(Marker, Grade) %>% 
   summarise(n=n())

moderateddots <- 
   ggplot(data=moderateddots.data, aes(x=Grade, y=Marker))+
   geom_point(shape=1, aes(size=as.factor(n)))+
   geom_point(data=moddescriptives, aes(x=modmed, y=Marker), shape=18, colour="red")+
   geom_point(data=moddescriptives, aes(x=modmean, y=Marker), shape=18, colour="blue")+
   guides(size="none")+
   xlim(17,100)+
   scale_x_continuous(breaks=CATEGORICAL$Grade)+
   xlab("Grade (red=median, blue=mean")+
   theme_classic()


finaldistribution <- moderated %>% 
   left_join(., CATEGORICAL) %>% 
   ggplot(aes(x=Grade, fill=Class)) +
   geom_bar() +
   xlim(17,100) +
   scale_x_continuous(breaks=CATEGORICAL$Grade) +
   theme_classic()


# save moderated marks
moderated_path <- paste0(full_path, "/moderated")
dir.create(moderated_path, showWarnings = FALSE)
write_csv(moderated, glue::glue("{moderated_path}/moderatedmarks.csv"))


module_codes <- filenames %>% str_extract_all("(PSYC|CPSY)\\d+") %>% unlist %>% unique
