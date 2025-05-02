## dissertation marks chopper upper
##
## once marking complete this chops up the processed data into subsets for each member of staff

library(tidyverse)
library(rio)

input<-"DissertationMarks605.csv"

data<-import(input)

supers<-data %>% select(Super) %>% unique.data.frame()%>% rename("StaffSurname"="Super")
seconds<-data %>% select(Second) %>% unique.data.frame() %>% rename("StaffSurname"="Second")
staff<-unique(rbind(supers, seconds))

for (person in staff$StaffSurname){
  super.data<-data %>% filter(Super==person)
  second.data<-data %>% filter(Second==person)
  
  person.data<-rbind(super.data, second.data)
  
  export(person.data, paste0("personal/",person,".csv"), format="csv")
}
