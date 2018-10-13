#gap minder 

library(gapminder)
library(tidyverse)

# hold<-gapminder::gapminder_unfiltered too little data

gapdir<-paste0(getwd(),"/gapminder")
gapfiles<-list.files(gapdir)

gapdata<-NULL

for (i in 1:length(gapfiles)){

  # import data one at a time
  test<-rio::import(paste0(gapdir,'/',gapfiles[i]))
  
  # figure out indicator for file
  idx<-gsub("[[:space:]]", "", colnames(test)[1])
  
  # reshape data to be long, a row for each country, year
  testlong<-gather(test,c(colnames(test)[-1]),key="year",value=idx)%>%
    filter(!is.na(idx)) %>%
    mutate(year=as.numeric(year))
  
  # rename columns properly
  colnames(testlong)<-c("country","year",idx)  

  # combine accumulating indices
  if (i==1){
    gapdata<-testlong
  }
  else {
    gapdata<-full_join(gapdata,testlong)
  } 
}

rio::export(gapdata,file="gapdata2.xlsx")