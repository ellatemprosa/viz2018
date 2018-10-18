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
  attr(testlong[,3],"source")<-gapfiles[[i]]

  # combine accumulating indices
  if (i==1){
    gapdata<-testlong
  }
  else {
    gapdata<-full_join(gapdata,testlong, by = c("country" = "country","year" = "year"))
  } 
}

regions<-rio::import("Data Geographies - v1 - by Gapminder.xlsx",sheet=2)

gapdata <- left_join(gapdata,regions,by = c("country" = "name"))

rio::export(gapdata,file="gapdata.xlsx")

# create metadata for gapdata2 ------------------
gapmeta<-psych::describe(gapdata)
ncols<-ncol(gapdata)
# get source
labels_vector <- c("Country","Year",
                   map_chr(3:ncols, function(x) 
                     ifelse(is.null(attr(gapdata[,x], "source")),"", attr(gapdata[,x], "source"))))

gapmeta$source<-labels_vector
gapmeta$source[gapmeta$source==""]<-"Data Geographies - v1 - by Gapminder.xlsx"

DT::datatable(select(gapmeta,-trimmed,-vars,-mad,-range,-skew,-kurtosis,-se),filter="top")%>%
  DT::formatSignif(c("mean","sd","median","min","max"),digits=2)
