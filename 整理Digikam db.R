
library(magrittr)
library(dplyr)
library(readr)
library(stringr)
library(exifr)
library(exiftoolr)
library(tidyr)
library(exiftoolr)


##read data
ImageInformation <- read.csv("D:/digikam/SQLite_db/YS_ImageInformation.csv", header = TRUE, quote = "\"",
                      dec = ".", fill = TRUE,encoding = "UTF-8")
Tags <- read.csv("D:/digikam/SQLite_db/YS_Tags.csv", header = TRUE, quote = "\"",
                             dec = ".", fill = TRUE,encoding = "UTF-8")

ImageTags <- read.csv("D:/digikam/SQLite_db/YS_ImageTags.csv", header = TRUE, quote = "\"",
                      dec = ".", fill = TRUE,encoding = "UTF-8")

##reform data 'Tags' to 'Tags_3'

Tags <- Tags[,1:3]
colnames(Tags)[1] <- "tagid"

Tags_1 <- reshape(Tags, v.names="name", timevar="pid", idvar="tagid",
                              direction="wide")
colnames(Tags_1)[4:9]<- c("Species", "Sex","Individual","Status","Camera","Age")

Tags_2<- Tags_1[,-c(2:3)]
Tags_3 <- Tags_2[rowSums(is.na(Tags_2)[,-1]) != ncol(Tags_2[,-1]),]


##reform data 'ImageInformation' to 'ImageInformation_4'


ImageInformation <- select(ImageInformation,c("imageid","creationDate"))

ImageInformation_1 <-inner_join(ImageInformation,ImageTags,by = 'imageid')
ImageInformation_2 <- left_join(ImageInformation_1, Tags, by = 'tagid')


ImageInformation_3 <- reshape(ImageInformation_2, v.names="name", timevar="pid", idvar="imageid",
        direction="wide")

ImageInformation_3 <- ImageInformation_3[,-6]



colnames(ImageInformation_3)[4:8]<- c("Species", "Status","Sex","Individual","Camera")


##'ImageInformation_4' 用另外一種方法

ImageInformation_4 <-right_join(ImageInformation_2,Tags_3,by = 'tagid')
ImageInformation_4 <- select(ImageInformation_4,c("imageid","creationDate","tagid","Species", "Status","Sex","Individual","Camera","Age"))

library(tidyverse)
ImageInformation_5 <- ImageInformation_4 %>% group_by(imageid) %>% summarise_all(function(x){
  if(sum(!is.na(x)) == 0){
    x <- NA
  }else{
    x[!is.na(x)]
  }
})



