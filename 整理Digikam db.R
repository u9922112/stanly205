
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

ImageInformation_5 <- ImageInformation_4[,-9]



write.table(ImageInformation_4, file = "D:/digikam/SQLite_db/YS_ImageInformation_4.csv", append = FALSE, quote = TRUE, sep = " ",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "UTF-8")



write(ImageInformation_4, file = "R-ask",
      ncolumns = if(is.character(x)) 1 else 5,
      append = FALSE, sep = " ")

write.csv(Image )

ImageInformation_5 <-ImageInformation_5 %>% 
  group_by(imageid) %>% 
  summarise_all(function(x){
  x[!is.na(x)]
})


require(stats)
formula(PlantGrowth)         # check the default formula
pg <- unstack(PlantGrowth)   # unstack according to this formula
pg
stack(pg)                    # now put it back together
stack(pg, select = -ctrl)    # omitting one vector



ImageTags_1 <- ImageTags  %>% 
  group_by(imageid)%>%
  summarise(
    imageid = c(imageid),
    tagid = c(tagid))

  
  summarize(id = c())

Tags_1 <- Tags  %>% 
  select('id','pid')
Tags_1$pid <- as.character(Tags_1$pid)
Tags_1$id <- as.character(Tags_1$id)


Test_a <- data.frame(V1 = c('4','4','1','2','3','1','2','4','4'),V2= c(1200,1601,1328,1735,1242,1161,1170,1184,1159))
Test_a <- data.frame(V1 = c(4,4,1,2,3,1,2,4,4),V2= c(1200,1601,1328,1735,1242,1161,1170,1184,1159),V3 = c('a','b','a','d','a','f','g','h','i'))


Test_a.1 <- Test_a %>%
  group_by(V3) %>%
  summarize(A = sum(), B = mean())
  
  
  
  
  
  Test_a %>%
  group_by(,V2) %>%
  c


Tags_1 <- Tags_1 %>%
  group_by(pid) %>%
  c

Tags_1 <- group_by(Tags_1)


%>%
  group_by('pid') %>%
  c

ImageTags <- aggregate(x = Camera_DB, by = list(Camera_DB$imageid), FUN = c,simplify = TRUE,drop = TRUE)
ImageTags <- ImageTags[,-2]
colnames(ImageTags)[1] <- "imageid"

Tlist_Tags <- read.csv("D:/digikam test/digikam db Tags.csv", header = TRUE, quote = "\"",
                       dec = ".", fill = TRUE,encoding = "UTF-8")

Tags <- tapply(Tags$id,Tags$pid,c,simplify = TRUE)
Tags <- as.data.frame(Tags,row.names = 1:8)
Tags_d <- data.frame(pid = names(Tags), id = 'Tags',row.names = 1:7)
Tags_d <- left_join(Tags_d,)


TList_e <- Tags$id
TList_f <- ImageTags$tagid  



library(rlist)
Extract_tags <- function(T){
  K <- function(x){intersect(x,T)}
  lapply(TList_e,K)}
F1 <- lapply(TList_f,Extract_tags)  


b <- function(a){as.numeric(as.character(a))}
F1 <- lapply(F1,b)
F2 <- do.call(rbind, F1)
F2 <- as.data.frame(F2)                        # Convert matrix to data.frame
colnames(F2)<- as.character(Tlist_Tags$name)
F3 <- F2
F3[] <- Tags$name[match(unlist(F2), Tags$id)]
F4 <- cbind(ImageTags,F3)
F5 <- right_join(ImageInformation,F4,by = "imageid")
colnames(F5)[2]<-"DateTimeOriginal"


F5$DateTimeOriginal <- lapply(F5$DateTimeOriginal,function(x){gsub("T"," ",x)})


Camera_db.1 <- F5

col_order <- c("imageid","DateTimeOriginal","tagid","Camera", "Species", "Sex",
               "Individual")
Camera_db.1 <- Camera_db.1[, col_order]
Camera_db.1




Change_time_format <- function(x){
  paste(replace(strsplit(as.character(x),"")[[1]],c(5,8),"-"),collapse = "")
}

Camera_db.1[,"DateTimeOriginal"] <- sapply(Camera_db.1$DateTimeOriginal,Change_time_format)
as.POSIXct(Camera_db.1$DateTimeOriginal, tz = "", format = "%Y-%m-%d %H:%M:%OS", optional = TRUE)




#Sorting the dataframe（將db依Camera,Species,Sex,Individual,DateTimeOriginal排列）
Camera_db.1 <- Camera_db.1[
  order(Camera_db.1$Camera, Camera_db.1$Species, Camera_db.1$Sex, Camera_db.1$Individual, Camera_db.1$DateTimeOriginal),
  ]

#Calculate the time difference (計算前後兩列資料的時間間隔)
TT <- as.POSIXct(Camera_db.1$DateTimeOriginal,  tz = "", format = "%Y-%m-%d %H:%M:%OS", optional = TRUE)
TT_1 <- as.POSIXct(c("",Camera_db.1$DateTimeOriginal)[-length(Camera_db.1$DateTimeOriginal)], format = "%Y-%m-%d %H:%M:%OS", optional = TRUE)

interval <- difftime(TT,TT_1,units = "hour")
interval <- replace(interval, is.na(interval),9999)
Camera_db.2 <- cbind(Camera_db.1, interval)

#Check Independence (以0.5小時判定是否為獨立照片)
check_independence <- function(x){
  if((x<0) || (x>0.5)){
    "I"
  }else{
    "D"
  }
}
Independence_col <- sapply(interval, check_independence)
Camera_db.f <- cbind(Camera_db.2, Independence_col)


#檢視Camera_db.f
View(Camera_db.f)




F5$creationDate <- replace(F5$creationDate,"T"," ")

replace()


Camera_clear <- cbind(ImageTags,F2) 



