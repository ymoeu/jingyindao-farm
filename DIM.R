rm(list=ls()) #清除全部对象
setwd("C:/Users/lulu/Desktop/data/DMI")
library(dplyr)
library(stringr)
#sample
sample <-as.data.frame(matrix(data = NA, nrow =0, ncol = 5, byrow = FALSE,  dimnames = NULL))
colnames(sample) <-c("栏舍","实际牛头数","应上料量","总上料量","日期")
sample <-transform(sample, 栏舍=as.character(栏舍),
                   实际牛头数=as.integer(as.character(实际牛头数)),
                   应上料量 = as.numeric(as.character(应上料量)),
                   总上料量=as.numeric(as.character(总上料量)),
                   日期=as.character(日期))
#data
file_table <-list.files("./")
#data <-read.csv("326.csv",skip = 1,check.names = F);head(data)
for (f in 1:length(file_table)){
  data <-read.csv(file_table[f],skip = 1,check.names = F)
  colnames(data) <- as.character(str_replace_all(colnames(data), " ", ""))
  #data <-data[3:length(rownames(data))-2,c("栏舍","配方模版","实际牛头数","应上料量(kg)","总上料量(kg)")]
  data <-dplyr::filter(data,grepl("产",配方模版))
  data$栏舍 <-as.character(data$栏舍)
  data$栏舍 <- as.character(str_replace_all(data$栏舍, "南", ""))
  data$栏舍 <- as.character(str_replace_all(data$栏舍, "北", ""))
  data <-arrange(data,栏舍)
  i <- 24
  while(i>14){
    data <-data[1:i,]
    if(data[i,1]=="[8]"){
      break
    }
    i <- i - 1
  }
  data <-filter(data,栏舍!="[13]")
  
  data <-data[c("栏舍","实际牛头数","应上料量(kg)","总上料量(kg)")]
  names(data) <-c("栏舍","实际牛头数","应上料量","总上料量")
  data$实际牛头数 <-as.integer(as.character(data$实际牛头数))
  data$应上料量 <-as.numeric(as.character(data$应上料量))
  data$总上料量 <-as.numeric(as.character(data$总上料量))
  data <-data%>%group_by(栏舍)%>%summarise_at(c("实际牛头数","应上料量","总上料量"),funs(sum))%>%as.data.frame()
  day <-read.csv(file_table[f],nrows = 1,check.names = F)
  data$日期 <-as.character(str_split(colnames(day)[1], '[析]',n=2,simplify = T)[2])
  sample <-rbind(sample,data)
}
names(sample) <-c("栏舍","实际牛头数","应上料量(kg)","总上料量(kg)","日期")
sample <-arrange(sample,日期,栏舍)
write.csv(sample,"../DIM.csv",row.names=F)
