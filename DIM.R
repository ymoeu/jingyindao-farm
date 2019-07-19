rm(list=ls()) #���ȫ������
setwd("C:/Users/lulu/Desktop/data/DMI")
library(dplyr)
library(stringr)
#sample
sample <-as.data.frame(matrix(data = NA, nrow =0, ncol = 5, byrow = FALSE,  dimnames = NULL))
colnames(sample) <-c("����","ʵ��ţͷ��","Ӧ������","��������","����")
sample <-transform(sample, ����=as.character(����),
                   ʵ��ţͷ��=as.integer(as.character(ʵ��ţͷ��)),
                   Ӧ������ = as.numeric(as.character(Ӧ������)),
                   ��������=as.numeric(as.character(��������)),
                   ����=as.character(����))
#data
file_table <-list.files("./")
#data <-read.csv("326.csv",skip = 1,check.names = F);head(data)
for (f in 1:length(file_table)){
  data <-read.csv(file_table[f],skip = 1,check.names = F)
  colnames(data) <- as.character(str_replace_all(colnames(data), " ", ""))
  #data <-data[3:length(rownames(data))-2,c("����","�䷽ģ��","ʵ��ţͷ��","Ӧ������(kg)","��������(kg)")]
  data <-dplyr::filter(data,grepl("��",�䷽ģ��))
  data$���� <-as.character(data$����)
  data$���� <- as.character(str_replace_all(data$����, "��", ""))
  data$���� <- as.character(str_replace_all(data$����, "��", ""))
  data <-arrange(data,����)
  i <- 24
  while(i>14){
    data <-data[1:i,]
    if(data[i,1]=="[8]"){
      break
    }
    i <- i - 1
  }
  data <-filter(data,����!="[13]")
  
  data <-data[c("����","ʵ��ţͷ��","Ӧ������(kg)","��������(kg)")]
  names(data) <-c("����","ʵ��ţͷ��","Ӧ������","��������")
  data$ʵ��ţͷ�� <-as.integer(as.character(data$ʵ��ţͷ��))
  data$Ӧ������ <-as.numeric(as.character(data$Ӧ������))
  data$�������� <-as.numeric(as.character(data$��������))
  data <-data%>%group_by(����)%>%summarise_at(c("ʵ��ţͷ��","Ӧ������","��������"),funs(sum))%>%as.data.frame()
  day <-read.csv(file_table[f],nrows = 1,check.names = F)
  data$���� <-as.character(str_split(colnames(day)[1], '[��]',n=2,simplify = T)[2])
  sample <-rbind(sample,data)
}
names(sample) <-c("����","ʵ��ţͷ��","Ӧ������(kg)","��������(kg)","����")
sample <-arrange(sample,����,����)
write.csv(sample,"../DIM.csv",row.names=F)