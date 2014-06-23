
library("openNLP")
library(tm)
library(e1071)
dataset<-readLines("C:\\Users\\Suresh\\Desktop\\laptop.txt")

text<-c()
class<-c()
temp<-c()
listofwords <-c()
for (i in 1:length(dataset)){
  if(i %% 2 ==1){
    text<-c(text,dataset[i])
  }
  else{
    class<-c(class,dataset[i])
  }
}

datas<-data.frame(text,class)



for (i in 1:150){

s <-datas[i,1]

s <- as.String(s)

## Need sentence and word token annotations.
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))

pos_tag_annotator <- Maxent_POS_Tag_Annotator()
pos_tag_annotator
a3 <- annotate(s, pos_tag_annotator, a2)


## Determine the distribution of POS tags for word tokens.
a3w <- subset(a3, type == "word")
tags <- sapply(a3w$features, `[[`, "POS")
tags
a<-stemDocument(unique(s[a2][which(tags=="JJ")+1]))
b<-stemDocument(unique(s[a2][which(tags=="JJR")+1]))
c<-stemDocument(unique(s[a2][which(tags=="JJS")+1]))
temp<-c(a,b,c)
temp<-temp[!removeWords(temp,stopwords("english"))==""]
temp<-gsub("*[[:punct:]]*", "", temp)
temp<-gsub("*\\d*", "", temp)
temp[unlist(lapply(unlist(temp),function(x) nchar(x))>1)]
temp<-temp[!temp==""]
temp<-temp[unlist(lapply(temp,function(x) length(strsplit(x,split=" ")[[1]])))<2]
temp<-lapply(temp,function(x) tolower(x))
temp<-unique(unlist(temp))

listofwords<-c(listofwords,temp)
}
listofwords<-unique(listofwords)

#making features

featurevectors<-data.frame()
tempfeatures<-c(1:length(listofwords)-1)*0


for (i in 1:150){
  
  s <-datas[i,1]
  
  s <- as.String(s)
  
  ## Need sentence and word token annotations.
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))
  
  pos_tag_annotator <- Maxent_POS_Tag_Annotator()
  pos_tag_annotator
  a3 <- annotate(s, pos_tag_annotator, a2)
  
  
  ## Determine the distribution of POS tags for word tokens.
  a3w <- subset(a3, type == "word")
  tags <- sapply(a3w$features, `[[`, "POS")
  tags
  a<-stemDocument(unique(s[a2][which(tags=="JJ")+1]))
  b<-stemDocument(unique(s[a2][which(tags=="JJR")+1]))
  c<-stemDocument(unique(s[a2][which(tags=="JJS")+1]))
  temp<-c(a,b,c)
  temp<-temp[!removeWords(temp,stopwords("english"))==""]
  temp<-gsub("*[[:punct:]]*", "", temp)
  temp<-gsub("*\\d*", "", temp)
  temp[unlist(lapply(unlist(temp),function(x) nchar(x))>1)]
  temp<-temp[!temp==""]
  temp<-temp[unlist(lapply(temp,function(x) length(strsplit(x,split=" ")[[1]])))<2]
  temp<-lapply(temp,function(x) tolower(x))
  temp<-unique(unlist(temp))
  
  tempfeatures[unlist(sapply(temp,function(x) which(listofwords==x)))]=1
  featurevectors<-rbind(featurevectors,tempfeatures)
}

class<-sapply(class,function(x) as.numeric(x))
class<-class[1:150]+1
featurevectors<-cbind(featurevectors,class)
colnames(featurevectors)
write.csv(file="C:\\Users\\Suresh\\Desktop\\laptopfeatures.txt",x=featurevectors)
model <- svm(class ~ ., data = featurevectors[1:100,])
predict(model,featurevectors[100:150,])
