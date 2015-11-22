##Prototype Processing

    library(RCurl)
    library(XML)
    library(SnowballC)
    library(tm)
    library(reshape2)
    library(sqldf)
    trim <- function (x) gsub("^\\s+|\\s+$", "", x)

    setwd("~/Google Drive/DOC/013-Viz/network/")

    data <- read.csv("data_0.csv")
    data$title <- NA
    
    tryCatch({
    for(i in 1:nrow(data)){
      file <- getURL(data[i,2])
      start <- regexpr('<title>',tolower(file))
      end <- regexpr('</title>',tolower(file))
      temp1 <- substr(file,start+7,end-1)
      name <- substr(temp1,regexpr('>',temp1),nchar(temp1))
      name <- gsub("&#8217;","'",name)
      name <- gsub("&amp;","&",name)
      name <- gsub("-"," ",name)
      end1 <- regexpr('\\|',name)
      end2 <- regexpr('•',name)
      if(end1 > 1){
        name <- substr(name,1,end1-1)
      } else if(end2 > 1){
        name <- substr(name,1,end2-1)
      }
      data$title[i]<-name
      print(i)
    }
    })
    
    data<-data[substr(data[,4],1,3)!="403" & substr(data[,4],1,3)!="ERR" ,]
    data<-data[nchar(data[,4])>0,]
    data$title<-trim(data$title)
    data$title<-gsub("&#039;","'",data$title)
    data$title<-gsub("&#039;","'",data$title)
    data$title<-gsub("&#8211;","",data$title)
    data$title<-gsub("&apos;","",data$title)
    data$title<-trim(data$title)
    data$Agency<-trim(data$Agency)
    data$Link<-trim(data$Link)

#Save files
    save(data,file="data.Rda") ##Whole file
    write.csv(data[,4],file="data2.csv",row.names=F) #Just the titles

##Bag of Words
load("data.Rda")
x <- read.csv("data2.csv", header = TRUE)
  

docs <- Corpus(DataframeSource(x))
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
list <- c("2015","bureau","business","census","federal",
          "lexology","new","nist","ntia","quarter","record","u.s.","fiercegovernmentit","startribunecom",
          "day","ntis","noaa","shows","standards","bloomberg","agency","first","inc","makes","govexeccom",
          "commerce","gets","fcw","million","export","wsj","provides","use",
          "using","forbidden","aims","third","update","latest","nextgovcom","releases",
          "second","url","show","will","uspto","text","takes","seeks","retrieved","says","takes",
          "top","text","slips","raquo","july","know","level","may","hits","gcn","draft","director",
          "can","among","americans","rate","small","state","nextgovcom","news","gross","calls","drive",
          "managers","bizwest","back","govt","atlanta","alaska","final","error")
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, list)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
dtm <- DocumentTermMatrix(docs)

findFreqTerms(dtm, 3)
dtm2 <- as.matrix(removeSparseTerms(dtm, 0.986))
dtm2<-as.data.frame(dtm2)
for(i in 1:ncol(dtm2)){
  dtm2[,i][dtm2[,i]>1]<-1
}

#Assign row IDs (identifical)
  data$id <- 1:nrow(data)
  dtm2$id <- 1:nrow(dtm2)

#DTM to article
  temp <- melt(dtm2, id.vars=c("id"))
  temp<-temp[temp$value>0,]
  temp$variable<-as.character(temp$variable)
  table(temp$variable)

#Node
  node_list<- rbind(
                    data.frame(var=temp$variable, grouping ="tag"),
                    data.frame(var=data$Agency, grouping = "agency"),
                    data.frame(var=data$title, grouping = "article")
                    )
  node_list$grouping <- as.character(node_list$grouping)
  nodes <- sqldf("SELECT var as node, count(var) count, grouping
                 FROM node_list 
                 GROUP BY var")
  
  nodes <- nodes[order(nodes$node),]

  nodes <- merge(nodes,data[,c("title","Link")],by.x = "node",by.y="title", all.x=T)
  nodes$Link[is.na(nodes$Link)]<-"#"
  #Short name
    nodes$num<-as.numeric(row.names(nodes))-1
  nodes$name <- paste("_",nodes$num,sep="")
  
  list <- paste("{'name':'", nodes$name,"','count':",nodes$count^1.3,",'group':'",nodes$grouping,
         "','linkCount':3,'label':'",gsub("[[:punct:]]","",nodes$node),"','userCount':true,'url':'",
          nodes$Link,"','shortName':'",paste(substr(gsub("[[:punct:]]","",nodes$node),1,15),"...",sep=""),"'},",sep="")
  writeLines(list,"nodes.txt")
    
#Link
  #Tag to Title -- NUMBERING NEEDS TO BE CLEAN, id=doc ID
    temp2<-merge(temp[,c("id","variable")],data[,c("id","title")],by="id")
    temp2<-temp2[,c(2:3)]
    colnames(temp2) <- c("origin","target")
  #Title to Agency
    temp3 <- data[,c("Agency","title")]
    colnames(temp3) <- c("origin","target")
  #Tag to Agency
    temp4 <- data[,c("id","Agency")]
    temp4 <- merge(temp,temp4,id="id")
    temp4 <- temp4[,c("Agency","variable")]
    colnames(temp4) <- c("origin","target")
    
    
  #link file (names)
    links_temp <- rbind(temp2,temp3,temp4)
    links_temp <- sqldf("SELECT origin, target
                         FROM links_temp
                         GROUP BY origin, target")
    links_temp$origin<-as.character(links_temp$origin)
  #link_num
    links <- merge(links_temp,nodes,by.x="origin",by.y="node",all.x=T)
    links <- links[,c("num","target")]
    colnames(links)<-c("origin","tar")
    links <- merge(links,nodes,by.x="tar",by.y="node",all.x=T)
    links<-links[,c("num","origin")]
    colnames(links) <- c("source","target")
    links$source <- links$source
    links$target <- links$target
    links <- links[order(links$source),]
  #write link
    list <- paste("{'source':",links$source,",'target':",links$target,", 'depth': 5,'count':5 },")
    writeLines(list,"link_pairs.txt")
