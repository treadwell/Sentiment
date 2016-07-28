#Ted Kwartler
#Ted@sportsanalytics.org
#Open Data Science Conference Workshop: Intro to Text Mining using R
#5-30-2015
#v4.0 Frequency Count & Dendogram

#Set the working directory
setwd("~/Documents/Macmillan/Projects/Sentiment analysis")

#libraries
library(tm)
library(ggplot2)
library(ggthemes)

#options, functions
options(stringsAsFactors = FALSE) #text strings will not be factors of categories
Sys.setlocale('LC_ALL','C') #some tweets are in different languages so you may get an error

tryTolower <- function(x){
  # return NA when there is an error
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

clean.corpus<-function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, custom.stopwords)
  return(corpus)
}

#Create custom stop words
custom.stopwords <- c(stopwords('english'), 'lol', 'smh', 'amp', 'beer')

#Dendogram coloring function
colLab <- function(n) {
  if (is.leaf(n)) {
    a <- attributes(n)
    labCol <- labelColors[clusMember[which(names(clusMember) == a$label)]]
    attr(n, "nodePar") <- c(a$nodePar, lab.col = labCol)
  }
  n
}

#bring in some text
text<-read.csv('SFDC_Survey.csv', header=TRUE)

#Keep the meta data, apply the functions to make a clean corpus
dd<-data.frame(id=text$ID,text=text$Experience.Essay)
custom.reader <- readTabular(mapping=list(content="text", id="id"))
corpus <- VCorpus(DataframeSource(dd), readerControl=list(reader=custom.reader))
corpus<-clean.corpus(corpus)

#Make a Document Term Matrix or Term Document Matrix depending on analysis
tdm<-TermDocumentMatrix(corpus)

#inspect popular words, appearing more than "30" times
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
head(v)
d <- data.frame(word = names(v),freq=v)
top.words<-subset(d, v>=100)
dim(top.words)
ggplot(top.words, aes(x=word, y=freq))+geom_bar(stat="identity") +coord_flip()+theme_gdocs()+
  geom_text(aes(label=freq), colour="white",hjust=1.25)

#inspect word associations
associations<-findAssocs(tdm, 'helpful', 0.30)[,1]
terms<-row.names(findAssocs(tdm,'helpful',0.30))
a.df<-data.frame(associations,terms)
a.df$terms<-factor(a.df$terms, levels=a.df$terms)
ggplot(a.df, aes(y=terms)) + geom_point(aes(x=associations), data=a.df)+
  theme_gdocs()+geom_text(aes(x=associations,label=associations), colour="red",hjust=-.25)

#Hierarchical Clustering
tdm2 <- removeSparseTerms(tdm, sparse=0.95) #shoot for ~40 terms
tdm2.df<-as.data.frame(inspect(tdm2))
hc <- hclust(dist(tdm2.df))
hcd <- as.dendrogram(hc)
clusMember <- cutree(hc, 4)
labelColors <- c("#CDB380", "#036564", "#EB6841", "#EDC951")
clusDendro <- dendrapply(hcd, colLab)
plot(clusDendro, main = "Hierarchical Dendrogram", type = "triangle")

#End