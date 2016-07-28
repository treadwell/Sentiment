#Ted Kwartler
#Ted@sportsanalytics.org
#Open Data Science Conference Workshop: Intro to Text Mining using R
#5-30-2015
#v8.0 Named Entity Recognition

#Set the working directory
setwd("~/Documents/Macmillan/Projects/Sentiment analysis")

#libraries
#install.packages("openNLP.en")
#install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/", type = "source")
library(openNLP)
library(tm)

#Read in the documents and combine into a single source, then tell R its a string
text<-read.csv('SFDC_Survey.csv', header=TRUE)
text<-as.String(text$Experience.Essay)

#OpenNLP Annotators
persons <- Maxent_Entity_Annotator(kind = 'person')
locations <- Maxent_Entity_Annotator(kind = 'location')
organizations <- Maxent_Entity_Annotator(kind = 'organization')
sent.token.annotator <- Maxent_Sent_Token_Annotator(language = "en")
word.token.annotator <- Maxent_Word_Token_Annotator(language = "en")
pos.tag.annotator <- Maxent_POS_Tag_Annotator(language = "en")

#annotate and apply
annotations <- annotate(text,
               list(sent.token.annotator,word.token.annotator,pos.tag.annotator,
                    persons,locations,organizations))

text.annotations<-AnnotatedPlainTextDocument(text,annotations)

#Extract Entities
entities <- function(doc, kind) {
  s <- doc$content
  a <- annotations(doc)[[1]]
  if(hasArg(kind)) {
    k <- sapply(a$features, `[[`, "kind")
    s[a[k == kind]]
  } else {
    s[a[a$type == "entity"]]
  }
}

people<-entities(text.annotations, kind = "person")
head(people)
locations<-entities(text.annotations, kind = "location")
head(locations)
organization<-entities(text.annotations, kind = "organization")
head(organization)
#End