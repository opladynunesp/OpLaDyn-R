#load text mining library
library(tm)

#set working directory (modify path as needed)
setwd("C:\\Users\\Fernanda\\Dropbox\\01-UNESP\\01-OpLaDyn\\MaterialTecnico\\MagnoMalta\\pt")

files <- DirSource(directory = "C:\\Users\\Fernanda\\Dropbox\\01-UNESP\\01-OpLaDyn\\MaterialTecnico\\MagnoMalta\\pt",encoding ="UTF-8" )

#get listing of .txt files in directory
filenames <- list.files(getwd(),pattern="*.txt")
filenames #print such list

#read files into a character vector
#files <- lapply(filenames,readLines) #replace by code in line 7

#create corpus from vector
#docs <- Corpus(VectorSource(files)) #replace by code in next line
docs<- VCorpus(x=files)

#inspect a particular document in corpus
writeLines(as.character(docs[[1]]))

#start preprocessing
#Transform to lower case
docs <-tm_map(docs,content_transformer(tolower))

#remove potentially problematic symbols
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
docs <- tm_map(docs, toSpace, "-")
#writeLines(as.character(docs[[1]])) #just check content

#docs <- tm_map(docs, toSpace, "’")
#docs <- tm_map(docs, toSpace, "‘")
#docs <- tm_map(docs, toSpace, "•")
#docs <- tm_map(docs, toSpace, """)
#docs <- tm_map(docs, toSpace, """"")

#remove punctuation
docs <- tm_map(docs, removePunctuation)
#writeLines(as.character(docs[[1]]))  #just check content

#Strip digits
docs <- tm_map(docs, removeNumbers)
#writeLines(as.character(docs[[1]])) #just check content

#remove stopwords
docs <- tm_map(docs, removeWords, stopwords("pt"))
#writeLines(as.character(docs[[1]])) #just check content

#remove whitespace
docs <- tm_map(docs, stripWhitespace)
#Good practice to check every now and then
#writeLines(as.character(docs[[1]]))  #just check content

#Stem document
#docsStem <- tm_map(docs,stemDocument)
#writeLines(as.character(docsStem[[1]]))

#fix up 1) differences between us and aussie english 2) general errors
docs <- tm_map(docs, content_transformer(gsub),
               pattern = "organiz", replacement = "organ")
docs <- tm_map(docs, content_transformer(gsub),
               pattern = "organis", replacement = "organ")
docs <- tm_map(docs, content_transformer(gsub),
               pattern = "andgovern", replacement = "govern")
docs <- tm_map(docs, content_transformer(gsub),
               pattern = "inenterpris", replacement = "enterpris")
docs <- tm_map(docs, content_transformer(gsub),
               pattern = "team-", replacement = "team")

			   #define and eliminate all custom stopwords
myStopwords <- c("can", "say","one","way","use",
                  "also","howev","tell","will",
                  "much","need","take","tend","even",
                  "like","particular","rather","said",
                  "get","well","make","ask","come","end",
                  "first","two","help","often","may",
                  "might","see","someth","thing","point",
                  "post","look","right","now","think","‘ve ",
                  "‘re ","anoth","put","set","new","good",
                  "want","sure","kind","larg","yes,","day","etc",
                  "quit","sinc","attempt","lack","seen","awar",
                  "littl","ever","moreov","though","found","abl",
                  "enough","far","earli","away","achiev","draw",
                  "last","never","brief","bit","entir","brief",
                  "great","lot")

docs <- tm_map(docs, removeWords, myStopwords)

#inspect a document as a check
writeLines(as.character(docs[[1]]))


#Create document-term matrix
dtm <- DocumentTermMatrix(docs)
dtm

#convert rownames to filenames
rownames(dtm) <- filenames

#collapse matrix by summing over columns
freq <- colSums(as.matrix(dtm))
freq

#length should be total number of terms
length(freq)

#create sort order (descending)
ord <- order(freq,decreasing=TRUE)

#List all terms in decreasing order of freq and write to disk
freq[ord]
write.csv(freq[ord],"word_freq.csv")

library(wordcloud)
#set.seed(142)   
wordcloud(names(freq), freq, min.freq=150)

wordcloud(names(freq), freq, min.freq=10,colors=brewer.pal(8,"Dark2"))



# *** Step 7 : Generate the Word cloud ***
set.seed(142)
#wordcloud(names(freq),min.freq=2,max.words=100,scale=c(2.2,1),colors=brewer.pal(8,"Dark2"),random.color=T, random.order=F)
