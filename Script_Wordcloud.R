
# Installer
# install.packages("tm")  # pour le text mining
# install.packages("SnowballC") # pour le text stemming
# install.packages("wordcloud") # g?n?rateur de word-cloud 
# install.packages("RColorBrewer") # Palettes de couleurs
# Charger
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("stringr")
getwd()
text <- readLines("Introduction.txt")

# Join many lines together
dummy <- paste(
  c("\n", sample(letters, 20, replace = TRUE), "\n"), 
  collapse = ""
) # complex random string as a split marker
text[text == ""] <- dummy #replace empty string by split marker
y <- paste(text, collapse = " ") #make one long string
text <- unlist(strsplit(y, dummy)) #cut the string at the split marker
gsub(" $", "", gsub("^ ", "", text)) # remove space at start and end

# replace special caracters
text <- str_replace_all(text, "[[:punct:]]", " ")
# Change to lowercase
text <- tolower(text)

df <- data.frame(doc_id = c(1), text = text, stringsAsFactors = FALSE)
docs <- Corpus(DataframeSource(df))

inspect(docs)

# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove other words
docs <- tm_map(docs, removeWords, c("that", "from",  "for", "between", "this", "but", "because",
                                    "have", "has", "since", "can", "yet", "also", "often", "our", "far",
                                    "coming", "suite", "due", "all", "not", "this", "such", "call", "male", "web", 
                                    "one", "early", "use", "new", "will", "the", "thus")) 

docs <- tm_map(docs, removeWords, c("the")) 

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 20)

# Redundant words
S1<-c("individuals", "individual","trutta")
for (j in 1:1){ #here do for as many synonim vectors as created
  i<-get(paste("S",j,sep=""))
  b<-data.frame(word=i[1],freq=sum(d$freq[d$word %in% i]))
  d<-d[!d$word %in% i,]
  d<-rbind(d,b)
}
# Wordloud plot
svg("word_cloud.svg", width = 8, height= 8)
set.seed(1234)

wordcloud(words = d$word, freq = d$freq, min.freq = 1.8,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Set2"))
dev.off()
