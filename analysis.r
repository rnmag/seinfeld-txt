## Cleaning and analysis ----
# Rafael N. Magalhães

# Load libraries
library(tm)
library(topicmodels)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)

setwd("C://Users//Rafael//Dropbox//GitHub//Seinfeld//scripts")

# List files
files <- list.files(getwd(), pattern = "*.txt")

# Read scripts
scripts <- lapply(files, readLines)

# Create corpus
corpus <- Corpus(VectorSource(scripts))

# Substituir por espaço caracteres problemáticos e termos fora do escopo
space <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, space, "-")
corpus <- tm_map(corpus, space, "'")
corpus <- tm_map(corpus, space, "•")
corpus <- tm_map(corpus, space, "\"")
corpus <- tm_map(corpus, space, "“")
corpus <- tm_map(corpus, space, "”")
corpus <- tm_map(corpus, space, "’")
corpus <- tm_map(corpus, space, "‘")


# Limpeza geral
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, PlainTextDocument)

# Verificação do banco
writeLines(as.character(corpus[[125]]))

# Matriz documento-termo
dtm <- DocumentTermMatrix(corpus)

# Transformar rótulos de linhas em nome de arquivos
rownames(dtm) <- files

# Criar frequências a partir da matriz e ordená-las
freq <- colSums(as.matrix(dtm))
length(freq)
ord <- data.frame(palavras = names(freq), freq = freq)
ord <- ord[order(-freq),]
write.csv(ord,"word_freq.csv")

# Gráfico de palavras mais frequentes
top_20 <- ord[1:20,]
ggplot(top_20, aes(x=reorder(palavras, freq), y=freq)) + geom_bar(stat="identity") +
  coord_flip() + ggtitle("Most frequent words") +
  labs(y="frequency", x="words") +
  theme(axis.text.x=element_text(size=11,face="bold", colour="black"),
        axis.text.y=element_text(size=11,colour="black", face="bold"),
        axis.title.x=element_text(size=14, face="bold"),
        axis.title.y=element_text(size=14,face="bold"),
        plot.title=element_text(size=18,face="bold"))
ggsave("word_frequency.png", type="cairo-png")

# Nuvem de palavras
png("wordclud.png", width = 900, height = 710)
wordcloud(corpus, max.words = 100, random.order = F, rot.per = .3, colors = brewer.pal(8, "BrBG"))
dev.off()

# Topics ----
burnin <- 1000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

# Number of topics
k <- 5

# Running the simulation
ldaOut <-LDA(dtm,
             k,
             method="Gibbs",
             control=list(nstart=nstart,
                          seed = seed,
                          best=best,
                          burnin = burnin,
                          iter = iter,
                          thin=thin))

# Output
ldaOut.topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopics.csv"))

# Most frequent terms in each topic
ldaOut.terms <- as.matrix(terms(ldaOut,6))
write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"TopicsToTerms.csv"))

# Term probabilities
topicProbabilities <- as.data.frame(ldaOut@gamma)
write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilities.csv"))

# Relative importance of first and second topics
topic1ToTopic2 <- lapply(1:nrow(dtm),function(x)
  sort(topicProbabilities[x,])[k]/sort(topicProbabilities[x,])[k-1])
write.csv(topic1ToTopic2,file=paste("LDAGibbs",k,"Topic1ToTopic2.csv"))

# Relative importance of second and third topics
topic2ToTopic3 <- lapply(1:nrow(dtm),function(x)
  sort(topicProbabilities[x,])[k-1]/sort(topicProbabilities[x,])[k-2])
write.csv(topic2ToTopic3,file=paste("LDAGibbs",k,"Topic2ToTopic3.csv"))

