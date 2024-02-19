install.packages("data.table")
install.packages("tidytext")
install.packages("tidyr")

library(data.table)
library(tidytext)
library(tidyr)
library(dplyr)
library(ggplot2)
library(tidyverse)

df <- read.csv("/Users/jacobwatson/Downloads/tweets_about_sprint.csv")
df$tweet_id <- seq.int(nrow(df))

# Tokenization 
text <- c("Because I could not stop for Death-",
          "He kindly stopped for me",
          "The Carriage held but just ourselves -",
          "and Immortality")
text_df <- tibble(line = 1:4, text = text)

text_df %>% unnest_tokens(word, text)

tidy_text <- df %>% unnest_tokens(word, tweet)

# Remove stop words 
data("stop_words")
print(stop_words)

tidy_text <- tidy_text %>% anti_join(stop_words, by = "word")

tidy_text %>% 
  count(word, sort = TRUE)

# visualization 
tidy_text %>% 
  count(word, sort = TRUE) %>% 
  filter(n > 2000) %>% 
  mutate(word = reorder(word,n)) %>% 
  ggplot(aes(word, n)) + 
  geom_bar(stat= "identity") + 
  coord_flip()

# stemming 
install.packages("SnowballC")
library(SnowballC)

tidy_text <- tidy_text %>% 
  mutate(word2 = wordStem(word))

#td-idf
tweets_words <- tidy_text %>%
  count(tweet_id, word2, sort = TRUE)

total_words <- tweets_words %>%
  group_by(tweet_id) %>%
  summarise(total = sum(n))

tweets_words <- left_join(tweets_words, total_words, by = "tweet_id")

#sentiment analysis
install.packages("textdata")
install.packages("syuzhet")

library(textdata)
library(syuzhet)

#sentiment databases
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

#determine unique sentiments from nrc
nrc <- data.frame(get_sentiments("nrc"))
unique(nrc$sentiment)

#sentiments of joy from NRC database
nrcjoy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")

tidy_text %>%
  inner_join(nrcjoy) %>%
  count(word, sentiment, sort = TRUE)

tidy_text %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)

#sentiments of only tweet with id 36
sentiment <- tidy_text %>%
  filter(tweet_id == 36) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)

#for any user
sentiment <- tidy_text %>%
  filter(user == "CA2016_sp") %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)

#first 2000 tweets
tweets <- tidy_text %>%
  filter(tweet_id <= 2000)
summary(tweets$tweet_id)

#number of tweets words indicating positive/negative sentiment based on afinn & bing databases
tweets$afinn <- get_sentiment(tweets$word, method = "afinn")
view(tweets)
tweets$bing <- get_sentiment(tweets$word, method = "bing")

#sum of positive/negative sentiment words for every tweet
tweets_sentiment <- tweets %>%
  group_by(tweet_id) %>%
  summarise(afinn = sum(afinn), bing = sum(bing))

#word cloud
install.packages("wordcloud")
install.packages("reshape2")
library(wordcloud)
library(reshape2)

tidy_text %>%
  count(word, sort = TRUE) %>%
  with(wordcloud(word, n, max.words = 200))

#positive & negative sentiment (comparison cloud)
tidy_text %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"), max.words = 200)

#TOPIC MODELING
install.packages("tm")
install.packages("topicmodels")
install.packages("slam")

library(tm)
library(topicmodels)
library(slam)

data <- read.csv("/Users/jacobwatson/Downloads/tweets_about_sprint.csv")

#trim data to 2000 tweets
data <- data[1:2000,]

#create ID field based on row number
data$tweet_id <- seq(nrow(data))

#make data readable
corpus <- Corpus(VectorSource(data$tweet))

#generate document term matrix
tweet_dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minWordlength = 2, 
                                                       removeNumbers = TRUE, 
                                                       removePunctuation = TRUE, stemming = TRUE))
dim(tweet_dtm)

ida <- LDA(tweet_dtm, k = 2)

install.packages("ldatuning")
library(ldatuning)

#find best k value
result <- FindTopicsNumber(tweet_dtm, topics = seq(from = 2, to = 22, by 4), 
                           metrics = c("Griffiths2004", "Arun2010", "Deveaud2014"),
                           method = "Gibbs", control = list(seed = 77),
                           mc.cores = 2L, verbose = TRUE)
FindTopicsNumber_plot(result)

lda <- LDA(tweet_dtm, k = 14)
lda_td <- tidy(lda)
lda_td

library(dplyr)

#generate top terms
top_terms <- lda_td %>%
  group_by(topic) %>%
  top_n(8, beta) %>%
  arrange(topic, -beta)

#top top terms
top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~topic, scales = "free") +
  scale_y_reordered()

#clean data
tidy_tweet <- data %>%
  unnest_tokens(word, tweet)

#create custom list of words to remove
library(stopwords)

wordstoremove <- data.frame("word" = c("https", "just", "sprintcar", "sprint", "amp", "rt", "get",
                                       "sprintcar"))

#remove words of a certain length
tidy_tweet <- tidy_tweet %>%
  anti_join(stop_words) %>%
  anti_join(wordstoremove) %>%
  mutate(nchar = nchar(word)) %>%
  filter(nchar > 2 & nchar < 12) %>%
  mutate(word = wordStem(word), languages = "english")

#tf-idf
tweet_words <- tidy_tweet %>%
  count(tweet_id, word, sort = TRUE)

#add tf-idf column
tweet_words <- tweet_words %>%
  bind_tf_idf(word, tweet_id, n)

summary(tweet_words$tf_idf)

tweet_words <- tweet_words %>%
  filter(tf_idf >= 0.4599)

install.packages("quanteda")
library(quanteda)

tweet_dtm <- tweet_words %>%
  cast_dfm(tweet_id, word, n)

lda2 <- LDA(tweet_dtm, k = 14)

lda_td2 <- tidy(lda2)

#generate top terms
top_terms2 <- lda_td2 %>%
  group_by(topic) %>%
  top_n(8, beta) %>%
  arrange(topic, -beta)

#plot top terms
top_terms2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~topic, scales = "free") +
  scale_y_reordered()

#show relevance of top term sets to each tweet
ap_document <- tidy(lda2, matrix = "gamma")


#TEXT CLASSIFICATION - Text Mining Case Study
corpus <- Corpus(ZipSource("/Users/jacobwatson/Downloads/AutoAndElectronics.zip", recursive = T))

inspect(corpus[[2]])


label <- c(rep("Auto", 1000), rep("Electronics", 1000))
label

#remove common words
corpus <- tm_map(corpus, removeWords, c("Lines", "Newsgroups", "Subject","Message-ID", "Date", 
                                        "Article_ID", "Sender", "Organization", "References",
                                        "From", "Path", "Sender"))

news_dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, wordLengths = c(3, 20),
                                                      removeNumbers = TRUE, removePunctuation = TRUE,
                                                      stemming = TRUE))
inspect(news_dtm)

#
result <- FindTopicsNumber(news_dtm, topics = seq(from = 2, to = 20, by 4), 
                           metrics = c("Griffiths2004", "Arun2010", "Deveaud2014"),
                           method = "Gibbs", control = list(seed = 77),
                           mc.cores = 2L, verbose = TRUE)

FindTopicsNumber_plot(result)

lda <- LDA(news_dtm, k = 8, control = list(seed = 100))
lda_td <- tidy(lda)
  
top_terms <- lda_td %>%
  group_by(topic) %>%
  top_n(8, beta) %>%
  arrange(topic, -beta)

#plot top terms
top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~topic, scales = "free") +
  scale_y_reordered()

ap_document <- tidy(lda, matrix = "gamma")
lda_gamma <- as.data.frame(lda@gamma)

#add label & lda_gamma columns
data <- cbind(label = label, lda_gamma)
colnames(data) <- c("label", "T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8")

str(data)

data$label <- as.factor(data$label)

#training & validation data
set.seed(100)
trainIndex <- createDataPartition(data$label, times = 1, p = 0.7, list = FALSE)

train_data <- data[trainIndex, ] 
validation_data <- data[-trainIndex, ]

#decision tree & confusion matrix (classifying electronics vs auto)
library(rpart)
library(rpart.plot)

tree1 <- rpart(label ~., data = train_data)
rpart.plot(tree1)

pred <- predict(tree1, newdata = validation_data, type = "class")
pred

library(caret)
confusionMatrix(pred, validation_data$label, positive = levels(validation_data$label)[2])

tree1

#ROC Curve - prob prediction rather than classification
library(pROC)

probability <- predict(tree1, newdata = validation_data, type = "prob")

roc <- roc(predictor = probability[,2], response = validation_data$label, 
           levels = levels(validation_data$label))
plot(roc)
roc$auc






  








  







