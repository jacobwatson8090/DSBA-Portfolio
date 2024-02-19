install.packages("data.table")
install.packages("tidytext")
install.packages("tidyr")
install.packages("SnowballC")
install.packages("textdata")
install.packages("syuzhet")
install.packages("wordcloud")
install.packages("reshape2")
install.packages("tm")
install.packages("topicmodels")
install.packages("slam")
install.packages("ldatuning")
install.packages("quanteda")

library(rpart)
library(rpart.plot)
library(caret)
library(pROC)
library(quanteda)
library(stopwords)
library(dplyr)
library(ldatuning)
library(tm)
library(topicmodels)
library(slam)
library(wordcloud)
library(reshape2)
library(textdata)
library(syuzhet)
library(SnowballC)
library(data.table)
library(tidytext)
library(tidyr)
library(dplyr)
library(ggplot2)
library(tidyverse)

#read
data <- fread("/Users/jacobwatson/Downloads/psychcentral_data.csv", sep=",", header=T, 
              strip.white = T, na.strings = c("NA","NaN","","?")) 
view(data)

#tokenization
text_df=data_frame(line=1:8360,text=data$q_content)
head(text_df)

tidy_text = text_df %>%  
  unnest_tokens(word, text)

data(stop_words)
tidy_text <- tidy_text %>%  
  anti_join(stop_words)

tidy_text %>%  
  count(word,sort = TRUE)

#visualization
tidy_text %>%  
  count(word, sort = TRUE) %>%  
  filter(n > 2000) %>%  
  mutate(word = reorder(word, n)) %>%  
  ggplot(aes(word, n)) +  geom_bar(stat = "identity") +  xlab(NULL) +  coord_flip()

#stemming
tidy_text <- data %>%  
  unnest_tokens(word, q_content) %>%  
  mutate(word = wordStem(word))

#remove stop words
tidy_text <- tidy_text %>%  
  anti_join(stop_words)

#word count
tidy_text %>%  
  count(word,sort = TRUE)

#visualization - 4000
tidy_text %>%  
  count(word, sort = TRUE) %>%  
  filter(n > 4000) %>%  
  mutate(word = reorder(word, n)) %>%  
  ggplot(aes(word, n)) +  geom_bar(stat = "identity") +  xlab(NULL) +  coord_flip()

#wordcloud
tidy_text %>%  
  anti_join(stop_words) %>%  
  count(word) %>%  
  with(wordcloud(word, n, max.words = 200))

#colored wordcloud
tidy_text %>%  
  inner_join(get_sentiments("bing")) %>%  
  count(word, sentiment, sort = TRUE) %>%  
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%  
  comparison.cloud(colors = c("Red", "Blue"), max.words = 100)

#same for columns
text_df=data_frame(line=1:8360,text=data$answers)

#tokenizing
tidy_text = text_df %>%  
  unnest_tokens(word, text)

#removing stop words
data(stop_words)
tidy_text <- tidy_text %>%  
  anti_join(stop_words)

#counting words
tidy_text %>%  
  count(word,sort = TRUE)

#Visualization - count > 2000
tidy_text %>%  
  count(word, sort = TRUE) %>%  
  filter(n > 4000) %>%
  mutate(word = reorder(word, n)) %>%  
  ggplot(aes(word, n)) +  geom_bar(stat = "identity") +  xlab(NULL) +  coord_flip()

#stemming
tidy_text <- data %>%  
  unnest_tokens(word, answers) %>%  
  mutate(word = wordStem(word)) 

#Removing stop words after stemming
tidy_text <- tidy_text %>%  
  anti_join(stop_words)

#Counting the words 
tidy_text %>%  
  count(word,sort = TRUE)

#Visualization count > 4000
tidy_text %>%  
  count(word, sort = TRUE) %>%  
  filter(n > 6000) %>%  
  mutate(word = reorder(word, n)) %>%  
  ggplot(aes(word, n)) +  geom_bar(stat = "identity") +  xlab(NULL) +  coord_flip()

#wordcloud
tidy_text %>%  
  anti_join(stop_words) %>%  
  count(word) %>%  
  with(wordcloud(word, n, max.words = 200))

#colored wordcloud
tidy_text %>%  
  inner_join(get_sentiments("bing")) %>%  
  count(word, sentiment, sort = TRUE) %>%  
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%  
  comparison.cloud(colors = c("Red", "Blue"), max.words = 100)

#DA on rows 1 through 1000
data <- data[1:1000,] 

corpus <- Corpus(VectorSource(data$q_content), readerControl=list(language="en"))
dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minWordLength = 2, 
                                         removeNumbers = TRUE, removePunctuation = TRUE, 
                                         stemDocument = TRUE))


rowTotals <- apply(dtm , 1, sum)


dtm.new <- dtm[rowTotals> 0, ] 


lda <- LDA(dtm.new, k = 5) 
lda_q5=tidy(lda) 

top_terms_q5 <- lda_q5 %>%  
  group_by(topic) %>%  
  top_n(10, beta) %>%  
  ungroup() %>%  
  arrange(topic, -beta)

top_terms_q5 %>%  
  mutate(term = reorder(term, beta)) %>%  
  ggplot(aes(term, beta, fill = factor(topic))) + geom_bar(stat = "identity", show.legend = FALSE) +  facet_wrap(~ topic, scales = "free") +  coord_flip()

#k = 2
lda <- LDA(dtm.new, k = 2) 
lda_q2=tidy(lda) 

top_terms_q2 <- lda_q2 %>%  
  group_by(topic) %>%  
  top_n(10, beta) %>%  
  ungroup() %>%  
  arrange(topic, -beta)

top_terms_q2 %>%  
  mutate(term = reorder(term, beta)) %>%  
  ggplot(aes(term, beta, fill = factor(topic))) + geom_bar(stat = "identity", show.legend = FALSE) +  facet_wrap(~ topic, scales = "free") +  coord_flip()


#k = 3
lda <- LDA(dtm.new, k = 3) 
lda_q3=tidy(lda) 

top_terms_q3 <- lda_q3 %>%  
  group_by(topic) %>%  
  top_n(10, beta) %>%  
  ungroup() %>%  
  arrange(topic, -beta)

top_terms_q3 %>%  
  mutate(term = reorder(term, beta)) %>%  
  ggplot(aes(term, beta, fill = factor(topic))) + geom_bar(stat = "identity", show.legend = FALSE) +  facet_wrap(~ topic, scales = "free") +  coord_flip()

#k = 4
lda <- LDA(dtm.new, k = 4) 
lda_q4=tidy(lda) 

top_terms_q4 <- lda_q4 %>%  
  group_by(topic) %>%  
  top_n(10, beta) %>%  
  ungroup() %>%  
  arrange(topic, -beta)

top_terms_q4 %>%  
  mutate(term = reorder(term, beta)) %>%  
  ggplot(aes(term, beta, fill = factor(topic))) + geom_bar(stat = "identity", show.legend = FALSE) +  facet_wrap(~ topic, scales = "free") +  coord_flip()

#k = 10
lda <- LDA(dtm.new, k = 10) 
lda_q10=tidy(lda) 

top_terms_q10 <- lda_q10 %>%  
  group_by(topic) %>%  
  top_n(10, beta) %>%  
  ungroup() %>%  
  arrange(topic, -beta)

top_terms_q10 %>%  
  mutate(term = reorder(term, beta)) %>%  
  ggplot(aes(term, beta, fill = factor(topic))) + geom_bar(stat = "identity", show.legend = FALSE) +  facet_wrap(~ topic, scales = "free") +  coord_flip()

# perform LDA on rows 1 through 1000 for columns
data <- data[1:1000,] 
corpus_ans <- Corpus(VectorSource(data$answers), readerControl=list(language="en"))
dtm_ans <- DocumentTermMatrix(corpus_ans, control = list(stopwords = TRUE, minWordLength = 2, removeNumbers = TRUE, removePunctuation = TRUE,  stemDocument = TRUE))
rowTotals <- apply(dtm_ans , 1, sum) 
dtm.new_ans <- dtm_ans[rowTotals> 0,]

#topic model for columns
lda_a10 <- LDA(dtm.new_ans, k = 10) 
lda_a10=tidy(lda_a10) 
top_terms_a10 <- lda_a10 %>%  
  group_by(topic) %>%  
  top_n(10, beta) %>%  
  ungroup() %>%  
  arrange(topic, -beta)

top_terms_a10 %>%  
  mutate(term = reorder(term, beta)) %>%  
  ggplot(aes(term, beta, fill = factor(topic))) +  geom_bar(stat = "identity", show.legend = FALSE) +  facet_wrap(~ topic, scales = "free") +  coord_flip()

#k = 2
lda_a2 <- LDA(dtm.new_ans, k = 2) 
lda_a2=tidy(lda_a2) 
top_terms_a2 <- lda_a2 %>%  
  group_by(topic) %>%  
  top_n(10, beta) %>%  
  ungroup() %>%  
  arrange(topic, -beta)

top_terms_a2 %>%  
  mutate(term = reorder(term, beta)) %>%  
  ggplot(aes(term, beta, fill = factor(topic))) +  geom_bar(stat = "identity", show.legend = FALSE) +  facet_wrap(~ topic, scales = "free") +  coord_flip()


#k = 8
lda_a8 <- LDA(dtm.new_ans, k = 8) 
lda_a8=tidy(lda_a8) 
top_terms_a8 <- lda_a8 %>%  
  group_by(topic) %>%  
  top_n(10, beta) %>%  
  ungroup() %>%  
  arrange(topic, -beta)

top_terms_a8 %>%  
  mutate(term = reorder(term, beta)) %>%  
  ggplot(aes(term, beta, fill = factor(topic))) +  geom_bar(stat = "identity", show.legend = FALSE) +  facet_wrap(~ topic, scales = "free") +  coord_flip()

#k = 11
lda_a11 <- LDA(dtm.new_ans, k = 11) 
lda_a11=tidy(lda_a11) 
top_terms_a11 <- lda_a11 %>%  
  group_by(topic) %>%  
  top_n(10, beta) %>%  
  ungroup() %>%  
  arrange(topic, -beta)

top_terms_a11 %>%  
  mutate(term = reorder(term, beta)) %>%  
  ggplot(aes(term, beta, fill = factor(topic))) +  geom_bar(stat = "identity", show.legend = FALSE) +  facet_wrap(~ topic, scales = "free") +  coord_flip()

#k = 14
lda_a14 <- LDA(dtm.new_ans, k = 14) 
lda_a14=tidy(lda_a14) 
top_terms_a14 <- lda_a14 %>%  
  group_by(topic) %>%  
  top_n(10, beta) %>%  
  ungroup() %>%  
  arrange(topic, -beta)

top_terms_a14 %>%  
  mutate(term = reorder(term, beta)) %>%  
  ggplot(aes(term, beta, fill = factor(topic))) +  geom_bar(stat = "identity", show.legend = FALSE) +  facet_wrap(~ topic, scales = "free") +  coord_flip()

            
  
