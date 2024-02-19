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

#read data
duplicatedata <- fread("/Users/jacobwatson/Downloads/INFO3238 Group Project/impermium_verification_labels-transformed.csv", 
                       sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?")) 

#select relevant columns
data <- duplicatedata[ , c(1:5)] 

#tokenization
text_df=data_frame(line=1:1077,text=data$Comment)
head(text_df)

tidy_text = text_df %>%  
  unnest_tokens(word, text)

data(stop_words)
tidy_text <- tidy_text %>%  
  anti_join(stop_words)

#Top 10 words
tidy_text %>%  
  count(word,sort = TRUE)

#Words w/ frequency > 75
tidy_text %>%  
  count(word, sort = TRUE) %>%  
  filter(n > 45) %>%  
  mutate(word = reorder(word, n)) %>%  
  ggplot(aes(word, n)) +  geom_bar(stat = "identity") +  xlab(NULL) +  coord_flip()

#wordcloud
tidy_text %>%  
  anti_join(stop_words) %>%  
  count(word) %>%  
  with(wordcloud(word, n, max.words = 200))

#sentimental wordcloud
tidy_text %>%  
  inner_join(get_sentiments("bing")) %>%  
  count(word, sentiment, sort = TRUE) %>%  
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%  
  comparison.cloud(colors = c("Red", "Blue"), max.words = 100)

#DA on rows 1 through 1000
data <- data[1:1000,] 

corpus <- Corpus(VectorSource(data$Comment), readerControl=list(language="en"))
dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minWordLength = 2, 
                                                 removeNumbers = TRUE, removePunctuation = TRUE, 
                                                 stemDocument = TRUE))


rowTotals <- apply(dtm , 1, sum)


dtm.new <- dtm[rowTotals> 0, ] 


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







