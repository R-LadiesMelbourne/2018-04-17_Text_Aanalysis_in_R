install.packages("dplyr")
install.packages("tidytext")
install.packages("purrr")
install.packages("stringi")

install.packages("ggplot2")

install.packages("wordcloud")
install.packages("topicmodels")
install.packages("tm")

library(tidyverse)
library(dplyr) #we are going to use piping = my personal favourite
# text mining library
library(tidytext)
library(purrr)



#remove all variables 
rm(list=ls(all=TRUE))

#setwd("C:/Users/masha/OneDrive - Victoria University/research/Text Analysis")

#setup the location of the files
all_txt <- list.files(path="Facebook newsroom", pattern = ".txt$", full.names=TRUE)


#lets read all txt files in the folder to a dataframe/tibble
text<-map_df(all_txt, ~ data_frame(txt = paste(readLines(file(.x)), collapse=" ")) %>% 
               mutate(filename = .x))


write.csv(text, file="data/newsroom_text.csv", row.names=FALSE)

#-------------------------Pre-processing

#reload text if you do not have it!
text<-read.csv("newsroom_text.csv")

#lets tidy up!

replace_reg <- "https://[A-Za-z\\d]+|http://[A-Za-z\\d]"
#remove_numbers<-"\\b\\d[^,]*(?:,|$)"
reg_words <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"

tidy_news<-text %>% 
  # removes link
  mutate(txt = str_replace_all(txt, replace_reg, "")) %>%
  #count number of words in each news 
  mutate(NWords=str_count(txt)) %>%
  
  #to unnest and lower case, remove numbers
  unnest_tokens(word, txt, drop=TRUE, token = "regex", pattern = reg_words, to_lower = TRUE) %>% 
  #remove stop words
  filter(str_detect(word, "[a-z']$"), !word %in% stop_words$word) 

write.csv(tidy_news, file="tidy_news.csv", row.names=FALSE)

# let's load file 2 - Transcript.txt

transcript<-read_lines(file="Transcript.txt")
transcript_df <- tibble(line = seq_along(transcript),
                  text = transcript)

#let's clean up the Transcript.txt
tidy_transcript<-transcript_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) 

write.csv(tidy_transcript, file="tidy_transcript.csv", row.names=FALSE)

#--------------------Word frequencies and word clouds
library(ggplot2)
fillColor = "#FFA07A"

#you can reload tidy_news.csv 
tidy_transcript<-read.csv("tidy_transcript.csv")

tidy_news<-read.csv("tidy_news.csv")

#let's identify top 20 words in the news files
tidy_news %>% count(word,sort = TRUE) %>%
 # ungroup()
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  head(20) %>%

  
#and show them on a graph
  
  ggplot(aes(x = word,y = n)) +
  geom_bar(stat='identity',colour="white", fill =fillColor) +
  geom_text(aes(x = word, y = 1, label = paste0("(",n,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Word', y = 'Word Count', 
       title = 'Top 20 most Common Words') +
  coord_flip() + 
  theme_bw() 

#we can do the same for the transcript file as well!
  tidy_transcript %>% count(word, sort = TRUE) %>%
    mutate(word = factor(word, levels = rev(unique(word)))) %>%
    top_n(30) %>%
    
  #and show them on a graph
  
  ggplot(aes(x = word,y = n)) +
    geom_bar(stat='identity',colour="white", fill =fillColor) +
    geom_text(aes(x = word, y = 1, label = paste0("(",n,")",sep="")),
              hjust=0, vjust=.5, size = 4, colour = 'black',
              fontface = 'bold') +
    labs(x = 'Word', y = 'Word Count', 
         title = 'Top 30 most Common Words') +
    coord_flip() + 
    theme_bw() 
  
  
#now let's do the word cloud
library('wordcloud')

tidy_news %>% count(word,sort = TRUE) %>%
  ungroup()  %>%
  head(50) %>%
  with(wordcloud(word, n, max.words = 50,colors=brewer.pal(8, "Dark2")))


tidy_transcript %>% count(word,sort = TRUE) %>%
  ungroup()  %>%
  head(50) %>%
  with(wordcloud(word, n, max.words = 50,colors=brewer.pal(8, "Dark2")))

#--------------------Sentiment analysis
library(dplyr)
library(stringr)

#reload tidy_news if you do not have it!
#make sure you reload tidy_news.csv files (=preprocessed text), not newsroom_text.csv (=raw)

tidy_news<-read.csv("tidy_news.csv")

#positive vs non-positive news
#AFINN dictionary
news_sentiments <- tidy_news %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(filename) %>%
  mutate(scoreAFINN = score) %>%
  arrange(desc(scoreAFINN)) %>%
  ungroup()

ggplot(news_sentiments,aes(x=filename,fill=factor(score))) + geom_bar(geom="dodge",position="fill") + coord_flip()

#--------------------Topic Modelling
library(topicmodels)
library(tm)


#reload text if you do not have it!
#make sure you reload newsroom_text.csv (=raw file) - we are going to use tm package to pre-process it
text<-read.csv("newsroom_text.csv")


#let's use tm package!
# Pre-process data
corpus = Corpus(VectorSource(text$txt))

corpus <- tm_map(corpus, tolower)

corpus <- tm_map(corpus, removePunctuation)

corpus <- tm_map(corpus, removeWords, stopwords("english"))

corpus <- tm_map(corpus, removeWords, UniqueLowIDF[1:500])

corpus <- tm_map(corpus, stemDocument)

dtm = DocumentTermMatrix(corpus)

# Remove sparse terms
dtm = removeSparseTerms(dtm, 0.997)

# Create data frame
labeledTerms = as.data.frame(as.matrix(dtm))

labeledTerms = labeledTerms[rowSums(abs(labeledTerms)) != 0,]

##############################################################################
#LDA Modelling Starts
###############################################################################

# set a seed so that the output of the model is predictable
news_lda <- LDA(labeledTerms, k = 7, control = list(seed = 13))

#The tidytext package provides this method for extracting the per-topic-per-word probabilities, 
# called   β  (“beta”), from the model
news_topics <- tidy(news_lda, matrix = "beta")

news_top_terms <- news_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

news_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() + theme_bw()





