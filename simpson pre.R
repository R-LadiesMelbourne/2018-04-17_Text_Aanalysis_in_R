Scripts = read_csv("C:/Users/masha/OneDrive - Victoria University/R ladies/the-simpsons-by-the-data/simpsons_script_lines.csv")

Characters = read_csv("C:/Users/masha/OneDrive - Victoria University/R ladies/the-simpsons-by-the-data/simpsons_characters.csv")

Scripts$character_id = as.integer(Scripts$character_id)

ScriptsCharacters = left_join(Scripts,Characters,
                              by = c("character_id" = "id") )

ScriptsCharacters = ScriptsCharacters %>% 
  filter(!is.na(name))

write.csv(ScriptsCharacters, "ScriptsCharacters.csv", row.names=FALSE)

#-------------------------------
#1

SC = ScriptsCharacters 

SC %>%
  unnest_tokens(word, normalized_text) %>%
  filter(!word %in% stop_words$word) %>%
  count(word,sort = TRUE) %>%
  ungroup() %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  head(20) %>%
  
  ggplot(aes(x = word,y = n)) +
  geom_bar(stat='identity',colour="white", fill =fillColor) +
  geom_text(aes(x = word, y = 1, label = paste0("(",n,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Word', y = 'Word Count', 
       title = 'Top 20 most Common Words') +
  coord_flip() + 
  theme_bw()

#-------------------------------
#2

SC %>%
unnest_tokens(word, normalized_text) %>%
  filter(!word %in% stop_words$word) %>%
  count(word,sort = TRUE) %>%
  ungroup()  %>%
  head(70) %>%
  
  with(wordcloud(word, n, max.words = 50,colors=brewer.pal(8, "Dark2")))


#--------------------------------
#3 Sentiment analysis
visualize_sentiments <- function(SCWords) {
  SCWords_sentiments <- SCWords %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(name) %>%
    summarize(score = sum(score * n) / sum(n)) %>%
    arrange(desc(score))
  
  SCWords_sentiments %>%
    mutate(name = reorder(name, score)) %>%
    ggplot(aes(name, score, fill = score > 0)) +
    geom_col(show.legend = TRUE) +
    coord_flip() +
    ylab("Average sentiment score") + theme_bw()
}


Top20Characters = head(TopCharacters,20)$name

SCWordsTop20Characters <- SC %>%
  unnest_tokens(word, normalized_text) %>%
  filter(name != "NA") %>%
  filter( name %in% Top20Characters) %>%
  count(name, word, sort = TRUE) %>%
  ungroup()

visualize_sentiments(SCWordsTop20Characters)


#--------------------------------
#4
SC = ScriptsCharacters %>%
select(id,name,normalized_text)

corpus = Corpus(VectorSource(SC$normalized_text))

# Pre-process data
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
simpsons_lda <- LDA(labeledTerms, k = 2, control = list(seed = 13))

#The tidytext package provides this method for extracting the per-topic-per-word probabilities, 
# called   β  (“beta”), from the model
simpsons_topics <- tidy(simpsons_lda, matrix = "beta")

simpsons_top_terms <- simpsons_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

simpsons_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() + theme_bw() 