

library(tidytext)
library(ggThemeAssist)
library(tm)
library(tidyverse)

# Text analysis

## data has few text columns. Lets check a couple of them

################ ranson column #########################


df_ransom <- df %>% 
  filter(!is.na(ransomnote)) %>% 
  filter(ransomnote != "") %>% 
  select(ransomnote)

## few records with notes for ransom

txt_ran <- tibble(text = df_ransom$ransomnote)
txt_ran %>% 
  unnest_tokens(output = word,
                input = text,
                token = "words",
                to_lower = TRUE) -> txt_unnest

# word frequency

txt_unnest %>% 
  group_by(word) %>% 
  summarise(cnt = n()) %>% 
  arrange(-cnt)

## most of the words are stop words and need to remove


txt_unnest %>% 
  anti_join(stop_words) %>% 
  group_by(word) %>% 
  summarise(cnt = n()) %>% 
  arrange(-cnt) -> txt_wordcloud

txt_unnest %>% 
  anti_join(stop_words) %>% 
  group_by(word) %>% 
  summarise(cnt = n()) %>% 
  arrange(-cnt) %>%
  top_n(10) %>% 
  ggplot(aes(reorder(word,cnt),cnt, fill = cnt)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Common words in ransom", x = "words", y = "count") +
  coord_flip() -> try

## using "ggThemeAssist" package to GUI based theme selection
try + theme(plot.subtitle = element_text(vjust = 1), 
    plot.caption = element_text(vjust = 1), 
    panel.background = element_rect(fill = NA, 
        colour = "antiquewhite1"), plot.background = element_rect(size = 0.2))

wordcloud::wordcloud(txt_wordcloud$word, txt_wordcloud$cnt,
                     min.freq = 5)

txt_wordcloud %>% mutate(document = rep(1,nrow(txt_wordcloud))) -> txt_wordcloud

# tidy to TDM

txt_wordcloud %>% 
  cast_dtm(document = document, term = word, value = cnt) -> txt_dtm
inspect(txt_dtm)

## other approach


df_dtm <- VectorSource(df_ransom) %>% 
  VCorpus()

clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  #corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, 
                   c(stopwords("en"), "where", "can", "i", "get", stopwords("SMART")))
  #corpus1 <- corpus
  corpus <- tm_map(corpus, stemDocument)
  #corpus <- tm_map(corpus, stemCompletion, corpus1)
  return(corpus)
}

df_dtm <- clean_corpus(df_dtm)
df_dtm <- TermDocumentMatrix(df_dtm)

inspect(df_dtm)
terms <- Terms(df_dtm)
head(terms)

##################  summary column #################################


df_sum <- df %>% 
  filter(!summary == "") %>% 
  filter(!is.na(summary)) %>% 
  select(summary)

head(df_sum,1)
## summary column contains dates, numbers, and common redundant info.

# lets take other appoarch of DTM first and then tidy data

df_sum$summary <- str_replace_all(df_sum$summary,"[^[:graph:]]"," ")
df_sum$summary <- gsub("\\:","",df_sum$summary)
df_sum$summary <- gsub("\\/","",df_sum$summary)
df_sum$summary <- gsub("\\.","",df_sum$summary)
df_sum$summary <- gsub("\\,","",df_sum$summary)
df_sum$summary <- gsub("  "," ",df_sum$summary)
df_sum$summary <- gsub("\\&","",df_sum$summary)
df_sum$summary <- gsub("\\$","",df_sum$summary)
df_sum$summary <- gsub("\\-","",df_sum$summary)

# almost clear

df_sum_tidy <- tibble(text = df_sum$summary)
df_sum_tidy %>% 
  unnest_tokens(output = word,
                input = text,
                token = "words",
                to_lower = TRUE) -> sum_unnest

sum_unnest %>% 
  anti_join(stop_words) %>% 
  group_by(word) %>% 
  summarise(cnt = n()) %>% 
  arrange(-cnt) %>% 
  filter(!str_detect(word,"[0-9]")) -> sum_clear
rm(sum_unnest)

sum_clear %>% 
  ggplot(aes(reorder(word,cnt),cnt, fill = cnt)) +
  geom_bar(stat = "identity") +
  labs(title = "Most frequent words", x = "words", y = "count")+
  coord_flip()

wordcloud::wordcloud(sum_clear$word, sum_clear$cnt)


##################  Motive column (india) #################################

df_mot <- df %>% 
  filter(country_txt == "India")%>% 
  filter(!motive == "", !motive == "Unknown") %>% 
  filter(!is.na(motive)) %>% 
  select(motive)

head(df_mot,5)

df_mot$motive <- str_replace_all(df_mot$motive,"[^[:graph:]]"," ")
df_mot$motive <- gsub("\\;","",df_mot$motive)
df_mot$motive <- gsub("\\,","",df_mot$motive)
df_mot$motive <- gsub("\\:","",df_mot$motive)
df_mot$motive <- gsub("\\/","",df_mot$motive)
df_mot$motive <- gsub("\\$","",df_mot$motive)
df_mot$motive <- gsub("\\&","",df_mot$motive)
df_mot$motive <- gsub("\\.","",df_mot$motive)
df_mot$motive <- gsub("[[:punct:]]","",df_mot$motive)


df_mot_tidy <- tibble(text = df_mot$motive)
df_mot_tidy %>% 
  unnest_tokens(output = word,
                input = text,
                token = "words") ->mot_tidy

# remove stopwords and numbers

mot_tidy %>% 
  anti_join(stop_words) %>% 
  group_by(word) %>% 
  summarise(cnt = n()) %>% 
  arrange(-cnt) %>% 
  filter(!str_detect(word, "[0-9]")) -> mot_india

wordcloud::wordcloud(mot_india$word, mot_india$cnt, min.freq = 100)


# 2-grams in motive

df_mot_tidy %>% 
  unnest_tokens(output = word,
                input = text,
                token = "ngrams", n = 2) -> mot_gram2

mot_gram2 %>% 
  separate(word,c("w1","w2")) %>% 
  filter(!w1 %in% stop_words$word,
         !w2 %in% stop_words$word) %>% 
  group_by(w1,w2) %>% 
  summarise(cnt = n()) %>% 
  unite("word", c(w1,w2), sep = " ") %>% 
  filter(!str_detect(word, "[0-9]")) %>% 
  arrange(-cnt) %>% 
  top_n(10) %>% 
  ggplot(aes(reorder(word,cnt),cnt,fill = cnt))+
  geom_bar(stat = "identity", show.legend = FALSE)+
  labs(title = "most common 2-grams", x = "words", y = "count")+
  coord_flip()+
  theme_minimal()
## specific motive  
  
  
  
  
  


