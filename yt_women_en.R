library(readr)
library(stringr)
library(dplyr)
library(tidytext)
library(stopwords)
library(quanteda)
library(mallet)
library(LDAvis)
library(servr)
library(textstem)

setwd('C:/Users/79175/Documents/rwd/YT/women_en')


# читаем файлы

women_en <- lapply(Sys.glob('*.csv'), read_csv)
women_en_df <- bind_rows(women_en, .id = 'video_id')

women_en_df <- women_en_df %>% 
  select(video_id, comment_id = id, textOriginal)


# чистим текст

women_en_df$clean <- gsub("[[:punct:]]", " ", women_en_df$textOriginal)
women_en_df$clean <- str_replace_all(women_en_df$clean, "[0-9]+", " ")
women_en_df$clean <- str_replace_all(women_en_df$clean, "[\t\\n\r]+", " ")
women_en_df$clean <- str_squish(women_en_df$clean)

# токинезация

women_en_lem <- unnest_tokens(women_en_df, word, clean)

# лемматизация

women_en_lem$lemma <- lemmatize_words(women_en_lem$word) 

# смотрим частотность

freq_word <- women_en_lem %>% count(lemma, sort=T)  # частотность слов


# комментарии со словом "красивый"
women_en_df %>% filter(str_detect(women_en_df$clean, 'beautiful'))




### тематическое моделирование

# загружаем стопслова

write_lines(stopwords("en"), "stopwords_en.txt")

mallet_instances_we <- mallet.import(id.array=women_en_df$comment_id,
                                     text.array=women_en_df$clean,
                                     stoplist="stopwords_en.txt")

topic_model_we <- MalletLDA(num.topics=20) # 20 тем
topic_model_we$loadDocuments(mallet_instances_we) 
topic_model_we$setAlphaOptimization(20, 50) 

vocabulary_we <- topic_model_we$getVocabulary() # словарь корпуса
word_freqs_we <- mallet.word.freqs(topic_model_we) # частотности

word_freqs_we %>% arrange(desc(doc.freq)) %>% head(10)

# тренируем модель 

topic_model_we$train(500)
topic_model_we$maximize(10)
doc_topics_we <- mallet.doc.topics(topic_model_we, smoothed=TRUE, normalized=TRUE)
topic_words_we <- mallet.topic.words(topic_model_we, smoothed=TRUE, normalized=TRUE)
topic_labels_we <- mallet.topic.labels(topic_model_we, topic_words_we, 5)

for (k in 1:nrow(topic_words_we)) {
  top <- paste(mallet.top.words(topic_model_we, topic_words_we[k,], 30)$words,collapse=" ")
  cat(paste(k, top, "\n"))
}

top_docs_we <- function(doc_topics_we, topic, docs, top.n=10) {
  head(docs[order(-doc_topics_we[,topic])], top.n)
}


top_docs_we(doc_topics_we, 1, women_en_df$clean)
plot(mallet.topic.hclust(doc_topics_we, topic_words_we, 0), labels=topic_labels_we)

# создаем json
doc_length_we <- str_count(women_en_df$clean, boundary("word"))
doc_length_we[doc_length_wr==0] <- 0.000001


json <- createJSON(phi=topic_words_we, 
                   theta=doc_topics_we, 
                   doc.length=doc_length_we, 
                   vocab=vocabulary_we, 
                   term.frequency=word_freqs_we$word.freq)

serVis(json, out.dir="women_en_20topics", open.browser=TRUE)


topic_labels_we[1]
top_docs_we(doc_topics_we, 1, women_en_df$clean)
