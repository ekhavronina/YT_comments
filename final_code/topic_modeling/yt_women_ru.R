library(readr)
library(stringr)
library(dplyr)
library(tidytext)
library(stopwords)
library(quanteda)

setwd('C:/Users/79175/Documents/rwd/YT/women_ru')


# читаем файлы
comments <- list()

#for (i in seq(1, 6)) {
#  comments[[i]] <- read_csv(paste0(i, '.csv'))
#}

comments <- lapply(Sys.glob('*.csv'), read_csv)

df <- bind_rows(comments, .id = 'video_id')
df <- df %>% 
  select(video_id, textOriginal)


# чистим текст
df$clean <- gsub("[[:punct:]]", " ", df$textOriginal)
df$clean <- str_replace_all(df$clean, "[0-9]+", " ")
df$clean <- str_replace_all(df$clean, "[\t\\n\r]+", " ")
df$clean <- str_squish(df$clean)


# я еще не выяснила причину, по которой mystem делит слишком длинные строки, 
# поэтому пока просто убираю, чтобы избежать ошибку
df <- df %>% 
  filter(!nchar(df$clean) == 4585)

df$clean <- system2("C:/Users/79175/Documents/rwd/mystem.exe", c("-d", "-l", "-c"),
                                input = df$clean, stdout = TRUE)


# делим на токены, смотрим частотность
comments_words <- df %>% unnest_tokens(word, clean)
word_freq <- comments_words %>% count(word, sort=T)       # частотность слов

comments_bigram <- df %>% unnest_tokens(bigram, clean, token='ngrams', n=2)
bigram_freq <- comments_bigram %>% count(bigram, sort=T)  # частотность биграмов

comments_trigram <- df %>% unnest_tokens(trigram, clean, token='ngrams', n=3)
trigram_freq <- comments_trigram %>% count(trigram, sort=T)  # частотность триграмов


# смотрим, например, комментарии со словом "красивый"
df %>% filter(str_detect(df$clean, 'красивый'))

# скрипт не завершен, добавлю позже
