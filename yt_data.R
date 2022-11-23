library(readr)
library(stringr)
library(dplyr)
library(tidytext)
library(stopwords)
library(quanteda)

setwd('C:/Users/79175/Documents/rwd/YT')

# примеры видео
yt_women <- list()
yt_women <- c(yt_women, 'https://www.youtube.com/c/onashemoglavnom/videos',
              'https://www.youtube.com/c/CodingGirlBlog/videos',
              'https://www.youtube.com/c/miracl6/videos',
              'https://www.youtube.com/c/%D0%A0%D0%BE%D1%81%D0%BB%D0%BE%D0%B2%D0%B5%D1%86/videos')


# читаем файлы
comments <- list()

for (i in seq(1, 6)) {
  comments[[i]] <- read_csv(paste0(i, '.csv'))
}

df <- bind_rows(comments, .id = 'video_id')
df <- df %>% select(video_id, textOriginal)


# чистим текст
df$clean <- gsub("[[:punct:]]", " ", df$textOriginal)
df$clean <- str_replace_all(df$clean, "[0-9]+", " ")
df$clean <- str_replace_all(df$clean, "[\t\\n\r]+", " ")
df$clean <- str_squish(df$clean)

df$clean <- system2("C:/Users/79175/Documents/rwd/mystem.exe", c("-d", "-l", "-c"),
                                input = df$clean, stdout = TRUE)


# делим на токены, смотрим частотность
comments_words <- df %>% unnest_tokens(word, clean)
word_freq <- comments_words %>% count(word, sort=T)       # частотность слов

comments_bigram <- df %>% unnest_tokens(bigram, clean, token='ngrams', n=2)
bigram_freq <- comments_bigram %>% count(bigram, sort=T)  # частотность биграмов
