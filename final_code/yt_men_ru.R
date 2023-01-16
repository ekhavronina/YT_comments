library(readr)
library(stringr)
library(dplyr)
library(tidytext)
library(stopwords)
library(quanteda)

setwd('C:/Users/79175/Documents/rwd/YT/men_ru')


# читаем файлы
#comments <- list()

#for (i in seq(1, 6)) {
#  comments[[i]] <- read_csv(paste0(i, '.csv'))
#}

men_ru <- lapply(Sys.glob('*.csv'), read_csv)

men_ru_df <- bind_rows(men_ru, .id = 'video_id')
men_ru_df <- men_ru_df %>% 
  select(id, video_id, textOriginal)


# чистим текст
men_ru_df$clean <- gsub("[[:punct:]]", " ", men_ru_df$textOriginal)
men_ru_df$clean <- str_replace_all(men_ru_df$clean, "[0-9]+", " ")
men_ru_df$clean <- str_replace_all(men_ru_df$clean, "[\t\\n\r]+", " ")
men_ru_df$clean <- str_squish(men_ru_df$clean)


# я еще не выяснила причину, по которой mystem делит слишком длинные строки, 
# поэтому пока просто убираю, чтобы избежать ошибку

rem <- men_ru_df %>% slice_max(nchar(clean), n=2)
men_ru_df <- anti_join(men_ru_df, rem, by = 'id')

men_ru_df$clean <- system2("C:/Users/79175/Documents/rwd/mystem.exe", c("-d", "-l", "-c"),
                    input = men_ru_df$clean, stdout = TRUE)


# делим на токены, смотрим частотность
comments_words <- men_ru_df %>% unnest_tokens(word, clean)
freq_word <- comments_words %>% count(word, sort=T)       # частотность слов

comments_bigram <- men_ru_df %>% unnest_tokens(bigram, clean, token='ngrams', n=2)
freq_bigram <- comments_bigram %>% count(bigram, sort=T)  # частотность биграмов

comments_trigram <- men_ru_df %>% unnest_tokens(trigram, clean, token='ngrams', n=3)
freq_trigram <- comments_trigram %>% count(trigram, sort=T)  # частотность триграмов


# смотрим комментарии со словом "красивый"
men_ru_df %>% filter(str_detect(men_ru_df$clean, 'красивый'))
