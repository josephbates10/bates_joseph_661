## -----------------------------------------
## loading packages
## -----------------------------------------

library(textdata)
library(tidyverse)
library(tidytext)
library(here)
library(wordcloud)

## -----------------------------------------
## loading and tidying data
## -----------------------------------------

book <- readLines(here::here("filename.txt"))

book_df <- tibble(text = book)

book_start <- grep("/*/*/* START OF",book_df$text) + 1
book_end <- grep("/*/*/* END OF",book_df$text) - 1

book_df <- book_df[book_start:book_end,]

tidy_book <- book_df %>% 
  mutate(linenumber = row_number()) %>% 
  unnest_tokens(word,text)


## -----------------------------------------
## sentiment analysis with inner join
## ----------------------------------------- 

nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

tidy_book %>% inner_join(nrc_joy) %>% count(word, sort = TRUE)

book_sentiment <- tidy_book %>% 
  inner_join(get_sentiments("bing")) %>%
  count(word, index = linenumber %/% 80, sentiment) %>% 
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(book_sentiment,
       aes(index, sentiment)) + geom_col(show.legend = FALSE)

## -----------------------------------------
## comparing the three sentiment dictionaries
## -----------------------------------------

afinn <- tidy_book %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber %/% 80) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(
  tidy_book %>% 
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  tidy_book %>% 
    inner_join(get_sentiments("nrc") %>% 
                 filter(sentiment %in% c("positive", 
                                         "negative"))
    ) %>%
    mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive", "negative")) %>% 
  count(sentiment)

get_sentiments("bing") %>% 
  count(sentiment)

## -----------------------------------------
## most common positive and negative words
## ----------------------------------------- 

bing_word_counts <- tidy_book %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>% filter(sentiment =="positive")

bing_word_counts %>% filter(sentiment =="negative")

bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

## -----------------------------------------
## Wordclouds
## ----------------------------------------- 

tidy_book %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))
