library(tidyverse)
library(tidytext)

#reading cleaned script
#clean_script <- readRDS("data/clean_script.RDS")
script <- readRDS("data/script.RDS")

Encoding(script$text) <- "windows-1252"


#changing all curly aposthropes into straight ones, this doesn't work though
extra_stopwords <- tibble(word = c("iâ", "â", "youâ", "donâ", "thatâ",
                                   "heâ", "weâ", "letâ", "itâ", "ll",
                                   "canâ", "thereâ", "ve", "whatâ",
                                   "sheâ", "didnâ"))

#unnest tokens
tidy_script <- script %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by="word") %>%
  anti_join(extra_stopwords, by="word") %>% 
  mutate(word = str_replace(word, "liâ", "li'l"))

tidy_script %>%
  count(word, sort=TRUE) %>%
  print(n=30)

tidy_script %>% 
  count(speaker, sort=TRUE) 


