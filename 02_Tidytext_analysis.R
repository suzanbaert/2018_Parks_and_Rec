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
                                   "sheâ", "didnâ", "wasnâ", "hey", "yeah"))

#unnest tokens
tidy_script <- script %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by="word") %>%
  anti_join(extra_stopwords, by="word") %>% 
  mutate(word = str_replace(word, "liâ", "li'l"))


#top words by episode
tidy_script %>%
  count(episode, word, sort=TRUE) %>%
  group_by(episode) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(x = word, y = n, fill = episode))+
  geom_col() + 
  facet_wrap(~episode, scales = "free_y") +
  coord_flip()


#top words by episode
tidy_script %>%
  filter(speaker %in% c("LESLIE", "TOM", "ANN", "RON", "APRIL", "BEN")) %>%
  count(speaker, word, sort=TRUE) %>%
  group_by(speaker) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(x = word, y = n, fill = speaker))+
  geom_col() + 
  facet_wrap(~speaker, scales = "free_y") +
  coord_flip()



###♥ AFINN
script %>%
  unnest_tokens(word, text) %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>% 
  ungroup() %>% 
  ggplot(aes(x=word, y=n, fill=sentiment)) +
  geom_col() +
  facet_wrap(~ sentiment, scales="free_y")+
  coord_flip()



### AFINN LESLIE
script %>%
  filter(speaker == "LESLIE") %>% 
  unnest_tokens(word, text) %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>% 
  ungroup() %>% 
  ggplot(aes(x=word, y=n, fill=sentiment)) +
  geom_col() +
  facet_wrap(~ sentiment, scales="free_y")+
  coord_flip()



### ALL NRC
script %>%
  unnest_tokens(word, text) %>% 
  inner_join(get_sentiments("nrc")) %>% 
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>% 
  ungroup() %>% 
  ggplot(aes(x=word, y=n, fill=sentiment)) +
  geom_col() +
  facet_wrap(~ sentiment, scales="free_y")+
  coord_flip()


### ALL NRC
script %>%
  unnest_tokens(word, text) %>% 
  inner_join(get_sentiments("nrc")) %>% 
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>% 
  ungroup() %>% 
  ggplot(aes(x=word, y=n, fill=sentiment)) +
  geom_col() +
  facet_wrap(~ sentiment, scales="free_y")+
  coord_flip()


reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}



script <- readRDS("data/script.RDS")


# by episode
script %>%
  unnest_tokens(word, text) %>% 
  count(episode, word, sort=TRUE) %>% 
  bind_tf_idf(word, episode, n) %>% 
  group_by(episode) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word = reorder_within(word, tf_idf, episode)) %>% 
  ggplot(aes(word, tf_idf, fill = episode)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ episode, scales = "free_y", ncol = 3) +
  scale_x_reordered() +
  coord_flip()




#by speaker
script %>%
  filter(speaker %in% c("LESLIE", "TOM", "ANN", "RON", "APRIL", "BEN", "ANDY")) %>%
  unnest_tokens(word, text) %>% 
  count(speaker, word, sort=TRUE) %>% 
  bind_tf_idf(word, speaker, n) %>% 
  group_by(speaker) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word = reorder_within(word, tf_idf, speaker)) %>% 
  ggplot(aes(word, tf_idf, fill = speaker)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ speaker, scales = "free", ncol = 3) +
  scale_x_reordered() +
  coord_flip()


total_words_by_speaker <- tidy_script %>%
  count(speaker, sort = TRUE) %>%
  rename(total = n)



tidy_script %>%
  filter(speaker %in% c("LESLIE", "TOM", "ANN", "RON", "APRIL", "BEN", "ANDY")) %>%
  group_by(speaker) %>% 
  inner_join(get_sentiments("bing"), by="word") %>% 
  group_by(speaker, sentiment) %>% 
  left_join(total_words_by_speaker, by="speaker") %>% 
  count(speaker, word, sentiment, total, sort=TRUE)

