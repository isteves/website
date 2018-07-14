library(tidyverse)
library(here)
library(tidytext)
library(wordcloud)

atlantic <- readLines("https://raw.githubusercontent.com/isteves/website/master/static/post/2018-07-12-rats-to-reefs/ratsreef-atlantic.txt")
bbc <- readLines("https://raw.githubusercontent.com/isteves/website/master/static/post/2018-07-12-rats-to-reefs/ratsreef-bbc.txt")
nature <- readLines("https://raw.githubusercontent.com/isteves/website/master/static/post/2018-07-12-rats-to-reefs/ratsreef-nature.txt")


bbc_words <- tibble(source = "bbc",
                    text = bbc) %>% 
    mutate(text = str_replace_all(text, "â", "'")) %>% #garbled quotes
    unnest_tokens(word, text) 

atlantic_words <- tibble(source = "atlantic",
                         text = atlantic) %>% 
    mutate(text = str_replace_all(text, "â", "'")) %>% #garbled quotes
    unnest_tokens(word, text)

nature_words <- tibble(source = "nature",
                       text = nature) %>% 
    mutate(text = str_replace_all(text, "â", "'"), #garbled quotes
           text = str_replace_all(text, "[0-9]", " ")) %>% #footnotes/etc
    unnest_tokens(word, text) 

all_words <- bind_rows(bbc_words, atlantic_words, nature_words) %>% 
    anti_join(stop_words, by = "word") %>% 
    filter(!str_detect(word, "[^a-zA-Z]"),
           word != "â") %>% #garbled quotes
    group_by(source) %>% 
    mutate(word = str_replace(word, "s$", "")) %>% #plural quick fix
    count(word) %>% 
    filter(n >= 2) %>% 
    spread(source, n, fill = 0) 

bind_rows(bbc_words, atlantic_words) %>% 
    anti_join(stop_words, by = "word") %>% 
    filter(!str_detect(word, "[^a-zA-Z]"),
           word != "â") %>% 
    group_by(source) %>% 
    mutate(word = str_replace(word, "s$", "")) %>% #plural quick fix
    count(word) %>% 
    filter(n > 1) %>% 
    acast(word ~ source, value.var = "n", fill = 0) %>% 
    comparison.cloud(colors = c("cadetblue", "dodgerblue4"),
                     max.words = 100)

all_words %>% arrange(desc(bbc), desc(atlantic)) 
all_words %>% arrange(desc(atlantic), desc(bbc))



all_words %>% 
    filter(word != "rat" & word != "reef" & word != "island") %>%
    with(wordcloud(word, atlantic, max.words = 100))
#guano
#nutrients
#fertilize islands with valuable nitrogen
#nitrogen 
#guano is also rich in phosphorus

all_words %>% 
    filter(word != "rat" & word != "reef" & word != "island") %>%
    with(wordcloud(word, bbc, max.words = 100))
#bird droppings 
#natural reef fertilizer
#guano
#seabird droppings
#fertilise
#deposit rich nutrients
#nutrients x2

#eradicate

all_words %>% 
    filter(word != "rat" & word != "reef" & word != "island") %>%
    with(wordcloud(word, nature, max.words = 40))



all_words %>% 
    summarize(atlantic = sum(atlantic),
              bbc = sum(bbc))
