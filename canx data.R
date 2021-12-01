```{r}
#install.packages("tm")
#install.packages("tidytext")
library(tm)
library(tidytext)
library(readr)
library(tidyverse)
```

```{r}
canxexport <- read_csv("canxexport.csv")
bqcanxflag <- read_csv("bqcanxflag.csv")
```

```{r}
canx_ds <- canxexport %>% 
  left_join(bqcanxflag, by = "uuid") %>% 
  distinct(uuid, .keep_all = T) %>% 
  mutate(canx_reason = `We would love to know why you'd like to leave us`,
         canx_text = `Please let us know more`) %>% 
  select(cancelsurveyflag, canx_reason, canx_text, uuid) %>% 
  drop_na(canx_reason)
canx_ds
```

What percentage of users have cancelled?
```{r}
canx_ds %>% 
  count(cancelsurveyflag) %>% 
  mutate(prop = n / sum(n))
```
We can see that around 60% of users who get to the survey part of the buy flow then go on to cancel

Do they say anything different?

```{r}
canx_ds %>% 
  group_by(cancelsurveyflag, canx_reason) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n)) %>% 
  arrange(- n)
#We can see that the canx_reason is multiple choice - that means we're going to have to unnest it

canx_ds %>% 
  mutate(canx_reason = strsplit(canx_reason, ",")) %>% 
  unnest() %>% 
  group_by(cancelsurveyflag, canx_reason) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n)) %>% 
  arrange(- n) %>% 
  ggplot(aes(canx_reason, prop, fill = factor(cancelsurveyflag))) +
  geom_col(position = position_dodge()) +
  coord_flip()

```
We can see cancelled or not - most people say the came thing, and that thing is they cancel because the FT is too expensive

Let's see if there is more detail in the verbatim responses 
```{r}
canx_ds %>% 
  drop_na(canx_text) %>% 
  unnest_tokens("word", canx_text) %>% 
  distinct(word, uuid, .keep_all = T) %>% 
  anti_join(stop_words) %>% 
  mutate(stem = stemDocument(word)) %>% 
  group_by(cancelsurveyflag, word) %>% 
  summarise(n = n()) %>% 
  group_by(cancelsurveyflag) %>% 
  slice_max(order_by = n, n = 10) %>% 
  ggplot(aes(word, n)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~cancelsurveyflag)
```

