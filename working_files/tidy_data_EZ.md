read data from Indeed
================

read data (existing)
--------------------

``` r
#datascience <- read_csv("./data/datascience_market/alldata.csv") %>% 
  #filter(!is.na(position))

datascience <- read_csv("./data/ds_500.csv")
  # filter(!is.na(position))
# head(datascience)
# anyNA(datascience$position)
```

minimum requirement of degree
-----------------------------

``` r
pattern_hi = "[Hh]igh [Ss]chool"
pattern_ba = "[Bb]achelor | \\bB\\.?A\\b | \\bB\\.?S\\b | [Cc]ollege | [Dd]egree"
pattern_ma = "[Mm]aster[^y] | [Aa]dvanced | \\bM\\.?[SA]\\b | [Gg]raduate"
pattern_phd = "\\b[Pp][Hh]\\.?[Dd]\\b | \\bM\\.?D\\b"
```

stacked bar chart

``` r
# datascience %>% 
#   mutate(degree = ifelse(str_detect(.$description, pattern_Hi) == TRUE, "high school",
#                       ifelse(str_detect(.$description, pattern_Ba) == TRUE, "bachelor",
#                           ifelse(str_detect(.$description, pattern_Ma) == TRUE, "master",
#                                ifelse(str_detect(.$description, pattern_Phd) == TRUE, "phd", "other"))))) %>% 
#   mutate(degree = factor(degree, levels = c("high school", "bachelor", "master", "phd", "other")),
#          flag = as.factor(flag)) %>% 
#   # ggplot(aes(x = degree)) + geom_bar(aes(fill = flag))
#   count(flag, degree) %>% 
#   mutate(flag = ifelse(flag == 1, "top500", "non_top500")) %>% 
#   spread(key = flag, value = n) %>% 
#   plot_ly(x = ~degree, y = ~top500, type = "bar", name = "top500") %>% 
#   add_trace(y = ~non_top500, name = "non top500") %>% 
#   layout(yaxis = list(title = 'Count'), barmode = 'stack')

datascience_OR <- datascience %>% 
  mutate(high_school = ifelse(str_detect(.$description, pattern_hi) == TRUE, 1, 0),
         bachelor = ifelse(str_detect(.$description, pattern_ba) == TRUE, 1, 0),
         master = ifelse(str_detect(.$description, pattern_ma) == TRUE, 1, 0), 
         phd = ifelse(str_detect(.$description, pattern_phd) == TRUE, 1, 0), 
         other = high_school + bachelor + master + phd) %>% # if other == 0, means non degree has been found
  mutate(flag = ifelse(flag == 1, "top500", "non_top500"), 
         flag = as.factor(flag), 
         other = ifelse(other == 0, 1, 0)) %>% # if other == 0, means non degree has been found
  gather(key = degree, value = indicator, high_school:other) %>% 
  count(flag, degree, indicator != 0) %>% 
  filter(`indicator != 0` == "TRUE") %>% 
  select(-`indicator != 0`) %>% 
  spread(key = flag, value = n)

datascience_OR %>% 
  mutate(
    non_top500_odds = (non_top500) / sum(datascience$flag == 0),
    top500_odds  = (top500) / sum(datascience$flag == 1),
    log_OR = log(top500_odds / non_top500_odds)
  ) %>% 
  mutate(pos_log_OR = ifelse(log_OR > 0, "top500 > non500", "non500 > top500")) %>% 
  mutate(degree = fct_reorder(degree, log_OR)) %>%
  ggplot(aes(degree, log_OR, fill = pos_log_OR)) +
  geom_col() +
  coord_flip() +
  ylab("log odds ratio") +
  scale_fill_discrete(name = "")
```

![](tidy_data_EZ_files/figure-markdown_github/unnamed-chunk-3-1.png)

tree map for related background
-------------------------------

``` r
pattern_bg = c("[Cc]omputer [Ss]cience | \\bC\\.?S\\b | [Mm]achine [Ll]earning | \\bM\\.?L\\b", 
                 "[Ss]tatistic", 
                 "[Mm]ath", 
                 "[Qq]uantitative", 
                 "[Ee]conomic", 
                 "[Bb]iolog", 
                 "[Bb]iostatis", 
                 "[Dd]ata [Ss]cience | \\bD\\.?S\\b", 
                 "[Cc]hemical [Ee]ngineering")
name_bg = c("CS", 
          "Statistics", 
          "Mathematics", 
          "Quantitative", 
          "Economics", 
          "Biology", 
          "Biostatistics", 
          "DS", 
          "Engineer")

bg_freq = data.frame(
  background = pattern_bg, 
  index = name_bg, 
  freq = rep(0, length(pattern_bg))
)

for (i in c(1:length(pattern_bg))) {
  bg_freq$freq[i] = sum(str_detect(datascience$description, as.character(bg_freq$background[i])))
}

bg_freq %>% 
  treemap(index = "index", 
          vSize = "freq", 
          type = "index",
          palette = "Blues")
```

![](tidy_data_EZ_files/figure-markdown_github/unnamed-chunk-4-1.png)

word frequency count
--------------------

Took 100 samples from 7000

``` r
datascience <- mutate(datascience, index = 1:nrow(datascience))
set.seed(1)
sample1 <- sample(1:nrow(datascience), 100, replace = FALSE)
sample1
```

    ##   [1] 1847 2588 3982 6313 1402 6243 6563 4590 4370  430 1431 1226 4769 2666
    ##  [15] 5342 3454 4979 6880 2636 5391 6481 1471 4517  871 1852 2675   93 2649
    ##  [29] 6023 2357 3338 4151 3416 1289 5725 4625 5494  747 5005 2844 5676 4473
    ##  [43] 5411 3822 3660 5453  162 3296 5057 4783 3298 5945 3024 1690  488  687
    ##  [57] 2182 3577 4565 2805 6293 2024 3164 2291 4484 1778 6906 5277  581 6026
    ##  [71] 2334 5778 2386 2297 3277 6137 6902 2682 5345 6604 2988 4897 2749 2236
    ##  [85] 5201 1393 4884  836 1686  984 1645  405 4407 6012 5343 5468 3122 2812
    ##  [99] 5559 4147

``` r
data_100 <- datascience[sample1,]
```

we’ll un-nest the tokens (i.e. words) in each description; the result is a tidy dataset in which each word is contained within a separate row.

word frequency in description

-   Single word

``` r
data(stop_words)
keep_letter_stop_words <- stop_words %>% filter(!word %in% c("C", "c", "R", "r"))

inspection_words_single = 
  datascience %>% 
  unnest_tokens(word, description) %>% 
  anti_join(x = ., keep_letter_stop_words)

inspection_words_single %>% 
  count(word, sort = TRUE) %>% 
  top_n(100) %>% 
  mutate(word = fct_reorder(word, n)) %>% 
  ggplot(aes(x = word, y = n)) + 
  geom_bar(stat = "identity", fill = "blue", alpha = .6) + 
  labs(y = "single world frequency") +
  coord_flip()
```

![](tidy_data_EZ_files/figure-markdown_github/unnamed-chunk-6-1.png)

-   Double word

``` r
data(stop_words)
keep_letter_stop_words <- stop_words %>% filter(!word %in% c("C", "c", "R", "r"))

inspection_words = 
  datascience %>% 
  unnest_tokens(word, description) %>% 
  anti_join(x = ., keep_letter_stop_words)

inspection_words %>%
  nest(word) %>%
  mutate(text = map(data, unlist), 
         text = map_chr(text, paste, collapse = " ")) %>% 
  select(-data) %>% 
  unnest_tokens(word, text, token = "ngrams", n = 2) %>% 
  count(word, sort = TRUE) %>% 
  top_n(50) %>% 
  mutate(word = fct_reorder(word, n)) %>% 
  ggplot(aes(x = word, y = n)) + 
  geom_bar(stat = "identity", fill = "blue", alpha = .6) + 
  labs(y = "double world frequency") +
  coord_flip()
```

![](tidy_data_EZ_files/figure-markdown_github/unnamed-chunk-7-1.png)

word cloud

``` r
word_cloud2 <- inspection_words %>%
  nest(word) %>%
  mutate(text = map(data, unlist), 
         text = map_chr(text, paste, collapse = " ")) %>% 
  select(-data) %>% 
  unnest_tokens(word, text, token = "ngrams", n = 2) %>% 
  count(word, sort = TRUE) %>% 
  top_n(50) 

wordcloud(words = word_cloud2$word, freq = word_cloud2$n, random.order=FALSE,
          rot.per=0.35, colors=brewer.pal(8, "Dark2"))
```

![](tidy_data_EZ_files/figure-markdown_github/unnamed-chunk-8-1.png)

-   Three words

``` r
inspection_words %>%
  nest(word) %>%
  mutate(text = map(data, unlist), 
         text = map_chr(text, paste, collapse = " ")) %>% 
  select(-data) %>% 
  unnest_tokens(word, text, token = "ngrams", n = 3) %>% 
  count(word, sort = TRUE) %>% 
  top_n(50) %>% 
  mutate(word = fct_reorder(word, n)) %>% 
  ggplot(aes(x = word, y = n)) + 
  geom_bar(stat = "identity", fill = "blue", alpha = .6) + 
  labs(y = "triple world frequency") +
  coord_flip()
```

![](tidy_data_EZ_files/figure-markdown_github/unnamed-chunk-9-1.png)

-   four words

``` r
inspection_words %>%
  nest(word) %>%
  mutate(text = map(data, unlist), 
         text = map_chr(text, paste, collapse = " ")) %>% 
  select(-data) %>% 
  unnest_tokens(word, text, token = "ngrams", n = 4) %>% 
  count(word, sort = TRUE) %>% 
  top_n(50) %>% 
  mutate(word = fct_reorder(word, n)) %>% 
  ggplot(aes(x = word, y = n)) + 
  geom_bar(stat = "identity", fill = "blue", alpha = .6) + 
  labs(y = "quadruple world frequency") +
  coord_flip()
```

![](tidy_data_EZ_files/figure-markdown_github/unnamed-chunk-10-1.png)

remove "equality"
-----------------

``` r
word = c("race", 
         "age", 
         "religion", 
         "color", 
         "equal", 
         "opportunity", 
         "employer",
         "employment", 
         "sex", 
         "gender", 
         "sexual", 
         "applicant",
         "applicants", 
         "qualification", 
         "qualifications", 
         "qualified", 
         "candidate", 
         "national", 
         "regard", 
         "identity", 
         "veteran", 
         "orientation", 
         "criminal", 
         "minority", 
         "marital", 
         "description")
eq_com = data.frame(
  word = word,
  lexicon = rep("SMART", length(word))
)

keep_letter_stop_words = 
  keep_letter_stop_words %>% 
  full_join(eq_com, key = "word")
```

``` r
inspection_words = 
  datascience %>% 
  unnest_tokens(word, description) %>% 
  anti_join(x = ., keep_letter_stop_words)

inspection_words %>%
  nest(word) %>%
  mutate(text = map(data, unlist), 
         text = map_chr(text, paste, collapse = " ")) %>% 
  select(-data) %>% 
  unnest_tokens(word, text, token = "ngrams", n = 2) %>% 
  count(word, sort = TRUE) %>% 
  top_n(50) %>% 
  mutate(word = fct_reorder(word, n)) %>% 
  ggplot(aes(x = word, y = n)) + 
  geom_bar(stat = "identity", fill = "blue", alpha = .6) + 
  labs(y = "double world frequency") +
  coord_flip()
```

![](tidy_data_EZ_files/figure-markdown_github/unnamed-chunk-12-1.png)

``` r
inspection_words %>%
  nest(word) %>%
  mutate(text = map(data, unlist), 
         text = map_chr(text, paste, collapse = " ")) %>% 
  select(-data) %>% 
  unnest_tokens(word, text, token = "ngrams", n = 3) %>% 
  count(word, sort = TRUE) %>% 
  top_n(50) %>% 
  mutate(word = fct_reorder(word, n)) %>% 
  ggplot(aes(x = word, y = n)) + 
  geom_bar(stat = "identity", fill = "blue", alpha = .6) + 
  labs(y = "triple world frequency") +
  coord_flip()
```

![](tidy_data_EZ_files/figure-markdown_github/unnamed-chunk-13-1.png)

### Comparing words across groups
