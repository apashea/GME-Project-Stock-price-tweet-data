### OPTIONAL: Cleaning up beforehand ###########################################
rm(list = ls(all = TRUE))                        # Remove all previous objects
gc()                                             # Garbage collection
assign("last.warning", NULL, envir = baseenv())  # Reset list of past warnings
################################################################################


# GME (GameStop) stock data retreived from Yahoo! Finance.
library(tidyverse)
library(quantmod)
getSymbols(Symbols = "GME", src = "yahoo")
tail(GME)
# Subsetting desired date range. The following Twitter data covers these dates.
GMES <- GME["2021-03-05/2021-03-11"]

# Fixing our margins.
par(mar=c(1,1,1,1))
# FIGURE 1 : A simple plot of GME's daily OHLC prices, trade volume, and closing 
# price adjusted for corporate actions.
plot.zoo(GMES, main = "GME, Friday 3/5/2021 to Thursday 3/11/2021, retreived from Yahoo! Finance")

# convert to df for later use, including converting our Index dates to POSIXct format including trading day closing time
# (8 p.m. UTC):
GME_df <- fortify.zoo(GMES)
trading_end <- as.POSIXct(c("2021-03-05 20:00:00", "2021-03-08 20:00:00","2021-03-09 20:00:00","2021-03-10 20:00:00","2021-03-11 20:00:00"), tz = "UTC")
GME_df <- GME_df %>%
  mutate(time = trading_end, price = GME.Adjusted) %>% 
  select(time, price) %>% 
  arrange(time)


# Importing packages for using the Twitter API and extracting tweet data:
library(rtweet)
library(httpuv)
library(ROAuth)
# Install/re-install the following packages if any incompatibility errors occur.
# install.packages("glue","stringr","reactable","dplyr")
# Given rtweet's simple authentication method to sign into your API, make sure if having authorization issues
# to delete any previous .httr-oauth file from your working directory. Running various rtweet functions, such
# as search_tweets(), will prompt an authorization browser pop-up.

# Store api keys. Replace X's with your own keys via your developer account page.
api_key <- "XXXXXXXXXXXXXXXXXXXXXXX"
api_secret_key <- "XXXXXXXXXXXXXXXXXXXXXXX"
access_token <- "XXXXXXXXXXXXXXXXXXXXXXX"
access_token_secret <- "XXXXXXXXXXXXXXXXXXXXXXX"

# Authenticate via web browser
token <- create_token(
  app = "aptweetanalysis",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

# Extracting our tweets. 18,000 is the limit for the free version of Twitter's developer API.
# All tweets include the hashtag #GME. We will also include retweet extraction under
# assumption that they reinforce popular views with more spread.
# Recording our timestamp is optional. For this project, it is "2021-03-11 15:23:55."
extraction_time <- as.data.frame(Sys.time())
twts_gme <- search_tweets("#GME", n = 18000, include_rts = TRUE, lang = "en")
twts_gme_ts <- twts_gme %>% mutate(timestamp = extraction_time)


# Writing extracted tweets dataframe to a csv file for later reference/use.
twts_gme_ts_unnest <- twts_gme_ts %>% drop_na() %>% unnest(cols = c(hashtags, symbols, urls_url, urls_t.co, urls_expanded_url,
                                                      media_url, media_t.co, media_expanded_url, media_type, ext_media_url,
                                                      ext_media_t.co, ext_media_expanded_url, mentions_user_id,
                                                      mentions_screen_name, geo_coords, coords_coords, bbox_coords,
                                                      timestamp)) %>% write_csv("C:/Users/andre/Desktop/R/twitter_gme/twts_gme_ts_unnest.csv")


flatten_lists_write_csv <- function(tibble_object, file_path_name) {
  set_lists_to_chars <- function(x) {
    if(class(x) == 'list') { y <- paste(unlist(x[1]), sep='', collapse=', ') } else { y <- x  }
    return(y) }
  new_frame <- data.frame(lapply(tibble_object, set_lists_to_chars), stringsAsFactors = F)
  write.csv(new_frame, file=file_path_name)
}

flatten_lists_write_csv(twts_gme_ts,"C:/Users/andre/Desktop/R/twitter_gme/twts_gme_ts.csv")



# For this extraction, only 17,871 tweets were successfully extracted. This is common; the free Twitter API 
# limits the extraction time window to the past 7 days.
# Selecting only our tweets and adding an index.
gme_df <- twts_gme_ts %>% mutate(index = 1:17871) %>% select(index, text, created_at, timestamp)


# Optional: writing our dataframe as a csv file for future use or reference. File size will be large (~5.6 GB for this example).
write_csv(gme_df,"C:/Users/andre/Desktop/R/twitter_gme/gme_df.csv")
# Reloading our csv file
gme_df_load <- read_csv("C:/Users/andre/Desktop/R/twitter_gme/gme_df.csv")

# FIGURE 2 : Basic visualization showing #GME tweet frequency per three-hour interval.
# #GME-related tweets sharply spiked during the evening of March 10th.
ts_plot(gme_df_load, "3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #GME Twitter statuses, March 5th to 11th, 2021",
    subtitle = "Tweet counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# Textual Analysis
library(tidytext)

# Loading our stop words from the tidytext package. Stop words let us remove words
# from our data that aren't useful, e.g., "the", "a", "and".
data("stop_words")
# Creating a dataframe of our own custom stop words.
custom_stop_words <- tribble(
  ~ word, ~lexicon,
  "t.co", "CUSTOM",
  "https", "CUSTOM"
)
# Combining our new stop word list.
stop_words2 <- stop_words %>%
  bind_rows(custom_stop_words)

# Tokenizing our data (separating and treating separately each individual word 
# and how many times each is used) and removing stop words from the list.
gme_tidy <- gme_df_load %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words2)

# FIGURE 3 : Top word counts throughout tweets. Figure shows top 10.
gme_count <- gme_tidy %>%
  count(word) %>%
  arrange(desc(n))
gme_count

# FIGURE 4 : Wordcloud of top 80 words. 
library(wordcloud)
wordcloud(
  words = gme_count$word,
  freq = gme_count$n,
  max.words = 80,
  scale = c(4, 1),
  colors = c("Red","Black","Green"),
  random.order = TRUE
)

# We can infer that "amc" is discussed very often with gme. Interestingly, the 
# 78 other words top words are used relatively just as often as one another.
# We will use a simple Gibbs LDA topic model to see if there are patterns in
# their usage.

# Exploratory topic modeling
library(topicmodels)
# Creating our document-term matrix and then generating our topic model. We
# will deliberately generate four topics (k = 4).
# We set a seed for reproducibility purposes (granted that this data should
# not be redistributed per Twitter's API terms, but it can be useful for
# team developers working on the same data and in principle is good practice.)
dtm_gme <- gme_tidy %>%
  count(word, index) %>%
  cast_dtm(index, word, n)
lda_gme <- LDA(
  dtm_gme,
  k = 4,
  method = "Gibbs",
  control = list(seed = 555)
)
# Tidying the topics.
gme_topics <- lda_gme %>%
  tidy(matrix = "beta")
# FIGURE 5 : Visualizing the 4 topics, including the top 20 words of each topic.
gme_beta <- gme_topics %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  mutate(term2 = fct_reorder(term, beta))
ggplot(gme_beta, aes(x = term2, y = beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  labs(title = "Topic Modeling last 17,871 tweets, using Gibbs LDA, k = 4", subtitle = "Extracted 2021-03-11 15:23:55", x = "Words", y = "Probability of belonging to topic")

# Note that for projects like these, it is useful to visualize topic models and
# word counts to find other presumably useless words to add to our custom stop
# words. This requires scrutiny, however. For example, "i'm" in topic 4 might
# seem useless. However, this topic includes words like "buy", "short", and "stock"
# which when combined with "i'm" might imply topic 4 represents tweets where
# current or potential investors are describing their own behavior or prescribing
# investment behaviors to others. Interesting questions can be generated simply
# from checking for seemingly useless words in topic models.
# Meanwhile, topic 3 seems to represent discussion over the political or litiguous
# events in the news and elsewhere over GME.

# Extracting and storing our topic ratios from our LDA model
topic_perc <- posterior(lda_gme)$topics
# Converting to a dataframe, reordering them
topic_perc_df <- as.data.frame(topic_perc)
topic_perc_order <-topic_perc_df[order(as.numeric(rownames(topic_perc_df))),,drop=FALSE] %>% mutate(index = 1:17871)
# Renaming our topics to avoid programming errors (from 1,2,3,4 to t1,t2,t3,t4)
tnames <- c("t1","t2","t3","t4","index")
names(topic_perc_order) <- tnames
# Converting our created_at column to POSIXct
gme_df_load_t <- gme_df_load %>% mutate(time = as.POSIXct(gme_df_load$created_at, format="%a %b %d %H:%M:%S +0000 %Y", tz="UTC")) %>% arrange(time)
# Aggregating together for later use
gme_df_load_p <- gme_df_load_t %>% inner_join(topic_perc_order)

# FIGURE 6 : Top 20 tweets most related to topic 4. Note that repetitions signify
# retweets of the same initial tweet.
t4_top20 <- gme_df_load_p %>% arrange(desc(t4)) %>% head(n = 20)
t4_top20$text


# FIGURE 7 : Simple barplot of representation of each topic via adding ratios of all tweets,
# again revealing the topics have near-equivalent representation. Note that for future iterations
# of this analysis it would be useful to try different parameters for our topic model, such as
# changing our number of topics generated or sampling method. For the sake of simplicity and
# illustration I only use k = 4 and the Gibbs method for this R script.
par(mar=c(2,2,2,2))
topic_perc_order %>% select(-index) %>% as.matrix() %>% barplot(main = "Topic representativeness")

# Preparation for stacked barchart.
gme_chart_df <- gme_df_load_p %>% mutate(Index = as.Date(time)) %>% select(Index,t1,t2,t3,t4)
gme_chart_df_long <- gme_chart_df %>% pivot_longer(-Index, names_to = "case", values_to = "val") %>%
  mutate(case = factor(case, levels = c("t1","t2","t3","t4")))
# FIGURE 8 : Visualizing daily topic proportions.
ggplot(gme_chart_df_long, aes(x = Index, y = val, fill = case))+
  geom_col(position = position_stack(reverse = FALSE), alpha = 0.5) +
  labs(title = "Daily topic ratios", x = "Date", y = "Number of Tweets")

# FIGURE 9 : Topic frequencies per second created. This particular graph is messy
# but gives some idea of topic appearances over time. Topic 2 appears very strongly
# around March 7-8 as we see from the near block-shape just over 0.5 during those dates,
# becoming drastically less recurrent as trading reopened Monday).
# Meanwhile topic 3, what might be deemed the "politics" topic given its inclusion of
# words like "manipulation", "aoc" (presumably Alexandria Ocasio-Cortez), and "senwarren"
# (Senator Warren) comes out the strongest March 11th--roughly at the same time the overlayed
# adjusted close price begins to fall. 
# Note that the price is automatically smoothed due to no trading on the weekend of March 6-7.
# The concept of this plot might be used for other data or modified to aid in analyzing
# the relationship between stock prices and tweet content.
# The concept was inspired by Renault's use of sentiment analysis to study the relationship between StockTwits sentiment
# and intraday stock price changes:
# Renault, Thomas, "Intraday online investor sentiment and return patterns in the U.S. stock market," Journal of Banking & Finance 84 (2017): 25-40. 
library(tidyr)
gme_gather <- gather(gme_df_load_p, key = Legend, value = Proportion, c("t1","t2","t3","t4"))
ggplot() +
  geom_line(data = gme_gather, aes(x = time, y = Proportion, group = Legend, color = Legend), alpha = 0.7) +
  geom_line(data = GME_df, aes(x = time, y = price/600, color = "Price"), size = 1.5) +
  scale_y_continuous(
    name = "Proportion of topic appearance",
    sec.axis = sec_axis(~.*600, name = "GME Adjusted Close Price")
  ) +
  scale_color_manual(values = c("black","red","green","blue","purple")) +
  labs(
    title = "Topic proportion appearance versus GME adjusted closing price, March 5-11, 2021",
    caption = "Source: Twitter API & Yahoo! Finance",
    x = "Date"
    ) +
  theme_bw()


# Sentiment Analysis

## on making own sentiment lexicon (see reponses on making own as well as available ones): 
##https://stackoverflow.com/questions/49360828/sentiment-lexicon-for-stock-market-prediction
library(tidytext)
library(textdata)
# Joining with the Loughran dictionary
sentiment_gme <- gme_tidy %>% inner_join(get_sentiments("loughran"))

# FIGURE 10 : Top counts with Loughran dictionary
sentiment_gme %>% count(sentiment) %>% arrange(desc(n))

# Getting sentiment counts, filtering our top 6 observed sentiments
gme_sentiment_counts <- sentiment_gme %>% 
  filter(sentiment %in% c("negative","positive","uncertainty","litigious", "constraining", "superfluous")) %>%
  count(word, sentiment) %>%
  group_by(sentiment) %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(word2 = fct_reorder(word,n))

# FIGURE 11 : Bar plot of our top 6 observed sentiments with the top words for each
ggplot(gme_sentiment_counts, aes(x = word2, y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  coord_flip() +
  labs(title = "Sentiment Word Counts", x = "", y = "Frequency")
