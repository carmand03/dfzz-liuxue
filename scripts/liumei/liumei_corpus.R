# Install HistText 

#Required package in order to install HistText from gitlab
install.packages("devtools")

# Installation of the HistText package itself
devtools::install_gitlab("enpchina/histtext-r-client")

# Configuration of the package (replace fields with actual server information)
histtext::set_config_file(domain="https://rapi.enpchina.eu",
                          user="enp_restudio",password="uOvgXiNTFR8XQ")

# If successfully configured, the following command will return "OK"
histtext::get_server_status()


# Load packages

library(tidyverse)
library(histtext)
library(fastTextR)
library(hrbrthemes)
library(viridis)

# Search documents (simple keyword) (224 results)

liumei <- search_documents('"留美"', corpus="dongfangzz") 

# Expanded search (using embedding)

# load embedding

library("fastTextR")

model <- fasttext()
model$load("embeddings/toastynews.bin")
nearest_neighbors <- ft_nearest_neighbors(model,"留美", k = 30L)
nearest_neighbors <- names(nearest_neighbors)

# Available Embedding : https://sharedocs.huma-num.fr/#/2547/8997/6-HistText/Embeddings 

# Add selected terms to the query (264 results)

liumei_exp <- search_documents('"留美"|"旅美"', corpus="dongfangzz") 

# inspect keywords in context using concordance 

liumei_conc <- search_concordance('"留美"|"旅美"', corpus="dongfangzz") # 408 occurrences

# select only documents that refer to students

# create a vector with relevant expressions for filtering (study, youth)

xue <- c("學", "靑年")
xue_vec <- paste(xue, sep = "", collapse = "|")

# search the vector within the target fields (After, Before, Title)

liumei_conc <- liumei_conc %>% mutate(xue = str_extract(After, xue_vec)) 
liumei_conc <- liumei_conc %>% mutate(xue2 = str_extract(Before, xue_vec))
liumei_conc <- liumei_conc %>% mutate(xue3 = str_extract(Title, xue_vec)) 
liumei_conc <- liumei_conc %>% mutate(match2 = paste0(xue, xue2, xue3))

# eliminate non matches 

liumei_conc_filtered <- liumei_conc %>% filter(!match2 == "NANANA") # 242 occurrences 

# Export list of documents for manual checking 

# write.csv(liumei_conc_filtered, "liumei_conc_to_check.csv")

# # Re-import list of manually filtered documents (143)

library(readr)
liumei_filtered <- read_csv("~/liuri/git/corpora/liumei_filtered.csv")

# Retrieve metadata and full text of documents

liumei_meta <- get_search_fields_content(liumei_filtered, corpus="dongfangzz", 
                                        search_fields=c(list_search_fields("dongfangzz"),
                                                        list_filter_fields("dongfangzz")))

# Retrieve year of publication (first four digits in DocId)

liumei_meta <- liumei_meta %>%
  mutate(Year = str_extract(DocId, "\\d{4}")) %>%
  mutate(Year = as.numeric(Year))

liumei_meta <- liumei_meta %>% relocate(Year, .after = DocId)


# Import list of properly segmented documents (14 short pieces amidst longer documents)

library(readr)
liumei_segmented <- read_delim("~/liuri/git/corpora/liumei_segmented.csv", 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)

liumei_segmented <- liumei_segmented %>% dplyr:: rename(text_seg = text)

# Join with complete list 

liumei_clean <- left_join(liumei_meta, liumei_segmented)

# Replace missing text in the text_seg columns by original text 

liumei_clean <- liumei_clean %>%
  mutate(text_seg = ifelse(is.na(text_seg), text, text_seg))

liumei_clean <- liumei_clean %>% relocate(text, .before = text_seg)

# Compute text length 

liumei_clean <- liumei_clean %>% mutate(length = nchar(text)) %>% mutate(length_seg = nchar(text_seg))
liumei_clean <- liumei_clean %>% relocate(category, .before = category_strd)


# Plot distribution over time 

liumei_clean %>% 
  ggplot( aes(x=Year)) +
  geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 1") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  ) +
  labs(title = "US-returned students in 東方雜誌",
       x = "Year",
       y = "Number of articles")

# Select data to compare with liuri 

liumei_year <- liumei_clean %>% select(DocId, Year) %>% mutate(Corpus = "留美")



# Compare with entire collection 

liumei_date <- liumei_year %>% dplyr:: rename(Date = Year)
liumei_stats <- stats_date(liumei_date,"dongfangzz",over_all=TRUE, to_plot = FALSE)
stats_date(liumei_date,"dongfangzz",over_all=TRUE, to_plot = TRUE)

# Categories of articles 

liumei_clean %>% group_by(category) %>% count(sort = TRUE) %>% mutate(percent = n/143*100)
liumei_clean %>% group_by(category_strd) %>% count(sort = TRUE) %>% mutate(percent = n/143*100)
liumei_clean %>% group_by(category_sup) %>% count(sort = TRUE) %>% mutate(percent = n/143*100)

# write.csv(liumei_clean, "~/liuri/git/output/liumei_clean.csv")
# write.csv(liumei_stats, "~/liuri/git/output/liumei_stats.csv")

## COMPARATIVE DISTRIBUTION OVER TIME 


# Load metadata on the liuri corpus

liuri_clean <- read_csv("git/output/liuri_clean.csv", 
                        col_types = cols(...1 = col_skip()))


liuri_year <- liuri_clean %>% select(DocId, Year) %>% mutate(Corpus = "留日")

liuri_liumei_year <- bind_rows(liuri_year, liumei_year) # bind datasets

liuri_liumei_year %>%
  group_by(Year, Corpus) %>% count() %>%
  ggplot(aes(Year,n, fill = Corpus)) + geom_col(alpha = 0.8, position="dodge") +
  labs(title = "Japan- and American-returned students in 東方雜誌",
       subtitle = "Number of articles mentioning 留美- and 留日-related terms",
       x = "Year",
       y = "Number of articles")


liuri_liumei_year %>%
  group_by(Year, Corpus) %>% count() %>%
  ggplot(aes(Year,n, fill = Corpus)) + geom_col() +
  facet_wrap(~Corpus, ncol = 1) +
  labs(title = "Japan- and U.S.-returned students in 東方雜誌",
       subtitle = "Number of articles mentioning 留美- and 留日-related terms",
       x = "Year",
       y = "Number of articles") + 
  guides(fill="none")

# Plot with same scale for each region

liuri_liumei_year %>% 
  ggplot( aes(x=Year)) +
  geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 1")  +
  labs(title = "Japan- and US-returned students in 東方雜誌",
       x = "Year ",
       y = "Number of Articles") +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0, "lines"),
    strip.text.x = element_text(size = 8),
    plot.title = element_text(size=13)
  ) +
  facet_wrap(~Corpus, ncol = 1)


liuri_liumei_year %>% 
  ggplot( aes(x=Year)) +
  geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 1")  +
  labs(title = "Japan- and US-returned students in 東方雜誌",
       x = "Year ",
       y = "Number of Articles") +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0, "lines"),
    strip.text.x = element_text(size = 8),
    plot.title = element_text(size=13)
  ) +
  facet_wrap(~Corpus, scale="free_y", ncol = 1)

liuri_liumei_year %>% group_by(Corpus, Date) %>% count() %>% 
  ggplot( aes(x=Date, y=n, group=Corpus, fill=Corpus)) +
  geom_area() +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position="none") +
  labs(title = "Japan- and US-returned students in 東方雜誌",
       x = "Year ",
       y = "Number of Articles") +
  theme_ipsum() +
  theme(
    legend.position="none",
    strip.text.x = element_text(size = 8),
    plot.title = element_text(size=13)
  ) +
  facet_wrap(~Corpus, ncol = 1)

library(ggplot2)
library(dplyr)
library(viridis)
library(hrbrthemes)

liuri_liumei_year %>% 
  group_by(Corpus, Year) %>% 
  count() %>% 
  ggplot(aes(x = Year, y = n, group = Corpus, fill = Corpus)) +
  geom_area() +
  scale_fill_viridis(discrete = TRUE) +
  labs(
    title = "Japan- and US-returned students in 東方雜誌",
    x = "Year",
    y = "Number of Articles"
  ) +
  theme_ipsum() +
  theme(
    legend.position = "none",
    strip.text.x = element_text(size = 8),
    plot.title = element_text(size = 13),
    panel.grid = element_blank(), # Remove grid
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10), # Improve x-axis readability
    axis.text.y = element_text(size = 10) # Improve y-axis readability
  ) +
  facet_wrap(~Corpus, ncol = 1)

liumei_no_text <- liumei_clean %>% select(-c(text, text_seg))

write.csv(liumei_no_text, "~/liuri/git/output/liumei_no_text.csv")

# 2 TOKENIZATION 

# Tokenization (or the operation of segmenting text into words) is a necessary preliminary step for any text analysis from counting word frequencies, to topic modeling or sentiment analysis.
# Chinese word segmentation on Chinese texts from the early 20th century poses challenges as the language was transitioning from the classical (wenyuan) language of the imperial administration and literati elites (in which one character is usually one word) 
# towards a national and vernacular language (baihua, ) closer to today's mandarin characterized by more complex forms and diverse combinations.
# In the following we used the latest version (2023) of the tokenizer specifically tailored for the "transitional Chinese" of the period under study (Blouin et al., 2023) 

# First we remove artificial white spaces within the raw text 

liumei_to_token <- liumei_clean %>% 
  mutate(text2 = str_replace_all(text_seg, "　", ""))%>% 
  mutate(text2 = str_replace_all(text2, " ", "")) %>% 　
  relocate(text2, .after = text_seg)

liumei_to_token <- liumei_to_token %>% mutate(length2 = nchar(text2))

# Next we apply the tokenizer

liumei_tokenized <- cws_on_df(liumei_to_token,
                             text_column = "text2",
                             id_column = "DocId",
                             model = "trftc_shunpao_23:zh:cws",
                             detailed_output = FALSE,
                             token_separator = " ",
                             verbose = TRUE)


# Note: When the text is too long (over ... characters), the tokenizer automatically split the article into multiple lines
# So we need to reconstruct the entire article afterwards by collapsing the text segments : 

liumei_token_merged <- liumei_tokenized %>%
  group_by(DocId) %>%
  summarise(text_token =paste(Text,collapse=''))

# join with metadata

liumei_tokenized_meta <- left_join(liumei_token_merged, liumei_clean)
liumei_tokenized_meta <- liumei_tokenized_meta %>% relocate(text_token, .after = text_seg)
liumei_tokenized_meta <- liumei_tokenized_meta %>% mutate(lgth_tok_txt = nchar(text_token))

# write.csv(liumei_tokenized_meta, "~/liuri/git/output/liumei_tokenized_meta.csv")

# Word Frequencies

# Examining word frequencies is useful exploratory step for having a preliminary overview of the vocabulary before conducted more sophisticated analyses like topic modeling or sentiment analysis.

library(tidytext)

liumei_tokens <- liumei_token_merged %>% 
  unnest_tokens(output = token, input = text_token) 

# Remove noisy output 

liumei_tokens_clean <- liumei_tokens %>% 
  filter(stringr::str_detect(liumei_tokens$token, "[\\p{Han}]")) %>% 
  mutate(token = str_replace_all(token, "一", "")) %>% 
  mutate(token = str_replace_all(token, "[digits:]+", "")) %>% 
  mutate(token = str_replace_all(token, "〇", "")) %>% 
  mutate(token = str_replace_all(token, "◎", "")) %>% 
  mutate(token = str_replace_all(token, "⊙", "")) %>% 
  mutate(token = str_replace_all(token, "●", "")) %>% 
  mutate(token = str_replace_all(token, "▲", "")) %>% 
  mutate(token = str_replace_all(token, "△", "")) 

liumei_tokens_clean <- liumei_tokens_clean %>% drop_na(token)

library(dplyr)

liumei_tokens_clean <- liumei_tokens_clean  %>% 
  mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))))

liumei_tokens_clean <- liumei_tokens_clean %>% na.omit()


# Join with metadata 

liumei_no_text <- liumei_clean %>% select(c(1:9))
liumei_tokens_meta <- left_join(liumei_tokens_clean, liumei_no_text)
liumei_tokens_meta <- liumei_tokens_meta %>% mutate(tok_length = nchar(token))

# Count tokens

liumei_token_count <- liumei_tokens_meta %>% group_by(token, tok_length) %>% count(sort = TRUE) 

liumei_token_count <- liumei_tokens_meta %>% filter(tok_length >1) %>% group_by(token, tok_length) %>% count(sort = TRUE) 

stop <- c("他們", "所以", "現在", "二十", "以後")

write.csv(liumei_no_text, "~/liuri/git/output/liumei_no_text.csv")
write.csv(liumei_tokens_meta, "~/liuri/git/output/liumei_tokens_meta.csv")
write.csv(liumei_token_count, "~/liuri/git/output/liumei_token_count.csv")

############################################################################################

# 3 TOPIC MODELING

library(stm)
library(stminsights)

# Filter out advertisements

liumei_no_ad <- liumei_tokenized_meta %>% filter(!category_sup == "廣告")

# Select metadata

meta <- liumei_no_ad %>% transmute(DocId, title, authors, category_sup, Year)  
meta <- as.data.frame(meta)

# create stm corpus object 

## MAXI
corpus <- stm::textProcessor(liumei_tokenized_meta$text_token,
                             metadata = meta, 
                             stem = FALSE, 
                             wordLengths = c(2, Inf), 
                             customstopwords = c("中國", "美國", "學生", "因為", "可以", "所以", "對於", "ion", "我們", "他們", "方面"),
                             verbose = FALSE) 
##

corpus <- stm::textProcessor(liumei_no_ad$text_token,
                             metadata = meta, 
                             stem = FALSE, 
                             wordLengths = c(2, Inf), 
                             customstopwords = c("中國", "因為", "可以", "因爲", "所以", "對於", "ion", "我們", "他們", "方面"),
                             verbose = FALSE) 


stm::plotRemoved(corpus$documents, lower.thresh = c(0,10, by=5)) 

out <- stm::prepDocuments(corpus$documents, 
                          corpus$vocab, 
                          corpus$meta) 

# Removing 17794 of 24067 terms (17794 of 49982 tokens) due to frequency 
# Your corpus now has 82 documents, 6273 terms and 32188 tokens..


# Identify doc removed (advertisements for Chinese Students' Quarterly) and remove them from the initial dataset with metadata 

corpus$docs.removed
docs.removed <- corpus$docs.removed
corpus.removed <- liumei_tokenized_meta[-c(docs.removed),  ]

# Inspect words removed 

wordsremoved <- as_tibble(out$words.removed) 
wordsremoved

# Build the 5-topic model 

# 5-topic model

mod.5 <- stm::stm(out$documents, 
                  out$vocab, K=5, 
                  prevalence =~ Year + category_sup, 
                  data=out$meta, verbose = FALSE)

year5 <- stm::estimateEffect(1:5 ~ Year, mod.5, meta=out$meta)
cat5 <- stm::estimateEffect(1:5 ~ category_sup, mod.5, meta=out$meta)


# Run_stminsights()

run_stminsights()

# Topic summary

plot.STM(mod.5,"summary", n = 7)

library(tidytext)

td_beta5 <- tidytext::tidy(mod.5) 

options(repr.plot.width=7, repr.plot.height=8, repr.plot.res=100) 

td_beta5 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col( show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic",
       subtitle = "Topics related to 留美 in 東方雜誌 (1905-1948)")

# extract topic proportions 

topicprop5<-make.dt(mod.5, meta)

# TOPICS OVER TIME 

topic5prop <- topicprop5%>% select(c(2:6))

#  By Year 

topic_proportion_per_year5 <- aggregate(topic5prop, by = list(Year = topicprop5$Year), mean)

library(reshape)
dfviz5y <- melt(topic_proportion_per_year5, id.vars = "Year")

library(RColorBrewer)
# Change color palette
color_palette5 <- brewer.pal(5, "Set3") 

ggplot(dfviz5y, aes(x=Year, y=value, fill=variable)) + 
  geom_bar(stat = "identity") + ylab("proportion") + 
  scale_fill_manual(values = color_palette5, name = "Topic") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title="Topics related to 留美 in 東方雜誌", 
       subtitle = "Topic proportion over time (5-topic)")

# Principal Component Analysis on Topic Proportions 

pca5 <- topicprop5 %>% select(DocId, Topic1, Topic2, Topic3, Topic4, Topic5)
pca5 <- pca5 %>% column_to_rownames("DocId")

library(FactoMineR)
library(Factoshiny)

# Factoshiny(pca5)

Factoshiny(pca5)

# Apply PCA to topic proportions
res.PCA<-PCA(pca5,graph=FALSE)
plot.PCA(res.PCA,choix='var',title="PCA Graph of Variables (topic proportions)")
plot.PCA(res.PCA,title="PCA Graph of Individuals (documents)")

# Apply Hierarchical Clustering to cluster documents with similar topical profiles 
res.PCA<-PCA(pca5,ncp=4,graph=FALSE)
res.HCPC<-HCPC(res.PCA,nb.clust=5,consol=FALSE,graph=FALSE)
plot.HCPC(res.HCPC,choice='tree',title='Arbre hiérarchique')
plot.HCPC(res.HCPC,choice='map',draw.tree=FALSE,title='Plan factoriel')
plot.HCPC(res.HCPC,choice='3D.map',ind.names=FALSE,centers.plot=FALSE,angle=60,title='Arbre hiérarchique sur le plan factoriel')


# Extract clusters 
pca_clusters <- as.data.frame(res.HCPC$data.clust)
pca_clusters <- rownames_to_column(pca_clusters, "DocId") 
pca_clusters <- pca_clusters %>% dplyr:: rename(topicluster = clust)

## Add Topic Labels 

pca_clusters <- pca_clusters %>% mutate (topiclabel = fct_recode(topicluster, "T5. Regulation/Organization" = "1", 
                                                                 "T4. Immigration/Economy" = "2", 
                                                                 "T2. International Treaties" = "3", 
                                                                 "T1. East/West" = "4", 
                                                                 "T3. Society/Culture" = "5"))

pca_clusters %>% group_by(topiclabel) %>% count(sort = TRUE)

# join with authors

liumei_pca_id <- pca_clusters %>% select(DocId, topiclabel)

liumei_pca_meta <- left_join(liumei_pca_id, liumei_no_text)
liumei_authors_topic <- liumei_pca_meta %>% select(DocId, authors, topiclabel)


liumei_authors_topic <- liumei_authors_topic %>% select(DocId, authors, topiclabel) %>%
  mutate(Name = str_replace_all(authors, "\\[", "")) %>%
  mutate(Name = str_replace_all(Name, "\\]", ""))%>%
  mutate(Name = str_replace_all(Name, "\\'", ""))

liumei_authors_topic <- liumei_authors_topic %>%
  separate_rows(Name, sep = ",\\s*")

liumei_authors_topic_unique <- liumei_authors_topic %>% select(authors, Name, topiclabel) %>% drop_na(Name) %>% unique()

library(readr)
liumei_author_joined_clean <- read_delim("data3/authors/authors_analysis/liumei_author_joined_clean.csv", 
                                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

liumei_authors_joined2 <- left_join(liumei_author_joined_clean, liumei_authors_topic_unique)

################################################################################################################

# 4 SENTIMENT ANALYSIS 

# Load dictionaries 

# NRC lexicon 

nrc_lexic <- read.delim("~/liuri/git/lexicon/Chinese-Traditional-NRC-EmoLex.txt")

nrc_lexic <- nrc_lexic %>% dplyr::rename(token = Chinese.Traditional.Word) %>% 
  select(token, negative, positive, English.Word) %>% 
  dplyr::rename(nrc_negative = negative, nrc_positive = positive)

# NRC dictionaries

tw_lexic <- read.csv("~/liuri/git/lexicon/opinion_word_utf8.csv", header=FALSE)

# Rename variables

tw_lexic <- tw_lexic %>% dplyr::rename(token = V1, copeopi = V2, tw_positive = V3, tw_neutral = V4, 
                                tw_negative = V5, tw_nonsent = V6, tw_nonword = V7)


# Join dictionaries with our corpus

liumei_senti_nrc <- liumei_tokens_meta %>% 
  select(DocId, Year, title, category_sup, authors, token, tok_length) %>%   
  inner_join(nrc_lexic) 

liumei_senti_nrc <- liumei_senti_nrc %>% unique()

liumei_senti_tw <- liumei_tokens_meta %>% 
  select(DocId, Year, title, category_sup, authors, token, tok_length) %>%   
  inner_join(tw_lexic)

liumei_senti_tw <- liumei_senti_tw %>% unique()

liumei_senti <- full_join(liumei_senti_nrc, liumei_senti_tw)
liumei_senti <- liumei_senti %>% relocate(English.Word, .after = token)
liumei_senti <- liumei_senti %>% filter(tok_length >1)

# Identify most negative / positive terms 

liumei_senti_token <- liumei_senti %>% select(token, English.Word, tok_length, nrc_negative, nrc_positive, 
                                            copeopi, tw_negative, tw_positive, tw_neutral, tw_nonsent, tw_nonword) %>% 
  mutate(nrc_ratio = (nrc_positive-nrc_negative), tw_ratio = (tw_positive - tw_negative)) %>% 
  group_by(token) %>% add_tally() 

liumei_senti_token <- liumei_senti_token %>% unique()

# Most negative tokens 

liumei_senti_token %>% select(token, English.Word, nrc_ratio) %>% arrange((nrc_ratio)) # based on NRC
liumei_senti_token %>% select(token, English.Word, tw_ratio) %>% arrange((tw_ratio)) # based on ANTUSD 
liumei_senti_token %>% select(token, English.Word, copeopi) %>% arrange((copeopi)) # based on CopeOpi

# Most positive articles 

liumei_senti_token %>% select(token, English.Word, nrc_ratio) %>% arrange(desc(nrc_ratio)) # based on NRC
liumei_senti_token  %>% select(token, English.Word, tw_ratio) %>% arrange(desc(tw_ratio)) # based on ANTUSD 
liumei_senti_token %>% select(token, English.Word, copeopi) %>% arrange(desc(copeopi)) # based on CopeOpi

# Plot as wordcloud

library(wordcloud)

liumei_senti_token %>% 
  filter(nrc_positive >0) %>% 
  with(wordcloud(token, n, max.words = 100))

library(wordcloud2)

nrc_positive <- liumei_senti_token %>%
  filter(nrc_positive > 0) %>%  # Keep only positive sentiment tokens
  select(token, n) 

wordcloud2(nrc_positive, size = 4, backgroundColor = "grey", color = "random-light")
wordcloud2(nrc_positive, size = 4, color = "random-light")

nrc_negative <- liumei_senti_token %>%
  filter(nrc_negative > 0) %>%  # Keep only positive sentiment tokens
  select(token, n) 

wordcloud2(nrc_negative, size = 4, backgroundColor = "grey", color = "random-light")
wordcloud2(nrc_negative, size = 4, color = "random-light")


library(reshape2)

par(mar = c(0.5, 0.5, 0.5, 0.5))  # Adjust margins
par(mfrow = c(1, 1))  # Ensure single plot

liumei_senti_token %>% drop_na(nrc_ratio) %>%
  mutate(sentiment = ifelse(nrc_ratio > 0, "positive", "negative")) %>%
  count(token, sentiment, sort = TRUE) %>%
  acast(token ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 500, scale = c(2, 2))


liumei_senti_token %>% drop_na(nrc_ratio) %>%
  mutate(sentiment = ifelse(nrc_ratio > 0, "positive", "negative")) %>%
  count(token, sentiment, sort = TRUE) %>%
  acast(token ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("steelblue", "red"),
                   max.words = 500, scale = c(2, 2))

liumei_senti_token %>% drop_na(nrc_ratio) %>%
  mutate(sentiment = ifelse(nrc_ratio > 0, "positive", "negative")) %>%
  count(token, sentiment, sort = TRUE) %>%
  acast(token ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("lightblue", "pink"),
                   max.words = 1000, scale = c(2, 2))



# Compute mean sentiment value per document

liumei_senti_docs <- liumei_senti %>% group_by(DocId, Year, title, category_sup, authors) %>%
  summarise(across(nrc_negative:tw_nonword, ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(nrc_ratio = (nrc_positive-nrc_negative)) %>% 
  mutate(tw_ratio = (tw_positive-tw_negative))

liumei_senti_docs <- liumei_senti_docs %>% relocate(nrc_ratio, .after = nrc_positive) %>% 
  relocate(tw_ratio, .after = tw_positive)

# Most negative articles 

liumei_senti_docs %>% select(DocId, Year, category_sup, title, nrc_ratio) %>% arrange((nrc_ratio)) # based on NRC
liumei_senti_docs %>% select(DocId, Year, category_sup, title, tw_ratio) %>% arrange((tw_ratio)) # based on ANTUSD 
liumei_senti_docs %>% select(DocId, Year, category_sup, title, copeopi) %>% arrange((copeopi)) # based on CopeOpi

# Most positive articles 

liumei_senti_docs %>% select(DocId, Year, category_sup, title, nrc_ratio) %>% arrange(desc(nrc_ratio)) # based on NRC
liumei_senti_docs  %>% select(DocId, Year, category_sup, title, tw_ratio) %>% arrange(desc(tw_ratio)) # based on ANTUSD 
liumei_senti_docs %>% select(DocId, Year, category_sup, title, copeopi) %>% arrange(desc(copeopi)) # based on CopeOpi

# Compute sentiment value by category

liumei_senti_category <- liumei_senti_docs %>% 
  group_by(category_sup) %>%
  summarise(across(c("nrc_negative", "nrc_positive", "copeopi", "tw_negative", "tw_positive"), ~ mean(.x, na.rm = TRUE)))  %>% 
  mutate(tw_ratio = (tw_positive-tw_negative)) %>% 
  mutate(nrc_ratio = (nrc_positive-nrc_negative)) 

liumei_senti_category <- liumei_senti_category %>% relocate(nrc_ratio, .after = nrc_positive) %>% 
  relocate(tw_ratio, .after = tw_positive)

# Compute sentiment value over time 

liumei_senti_year<- liumei_senti_docs %>%
  group_by(Year) %>%
  summarise(across(c("nrc_negative", "nrc_positive", "copeopi", "tw_negative", "tw_positive"), ~ mean(.x, na.rm = TRUE)))  %>% 
  mutate(tw_ratio = (tw_positive-tw_negative)) %>% 
  mutate(nrc_ratio = (nrc_positive-nrc_negative)) %>% 
  relocate(nrc_ratio, .after = nrc_positive) %>% 
  relocate(tw_ratio, .after = tw_positive)

# plot sentiment value over time 

p1 <- liumei_senti_year %>% 
  ggplot() + 
  geom_col(aes(x = Year, y = nrc_ratio)) + 
  labs(title = "Sentiment analysis of 留美 in 東方雜誌", 
       subtitle = "Sentiment value over time", 
       x = "Year", 
       y = "Mean value",
       caption = "Based on NRC Lexicon (Canada)")

p2 <- liumei_senti_year %>% 
  ggplot() + 
  geom_col(aes(x = Year, y = tw_ratio)) + 
  labs(title = "Sentiment analysis of 留美 in 東方雜誌", 
       subtitle = "Sentiment value over time", 
       x = "Year", 
       y = "Mean value",
       caption = "Based on ANTUSD Lexicon (Taiwan)")

p3 <- liumei_senti_year %>% 
  ggplot() + 
  geom_col(aes(x = Year, y = copeopi)) + 
  labs(title = "Sentiment analysis of 留美 in 東方雜誌", 
       subtitle = "Sentiment value over time", 
       x = "Year", 
       y = "Mean value",
       caption = "Based on CopeOpi Lexicon (Taiwan)")


library(plotly)
ggplotly(p1)
ggplotly(p2)
ggplotly(p3)

# Compute sentiment value by author 

liumei_senti_authors <- liumei_senti_docs %>%
  group_by(authors) %>%
  summarise(across(c("nrc_negative", "nrc_positive", "copeopi", "tw_negative", "tw_positive"), ~ mean(.x, na.rm = TRUE)))  %>% 
  mutate(tw_ratio = (tw_positive-tw_negative)) %>% 
  mutate(nrc_ratio = (nrc_positive-nrc_negative)) %>% 
  relocate(nrc_ratio, .after = nrc_positive) %>% 
  relocate(tw_ratio, .after = tw_positive)

# Most negative authors 

liumei_senti_authors %>% select(authors, nrc_ratio) %>% arrange((nrc_ratio)) # based on NRC
liumei_senti_authors %>% select(authors, tw_ratio) %>% arrange((tw_ratio)) # based on ANTUSD 
liumei_senti_authors %>% select(authors, copeopi) %>% arrange((copeopi)) # based on CopeOpi

# Most positive authors 

liumei_senti_authors %>% select(authors, nrc_ratio) %>% arrange(desc(nrc_ratio)) # based on NRC
liumei_senti_authors  %>% select(authors, tw_ratio) %>% arrange(desc(tw_ratio)) # based on ANTUSD 
liumei_senti_authors %>% select(authors, copeopi) %>% arrange(desc(copeopi)) # based on CopeOpi

# Export sentiment values

write.csv(liumei_senti_token, "~/liuri/git/output/liumei_senti_token.csv")
write.csv(liumei_senti_docs, "~/liuri/git/output/liumei_senti_docs.csv")
write.csv(liumei_senti_year, "~/liuri/git/output/liumei_senti_year.csv")
write.csv(liumei_senti_authors, "~/liuri/git/output/liumei_senti_authors.csv")
write.csv(liumei_senti_category, "~/liuri/git/output/liumei_senti_category.csv")


# Join sentiment values with topic proportions

liumei_topic_senti_docs <- left_join(pca_clusters, liumei_senti_docs)

write.csv(liumei_topic_senti_docs, "~/liuri/git/output/liumei_topic_senti_docs.csv")

save.image("~/liuri/git/git_liumei.RData")
save.image("~/liuri/git/git_liumei2.RData")
