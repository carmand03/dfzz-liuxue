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
library(tidytext)
library(stm)
library(plotly)
library(FactoMineR)
library(reshape)
library(RColorBrewer)


# 1 BUILD THE CORPUS

# Search documents (simple keyword) (152 results)

liuri <- search_documents('"留日"', corpus="dongfangzz") 

# Expanded search (using embedding)

# load embedding

library("fastTextR")

model <- fasttext()
model$load("~/liuri/git/embeddings/toastynews.bin")
nearest_neighbors <- ft_nearest_neighbors(model,"留日", k = 30L)
nearest_neighbors <- names(nearest_neighbors)

# Available Embedding : https://sharedocs.huma-num.fr/#/2547/8997/6-HistText/Embeddings 

# Add selected terms to the query (254 results)

liuri_exp <- search_documents('"留日" | "旅日" | "遊日"', corpus="dongfangzz") 

# inspect keywords in context using concordance 

liuri_conc <- search_concordance('"留日" | "旅日" | "遊日"', corpus="dongfangzz") # 335 occurrences

# select only documents that refer to students

# create a vector with relevant expressions for filtering (study, youth)

xue <- c("學", "靑年")
xue_vec <- paste(xue, sep = "", collapse = "|")

# search the vector within the target fields (After, Before, Title)

liuri_conc <- liuri_conc %>% mutate(xue = str_extract(After, xue_vec)) 
liuri_conc <- liuri_conc %>% mutate(xue2 = str_extract(Before, xue_vec))
liuri_conc <- liuri_conc %>% mutate(xue3 = str_extract(Title, xue_vec)) 
liuri_conc <- liuri_conc %>% mutate(match2 = paste0(xue, xue2, xue3))

# Eliminate non matches 

liuri_conc_filtered <- liuri_conc %>% filter(!match2 == "NANANA") # 136 occurrences 

# Export list of documents for manual checking 

write.csv(liuri_conc_filtered, "liuri_conc_to_check.csv")

# Re-import list of manually filtered documents (107)

library(readr)
liuri_filtered <- read_csv("~/liuri/git/corpora/liuri_filtered.csv")

# Retrieve metadata and full text of documents

liuri_meta <- get_search_fields_content(liuri_filtered, corpus="dongfangzz", 
                                        search_fields=c(list_search_fields("dongfangzz"),
                                                        list_filter_fields("dongfangzz")))

# Retrieve year of publication (first four digits in DocId)

liuri_meta <- liuri_meta %>%
  mutate(Year = str_extract(DocId, "\\d{4}")) %>%
  mutate(Year = as.numeric(Year))

liuri_meta <- liuri_meta %>% relocate(Year, .after = DocId)


# Import list of properly segmented documents (34 short pieces amidst longer documents)

library(readr)
liuri_segmented <- read_delim("git/data/liuri_segmented.csv", 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)

liuri_segmented <- liuri_segmented %>%  dplyr:: rename(text_seg = text)

# Join with complete list 

liuri_clean <- left_join(liuri_meta, liuri_segmented)
liuri_clean <- liuri_clean %>% unique()

# Replace missing text in the text_seg columns by original text 

liuri_clean <- liuri_clean %>%
  mutate(text_seg = ifelse(is.na(text_seg), text, text_seg))

liuri_clean <- liuri_clean %>% relocate(text, .before = text_seg)

# Compute text length 

liuri_clean <- liuri_clean %>% mutate(length = nchar(text)) %>% mutate(length_seg = nchar(text_seg))
liuri_clean <- liuri_clean %>% relocate(category, .before = category_strd)

# Plot distribution over time 

liuri_clean %>% 
  ggplot( aes(x=Year)) +
  geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 1") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  ) +
  labs(title = "Japan-returned students in 東方雜誌",
       x = "Year",
       y = "Number of articles")

# Select data to compare with liumei 

liuri_year <- liuri_clean %>% select(DocId, Year) %>% mutate(Corpus = "留日")

# Compare with entire collection 

liuri_date <- liuri_year %>% rename(Date = Year)
liuri_stats <- stats_date(liuri_date,"dongfangzz",over_all=TRUE, to_plot = FALSE)
stats_date(liuri_date,"dongfangzz",over_all=TRUE, to_plot = TRUE)

# Categories of articles  

liuri_clean %>% group_by(category) %>% count(sort = TRUE) %>% mutate(percent = n/107*100)
liuri_clean %>% group_by(category_strd) %>% count(sort = TRUE) %>% mutate(percent = n/107*100)
liuri_clean %>% group_by(category_sup) %>% count(sort = TRUE) %>% mutate(percent = n/107*100)

write.csv(liuri_clean, "~/liuri/git/output/liuri_clean.csv")
write.csv(liuri_stats, "~/liuri/git/output/liuri_stats.csv")


# 2 TOKENIZATION 

# Tokenization (or the operation of segmenting text into words) is a necessary preliminary step for any text analysis from counting word frequencies, to topic modeling or sentiment analysis.
# Chinese word segmentation on Chinese texts from the early 20th century poses challenges as the language was transitioning from the classical (wenyuan) language of the imperial administration and literati elites (in which one character is usually one word) 
# towards a national and vernacular language (baihua, ) closer to today's mandarin characterized by more complex forms and diverse combinations.
# In the following we used the latest version (2023) of the tokenizer specifically tailored for the "transitional Chinese" of the period under study (Blouin et al., 2023) 

# First we remove artificial white spaces from the raw text 

liuri_to_token <- liuri_clean %>% 
  mutate(text2 = str_replace_all(text_seg, "　", ""))%>% 
  mutate(text2 = str_replace_all(text2, " ", "")) %>% 　
  relocate(text2, .after = text_seg)

liuri_to_token <- liuri_to_token %>% mutate(length2 = nchar(text2))


# Next we apply the tokenizer

liuri_tokenized <- cws_on_df(liuri_to_token,
                             text_column = "text2",
                             id_column = "DocId",
                             model = "trftc_shunpao_23:zh:cws",
                             detailed_output = FALSE,
                             token_separator = " ",
                             verbose = TRUE)



# Note: When the text is too long (over ... characters), the tokenizer automatically split the article into multiple lines
# So we need to reconstruct the entire article afterwards by collapsing the text segments : 

liuri_token_merged <- liuri_tokenized %>%
  group_by(DocId) %>%
  summarise(text_token =paste(Text,collapse=''))

# join with metadata

liuri_tokenized_meta <- left_join(liuri_token_merged, liuri_clean)
liuri_tokenized_meta <- liuri_tokenized_meta %>% relocate(text_token, .after = text_seg)
liuri_tokenized_meta <- liuri_tokenized_meta %>% mutate(lgth_tok_txt = nchar(text_token))

write.csv(liuri_tokenized_meta, "~/liuri/git/output/liuri_tokenized_meta.csv")

# Word Frequencies

library(tidytext)

liuri_tokens <- liuri_token_merged %>% 
  unnest_tokens(output = token, input = text_token) 

# remove noisy output 

liuri_tokens_clean <- liuri_tokens %>% 
  filter(stringr::str_detect(liuri_tokens$token, "[\\p{Han}]")) %>% 
  mutate(token = str_replace_all(token, "一", "")) %>% 
  mutate(token = str_replace_all(token, "[digits:]+", "")) %>% 
  mutate(token = str_replace_all(token, "〇", "")) %>% 
  mutate(token = str_replace_all(token, "◎", "")) %>% 
  mutate(token = str_replace_all(token, "⊙", "")) %>% 
  mutate(token = str_replace_all(token, "●", "")) %>% 
  mutate(token = str_replace_all(token, "▲", "")) %>% 
  mutate(token = str_replace_all(token, "△", "")) 

liuri_tokens_clean <- liuri_tokens_clean %>% drop_na(token)

# join with metadata 

liuri_no_text <- liuri_clean %>% select(c(1:9))
liuri_tokens_meta <- left_join(liuri_tokens_clean, liuri_no_text)
liuri_tokens_meta <- liuri_tokens_meta %>% mutate(tok_length = nchar(token))

# token count

liuri_token_count <- liuri_tokens_meta %>% group_by(token, tok_length) %>% count() 

write.csv(liuri_no_text, "~/liuri/git/output/liuri_no_text.csv")
write.csv(liuri_tokens_meta, "~/liuri/git/output/liuri_tokens_meta.csv")
write.csv(liuri_token_count, "~/liuri/git/output/liuri_token_count.csv")

############################################################################################

# 3 TOPIC MODELING

library(stm)
library(stminsights)



# select metadata

meta <- liuri_tokenized_meta %>% transmute(DocId, title, authors, category_sup, Year)  
meta <- as.data.frame(meta)

# create stm corpus object 

corpus <- stm::textProcessor(liuri_tokenized_meta$text_token,
                             metadata = meta, 
                             stem = FALSE, 
                             wordLengths = c(2, Inf), 
                             customstopwords = c("中國", "日本", "可以", "學生", "我們"),
                             verbose = FALSE) 

stm::plotRemoved(corpus$documents, lower.thresh = c(0,10, by=5)) 

out <- stm::prepDocuments(corpus$documents, 
                          corpus$vocab, 
                          corpus$meta) 


# Removing 16487 of 22478 terms (16487 of 44936 tokens) due to frequency 
# Removing 1 Documents with No Words 
#  Your corpus now has 106 documents, 5991 terms and 28449 tokens.

# Identify doc removed and remove it from the initial dataset with metadata

corpus$docs.removed
corpus.removed <- liuri_tokenized_meta[-c(97),  ]

# Inspect words removed 

wordsremoved <- as_tibble(out$words.removed) 
wordsremoved

# build the 5-topic model 

# 5-topic model

mod.5 <- stm::stm(out$documents, 
                  out$vocab, K=5, 
                  prevalence =~ Year + category_sup, 
                  data=out$meta, verbose = FALSE)

year5 <- stm::estimateEffect(1:5 ~ Year, mod.5, meta=out$meta)
cat5 <- stm::estimateEffect(1:5 ~ category_sup, mod.5, meta=out$meta)


# run_stminsights()

run_stminsights()

# topic summary

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
       subtitle = "Topics related to 留日 in 東方雜誌 (1905-1948)")

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
  labs(title="Topics related to 留日 in 東方雜誌", 
       subtitle = "Topic proportion over time (5-topic)")


# Principal Component Analysis on Topic Proportions 

pca5 <- topicprop5 %>% select(DocId, Topic1, Topic2, Topic3, Topic4, Topic5)
pca5 <- pca5 %>% column_to_rownames("DocId")

library(FactoMineR)
library(Factoshiny)

# Factoshiny(pca5)

# Apply PCA to topic proportions
res.PCA<-PCA(pca5,graph=FALSE)
plot.PCA(res.PCA,choix='var',title="PCA Graph of Variables (topic proportions)")
plot.PCA(res.PCA,title="PCA Graph of Individuals (documents)")

# Apply Hierarchical Clustering to cluster documents with similar topical profiles 
res.HCPC<-HCPC(res.PCA,nb.clust=5,consol=FALSE,graph=FALSE)
plot.HCPC(res.HCPC,choice='tree',title='Tree Map')
plot.HCPC(res.HCPC,choice='map',draw.tree=FALSE,title='Factor Map')
plot.HCPC(res.HCPC,choice='3D.map',ind.names=FALSE,centers.plot=FALSE,angle=60,title='3D-Tree on Factor Map')


# Extract clusters 
pca_clusters <- as.data.frame(res.HCPC$data.clust)
pca_clusters <- rownames_to_column(pca_clusters, "DocId") 
pca_clusters <- pca_clusters %>% dplyr:: rename(topicluster = clust)

## Add Topic Labels 

pca_clusters <- pca_clusters %>% mutate (topiclabel = fct_recode(topicluster, "T3. Regulations/Petitions" = "1", 
                                                                  "T2. Education/Political Reforms" = "2", 
                                                                  "T4. Diplomacy/Cultural Exchanges" = "3", 
                                                                  "T1. Sino-Japanese Tensions" = "4", 
                                                                  "T5. Science/Society" = "5"))

pca_clusters %>% group_by(topiclabel) %>% count(sort = TRUE)

# join with authors

liuri_pca_id <- pca_clusters %>% select(DocId, topiclabel)

liuri_pca_meta <- left_join(liuri_pca_id, liuri_clean)
liuri_pca_meta <- liuri_pca_meta %>% select(-c(text, text_seg))
liuri_authors_topic <- liuri_pca_meta %>% select(DocId, authors, topiclabel)


liuri_authors_topic <- liuri_authors_topic %>% select(DocId, authors, topiclabel) %>%
  mutate(Name = str_replace_all(authors, "\\[", "")) %>%
  mutate(Name = str_replace_all(Name, "\\]", ""))%>%
  mutate(Name = str_replace_all(Name, "\\'", ""))

liuri_authors_topic <- liuri_authors_topic %>%
  separate_rows(Name, sep = ",\\s*")

liuri_authors_topic_unique <- liuri_authors_topic %>% select(authors, Name, topiclabel) %>% drop_na(Name) %>% unique()

liuri_authors_joined2 <- left_join(liuri_author_joined_clean, liuri_authors_topic_unique)


################################################################################################################

# 4 SENTIMENT ANALYSIS 

# Load dictionaries 

# NRC lexicon 

nrc_lexic <- read.delim("~/liuri/git/lexicon/Chinese-Traditional-NRC-EmoLex.txt")

nrc_lexic <- nrc_lexic %>% dplyr::rename(token = Chinese.Traditional.Word) %>% 
  select(token, negative, positive, English.Word) %>% 
  rename(nrc_negative = negative, nrc_positive = positive)

# NRC dictionaries

tw_lexic <- read.csv("~/liuri/git/lexicon/opinion_word_utf8.csv", header=FALSE)

# Rename variables

tw_lexic <- tw_lexic %>% rename(token = V1, copeopi = V2, tw_positive = V3, tw_neutral = V4, 
                                tw_negative = V5, tw_nonsent = V6, tw_nonword = V7)


# Join dictionaries with our corpus

liuri_senti_nrc <- liuri_tokens_meta %>% 
  select(DocId, Year, title, category_sup, authors, token, tok_length) %>%   
  inner_join(nrc_lexic) 

liuri_senti_nrc <- liuri_senti_nrc %>% unique()

liuri_senti_tw <- liuri_tokens_meta %>% 
  select(DocId, Year, title, category_sup, authors, token, tok_length) %>%   
  inner_join(tw_lexic)

liuri_senti_tw <- liuri_senti_tw %>% unique()

liuri_senti <- full_join(liuri_senti_nrc, liuri_senti_tw)
liuri_senti <- liuri_senti %>% relocate(English.Word, .after = token)
liuri_senti <- liuri_senti %>% filter(tok_length >1)

# Identify most negative / positive terms 

liuri_senti_token <- liuri_senti %>% select(token, English.Word, tok_length, nrc_negative, nrc_positive, 
                                            copeopi, tw_negative, tw_positive, tw_neutral, tw_nonsent, tw_nonword) %>% 
  mutate(nrc_ratio = (nrc_positive-nrc_negative), tw_ratio = (tw_positive - tw_negative)) %>% 
  group_by(token) %>% add_tally() 

liuri_senti_token <- liuri_senti_token %>% unique()

# Most negative tokens 

liuri_senti_token %>% select(token, English.Word, nrc_ratio) %>% arrange((nrc_ratio)) # based on NRC
liuri_senti_token %>% select(token, English.Word, tw_ratio) %>% arrange((tw_ratio)) # based on ANTUSD 
liuri_senti_token %>% select(token, English.Word, copeopi) %>% arrange((copeopi)) # based on CopeOpi

# Most positive articles 

liuri_senti_token %>% select(token, English.Word, nrc_ratio) %>% arrange(desc(nrc_ratio)) # based on NRC
liuri_senti_token  %>% select(token, English.Word, tw_ratio) %>% arrange(desc(tw_ratio)) # based on ANTUSD 
liuri_senti_token %>% select(token, English.Word, copeopi) %>% arrange(desc(copeopi)) # based on CopeOpi

# Plot as wordcloud

library(wordcloud)

liuri_senti_token %>% 
  filter(nrc_positive >0) %>% 
  with(wordcloud(token, n, max.words = 100))

library(wordcloud2)

nrc_positive <- liuri_senti_token %>%
  filter(nrc_positive > 0) %>%  # Keep only positive sentiment tokens
  select(token, n) 

wordcloud2(nrc_positive, size = 4, backgroundColor = "grey", color = "random-light")
wordcloud2(nrc_positive, size = 4, color = "random-light")

nrc_negative <- liuri_senti_token %>%
  filter(nrc_negative > 0) %>%  # Keep only positive sentiment tokens
  select(token, n) 

wordcloud2(nrc_negative, size = 4, backgroundColor = "grey", color = "random-light")
wordcloud2(nrc_negative, size = 4, color = "random-light")


library(reshape2)

par(mar = c(0.5, 0.5, 0.5, 0.5))  # Adjust margins
par(mfrow = c(1, 1))  # Ensure single plot

liuri_senti_token %>% drop_na(nrc_ratio) %>%
  mutate(sentiment = ifelse(nrc_ratio > 0, "positive", "negative")) %>%
  count(token, sentiment, sort = TRUE) %>%
  acast(token ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 500, scale = c(2, 2))


liuri_senti_token %>% drop_na(nrc_ratio) %>%
  mutate(sentiment = ifelse(nrc_ratio > 0, "positive", "negative")) %>%
  count(token, sentiment, sort = TRUE) %>%
  acast(token ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("steelblue", "red"),
                   max.words = 500, scale = c(2, 2))

liuri_senti_token %>% drop_na(nrc_ratio) %>%
  mutate(sentiment = ifelse(nrc_ratio > 0, "positive", "negative")) %>%
  count(token, sentiment, sort = TRUE) %>%
  acast(token ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("lightblue", "pink"),
                   max.words = 1000, scale = c(2, 2))



# Compute mean sentiment value per document

liuri_senti_docs <- liuri_senti %>% group_by(DocId, Year, title, category_sup, authors) %>%
  summarise(across(nrc_negative:tw_nonword, ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(nrc_ratio = (nrc_positive-nrc_negative)) %>% 
  mutate(tw_ratio = (tw_positive-tw_negative))

liuri_senti_docs <- liuri_senti_docs %>% relocate(nrc_ratio, .after = nrc_positive) %>% 
  relocate(tw_ratio, .after = tw_positive)

# Most negative articles 

liuri_senti_docs %>% select(DocId, Year, category_sup, title, nrc_ratio) %>% arrange((nrc_ratio)) # based on NRC
liuri_senti_docs %>% select(DocId, Year, category_sup, title, tw_ratio) %>% arrange((tw_ratio)) # based on ANTUSD 
liuri_senti_docs %>% select(DocId, Year, category_sup, title, copeopi) %>% arrange((copeopi)) # based on CopeOpi

# Most positive articles 

liuri_senti_docs %>% select(DocId, Year, category_sup, title, nrc_ratio) %>% arrange(desc(nrc_ratio)) # based on NRC
liuri_senti_docs  %>% select(DocId, Year, category_sup, title, tw_ratio) %>% arrange(desc(tw_ratio)) # based on ANTUSD 
liuri_senti_docs %>% select(DocId, Year, category_sup, title, copeopi) %>% arrange(desc(copeopi)) # based on CopeOpi

# Compute sentiment value by category

liuri_senti_category <- liuri_senti_docs %>% 
  group_by(category_sup) %>%
  summarise(across(c("nrc_negative", "nrc_positive", "copeopi", "tw_negative", "tw_positive"), ~ mean(.x, na.rm = TRUE)))  %>% 
  mutate(tw_ratio = (tw_positive-tw_negative)) %>% 
  mutate(nrc_ratio = (nrc_positive-nrc_negative)) 

liuri_senti_category <- liuri_senti_category %>% relocate(nrc_ratio, .after = nrc_positive) %>% 
  relocate(tw_ratio, .after = tw_positive)

# Compute sentiment value over time 

liuri_senti_year<- liuri_senti_docs %>%
  group_by(Year) %>%
  summarise(across(c("nrc_negative", "nrc_positive", "copeopi", "tw_negative", "tw_positive"), ~ mean(.x, na.rm = TRUE)))  %>% 
  mutate(tw_ratio = (tw_positive-tw_negative)) %>% 
  mutate(nrc_ratio = (nrc_positive-nrc_negative)) %>% 
  relocate(nrc_ratio, .after = nrc_positive) %>% 
  relocate(tw_ratio, .after = tw_positive)

# plot sentiment value over time 

p1 <- liuri_senti_year %>% 
  ggplot() + 
  geom_col(aes(x = Year, y = nrc_ratio)) + 
  labs(title = "Sentiment analysis of 留日 in 東方雜誌", 
       subtitle = "Sentiment value over time", 
       x = "Year", 
       y = "Mean value",
       caption = "Based on NRC Lexicon (Canada)")

p2 <- liuri_senti_year %>% 
  ggplot() + 
  geom_col(aes(x = Year, y = tw_ratio)) + 
  labs(title = "Sentiment analysis of 留日 in 東方雜誌", 
       subtitle = "Sentiment value over time", 
       x = "Year", 
       y = "Mean value",
       caption = "Based on ANTUSD Lexicon (Taiwan)")

p3 <- liuri_senti_year %>% 
  ggplot() + 
  geom_col(aes(x = Year, y = copeopi)) + 
  labs(title = "Sentiment analysis of 留日 in 東方雜誌", 
       subtitle = "Sentiment value over time", 
       x = "Year", 
       y = "Mean value",
       caption = "Based on CopeOpi Lexicon (Taiwan)")


library(plotly)
ggplotly(p1)
ggplotly(p2)
ggplotly(p3)

# Compute sentiment value by author 

liuri_senti_authors <- liuri_senti_docs %>%
  group_by(authors) %>%
  summarise(across(c("nrc_negative", "nrc_positive", "copeopi", "tw_negative", "tw_positive"), ~ mean(.x, na.rm = TRUE)))  %>% 
  mutate(tw_ratio = (tw_positive-tw_negative)) %>% 
  mutate(nrc_ratio = (nrc_positive-nrc_negative)) %>% 
  relocate(nrc_ratio, .after = nrc_positive) %>% 
  relocate(tw_ratio, .after = tw_positive)

# Most negative authors 

liuri_senti_authors %>% select(authors, nrc_ratio) %>% arrange((nrc_ratio)) # based on NRC
liuri_senti_authors %>% select(authors, tw_ratio) %>% arrange((tw_ratio)) # based on ANTUSD 
liuri_senti_authors %>% select(authors, copeopi) %>% arrange((copeopi)) # based on CopeOpi

# Most positive authors 

liuri_senti_authors %>% select(authors, nrc_ratio) %>% arrange(desc(nrc_ratio)) # based on NRC
liuri_senti_authors  %>% select(authors, tw_ratio) %>% arrange(desc(tw_ratio)) # based on ANTUSD 
liuri_senti_authors %>% select(authors, copeopi) %>% arrange(desc(copeopi)) # based on CopeOpi

# Export sentiment values

write.csv(liuri_senti_token, "~/liuri/git/output/liuri_senti_token.csv")
write.csv(liuri_senti_docs, "~/liuri/git/output/liuri_senti_docs.csv")
write.csv(liuri_senti_year, "~/liuri/git/output/liuri_senti_year.csv")
write.csv(liuri_senti_authors, "~/liuri/git/output/liuri_senti_authors.csv")
write.csv(liuri_senti_category, "~/liuri/git/output/liuri_senti_category.csv")


# Join sentiment values with topic proportions

liuri_topic_senti_docs <- left_join(pca_clusters, liuri_senti_docs)

write.csv(liuri_topic_senti_docs, "~/liuri/git/output/liuri_topic_senti_docs.csv")

# 5 AUTHORSHIP 

# Clean authors names and create list of unique authors
# Search the names in biographical sources (Wikipedia and IMH) 
  # -> export and check for redunding or irrelevant results
  # -> reimport curated list of unique authors  
# Extract biographical information on these authors using 
  # regular expressions (birth year and place, list of disciplines and occupations)  
  # NER (institutional affiliations)
# Note: Conversational tools like Chat GPT or Claude were not available at the time when this research was initially conducted, 
  # but today it would be more efficient to use them to extract the desired information with higher precision, using prompts
# reimport list of authors with the complete and curated biographical information 
# statistical analysis of authors' attributes (generation, native place, educational background, occupation, political affiliation)
# joined with article metadata : count number of authored articles, number of articles per author, and co-authored articles 
# joined with sentiment value and topics, and prepare data for MCA 

## STATS
## MCA 

save.image("~/liuri/git/git_liuri.RData")
