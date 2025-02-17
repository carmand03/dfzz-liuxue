
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


library(histtext)
library(tidyverse)
library(FactoMineR)
library(Factoshiny)

# clean authors

liuri_authors <- liuri_clean %>% select(DocId, authors) %>%
  mutate(Author = str_replace_all(authors, "\\[", "")) %>%
  mutate(Author = str_replace_all(Author, "\\]", ""))%>%
  mutate(Author = str_replace_all(Author, "\\'", ""))

liuri_authors <- liuri_authors %>%
  separate_rows(Author, sep = ",\\s*")

# Count number of authored articles, number of articles per author, and co-authored articles 

liuri_authors %>% group_by(Author) %>% count(sort = TRUE)
liuri_authors %>% group_by(DocId) %>% count(sort = TRUE)

# Search authors in Who's who and Wikipedia

# Create function for multiple queries 

multiple_search <- function(queries, corpus) {
  results <- histtext::search_documents_ex(queries[1], corpus) %>%
    mutate(Q=queries[1])
  for(q in queries){
    new_result <- histtext::search_documents_ex(q, corpus) %>%
      mutate(Q=q)
    results <- dplyr::bind_rows(results, new_result)
  }
  distinct(results)
}

# Create a list to query 

liuri_authors_list <- liuri_authors %>% drop_na(Author) %>% filter(!Author == "留日學生") %>% mutate(Queries=str_glue('"{Author}"')) 


# Search in Wikipedia 

liuri_authors_wiki <- multiple_search(liuri_authors_list$Queries, "wikibio-zh") # 338 results
liuri_authors_wiki <- liuri_authors_wiki %>% group_by(Q) %>% add_tally()

# Eliminate irrelevant results 

liuri_list_names <- liuri_authors_list$Author
liuri_list_names <-  paste(liuri_list_names, sep = "", collapse = "|")
liuri_authors_wiki <- liuri_authors_wiki %>% mutate(title_match = str_extract(Title, liuri_list_names))
liuri_authors_wiki <- liuri_authors_wiki %>% group_by(title_match) %>% add_tally()

liuri_authors_wiki_match <- liuri_authors_wiki %>% filter(!is.na(title_match)) # 14 matches

# 14 bios remain

# Extract full text of biographies

liuri_authors_wiki_ft <- histtext::get_documents(liuri_authors_wiki_match, "wikibio-zh")


############ Search in Who's Who publications (IMH collection)

liuri_authors_imh <- multiple_search(liuri_authors_list$Queries, "imh-zh") # 117 results

liuri_authors_imh <- liuri_authors_imh %>% mutate(title_match = str_extract(Title, liuri_list_names))
liuri_authors_imh <- liuri_authors_imh %>% group_by(title_match) %>% add_tally()

# Eliminate irrelevant results 

liuri_authors_imh_match <- liuri_authors_imh %>% filter(!is.na(title_match)) #  69 results remain

liuri_authors_imh_match <- liuri_authors_imh_match %>% distinct(DocId, Title) %>% 
  mutate(Title_clean = str_replace(Title, "【", ""))  %>% 
  mutate(Title_clean = str_replace(Title_clean, "】", "")) %>% 
  mutate(Title_clean = str_extract(Title_clean, "^.{3}")) %>% relocate(title_match, .after = Title_clean)

liuri_authors_imh_match <- liuri_authors_imh_match %>% mutate(title_length = nchar(Title)) %>% mutate(title_length2 = nchar(Title_clean)) %>%
  mutate(query_length = nchar(title_match)) %>% mutate(dif = (query_length-title_length))  %>% mutate(dif2 = (query_length-title_length2))

liuri_authors_imh_filtered <- liuri_authors_imh_match %>% filter(!dif2 == "-1")

liuri_authors_imh_unique <- liuri_authors_imh_filtered %>% 
  select(DocId, title_match) %>% rename(Name = title_match) %>% 
  group_by(Name) %>% count() 

liuri_authors_imh_filtered <- liuri_authors_imh_filtered %>% select(DocId, title_match) %>% rename(Name = title_match)

# 49 biographies referring to 13 unique names remain, with from 1 to 6 bios per author

# Retrieve full text 

liuri_authors_imh_ft <- histtext::get_documents(liuri_authors_imh_filtered, "imh-zh")

## Bind the two datasets 

liuri_authors_bio <- bind_rows(liuri_authors_imh_ft, liuri_authors_wiki_ft)

# Retrieve year of birth 

liuri_authors_bio$year <- regmatches(liuri_authors_bio$Text, gregexpr("\\d{4}", liuri_authors_bio$Text))
liuri_authors_bio$birth <- regmatches(liuri_authors_bio$year, regexpr("[[:digit:]]+", liuri_authors_bio$year))
liuri_authors_bio$year <- NULL
liuri_authors_bio_filtered <- liuri_authors_bio %>% filter(birth < 1948) %>% filter(birth > 1800) # 20 names remain


# Retrieve native provinces based on list 

library(readr)
provinces <- read_delim("~/liuri/git/indexes/provinces.csv",
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)

provinces <- provinces %>% na.omit()

# create vector 

prov_list <- provinces$Province
prov_list <-  paste(prov_list, sep = "", collapse = "|")
liuri_authors_bio_filtered <- liuri_authors_bio_filtered %>% mutate(birthplace = str_extract(Text, prov_list))

# Retrieve disciplines based on typology

# Load typology 

library(readr)
disciplines <- read_delim("~/liuri/git/indexes/disciplines.csv", 
                                    delim = ";", escape_double = FALSE, trim_ws = TRUE)

# create vector

disc_list <- disciplines$Level2__ZhT_MCBD
disc_vec <-  paste(disc_list, sep = "", collapse = "|")

liuri_authors_bio_filtered <- liuri_authors_bio_filtered %>% 
  mutate(discipline = str_extract_all(Text, disc_vec)) %>% 
  mutate(discipline = as.character(discipline))


# identify those who studied or traveled to foreign countries 

liuri_authors_bio_filtered <- liuri_authors_bio_filtered %>% 
  mutate(edu_country = str_extract_all(Text, "留美|留日|留法|留德|留英")) %>% 
  mutate(edu_country = as.character(edu_country)) %>% 
  mutate(country = str_extract_all(Text, "美國|日本|法國|德國|英國")) %>% 
  mutate(country = as.character(country))

# Retrieve NER 

### Extract Named entities from biographies 

liuri_authors_imh_ner <- ner_on_corpus(liuri_authors_imh_ft, corpus = "imh-zh") 
liuri_authors_wiki_ner <- ner_on_corpus(liuri_authors_wiki_ft, corpus = "wikibio-zh")
liuri_authors_ner <- bind_rows(liuri_authors_imh_ner, liuri_authors_wiki_ner) # bind the two lists

# In the following, we will focus on organizations in order to define occupational profiles, but we can also examine other types of entities, 
# such as the events in which they were involved, or the creative works they produced in their careers.

# Filter organizations

liuri_authors_org <- liuri_authors_ner %>% filter(Type == "ORG") 

# Preliminary cleaning 

liuri_authors_org <- liuri_authors_org %>% 
  mutate(text_clean = str_replace(Text, "《", "")) %>% 
  mutate(text_clean = str_replace(text_clean, ">", "")) %>% 
  relocate(text_clean, .after = Text) %>% 
  mutate(text_clean, text_clean=str_replace(text_clean,"（.*","")) %>% 
  mutate(text_clean, text_clean=str_replace(text_clean,"\\(.*","")) %>% 
  mutate(text_clean, text_clean=str_replace(text_clean,"\\(.*",""))%>% 
  mutate(text_clean = str_replace(text_clean, " ", "")) %>% 
  mutate(text_clean = str_replace(text_clean, " ", ""))%>% 
  mutate(length = nchar(text_clean)) %>% 
  relocate(length, .after = text_clean) 

liuri_authors_org <- liuri_authors_org %>% filter(length > 1) # 463

# Extract suffixes and prefixes to help classify and locate institutions 

liuri_authors_org <- liuri_authors_org %>% 
  mutate(class = str_sub(text_clean,-2,-1)) %>% 
  relocate(class, .after = text_clean) 

liuri_authors_org <- liuri_authors_org %>% 
  mutate(pre = str_sub(text_clean, 1, 2)) %>% 
  relocate(pre, .before = text_clean) 

# Remove residual noise (position verbs, etc)

liuri_authors_org <- liuri_authors_org %>% 
  mutate(text_clean = str_replace(text_clean, "任", ""))%>% 
  mutate(text_clean = str_replace(text_clean, "入", ""))%>% 
  mutate(text_clean = str_replace(text_clean, "久", ""))%>% 
  mutate(text_clean = str_replace(text_clean, "于", ""))%>% 
  mutate(text_clean = str_replace(text_clean, "於", ""))%>% 
  mutate(text_clean =  str_replace(text_clean, "從", "")) %>% 
  mutate(length = nchar(text_clean)) %>% 
  filter(length > 1) %>% # 
  mutate(pre = str_sub(text_clean, 1, 2)) 

liuri_authors_org <- liuri_authors_org %>% 
  filter(stringr::str_detect(liuri_authors_org$text_clean, "[\\p{Han}]"))

# 461 organizations remain 

# Most important institutions (altogether, across biographies)

liuri_authors_org %>% group_by(text_clean) %>% 
  count(sort = TRUE) 

liuri_authors_org %>% distinct(DocId, text_clean) %>% 
  group_by(text_clean) %>% count(sort = TRUE)

# Most important types of institutions

liuri_authors_org %>% distinct(DocId, text_clean, class) %>% 
  group_by(class) %>% count(sort = TRUE)

# Export the list of authors with extracted information for manual checking to eliminate redunding or irrelevant results.
# Export the list of organizational affiliations to homogenize and categorize the authors and their occupations. 

write.csv(liuri_authors_org, "~/liuri/git/output/liuri_authors_org.csv")
write.csv(liuri_authors_bio_filtered, "~/liuri/git/output/liuri_authors_bio_filtered.csv")

# Reimport curated list of authors and their attributes 

library(readr)
liuri_authors_to_analyze <- read_delim("git/authors/liuri_authors_to_analyze.csv", 
                                       delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Replace empty fields with NAs

liuri_authors_to_analyze <-liuri_authors_to_analyze %>% 
  mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))))

# Nationality 

liuri_authors_to_analyze %>% group_by(Nationality) %>% count(sort = TRUE)

# Generation/

liuri_authors_to_analyze %>% group_by(Generation) %>% count()

library(hrbrthemes)
library(viridis)

liuri_authors_to_analyze %>% drop_na(BirthYear) %>% 
  mutate(BirthYear = as.numeric(BirthYear))%>% 
  ggplot( aes(x=BirthYear)) +
  geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 1") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  ) +
  labs(title = "Authors' Year of Birth (留日)",
       x = "Year",
       y = "Frequency")

# Native place 

liuri_authors_to_analyze %>% group_by(Birthplace) %>% count(sort = TRUE)


## country of education 

liuri_authors_to_analyze %>% group_by(Country.of.Education) %>% count(sort = TRUE)
liuri_authors_to_analyze %>% group_by(Education.Country) %>% count(sort = TRUE)

# professional background 

liuri_authors_to_analyze %>% group_by(Occupation) %>% count(sort = TRUE)
liuri_authors_to_analyze %>% group_by(Occupation2) %>% count(sort = TRUE)
liuri_authors_to_analyze %>% group_by(Specialization) %>% count(sort = TRUE)

# Political Affiliation

liuri_authors_to_analyze %>% group_by(Party) %>% count(sort = TRUE)

# DL = Democratic League
# GMD = Guomindang
# TMH = Tongmenghui


# Prepare data for MCA

# MCA 

liuri_mca <- liuri_authors_to_analyze %>% select(Name, Generation, 'Education Country', Occupation, Sentiment, Topic)

liuri_mca <-  liuri_mca %>% drop_na(Generation) # 38 remain

liuri_mca <- column_to_rownames(liuri_mca, "Name")


library(FactoMineR)
library(Factoshiny)

# Factoshiny(liuri_mca)
Factoshiny(liuri_mca)


res.MCA<-MCA(liuri_mca,graph=FALSE)
plot.MCA(res.MCA, choix='var',title="Category Plot (留日)",col.var=c(1,2,3,4,5))
plot.MCA(res.MCA,col.var=c(1,1,2,2,2,2,3,3,3,3,3,3,4,4,5,5,5,5,5),title="Biplot (留日)", cex=0.8,cex.main=0.8,cex.axis=0.8, autoLab = "yes", label =c('ind','var'))
plot.MCA(res.MCA,invisible= 'ind',col.var=c(1,1,2,2,2,2,3,3,3,3,3,3,4,4,5,5,5,5,5),title="Biplot: Variables (留日)", label =c('var'))
plot.MCA(res.MCA,invisible= 'var',title="Biplot: Individuals (留日)", autoLab = "yes", label =c('ind'))

plot.MCA(res.MCA, choix='var',title="Category Plot (留日)",col.var=c(1,2,3,4,5))
plot.MCA(res.MCA,col.var=c(1,1,2,2,2,2,3,3,3,3,3,3,4,4,5,5,5,5,5),title="Biplot (留日)", autoLab = "yes", label =c('ind','var'))
plot.MCA(res.MCA,invisible= 'ind',col.var=c(1,1,2,2,2,2,3,3,3,3,3,3,4,4,5,5,5,5,5),  autoLab = "yes", title="Biplot: Variables (留日)",label =c('var'))
plot.MCA(res.MCA,invisible= 'var',title="Biplot: Individuals (留日)", autoLab = "yes", label =c('ind'))
dimdesc(res.MCA)

# dim 2/3

res.MCA<-MCA(liuri_mca,graph=FALSE)
plot.MCA(res.MCA, choix='var',title="Category Plot (留日), Dimensions 2/3", axes=c(2,3),col.var=c(1,2,3,4,5))
plot.MCA(res.MCA,axes=c(2,3),col.var=c(1,1,2,2,2,2,3,3,3,3,3,3,4,4,5,5,5,5,5),title="Biplot (留日), Dimensions 2/3", cex=0.8,cex.main=0.8,cex.axis=0.8, autoLab = "yes", label =c('ind','var'))
plot.MCA(res.MCA,axes=c(2,3),invisible= 'ind',col.var=c(1,1,2,2,2,2,3,3,3,3,3,3,4,4,5,5,5,5,5),title="Biplot: Variables (留日), Dimensions 2/3", label =c('var'))
plot.MCA(res.MCA,axes=c(2,3),invisible= 'var',title="Biplot: Individuals (留日), Dimensions 2/3",label =c('ind'))


plot.MCA(res.MCA, choix='var',title="Category Plot (留日)",axes=c(2,3),col.var=c(1,2,3,4,5))
plot.MCA(res.MCA,axes=c(2,3),col.var=c(1,1,1,1,2,2,2,2,2,3,3,3,3,3,3,4,4,5,5,5,5,5),title="Biplot (留日), Dimensions 2/3",label =c('ind','var'))
plot.MCA(res.MCA,axes=c(2,3),invisible= 'ind',col.var=c(1,1,1,1,2,2,2,2,2,3,3,3,3,3,3,4,4,5,5,5,5,5),title="Biplot: Variables (留日)", label =c('var'))
plot.MCA(res.MCA,axes=c(2,3),invisible= 'var',title="Biplot: Individuals (留日)", autoLab = "yes", label =c('ind'))

# HCPC 

res.MCA<-MCA(liuri_mca,ncp=14,graph=FALSE)
res.HCPC<-HCPC(res.MCA,nb.clust=5,consol=FALSE,graph=FALSE)
plot.HCPC(res.HCPC,choice='tree',title='Tree Map (留日)')
plot.HCPC(res.HCPC,choice='map',draw.tree=FALSE,title='Factor Map (留日)')
plot.HCPC(res.HCPC,choice='3D.map',ind.names=FALSE,centers.plot=FALSE,angle=60,title='3D-Tree Map (留日)')

summary(res.HCPC)


save.image("~/liuri/git/liuri_authors2.RData")
