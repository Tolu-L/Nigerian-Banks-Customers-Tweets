---
  title: "Nigerian Bank Digital Service- Customers Insights from Twitter "
author: 'Tolu_L'
date: "05/03/2022"
output:
  word_document: default
html_document: default
pdf_document: default
---
  
  
  ```{r setup, include = FALSE}
knitr::opts_chunk$set(cache = TRUE)
```


```{r echo=TRUE, message=TRUE, warning=TRUE}
# For loading relevant libraries
library(rtweet)
library(readr)
library(httpuv)
library(httr)
library(httr2)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidytext)
library (igraph)
library(ggraph)
library(ggplot2)
library(textstem)
library(readr)
library(koRpus)
library(magrittr)
library(tidyr)
library(lexicon)
library(parallel)
library(knitr)
library(textclean)
library(udpipe)
library(textstem)
library(reshape2)
library(rvest)
library(sentimentr)
library(quanteda)
library(stm)
library(wordcloud)
library(tm)
library(SentimentAnalysis)
library(widyr)
library(text)
```

INTRODUCTION

Twitter is part of the top social platform used by Nigerians, expressing and sharing their daily thoughts on politics, experiences. Since the introduction of spaces om twitter, various rooms have been created where individuals have further expressed their opinions on “Horrible bosses”, “Incumbent Vice president declaring his intention to run for the 2023 presidential election”, “Complaint on the user experience with Nigerian banks”, “Africans taking jobs of the foundational black American” to name a few.

According to The Nigerian Communications Commission (NCC),around 80 million Nigerians have access to the internet, 61.4% have Twitter accounts which in the shown in Figure 1.1. In October 2020, Twitter was used by the Nigerian youth to champion a protest to end police brutality, #ENDSARS which provided a channel for communication with the conveyor of the protest on fundraising, medical assistance and monitoring the people arrested for protesting. Sadly, the Nigerian government fired shots at its innocent citizens leaving some dead and the government denied the killings. However, videos were circulated on Twitter on the incidents which were later reported in the news. After that incident, the Nigerian government banned the use of Twitter in 2021 because the president made a hate speech that was deleted by Twitter.

Asides from promoting activism, Twitter is being used by businesses, either small, medium, and large corporations to promote their products and services, and giving customers direct access to an open market to buy whatever product they want by simply sending a direct message to the seller or simply responding to the tweet the product or service is being advertised or clicking on a link to access the product/service. Twitter also allows customers to easily call out a business for providing poor service or substandard products, and reputable brands try as much as possible to avoid being “dragged” on Twitter and try to resolve any issues promptly.

For this project, I decided to focus on the user experience of customers of largest traditional banks in Nigeria with the acronym FUGAZ with the letters representing First Bank of Nigeria, United Bank of Africa, Guarantee Trust Bank, Access Bank, and Zenith Bank. Daily, there are complaints about flaws in some of the services provided by banks in Nigeria and Twitter is an interesting place to read the thoughts and complaints of the customers.

To access the twitter API, I had to contact Twitter as my normal user account could not provide the data needed, I was informed that it was compulsory to set up a developer account, after which an access token, and a token secret must be generated. There are a lot of restrictions if you do not have a premium account and can only pull a restricted number of tweets across profiles with a further limit of 6-9 days, hereby limiting the scope of the work. 

```{r}
# whatever name you assigned to your created app
appname <- "PendingDataScientist"

## api key
key <- "wJkR3380E19U6DQLbXwGANPq0"

## api secret 
secret <- "zubQKWiiOz8T5k3rq3qFdYj451WECepdRrDBZ4ur0ECKu9B9c4"

accessToken <- "356231953-xNZbFtQ5neSHYzwjZ9epEe00wgFbFXPxCeiyi00n"
accessTokenSecret <- "ERvM7jQ8PgJPqXpTAtNjBNNJCztoKaGQEVX6zPweOVqCh"


# Finally, you can create a token that authenticates access to tweets! Note that the authentication process below will open a window in your browser.

# create token named "twitter_token"
twitter.token <- create_token(app = appname,consumer_key = key,consumer_secret =secret, access_token=accessToken,access_secret=accessTokenSecret)

get_token()


# whatever name you assigned to your created app
appname <- "PendingDataScientist"
rm(accessTokenSecret)
## api key
key <- "wJkR3380E19U6DQLbXwGANPq0"

## api secret 
secret <- "zubQKWiiOz8T5k3rq3qFdYj451WECepdRrDBZ4ur0ECKu9B9c4"

accessToken <- "356231953-xNZbFtQ5neSHYzwjZ9epEe00wgFbFXPxCeiyi00n"
accessTokenSecret <- "ERvM7jQ8PgJPqXpTAtNjBNNJCztoKaGQEVX6zPweOVqCh"


# Finally, you can create a token that authenticates access to tweets! Note that the authentication process below will open a window in your browser.

# create token named "twitter_token"
twitter.token <- create_token(app = appname,consumer_key = key,consumer_secret =secret, access_token=accessToken,access_secret=accessTokenSecret)

get_token()
```

```{r}
#Load and Merge the data 

#GTBANK
Gtbank.Nigeria<- search_tweets (q= ("gtbank OR gthelp"), n=18000,  lang="en", include_rts=FALSE, retryonratelimit=TRUE)
Gtbank.3<- search_tweets (q= ("gtbank OR gthelp"), n=18000,  lang="en", include_rts=FALSE, retryonratelimit=TRUE)
GTBANK<- rbind(Gtbank.Nigeria,Gtbank.3)
GTBANK<-read.csv("GTBANK.csv")

# Create a Column for the Bank name
GTBANK$Bank_Name <- "GT Bank"

#FIRST BANK
Firstbank.Nigeria<- search_tweets(q= ("FirstBankngr OR FBN_help"), n=18000, lang="en", include_rts=FALSE, retryonratelimit=TRUE)
Firstbank.2<-search_tweets(q= ("FirstBankngr OR FBN_help"), n=18000, lang="en", include_rts=FALSE, retryonratelimit=TRUE)
FIRST.BANK<- rbind(Firstbank.Nigeria,Firstbank.2)
FIRST.BANK<- read.csv("FIRST.BANK.csv")

# Create a Column for the Bank name
FIRST.BANK$Bank_Name <- "First Bank"

#ZENITH BANK

Zenith.Nigeria<- search_tweets(q= ("Zenith Bank/"), n=30000, lang="en", include_rts=FALSE, retryonratelimit=TRUE)
Zenith2<- search_tweets(q= ("Zenith Bank/"), n=30000, lang="en", include_rts=FALSE, retryonratelimit=TRUE)

ZENITH.BANK<- rbind(Zenith.Nigeria,Zenith2)
ZENITH.BANK<- read.csv("ZENITH.BANK.csv")

# Create a Column for the Bank name
ZENITH.BANK$Bank_Name <- "Zenith Bank"

#ACCESS BANK

Access.Nigeria<- search_tweets(q= ("accessbank_help OR Access bank"), n=2000000, lang="en", include_rts=FALSE,retryonratelimit=TRUE)
Access2<- search_tweets(q= ("accessbank_help OR Access bank"), n=2000000, lang="en", include_rts=FALSE,retryonratelimit=TRUE)
ACCESS.BANK<- rbind(Access.Nigeria,Access2)
ACCESS.BANK<- read.csv("ACCESS.BANK.csv")


# Create a Column for the Bank name
ACCESS.BANK$Bank_Name <- "Access Bank"

#UBA  
UBA.Nigeria<- search_tweets(q= ("UBAGroup"), n=2000000,lang="en", include_rts=FALSE,retryonratelimit=TRUE)
UBA.2<- search_tweets(q= ("UBACares"), n=2000000,lang="en", include_rts=FALSE,retryonratelimit=TRUE)
UBA.BANK<- rbind(UBA.2,UBA.Nigeria)
UBA.BANK<- read.csv("UBA.BANK.csv")
# Create a Column for the Bank name
UBA.BANK$Bank_Name <- "UBA"


```{r}
#Load and Merge the data 

#GTBANK

GTBANK<-read.csv("GTBANK.csv")

# Create a Column for the Bank name
GTBANK$Bank_Name <- "GT Bank"

#FIRST BANK

FIRST.BANK<- read.csv("FIRST.BANK.csv")

# Create a Column for the Bank name
FIRST.BANK$Bank_Name <- "First Bank"

#ZENITH BANK


ZENITH.BANK<- read.csv("ZENITH.BANK.csv")

# Create a Column for the Bank name
ZENITH.BANK$Bank_Name <- "Zenith Bank"

#ACCESS BANK


ACCESS.BANK<- read.csv("ACCESS.BANK.csv")


# Create a Column for the Bank name
ACCESS.BANK$Bank_Name <- "Access Bank"

#UBA  

UBA.BANK<- read.csv("UBA.BANK.csv")
# Create a Column for the Bank name
UBA.BANK$Bank_Name <- "UBA"


# Update Bank tweets and select the relevant columns and save as a CSV file

GTBANK <- GTBANK %>% select(screen_name, text, display_text_width,Bank_Name)
save_as_csv(GTBANK,"GTBANK.csv")

ZENITH.BANK <-ZENITH.BANK %>% select(screen_name, text, display_text_width,Bank_Name)
save_as_csv(ZENITH.BANK,"ZENITH.BANK.csv")

ACCESS.BANK<- ACCESS.BANK %>% select(screen_name, text,display_text_width,Bank_Name)
save_as_csv(ACCESS.BANK,"ACCESS.BANK.csv")

UBA.BANK<- UBA.BANK %>% select(screen_name, text, display_text_width,Bank_Name)
save_as_csv(UBA.BANK,"UBA.BANK.csv")

FIRST.BANK<-FIRST.BANK %>% select(screen_name, text,display_text_width,Bank_Name)
save_as_csv(FIRST.BANK,"FIRST.BANK.csv")

#Merging the data from all the banks and saving as a CSV file
FUGAZ<- rbind(GTBANK,ZENITH.BANK,ACCESS.BANK,UBA.BANK,FIRST.BANK)
FUGAZ$Bank_Name<- as.factor(FUGAZ$Bank_Name)
save_as_csv(FUGAZ,"FUGAZ.csv")


# Visualise the number of tweets per bank
FUGAZ %>%
  group_by(Bank_Name) %>%
  ggplot(aes(Bank_Name,display_text_width,)) +
  geom_col( fill="darkblue") +
  labs(x="No of Tweets",y = "Bank Name")

```

```{r}
#Histogram for distribution for tweet length 
ggplot(FUGAZ,aes(x=display_text_width))+geom_histogram(fill="darkblue",col="black",bins=100)+xlab("No of Tweet characters")
```


```{R message=TRUE, warning=TRUE}
# Data Cleaning
# Create stop word 
data("stop_words")
fry <- as.data.frame(lexicon::sw_fry_1000) %>% rename(word='lexicon::sw_fry_1000')
loughran_mcdonald_sw<- as.data.frame(lexicon::sw_loughran_mcdonald_long) %>% rename(word='lexicon::sw_loughran_mcdonald_long')
custom.stop.words <- c("emmyrichie","youfirst","date","boss","mobile","till","bannks","banke","cuppymusic","mrfunny","monday","business","phone","abeg","credite","didnt","register","token","handle","pbtip","april","acct","earn","whatsapp","review","march","understand","balogun","personal","ajayikabir","nice","release","seriki","yaho","comment","david","drug","john","forever","operation","recently","ibadan","pain","strand","afternoon","wasn’t","aytobay","sake","video","michael","recipient","avail","bello","inbox","myfcmb","allyzander","mhizqeez","dlaureate","starrnene","submit","zinoieesky","manage","restrict","comot","device","luck","simply","benefit","media","officer","resend","retrieve","sportybet","assu","dstv","fuel","location","lock","delete","theyve","burnt","holder","aadeduntan","effort","hang","website","hospital","notify","annually","opay","totally","literally","wemabank","illegal","tigrayan","tomorrow","isnt","hasnt","fuck","davido","statement","smilingpen","asap","cash","staff","lilpharry","sincerely","reiterat","aladeayoo","emmanuel","havent","mrodanz","funny","address","code","explain","bnairal","naijapr","ernestadiq","mrbankstip","baba","allah","empathize","reiterat","sunday","ramadan","tueday","Wednesday","Thursday","Saturday","sunday", "simpleppearl","tundeeddnut","iamneeyee","oblcubana","theonlyologi","victorisrael","realnonireal","click","greentip","damiadenuga","samwellsg","villageboy","enitanlago","public","	wont","harjoke","mmalove","russian","tunkaynytee","	somtosocial","talentedfbg","nwaenyiztndu","hafsatpaki","oooo","sheddykinga","mribitoye","cdasola","wizkid","thekayodeojo","thekayodeojo","betodd","babykawhi","werey","damixx","lago","damixx","nkechi","bobinrin","instablogja","daniel","ifeanyi","tekmayor","kindly","app","message","dey","deduct","bless","guy","sir","god","pls","plz","una","amp","don","atm","link","ooo","oooo","ive","dem","bro","wey","wont","cos","omo","abi","sha","plc","lol","details","ill","wetin","trend","channel","sef","acc","peace","nah","prsy","rob","seek","dis","wtf","shey","'all","yall","biko","min","banker","printer","wow","mum","dad","wit","dat","rider","dispatch","oga","ole","gistlover","cuz","pin","sporty","taiwo","haba","nah","ment","wick","abi","sta","mrmacaronii","tunde","tho","sterl","sef","gtbank","access","account","zenith","fbnhelp","gtbankhelp","firstbankngr","simpleppearls","ubagroup","debit","credit","ubacares","zenithbank","gtb","myaccessbank","ubagroup","uba","accessmore","firstmobile","htfttipster","aabdullahi","sunixpr","obashope","justkingss","feyiiiiiiii","arinzechi","mobilepunch","slayjimmy","amooolalekan","mtnyellotop","oloriblessing","princee","chidera","emmyemmy")

custom.stop.words<- as.data.frame(custom.stop.words)

names(custom.stop.words)[names(custom.stop.words) == "custom.stop.words"] <- "word"

stop.words.2<- rbind(loughran_mcdonald_sw,custom.stop.words)

# Eliminate numbers from tweets
FUGAZ$text <- gsub('[[:digit:]]+','', FUGAZ$text, perl=T)

# Remove Punctuation
FUGAZ$text <- gsub('[[:punct:]]','', FUGAZ$text, perl=T)

# Change tweets characters to lower case
FUGAZ$text <- tolower(FUGAZ$text)

#convert text to ASCII format
FUGAZ$text <- iconv(FUGAZ$text, from = 'UTF-8', to = 'ASCII//TRANSLIT')

#filtering out comments with languages other than English
FUGAZ%>%
  mutate(language = cld2::detect_language(text))%>%
  filter( language=="en")

# Tokenisation

# Unigram
FUGAZ_tokens<-FUGAZ%>%
  unnest_tokens(word,text) %>%
  mutate(word=textstem::lemmatize_words(word))


# Tokens per Bank Name
Bank.Tokens<- FUGAZ_tokens%>%
  anti_join(fry)%>%
  anti_join(stop_words)%>%
  anti_join(stop.words.2)%>%
  filter(! word %in% sw_fry_100)%>%
  group_by(Bank_Name, word)%>%
  summarise(n=n())%>%
  bind_tf_idf(word,Bank_Name,n)%>%
  mutate(length=nchar(word))%>%
  filter(length>3 & length<12)

# Visualise Bank's frequent words
Bank.Tokens %>% 
  group_by(Bank_Name) %>%
  arrange(desc(tf_idf)) %>%
  slice(1:20) %>%
  ggplot(aes(tf_idf, word)) + geom_bar(stat= "identity", fill = "lightblue", color= "black") +   facet_wrap(~Bank_Name, scale = "free_y") + labs(title = "Top words of each Year", x = "Frequency", y = "Words")

# Bigram
Bigram <- FUGAZ%>%
  unnest_tokens(bigram,text,token = "ngrams",n=2)

# To examine the most common bigram 
Bigram%>%
  count(bigram,sort=TRUE)

Bigram_seperated <- Bigram%>%
  separate(bigram,c("word1","word2"), sep=" ")

filtered.bigram <-Bigram_seperated%>%
  filter(!word1 %in% stop.words.2$word)%>%
  filter(!word2 %in% stop.words.2$word)%>%
  filter(!word1 %in% stop_words$word)%>%
  filter(!word2 %in% stop_words$word)%>%
  mutate(length.word1=nchar(word1))%>%
  mutate(length.word2=nchar(word2))%>%
  filter(length.word1>3 & length.word1<12)%>%
  filter(length.word2>3 & length.word2<12)

# We can decide to unite the bigram
United.Bigram <- filtered.bigram%>%
  unite(bigram,word1,word2,sep=" ")

Bigram.tf_idf<- United.Bigram%>%
  count(Bank_Name,bigram)%>%
  bind_tf_idf(bigram, Bank_Name,n)%>%
  arrange(desc(tf_idf))



Bigram.tf_idf%>%
  group_by(Bank_Name)%>%
  top_n(10,tf_idf)%>%
  ggplot(aes(tf_idf, reorder(bigram,tf_idf))) + geom_bar(stat= "identity", fill = "lightblue", color = "black") +   facet_wrap(~Bank_Name, scale = "free_y", ncol =1) + labs(title = "Top words of each Category", x = "Frequency", y = "words")


# Trigram
Trigram <- FUGAZ%>%
  unnest_tokens(trigram,text,token = "ngrams",n=3)

# To examine the most common Trigram 
Trigram%>%
  count(trigram,sort=TRUE)

Trigram_seperated <- Trigram%>%
  separate(trigram,c("word1","word2", "word3"), sep=" ")

filtered.trigram <-Trigram_seperated%>%
  filter(!word1 %in% stop.words.2$word)%>%
  filter(!word2 %in% stop.words.2$word)%>%
  filter(!word3 %in% stop.words.2$word)%>%
  filter(!word1 %in% stop_words$word)%>%
  filter(!word2 %in% stop_words$word)%>%
  filter(!word3 %in% stop_words$word)%>%
  mutate(length.word1=nchar(word1))%>%
  mutate(length.word3=nchar(word3))%>%
  mutate(length.word2=nchar(word2))%>%
  filter(length.word1>3 & length.word1<10)%>%
  filter(length.word2>3 & length.word2<10)%>%
  filter(length.word3>3 & length.word3<10)

# We can decide to unite the bigram
United.Trigram <- filtered.trigram%>%
  unite(trigram,word1,word2,word3,sep=" ")

Trigram.tf_idf<- United.Trigram%>%
  count(Bank_Name,trigram)%>%
  bind_tf_idf(trigram, Bank_Name,n)%>%
  arrange(desc(tf_idf))



Trigram.tf_idf%>%
  group_by(Bank_Name)%>%
  top_n(10,tf_idf)%>%
  ggplot(aes(tf_idf, reorder(trigram,tf_idf))) + geom_bar(stat= "identity", fill = "lightblue", color = "black") +   facet_wrap(~Bank_Name, scale = "free_y", ncol =1) + labs(title = "Top words of each Category", x = "Frequency", y = "words")
```

```{r}
#Load Model
lang.model <- udpipe::udpipe_load_model("english-ewt-ud-2.5-191206.udpipe")

# No of Cores
core_no <- parallel::detectCores()-1

Bank.Tokens.ud<-udpipe::udpipe(object=lang.model,x=Bank.Tokens$word, parallel.cores=core_no, parallel.chunks=100, trace=T)


# Unigram


Bank.Tokens.df<-as.data.frame(Bank.Tokens.ud)
saveRDS(Bank.Tokens.df,file="Bank.Tokens.df")

# Initiate and Stop cluster
cluster <- parallel::makeCluster(core_no)
parallel::stopCluster(cluster)


# Combining the lemmatised udpipe output of the tweets with the other dataframe

Bank.Tokens <- Bank.Tokens %>%
  mutate(doc_id=row_number())

length(unique(Bank.Tokens$doc_id))
length(unique(Bank.Tokens.df$doc_id))

Bank.Tokens.df.2<- Bank.Tokens.df %>% 
  mutate(doc_id = as.numeric(doc_id)) %>% 
  left_join(Bank.Tokens)

# Remove short and long words from the udpipe output

Bank.Tokens.df.2$word_length <- nchar(Bank.Tokens.df.2$lemma)

quantile(Bank.Tokens.df.2$word_length, c(0.025, 0.975))

#Remove stop words after lemmatization
Tagged.Bank.df<- Bank.Tokens.df.2%>%
  mutate(word = lemma) %>% 
  anti_join(stop.words.2) %>%
  anti_join(stop_words) %>%
  anti_join(fry) %>%
  filter(! word %in% sw_fry_100)

length(Tagged.Bank.df$lemma)

# Performing TD-IF

# Word COUNT
Tokens.No <- Tagged.Bank.df%>%
  count(word, sort=TRUE)

# Count and visualise the top 10 dominant words by Bank
Bank.Name.token.count<- Tagged.Bank.df %>%
  group_by(Bank_Name) %>%
  count(word, sort=TRUE) 


# TF-IDF ON BANK TOKENS
TF_IDF<- Bank.Name.token.count%>%
  bind_tf_idf(word,Bank_Name,n)%>%
  arrange(desc(tf_idf)) 

# Inspect TF-IDF cut-precentile 
cutoffs <- quantile(TF_IDF$tf_idf, na.rm = T, c(0.025, 0.975))
quantile(TF_IDF$tf_idf, na.rm = T, c(0.025, 0.975))
```


* Plotting TF_IDF
```{r}
TF_IDF %>% filter(tf_idf <.00175) %>% 
  ggplot(., aes(x=tf_idf, fill="blue")) + 
  geom_histogram() +
  labs(title="Distribution of TF-IDF", x="TF-IDF", y="Frequency") +
  scale_y_continuous(labels=scales::comma)+ scale_fill_manual(values = c("blue")) +
  theme(legend.position="none")
```


```{r}
# Part B: Sentiment Analysis
#Create dataframe for sentiment analysis
sentiment.data <-  Bank.Tokens.df.2
```

```{r message=FALSE, warning=FALSE}
#Loading all sentiment dictionaries
nrc.dict <- tidytext::get_sentiments("nrc")
bing.dict <- tidytext::get_sentiments("bing")
afinn.dict <- tidytext::get_sentiments("afinn")
lm.dict <- tidytext::get_sentiments("loughran")
```



```{r nrc, message=FALSE, warning=FALSE}
# Extracting Sentiments
# NRC
# Joining with main dataframe
nrc.sentiment <- sentiment.data %>%
  inner_join(nrc.dict)

#Count number of words for each listing
count.nrc <- nrc.sentiment %>%
  group_by(Bank_Name)%>%
  summarise(count=n()) %>%
  ungroup()

#Visualise the data
nrc.sentiment %>%
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

#Grouping the data of sentiment
bank.tweets.nrc <- nrc.sentiment %>%
  group_by(Bank_Name, sentiment)%>%
  summarise(count=n()) %>%
  ungroup()

#Pivoting data and creating new variables
bank.tweets.nrc <- bank.tweets.nrc %>% 
  pivot_wider(names_from=sentiment, values_from=count, values_fill = 0L) %>% 
  left_join(count.nrc) %>%
  mutate(nrc.polarity = (positive-negative)/(positive+negative), #polarity variable
         nrc.anger = anger/count,
         nrc.anticipation = anticipation/count,
         nrc.disgust = disgust/count,
         nrc.fear = fear/count,
         nrc.joy = joy/count,
         nrc.negative = negative/count,
         nrc.positive = positive/count,
         nrc.sadness = sadness/count,
         nrc.surprise = surprise/count,
         nrc.trust = trust/count) 

#Remove count variable
bank.tweets.nrc$count <- NULL 
```



```{r afinn, message=FALSE, warning=FALSE}
# AFINN
#Join with main dataframe
afinn.sentiment <- sentiment.data %>%
  inner_join(afinn.dict)
#Group by the data
bank.tweet.afinn <- afinn.sentiment %>%
  group_by(Bank_Name)%>%
  summarise(afinn.sentiment=sum(value))

#Visualise the data
afinn.sentiment %>%
  count(word, value, sort = TRUE) %>%
  reshape2::acast(word ~ value, value.var = "n", fill = 0) %>%
  wordcloud::comparison.cloud(colors = c("gray20", "blue","red","green","yellow","lightblue", "darkgreen", "darkred", "orange")
                              ,max.words = 100)
```



```{r bing, message=FALSE, warning=FALSE}
# BING
#Join with main dataframe
bing.sentiment <- sentiment.data %>%
  inner_join(bing.dict)
#Count number of words for each listing
count.bing <- bing.sentiment %>%
  group_by(Bank_Name)%>%
  summarise(count=n()) %>%
  ungroup()
#Visualise the data
bing.sentiment %>%
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
#Group by data with the variables
bank.tweet.bing <- bing.sentiment %>%
  group_by(Bank_Name,sentiment)%>%
  summarise(count=n()) %>%
  ungroup()
#Transforming data and creating new variables
bank.tweet.bing <- bank.tweet.bing %>% 
  pivot_wider(names_from=sentiment, values_from=count, values_fill = 0L) %>% 
  left_join(count.bing) %>%
  mutate(bing_polarity = (positive-negative)/(positive+negative),
         bing_negative = negative/count,
         bing_positive = positive/count)
#Visualise the word cloud
bing.sentiment %>%
  count(word, sentiment, sort = TRUE) %>%
  reshape2::acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  wordcloud::comparison.cloud(colors = c("red","darkgreen"),max.words = 200)

bank.tweet.bing$count = NULL
```



```{r}
# Loughran
#Join with main dataframe
lm.sentiment<- sentiment.data %>%
  inner_join(lm.dict)
#Count number of words for each listing
count.lm <- lm.sentiment %>%
  group_by(Bank_Name)%>%
  summarise(count=n()) %>%
  ungroup()
#Visualise the data
lm.sentiment %>%
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  top_n(20)%>%
  ungroup() %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

#Group by data with the variables
bank.tweet.lm<- lm.sentiment %>%
  group_by(Bank_Name,sentiment)%>%
  summarise(count=n()) %>%
  ungroup()
#Transforming data and creating new variables
bank.tweet.lm<-bank.tweet.lm%>%
  pivot_wider(names_from=sentiment, values_from=count, values_fill = 0L) %>% 
  left_join(count.lm)%>%
  mutate(lm_polarity = (positive-negative)/(positive+negative),
         lm_constraining = constraining/count,
         lm_litigious = litigious/count,
         lm_negative = negative/count,
         lm_positive = positive/count,
         lm_uncertainty = uncertainty/count,
         lm_superfluous = superfluous/count)

#Visualise the word cloud
lm.sentiment%>%
  count(word, sentiment, sort = TRUE) %>%
  reshape2::acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  wordcloud::comparison.cloud(colors = c("gray20", "blue","red","green","yellow","lightblue"),max.words = 30)

#Checking correlation
corr <- bank.tweet.lm%>%select(constraining:superfluous)

#Visualise tabular form
round(cor(na.omit(corr)),2)

#Remove count variable
bank.tweet.lm$count <- NULL
```



```{r wordnet, message=FALSE, warning=FALSE}
# WORDNET
wordnet.sentiment <- sentiment.data %>%
  inner_join(corpus::affect_wordnet, by= c("word" = "term"))
#Count number of words for each listing
count.word <- wordnet.sentiment %>%
  group_by(Bank_Name)%>%
  summarise(count=n()) %>%
  ungroup()
#Visualise the data
wordnet.sentiment %>%
  count(word, emotion, sort = TRUE) %>%
  group_by(emotion) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n, fill = emotion)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~emotion, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
#Group by data with the variables
bank.tweet.wordnet <- wordnet.sentiment %>%
  group_by(Bank_Name,emotion)%>%
  summarise(count=n()) %>%
  ungroup()
#Transforming data and creating new variables
bank.tweet.wordnet <- bank.tweet.wordnet %>% 
  pivot_wider(names_from=emotion, values_from=count, values_fill = 0L) %>% 
  left_join(count.word) %>%
  mutate(wn_polarity = (Positive-Negative)/(Positive+Negative),
         wn_Positive = Positive/count,
         wn_Negative = Negative/count,
         wn_Ambiguous = Ambiguous/count,
         wn_Neutral = Neutral/count)
#Visualise the word cloud
wordnet.sentiment %>%
  count(word, emotion, sort = TRUE) %>%
  reshape2::acast(word ~ emotion, value.var = "n", fill = 0) %>%
  wordcloud::comparison.cloud(colors = c("blue","red","green","lightblue"),max.words = 30)
#Remove count variable
bank.tweet.wordnet$count <- NULL
```


```{r message=FALSE, warning=FALSE, results=FALSE}
# Topic modelling and LDA

Bank.df <- Tagged.Bank.df %>% 
  filter(upos %in% c("VERB", "NOUN", "ADJ")) %>%
  group_by(Bank_Name) %>%
  summarise(pos.tagged.doc=paste(lemma,collapse=" "))


subset_mda <- Tagged.Bank.df %>% 
  select(Bank_Name)

New.Bank.df<- Bank.df%>%
  left_join(subset_mda)%>%
  distinct(.)

New.Bank.df<- New.Bank.df%>%
  na.omit(.)

processes.bank.tweet<- stm::textProcessor(New.Bank.df$pos.tagged.doc,
                                          metadata = New.Bank.df,lowercase = FALSE, removestopwords = FALSE, 
                                          removenumbers = FALSE, 
                                          removepunctuation = FALSE, 
                                          ucp = FALSE,
                                          stem = FALSE)
threshold <- round(1/100 * length(processes.bank.tweet$documents),0)

out <- prepDocuments(processes.bank.tweet$documents, 
                     processes.bank.tweet$vocab,
                     processes.bank.tweet$meta,
                     lower.thresh = threshold)

```
```
