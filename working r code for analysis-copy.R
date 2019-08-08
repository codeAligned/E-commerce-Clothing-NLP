#Working R code for Analysis
#Read Data
women_clothes = read.csv('Womens Clothing E-Commerce Reviews.csv',stringsAsFactors = FALSE,na.strings='')
str(women_clothes)
head(women_clothes)
summary(women_clothes)

#check for NAs
sapply(women_clothes, function(x)sum(is.na(x)))
#omit all NAs
women_clothes_cleaned = na.omit(women_clothes)
#check for NAs omitted
sapply(women_clothes_cleaned, function(women_clothes_cleaned)sum(is.na(x)))
#check data
str(women_clothes_cleaned)
#write cleaned dataset
write.csv(women_clothes_cleaned, 'women_clothes_cleaned.csv', row.names = F)


#read cleaned data
women_clothes_cleaned=read.csv('women_clothes_cleaned.csv',stringsAsFactors = FALSE)
install.packages('dplyr')
install.packages('ggplot2')
install.packages('ggthemes')
install.packages('stringr')
install.packages('qdap')
install.packages('tidytext')
install.packages('wordcloud')

#Data Processing
#create a corpus
library(tm)
corpus <- Corpus(VectorSource(data$Review.Text))
#convert to lower case
corpus = tm_map(corpus,FUN = content_transformer(tolower))
#remove punctuation
corpus = tm_map(corpus,FUN = removePunctuation)
#remove stopwords
corpus = tm_map(corpus,FUN = removeWords,c(stopwords('english')))
#remove whitespace
corpus = tm_map(corpus,FUN = stripWhitespace)
#create a dictionary
dict = findFreqTerms(DocumentTermMatrix(Corpus(VectorSource(data$Review.Text))),lowfreq = 0)
dict_corpus = Corpus(VectorSource(dict))
#stem document
corpus = tm_map(corpus,FUN = stemDocument)
#create a document term matrix
dtm = DocumentTermMatrix(corpus)
tdm = TermDocumentMatrix(corpus)
#remove sparse terms
xdtm = removeSparseTerms(dtm, sparse=0.9)
xtdm = removeSparseTerms(tdm, sparse=0.9)
#complete stems
xdtm = as.data.frame(as.matrix(xdtm))
colnames(xdtm) = stemCompletion(x = colnames(xdtm),dictionary = dict_corpus,type='prevalent')
colnames(xdtm) = make.names(colnames(xdtm))
#browse tokens
sort(colSums(xdtm),decreasing = T)

#clustering
d = dist(x = xtdm,method = 'euclidean') 
clusters = hclust(d = d,method='complete')
plot(clusters)


# Descriptive Analysis
# reviews by class
library(magrittr)
library(dplyr)
library(gridExtra)
library(grid)
most_reviewed_products <- clothes %>%
  select(Class.Name) %>%
  filter(Class.Name != '') %>%
  group_by(Class.Name) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(10)

colnames(most_reviewed_products)[1] <- "Class of Product"
colnames(most_reviewed_products)[2] <- "Number of Reviews"
table1 <- tableGrob(most_reviewed_products)
grid.arrange(table1,ncol=1)

#ratings percentage by Department
library(ggplot2)
phisto <- clothes %>% filter(!is.na(Department.Name), Department.Name != 'Trend') %>% mutate(Department.Name = factor(Department.Name)) %>% group_by(Department.Name) %>% count(Rating) %>% mutate(perc = n/sum(n))
phisto %>% ggplot(aes(x=Rating, y = perc*100, fill = Department.Name)) + geom_bar(stat = 'identity', show.legend = FALSE) + facet_wrap(~Department.Name) + ylab('Percentage of reviews (%)') + geom_text(aes(label=round(perc*100,2)))

#reviews by age
qplot(Age, data=clothes, geom="density", alpha=I(0.5), main="Reviewers' Age", xlab="Age", fill=I("lightblue"),  alpha = .3, show.legend = NA)
clothes$bins <- cut(clothes$Age, breaks = c(0,20,40,60,80,100), labels = c("Centennials(0-20)","Gen Y(21-40)","Gen X(41-60)","Baby Boomers(61-80)","Traditionalists(81-100)"))
age_groups <- clothes %>%
  select(bins,Age) %>%
  group_by(bins) %>%
  summarise(count = n())

#create bar chart
ggplot(data=age_groups,aes(x=bins,y=count)) + 
  geom_bar(stat = "identity",fill="lightblue") + 
  labs(x = 'Age Groups', y = 'Number of Reviews')

#Class by Age
#facet wrap by age and look at Dept distribution in each
clothes=filter(clothes, Class.Name!='Casual bottoms')
clothes=filter(clothes, Class.Name!='Chemises')
ages <- clothes  %>% select(Clothing.ID, Age, Class.Name) %>% mutate(Age_group = ifelse(Age < 20, '0-19', ifelse(Age < 40, '20-39', ifelse(Age < 60, '40-59', ifelse(Age < 80, '60-79', ifelse(Age < 100, '80-99')))))) 

ages <- ages %>% mutate(Age_group = factor(Age_group), Class.Name = factor(Class.Name, levels = rev(c( 'Trend', 'Dresses', 'Intimates', 'Jackets', 'Blouses', 'Fine gauge', 'Jeans','Knits','Layering','Legwear','lounge','Outerwear','Pants','Shorts','Skirts','Sleep','Swearter','Swim'))))

ages %>% filter(Age < 80& Age> 20) %>% group_by(Age_group) %>% count(Class.Name) %>% ggplot(aes(Class.Name, n, fill = Age_group)) + geom_bar(stat='identity', show.legend = FALSE) + facet_wrap(~Age_group, scales = 'free') + xlab('Department') + ylab('Number of Reviews') + geom_text(aes(label = n), hjust = .73) + coord_flip()


#Sentiment Analysis
#view data
str(women_clothes_cleaned)
women_clothes_cleaned$Title[1:3]
women_clothes_cleaned$Review.Text[1:3]
#fix data type
women_clothes_cleaned$Title = as.character(women_clothes_cleaned$Title)
women_clothes_cleaned$Review.Text = as.character(women_clothes_cleaned$Review.Text)
#Character, Words and Sentences for all Reviews
mean_char = mean(nchar(women_clothes_cleaned$Review.Text))
mean_char
##Words across all reviews
library(stringr)
mean_words = mean(str_count(string = women_clothes_cleaned$Review.Text,pattern = '\\S+'))
mean_words
#Review length and Ratings
##Are longer review associated with better ratings
##Review length in characters
cor(nchar(women_clothes_cleaned$Review.Text), women_clothes_cleaned$Rating)
cor.test(nchar(women_clothes_cleaned$Review.Text), women_clothes_cleaned$Rating)
##Are longer review associated with more positive feedback
cor(nchar(women_clothes_cleaned$Review.Text), women_clothes_cleaned$Positive.Feedback.Count)
##Are better ratings associated with more positive feedback
cor(women_clothes_cleaned$Rating, women_clothes_cleaned$Positive.Feedback.Count)
#Most Common Words
library(qdapDictionaries)
library(qdapRegex)
library(qdapTools)
library(RColorBrewer)
library(qdap)
freq_terms(text.var = women_clothes_cleaned$Review.Text,top = 25)
##look at the top25 list after removing the stopwords
freq_terms(text.var=women_clothes_cleaned$Review.Text,top=25,stopwords = Top200Words)
#Words in review
library(dplyr)
library(tidytext)
women_clothes_cleaned %>%
  select(X,Review.Text)%>%
  group_by(X)%>%
  unnest_tokens(output = word,input=Review.Text)%>%
  count()
#Total words
women_clothes_cleaned %>%
  select(X,Review.Text)%>%
  group_by(X)%>%
  unnest_tokens(output = word,input=Review.Text)%>%
  ungroup()%>%
  count()
#Word Lexicons
##Bing Lexicon
women_clothes_cleaned%>%
  group_by(X)%>%
  unnest_tokens(output = word, input = Review.Text)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment)
##Positive and Negative Words in Reviews
women_clothes_cleaned%>%
  group_by(X)%>%
  unnest_tokens(output = word, input = Review.Text)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment)%>%
  count()
##Plot it
library(ggplot2)
library(ggthemes)
women_clothes_cleaned%>%
  group_by(X)%>%
  unnest_tokens(output = word, input = Review.Text)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment)%>%
  count()%>%
  ggplot(aes(x=sentiment,y=n,fill=sentiment))+geom_col()+theme_economist()+guides(fill=F)
##Does higher proportion of positive reviews mean higher rating?
women_clothes_cleaned %>%
  select(X,Review.Text,Rating)%>%
  group_by(X)%>%
  unnest_tokens(output=word,input=Review.Text)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(Rating,sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n))%>%
  ggplot(aes(x=Rating,y=proportion,fill=sentiment))+geom_col()+theme_economist()
##Does higher proportion of positive reviews mean recommendation?
women_clothes_cleaned %>%
  select(X,Review.Text,Recommended.IND)%>%
  group_by(X)%>%
  unnest_tokens(output=word,input=Review.Text)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(Recommended.IND,sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n))%>%
  ggplot(aes(x=Recommended.IND,y=proportion,fill=sentiment))+geom_col()+theme_economist()
##Correlation between Positive Words and Review helpfulness
women_clothes_cleaned%>%
  group_by(X)%>%
  unnest_tokens(output = word, input = Review.Text)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(X,Rating)%>%
  summarize(positivity = sum(sentiment=='positive')/n())%>%
  ungroup()%>%
  summarize(correlation = cor(positivity,Rating))
##Correlation between Positive Words and Positive Feedback counts
women_clothes_cleaned%>%
  group_by(X)%>%
  unnest_tokens(output = word, input = Review.Text)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(X,Positive.Feedback.Count)%>%
  summarize(positivity = sum(sentiment=='positive')/n())%>%
  ungroup()%>%
  summarize(correlation = cor(positivity,Positive.Feedback.Count))
##nrc lexicon
get_sentiments('nrc')%>%
  group_by(sentiment)%>%
  count()
##Emotions in Reviews
women_clothes_cleaned%>%
  group_by(X)%>%
  unnest_tokens(output = word, input = Review.Text)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(sentiment)%>%
  count()
##plot
library("ggplot2")
library(ggthemes)
women_clothes_cleaned%>%
  group_by(X)%>%
  unnest_tokens(output = word, input = Review.Text)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(sentiment)%>%
  count()%>%
  ggplot(aes(x=reorder(sentiment,X = n),y=n,fill=sentiment))+geom_col()+guides(fill=F)+coord_flip()+theme_wsj()
##Ratings of each Review based on Emotions Expressed
women_clothes_cleaned%>%
  group_by(X)%>%
  unnest_tokens(output = word, input = Review.Text)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(X,sentiment,Rating)%>%
  count()
##Ratings of all Reviews based on Emotion Expressed
women_clothes_cleaned%>%
  group_by(X)%>%
  unnest_tokens(output = word, input = Review.Text)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(X,sentiment,Rating)%>%
  count()%>%
  group_by(sentiment, Rating)%>%
  summarize(n = mean(n))%>%
  data.frame()
##plot it
women_clothes_cleaned%>%
  group_by(X)%>%
  unnest_tokens(output = word, input = Review.Text)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(X,sentiment,Rating)%>%
  count()%>%
  group_by(sentiment, Rating)%>%
  summarize(n = mean(n))%>%
  ungroup()%>%
  ggplot(aes(x=Rating,y=n,fill=Rating))+
  geom_col()+
  facet_wrap(~sentiment)+
  guides(fill=F)+coord_flip()
#Correlation between emotion expressed and review rating
women_clothes_cleaned%>%
  group_by(X)%>%
  unnest_tokens(output = word, input = Review.Text)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(X,sentiment,Rating)%>%
  count()%>%
  ungroup()%>%
  group_by(sentiment)%>%
  summarize(correlation = cor(n,Rating))
##Scatterplot of relationship
women_clothes_cleaned%>%
  group_by(X)%>%
  unnest_tokens(output = word, input = Review.Text)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(X,sentiment,Rating)%>%
  count()%>%
  ungroup()%>%
  group_by(sentiment)%>%
  ggplot(aes(x=Rating,y=n))+geom_point()+facet_wrap(~sentiment)+geom_smooth(method='lm',se=F)
#afinn Lexicon
as.data.frame(get_sentiments('afinn'))[1:25,]
##examine the sentiment of all reviews.
women_clothes_cleaned %>%
  select(X,Review.Text)%>%
  group_by(X)%>%
  unnest_tokens(output=word,input=Review.Text)%>%
  inner_join(get_sentiments('afinn'))%>%
  summarize(reviewSentiment = mean(score))%>%
  ungroup()%>%
  summarize(min=min(reviewSentiment),max=max(reviewSentiment),median=median(reviewSentiment),mean=mean(reviewSentiment))
##plot it
women_clothes_cleaned %>%
  select(X,Review.Text)%>%
  group_by(X)%>%
  unnest_tokens(output=word,input=Review.Text)%>%
  inner_join(get_sentiments('afinn'))%>%
  summarize(reviewSentiment = mean(score))%>%
  ungroup()%>%
  ggplot(aes(x=reviewSentiment,fill=reviewSentiment>0))+
  geom_histogram(binwidth = 0.1)+
  scale_x_continuous(breaks=seq(-5,5,1))+scale_fill_manual(values=c('tomato','seagreen'))+
  guides(fill=F)+
  theme_wsj()
#wordcloud
install.packages("wordcloud")
library(wordcloud)
wordcloudData = 
  women_clothes_cleaned%>%
  group_by(X)%>%
  unnest_tokens(output=word,input=Review.Text)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(freq = n())%>%
  arrange(desc(freq))%>%
  ungroup()%>%
  data.frame()
set.seed(617)
wordcloud(words = wordcloudData$word,wordcloudData$freq,scale=c(2,0.5),max.words = 250,colors=brewer.pal(9,"Spectral"))
##comparison cloud to contrast positive and negative words in the reviews
library(tidyr)
wordcloudData = 
  women_clothes_cleaned%>%
  group_by(X)%>%
  unnest_tokens(output=word,input=Review.Text)%>%
  anti_join(stop_words)%>%
  inner_join(get_sentiments('bing'))%>%
  ungroup()%>%
  count(sentiment,word,sort=T)%>%
  spread(key=sentiment,value = n,fill=0)%>%
  data.frame()
rownames(wordcloudData) = wordcloudData[,'word']
wordcloudData = wordcloudData[,c('positive','negative')]
set.seed(617)
comparison.cloud(term.matrix = wordcloudData,scale = c(2,0.5),max.words = 200, rot.per=0)


#explore the data
setwd('~/Desktop/Data')
women_clothes = read.csv('Womens Clothing E-Commerce Reviews.csv',stringsAsFactors = FALSE,na.strings='')
str(women_clothes)
head(women_clothes)
summary(women_clothes)

#check for NAs
sapply(women_clothes, function(x)sum(is.na(x)))
#omit all NAs
women_clothes_cleaned = na.omit(women_clothes)
#check for NAs omitted
sapply(women_clothes_cleaned, function(women_clothes_cleaned)sum(is.na(x)))
#check data
str(women_clothes_cleaned)
#write cleaned dataset
write.csv(women_clothes_cleaned, 'women_clothes_cleaned.csv', row.names = F)


#read cleaned data
women_clothes_cleaned=read.csv('women_clothes_cleaned.csv',stringsAsFactors = FALSE)
install.packages('dplyr')
install.packages('ggplot2')
install.packages('ggthemes')
install.packages('stringr')
install.packages('qdap')
install.packages('tidytext')
install.packages('wordcloud')

#==============================================================
#predictive Analysis
#create a corpus
library(tm)
corpus=Corpus(VectorSource(women_clothes_cleaned$Review.Text))

#clean text
#convert to lower case
#remove punctuation
#remove whitespace
corpus=tm_map(corpus,FUN = content_transformer(tolower))
corpus[[45]]1
corpus = tm_map(corpus,FUN = removePunctuation)
corpus = tm_map(corpus, FUN = removeWords, c(stopwords('english')))
corpus = tm_map(corpus, FUN = stripWhitespace)

term_count = freq_terms(corpus,20)
plot(term_count)



#create a dictionary
dict=findFreqTerms(DocumentTermMatrix(Corpus(VectorSource(women_clothes_cleaned$Review.Text))),lowfreq=0)
dict_corpus=Corpus(VectorSource(dict))
corpus=tm_map(corpus,FUN = stemDocument)

#create a document term matrix
dtm = DocumentTermMatrix(corpus)
dtm
dim(dtm)

#remove sparse terms. 
#Remove infrequently occurring terms. 
#Here we will remove terms that appear in fewer than 5% of the reviews.
xdtm=removeSparseTerms(dtm,sparse = 0.95)
xdtm

#complete stems
xdtm=as.data.frame(as.matrix(xdtm))
colnames(xdtm)=stemCompletion(x=colnames(xdtm),dictionary = dict_corpus,type='prevalent')
colnames(xdtm)=make.names(colnames(xdtm))

sort(colSums(xdtm),decreasing=T)

#document term matrix-tfidf
dtm_tfidf=DocumentTermMatrix(x=corpus,control = list(weighting=function(x) weightTfIdf(x,normalize=F)))
xdtm_tfidf = removeSparseTerms(dtm_tfidf,sparse = 0.95)
xdtm_tfidf = as.data.frame(as.matrix(xdtm_tfidf))
colnames(xdtm_tfidf) = stemCompletion(x = colnames(xdtm_tfidf),dictionary = dict_corpus,type='prevalent')
colnames(xdtm_tfidf) = make.names(colnames(xdtm_tfidf))
sort(colSums(xdtm_tfidf),decreasing = T)


library(tidyr); library(dplyr); library(ggplot2); library(ggthemes)
data.frame(term = colnames(xdtm),tf = colMeans(xdtm), tfidf = colMeans(xdtm_tfidf))%>%
  arrange(desc(tf))%>%
  top_n(20)%>%
  gather(key=weighting_method,value=weight,2:3)%>%
  ggplot(aes(x=term,y=weight,fill=weighting_method))+geom_col(position='dodge')+coord_flip()+theme_economist()

#combine data
women_clothes_cleaned_data=cbind(Rating = women_clothes_cleaned$Rating, xdtm)
women_clothes_cleaned_tfidf = cbind(Rating = women_clothes_cleaned$Rating, xdtm_tfidf)

sort(colSums(women_clothes_cleaned_data[women_clothes_cleaned_data$Rating==5,-women_clothes_cleaned_data$Rating]),decreasing=T)
sort(colSums(women_clothes_cleaned_tfidf[women_clothes_cleaned_data$Rating==5,-women_clothes_cleaned_tfidf$Rating]),decreasing=T)
#Split data using tf
set.seed(617)
split = sample(1:nrow(women_clothes_cleaned_data),size = 0.7*nrow(women_clothes_cleaned_data))
train = women_clothes_cleaned_data[split,]
test = women_clothes_cleaned_data[-split,]
library(rpart);library(rpart.plot)
tree = rpart(Rating~.,train)
rpart.plot((tree))
#cart prediction
pred_tree=predict(tree,newdata = test)
rmse_tree=sqrt(mean((pred_tree - test$Rating)^2));rmse_tree
#1.007508

#regression
reg=lm(Rating~.,train)
summary(reg)
pred_reg=predict(reg, newdata = test)
rmse_reg=sqrt(mean((pred_reg-test$Rating)^2));rmse_reg
#0.9183897

#split data of tfidf
set.seed(617)
split = sample(1:nrow(women_clothes_cleaned_tfidf),size = 0.7*nrow(women_clothes_cleaned_tfidf))
train = women_clothes_cleaned_tfidf[split,]
test = women_clothes_cleaned_tfidf[-split,]
library(rpart);library(rpart.plot)
tree=rpart(Rating~.,train)
rpart.plot(tree)

#cart prediction
pred_tree=predict(tree,newdata = test)
rmse_tree=sqrt(mean((pred_tree-test$Rating)^2));rmse_tree 
#1.007508

#regression
reg = lm(Rating~.,train)
summary(reg)

#regression prediction
pred_reg=predict(reg,newdata = test)
rmse_reg=sqrt(mean((pred_reg-test$Rating)^2));rmse_reg
#0.9183897

##Predict sentiment of reviews
#Associate 4 and 5-score with a positive sentiment and 1- and 2-score reviews with a negative sentiment.
#3-star reviews would be neutral.

table(women_clothes_cleaned$Rating)
women_clothes_cleaned_sub=subset(women_clothes_cleaned, Rating!=3)
women_clothes_cleaned_sub$positive=as.factor(women_clothes_cleaned_sub$Rating>3)
table(women_clothes_cleaned_sub$positive)

#repeat the previous steps of preparing data.
corpus1=Corpus(VectorSource(women_clothes_cleaned_sub$Review.Text))
corpus1 = tm_map(corpus1, content_transformer(tolower))
corpus1 = tm_map(corpus1, removePunctuation)
corpus1 = tm_map(corpus1, removeWords,stopwords("english"))
corpus1 = tm_map(corpus1, stemDocument)

dtm1 = DocumentTermMatrix(corpus1)



#remove sparse terms. 
#Remove infrequently occurring terms. 
#Here we will remove terms that appear in fewer than 5% of the reviews.
xdtm1=removeSparseTerms(dtm1,sparse = 0.95)

#complete stems
xdtm1=as.data.frame(as.matrix(xdtm1))
colnames(xdtm1)=stemCompletion(x=colnames(xdtm1),dictionary = dict_corpus,type='prevalent')
colnames(xdtm1)=make.names(colnames(xdtm1))
#now add our dependent variable back and let's take a look at the result.
xdtm1$positive=women_clothes_cleaned_sub$positive

#split data
library(caTools)
set.seed(617)
split =sample.split(xdtm1$positive,SplitRatio=0.7)
xdtm1$split=split
train = subset(xdtm1,split==TRUE)
test = subset(xdtm1,split==TRUE)
nrow(train)

#calculate baseline accuracy
table(train$positive)
#FALSE  TRUE 
#1436 10603
10603/nrow(train)
#0.880721 

#classification tree
tree1=rpart(positive~.,data=train,method='class')
prp(tree1)
predict=predict(tree1,newdata = test,type='class')
table(test$positive,predict)
#predict
#       FALSE  TRUE
#FALSE   188  1248
#TRUE    115 10488
(188+10488)/nrow(test)#0.8867846 the cart model has 0.6% improvement over
#the baseline model. 

#cross validation-enhance model
library(caret)
library(e1071)
numFolds=trainControl(method='cv',number=10)
cpGrid=expand.grid(.cp=seq(0.001,0.01,0.001))
train(positive~., data=train,method='rpart',trControl=numFolds,tuneGrid=cpGrid)
#The cross validation gave me the optimal parameter cp = 0.002
#so I will now re-build the tree with this parameter.

tree_improved=rpart(positive~.,data=train,method='class',cp=0.002)
prp(tree_improved)

#the tree got more splits now gives us a good overview of which words 
#are used to make the split decisions, and hence which words contribute
#most to the positive/negative sentiment.

predict_improved=predict(tree_improved,newdata=test,type='class')
table(test$positive,predict_improved)
#predict_improved
#  FALSE  TRUE
#FALSE   314  1122
#TRUE    117 10486
(314+10486)/nrow(test) 
#0.8970845 improved 1% over the baseline.

