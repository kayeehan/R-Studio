library(RSelenium)
library(RmecabKo)
library(rvest)
library(stringr)
library(dplyr)
library(tm)
library(NLP)
library(KoNLP)
useNIADic()
RmecabKo::install_mecab("c:/mecab")
library(RcppMeCab)
library(e1071)
library(gmodels)
library(ggplot2)
library(ggpattern)
library(reshape2)
library(wordcloud)
library(wordcloud2)
library(remotes)
remotes::install_github("coolbutuseless/ggpattern")
remdr<-remoteDriver(remoteServerAddr='localhost',port=7777,browserName='chrome')
rm(list=ls())


remdr$open()
remdr$navigate('https://play.google.com/store/apps')
#인기순위에 있는 45개 URL 출력
html <- read_html(remdr$getPageSource()[[1]])
app_url <- c()
app_url <- html_nodes(html,xpath='//*[@id="yDmH0d"]/c-wiz[2]/div/div/div[1]/c-wiz/div/c-wiz/c-wiz[3]/c-wiz/section/div/div[3]/div/div/div/div[1]')%>%
  html_nodes('a')%>%html_attr('href')
app_url

#45개의 어플리케이션의 카테고리 URL 출력
category_url <-c()
for (i in 1:length(app_url)){
  html <- read_html(paste0('https://play.google.com',app_url[i]))
  Sys.sleep(5)
  category_url <- c(category_url, html_nodes(html,'div.VfPpkd-XPtOyb-FCjw3e.kz2bF>div>a')%>%html_attr('href'))
}
category_url <- unique(category_url)
data.frame(app_url=app_url,category_url=category_url)



#각 카테고리 인기어플 URL 출력_Rselenium을 이용

result <-data.frame()
for (i in 1:length(category_url)){
  remdr$navigate(paste0('https://play.google.com',category_url[i]))
  Sys.sleep(3)
  html <- remdr$getPageSource()[[1]]%>%read_html()
  Sys.sleep(3)
  tmp_url <- html_nodes(html,'a.Si6A0c.itIJzb')%>%
    html_attr('href')
  tmp_name <- html_nodes(html,'a.Si6A0c.itIJzb')%>%
    html_nodes('span.sT93pb.DdYX5.OnEJge')%>%html_text()
  result <- rbind(result,data.frame(category_url=category_url[i],app_url=tmp_url,app_name=tmp_name))
  Sys.sleep(1)
}

View(result)
write.csv(result,file='c:/data_bigdata/app_result.csv')
write(result$app_url,file='c:/data_bigdata/result_app_url.txt')

#카테고리 분류
result$category <- str_remove(result$category_url,'/store/apps/category/')

error_files<-c()
text_result <-data.frame()
for (i in 1:length(result$app_url))
  for (i in 101:length(result$app_url)){
    remdr$navigate(paste0('https://play.google.com',result$app_url[i]))
    Sys.sleep(sample(c(1:4),1))
    #리뷰클릭
    try(webElemButton <-remdr$findElement(using='xpath',value='//*[@id="yDmH0d"]/c-wiz[2]/div/div/div[1]/div[2]/div/div[1]/c-wiz[3]/section/header/div/div[2]/button'),silent=T)
    try(webElemButton <- remdr$findElement(using='xpath',value='//*[@id="yDmH0d"]/c-wiz[3]/div/div/div[1]/div[2]/div/div[1]/c-wiz[3]/section/header/div/div[2]/button'),silent=T)
    try(webElemButton <-remdr$findElement(using='xpath',value='//*[@id="yDmH0d"]/c-wiz[4]/div/div/div[1]/div[2]/div/div[1]/c-wiz[3]/section/header/div/div[2]/button'),silent=T)
    try(webElemButton <-remdr$findElement(using='xpath',value='//*[@id="yDmH0d"]/c-wiz[5]/div/div/div[1]/div[2]/div/div[1]/c-wiz[3]/section/header/div/div[2]/button'),silent=T)
    remdr$mouseMoveToLocation(webElement = webElemButton)
    remdr$click()
    #리뷰창 클릭  
    try(webElemButton <- remdr$findElement(using='xpath',value='//*[@id="yDmH0d"]/div[6]/div[2]/div/div/div/div/div[2]/div/div[1]/div[1]/div[1]'),silent=T)
    try(webElemButton <- remdr$findElement(using='xpath',value='//*[@id="yDmH0d"]/div[5]/div[2]/div/div/div/div/div[2]/div/div[1]/div[1]/div[1]'),silent=T)
    Sys.sleep(1)
    try(webElemButton <- remdr$findElement(using='xpath',value='//*[@id="yDmH0d"]/div[4]/div[2]/div/div/div/div/div[2]/div/div[1]/div[1]/div[1]'),silent=T)
    remdr$mouseMoveToLocation(webElement = webElemButton)
    remdr$click()
    for (j in 1:10){                        
      #스크롤링
      remdr$findElement("css", "body")$sendKeysToElement(list(key = "end"))
      Sys.sleep(1)
    }
    html <- remdr$getPageSource()[[1]]%>%read_html()
    Sys.sleep(sample(c(3:5),1))
    star <- html_nodes(html,'div.Jx4nYe>div')%>%html_attr('aria-label')%>%str_remove_all('별표 5개 만점에 |를 받았습니다.')
    date <- html_nodes(html,'span.bp9Aid')%>%html_text()
    review <- html_nodes(html,'div.h3YV2d')%>%html_text()
    text_result <- rbind(text_result, data.frame(category=result$category[i],
                                                 date=date,
                                                 app_name=result$app_name[i],
                                                 star=star,
                                                 review=review))
    
  }
nrow(text_result)
View(text_result)
write.csv(text_result,file='c:/data_bigdata/app_text_result.csv')
length(unique(result$category))
length(unique(text_result$category))
length(unique(result$app_name))
length(unique(text_result$app_name))

text_result[text_result$category %in% 'FINANCE',]
unique(text_result$category)
result$app_url
#쿼리 실행되지 않은 행 추가 실행
undo_result <- result[result$app_name %in% setdiff(result$app_name,text_result$app_name),]
View(undo_result)

remdr$open()
for (i in 1:length(undo_result$app_url))
  for (i in 1:length(undo_result$app_url)){
    remdr$navigate(paste0('https://play.google.com',undo_result$app_url[i]))
    Sys.sleep(sample(c(1:4),1))
    #리뷰클릭
    try(webElemButton <-remdr$findElement(using='xpath',value='//*[@id="yDmH0d"]/c-wiz[2]/div/div/div[1]/div[2]/div/div[1]/c-wiz[3]/section/header/div/div[2]/button'),silent=T)
    try(webElemButton <- remdr$findElement(using='xpath',value='//*[@id="yDmH0d"]/c-wiz[3]/div/div/div[1]/div[2]/div/div[1]/c-wiz[3]/section/header/div/div[2]/button'),silent=T)
    Sys.sleep(1)
    try(webElemButton <-remdr$findElement(using='xpath',value='//*[@id="yDmH0d"]/c-wiz[4]/div/div/div[1]/div[2]/div/div[1]/c-wiz[3]/section/header/div/div[2]/button'),silent=T)
    try(webElemButton <-remdr$findElement(using='xpath',value='//*[@id="yDmH0d"]/c-wiz[5]/div/div/div[1]/div[2]/div/div[1]/c-wiz[3]/section/header/div/div[2]/button'),silent=T)
    try(remdr$mouseMoveToLocation(webElement = webElemButton),silent=T)
    try(remdr$click(),silent=T)
    #리뷰창 클릭  
    try(webElemButton <- remdr$findElement(using='xpath',value='//*[@id="yDmH0d"]/div[6]/div[2]/div/div/div/div/div[2]/div/div[1]/div[1]/div[1]'),silent=T)
    try(webElemButton <- remdr$findElement(using='xpath',value='//*[@id="yDmH0d"]/div[5]/div[2]/div/div/div/div/div[2]/div/div[1]/div[1]/div[1]'),silent=T)
    Sys.sleep(1)
    try(webElemButton <- remdr$findElement(using='xpath',value='//*[@id="yDmH0d"]/div[4]/div[2]/div/div/div/div/div[2]/div/div[1]/div[1]/div[1]'),silent=T)
    try(remdr$mouseMoveToLocation(webElement = webElemButton),silent=T)
    try(remdr$click(),silent=T)
    for (j in 1:10){                        
      #스크롤링
      remdr$findElement("css", "body")$sendKeysToElement(list(key = "end"))
      Sys.sleep(1)
    }
    html <- remdr$getPageSource()[[1]]%>%read_html()
    Sys.sleep(sample(c(3:5),1))
    star <- html_nodes(html,'div.Jx4nYe>div')%>%html_attr('aria-label')%>%str_remove_all('별표 5개 만점에 |를 받았습니다.')
    date <- html_nodes(html,'span.bp9Aid')%>%html_text()
    review <- html_nodes(html,'div.h3YV2d')%>%html_text()
    try(text_result <- rbind(text_result, data.frame(category=undo_result$category[i],
                                                     date=date,
                                                     app_name=undo_result$app_name[i],
                                                     star=star,
                                                     review=review)),silent=T)
    
  }
nrow(text_result)
result[result$app_name %in% setdiff(result$app_name,text_result$app_name),'app_url']
text_result <- unique(text_result)
write.csv(text_result,file='c:/data_bigdata/app_text_result.csv')

rm(list=ls())
result <- read.csv('c:/data_bigdata/app_text_result.csv',header=T)

#1점과 5점만 출력해서 긍정, 부정으로 입력
final <- data.frame(result%>%
                      filter(star %in% c('1개','5개'))%>%
                      mutate(sentiment=ifelse(star=='1개','부정','긍정')))

#영어로만 써있는 후기,특수문자나 짧은 글자만 있는 행 삭제
final <- final[-grep('^[A-z]+\\s*[A-z]*\\s*[A-z]*\\s*[A-z]*[\\.A-z]+$',final$review),]
final <- final[-grep('^[A-z]+[\\s[:punct:]A-z]*[\\s[:punct:]A-z]+[[:punct:]A-z]+$',final$review),]
final <- final[-grep('^[A-z|[:punct:]]+\\s+[A-z|[:punct:]]*\\s*[A-z|[:punct:]]+\\s+[A-z|[:punct:]]*\\s*[A-z|[:punct:]]*[|[:punct:]A-z]+$',final$review),]
final <- final[-grep('^[[:punct:]]+\\s*[[:punct:]]*\\s*[[:punct:]]*\\s*[[:punct:]]*\\s*[[:punct:]]+$',final$review),]
final <- final[nchar(final$review)>6,]
final <- final[-grep('ㅡㅡㅡㅡㅡㅡ드ㅡ드ㅡㅡㅡㅡㅡㅡㅡㅡ',final$review),]

#비율 퍼센트 확인
nrow(subset(final,sentiment=='긍정'))/nrow(final) #48.15%
nrow(subset(final,sentiment=='부정'))/nrow(final) #51.84%


#단어 확인 위한 함수 생성
df_freq <- function(x,pattern,num){
  data.frame(table(unlist(str_extract_all(x,pattern))))%>%
    arrange(desc(Freq))%>%
    head(n=num)
}

#전처리 위한 함수 생성-별표 별로 의미있는 문자인지 확인하기 위해
#다른 항목이 12퍼센트 이하는 삭제
pre_table <- function(x,pattern,col){
  table(x[grep(pattern,x$review),col])
}
pre_per <- function(x,pattern,col){
  prop.table(table(x[grep(pattern,x$review),col]))*100
}


#감정 이모티콘 확인
pre_per(final,'ㅋㅋ|ㅎㅎ','sentiment') #삭제
pre_per(final,'\\^{2}','sentiment') #삭제
pre_per(final,'♥|♡','sentiment') #유지
pre_per(final,'ㅡㅡ','sentiment') #유지
final$review <- str_replace_all(final$review,'ㅡ{3,}',' ')
final$review <- str_replace_all(final$review,c('♥'='하트','♡'='하트','ㅡㅡ'='무표정',
                                               ':\\)'='미소',':\\('='화남'))
final$review <- str_replace_all(final$review,'[ㄱ-ㅎ\\^ㅏ-ㅠㅣ~\\?\\!ㆍㆅ;★¿°”《 「○○○/@#%※*]{1,}',' ')



#이모지 확인
df_freq(final$review,'<U\\+[\\d\\w]+>',200)
pre_per(final,'<U\\+0001F44D>','sentiment') #유지 엄지
pre_per(final,'<U\\+2764>','sentiment') #유지 하트
pre_per(final,'<U\\+11A2>','sentiment') #삭제
pre_per(final,'<U\\+0001F621>','sentiment') #유지 화남
pre_table(final,'<U\\+0001F60A>','sentiment') #유지 미소
pre_table(final,'<U\\+0001F44E>','sentiment') #삭제
pre_per(final,'<U\\+FE0F>','sentiment') #삭제

final$review <- str_replace_all(final$review,c('<U\\+0001F44D>'='엄지','<U\\+2764>'='하트',
                                               '<U\\+0001F621>'='화남','<U\\+0001F60A>'='미소'))
final$review <- str_replace_all(final$review,'<U\\+[\\d\\w]+>',' ')

#괄호 안의 단어 삭제
df_freq(final$review,'[\\(<\\[]+[\\w\\d[:punct:]]*[\\w\\d[:punct:]]+[\\)>\\]]+',100)
final$review <- str_replace_all(final$review,'[\\(<\\[]+[\\w\\d]+[\\)>\\]]+',' ')

#시간/일자 나타내는 단어 확인
df_freq(final$review,'\\d+(년|월|일|시|분|초)\\w+',100)
df_freq(final$review,'\\S+3일',100)
pre_per(final,'\\d+째','sentiment') 
pre_per(final,'1년','sentiment') 
df_freq(final$review,'\\d+(년|월|일|시|분|초)',100)
final$review <- str_replace_all(final$review,'\\d+(년|월|일|시|분|초)\\w+',' ')
final$review <- str_replace_all(final$review,'\\d+(년|월|일|시|분|초)',' ')

#숫자 + 문자 확인 -> 삭제
df_freq(final$review,'\\d+\\w+',100)
df_freq(final$review,'\\w+[[:punct:]]+\\d+',100)
df_freq(final$review,'\\w+\\d+',100)
pre_per(final,'1','sentiment') 
pre_per(final,'한개|한번','sentiment') 
pre_per(final,'하나도','sentiment') 
pre_per(final,'5점도','sentiment') #삭제

final$review <- str_replace_all(final$review,'\\d+\\w+',' ')
final$review <- str_replace_all(final$review,'\\w+[[:punct:]]+\\d+',' ')
final$review <- str_replace_all(final$review,'\\w+\\d+',' ')

#특수문자+영어 삭제
df_freq(final$review,'[A-Za-z0-9]+[\\-\\./#"\\)\\(@_)]+[A-Za-z0-9]+',50)
final$review <- str_replace_all(final$review,'[A-Za-z0-9]+[\\-\\./#"\\)\\(@_)]+[A-Za-z0-9]+',' ')

#특수문자 +한글 삭제
df_freq(final$review,'[A-z]+[[:punct:]]+[A-z가-힣]+',50) #삭제
final$review <- str_replace_all(final$review,'[A-z]+[[:punct:]]+[A-z가-힣]+',' ')

#특수문자 삭제
df_freq(final$review,'[[:punct:]ㅡ]{1}',100)
final$review <- str_replace_all(final$review,'[ㅡ[:punct:]]{1}',' ')

nrow(final)

#2개이상 공백 제거
final$review <- str_squish(final$review)

#final 파일 저장
write.csv(final,file='c:/data_bigdata/project_text_1,5.csv')

View(final)
#review가 긴 항목부터 순서대로 자름
final_select <- data.frame(final%>%
                      group_by(sentiment,category)%>%
                      mutate(rank=dense_rank(desc(nchar(review))))%>%
                      filter(rank<=170))
nrow(final_select) #8834
View(final_select)
#비율 퍼센트 확인
nrow(subset(final_select,sentiment=='긍정'))/nrow(final_select) #50.48%
nrow(subset(final_select,sentiment=='부정'))/nrow(final_select) #49.52%


#tagging 추가 후 필요 태그 확인
pos_review <- base::enc2utf8(final_select$review)%>%
  RcppMeCab::pos()
final_select$tagging <- sapply(pos_review,function(x) paste(unlist(x),collapse=' '))
View(final_select)


df_freq(final_select$tagging,'[\\w\\d]{2,}(/JX)',10) #보조사-삭제
pre_per(final_select,'조차','sentiment') 
pre_per(final_select,'밖에','sentiment')
pre_per(final_select,'까지','sentiment') 

df_freq(final_select$tagging,'[\\w\\d]{2,}(/XR)',10) #어근-유지
pre_per(final_select,'간편','sentiment') 
pre_per(final_select,'답답','sentiment') 

df_freq(final_select$tagging,'[\\w\\d]{2,}(/XSA)',10)#형용사 파생 접미사-삭제
pre_per(final_select,'스럽','sentiment') 
pre_per(final_select,'해서','sentiment') 

df_freq(final_select$tagging,'[\\w\\d]{2,}(/EC)',100)#연결 어미 -삭제
pre_per(final_select,'는데','sentiment')
pre_per(final_select,'는지','sentiment')

df_freq(final_select$tagging,'[\\w\\d]{2,}/VCN\\+EF',100) #부정지정사+종결어미-삭제(양이적음)
df_freq(final_select$tagging,'[\\w\\d]{2,}/VA\\+EF',100) #형용사+종결어미-유지
df_freq(final_select$tagging,'[\\w\\d]{2,}/VV\\+EF',100) #동사+종결어미-삭제
df_freq(final_select$tagging,'[\\w\\d]{2,}/VX\\+EF',100) #보조용언+종결어미-삭제
pre_per(final_select,'아닙니까','sentiment') #양이적음
pre_per(final_select,'편해요','sentiment')
pre_per(final_select,'됩니다','sentiment')
pre_per(final_select,'합니다','sentiment') 

df_freq(final_select$tagging,'[\\w\\d]{2,}/VCP\\+EC',100)#긍정지정사+연결어미-삭제
df_freq(final_select$tagging,'[\\w\\d]{2,}/VCN\\+EC',100)#부정지정사+연결어미-유지
df_freq(final_select$tagging,'[\\w\\d]{2,}/VA+\\+EC',100)#형용사+연결어미-유지
df_freq(final_select$tagging,'[\\w\\d]{2,}/VV+\\+EC',100) #동사+연결어미-삭제
pre_per(final_select,'인데','sentiment') #
pre_per(final_select,'아닌가요','sentiment')
pre_per(final_select,'아까워','sentiment')
pre_per(final_select,'보입니다','sentiment') 

df_freq(final_select$tagging,'[\\w\\d]{2,}/VV\\+ETN',100) #동사+명사형 전성 어미-유지
df_freq(final_select$tagging,'[\\w\\d]{2,}/VA\\+ETN',100) #형용사+명사형 전성 어미-유지
df_freq(final_select$tagging,'[\\w\\d]{2,}/VCN\\+ETN',100) #부정지정사+명사형 전성 어미-삭제
pre_per(final_select,'걸림','sentiment') #VV+ETN
pre_per(final_select,'아까움','sentiment') #VA+ETN

df_freq(final_select$tagging,'[\\w\\d]{2,}/VV\\+ETM',100) #동사+관형형 전성 어미-유지
df_freq(final_select$tagging,'[\\w\\d]{2,}/VA\\+ETM',100) #형용사+관형형 전성 어미-유지
pre_per(final_select,'만든','sentiment') #VV+ETM
pre_per(final_select,'고칠','sentiment') #VV+ETM
pre_per(final_select,'빠른','sentiment') #VA+ETM


df_freq(final_select$tagging,'[\\w\\d]{2,}(/NNP)',30)
pre_per(final_select,'리뷰','sentiment') #VV+ETM
pre_per(final_select,'모바일','sentiment')


#어플이름 불용어 처리로 삭제
stopwords_name <-  sapply(unlist(sapply(sapply(unique(final_select$app_name),
                                        function(x) strsplit(x,'\\(')),
                                 function(y) strsplit(y,'-'))),
                          function(z) strsplit(z,':'))
stopwords_name <- str_replace_all(unlist(stopwords_name),'[[:punct:]]',' ')%>%
  str_squish()
stopwords_name <- unique(stopwords_name)

#코퍼스처리
final_review_corpus <- VCorpus(VectorSource(final_select$review))
inspect(final_review_corpus)
final_review_corpus <- tm_map(final_review_corpus,removeWords,stopwords_name)

#tokenize 처리-명사
words_nn <- function(x){
  pos <- base::enc2utf8(as.character(x))%>%RcppMeCab::pos()
  str_match_all(pos,'([가-힣]{2,})(/NNG)')[[1]][,2]
  
}

#dtm으로 변경
corpus_dtm_nn <- DocumentTermMatrix(final_review_corpus,
                                 control=list(tokenize=words_nn))
colSums(as.matrix(corpus_dtm_nn))
inspect(corpus_dtm_nn)
idx_nn <- sample(2,nrow(corpus_dtm_nn),replace=T,prob=c(0.8,0.2))

#학습데이터
train_corpus_dtm_nn <- corpus_dtm_nn[idx_nn==1,]
nrow(train_corpus_dtm_nn) #7089
train_labels_nn <- final_select[idx_nn==1,'sentiment']
length(train_labels_nn) 
#테스트데이터
test_corpus_dtm_nn <- corpus_dtm_nn[idx_nn==2,]
nrow(test_corpus_dtm_nn) #1745
test_labels_nn <- final_select[idx_nn==2,'sentiment']
length(test_labels_nn)

#숫자를 yes or no로 만들기
convert_count <- function(x){
  x <- ifelse(x>0,'YES','NO')
}

train_final_nn <- apply(train_corpus_dtm_nn,MARGIN=2,convert_count)
test_final_nn <- apply(test_corpus_dtm_nn,MARGIN=2,convert_count)

nb_nn <- naiveBayes(train_final_nn,train_labels_nn)
test_predict_nn <- predict(nb_nn,test_final_nn)
sum(test_predict_nn == test_labels_nn)/length(test_labels_nn)

CrossTable(x=test_labels,y=test_predict_nn,prop.chisq = F,
           dnn=c('실제','예측값'))


capture.output(print('tokenize를 일반명사만으로 구성했을때+tf-idf 가중치 사용하지 않았을때'),
               sum(test_predict_nn == test_labels_nn)/length(test_labels_nn),
               CrossTable(x=test_labels_nn,y=test_predict_nn,prop.chisq = F,
                          dnn=c('실제','예측값')),
               file='c:/data_bigdata/review_predict_final.txt',
               append=T)


#tokenize 처리-명사 외 형용사 등 추가
words_total <- function(x){
  pos <- base::enc2utf8(as.character(x))%>%RcppMeCab::pos()
  str_match_all(pos,'([가-힣]{2,})(/XR|/MAG|/VCN\\+EC|/VA\\+EC|/VA\\+EF|/VV\\+ETN|/VA\\+ETN|/VA\\+ETM|/NNG|/VA|/VX)')[[1]][,2]
  
}

#dtm으로 변경 - 명사 외 다른 토큰 추가 + dfidf 가중치 미사용
corpus_dtm_noweight <- DocumentTermMatrix(final_review_corpus,
                                          control=list(tokenize=words_total))
colSums(as.matrix(corpus_dtm_noweight))
inspect(corpus_dtm_noweight)
idx_noweight <- sample(2,nrow(corpus_dtm_noweight),replace=T,prob=c(0.8,0.2))

#학습데이터
train_corpus_dtm_noweight <- corpus_dtm_noweight[idx_noweight==1,]
nrow(train_corpus_dtm_noweight) #7069
train_labels_noweight <- final_select[idx_noweight==1,'sentiment']
length(train_labels_noweight) 
#테스트데이터
test_corpus_dtm_noweight <- corpus_dtm_noweight[idx_noweight==2,]
nrow(test_corpus_dtm_noweight) #1765
test_labels_noweight <- final_select[idx_noweight==2,'sentiment']
length(test_labels_noweight)


#숫자를 yes or no로 만들기 
convert_count <- function(x){
  x <- ifelse(x>0,'YES','NO')
}

train_final_noweight <- apply(train_corpus_dtm_noweight,MARGIN=2,convert_count)
test_final_noweight <- apply(test_corpus_dtm_noweight,MARGIN=2,convert_count)

nb_noweight <- naiveBayes(train_final_noweight,train_labels_noweight)
test_predict_noweight <- predict(nb_noweight,test_final_noweight)
sum(test_predict_noweight == test_labels_noweight)/length(test_labels_noweight)


capture.output(print('명사, 형용사, 어미 등 토큰 추가, TF-IDF 가중치 미사용 '),
               sum(test_predict_noweight == test_labels_noweight)/length(test_labels_noweight),
               CrossTable(x=test_labels_noweight,y=test_predict_noweight,prop.chisq = F,
                          dnn=c('실제','예측값')),
               file='c:/data_bigdata/review_predict_final.txt',
               append=T)


#dtm으로 변경 - 명사 외 다른 토큰 추가+tf-idf 가중치 부여
corpus_dtm_total <- DocumentTermMatrix(final_review_corpus,
                                 control=list(tokenize=words_total,
                                              weighting=weightTfIdf))
colSums(as.matrix(corpus_dtm_total))
inspect(corpus_dtm_total)

idx_total <- sample(2,nrow(corpus_dtm_total),replace=T,prob=c(0.8,0.2))

#학습데이터
train_corpus_dtm_total <- corpus_dtm_total[idx_total==1,]
nrow(train_corpus_dtm_total) #7078
train_labels_total <- final_select[idx_total==1,'sentiment']
length(train_labels_total) 
#테스트데이터
test_corpus_dtm_total <- corpus_dtm_total[idx_total==2,]
nrow(test_corpus_dtm_total) #1756
test_labels_total <- final_select[idx_total==2,'sentiment']
length(test_labels_total)

#숫자를 yes or no로 만들기
convert_count <- function(x){
  x <- ifelse(x>0,'YES','NO')
}

train_final_total <- apply(train_corpus_dtm_total,MARGIN=2,convert_count)
test_final_total <- apply(test_corpus_dtm_total,MARGIN=2,convert_count)

nb_total <- naiveBayes(train_final_total,train_labels_total)
test_predict_total <- predict(nb_total,test_final_total)
sum(test_predict_total == test_labels_total)/length(test_labels_total)


capture.output(print('명사, 형용사, 어미 등 토큰 추가 +가중치 추가 / 최종 모델'),
               sum(test_predict_total == test_labels_total)/length(test_labels_total),
               CrossTable(x=test_labels_total,y=test_predict_total,prop.chisq = F,
                          dnn=c('실제','예측값')),
               file='c:/data_bigdata/review_predict_final.txt',
               append=T)

#파일 저장
save(nb_total,file='c:/data_bigdata/nb_total.RData')
save(corpus_dtm_total,file='c:/data_bigdata/corpus_dtm_total.RData')
load('c:/data_bigdata/corpus_dtm_total.RData')
load('c:/data_bigdata/nb_total.RData')
terms_corpus_dtm_total <- Terms(corpus_dtm_total)

#token 7개 더 추가 -> 무의미한 결과
words_total_add <- function(x){
  pos <- base::enc2utf8(as.character(x))%>%RcppMeCab::pos()
  str_match_all(pos,'([가-힣]{2,})(/XR|/VCN\\+EC|/VA\\+EC|/VA\\+EF|/VV\\+ETN|/VA\\+ETN|
                /VA\\+ETM|/NNG|/VA|/VX|/JX|/XSN|/ETM|/VCN\\+EF|/VV\\+ETM|/NNP|/NNB)')[[1]][,2]
  
}
corpus_dtm_total_add <- DocumentTermMatrix(final_review_corpus,
                                       control=list(tokenize=words_total_add,
                                                    weighting=weightTfIdf))
colSums(as.matrix(corpus_dtm_total))

idx <- sample(2,nrow(corpus_dtm_total_add),replace=T,prob=c(0.8,0.2))

#학습데이터
train_corpus_dtm_total_add <- corpus_dtm_total_add[idx==1,]
nrow(train_corpus_dtm_total_add) #7045
train_labels <- final_select[idx==1,'sentiment']
length(train_labels) 
#테스트데이터
test_corpus_dtm_total_add <- corpus_dtm_total_add[idx==2,]
nrow(test_corpus_dtm_total_add) #1789
test_labels <- final_select[idx==2,'sentiment']
length(test_labels)

#숫자를 yes or no로 만들기
convert_count <- function(x){
  x <- ifelse(x>0,'YES','NO')
}

train_final_total_add <- apply(train_corpus_dtm_total_add,MARGIN=2,convert_count)
test_final_total_add <- apply(test_corpus_dtm_total_add,MARGIN=2,convert_count)

nb_total_add <- naiveBayes(train_final_total_add,train_labels)
test_predict_total_add <- predict(nb_total_add,test_final_total_add)
sum(test_predict_total_add == test_labels)/length(test_labels)

CrossTable(x=test_labels,y=test_predict_total_add,prop.chisq = F,
           dnn=c('실제','예측값'))

capture.output(print('보조사, 고유명사, 명사 파생 접미사 등 토큰 추가 '),
               sum(test_predict_total_add == test_labels)/length(test_labels),
               CrossTable(x=test_labels,y=test_predict_total_add,prop.chisq = F,
                          dnn=c('실제','예측값')),
               file='c:/data_bigdata/review_predict.txt',
               append=T)

#시각화
#1번 donut chart 그리기
test_predict_nn <- predict(nb_nn,test_final_nn)
sum(test_predict_nn == test_labels_nn)/length(test_labels_nn)

data_nn <- data.frame(category=c('nn','other'),
                      number=c(sum(test_predict_nn == test_labels_nn)/length(test_labels_nn),
                               1-sum(test_predict_nn == test_labels_nn)/length(test_labels_nn)))
data_nn$fraction <- cumsum(data_nn$number)
data_nn$ymin <- c(0, head(data_nn$fraction,n=-1))

ggplot(data_nn,aes(ymax=fraction, ymin=ymin, xmax=4, xmin=3,fill=category,pattern=category)) +
  geom_rect_pattern(colour='white',pattern_fill='#0181D8')+
  scale_fill_manual(values=c('#0181D8','#FFFFFF')) +
  scale_pattern_manual(values=c('none','stripe'))+
  coord_polar(theta="y") +
  xlim(c(1, 4)) +
  theme_void() +
  theme(legend.position = "none")+
  annotate("text", x=1, y=0, label = paste0(round(sum(test_predict_nn == test_labels_nn)/length(test_labels_nn)*100,2),'%'), size=15,fontface=2,color = "black")

#2번
sum(test_predict_noweight == test_labels_noweight)/length(test_labels_noweight)

data_noweight <- data.frame(category=c('noweight','other'),
                            number=c(sum(test_predict_noweight == test_labels_noweight)/length(test_labels_noweight),
                                     1-sum(test_predict_noweight == test_labels_noweight)/length(test_labels_noweight)))
data_noweight$fraction <- cumsum(data_noweight$number)
data_noweight$ymin <- c(0, head(data_noweight$fraction,n=-1))

ggplot(data_noweight,aes(ymax=fraction, ymin=ymin, xmax=4, xmin=3,fill=category,pattern=category)) +
  geom_rect_pattern(colour='white',pattern_fill='#0181D8')+
  scale_fill_manual(values=c('#0181D8','#FFFFFF')) +
  scale_pattern_manual(values=c('none','stripe'))+
  coord_polar(theta="y") +
  xlim(c(1, 4)) +
  theme_void() +
  theme(legend.position = "none")+
  annotate("text", x=1, y=0, label = paste0(round(sum(test_predict_noweight == test_labels_noweight)/length(test_labels_noweight)*100,2),'%'),size=15,fontface=2,color = "black")



#3번 최종
sum(test_predict_total == test_labels_total)/length(test_labels_total)

data_total <- data.frame(category=c('total','other'),
                         number=c(sum(test_predict_total == test_labels_total)/length(test_labels_total),
                                  1-sum(test_predict_total == test_labels_total)/length(test_labels_total)))
data_total$fraction <- cumsum(data_total$number)
data_total$ymin <- c(0, head(data_total$fraction,n=-1))

ggplot(data_total,aes(ymax=fraction, ymin=ymin, xmax=4, xmin=3,fill=category,pattern=category)) +
  geom_rect_pattern(colour='white',pattern_fill='#0181D8')+
  scale_fill_manual(values=c('#FFFFFF','#0181D8')) +
  scale_pattern_manual(values=c('stripe','none'))+
  coord_polar(theta="y") +
  xlim(c(1, 4)) +
  theme_void() +
  theme(legend.position = "none")+
  annotate("text", x=1, y=0, label = paste0(round(sum(test_predict_total == test_labels_total)/length(test_labels_total)*100,2),'%'),size=15,fontface=2,color = "black")




#test set 추가 확인
final_select <- data.frame(final%>%
                             group_by(sentiment,category)%>%
                             mutate(rank=dense_rank(desc(nchar(review))))%>%
                             filter(rank %in% c(420:440)))
View(final_select)
nrow(final_select)

evaluation <- function(x,label){
  x <- str_replace_all(x,'ㅡ{3,}',' ')
  x <- str_replace_all(x,c('♥'='하트','♡'='하트','ㅡㅡ'='무표정',
                           ':\\)'='미소',':\\('='화남'))
  x <- str_replace_all(x,'[ㄱ-ㅎ\\^ㅏ-ㅠㅣ~\\?\\!ㆍㆅ;★¿°”《 「○○○/@#%※*]{1,}',' ')
  x <- str_replace_all(x,c('<U\\+0001F44D>'='엄지','<U\\+2764>'='하트',
                           '<U\\+0001F621>'='화남','<U\\+0001F60A>'='미소'))
  x <- str_replace_all(x,'<U\\+[\\d\\w]+>',' ')
  x <- str_replace_all(x,'[\\(<\\[]+[\\w\\d]+[\\)>\\]]+',' ')
  x <- str_replace_all(x,'\\d+(년|월|일|시|분|초)\\w+',' ')
  x <- str_replace_all(x,'\\d+(년|월|일|시|분|초)',' ')
  x <- str_replace_all(x,'\\d+\\w+',' ')
  x <- str_replace_all(x,'\\w+[[:punct:]]+\\d+',' ')
  x <- str_replace_all(x,'\\w+\\d+',' ')
  x <- str_replace_all(x,'[A-Za-z0-9]+[\\-\\./#"\\)\\(@_)]+[A-Za-z0-9]+',' ')
  x <- str_replace_all(x,'[A-z]+[[:punct:]]+[A-z가-힣]+',' ')
  x <- str_replace_all(x,'[ㅡ[:punct:]]{1}',' ')
  x <- str_squish(x)
  
  y <- VCorpus(VectorSource(x))
  y <- tm_map(y,removeWords,stopwords_name)
  z <- DocumentTermMatrix(y,control=list(tokenize=words_total,
                                         dictionary=terms_corpus_dtm_total,
                                         weighting=weightTfIdf))
  
  result <- apply(z,MARGIN=2,convert_count)
  predic_test <<- predict(nb_total,result)
  sum(predic_test == label)/length(label)
}
evaluation(final_select$review,final_select$sentiment)
final_select[predic_test!=final_select$sentiment,]


capture.output(print('train, test set 외에 다른 리뷰 확인 : 길이가 긴 순서 180:200행'),
               evaluation(final_select$review,final_select$sentiment),
               CrossTable(x=final_select$sentiment,y=predic_test,prop.chisq = F,
                          dnn=c('실제','예측값')),
               file='c:/data_bigdata/review_predict_final.txt',
               append=T)

data <- data.frame(category=c('total','other'),
                         number=c(sum(predic_test == final_select$sentiment)/length(final_select$sentiment),
                                  1-sum(predic_test == final_select$sentiment)/length(final_select$sentiment)))
data$fraction <- cumsum(data$number)
data$ymin <- c(0, head(data$fraction,n=-1))

ggplot(data,aes(ymax=fraction, ymin=ymin, xmax=4, xmin=3,fill=category,pattern=category)) +
  geom_rect_pattern(colour='white',pattern_fill='#0181D8')+
  scale_fill_manual(values=c('#FFFFFF','#0181D8')) +
  scale_pattern_manual(values=c('stripe','none'))+
  coord_polar(theta="y") +
  xlim(c(1, 4)) +
  theme_void() +
  theme(legend.position = "none")+
  annotate("text", x=1, y=0, label = paste0(round(sum(predic_test == final_select$sentiment)/length(final_select$sentiment)*100,2),'%'),size=15,fontface=2,color = "black")



capture.output(print('train, test set 외에 다른 리뷰 확인 : 길이가 긴 순서 420:440행'),
               evaluation(final_select$review,final_select$sentiment),
               CrossTable(x=final_select$sentiment,y=predic_test,prop.chisq = F,
                          dnn=c('실제','예측값')),
               file='c:/data_bigdata/review_predict_final.txt',
               append=T)
View(final_select[final_select$sentiment!=predic_test,])

data <- data.frame(category=c('total','other'),
                   number=c(sum(predic_test == final_select$sentiment)/length(final_select$sentiment),
                            1-sum(predic_test == final_select$sentiment)/length(final_select$sentiment)))
data$fraction <- cumsum(data$number)
data$ymin <- c(0, head(data$fraction,n=-1))

ggplot(data,aes(ymax=fraction, ymin=ymin, xmax=4, xmin=3,fill=category,pattern=category)) +
  geom_rect_pattern(colour='white',pattern_fill='#0181D8')+
  scale_fill_manual(values=c('#FFFFFF','#0181D8')) +
  scale_pattern_manual(values=c('stripe','none'))+
  coord_polar(theta="y") +
  xlim(c(1, 4)) +
  theme_void() +
  theme(legend.position = "none")+
  annotate("text", x=1, y=0, label = paste0(round(sum(predic_test == final_select$sentiment)/length(final_select$sentiment)*100,2),'%'),size=15,fontface=2,color = "black")


#4점과 2점의 경우도 구분 가능한지 확인
final <- data.frame(result%>%
                      filter(star %in% c('2개','4개'))%>%
                      mutate(sentiment=ifelse(star=='2개','부정','긍정')))

test <- final[c(1:3000),]

evaluation(test$review,test$sentiment)

capture.output(print('최종 모델링에 평점 2개, 4개 출력 후 test '),
               evaluation(test$review,test$sentiment),
               CrossTable(x=test$sentiment,y=predic_test,prop.chisq = F,
                          dnn=c('실제','예측값')),
               file='c:/data_bigdata/review_predict_final.txt',
               append=T)

data <- data.frame(category=c('total','other'),
                   number=c(sum(predic_test == test$sentiment)/length(test$sentiment),
                            1-sum(predic_test == test$sentiment)/length(test$sentiment)))
data$fraction <- cumsum(data$number)
data$ymin <- c(0, head(data$fraction,n=-1))

ggplot(data,aes(ymax=fraction, ymin=ymin, xmax=4, xmin=3,fill=category,pattern=category)) +
  geom_rect_pattern(colour='white',pattern_fill='#0181D8')+
  scale_fill_manual(values=c('#FFFFFF','#0181D8')) +
  scale_pattern_manual(values=c('stripe','none'))+
  coord_polar(theta="y") +
  xlim(c(1, 4)) +
  theme_void() +
  theme(legend.position = "none")+
  annotate("text", x=1, y=0, label = paste0(round(sum(predic_test == test$sentiment)/length(test$sentiment)*100,2),'%'),size=15,fontface=2,color = "black")




#wordcloud 생성
#전체적으로 많이 나온 말 확인

total_noun <- data.frame(table(unlist(lapply(final[,'tagging'], 
                               function(x) str_match_all(x,'([가-힣]{2,})/NNG')[[1]][,2]))))
wordcloud2(total_noun)


total_noun%>%arrange(desc(Freq))

#긍정과 부정의 compare wordcloud 생성
positive <-data.frame(table(final[final$sentiment=='긍정','tagging']%>%
  str_match_all(pos,'([가-힣]{2,})(/XR|/MAG|/VCN\\+EC|/VA\\+EC|/VA\\+EF|/VV\\+ETN|/VA\\+ETN|/VA\\+ETM|/NNG|/VA|/VX)')[[1]][,2]))

positvie <- data.frame(table(unlist(lapply(final[final$sentiment=='긍정','tagging'], 
       function(x) str_match_all(x,'([가-힣]{2,})/NNG')[[1]][,2]))))

negative <- data.frame(table(unlist(lapply(final[final$sentiment=='부정','tagging'], 
                                           function(x) str_match_all(x,'([가-힣]{2,})/NNG')[[1]][,2]))))

positvie$sentiment <-'긍정'
negative$sentiment <- '부정'
df<-rbind(positvie,negative)

df_compar<-acast(df,Var1~sentiment,value.var='Freq',fill=0)
head(df_compar)
library(wordcloud)
windows(width=15,height=15) 
wordcloud::comparison.cloud(df_compar,colors=c('#D94925','#0181D8'),
                            title.colors = c('#D94925','#0181D8'),
                            title.size = 1,
                            max.words = 1000)


