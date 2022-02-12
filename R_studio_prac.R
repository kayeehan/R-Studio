employees<-read.csv('C:/data_bigdata/emp_mod.csv',header=T)
departments<-read.csv('C:/data_bigdata/dept.csv',header=T)
sales<-read.csv('C:/data_bigdata/fruits_sales.csv',header=T)
blood<-read.csv('C:/data_bigdata/blood.csv',header=T)
survey<-read.csv('C:/data_bigdata/survey.csv',header=F)
seoul<-readLines('C:/data_bigdata/seoul.txt')
install.packages('plyr')
library(plyr)
install.packages("dplyr")
library(dplyr)
install.packages("sqldf")
library(sqldf)
library(RColorBrewer)
install.packages("plotrix")
library(plotrix)
install.packages("ggfortify") #data.frame을 전환없이 바로 시계열 그래프로 그릴 수 있음.
library(ggfortify)
install.packages("zoo")
library(zoo)
library(stringr) #str_detect 외 str 함수 사용을 위해 사용
library(reshape2)
#형태소 분석을 위한 library
install.packages('rJava')
library(rJava)
install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), type = "binary")
install.packages('remotes')
remotes::install_github(force = T,'haven-jeon/KoNLP', upgrade = "never",
                        INSTALL_opts=c("--no-multiarch"))
library(KoNLP)
useNIADic()
install.packages("rvest") #스크래핑을 위한 library
library(rvest)
install.packages('jsonlite')
library(jsonlite)
library(RSelenium)
remdr<-remoteDriver(remoteServerAddr='localhost',port=7777,browserName='chrome')



#[문제] COMMISSION_PCT가 NA인 사원들 급여 평균과 
#COMMISSION_PCT가 NA가 아닌 사원들의 급여 평균 차를 구하세요

x<-employees[is.na(employees$COMMISSION_PCT),'SALARY']
y<-employees[!is.na(employees$COMMISSION_PCT),'SALARY']
mean(x,na.rm=T)-mean(y,na.rm=T)
#SQL
sqldf('select avg(e1.salary)-avg(e2.avg_sal)
      from (select avg(salary) avg_sal
            from employees
            where COMMISSION_PCT is not null) e2, employees e1
      where COMMISSION_PCT is null')

#[문제] COMMISSION_PCT가 NA인 사원들 중 급여가 제일 낮은 사람의 정보를 출력하세요.
employees%>%
  dplyr::filter(is.na(COMMISSION_PCT))%>%
  dplyr::mutate(rank=dense_rank(SALARY))%>%
  dplyr::filter(rank==1)

#sql
sqldf('select *
      from employees
      where commission_pct is null
      and salary = (select min(salary)
                          from employees
                          where commission_pct is null)')


#[문제]자신의 부서 평균 급여 보다 더 많이 받는 사원들의 EMPLOYEE_ID, DEPARTMENT_ID, SALARY를 출력해주세요.
x<-aggregate(SALARY~DEPARTMENT_ID,employees,mean)
y<-merge(employees,x,by='DEPARTMENT_ID')
y
y[y$SALARY.x>y$SALARY.y,c('EMPLOYEE_ID','DEPARTMENT_ID','SALARY.x')]

x<-plyr::ddply(employees,'DEPARTMENT_ID',mutate,AVG=mean(SALARY))
x[x$SALARY>x$AVG,c('EMPLOYEE_ID','DEPARTMENT_ID','SALARY','AVG')]

plyr::ddply(employees,'DEPARTMENT_ID',subset,SALARY>mean(SALARY))[,c('EMPLOYEE_ID','DEPARTMENT_ID','SALARY')]
mean(employees$SALARY) #6461.832

#[문제] 110번 사원의 job_id와 동일한 사원들 중에 110번 사원의 급여보다 더 많이 받는 사원들의 정보를 추출하세요.
subset(employees,
       JOB_ID==employees[employees$EMPLOYEE_ID==110,'JOB_ID']&
         SALARY>employees[employees$EMPLOYEE_ID==110,'SALARY'])

#[문제] 동일한 날짜에 입사한 사원들의 정보를 출력해주세요.
x<-employees[,c('EMPLOYEE_ID','HIRE_DATE')]
y<-NULL
for (i in 1:nrow(employees)){
  y<-c(y,ifelse(x[,2][i]%in%x[,2][-i],x[,1][i],NA))
}
y
employees[employees$EMPLOYEE_ID %in% y,]


#[문제]sales의 fruits 컬럼을 생성하고 qty개수만큼의 과일이름을 출력해주세요.
sales$name[16]
fruits<-NULL
for (i in c(1:nrow(sales))){
  n<-NULL
  for (j in 1:sales$qty[i]){
    n<-paste(n,sales$name[i])
  }
  fruits<-c(fruits,n)
}
fruits
sales$fruits<-fruits
sales

#[문제]사원 번호를 입력 값으로 받아서  'LAST_NAME님의 급여는 SALARY만원 입니다.'를 출력하고,
#사원이 존재하지 않을 때에는 '사원이 존재하지 않습니다'를 출력하는 find함수를 작성하세요.

find <- function(x){
  if(!'employees' %in% ls()){
    employees<-read.csv('C:/data_bigdata/emp_mod.csv',header=T)
  }
  if(!x %in% employees$EMPLOYEE_ID){
    print('사원이 존재하지 않습니다')
  }else{
    paste0(employees[employees$EMPLOYEE_ID==x,'LAST_NAME'],'님의 급여는 ',employees[employees$EMPLOYEE_ID==x,'SALARY'],'만원 입니다.')
  }
}
find(103)
find(10)

# [문제]ann_sal 새로운 컬럼을 생성하세요. 값은 commission_pct NA 면 salary * 12, 
# 아니면 (salary * 12) + (salary * 12 * commission_pct) 입력한 후
# ann_sal컬럼의 값에 내림차순 기준으로 10위까지 출력해주세요.

employees%>%
  mutate(ann_sal=ifelse(is.na(COMMISSION_PCT),SALARY*12,(SALARY*12)+(SALARY*12*COMMISSION_PCT)),
        rank=dplyr::dense_rank(desc(ann_sal)))%>%
  dplyr::filter(rank<=10)%>%
  dplyr::arrange(rank)
  

#[문제] 1~100의 정수를 차례로 출력하되 3의 배수에서는 숫자 대신 *을 출력하는 FOR문 코드를 작성하시오
#1
num<-NULL
for(i in 1:100){
  n<-NULL
  if(i%%3==0){
    n<-c(n,'*')
  }else{
    n<-c(n,i)
  }
  num<-c(num,n)
}
num
#2
for(i in 1:100){
  if(i%%3!=0){
    cat(i,' ', append = T)
  }else{
    cat('*',' ', append= T)
  }
}


#[문제] 급여가 3000 이하이고 또는 JOB_ID가 ST_CLERK인 사원들의 employee_id, salary, job_id, department_name을 출력해주세요
x<-merge(employees,departments,by='DEPARTMENT_ID')[,c('EMPLOYEE_ID','SALARY','JOB_ID','DEPARTMENT_NAME')]
x[x$SALARY<=3000&x$JOB_ID=='ST_CLERK',]


#[문제] 30번 또는 50번 부서 사원들이면서 급여는 5000이상인 사원들의 employee_id, salary, department_id를 출력하세요.

3.1) dplyr 패키지에 있는 함수를 이용하세요
employees%>%
  dplyr::filter(DEPARTMENT_ID %in% c(30,50) & SALARY>=5000)%>%
  dplyr::select(EMPLOYEE_ID,SALARY,DEPARTMENT_ID)

3.2) sqldf 함수를 이용하세요
sqldf::sqldf('SELECT employee_id, salary, department_id
             FROM employees
             WHERE department_id in (30,50)
             AND salary >= 5000')


#[문제] 입력값으로 들어온 수에 대한 짝수의 합을 구하는 hap함수를 생성하세요 hap(1:10)
s<-NULL
hap <- function(...){
  x<-c(...)
  for (i in x){
    if(i%%2==0){
      s<-c(s,i)
    }
  }
  return(sum(s))
}
hap(1:10)


#[문제] JOB_ID별 급여를 가장 적게 받는 사원들의 employee_id, last_name, salary, job_id를 추출해 주세요.
#1
x<-employees%>%
  dplyr::group_by(JOB_ID)%>%
  dplyr::mutate(min_sal=min(SALARY))%>%
  dplyr::filter(SALARY==min_sal)%>%
  dplyr::select(EMPLOYEE_ID,LAST_NAME,SALARY,JOB_ID)%>%
  arrange(4)
data.frame(x)
#2
plyr::ddply(employees[,c('EMPLOYEE_ID','LAST_NAME','SALARY','JOB_ID')],
            'JOB_ID',subset,SALARY==min(SALARY))

#[문제] 부서별 최고급여를 받고 있는 사원의 employee_id, last_name, salary, department_id를 출력해주세요.
#1
employees%>%
  dplyr::group_by(DEPARTMENT_ID)%>%
  dplyr::mutate(max_sal=max(SALARY))%>%
  dplyr::filter(SALARY==max_sal)%>%
  dplyr::select(EMPLOYEE_ID,LAST_NAME,SALARY,DEPARTMENT_ID)%>%
  dplyr::arrange(DEPARTMENT_ID)
#2
plyr::ddply(employees[,c('EMPLOYEE_ID','LAST_NAME','SALARY','DEPARTMENT_ID')],
            'DEPARTMENT_ID',subset,SALARY==max(SALARY))
#3
sqldf("select *
        from (select employee_id, last_name, salary, department_id, max(salary) over(partition by department_id) max_sal
              from employees)
              where salary = max_sal")


#[문제] 홀수달에 입사한 사원들과 짝수달에 입사한 사원들을 구분하고 급여를 내림차순으로 출력하시오
  x<-employees%>%
    dplyr::mutate(MON=ifelse(lubridate::month(HIRE_DATE)%%2==0,'even','odd'))%>%
    dplyr::group_by(MON)%>%
    dplyr::mutate(RANK=dense_rank(desc(SALARY)))%>%
    dplyr::arrange(MON,RANK)
  data.frame(x)

#[문제] 년도별(행) 분기별(열) 총액급여, 행의 합, 열의 합을 출력, 분기별,년도별 평균도 출력 
x <- tapply(employees$SALARY,list(lubridate::year(employees$HIRE_DATE),lubridate::quarter(employees$HIRE_DATE)),sum,default = 0)
x <- data.frame(x)
names(x) <- c('1분기','2분기','3분기','4분기')
x$분기총합 <- apply(x,MARGIN = 1,sum)
x
x1 <- x
x <- rbind(x,apply(x,MARGIN=2,sum))
x
x$분기평균 <- apply(x[,c('1분기','2분기','3분기','4분기')],MARGIN=1,mean)
x
x1
x <- rbind(x,apply(x1,MARGIN=2,mean))
x
rownames(x)[c(9,10)] <- c('년도총합','년도평균')
x
x[NROW(x),length(x)] <- 0
trunc(x)


#[문제] 부서별 최대급여를 받고있는 사원들의 정보를 출력(단, COMMISSION_PCT가 NA값이 아닌 것만)하시오
#1
x<-employees%>%
  dplyr::filter(!is.na(COMMISSION_PCT))%>%
  dplyr::group_by(DEPARTMENT_ID)%>%
  dplyr::mutate(m_s=max(SALARY))%>%
  dplyr::filter(SALARY==m_s)
data.frame(x)
#2
a <- aggregate(SALARY ~ DEPARTMENT_ID, employees,max)
a <- merge(a,employees,by=c('DEPARTMENT_ID','SALARY'))
a[!is.na(a$COMMISSION_PCT),]  

#[문제] last_name의 글자의 수가 10이상인 사원의 employee_id, last_name 출력하세요.(22-01-11)
employees%>%
  dplyr::filter(nchar(LAST_NAME)>=10)%>%
  dplyr::select(EMPLOYEE_ID,LAST_NAME)


#[문제] 구구단 5단 ~ 7단을 보기와 같이 출력해주세요.
# [보기]
# 5 * 1 = 5    6 * 1 = 6    7 * 1 = 7 
# 5 * 2 = 10    6 * 2 = 12    7 * 2 = 14
#1 컬럼별로 구분
gugudan<-NULL
for (i in 5:7) {
  gu<-NULL
  for (j in 1:9){
    gu<-c(gu,paste0(i,' * ',j,' = ',i*j))
  }
  gugudan<-cbind(gugudan,gu)
}
gugudan
#2
for(i in 1:9){
  for(j in 5:7){
    cat(j,' * ',i,' = ',i*j,'\t')
  }
  cat('\n')
}

#[문제]  70번 부서 사원이면서 급여는 10000이상 받는 사원들의 LAST_NAME, HIRE_DATE,SALARY,DEPARTMENT_ID를 출력해주세요. 
#1
employees%>%
  dplyr::filter(DEPARTMENT_ID==70&SALARY>=10000)%>%
  dplyr::select(LAST_NAME,HIRE_DATE,SALARY,DEPARTMENT_ID)
#2
employees[which(employees$DEPARTMENT_ID==70 & employees$SALARY >= 10000),
          c('LAST_NAME', 'HIRE_DATE','SALARY','DEPARTMENT_ID')]
#3
subset(employees,DEPARTMENT_ID==70 & SALARY >= 10000,
       select=c(LAST_NAME, HIRE_DATE,SALARY,DEPARTMENT_ID))


#[문제] 사원의 이름과 급여, 급여가 5000이하면 "C", 급여가 12000이하면 "B", 급여가 17000이하면 "A", 나머지 경우 "S"를 출력해주세요.
#1
employees%>%
  dplyr::mutate(grade=ifelse(SALARY<=5000,'C',ifelse(SALARY<=12000,'B',ifelse(SALARY<=17000,'A','S'))))%>%
  dplyr::select(LAST_NAME,SALARY,grade)
#2
sqldf("select last_name, salary,
              case
                 when salary <= 5000 then 'C'
                 when salary <= 12000 then 'B'
                 when salary <= 17000 then 'A'
                 else 'S'
                 end 등급
      from employees")

#[문제] commission_pct에 NA가아닌 사원들의 이름,잡아이디,급여,부서아이디,부서명을 출력해주세요. 단 부서가 없는 사원도 출력해주세요.
x<-merge(employees[!is.na(employees$COMMISSION_PCT),],departments,by='DEPARTMENT_ID',all.x=T)
x[,c('LAST_NAME','JOB_ID','SALARY','DEPARTMENT_ID','DEPARTMENT_NAME')]

#[문제] 분산을 구하는 var 함수를 만드세요.
var1<-function(...){
  x<-na.omit(c(...))
  va<-(sum((x-mean(x))**2))/(length(x)-1)
  return(va)
}
var1(1:10)
var(1:10)
var(c(1,2,3,5,8))



#[문제] fruits_sales.csv file 읽어 들인 후 과일 이름별, 년도별 판매량을 구한 후 행의 합과 열의 합을 구하세요
#1
x<-tapply(sales$qty,list(sales$name,sales$year),sum)
r<-apply(x,MARGIN=2,sum)
x<-rbind(x,r)
c<-apply(x,MARGIN = 1,sum)
res<-cbind(x,c)
res<-data.frame(res)
rownames(res)[nrow(x)]<-'합'
colnames(res)[ncol(x)]<-'합'
res
#2
x<-tapply(sales$qty,list(sales$year,sales$name),sum,default=0)
class(x)
x<-data.frame(x)
x$행의합<-apply(x,1,sum)
x
x<-rbind(x,apply(x,2,sum))
rownames(x)[nrow(x)]<-'열의 합'
x
#[문제] JOB_ID별 인원수를 출력해주세요.
#1
x<-employees%>%
  dplyr::group_by(JOB_ID)%>%
  dplyr::count()
data.frame(x)
#2
aggregate(EMPLOYEE_ID~JOB_ID,employees, NROW)

#[문제] last_name, salary, 급여가 10000  이상이면 H, 5000이상 10000보다 작으면 M, 나머지는 L가 입력되어 있는 새로운 컬럼을 생성하세요.  
employees%>%
  dplyr::mutate(grade=ifelse(SALARY>=10000,'H',ifelse(SALARY>=5000,'M','L')))%>%
  dplyr::select(LAST_NAME,SALARY,grade)


#[문제] employees 파일에서 입사년도별, job_id별 입사자수를 구하고, 세로형 그래프로 표현해주세요.
y<-xtabs(~JOB_ID+lubridate::year(HIRE_DATE),employees)
addmargins(y,1)
barplot(height=y,
        main='입사년도별, JOB_id별 인원수',
        las=2,
        ylim = c(0,30),
        density = 60,
        col=rainbow(ncol(y)))
legend('topleft',legend=rownames(y),
       col=brewer.pal(12,'Set3'),
       pch=15, cex=0.7)
display.brewer.all() #색상 보기
?barplot

#[문제]employees중 manager가 아닌 사원들의 부서별 SALARY 평균을 출력하고 이를 세로형 그래프로 나타내주세요.(부서 없는 정보 제외)
#sqldf
x<-sqldf('SELECT department_id, avg_sal
FROM(SELECT department_id, avg(salary) over(partition by department_id) avg_sal
      FROM employees e
      WHERE not exists (SELECT 1
                        FROM employees
                        WHERE e.employee_id = manager_id)
        union
        SELECT department_id, NULL
        FROM departments)
WHERE avg_sal is not null
AND department_id is not null')
x
#dplyr
x<-employees%>%
  filter(!EMPLOYEE_ID %in% MANAGER_ID)%>%
  group_by(DEPARTMENT_ID)%>%
  summarise(mean(SALARY))
x<-data.frame(x)
x<-x[-11,]
x<-round(x)

install.packages("dplyr")
library(dplyr)
emp<-employees%>%
  dplyr::filter(!EMPLOYEE_ID %in% MANAGER_ID)%>%
  dplyr::group_by(DEPARTMENT_ID)%>%
  dplyr::summarise(mean(SALARY))
emp<-data.frame(emp)[!is.na(emp$DEPARTMENT_ID),]
names(emp)<-c('DEPT_ID','MEAN_S')
#최대값 최소값에 대해서는 다른색 지정하고 싶을때
colors<-NULL
for (i in 1:nrow(emp)){
  if(emp$MEAN_S[i]==max(emp$MEAN_S)){
    colors<-c(colors,'red1')
  }else if(emp$MEAN_S[i]==min(emp$MEAN_S)){
    colors<-c(colors,'yellow')
  }else{
    colors<-c(colors,'orange2')
  }
}

barplot(height=emp$MEAN_S,
        names.arg = emp$DEPT_ID,
        main='부서별 비관리자 평균급여',
        xlab='부서번호',ylab='급여',
        col=colors)

library(ggplot2)
emp$DEPT_ID<-as.character(emp$DEPT_ID)
ggplot(emp,aes(x=DEPT_ID,y=MEAN_S,fill=as.factor(MEAN_S)))+ #fill을 factor로 변경해줘야함.
  geom_bar(stat='identity')+
  labs(title='부서별 비관리자 평균급여',xlab='부서번호',ylab='급여')+
  theme(plot.title=element_text(face='bold'))+
  scale_fill_manual(values=colors) #dept_id의 순서랑 row순서가 맞지 않아서 다름

emp$SAL<-c(ifelse(emp$MEAN_S==max(emp$MEAN_S),'max',ifelse(emp$MEAN_S==min(emp$MEAN_S),'min','mid')))
colors1<-c('max'='red',
           'min'='yellow',
           'mid'='orange2')
ggplot(emp,aes(x=DEPT_ID,y=MEAN_S,fill=SAL))+
  geom_bar(stat='identity')+
  labs(title='부서별 비관리자 평균급여',xlab='부서번호',ylab='급여')+
  theme(plot.title=element_text(face='bold'))+
  scale_x_discrete(limits=emp$DEPT_ID)+
  scale_fill_manual(values=colors1)

#[문제] 부서별 최고 급여자들의 급여와 최저 급여자들의 급여를 세로형 그래프로 비교하세요.
x<-employees%>%
  group_by(DEPARTMENT_ID)%>%
  summarise(max=max(SALARY),
            min=min(SALARY))
x<-data.frame(x)
x<-x[-which(is.na(x)),]
y<-t(x)
colnames(y)<-y[1,]
z<-y[2:3,]
result<-barplot(height = z,
        beside=T,
        xlab = '부서번호',ylab='급여(만원)',
        ylim=c(0,25000),
        legend.text=rownames(z),
        col=c('khaki4','ivory'))


#[문제]kosis에서 하루 1회이상 외식률 2010년~2019년 자료를 추출하여, 연령별2별로 그래프를 출력하세요.
eat<-read.csv('C:/data_bigdata/하루_1회_이상_외식률_추이.csv',header=T)
head(eat)

eating<-eat[eat$특성별.1. %in% c('연령별2','소득수준별'),c('특성별.1.','특성별.2.','X2010.1','X2011.1',
                                                'X2012.1','X2013.1','X2014.1','X2015.1','X2016.1','X2017.1','X2018.1','X2019.1')]
eat_age<-eating[eating$특성별.1.=='연령별2',]
eat_age<-eat_age[,-1]
eat_age1<-data.frame(t(eat_age))
colnames(eat_age1)<-eat_age1[1,]
eat_age1<-eat_age1[-1,]
rownames(eat_age1)<-c(2010:2019)
eat_age1
eat_age1[,1:ncol(eat_age1)]<- lapply(eat_age1[,1:ncol(eat_age1)],as.numeric)
str(eat_age1)

barplot(height = as.matrix(eat_age1),
        beside=F,
        col=terrain.colors(14),
        las=2,
        ylim = c(0,500),
        legend.text = rownames(eat_age1))
#[문제]kosis에서 하루 1회이상 외식률 2010년~2019년 자료를 추출하여, 연령별1별로 원형그래프를 출력하세요.
#이때, plots화면에 년도별로 다른 그래프를 그리세요.
eating<-eat[eat$특성별.1.=='연령별1',c('특성별.2.','X2010.1','X2011.1','X2012.1','X2013.1','X2014.1','X2015.1','X2016.1','X2017.1','X2018.1','X2019.1')]
rownames(eating)<-eating[,1]
eating<-eating[,-1]
names(eating)<-c(2010:2019)
eating[,1:ncol(eating)]<-lapply(eating[,1:ncol(eating)],as.numeric)
str(eating)

par(mfrow=c(2,5))
for (i in 1:ncol(eating)){
  pie(eating[,i],labels = paste0(rownames(eating),' ',eating[,i],'%'),
      main=paste(colnames(eating)[i],'년도 연령별 외식률'),
      cex=1)
}
graphics.off()

#[문제]employees에서 각 입사연도별 JOB_ID 비율의 원형그래프를 한화면에 입사연도 개수만큼 출력하세요.
#min label 나오지 않게 개선 필요
head(employees)
emp<-tapply(employees$EMPLOYEE_ID,list(employees$JOB_ID,lubridate::year(employees$HIRE_DATE)),length,default=0)
str(emp)
emp<-data.frame(emp)
p_emp<-t
colnames(emp)<-c(2001:2008)
par(mfrow=c(4,2))
for(i in 1:ncol(emp)){
  pie(emp[,i],labels=paste0(rownames(emp),'(',round(emp[emp[,i]!=max(emp[i]),i]/sum(emp[,i]),2),'%)'),
      main=paste(colnames(emp)[i],'년도 JOB_ID별 입사인원'),
      cex=1,
      col=brewer.pal(8,'Set3'))
}
           

#[문제]sale파일에서 연도별 판매량을 plot을 사용해서 비교하세요.
apple<-sales[sales$name=='apple',c('year','qty')]
banana<-sales[sales$name=='banana',c('year','qty')]
orange<-sales[sales$name=='orange',c('year','qty')]
berry<-sales[sales$name=='berry',c('year','qty')]

plot(apple$year,apple$qty,ylim=c(0,20),xlab='년도',ylab='',axes=F,type='o',col='red2')
lines(banana$year,banana$qty,type='o',col='yellow3')
lines(orange$year,orange$qty,type='o',col='orange2')
lines(berry$year,berry$qty,type='o',col='purple2')
legend('topleft',legend = c('apple','banana','orange','berry'),pch = 15,
       col=c('red2','yellow3','orange2','purple2'))
axis(1,at=2014:2017)
axis(2)

#[문제]3,6,9가 들어간 숫자에는 숫자 대신 '짝'을 출력하는 v369 함수를 만드세요.

#369에 짝 출력
v369<-function(x){
  y<-NULL
  k<-NULL
  y<-strsplit(as.character(x), "") #숫자를 쪼개기
  z<-NULL
  for(i in 1:length(unlist(y))){
    z<-c(z,which(unlist(y)[[i]]==c('3','6','9')))
  }    
  if(sum(z)>=1){
      k<-'짝'
    }else{
      return(x)
    }
  return(k)
}
v369(1)
v369(31)
#369 개수만큼 짝 출력
v369<-function(x){
  y<-NULL
  k<-NULL
  y<-strsplit(as.character(x), "")
  z<-NULL
  for(i in 1:length(unlist(y))){
    z<-c(z,unlist(y)[[i]] %in% c('3','6','9'))
  }    
  if(sum(z)>=1){
    for(j in 1:sum(z)){
      k<-paste0(k,'짝')
    }
  }else{
    return(x)
  }
  return(k)
}
v369(333)
v369(6)

#grep 이용

v369<-function(x){
  y<-NULL
  k<-NULL
  y<-strsplit(as.character(x), "")
  z<-NULL
  for(i in 1:length(unlist(y))){
    z<-c(z,grep(unlist(y)[[i]],c('3','6','9'),value=F))
  }    
  if(sum(as.integer(z))>=1){
    for(j in 1:sum(as.integer(z))){
      k<-paste0(k,'짝')
    }
  }else{
    return(x)
  }
  return(k)
}
v369(133)

#str_count 이용

v369<-function(x){
  z<-NULL
  k<-NULL
  z<-str_count(x,'(3|6|9)')
  for (i in 1:z){
    if(z>=1){
      k<-paste0(k,'짝')
    }else{
      return(x)
    }
  }
  return(k)
}
v369(133)
v369(1739)



#[문제]숫자를 넣으면 해당 숫자의 구구단이 출력되는 gugudan 함수를 만드세요.

gugudan<-function(x){
  for (i in x){
    y<-NULL
    for (j in 1:9){
      cat(y,i,' x ',j,' = ',i*j,'\n')
    }
  }
}
gugudan(2)


#AirPassengers로 년도별 탑승객 수 시각화하기
AirPassengers
library(reshape2)
str(AirPassengers)
names(AirPassengers)
class(AirPassengers)

plot(AirPassengers,ylab="AirPassengers('000s)",lwd=2)

?timestamp
air<-data.frame(.preformat.ts(AirPassengers))
air
air$year<-rownames(air)

airs<-melt(air,measure.vars =c(colnames(air)[-13]))
head(airs)
min(airs$value)
library(ggplot2)
str(airs)

ggplot(airs,aes(x=year,y=as.numeric(value),fill=variable))+
  theme_bw()+
  geom_bar(stat='identity')+
  scale_y_continuous(breaks=0,name='count')

#[문제] 부서별 인원수를 막대그래프로 시각화하고, 최대인원과 최소인원을 출력해주세요.
x<-employees%>%
  group_by(DEPARTMENT_ID)%>%
  dplyr::count(DEPARTMENT_ID)
x<-data.frame(x) 
x<-x[!is.na(x$DEPARTMENT_ID),]
library(ggplot2)
x$DEPARTMENT_ID<-as.character(x$DEPARTMENT_ID)
ggplot(x,aes(x=DEPARTMENT_ID,y=n,fill=n))+
  geom_bar(stat='identity')+
  scale_x_discrete(limits=x$DEPARTMENT_ID)+
  geom_text(data=subset(x,n==max(n)|n==min(n)),aes(label=n))


#[문제] exam.csv file 을 읽어들여,학생들의 이름을 기준으로 과목점수를 그룹형 막대그래프로 생성하세요.  
#(범례와 barlabel도 기입해주세요.)
exam<-read.csv('C:/data_bigdata/exam.csv',header=T)
exam
ggplot(exam,aes(x=name,y=grade,fill=subject))+
  geom_bar(stat='identity')+
  geom_text(aes(label=grade),position=position_stack(vjust=0.5))+
  scale_fill_discrete(name='과목',labels=c('Python','R','SQL'))+
  theme(legend.title.align=0.5,
        legend.box.background = element_rect())

#[문제] cost.txt 데이터를 읽어들여, 보기처럼 분석해주세요.
[보기]
도수  상대도수  누적도수
50점이상~60점미만     2     0.04        2
60점이상~70점미만    13     0.26       15
70점이상~80점미만    16     0.32       31
80점이상~90점미만     7     0.14       38
90점이상~100점미만    7     0.14       45
100점이상             5     0.10       50

cost<-read.table('C:/data_bigdata/cost.txt')
class(cost)
cost<-as.matrix(cost)
dim(cost)<-c(50,1)
cut(cost,breaks = c(50, 60, 70, 80, 90, 100,Inf),
    labels = c("50점이상~60점미만", "60점이상~70점미만", "70점이상~80점미만", "80점이상~90점미만","90점이상~100점미만","100점이상"),
    right = TRUE)
x<-cut(cost,breaks=seq(50,110,10),right=F,labels=c('50이상~60미만','60이상~70미만',
                                                   '70이상~80미만','80이상~90미만',
                                                   '90이상~100미만','100이상~110미만'))
x<-table(x)
y<-cbind(x,prop.table(x))
colnames(y)<-c('도수','상대도수')
y<-data.frame(y)
y$누적도수<-cumsum(y$도수)
y

#[문제] 코로나 바이러스 데이터에서 가장 최근 날짜의 국가명 Australia의 Province.state별로 
#확진자, 사망자, 회복자 수를 ggplot을 이용해 그룹형 막대그래프로 표시해주세요.(꾸미는건 자유)
covid<-read.csv('C:/data_bigdata/covid_19_clean_complete.csv',header=T)
head(covid)
str(covid)
covid$Date<-as.Date(covid$Date)
aus_cov<-covid[covid$Country.Region=='Australia',]
head(aus_cov)
aus<-aus_cov[aus_cov$Date==max(aus_cov$Date),c('Province.State','Confirmed','Deaths','Recovered')]
aus
library(reshape2)
a<-melt(aus,measure.vars=c('Confirmed','Deaths','Recovered'))
library(ggplot2)
ggplot(a,aes(x=Province.State,y=value,fill=variable))+
  geom_bar(stat='identity',position=position_dodge())+
  theme(axis.text.x = element_text(angle=90,face = 'bold'))+
  labs(title='Australia 주별 covid 현황수',x='주',y='수(명)')+
  theme(plot.title=element_text(face='bold',hjust=0.5))+
  theme(axis.title.x=element_text(face='italic',color='darkblue'))+
  theme(axis.title.y=element_text(face='italic',color='darkblue'))+
  theme(legend.title.align=0.5,
        legend.box.background = element_rect())+
  scale_fill_discrete(labels=c('확진자수','사망자수','회복자수'))


#[문제] blood.csv 파일을 읽어 들여서 도수분포표를 작성 하시고 pie chart도 생성해 주세요.
BLOODTYPE   CN   PCT 누적도수
A      7   0.35        7
AB     4   0.20        11  
B      3   0.15        14
O      6   0.30        15

aggregate(NAME~BLOODTYPE,blood,length)
library(plyr)
freq_b<-plyr::count(blood,'blood$BLOODTYPE')
names(freq_b)<-c('BLOODTYPE','CN')
freq_b$PCT<-prop.table(freq_b$CN)
freq_b$누적도수<-cumsum(freq_b$CN)
freq_b
#1
pie(freq_b$PCT,labels=paste0(freq_b$PCT*100,'%'),main='혈액형별 비율분포',col=brewer.pal(4,'Set3'))
legend("bottomleft", freq_b$BLOODTYPE,fill=brewer.pal(4,'Set3'))
col=brewer.pal(4,'Set3')
#2
ggplot(freq_b, aes(x="", y=PCT, fill=BLOODTYPE))+ 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + 
  theme_void() +
  labs(x = NULL, y = NULL, fill = NULL) + 
  scale_fill_brewer(palette="BuPu", direction=-1) +
  geom_text(aes(label = paste0(PCT*100,'%' )), size=3, position=position_stack(vjust=0.5)) 

#[문제] 같은 날짜에 입사한 사원들의 급여를 날짜를 기준으로 ggplot을 이용해 라인 그래프로시각화 해주세요.
emp<-employees%>%
  dplyr::select(HIRE_DATE,SALARY)%>%
  dplyr::arrange(HIRE_DATE)
emp$HIRE_DATE<-as.Date(emp$HIRE_DATE)
str(emp)
ggplot(emp,aes(x=HIRE_DATE,y=SALARY,fill=SALARY))+
  geom_line()+
  scale_x_date(date_labels = '%Y-%m',date_breaks = '3 months')+ #date_breaks='1 days'
  theme(axis.text.x=element_text(angle=90))


#[문제] 사원의 last_name, 근무일수를 출력하세요.
str(employees)
employees$HIRE_DATE<-as.Date(employees$HIRE_DATE)
employees%>%
  mutate(DAYS=Sys.Date()-HIRE_DATE)%>%
  select(LAST_NAME,DAYS)


#[문제] commission_pct에 NA인 사원들의 LAST_NAME, SALARY, DEPARTMENT_ID,DEPARTMENT_NAME을 출력해주세요.
emp<-merge(employees,departments,by='DEPARTMENT_ID')
emp[is.na(emp$COMMISSION_PCT),c('LAST_NAME','SALARY','DEPARTMENT_ID','DEPARTMENT_NAME')]


#[문제] JOB_ID별 급여를 많이 받는 사원 1등만 추출해 주세요.
employees%>%
  group_by(JOB_ID)%>%
  filter(SALARY==max(SALARY))

#[문제] fruits_sales.csv를 이용해 년도별 과일 판매량을 스택형 막대그래프로 생성해주세요. 범례와 barlabels도 함께 생성해주세요. 
sales
library(ggplot2)
ggplot(sales,aes(x=year,y=qty,fill=name))+
  geom_bar(stat='identity',position=position_stack())+
  geom_text(aes(label=qty),position = position_stack(0.5))+
  scale_fill_manual(name='fruits',values=c('red1','yellow2','purple','orange2'))

#[문제] 부서별 인원수를  ggplot을 이용해서 시각화 해주세요.
emp<-employees%>%
  group_by(DEPARTMENT_ID)%>%
  count()
str(emp)
emp<-data.frame(emp)
emp$DEPARTMENT_ID<-as.character(emp$DEPARTMENT_ID)
emp<-emp[!is.na(emp$DEPARTMENT_ID),]
ggplot(emp,aes(x=DEPARTMENT_ID,y=n,fill=SAL))+
  geom_bar(stat='identity')+
  labs(title='부서별 인원수',x='부서코드',y='')+
  geom_text(aes(y=n,label=n))+
  scale_fill_manual(values=colors1,guide='none')

emp$SAL<-c(ifelse(emp$n==max(emp$n),'max',ifelse(emp$n==min(emp$n),'min','mid')))
colors1<-c('max'='red',
           'min'='yellow',
           'mid'='orange2')

#[문제] repeat문을 이용해서 7단을 출력해주세요.
i<-1
repeat{
  cat('7 *',i,' = ',7*i,'\n')
  if(i==9){
    break
  }
  i<-i+1
}


#[문제] median을 사용하지 않고 중앙값을 리턴하는 함수를 만들어주세요.
med<-function(...){
  x<-c(...)
  if(length(x)%%2==0){
    mean(x[length(x)/2],x[(length(x)/2)+1])
  }else{
    x[(length(x)+1)/2]
  }
}
med(1,2,3,4)


#[문제] 군별로 plot차트를 그리시오(감염병컬럼은 제거, x축은 날짜, na값 -> 0)
data<-read.csv('C:/data_bigdata/감염병_군별_발생현황.csv',header=T)
data
head(data)
str(data)
data<-data[-1,-2]

data[,2:ncol(data)]<-lapply(data[,2:ncol(data)],as.integer)
data<-replace(data,is.na(data),0)
colnames(data)<-c('군별','2015','2016','2017','2018','2019')

result<-data%>%
  group_by(군별)%>%
  dplyr::summarise('2015'=sum(data$`2015`),
                   '2016'=sum(data$`2016`),
                   '2017'=sum(data$`2017`),
                   '2018'=sum(data$`2018`),
                   '2019'=sum(data$`2019`))
result<-data.frame(result)
rownames(result)<-result[,1]
result<-result[,-1]
colnames(result)<-c(2015:2019)
result
barplot(height=as.matrix(result),
        names.arg =colnames(result),
        ylim=c(0,800000),
        col=rainbow(4))
legend("topleft",legend = rownames(result),col=rainbow(4),pch=15,cex=0.8)
options(scipen = 999)

#[문제] 전출지_전입지_시도_별_이동자수.csv파일을 읽어와 광역시가 들어간 지역만 추출하여 "광역시"를 
#"metropolitan"으로 바꿔준 후 막대그래프를 그리시오(단 x축은 광역시, facet_grid()옵션을 이용하여 날짜별로 나누기) 
move<-read.csv('C:/data_bigdata/시도_별_이동자수.csv',header=T)
head(move)
library(stringr)
moving<-move%>%
  filter(str_detect(전입지별,'광역시')&전출지별=='서울특별시')%>%
  select(전입지별,X2015,X2016,X2017,X2018,X2019,X2020,X2021)
move[str_detect(move$전입지별,'광역시')&move$전출지별=='서울특별시',c('전입지별','X2015','X2016','X2017','X2018','X2019','X2020','X2021')]
move[grep('광역시$',move$전입지별),]

moving$전입지별<-gsub('광역시','metropolitan',moving$전입지별)
moving
colnames(moving)<-c('전입지별',2015:2021)
str(moving)
moving[,2:ncol(moving)]<-lapply(moving[,2:ncol(moving)],as.integer)
mov<-melt(moving)
library(ggplot2)
ggplot(data=mov,aes(x=전입지별,y=value,fill=variable))+
  geom_bar(stat='identity')+
  facet_grid(rows=mov$variable)+
  labs(title='연도별 전입자수',x='',y='전입자수')+
  theme(axis.text.x=element_text(angle=90))+
  scale_fill_brewer(palette = "Dark2")+
  theme(legend.position='none')

#[문제] employees테이블에서 부서인원수를 ggplot을 이용해서 막대그래프로 시각화해주세요
emp<-aggregate(EMPLOYEE_ID~DEPARTMENT_ID,employees,length)
library(ggplot2)
emp$DEPARTMENT_ID<-as.character(emp$DEPARTMENT_ID)
ggplot(emp,aes(x=DEPARTMENT_ID,y=EMPLOYEE_ID,fill=EMPLOYEE_ID))+
  geom_bar(stat='identity')+
  theme(legend.position = 'none')+
  labs(title='부서별 인원수',x='부서번호',y='인원수')

#[문제] exam.csv file에는 학생들의 시험점수가 있습니다. 학생들의 R 점수를 막대그래프로 출력해주세요
exam
ggplot(exam,aes(x=name,y=grade,fill=name))+
  geom_bar(data=subset(exam,subject=='R'),stat='identity')+
  theme(legend.position = 'none')+
  labs(title='R점수',x='',y='')+
  theme(plot.title=element_text(face='bold',hjust=0.5,size=20))+
  scale_fill_brewer(palette = 'Spectral')

#[문제] 다음의 전화번호 리스트에서 02로 시작하는 서울 번호만 출력해주세요.
number <- '전화1 02)123-4567 내선번호 123 전화2 031)456-7778 내선번호 456 전화3 024-457-1111 내선번호 777
전화4 02-4574578 내선번호 025 전화5 070-0700-0700 내선번호 031'
str_extract_all(number,'[0]+[2]+[)-]+\\d+\\W*\\d*')
str_extract_all(number,'(02)+[)-]+\\d+\\-*\\d+')

#[문제] 다음 도시에서 한글로 적힌 도시만 출력해서, 빈도수 2 이상인 도시만 시각화해주세요.
city <- '서울 seoul newyork 뉴욕 뉴저지 서울 강릉 양양 yangyang 뉴욕 서울 강릉 NY 서울 뉴욕 남양주 양주
안양 안산 suwon 서울 ansan 안양 뉴욕'
city_f<-str_extract_all(city,'[가-힣]+')
df<-data.frame(table(city_f))
df
install.packages("wordcloud")
library(wordcloud)
wordcloud(df$city_f,df$Freq,
          scale=c(9,0.5),
          min.freq = 2,
          colors=brewer.pal(5,'Set1'),
          random.order = F)
library(wordcloud2)
wordcloud2(df[df$Freq>=2,])

#[문제]'시장님'이 들어간 요청사항의 개수를 확인하고 관련 요청 내용을 wordcloud로 시각화해주세요.
seoul
mayor<-grep('\\시장님+',seoul,value=T)
#앞에 숫자 제거
str_extract_all(mayor,'^\\d+')
mayor<-gsub('^\\d+','',mayor)
#뒤에 숫자 및 날짜 제거
str_extract_all(mayor,'\\d{4}-\\d{2}-\\d{2}\\s\\d+')
mayor<-str_replace_all(mayor,'\\d{4}-\\d{2}-\\d{2}\\s\\d+','')
mayor<-str_squish(mayor)
class(mayor)
df<-data.frame(table(mayor))

wordcloud2(df,
           size=0.1)

#[문제]네이버 영화의 네티즌 평점에서 현재 상영작의 모든 영화에 대한 평점 및 comment를 1~100페이지까지 스크래핑해주세요.
movie<-data.frame()
#페이지의 각 테이블 별 자료 출력
html<-str_trim(read_html('https://movie.naver.com/movie/point/af/list.naver?&page=1')%>%
  html_nodes('.title')%>%
  html_text())

html
#영화이름 출력
name<-str_extract_all(html,'^.+')
#영화 평점 출력
point<-str_match(html,'별점 - 총 10점 중(\\d{1,2})')[,2]
#영화 코멘트 출력
html<-str_remove(html,'^.+')
html<-str_remove(html,'별점 - 총 10점 중(\\d{1,2})')
html<-str_remove(html,'\\\t신고')
comment<-str_trim(html)

movie<-data.frame()
for (i in 1:100){
  html<-str_trim(read_html(paste0('https://movie.naver.com/movie/point/af/list.naver?&page=',i))%>%
                   html_nodes('.title')%>%
                   html_text())
  #영화이름 출력
  name<-unlist(str_extract_all(html,'^.+'))
  #영화 평점 출력
  point<-str_match(html,'별점 - 총 10점 중(\\d{1,2})')[,2]
  #영화 코멘트 출력
  html<-str_remove(html,'^.+')
  html<-str_remove(html,'별점 - 총 10점 중(\\d{1,2})')
  html<-str_remove(html,'\\\t신고')
  comment<-str_trim(html)
  movie<-rbind(movie,data.frame(name=name,point=point,comment=comment))
  Sys.sleep(1)
}
View(movie)
#[문제]위의 스크래핑한 자료에서 영화별 평균 평점을 구하고 영화별 comment를 한 컬럼에 붙여 넣어주세요.
library(dplyr)
str(movie)

#평균 평점 구하기
movie$point<-as.integer(movie$point)
mean_movie<-data.frame(movie%>%
    group_by(name)%>%
      dplyr::summarise(mean = mean(point)))

head(movie)
mean_movie
#comment 합치기
x<-movie%>%
  group_by(name)%>%
  mutate(total=paste(comment,collapse='\t'))
result<-merge(mean_movie,x,by='name')

View(result)
result<-result[,c(1,2,5)]
#리뷰수 추가하기
result<-data.frame(result%>%
  group_by(name)%>%
  add_count())
result<-unique(result)
View(result)
#리뷰수가 5개 이상인 리뷰 중에 평점이 가장 높은 다섯가지 영화 출력
top5<-result%>%
  filter(n>=5)%>%
  mutate(rank=dense_rank(desc(mean)))%>%
  filter(rank<=5)
View(top5)

#다나와 홈페이지에서 애플 노트북만 선택하여 모든 페이지의 노트북명, 사양정보, 가격정보 데이터 프레임으로 저장하세요.
remdr$open()
remdr$navigate('http://prod.danawa.com/list/?cate=112758')
remdr$findElement(using='class',value='spec_opt_view')$clickElement()
remdr$findElement(using='xpath',value='//*[@id="searchMaker1452"]')$clickElement()

library(stringr)
apple<-data.frame()
memnpri<-NULL
a_spec<-NULL
a_name<-NULL
id<-NULL
for (i in 1:6){
  #페이지이동 - 3,4페이지가 다르므로 xpath 사용 불가
  #remdr$findElement(using='xpath',value='//*[@id="productListArea"]/div[5]/div/div/a[1]')$clickElement()
  #페이지이동
  remdr$findElement(using='css',value=paste0('div.number_wrap > a:nth-child(',i,')'))$clickElement()
  Sys.sleep(2)
  html <- read_html(remdr$getPageSource()[[1]])
  id_tmp<-as.character(na.omit(html_nodes(html,'div.main_prodlist.main_prodlist_list>ul.product_list>li')%>%html_attr('id')))
  id<-c(id,id_tmp)
  for (j in 1:length(id)){
    id_info <- html_nodes(html,paste0('li#',id[j]))
    name<-html_nodes(id_info,'p.prod_name')%>%html_text()%>%str_trim%>%str_remove_all('인기\\s+순위\\d{1,2}')%>%str_trim
    spec <- html_nodes(id_info,'div.spec_list')%>%html_text()%>%str_trim
    p_id<-html_nodes(id_info,'div.prod_pricelist>ul>li')%>%html_attr('id')
    a_name <- c(a_name,name)
    a_spec<- c(a_spec,spec)
    m_p<-NULL
    for (k in 1:length(p_id)){
      memory <- html_nodes(html,xpath=paste0('//*[@id="',p_id[k],'"]//div/p'))%>%html_text()%>%str_trim
      price <- html_nodes(html,xpath=paste0('//*[@id="',p_id[k],'"]/p[2]/a'))%>%html_text()%>%str_trim
      m_p<- paste(m_p,paste(memory,price,sep=' : '),sep = ' / ')%>%str_remove('^\\s+/\\s+')
      if (k==length(p_id)){
        memnpri <-c(memnpri,m_p)
      }
    }
  }
  apple <- rbind(apple,data.frame('노트북명'=a_name,'스펙'=a_spec,'가격'=memnpri))
}

View(apple)

length(a_name)
length(a_spec)
length(memnpri)


#네이버 주식의 검색상위 종목 출력
remdr$open()
#삼성전자
remdr$navigate('https://finance.naver.com/item/main.naver?code=005930')
#인기주식
remdr$navigate('https://finance.naver.com/sise/lastsearch2.naver')
html <- read_html(remdr$getPageSource()[[1]])
xml2::xml_remove(html_nodes(html,'td.blank_08'))
xml2::xml_remove(html_nodes(html,'td.blank_06'))
pop<-NULL
for (i in 1:90){
  search<-html_nodes(html,'table.type_5')%>%html_nodes(paste0('tbody>tr:nth-child(',i,')'))%>%html_text%>%str_squish()
  pop <- c(pop,search)
}
pop
pop <- grep('\\w+',pop,value=T)
pop
name<-unlist(str_split(pop[1],' '))
length(name)
result<-data.frame()
for (i in 1:length(pop)){
  tmp<-unlist(str_split(pop[i],' '))
  result<- rbind(result,tmp)
}
result
colnames(result)<-result[1,]
result<-result[-1,-1]
result$전일비 <- ifelse(result$등락률<0,paste('▼',result$전일비),paste('▲',result$전일비))
View(result)
ifelse(result$등락률<0,paste('▼',result$전일비),paste('▲',result$전일비))

install.packages("formattable")
library(formattable)

formattable(result,
            list(`등락률`=formatter("span", style=x~style(color= ifelse(x>0, "red", 'blue'))),
                 `전일비`=formatter("span", style=x~style(color= ifelse(result$등락률>0, "red", 'blue')))))


