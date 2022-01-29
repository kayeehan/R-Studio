employees<-read.csv('C:/data_bigdata/emp_mod.csv',header=T)
departments<-read.csv('C:/data_bigdata/dept.csv',header=T)
sales<-read.csv('C:/data_bigdata/fruits_sales.csv',header=T)
blood<-read.csv('C:/data_bigdata/blood.csv',header=T)
survey<-read.csv('C:/data_bigdata/survey.csv',header=F)
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






#22.01.20
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

#22.01.21
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

#22.01.22
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
  
#22.01.23 
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

#22.01.24
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

#22.01.25
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
           
#22.01.26
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

grep(6,c('3','6','9'),value=T)

v369 <-function(x){
  ifelse(grep(as.character(x),c('3','6','9'),value=T) %in% c('3','6','9'),print('짝'),print(x))
}
v369(3)
v369(13)

v369 <-function(x){
  ifelse(grep(x,c('3','6','9'),value=T) %in% c('3','6','9'),print('짝'),print(x))
}
#22.01.27

v369 <-function(x){
  pattern<-NULL
  pattern<-paste0('^\\w*',x,'\\&$')
  ifelse(grep(pattern,c('3','6','9'),value=T) %in% c('3','6','9'),print('짝'),print(x))
}
v369(3)
v369(13)
x<-13
strsplit(as.character(x), "")
pattern<-paste0('^\\w*',3,'\\&$')
grep('^\\w*3\\w*$',c('3','6','9'),value=T) #grep함수:특정 문자가 포함된 리스트 출력

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

#22.01.28
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


