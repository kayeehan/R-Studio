employees<-read.csv('C:/data_bigdata/emp_mod.csv',header=T)
departments<-read.csv('C:/data_bigdata/dept.csv',header=T)
sales<-read.csv('C:/data_bigdata/fruits_sales.csv',header=T)
install.packages('plyr')
library(plyr)
install.packages("dplyr")
library(dplyr)
install.packages("sqldf")
library(sqldf)

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
  