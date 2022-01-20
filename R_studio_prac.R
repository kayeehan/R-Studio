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
#문제] COMMISSION_PCT가 NA인 사원들 급여 평균과 
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

#문제] COMMISSION_PCT가 NA인 사원들 중 급여가 제일 낮은 사람의 정보를 출력하세요.
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







