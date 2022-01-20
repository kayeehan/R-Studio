#22.01.20 R test
install.packages("sqldf")
library(sqldf)
employees<-read.csv('C:/data_bigdata/emp_mod.csv',header=T)
#[문제151]동일한 날짜에 입사한 사원들의 정보를 출력해주세요.(sqldf를 이용하세요)
sqldf('select e.*
      from employees e
      where exists (select "x"
                    from employees
                    where hire_date=e.hire_date
                    and employee_id<>e.employee_id)')
#뭐지