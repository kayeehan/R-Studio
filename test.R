total<-employees%>%
  dplyr::group_by(lubridate::year(HIRE_DATE))%>%
  dplyr::count()
total<-data.frame(total)
names(total)<-c('YEAR','NUM')
t(total)[2,]

data<-tapply(employees$EMPLOYEE_ID,list(employees$JOB_ID,lubridate::year(employees$HIRE_DATE)),NROW,default=0)
data
data<-rbind(data,t(total)[2,])
rownames(data)[nrow(data)]<-'TOTAL'
data<-data.frame(data)

p_d<-pie(data[-nrow(data),1],
         labels=paste0(rownames(data)[-nrow(data)],data[,1]/data[nrow(data),1],'%'))  