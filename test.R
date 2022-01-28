x<-employees%>%
  filter(!EMPLOYEE_ID %in% MANAGER_ID)%>%
  group_by(DEPARTMENT_ID)%>%
  summarise(mean(SALARY))
x<-data.frame(x)
x<-x[-11,]
x<-round(x)
class(x)
str(x)
names(x)<-c('부서','평균급여')

ggplot(data=x,aes(x=부서,y=평균급여,fill=as.factor(평균급여)))+
  geom_bar(stat='identity')+
  labs(title='비관리자의 평균 급여',xlab='부서',ylab='천원')+
  geom_text(aes(label=평균급여),position=position_dodge(0.7),colour='darkred')+
  theme_bw()+
  theme(legend.title.align = 0.5, 
        legend.box.background = element_rect())+
 # scale_fill_brewer(palette='Set3')
  scale_fill_manual(values=brewer.pal(10, 'Set3'))
?brewer.pal  

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

head(employees)
emp<-tapply(employees$EMPLOYEE_ID,list(employees$JOB_ID,lubridate::year(employees$HIRE_DATE)),length,default=0)
str(emp)
emp<-data.frame(emp)
colnames(emp)<-c(2001:2008)
par(mfrow=c(4,2))
for(i in 1:ncol(emp)){
  pie(emp[,i],labels=paste0(rownames(emp),'(',round(emp[,i]/sum(emp[,i]),2),'%)'),
      main=paste(colnames(emp)[i],'년도 JOB_ID별 입사인원'),
      cex=1,
      col=brewer.pal(8,'Set3'))
}

#dplyr
x<-employees%>%
  filter(!EMPLOYEE_ID %in% MANAGER_ID)%>%
  group_by(DEPARTMENT_ID)%>%
  dplyr::summarise(mean(SALARY))
x<-data.frame(x)
x<-x[-11,]
x<-round(x)
graphics.off()
bar_x<-barplot(height=x$mean.SALARY.,
               names.arg =x$DEPARTMENT_ID,
               main='부서별 비관리자 평균급여',
               xlab='부서번호',ylab='평균급여(만원)',
               ylim=c(0,10500),
               col=heat.colors(14))
text(x=bar_x,y=x$mean.SALARY.,
     labels =x$mean.SALARY. ,
     cex=0.7,
     pos=3)
#ggplot
names(x)<-c('부서','평균급여')
str(x)
x$부서<-as.character(x$부서)
ggplot(data=x,aes(x=부서,y=평균급여,fill=as.factor(평균급여)))+
  geom_bar(stat='identity')+
  labs(title='비관리자의 평균 급여',xlab='부서',ylab='천원')+
  geom_text(aes(label=평균급여),position=position_dodge(0.7),colour='darkred')+
  theme(legend.title.align = 0.5, 
        legend.box.background = element_rect())


