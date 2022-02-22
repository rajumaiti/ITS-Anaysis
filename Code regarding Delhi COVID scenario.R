##### Delhi data (daily cases and daily deaths) #####

### loading required packages

library("scales")        
library("stargazer")     
library("dplyr")         
library("pander")        
library("Wats")          
library("lmtest")
library("sandwich")

### loading required data

dl=read.csv("# the path for the required file")

View(dl)

### highest and lowest cases and deaths

max(dl$cases);min(dl$cases)
max(dl$deaths);min(dl$deaths)

### creating the required setup for ITS analysis

n=length(dl$cases) # total data-points
l=35 # after that, intervention started in Delhi

dl$time=rep(1:nrow(dl))
dl$treatment=c(rep(0,l),rep(1,nrow(dl)-l))
dl$timesince=c(rep(0,l), rep(1:(nrow(dl)-l)))

View(dl)

##### ITS regression setup for daily cases in Delhi

c=log(dl$cases +1)
reg_c=lm(c ~ time + treatment + timesince, data = dl)
summary(reg_c)

dwtest(reg_c) # DW statistic for this particular setup

stargazer(reg_c, type = "text", 
          dep.var.labels = ("Daily cases (log)"),
          column.labels = "Model results",
          covariate.labels = c("Time", "Treatment", "Time Since Treatment"),
          omit.stat = "all", 
          digits = 3 ,title="Daily cases for Delhi") # tabular representation of the results

# graphical representation of this analysis

pred_c=predict(reg_c, dl )

date=seq(as.Date("2021-03-15"), by="1 day", length.out=nrow(dl))
plot(date, c,
     col = "brown",
     xaxt="n", 
     ylim = c( (min (c) - 1), (max (c) + 1)),
     xlab = "", 
     ylab = "",cex.axis=2,lwd=2)
axis.Date(1,at=seq(min(date),max(date)+15,by="months"),format="%B",cex.axis=2)
mtext(side=1,line=2,"Months",font=2,cex=2)
mtext(side=2,line=3,"Daily cases (log)",font=2,cex=1.5)
mtext(side=3,line=0.5,"Daily cases for Delhi",font=2,cex=2)
lines(date,pred_c, col="dodgerblue4", lwd = 3 )

# Lines marking the interruption and the incubation period

abline(v=as.numeric(date[l]),col="red",lwd=2)
text(date[l],10.5, "Intervention starts", col="red",cex=2, pos=2)

abline(v=as.numeric(date[(l+14)]),col="blue",lwd=2)
text(date[(l+14)],4,"14-day Incubation period",col="blue",cex=2,pos=4)

### Counterfactual

pred_c1=predict(reg_c, dl) 

# Creating a new data-set where Treatment and Time Since Treatment are equal 
# to 0 to eliminate the intervention effects

dl_new=as.data.frame(cbind(time = rep(1 : nrow(dl)), treatment = rep(0)
                           , timesince = rep(0))) 

pred_c2=predict(reg_c,dl_new) 

plot(date,c,
     bty="n",
     col = gray(0.5,0.5), pch=19,
     xaxt="n", 
     ylim =  c( (min ( c ) - 4), (max (c) + 4)),
     xlab = "", 
     ylab = "",cex.axis=2,lwd=2)
axis.Date(1,at=seq(min(date),max(date)+15,by="months"),format="%B",cex.axis=2)
mtext(side=1,line=2,"Months",font=2,cex=2)
mtext(side=2,line=3,"Daily cases (log)",font=2,cex=1.5)
mtext(side=3,line=0.5,"Daily cases for Delhi",font=2,cex=2)

lines(date[1:(l-1)], pred_c1[1:(l-1)], col="dodgerblue4", lwd = 3 )
lines(date[(l+1):n], pred_c1[(l+1):n], col="dodgerblue4", lwd = 3 )
lines(date[l:n], pred_c2[l:n], col="darkorange2", lwd = 3, lty = 5 ) 

text(date[70], 9, labels = "Predicted", pos = 4, cex = 2, col = "dodgerblue3")
text(date[60], 12, labels = "Counterfactual", pos = 4, cex = 2, col = "darkorange2")

# Line marking the interruption

abline(v=as.numeric(date[l]),col="red",lwd=2)
text(date[l],4, "Intervention starts", col="red",cex=2, pos=4 )

### Delayed time effects
# 14-day delay

dl$treatment2=c(rep(0,(l+14)),rep(1,nrow(dl)-(l+14)))
dl$timesince2=c(rep(0,(l+14)), rep(1:(nrow(dl)-(l+14))))
View(dl)

reg_c2=lm(c ~ time + treatment2 + timesince2, data = dl)
summary(reg_c2)

dwtest(reg_c2) # DW statistic for this particular setup

stargazer(reg_c2, type = "text", 
          dep.var.labels = ("Daily Cases (log)"),
          column.labels = "Model results",
          covariate.labels = c("Time", "Treatment - 14 days delay", 
                               "Time Since Treatment - 14 days delay"),
          omit.stat = "all", 
          digits = 3 ,title="Daily cases for Delhi") # tabular representation of the results

# 21-day delay

dl$treatment3=c(rep(0,(l+21)),rep(1,nrow(dl)-(l+21)))
dl$timesince3=c(rep(0,(l+21)), rep(1:(nrow(dl)-(l+21))))
View(dl)

reg_c3=lm(c ~ time + treatment3 + timesince3, data = dl)
summary(reg_c3)

dwtest(reg_c3) # DW statistic for this particular setup

stargazer(reg_c3, type = "text", 
          dep.var.labels = ("Daily Cases (log)"),
          column.labels = "Model results",
          covariate.labels = c("Time", "Treatment - 21 days delay", 
                               "Time Since Treatment - 21 days delay"),
          omit.stat = "all", 
          digits = 3 ,title="Daily cases for Delhi") # tabular representation of the results


##### About daily deaths
### ITS regression setup for daily deaths in Delhi

d=log(dl$deaths +1)
reg_d=lm(d ~ time + treatment + timesince, data = dl)
summary(reg_d)

dwtest(reg_d) # DW statistic for this particular setup

stargazer(reg_d, type = "text", 
          dep.var.labels = ("Daily deaths (log)"),
          column.labels = "Model results",
          covariate.labels = c("Time", "Treatment", "Time Since Treatment"),
          omit.stat = "all", 
          digits = 3, title="Daily deaths for Delhi" ) # tabular representation of the results

# graphical representation of the analysis

pred_d=predict(reg_d,dl)

date=seq(as.Date("2021-03-15"), by="1 day", length.out=nrow(dl))
plot(date,d,
     col = "brown",
     xaxt = "n", 
     ylim = c((min(d)-0.5), (max (d) + 0.5)),
     xlab = "", 
     ylab = "",cex.axis=2,lwd=2)
axis.Date(1,at=seq(min(date),max(date)+15,by="months"),format="%B",cex.axis=2)
mtext(side=1,line=2,"Months",font=2,cex=2)
mtext(side=2,line=3,"Daily deaths (log)",font=2,cex=1.5)
mtext(side=3,line=0.5,"Daily deaths for Delhi",font=2,cex=2)

lines(date,pred_d, col="dodgerblue4", lwd = 3 )

# Lines marking the interruption and the incubation period

abline(v=as.numeric(date[l]),col="red",lwd=2)
text(date[l],6, "Intervention starts", col="red",cex=2, pos=2)

abline(v=as.numeric(date[(l+14)]),col="blue",lwd=2)
text(date[(l+14)],3,"14-day Incubation period",col="blue",cex=2,pos=4)

### Counterfactual

pred_d1=predict(reg_d, dl) 

# Creating a new data-set where Treatment and Time Since Treatment are equal 
# to 0 to eliminate the intervention effects

pred_d2=predict(reg_d,dl_new) 

plot(date,d,
     bty="n",
     col = gray(0.5,0.5), pch=19,
     xaxt="n", 
     ylim =  c( (min (d ) - 2.5), (max (d) + 2.5)),
     xlab = "", 
     ylab = "",cex.axis=2,lwd=2)

axis.Date(1,at=seq(min(date),max(date)+15,by="months"),format="%B",cex.axis=2)
mtext(side=1,line=2,"Months",font=2,cex=2)
mtext(side=2,line=3,"Daily deaths (log)",font=2,cex=1.5)
mtext(side=3,line=0.5,"Daily deaths for Delhi",font=2,cex=2)

lines(date[1:(l-1)], pred_d1[1:(l-1)], col="dodgerblue4", lwd = 3 )
lines(date[(l+1):n], pred_d1[(l+1):n], col="dodgerblue4", lwd = 3 )
lines(date[l:n], pred_d2[l:n], col="darkorange2", lwd = 3, lty = 5 ) 

text(date[70], 5.5, labels = "Predicted", pos = 4, cex = 2, col = "dodgerblue3")
text(date[60], 7, labels = "Counterfactual", pos = 4, cex = 2, col = "darkorange2")

# Line marking the interruption

abline(v=as.numeric(date[l]), col="red", lwd=2 )
text(date[l],2, "Intervention starts", col="red",cex=2, pos=4 )

### Delayed time effects
# 14-day delay

reg_d2=lm(d ~ time + treatment2 + timesince2, data = dl)
summary(reg_d2)

dwtest(reg_d2) # DW statistic for this particular setup

stargazer(reg_d2, type = "text", 
          dep.var.labels = ("Daily deaths (log)"),
          column.labels = "Model results",
          covariate.labels = c("Time", "Treatment - 14 days delay", 
                               "Time Since Treatment - 14 days delay"),
          omit.stat = "all", 
          digits = 3, title="Daily deaths for Delhi" ) # tabular representation of the results

# 21-day delay

reg_d3=lm(d ~ time + treatment3 + timesince3, data = dl)
summary(reg_d3)

dwtest(reg_d3) # DW statistic for this particular setup

stargazer(reg_d3, type = "text", 
          dep.var.labels = ("Daily deaths (log)"),
          column.labels = "Model results",
          covariate.labels = c("Time", "Treatment - 21 days delay", 
                               "Time Since Treatment - 21 days delay"),
          omit.stat = "all", 
          digits = 3 ,title="Daily deaths for Delhi") # tabular representation of the results

##### Graphical representation of overall Delhi COVID scenario

par(mfrow=c(1,2))

plot(date, dl$cases,
     col = "brown",
     xaxt="n",xlab="",ylab="",
     ylim = c( 0, (max (dl$cases))),cex.axis=1.4,lwd=2,type="l")
axis.Date(1,at=seq(min(date),max(date)+15,by="months"),format="%B",cex.axis=2)
mtext(side=1,line=2,"Months",font=2,cex=2)
mtext(side=3,line=0.5,"Daily scenario in Delhi (Linear scale)",font=1.5,cex=2)
lines(date,dl$deaths, col="dodgerblue4", lwd = 3,type="l" )
text(date[50], 25000, labels = "Daily cases", pos = 4, cex = 2, col = "brown")
text(date[25], 2000, labels = "Daily deaths", pos = 4, cex = 2, col = "dodgerblue4")


plot(date, c,
     col = "brown",
     xaxt="n",xlab="",ylab="",
     ylim = c( min(c)-5, (max (c)+1)),cex.axis=1.4,lwd=2,type="l")
axis.Date(1,at=seq(min(date),max(date)+15,by="months"),format="%B",cex.axis=2)
mtext(side=1,line=2,"Months",font=2,cex=2)
mtext(side=3,line=0.5,"Logarithmic scale",font=1.5,cex=2)
lines(date,d, col="dodgerblue4", lwd = 3,type="l" )
text(date[35], 10.8, labels = "Daily cases (log)", pos = 4, cex = 2, col = "brown")
text(date[27], 1, labels = "Daily deaths (log)", pos = 4, cex = 2, col = "dodgerblue4")

par(mfrow=c(1,1))

##### End #####

