##### DW plots for Delhi data-set #####

### loading some required packages

library(readxl)
library(dplyr)

### loading required data

data = read_excel("# the path for the required file", sheet = "DELHI")
View(data)
str(data)
data = data %>% select(DATE,CONFIRMED, DECEASED) %>% filter(DATE >= "2021-03-15" & DATE < "2021-06-15")

### Adding extra columns to data

add_col_data = function (dataset, lockdown.date){
  
  dataset$time=1:nrow(dataset)
  
  dataset$level=NA
  dataset$level[dataset$DATE<lockdown.date]=0
  dataset$level[dataset$DATE>=lockdown.date]=1
  
  dataset$trend=NA
  dataset$trend[dataset$DATE<lockdown.date]=0
  dataset$trend[dataset$DATE>=lockdown.date]=seq(1:sum(dataset$DATE>=lockdown.date))
  
  return(dataset)
  
}

# Fitting the regression model

model_fitting = function(dataset, variable){
  m=lm(variable~time + level + trend,
        data=dataset)
  
  return(m)
}

# summary

model_summary = function(dataset, variable){
  m=lm(variable~time + level + trend,
        data=dataset)
  
  return(summary(m))
}

# Preparing the data-set for analysis

data$DATE = as.Date(data$DATE)
data = add_col_data(data, "2021-04-19")

View(data)

# Regression models for daily new cases

d1 = model_fitting(data, log(data$CONFIRMED+1))
d2 = model_summary(data, log(data$CONFIRMED+1))

# Regression models for daily death cases

d3 = model_fitting(data, log(data$DECEASED+1))
d4 = model_summary(data, log(data$DECEASED+1))

### loading more required packages

library(ggplot2)
library(ggpubr)
library(dotwhisker)
theme_set(theme_pubr())

### Re-analysis for window of 15 to 21 days post intervention

date.list = seq(as.Date("2021-06-15"),  as.Date("2021-06-21"), by= 1)
reg1 = list()
reg2 = list()

for (i in seq_along(date.list)){
  dt = read_excel("# the path for the required file", sheet = "DELHI")
  dt$DATE = as.Date(dt$DATE)
  dt = dt %>% select(DATE,CONFIRMED, DECEASED) %>% filter(DATE >= "2021-03-15" & DATE <= date.list[i])
  dt = add_col_data(dt, "2021-04-19")
  dt["logconfirmed"] = log(dt$CONFIRMED+1)
  dt["logdeceased"] = log(dt$DECEASED+1)
  reg1[[i]] = model_fitting(dt, dt$logconfirmed)
  reg2[[i]] = model_fitting(dt, dt$logdeceased)
  
}

### DW plot

plt1 = dwplot(reg1,ci = 0.99,show_intercept = F) %>% relabel_predictors(c(time = 'Time',level = "Level",trend = "Trend"))+
  theme_classic()+geom_vline(xintercept = 0, color = "grey60",linetype = 5)
plt1 = plt1+scale_color_discrete(name = "Days post intervention",labels = c('15','16','17','18','19','20','21'))
plt1 = plt1+labs(title = "Daily cases (log) in Delhi")+theme(plot.title = element_text(face = "bold",size = 10,hjust = 0.5),axis.title = element_text(size = 10),
                                                         axis.text = element_text(size = 10),
                                                         legend.text=element_text(size=10),
                                                         legend.title = element_text(size=10),legend.position = "none")
plt2 = dwplot(reg2,ci = 0.99) %>% relabel_predictors(c(time = 'Time',level = "Level",trend = "Trend"))+
  theme_classic()+geom_vline(xintercept = 0, color = "grey60",linetype = 5)
plt2 = plt2+scale_color_discrete(name = "Days post intervention",labels = c('15','16','17','18','19','20','21'))
plt2 = plt2+labs(title = "Daily deaths (log) in Delhi")+theme(plot.title = element_text(face = "bold",size = 10,hjust = 0.5),axis.title = element_text(size = 10),
                                                     axis.text = element_text(size = 10),
                                                     legend.text=element_text(size=10),
                                                     legend.title = element_text(size=10),legend.position = "right")

figure=ggarrange(plt1,plt2,ncol = 2, nrow = 1)
figure

##### End #####