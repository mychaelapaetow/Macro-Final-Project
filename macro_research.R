##Phillip's Curve Research Project 

##====Section 1: Set Up====##
##Load dependencies 
library(lmtest)
library(ggplot2)
library(quantmod)
library(xts)
library(gridExtra)
library(stargazer)
library(xlsx)
library(rJava)
library(readxl)
library(mFilter)
library(TeachingDesmos)


##====Section 2: Getting/Cleaning the Data====##
##Get data from FRED
my_data <- read_excel(file.choose("pcdata.xls"))

pc_data = subset(my_data, select = -c(IR,PCE,NROU, URATE, UNRATE) )

pc_summary = subset(pc_data, select = -c(DATE) )

pc_clean <- pc_data[complete.cases(pc_data), ]


infl <- as.numeric(data$INF)
class(infl)

##====Section 3: Statistical Analysis====##
##Summary Statistics
summary(pc_summary)

stargazer(pc_summary,
          digits=1,
          title="Descriptive Statistics",
          type = "text",
          out="descriptive_table.doc")

model_pc <- lm(INF ~ UGAP + INFEXP + RIP + PROD + INFLAG, data = pc_data)
model_wage_pc <- lm(WAGE_INF ~ UGAP + INFEXP + RIP + PROD + WAGE_INFLAG, data = pc_data)
summary(model_pc)
summary(model_wage_pc)

predict_model <- predict(model_pc)

write.xlsx(predict_model, file = "hopethisworks.xlsx",
           sheetName = "Predicted Values", append = FALSE)


stargazer(model_pc,
          type = "html",
          out = "pc_output.doc")

stargazer(model_wage_pc,
          type= "html",
          out= "wage_pc_output.doc")

##Plotting Trendlines
ggplot2::ggplot(data = pc_data, mapping = aes(x=DATE, y=UGAP))+
  geom_line()+
  xlab("Year")+
  ylab("Unemployment Gap (%)")
ggplot2::ggplot(data = pc_data, mapping = aes(x=DATE, y=INF))+
  geom_line()+
  xlab("Year")+
  ylab("Core PCE (% change from year ago)")
ggplot2::ggplot(data = pc_data, mapping = aes(x=DATE, y=WAGE_INF))+
  geom_line()+
  xlab("Year")+
  ylab("Wage Inflation (% change from year ago)")

##Plotting Naturnal Rate of Unemployment v. Unemployment Rate 
ggplot() + 
  geom_line(data = my_data, aes(x = DATE, y = NROU, colour='blue'), size=1) +
  geom_line(data = my_data, aes(x = DATE, y = UNRATE, colour='dark red'), size=1) +
  scale_color_discrete(name = " ", labels = c("Natural Rate of Unemployment", "Unemployment Rate"))+
  theme(axis.title.y=element_blank())

##Plotting Core PCE v. Unemployment Gap
ggplot() + 
  geom_line(data = pc_data, aes(x = DATE, y = INF, colour='blue'), size=1) +
  geom_line(data = pc_data, aes(x = DATE, y = UGAP, colour='red'), size=1) +
  scale_color_discrete(name = " ", labels = c("Core PCE", "Unemployment Gap"))+
  theme(axis.title.y=element_blank())

##Plotting Wage Inflation  v. Unemployment Gap
ggplot() + 
  geom_line(data = pc_data, aes(x = DATE, y = WAGE_INF, colour='blue'), size=0.5) +
  geom_line(data = pc_data, aes(x = DATE, y = UGAP, colour='red'), size=1) +
  scale_color_discrete(name = " ", labels = c("Wage Inflation", "Unemployment Gap"))+
  theme(axis.title.y=element_blank())


##Plotting Inflation PCE v. Inflation Expectations
ggplot() + 
  geom_line(data = pc_data, aes(x = DATE, y = INF, colour='blue'), size=1) +
  geom_line(data = pc_data, aes(x = DATE, y = INFEXP, colour='red'), size=1) +
  scale_color_discrete(name = " ", labels = c("Core PCE Inflation", "Expected Inflation"))+
  theme(axis.title.y=element_blank())

##Plotting Wage Inflation v. Inflation Expectations
ggplot() + 
  geom_line(data = pc_data, aes(x = DATE, y = WAGE_INF, colour='blue'), size=0.5) +
  geom_line(data = pc_data, aes(x = DATE, y = INFEXP, colour='red'), size=1) +
  scale_color_discrete(name = " ", labels = c("Core PCE Inflation", "Expected Inflation"))+
  theme(axis.title.y=element_blank())


##Plotting the Phillip's Curve
ggplot2::ggplot(data = pc_data, mapping = aes(x=UGAP, y=INF))+
  geom_point(col="lightblue")+
  geom_smooth(method='lm')+
  labs(title = "The Phillip's Curve 1961-2019")+
  labs(caption = "based on data from Federal Reserve Bank of St. Louis (FRED)")+
  xlab("Unemployment Gap (%)")+
  ylab("Core PCE (% change from year ago)")

##Plotting the Phillip's Curve
ggplot2::ggplot(data = pc_data, mapping = aes(x=UGAP, y=INF_PRE))+
  geom_point(col="lightblue")+
  geom_smooth(method='lm')+
  labs(title = "The Phillip's Curve 1961-2019")+
  labs(caption = "based on data from Federal Reserve Bank of St. Louis (FRED)")+
  xlab("Unemployment Gap (%)")+
  ylab("Core PCE (% change from year ago)")


subset_1980 <- pc_data[pc_data$DATE<=("1989-10-01")  & 
                         pc_data$DATE>=("1983-01-01"),] 

subset_1990 <- pc_data[pc_data$DATE<=("1999-10-01")  & 
                         pc_data$DATE>=("1990-01-01"),] 

subset_2000 <- pc_data[pc_data$DATE<=("2009-10-01")  & 
                         pc_data$DATE>=("2000-01-01"),] 

subset_2010 <- pc_data[pc_data$DATE<=("2019-10-01")  & 
                                        pc_data$DATE>=("2010-01-01"),] 

##Setting axis parameters 
My_Theme = theme(
  axis.title.x = element_text(size = 7),
  axis.text.x = element_text(size = 5),
  axis.title.y = element_text(size = 5),
  axis.text.y = element_text(size = 5),
  plot.title = element_text(size = 7))

#1980s
Plot_1980 = ggplot2::ggplot(data = subset_1980, mapping = aes(x=UGAP, y=INF))+
  geom_point(col="gray")+
  geom_smooth(method='lm')+
  labs(title = "The Phillip's Curve 1980s")+
  xlab("Unemployment Rate (%)")+
  ylab("PCE excluding food and energy (% change from year ago)")+
  My_Theme


#1990s
Plot_1990 = ggplot2::ggplot(data = subset_1990, mapping = aes(x=UGAP, y=INF))+
  geom_point(col="gray")+
  geom_smooth(method='lm')+
  labs(title = "The Phillip's Curve 1990s")+
  xlab("Unemployment Rate (%)")+
  ylab("PCE excluding food and energy (% change from year ago)")+
  My_Theme


#2000s
Plot_2000 = ggplot2::ggplot(data = subset_2000, mapping = aes(x=UGAP, y=INF))+
  geom_point(col="gray")+
  geom_smooth(method='lm')+
  labs(title = "The Phillip's Curve 2000s")+
  xlab("Unemployment Rate (%)")+
  ylab("PCE excluding food and energy (% change from year ago)")+
  My_Theme



#2010s
Plot_2010 = ggplot2::ggplot(data = subset_2010, mapping = aes(x=UGAP, y=INF))+
  geom_point(col="gray")+
  geom_smooth(method='lm')+
  labs(title = "The Phillip's Curve 2010s")+
  xlab("Unemployment Rate (%)")+
  ylab("PCE excluding food and energy (% change from year ago)")+
  My_Theme


##Combine each decade graph into a single page
gridExtra::grid.arrange(Plot_1980s, Plot_1990s, Plot_2000s, Plot_2010s, nrow=2, ncol=2)

##OLS Regression/Price Inflation
model_pc_1980s <- lm(INF ~ UGAP + INFEXP + RIP + PROD + INFLAG, data = subset_1980)

model_pc_1990s <- lm(INF ~ UGAP + INFEXP + RIP + PROD + INFLAG, data = subset_1990)

model_pc_2000s <- lm(INF ~ UGAP + INFEXP + RIP + PROD + INFLAG, data = subset_2000)

model_pc_2010s <- lm(INF ~ UGAP + INFEXP + RIP + PROD + INFLAG, data = subset_2010)

##OLS Regression/Wage Inflation
model_pc_wage_1980s <- lm(WAGE_INF ~ UGAP + INFEXP + RIP + PROD + WAGE_INFLAG, data = subset_1980)

model_pc_wgae_1990s <- lm(WAGE_INF ~ UGAP + INFEXP + RIP + PROD + WAGE_INFLAG, data = subset_1990)

model_pc_wage_2000s <- lm(WAGE_INF ~ UGAP + INFEXP + RIP + PROD + WAGE_INFLAG, data = subset_2000)

model_pc_wage_2010s <- lm(WAGE_INF ~ UGAP + INFEXP + RIP + PROD + WAGE_INFLAG, data = subset_2010)

#Model Summary.Price
summary(model_pc_1980s)

summary(model_pc_1990s)

summary(model_pc_2000s)

summary(model_pc_2010s)

#Model Summary/Wage
summary(model_pc_wage_1980s)

summary(model_pc_wgae_1990s)

summary(model_pc_wage_2000s)

summary(model_pc_wage_2010s)

stargazer(model_pc_1980s, model_pc_1990s, model_pc_2000s, model_pc_2010s,
          type = "html",
          out="pc_decades.doc")

stargazer(model_pc_wage_1980s, model_pc_wgae_1990s, model_pc_wage_2000s, model_pc_wage_2010s,
          type = "html",
          out= "pc_wage_decades.doc")



