


rm(list=ls())  # 这行代码是清空当前工作环境中的所有对象，确保你开始一个干净的工作空间
library(parallel)
library(lmtest)
library(DescTools)
library(foreign)
library(Matrix)
library(lfe)  
library(magrittr)
library(margins)
library(naniar)
library(dplyr)
library(plotly)
library(zoo)
library(readxl)
library(mice)
library(rio)
library(orca)
library(DMwR2)
library(car)
library(AER)
library(lme4)
library(ggplot2)
library(brms)
library(mgcv)
library(future)
library(plm)
library(dplyr)
library(data.table)
library(fixest)
library(AER)
library(clubSandwich)
library(lmtest)
library(ggpubr)
library(marginaleffects)
library(webshot)
library(htmlwidgets)
library(scales)
library(scatterplot3d)
library(stargazer)
############################## 读取数据############################################
## 数据路径

sentiment_UTC_data='dataset2018_2019_numbermorethan20_unique'

str0='D:/Sentiment_Brazil/R_codes/'

input_file_name= paste(str0, sentiment_UTC_data, '.csv', sep="")  # 字符串连接
result0 = read.csv(input_file_name, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                   strip.white = FALSE)

print(nrow(result0))

# 删除数据框中包含 NA 或 NaN 的行。
result0 <- na.omit(result0)


result0$GDP<- result0$GDP/1000
result0$Surface_pressure<- result0$Surface_pressure/1000
result0$population500<- result0$population500/1000
result0$population1000<- result0$population1000/1000
result0$population1500<- result0$population1500/1000
result0$settlement500 <-result0$settlement500/1000
result0$settlement1000 <-result0$settlement1000/1000
result0$settlement1500 <-result0$settlement1500/1000

result0$population500[result0$population500 <0 ] <- 0
result0$population1000[result0$population1000 <0 ] <- 0
result0$population1500[result0$population1500 <0 ] <- 0


##############################
# 在原始数据框中增加一列，该列的数值为对应 userid 出现的频数

result0 <- result0 %>%
  group_by(userid) %>%
  mutate(freq = n()) %>%  # 新增一列 freq，表示 userid 出现的频数
  ungroup()               # 去掉分组



# 统计唯一用户数量
unique_users <- n_distinct(result0$userid)
cat("总用户数:", unique_users, "\n")

## 去除 countryside 数据
#result0 <- subset(result0, !(CityName %in% c("countryside")))

## 去除异常值，
#query_sample2<-subset(result0, !(sentiment > 0.95 & UTC500>0.2) )
#query_sample2<-subset(query_sample2, !(sentiment < 0.25 & UTC500<0.05))

query_sample2 = subset(result0, (freq>50) ) # 过滤tweet文本过少的数据阈值设为：50, 100, 150, 200, 250, 300,
query_sample2 <- na.omit(query_sample2)


unique_users <- n_distinct(query_sample2$userid)
cat("总用户数:", unique_users, "\n")

#query_sample2 = subset(query_sample2, (useridtotalnumber<1500) ) # 

###如果超过 1,000,000 行，data.table 的优势会更加明显。
# 转换为data.table
#result <- as.data.table(result0)

# 计算均值和获取第一个字符串
#result <- result[, lapply(.SD, function(x) if(is.numeric(x)) mean(x, na.rm = TRUE) else unique(x)[1]), 
#by = .(userid, yearmonthday, CityName)]


#write.csv(result,'D:/Sentiment_Brazil/R_codes/pdataframe202410292.csv')



####对于 100,000 行的数据，dplyr 和 data.table 通常都能在数秒内完成。

#query_sample2 <- query_sample2 %>%
#group_by(userid, yearmonthday, CityName) %>%
#summarise(
# across(where(is.numeric), mean, na.rm = TRUE),  # 对数值列取均值
# across(where(is.character), ~ unique(.)[1]),   # 对字符串列取第一个唯一值
# .groups = "drop"
# )


sd_utc5<-sd(query_sample2$UTC500)
sd_utc10<-sd(query_sample2$UTC1000)
sd_utc15<-sd(query_sample2$UTC1500)
sd_sent<-sd(query_sample2$sentiment)
mean_utc5<-mean(query_sample2$UTC500)
mean_utc10<-mean(query_sample2$UTC1000)
mean_utc15<-mean(query_sample2$UTC1500)
mean_sent<-mean(query_sample2$sentiment)

mean_PM25<-mean(query_sample2$PM25)
sd_PM25<-sd(query_sample2$PM25)

mean_PM25_2<-mean(query_sample2$PM25_2)
sd_PM25_2<-sd(query_sample2$PM25_2)

mean_Precipitation<-mean(query_sample2$Precipitation)
sd_Precipitation<-sd(query_sample2$Precipitation)

mean_GDP<-mean(query_sample2$GDP)
sd_GDP<-sd(query_sample2$GDP)

mean_humidity<-mean(query_sample2$humidity)
sd_humidity<-sd(query_sample2$humidity)

mean_Wind<-mean(query_sample2$Wind)
sd_Wind<-sd(query_sample2$Wind)

mean_cloudcover<-mean(query_sample2$cloudcover)
sd_cloudcover<-sd(query_sample2$cloudcover)

mean_Surface_pressure<-mean(query_sample2$Surface_pressure)
sd_Surface_pressure<-sd(query_sample2$Surface_pressure)

mean_MeanTemperature<-mean(query_sample2$MeanTemperature)
sd_MeanTemperature<-sd(query_sample2$MeanTemperature)

mean_Skintemperature<-mean(query_sample2$Skintemperature)
sd_Skintemperature<-sd(query_sample2$Skintemperature)


mean_population500<-mean(query_sample2$population500)
sd_population500<-sd(query_sample2$population500)
mean_population1000<-mean(query_sample2$population1000)
sd_population1000<-sd(query_sample2$population1000)
mean_population1500<-mean(query_sample2$population1500)
sd_population1500<-sd(query_sample2$population1500)

mean_impervious500<-mean(query_sample2$impervious500)
sd_impervious500<-sd(query_sample2$impervious500)
mean_impervious1000<-mean(query_sample2$impervious1000)
sd_impervious1000<-sd(query_sample2$impervious1000)
mean_impervious1500<-mean(query_sample2$impervious1500)
sd_impervious1500<-sd(query_sample2$impervious1500)

mean_nightlight500<-mean(query_sample2$nightlight500)
sd_nightlight500<-sd(query_sample2$nightlight500)
mean_nightlight1000<-mean(query_sample2$nightlight1000)
sd_nightlight1000<-sd(query_sample2$nightlight1000)
mean_nightlight1500<-mean(query_sample2$nightlight1500)
sd_nightlight1500<-sd(query_sample2$nightlight1500)

mean_settlement500<-mean(query_sample2$settlement500)
sd_settlement500<-sd(query_sample2$settlement500)
mean_settlement1000<-mean(query_sample2$settlement1000)
sd_settlement1000<-sd(query_sample2$settlement1000)
mean_settlement1500<-mean(query_sample2$settlement1500)
sd_settlement1500<-sd(query_sample2$settlement1500)

mean_NOUTC500<-mean(query_sample2$NOUTC500)
sd_NOUTC500<-sd(query_sample2$NOUTC500)
mean_NOUTC1000<-mean(query_sample2$NOUTC1000)
sd_NOUTC1000<-sd(query_sample2$NOUTC1000)
mean_NOUTC1500<-mean(query_sample2$NOUTC1500)
sd_NOUTC1500<-sd(query_sample2$NOUTC1500)



query_sample2 <- na.omit(query_sample2)

#########################一次回归模型训练 ################################
model1 <- felm(sentiment ~ 1+ UTC1000
              |yearmonthday+userid+ CityName,  data = query_sample2)


# 查看模型摘要
print(summary(model1))




#########################一次回归模型训练 ################################




model <- felm(sentiment ~ 1+ UTC1000+
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                Skintemperature+
                population1000+ nightlight1000+ impervious1000+GDP
              |yearmonthday+userid+ CityName,  data = query_sample2)


# 查看模型摘要
print(summary(model))


# 导出模型结果为各种所需表格
text_code <- stargazer(model1, model, type = "text", title = "Cooling Heterogeneity in different Elevation")
write(text_code, file = "D:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes/dUTC1000_fix_userid_city_day_tab_20241218.txt")
latex_code <- stargazer(model1, model, type = "latex", title = "Cooling Heterogeneity in different Elevation")
write(latex_code, file = "D:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes/UTC1000_fix_userid_city_day_tab_20241218.tex")






###########################################################################
#检验残差与内生变量的相关性
###########################################################################


#通过检验模型残差是否与 UTC1000 显著相关，可以判断是否存在内生性
#H0：残差与 UTC1000 不相关
#H1：残差与 UTC1000 显著相关，存在内生性

residuals_ols <- residuals(model) # 提取残差

# 检验残差与 UTC1000 的相关性
cor_test <- cor.test(residuals_ols, query_sample2$UTC1000)
print(cor_test)


#如果数据和检验逻辑都正确，这一结果说明当前模型对 UTC1000 的处理没有内生性问题，可以直接使用 OLS 模型结果


###########################################################################

# 指定输出文件的路径和名称
output_file <- "D:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes/UTC1000_fix_userid_city_day_20241205.txt"
con <- file(output_file, open = "a")# 打开文件连接，追加模式
writeLines("\n--- New Model Summary ---\n", con)# 写入一个分隔符（可选），以便区分不同的模型摘要
model_summary <- capture.output(summary(model))# 捕获模型摘要的输出
writeLines(model_summary, con = output_file)# 将模型摘要追加写入文件
# 关闭文件连接 close(con)


# 获取系数和置信度
coef <- model$coefficients[1:1,]
cfint <- confint(model, c("UTC1000"),level=0.95)
colnames(cfint) <- c("min","max")


# 计算VIF
vif_values <- vif(model)

# 打印VIF值；VIF值大于10才被认为存在严重的共线性问题;增加样本量可能有助于减轻共线性问题
print("VIF:")
print(vif_values)


#########################二次回归模型训练 ################################


model <- felm(sentiment ~ 1+ UTC1000+UTC1000_2+
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                Skintemperature+
                population1000+ nightlight1000+ impervious1000+GDP
              |yearmonthday+userid+ CityName,  data = query_sample2)


# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)




############################ UTC effects ###########################################

# 获取系数

coeffs <-coef(model) 

## 协方差求解
cov <- vcovCR(model,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解


# 1. 提取模型中估计的协方差
V <- structure(c(cov["UTC1000","UTC1000"],cov["UTC1000","UTC1000_2"],cov["UTC1000_2","UTC1000"],
                 cov["UTC1000_2","UTC1000_2"]), .Dim = c(2L, 2L), .Dimnames = list(c("UTC1000", "UTC1000_2"), c("UTC1000", "UTC1000_2")))

# 2. 提前模型中估计的系数
beta <- c(coeffs["UTC1000"],coeffs["UTC1000_2"]) 


# 3. 获取模型剩余自由度
#residual degrees of freedom
n <- df.residual(model) # 计算回归模型 mod 的剩余自由度


# 4. 构建计算的范围
UTC <- seq(0, 1, length.out = 200) # 构建1000个0道1 的数据， (向量大小1000x1)
X <- cbind(UTC,UTC^2)

#calculate mean effects and limits
me <- X %*% beta 
# 5. 计算效应的标准误差，
se <- sqrt( rowSums(X * (X %*% V)) ) 

# 6. 计算下限
Low <- me + se * qt(0.025, n)
# 7. 计算上限
UP <- me - se * qt(0.025,n)


# 8. 数据保存

# 构建数据框
# 构建数据框并直接命名
Data_marginaleffects_UTC1000_UTC <- data.frame(UTC = UTC, mean_value = me, UP_value = UP, Low_value = Low)
intername='UTC_effects'
save_file_path= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1000/',  'UTC_effects', '.csv', sep="")  # 字符串连接

write.csv(Data_marginaleffects_UTC1000_UTC,save_file_path)




#########################二次回归模型训练  marginal effects################################

##########################################
# 提取模型系数
#coeffs <-coef(model) # 获取系数

##calculate and save marginal effects, with errors clustered regionally
#cov <- vcovCR(model,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解


#for UTC1000_2
UTC1000_values <- seq(min(query_sample2$UTC1000), max(query_sample2$UTC1000), length.out = 100)

me <- coeffs["UTC1000"] + coeffs["UTC1000_2"]*UTC1000_values
se <- sqrt(cov["UTC1000", "UTC1000"] +
             UTC1000_values^2*cov["UTC1000_2", "UTC1000_2"] +
             2*UTC1000_values*cov["UTC1000", "UTC1000_2"])



###############################绘图################################
# 上限值
UP<- me+1.96 *se
# 下限值
Low<- me-1.96 *se

# 获取变量数值
UTC1000<-UTC1000_values

# 构建数据框
Data_marginaleffects_UTC1000_UTC1000 <- data.frame(UTC1000 = UTC1000, mean_value = me, UP_value = UP, Low_value = Low)


# 8. 数据保存
#intername='UTC_UTC1000'
# 构建数据框
# 构建数据框并直接命名


save_file_path= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1000/',  'UTC_UTC1000', '.csv', sep="")  # 字符串连接

write.csv(Data_marginaleffects_UTC1000_UTC1000, save_file_path)





########################################Marginal effect NOUTC1000 #########################################

model <- felm(sentiment ~ 1+ UTC1000+ UTC1000:NOUTC1000+
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                Skintemperature+
                population1000+ nightlight1000+ impervious1000+GDP
              |yearmonthday+userid+ CityName,  data = query_sample2)


# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)



##########################################
# 提取模型系数
coeffs <-coef(model) # 获取系数

##calculate and save marginal effects, with errors clustered regionally
cov <- vcovCR(model,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解


#for NOUTC1000
NOUTC1000_values <- seq(min(query_sample2$NOUTC1000), max(query_sample2$NOUTC1000), length.out = 100)

me <- coeffs["UTC1000"] + coeffs["UTC1000:NOUTC1000"]*NOUTC1000_values
se <- sqrt(cov["UTC1000", "UTC1000"] +
             NOUTC1000_values^2*cov["UTC1000:NOUTC1000", "UTC1000:NOUTC1000"] +
             2*NOUTC1000_values*cov["UTC1000", "UTC1000:NOUTC1000"])



###############################绘图################################
# 上限值
UP<- me+1.96 *se
# 下限值
Low<- me-1.96 *se

# 获取变量数值
NOUTC1000<-NOUTC1000_values

# 构建数据框
Data_marginaleffects_UTC1000_NOUTC <- data.frame(NOUTC1000 = NOUTC1000, mean_value = me, UP_value = UP, Low_value = Low)


# 8. 数据保存
intername='UTC_NOUTC1000'
# 构建数据框
# 构建数据框并直接命名


save_file_path= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1000/',  'UTC_NOUTC1000', '.csv', sep="")  # 字符串连接

write.csv(Data_marginaleffects_UTC1000_NOUTC, save_file_path)




#########################################Precipitation########################################



model <- felm(sentiment ~ 1+ UTC1000+ UTC1000:Precipitation+
                humidity+ Wind + cloudcover+ Surface_pressure+
                Skintemperature+
                population1000+ nightlight1000+ impervious1000+settlement1000+GDP
              |yearmonthday+userid+ CityName,  data = query_sample2)


# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)


##########################################
# 提取模型系数
coeffs <-coef(model) # 获取系数

##calculate and save marginal effects, with errors clustered regionally
cov <- vcovCR(model,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解



#for Precipitation
Precipitation_values <- seq(min(query_sample2$Precipitation), max(query_sample2$Precipitation), length.out = 100)

me <- coeffs["UTC1000"] + coeffs["UTC1000:Precipitation"]*Precipitation_values
se <- sqrt(cov["UTC1000", "UTC1000"] +
             Precipitation_values^2*cov["UTC1000:Precipitation", "UTC1000:Precipitation"] +
             2*Precipitation_values*cov["UTC1000", "UTC1000:Precipitation"])


###############################绘图################################
# 上限值
UP<- me+1.96 *se
# 下限值
Low<- me-1.96 *se

# 获取变量数值
Precipitation<-Precipitation_values

# 8. 数据保存


# 构建数据框
Data_marginaleffects_UTC1000_Precipitation <- data.frame(Precipitation = Precipitation, mean_value = me, UP_value = UP, Low_value = Low)
#保存数据
intername='UTC_Precipitation'

# 构建数据框
# 构建数据框并直接命名


save_file_path= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1000/',  intername, '.csv', sep="")  # 字符串连接

write.csv(Data_marginaleffects_UTC1000_Precipitation, save_file_path)






#########################################humidity########################################

model <- felm(sentiment ~ 1+ UTC1000+ UTC1000:humidity+
                Precipitation+ Wind + cloudcover+ Surface_pressure+
                Skintemperature+
                population1000+ nightlight1000+ impervious1000+GDP
              |yearmonthday+userid+ CityName,  data = query_sample2)


# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)


##########################################
# 提取模型系数
coeffs <-coef(model) # 获取系数

##calculate and save marginal effects, with errors clustered regionally
cov <- vcovCR(model,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解


#for humidity
humidity_values <- seq(min(query_sample2$humidity), max(query_sample2$humidity), length.out = 100)

me <- coeffs["UTC1000"] + coeffs["UTC1000:humidity"]*humidity_values
se <- sqrt(cov["UTC1000", "UTC1000"] +
             humidity_values^2*cov["UTC1000:humidity", "UTC1000:humidity"] +
             2*humidity_values*cov["UTC1000", "UTC1000:humidity"])



###############################绘图################################
# 上限值
UP<- me+1.96 *se
# 下限值
Low<- me-1.96 *se

# 获取变量数值
humidity<-humidity_values

# 构建数据框
Data_marginaleffects_UTC1000_humidity <- data.frame(humidity = humidity, mean_value = me, UP_value = UP, Low_value = Low)


# 8. 数据保存
intername='UTC_humidity'
# 构建数据框
# 构建数据框并直接命名


save_file_path= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1000/',  'UTC_humidity', '.csv', sep="")  # 字符串连接

write.csv(Data_marginaleffects_UTC1000_humidity, save_file_path)








#########################################Wind########################################

model <- felm(sentiment ~ 1+ UTC1000+ UTC1000:Wind+
                Precipitation+  humidity + cloudcover+ Surface_pressure+
                Skintemperature+
                population1000+ nightlight1000+ impervious1000+GDP
              |yearmonthday+userid+ CityName,  data = query_sample2)

# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)




##########################################
# 提取模型系数
coeffs <-coef(model) # 获取系数

##calculate and save marginal effects, with errors clustered regionally
cov <- vcovCR(model,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解


#for Wind
Wind_values <- seq(min(query_sample2$Wind), max(query_sample2$Wind), length.out = 100)

me <- coeffs["UTC1000"] + coeffs["UTC1000:Wind"]*Wind_values
se <- sqrt(cov["UTC1000", "UTC1000"] +
             Wind_values^2*cov["UTC1000:Wind", "UTC1000:Wind"] +
             2*Wind_values*cov["UTC1000", "UTC1000:Wind"])




###############################绘图################################
# 上限值
UP<- me+1.96 *se
# 下限值
Low<- me-1.96 *se

# 获取变量数值
Wind<-Wind_values

# 构建数据框
Data_marginaleffects_UTC1000_Wind <- data.frame(Wind = Wind, mean_value = me, UP_value = UP, Low_value = Low)


# 8. 数据保存
intername='UTC_Wind'
# 构建数据框
# 构建数据框并直接命名


save_file_path= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1000/',  intername, '.csv', sep="")  # 字符串连接

write.csv(Data_marginaleffects_UTC1000_Wind, save_file_path)




#########################################cloudcover########################################

model <- felm(sentiment ~ 1+ UTC1000+ UTC1000:cloudcover+
                Precipitation+  humidity+ Wind + Surface_pressure+
                Skintemperature+
                population1000+ nightlight1000+ impervious1000+GDP
              |yearmonthday+userid+ CityName,  data = query_sample2)


# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)


##########################################
# 提取模型系数
coeffs <-coef(model) # 获取系数

##calculate and save marginal effects, with errors clustered regionally
cov <- vcovCR(model,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解


#for cloudcover
cloudcover_values <- seq(min(query_sample2$cloudcover), max(query_sample2$cloudcover), length.out = 100)

me <- coeffs["UTC1000"] + coeffs["UTC1000:cloudcover"]*cloudcover_values
se <- sqrt(cov["UTC1000", "UTC1000"] +
             cloudcover_values^2*cov["UTC1000:cloudcover", "UTC1000:cloudcover"] +
             2*cloudcover_values*cov["UTC1000", "UTC1000:cloudcover"])


###############################绘图################################
# 上限值
UP<- me+1.96 *se
# 下限值
Low<- me-1.96 *se

# 获取变量数值
cloudcover<-cloudcover_values

# 构建数据框
Data_marginaleffects_UTC1000_cloudcover <- data.frame(cloudcover = cloudcover, mean_value = me, UP_value = UP, Low_value = Low)


# 8. 数据保存
intername='UTC_cloudcover'
# 构建数据框
# 构建数据框并直接命名


save_file_path= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1000/',  'UTC_cloudcover', '.csv', sep="")  # 字符串连接

write.csv(Data_marginaleffects_UTC1000_cloudcover, save_file_path)




#########################################Surface_pressure########################################

model <- felm(sentiment ~ 1+ UTC1000+ UTC1000:Surface_pressure+
                Precipitation+  humidity+ Wind + cloudcover+
                Skintemperature+
                population1000+ nightlight1000+ impervious1000+GDP
              |yearmonthday+userid+ CityName,  data = query_sample2)


# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)

##########################################
# 提取模型系数
coeffs <-coef(model) # 获取系数

##calculate and save marginal effects, with errors clustered regionally
cov <- vcovCR(model,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解



#for Surface_pressure
Surface_pressure_values <- seq(min(query_sample2$Surface_pressure), max(query_sample2$Surface_pressure), length.out = 100)

me <- coeffs["UTC1000"] + coeffs["UTC1000:Surface_pressure"]*Surface_pressure_values
se <- sqrt(cov["UTC1000", "UTC1000"] +
             Surface_pressure_values^2*cov["UTC1000:Surface_pressure", "UTC1000:Surface_pressure"] +
             2*Surface_pressure_values*cov["UTC1000", "UTC1000:Surface_pressure"])


###############################绘图################################
# 上限值
UP<- me+1.96 *se
# 下限值
Low<- me-1.96 *se

# 获取变量数值
Surface_pressure<-Surface_pressure_values

# 构建数据框
Data_marginaleffects_UTC1000_Surface_pressure <- data.frame(Surface_pressure = Surface_pressure, mean_value = me, UP_value = UP, Low_value = Low)


# 8. 数据保存
# 构建数据框
# 构建数据框并直接命名


save_file_path= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1000/',  'UTC_Surface_pressure', '.csv', sep="")  # 字符串连接

write.csv(Data_marginaleffects_UTC1000_Surface_pressure, save_file_path)






#########################################Skintemperature########################################

model <- felm(sentiment ~ 1+ UTC1000+ UTC1000:Skintemperature+
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                population1000+ nightlight1000+ impervious1000+GDP
              |yearmonthday+userid+ CityName,  data = query_sample2)


# 查看模型摘要
print(summary(model))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)


##########################################
# 提取模型系数
coeffs <-coef(model) # 获取系数

##calculate and save marginal effects, with errors clustered regionally
cov <- vcovCR(model,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解


#for Skintemperature
Skintemperature_values <- seq(min(query_sample2$Skintemperature), max(query_sample2$Skintemperature), length.out = 100)

me <- coeffs["UTC1000"] + coeffs["UTC1000:Skintemperature"]*Skintemperature_values
se <- sqrt(cov["UTC1000", "UTC1000"] +
             Skintemperature_values^2*cov["UTC1000:Skintemperature", "UTC1000:Skintemperature"] +
             2*Skintemperature_values*cov["UTC1000", "UTC1000:Skintemperature"])



###############################绘图################################
# 上限值
UP<- me+1.96 *se
# 下限值
Low<- me-1.96 *se

# 获取变量数值
Skintemperature<-Skintemperature_values

# 构建数据框
Data_marginaleffects_UTC1000_Skintemperature <- data.frame(Skintemperature = Skintemperature, mean_value = me, UP_value = UP, Low_value = Low)


# 8. 数据保存
intername='UTC_Skintemperature'
# 构建数据框
# 构建数据框并直接命名


save_file_path= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1000/',  'UTC_Skintemperature', '.csv', sep="")  # 字符串连接

write.csv(Data_marginaleffects_UTC1000_Skintemperature, save_file_path)



#########################################population1000########################################

model <- felm(sentiment ~ 1+ UTC1000+ UTC1000:population1000+
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                Skintemperature+
                nightlight1000+ settlement1000+ impervious1000+GDP
              |yearmonthday+userid+ CityName,  data = query_sample2)


# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)
##########################################

# 提取模型系数
coeffs <-coef(model) # 获取系数

##calculate and save marginal effects, with errors clustered regionally
cov <- vcovCR(model,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解


#for population1000
population1000_values <- seq(min(query_sample2$population1000), max(query_sample2$population1000), length.out = 100)

me <- coeffs["UTC1000"] + coeffs["UTC1000:population1000"]*population1000_values
se <- sqrt(cov["UTC1000", "UTC1000"] +
             population1000_values^2*cov["UTC1000:population1000", "UTC1000:population1000"] +
             2*population1000_values*cov["UTC1000", "UTC1000:population1000"])



###############################绘图################################
# 上限值
UP<- me+1.96 *se
# 下限值
Low<- me-1.96 *se

# 获取变量数值
population1000<-population1000_values

# 构建数据框
Data_marginaleffects_UTC1000_population1000 <- data.frame(population1000 = population1000, mean_value = me, UP_value = UP, Low_value = Low)


# 8. 数据保存
intername='UTC_population1000'
# 构建数据框
# 构建数据框并直接命名


save_file_path= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1000/',  'UTC_population1000', '.csv', sep="")  # 字符串连接

write.csv(Data_marginaleffects_UTC1000_population1000, save_file_path)



#########################################nightlight1000########################################

model <- felm(sentiment ~ 1+ UTC1000+ UTC1000:nightlight1000+
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                Skintemperature+
                population1000+ impervious1000+GDP
              |yearmonthday+userid+ CityName,  data = query_sample2)

# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)

##########################################
# 提取模型系数
coeffs <-coef(model) # 获取系数

##calculate and save marginal effects, with errors clustered regionally
cov <- vcovCR(model,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解


#for nightlight1000
nightlight1000_values <- seq(min(query_sample2$nightlight1000), max(query_sample2$nightlight1000), length.out = 100)

me <- coeffs["UTC1000"] + coeffs["UTC1000:nightlight1000"]*nightlight1000_values
se <- sqrt(cov["UTC1000", "UTC1000"] +
             nightlight1000_values^2*cov["UTC1000:nightlight1000", "UTC1000:nightlight1000"] +
             2*nightlight1000_values*cov["UTC1000", "UTC1000:nightlight1000"])



###############################绘图################################
# 上限值
UP<- me+1.96 *se
# 下限值
Low<- me-1.96 *se

# 获取变量数值
nightlight1000<-nightlight1000_values

# 构建数据框
Data_marginaleffects_UTC1000_nightlight1000 <- data.frame(nightlight1000 = nightlight1000, mean_value = me, UP_value = UP, Low_value = Low)


# 8. 数据保存
intername='UTC_nightlight1000'
# 构建数据框
# 构建数据框并直接命名


save_file_path= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1000/',  'UTC_nightlight1000', '.csv', sep="")  # 字符串连接

write.csv(Data_marginaleffects_UTC1000_nightlight1000, save_file_path)



#########################################settlement1000########################################

model <- felm(sentiment ~ 1+ UTC1000+ UTC1000:settlement1000+
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                Skintemperature+
                population1000+ nightlight1000+ impervious1000+GDP
              |yearmonthday+userid+ CityName,  data = query_sample2)


# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)



##########################################
# 提取模型系数
coeffs <-coef(model) # 获取系数

##calculate and save marginal effects, with errors clustered regionally
cov <- vcovCR(model,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解


#for settlement1000
settlement1000_values <- seq(min(query_sample2$settlement1000), max(query_sample2$settlement1000), length.out = 100)

me <- coeffs["UTC1000"] + coeffs["UTC1000:settlement1000"]*settlement1000_values
se <- sqrt(cov["UTC1000", "UTC1000"] +
             settlement1000_values^2*cov["UTC1000:settlement1000", "UTC1000:settlement1000"] +
             2*settlement1000_values*cov["UTC1000", "UTC1000:settlement1000"])



###############################绘图################################
# 上限值
UP<- me+1.96 *se
# 下限值
Low<- me-1.96 *se

# 获取变量数值
settlement1000<-settlement1000_values

# 构建数据框
Data_marginaleffects_UTC1000_settlement1000 <- data.frame(settlement1000 = settlement1000, mean_value = me, UP_value = UP, Low_value = Low)


# 8. 数据保存
intername='UTC_settlement1000'
# 构建数据框
# 构建数据框并直接命名


save_file_path= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1000/',  'UTC_settlement1000', '.csv', sep="")  # 字符串连接

write.csv(Data_marginaleffects_UTC1000_settlement1000, save_file_path)








#########################################impervious1000########################################

model <- felm(sentiment ~ 1+ UTC1000+UTC1000:impervious1000+
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                Skintemperature+
                population1000+ nightlight1000+GDP
              |yearmonthday+userid+ CityName,  data = query_sample2)


# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)

##########################################
# 提取模型系数
coeffs <-coef(model) # 获取系数

##calculate and save marginal effects, with errors clustered regionally
cov <- vcovCR(model,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解


#for impervious1000
impervious1000_values <- seq(min(query_sample2$impervious1000), max(query_sample2$impervious1000), length.out = 100)

me <- coeffs["UTC1000"] + coeffs["UTC1000:impervious1000"]*impervious1000_values
se <- sqrt(cov["UTC1000", "UTC1000"] +
             impervious1000_values^2*cov["UTC1000:impervious1000", "UTC1000:impervious1000"] +
             2*impervious1000_values*cov["UTC1000", "UTC1000:impervious1000"])



###############################绘图################################
# 上限值
UP<- me+1.96 *se
# 下限值
Low<- me-1.96 *se

# 获取变量数值
impervious1000<-impervious1000_values

# 构建数据框
Data_marginaleffects_UTC1000_impervious1000 <- data.frame(impervious1000 = impervious1000, mean_value = me, UP_value = UP, Low_value = Low)


# 8. 数据保存
intername='UTC_impervious1000'
# 构建数据框
# 构建数据框并直接命名


save_file_path= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1000/',  'UTC_impervious1000', '.csv', sep="")  # 字符串连接

write.csv(Data_marginaleffects_UTC1000_impervious1000, save_file_path)



#########################################GDP########################################

model <- felm(sentiment ~ 1+ UTC1000 + UTC1000:GDP+
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                Skintemperature+
                population1000+ nightlight1000+ impervious1000
              |yearmonthday+userid+ CityName,  data = query_sample2)


# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)

##########################################
# 提取模型系数
coeffs <-coef(model) # 获取系数

##calculate and save marginal effects, with errors clustered regionally
cov <- vcovCR(model,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解


#for GDP
GDP_values <- seq(min(query_sample2$GDP), max(query_sample2$GDP), length.out = 100)

me <- coeffs["UTC1000"] + coeffs["UTC1000:GDP"]*GDP_values
se <- sqrt(cov["UTC1000", "UTC1000"] +
             GDP_values^2*cov["UTC1000:GDP", "UTC1000:GDP"] +
             2*GDP_values*cov["UTC1000", "UTC1000:GDP"])



###############################绘图################################
# 上限值
UP<- me+1.96 *se
# 下限值
Low<- me-1.96 *se

# 获取变量数值
GDP<-GDP_values

# 构建数据框
Data_marginaleffects_UTC1000_GDP <- data.frame(GDP = GDP, mean_value = me, UP_value = UP, Low_value = Low)


# 8. 数据保存
intername='UTC_GDP'
# 构建数据框
# 构建数据框并直接命名


save_file_path= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1000/',  'UTC_GDP', '.csv', sep="")  # 字符串连接

write.csv(Data_marginaleffects_UTC1000_GDP, save_file_path)









#########################二次回归模型中的交互项模型 ################################


#########################################UTC1000 城市环境和经济的交互项########################################

model <- felm(sentiment ~ 1+ UTC1000+UTC1000_2+ UTC1000:impervious1000+ UTC1000:population1000+ 
                UTC1000:nightlight1000+UTC1000:GDP+  UTC1000:settlement1000+
                Precipitation+  humidity+ Wind + cloudcover+ Surface_pressure+
                Skintemperature
              |yearmonthday+userid+ CityName,  data = query_sample2)


# 查看模型摘要
print(summary(model))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)


close(con)






