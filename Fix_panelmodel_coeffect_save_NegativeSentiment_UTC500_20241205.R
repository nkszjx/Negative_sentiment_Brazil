

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
library(extrafont)
library(showtext)
library(future)
library(future.apply)
library(stargazer)
library(tidyr)
############################## 读取数据############################################
## 数据路径

sentiment_UTC_data='dataset2018_2019_2020_2021_numbermorethan10_unique'

str0='E:/Sentiment_Brazil/R_codes/'

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
result <- as.data.table(query_sample2)

# 计算均值和获取第一个字符串
#result <- result[, lapply(.SD, function(x) if(is.numeric(x)) mean(x, na.rm = TRUE) else unique(x)[1]), 
#by = .(userid, yearmonthday, CityName)]
#write.csv(result,'E:/Sentiment_Brazil/R_codes/pdataframe20251013.csv')



####对于 100,000 行的数据，dplyr 和 data.table 通常都能在数秒内完成。

#query_sample2 <- query_sample2 %>%
#group_by(userid, yearmonthday, CityName) %>%
#summarise(
# across(where(is.numeric), mean, na.rm = TRUE),  # 对数值列取均值
# across(where(is.character), ~ unique(.)[1]),   # 对字符串列取第一个唯一值
# .groups = "drop"
# )

# 原始UTC数据扩大100
#query_sample2$UTC500<- 100*query_sample2$UTC500
#query_sample2$UTC1000<- 100*query_sample2$UTC1000
#query_sample2$UTC1500<- 100*query_sample2$UTC1500
# 原始NOUTC数据扩大100
#query_sample2$NOUTC500<- 100*query_sample2$NOUTC500
#query_sample2$NOUTC1000<- 100*query_sample2$NOUTC1000
#query_sample2$NOUTC1500<- 100*query_sample2$NOUTC1500

sd_utc5<-sd(query_sample2$UTC500)
sd_utc10<-sd(query_sample2$UTC1000)
sd_utc15<-sd(query_sample2$UTC1500)
sd_sent<-sd(query_sample2$sentiment)
mean_UTC500<-mean(query_sample2$UTC500)
mean_UTC1000<-mean(query_sample2$UTC1000)
mean_UTC1500<-mean(query_sample2$UTC1500)
mean_sent<-mean(query_sample2$sentiment)

mean_PM25<-mean(query_sample2$PM25)
sd_PM25<-sd(query_sample2$PM25)

mean_PM25_2<-mean(query_sample2$PM25_2)
sd_PM25_2<-sd(query_sample2$PM25_2)

mean_Precipitation<-mean(query_sample2$Precipitation)
sd_Precipitation<-sd(query_sample2$Precipitation)
mean_humidity<-mean(query_sample2$humidity)
sd_humidity<-sd(query_sample2$humidity)


mean_GDP<-mean(query_sample2$GDP)
sd_GDP<-sd(query_sample2$GDP)


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

#######################所研究的变量不去中心化处理，其他变量做中心化处理##############################################
## ####### 去中心化的UTC#############
## query_sample2$UTC500<-query_sample2$UTC500-mean_UTC500
## query_sample2$UTC1000<-query_sample2$UTC1000-mean_UTC1000
## query_sample2$UTC1500<-query_sample2$UTC1500-mean_UTC1500
## ####### 去中心化的UTC_2#############
## # 二次项
## query_sample2$UTC500_2<-query_sample2$UTC500*query_sample2$UTC500
## query_sample2$UTC1000_2<-query_sample2$UTC1000*query_sample2$UTC1000
## query_sample2$UTC1500_2<-query_sample2$UTC1500*query_sample2$UTC1500


#####################################################################

######## 去中心化的NOUTC#############
query_sample2$NOUTC500<-query_sample2$NOUTC500-mean_NOUTC500
query_sample2$NOUTC1000<-query_sample2$NOUTC1000-mean_NOUTC1000
query_sample2$NOUTC1500<-query_sample2$NOUTC1500-mean_NOUTC1500
#

######## 去中心化的 #############
query_sample2$Skintemperature<- query_sample2$Skintemperature- mean_Skintemperature
query_sample2$Surface_pressure<- query_sample2$Surface_pressure- mean_Surface_pressure

query_sample2$PM25<- query_sample2$PM25-mean_PM25

query_sample2$Precipitation<- query_sample2$Precipitation- mean_Precipitation
query_sample2$humidity <- query_sample2$humidity- mean_humidity

query_sample2$population500<- query_sample2$population500 - mean_population500
query_sample2$population1000<- query_sample2$population1000 - mean_population1000
query_sample2$population1500<- query_sample2$population1500 - mean_population1500

query_sample2$impervious500 <- query_sample2$impervious500 - mean_impervious500
query_sample2$impervious1000 <- query_sample2$impervious1000 - mean_impervious1000
query_sample2$impervious1500 <- query_sample2$impervious1500 - mean_impervious1500

query_sample2$nightlight500<- query_sample2$nightlight500 - mean_nightlight500
query_sample2$nightlight1000<- query_sample2$nightlight1000 - mean_nightlight1000
query_sample2$nightlight1500<- query_sample2$nightlight1500 - mean_nightlight1500

query_sample2$settlement500<- query_sample2$settlement500 - mean_settlement500
query_sample2$settlement1000<- query_sample2$settlement1000 - mean_settlement1000
query_sample2$settlement1500<- query_sample2$settlement1500 - mean_settlement1500

################## 二次化 #######################
query_sample2$Precipitation_2<- query_sample2$Precipitation*query_sample2$Precipitation
query_sample2$humidity_2<- query_sample2$humidity*query_sample2$humidity


#######################################增加疫情前后列#########################################################
query_sample2 <- na.omit(query_sample2)

# 获取年份数据2018-2022
year <- substr(query_sample2$yearmonth, start = 2, stop = 5)
names(year)[1] <- "year"
query_sample2 <- cbind (query_sample2,year)
# 获取月份数据2018-2022
month <- substr(query_sample2$yearmonth, start = 7, stop = 8)
names(month)[1] <- "month"
query_sample2 <- cbind (query_sample2,month)

# 获取 covid-19 前后标签
covid <- ifelse(query_sample2$year < '2020', "before",
                ifelse(query_sample2$year == '2020' & (query_sample2$month == '01' | query_sample2$month == '02'), "before", "during"))
covid <- data.frame(covid)  # 将向量转换为数据框
names(covid) <- "covid"  # 重命名列为 "covid"
query_sample2 <- cbind(query_sample2, covid)  # 合并covid列到数据框


############查看各个系数相关性############
library(corrplot)
corrplot(cor(query_sample2[, c("UTC500", "NOUTC500","Precipitation", "humidity", "Wind", "cloudcover", 
                               "Surface_pressure", "Skintemperature", "population500", 
                               "nightlight500", "impervious500", "settlement500","GDP")], use = "complete.obs"))

set.seed(123)
query_sample_reduced <- query_sample2[sample(1:nrow(query_sample2), size = 1000000), ]
##################################################################################################################




#########################一次回归模型训练 ################################
model_1 <- felm(sentiment ~ 1+ UTC500
                |yearmonthday+userid,  data = query_sample2)


# 查看模型摘要
print(summary(model_1))

# 指定输出文件的路径和名称
output_file <- "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/ResultsUTC500_fix_indivdual_city_day_20250923.txt"
con <- file(output_file, open = "a")# 打开文件连接，追加模式
writeLines("\n--- New Model Summary ---\n", con)# 写入一个分隔符（可选），以便区分不同的模型摘要
model_summary <- capture.output(summary(model_1))# 捕获模型摘要的输出
writeLines(model_summary, con = output_file)# 将模型摘要追加写入文件
# 关闭文件连接 close(con)

############################## 线性模型####################################################
model500_01 <- felm(sentiment ~ 1 + UTC500 + NOUTC500+ PM25+
                  Precipitation + humidity + Wind + cloudcover + Surface_pressure+Skintemperature +
                  settlement500+population500+ nightlight500+GDP
                | yearmonthday + userid, 
                data = query_sample2)
print(summary(model500_01))

model1000_01 <- felm(sentiment ~ 1 + UTC1000 + NOUTC1000+ PM25+
                       Precipitation + humidity + Wind + cloudcover + Surface_pressure+Skintemperature +
                       settlement1000+population1000+ nightlight1000+GDP
                     | yearmonthday + userid, 
                     data = query_sample2)
print(summary(model1000_01))


model1500_01 <- felm(sentiment ~ 1 + UTC1500 + NOUTC1500+ PM25+
                       Precipitation + humidity + Wind + cloudcover + Surface_pressure+Skintemperature +
                       settlement1500+population1500+ nightlight1500+GDP
                     | yearmonthday + userid, 
                     data = query_sample2)
print(summary(model1500_01))




################################ 线性模型 交互项#######################################


model1500_01 <- felm(sentiment ~ 1 + UTC1500 + UTC1500:covid+ NOUTC1500+ PM25+ 
                       Precipitation + humidity + Wind + cloudcover + Surface_pressure+Skintemperature +
                       settlement1500+population1500+ nightlight1500+GDP
                     | yearmonthday + userid, 
                     data = query_sample2)
print(summary(model1500_01))

model1000_001 <- felm(sentiment ~ 1 + UTC1000 + UTC1000:covid+ NOUTC1000+ PM25+ 
                        Precipitation + humidity + Wind + cloudcover + Surface_pressure+Skintemperature +
                        settlement1000+population1000+ nightlight1000+GDP
                      | yearmonthday + userid, 
                      data = query_sample2)
print(summary(model1000_001))




model500_001 <- felm(sentiment ~ 1 + UTC500 + UTC500:covid+ NOUTC500+ PM25+ 
                       Precipitation + humidity + Wind + cloudcover + Surface_pressure+Skintemperature +
                       settlement500+population500+ nightlight500+GDP
                     | yearmonthday + userid, 
                     data = query_sample2)
print(summary(model500_001))

####################################UTC 常规##############################
model_2 <- felm(sentiment ~ 1 + UTC500 + UTC500_2+ NOUTC500+ PM25+
                  Precipitation + humidity + Wind + cloudcover + Surface_pressure+Skintemperature +
                  settlement500+population500+ nightlight500+GDP
                | yearmonthday + userid, 
                data = query_sample2)
print(summary(model_2))

####################### 计算VIF############################
vif_values <- vif(model_2)

# 打印VIF值；VIF值大于10才被认为存在严重的共线性问题;增加样本量可能有助于减轻共线性问题
print("VIF:")
print(vif_values)
#####################保存模型参数#######################
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model_2))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)




####################################二次项模型中UTC500 和UTC500_2 与疫情的交互##############################
## 存在二次项不显著 UTC500_2:covidduring -1.7748092  1.5852590  -1.120 0.262897 

model_03 <- felm(sentiment ~ 1 + UTC500 + UTC500_2+ UTC500_2:covid+ UTC500:covid +  NOUTC500+PM25+
                   Precipitation + humidity + Wind + cloudcover + Surface_pressure+Skintemperature +
                   settlement500+population500+ nightlight500+GDP
                 | yearmonthday + userid, 
                 data = query_sample2)
print(summary(model_03))

#####################保存模型参数#######################
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model_03))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)


####################################一次项模型中UTC500 疫情的交互##############################

model_3 <- felm(sentiment ~ 1 + UTC500 + UTC500_2+ UTC500:covid +  NOUTC500+PM25+
                  Precipitation + humidity + Wind + cloudcover + Surface_pressure+Skintemperature +
                  settlement500+population500+ nightlight500+GDP
                | yearmonthday + userid, 
                data = query_sample2)
print(summary(model_3))

#####################保存模型参数#######################
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model_3))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)


##################################模型协方差计算#################################
# 提取模型系数
coeffs <-coef(model_3) # 获取系数

##calculate and save marginal effects, with errors clustered regionally

cov<- vcovCR(model_3,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解
#cov_simple <- vcov(model_3)  # 协方差矩阵,



############################ UTC effects_before ###########################################

# 1. 提取UTC500和UTC500_2的方差-协方差矩阵（仅保留这两个变量）
# 确保仅包含目标变量的协方差，避免其他变量干扰
V <- cov[c("UTC500", "UTC500_2"), c("UTC500", "UTC500_2")]

# 2. 提取UTC500和UTC500_2的系数（确认UTC500_2是UTC500的平方项）
beta <- c(coeffs["UTC500"], coeffs["UTC500_2"])

# 3. 获取模型残差自由度（用于计算t分布分位数）
residual_df <- df.residual(model_3)

# 4. 构建UTC500的取值范围（0到1之间生成200个点）
UTC <- seq(0, 1, length.out = 200)
# 构建设计矩阵（包含一次项和二次项）
X <- cbind(UTC, UTC^2)  # 确保与模型中的UTC500和UTC500_2对应

# 5. 计算平均效应
mean_effect <- X %*% beta 

# 6. 计算效应的标准误差
# 利用方差-协方差矩阵计算每个点的标准误
se <- sqrt(rowSums(X %*% V * X))

# 7. 计算95%置信区间（t分布）
# 注意：qt(0.025, df)为负值，因此下限=均值+se*负值，上限=均值-se*负值
ci_low <- mean_effect + se * qt(0.025, df = residual_df)
ci_high <- mean_effect - se * qt(0.025, df = residual_df)

# 8. 整理结果并保存
# 构建数据框（包含UTC值、平均效应及置信区间）
Data_effects_UTC500_UTC_before <- data.frame(
  UTC = UTC,
  mean_value = as.vector(mean_effect),  # 转换为向量便于存储
  Low_value = as.vector(ci_low),
  UP_value = as.vector(ci_high)
)

save_file_path= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_effects_before', '.csv', sep="")  # 字符串连接
write.csv(Data_effects_UTC500_UTC_before,save_file_path)




############################ UTC effects during ###########################################


####################### 1. 提取疫情期间效应所需的系数与协方差 ##############################
# （1）提取关键系数：UTC500主效应、UTC500二次项、UTC500:covidduring交互项
# 提取所有系数（明确命名便于后续调用）
beta1 <- coeffs["UTC500"]          # UTC500主效应系数（β₁）
beta2 <- coeffs["UTC500_2"]        # UTC500二次项系数（β₂）
beta3 <- coeffs["UTC500:covidduring"]  # 交互项系数（β₃）

# （2）提取上述3个系数的协方差矩阵（含三者间的协方差，影响标准误计算）
# 协方差矩阵需包含：UTC500、UTC500_2、UTC500:covidduring
# （2）提取上述3个系数的协方差矩阵（含三者间的协方差，影响标准误计算）
# 协方差矩阵需包含：UTC500、UTC500_2、UTC500:covidduring

cov_matrix <- as.matrix(cov)  # 转换为矩阵格式，便于提取子集

V_original <- cov_matrix[
  rownames(cov_matrix) %in% c("UTC500", "UTC500_2", "UTC500:covidduring"),
  colnames(cov_matrix) %in% c("UTC500", "UTC500_2", "UTC500:covidduring")
]

# 重命名行/列，避免混淆
rownames(V_original) <- colnames(V_original) <- c("beta1", "beta2", "beta3")

####################### 2. 构建匹配维度的系数与协方差矩阵 ##############################
# （1）疫情期间的效应系数（2个元素，与X的列数匹配）
beta <- c(
  beta_linear = beta1 + beta3,  # 合并后的线性项系数（β₁+β₃）
  beta_quad = beta2             # 二次项系数（β₂）
)

# （2）构建2×2协方差矩阵（与beta、X维度匹配）
V <- matrix(
  nrow = 2, ncol = 2,
  dimnames = list(c("beta_linear", "beta_quad"), c("beta_linear", "beta_quad"))
)
# 填充协方差矩阵（基于方差运算规则）
V["beta_linear", "beta_linear"] <- V_original["beta1","beta1"] + V_original["beta3","beta3"] + 2*V_original["beta1","beta3"]  # Var(β₁+β₃)
V["beta_linear", "beta_quad"]   <- V_original["beta1","beta2"] + V_original["beta3","beta2"]  # Cov(β₁+β₃, β₂)
V["beta_quad", "beta_linear"]   <- V["beta_linear", "beta_quad"]  # 协方差矩阵对称
V["beta_quad", "beta_quad"]     <- V_original["beta2","beta2"]  # Var(β₂)




####################### 3. 计算疫情期间UTC500的效应及置信区间 ##############################
# （1）构建UTC500的取值范围（与疫情前一致，便于对比，0到1生成200个点）
UTC <- seq(0, 1, length.out = 200)

# （2）构建设计矩阵X：对应疫情期间的效应公式
# X第一列：UTC500（对应线性项总系数 beta_linear）
# X第二列：UTC500²（对应二次项系数 beta_quad）
X <- cbind(
  UTC = UTC,
  UTC_sq = UTC^2
)

# （3）计算疫情期间UTC500的平均效应
mean_effect <- X %*% beta  # 矩阵乘法，得到每个UTC值对应的效应

# （4）计算效应的聚类稳健标准误（关键：考虑3个系数的协方差）
# 公式逻辑：se = sqrt( X × V × X' )，rowSums(X %*% V * X) 等价于对角线元素提取
se <- sqrt(rowSums(X %*% V * X))

# （5）计算95%置信区间（t分布，使用模型残差自由度）
residual_df <- df.residual(model_3)  # 模型输出中为 4256966
ci_low <- mean_effect + se * qt(0.025, df = residual_df)  # 下限
ci_high <- mean_effect - se * qt(0.025, df = residual_df)  # 上限


####################### 4. 整理结果并保存 ##############################
Data_effects_UTC500_UTC_during <- data.frame(
  UTC = UTC,
  mean_value = as.vector(mean_effect),  # 转换为向量便于存储
  Low_value = as.vector(ci_low),            # 95%置信区间下限
  UP_value = as.vector(ci_high)           # 95%置信区间上限
)

#步骤 7:  数据保存
save_file_path= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_effects_during', '.csv', sep="")  # 字符串连接
write.csv(Data_effects_UTC500_UTC_during,save_file_path)




######################### marginal effects################################


######################### 1. 提取模型系数#####################
beta1 <- coeffs["UTC500"]          # β_UTC500
beta2 <- coeffs["UTC500_2"]        # β_UTC500_2
beta3 <- coeffs["UTC500:covidduring"]  # β_UTC500:covidduring

cov_matrix <- as.matrix(cov)  # 转换为矩阵格式，便于提取子集
# 提取聚类稳健协方差矩阵（3个关键系数）
V <- cov_matrix[
  c("UTC500", "UTC500_2", "UTC500:covidduring"),
  c("UTC500", "UTC500_2", "UTC500:covidduring")
]
colnames(V) <- rownames(V) <- c("beta1", "beta2", "beta3")  # 重命名便于引用


####################### 2. 构建UTC500的取值范围 ##############################
UTC <- seq(0, 1, length.out = 200)  # 与之前一致的取值范围


####################### 3. 疫情前的边际效应 ##############################
# （1）边际效应值
me_before <- as.vector(beta1 + 2 * beta2 * UTC)

# （2）边际效应的标准误（基于方差性质）
# 方差 = Var(beta1) + (2*UTC)^2*Var(beta2) + 2*(2*UTC)*Cov(beta1, beta2)
var_me_before <- V["beta1","beta1"] + (2*UTC)^2 * V["beta2","beta2"] + 2*(2*UTC)*V["beta1","beta2"]
se_before <- sqrt(var_me_before)

# （3）95%置信区间
residual_df <- df.residual(model_3)
ci_low_before <- me_before + se_before * qt(0.025, df = residual_df)
ci_high_before <- me_before - se_before * qt(0.025, df = residual_df)


####################### 4. 疫情期间的边际效应 ##############################
# （1）边际效应值
me_during <- as.vector( (beta1 + beta3) + 2 * beta2 * UTC )

# （2）边际效应的标准误
# 方差 = Var(beta1+beta3) + (2*UTC)^2*Var(beta2) + 2*(2*UTC)*Cov(beta1+beta3, beta2)
# 其中：Var(beta1+beta3) = Var(beta1) + Var(beta3) + 2*Cov(beta1, beta3)
#       Cov(beta1+beta3, beta2) = Cov(beta1, beta2) + Cov(beta3, beta2)
var_me_during <- (V["beta1","beta1"] + V["beta3","beta3"] + 2*V["beta1","beta3"]) + 
  (2*UTC)^2 * V["beta2","beta2"] + 
  2*(2*UTC)*(V["beta1","beta2"] + V["beta3","beta2"])
se_during <- sqrt(var_me_during)

# （3）95%置信区间
ci_low_during <- me_during + se_during * qt(0.025, df = residual_df)
ci_high_during <- me_during - se_during * qt(0.025, df = residual_df)


####################### 5. 边际效应保存 ##############################
# 构建数据框,保存疫情前边界效应数据
marginaleffects_UTC500_before <- data.frame(UTC500 = UTC, mean_value = me_before, 
                                            UP_value = ci_high_before, Low_value = ci_low_before)
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_UTC500_before', '.csv', sep="")  # 字符串连接

write.csv(marginaleffects_UTC500_before, save_file_path_before)


# 构建数据框,保存疫情期间边界效应数据

marginaleffects_UTC500_during <- data.frame(UTC500 = UTC, mean_value = me_during, 
                                            UP_value = ci_high_during, Low_value = ci_low_during)
save_file_path_during= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_UTC500_during', '.csv', sep="")  # 字符串连接

write.csv(marginaleffects_UTC500_during, save_file_path_during)










####################################UTC与疫情和 PM25 的交互##############################

model_interaction0 <- felm(sentiment ~ 1 + UTC500+UTC500_2+ UTC500:PM25:covid + NOUTC500+ 
                             Precipitation+humidity+ Wind + cloudcover + Surface_pressure+Skintemperature +
                             population500 + nightlight500+ settlement500+GDP
                           | yearmonthday + userid , 
                           data = query_sample2)

print(summary(model_interaction0))


#####################保存模型参数#######################
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model_interaction0))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)

# 提取模型系数
coeffs <-coef(model_interaction0) # 获取系数

##calculate and save marginal effects, with errors clustered regionally
cov <- vcovCR(model_interaction0,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解


####################before COVID19 边界效应 #######################

# --------------------------
# 2. 提取模型参数（同前）
# --------------------------
alpha1 <- coeffs["UTC500"]                  # -8.0401
alpha2 <- coeffs["UTC500_2"]                # 9.5740
alpha3 <- coeffs["UTC500:PM25:covidbefore"] # 0.0012
alpha4 <- coeffs["UTC500:PM25:covidduring"] # 0.0106

# 协方差矩阵（用于标准误，可选，若仅可视化效应值可省略）
var_names <- c("UTC500", "UTC500_2", "UTC500:PM25:covidbefore", "UTC500:PM25:covidduring")
V <- cov[var_names, var_names] 

# 模型自由度（用于置信区间，可选）
df <- df.residual(model_interaction0)


# --------------------------
# 3. 构建二维网格（UTC500 × PM2.5）
# --------------------------
# 3.1 设定两个变量的取值范围（基于原始数据，避免外推过多）
# UTC500：取原始数据的5%~95%分位数（排除极端值）

UTC_grid <- seq(0, 1, length.out = 500)  # 100个点

# PM2.5：取原始数据的0%~95%分位数（避免极端污染值）
pm25_range <- quantile(query_sample2$PM25, probs = c(0, 1), na.rm = TRUE)
PM25_grid <- seq(pm25_range[1], pm25_range[2], length.out = 500)  # 100个点

# 3.2 生成二维网格（100×100=10000个组合）
grid_data_PM25 <- expand.grid(
  UTC500 = UTC_grid,
  PM25 = PM25_grid
)


# --------------------------
# 5. 计算二维网格中每个点的边际效应（分疫情阶段）
# --------------------------
# 5.1 疫情前边际效应（covidbefore=1）
grid_data_PM25$marginal_pre <- alpha1 + 2 * alpha2 * grid_data_PM25$UTC500 + alpha3 * grid_data_PM25$PM25

# 5.2 疫情期间边际效应（covidduring=1）
grid_data_PM25$marginal_during <- alpha1 + 2 * alpha2 * grid_data_PM25$UTC500 + alpha4 * grid_data_PM25$PM25


# --------------------------
# 6. （可选）计算边际效应的标准误（误差传播）
# --------------------------
# 构建权重矩阵（每行对应一个网格点的权重）
grid_data_PM25 <- grid_data_PM25 %>%
  mutate(
    # 疫情前权重：[beta1, beta2, alpha3, alpha4]的权重
    w1_pre = 1,
    w2_pre = 2 * UTC500,
    w3_pre = PM25,
    w4_pre = 0,
    # 疫情期间权重
    w1_during = 1,
    w2_during = 2 * UTC500,
    w3_during = 0,
    w4_during = PM25
  )

# 计算疫情前标准误
se_pre <- apply(grid_data_PM25[, c("w1_pre", "w2_pre", "w3_pre", "w4_pre")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_PM25$se_pre <- se_pre

# 计算疫情期间标准误
se_during <- apply(grid_data_PM25[, c("w1_during", "w2_during", "w3_during", "w4_during")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_PM25$se_during <- se_during


# --------------------------
# 5. 计算95%置信区间
# --------------------------
t_low <- qt(0.025, df = df)
t_high <- qt(0.975, df = df)


grid_data_PM25$low_pre<-grid_data_PM25$marginal_pre + grid_data_PM25$se_pre*t_low
grid_data_PM25$up_pre<-grid_data_PM25$marginal_pre + grid_data_PM25$se_pre*t_high

grid_data_PM25$low_during<-grid_data_PM25$marginal_during + grid_data_PM25$se_during*t_low
grid_data_PM25$up_during<-grid_data_PM25$marginal_during + grid_data_PM25$se_during*t_high


############################################################

# 假设要获取"col1"、"col3"、"col5"这三列

grid_data_PM25$PM25<- grid_data_PM25$PM25+mean_PM25  ## 对去中心化的值进行还原
UTC_PM25_grid_before_during<- grid_data_PM25[, c("UTC500","PM25", "marginal_pre", "low_pre", "up_pre","marginal_during", "low_during", "up_during")]

save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_PM25_grid_before_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_PM25_grid_before_during, save_file_path_before)

################# 获取等于mean_UTC500的数值位置 85##################

target_UTC500 <- grid_data_PM25$UTC500[85]### 选择固定阈值处的数据
# 筛选出UTC等于该值的所有行
sub_UTC_PM25_grid_before_during <- subset(UTC_PM25_grid_before_during, UTC500 == target_UTC500)

## before
UTC_PM25_grid_before<- sub_UTC_PM25_grid_before_during[, c("UTC500","PM25", "marginal_pre", "low_pre","up_pre")]
colnames(UTC_PM25_grid_before) <- c("UTC500","PM25", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_PM25_meanUTC_before', '.csv', sep="")  # 字符串连接
write.csv(UTC_PM25_grid_before, save_file_path_before)

## during 
UTC_PM25_grid_during<- sub_UTC_PM25_grid_before_during[, c("UTC500","PM25", "marginal_during", "low_during","up_during")]
colnames(UTC_PM25_grid_during) <- c("UTC500","PM25", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_PM25_meanUTC_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_PM25_grid_during, save_file_path_before)


###################





# # 7. 可视化：二维热力图（分疫情阶段）
# # --------------------------
# # 7.1 数据整理：将宽格式转为长格式（便于分面）
# plot_data <- UTC_PM25_grid_before_during %>%
#   select(UTC500, PM25, marginal_pre, marginal_during) %>%
#   pivot_longer(
#     cols = starts_with("marginal"),
#     names_to = "period",
#     values_to = "marginal_effect",
#     names_prefix = "marginal_"
#   ) %>%
#   mutate(
#     period = case_when(
#       period == "pre" ~ "Before Covid-19",
#       period == "during" ~ "During Covid-19"
#     )
#   )
# 
# 
# # 7.2 绘制热力图（填充色=边际效应大小，等高线=效应等值线）
# ggplot(plot_data, aes(x = UTC500, y = PM25)) +
#   geom_tile(aes(fill = marginal_effect), alpha = 1.0) +
#   geom_contour(aes(z = marginal_effect), color = "white", size = 1.0, breaks = seq(-10, 10, 5)) +
#   metR::geom_text_contour(aes(z = marginal_effect), color = "white", size = 3, breaks = seq(-10, 10, 5)) +
#   facet_wrap(~period, ncol =2) +
#   # 移除midpoint参数，使用默认viridis色阶
#   scale_fill_viridis_c(
#     option = "inferno",  # 可选："viridis", "magma", "inferno", "plasma",  "rocket", "mako", "turbo"
#     name = "Marginal Effects",
#     limits = c(min(plot_data$marginal_effect), max(plot_data$marginal_effect))
#   ) +
#   labs(
#     x = "UTC500",
#     y = "PM2.5（μg/m³）",
#     title = "The marginal effects of trees varying based on levels of PM2.5 (buffer of 500 m)"
#   ) +
#   theme_bw() +
#   theme(
#     plot.title = element_text(hjust = 0.5),
#     legend.position = "bottom",
#     legend.key.width = unit(3, "cm")
#   )
# 


####################################UTC二次项与疫情和 Precipitation 的交互##############################

model_interaction1 <- felm(sentiment ~ 1 + UTC500+UTC500_2+ UTC500:Precipitation:covid  + NOUTC500+PM25+
                             humidity+ Wind + cloudcover + Surface_pressure+Skintemperature +
                             population500 + nightlight500+ settlement500+GDP
                           | yearmonthday + userid, 
                           data = query_sample2)

print(summary(model_interaction1))
#####################保存模型参数#######################
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model_interaction1))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

##################################模型协方差计算#################################
# 提取模型系数
coeffs <-coef(model_interaction1) # 获取系数

##calculate and save marginal effects, with errors clustered regionally
cov <- vcovCR(model_interaction1,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解


####################before COVID19  生成效应面#######################

####################before COVID19 边界效应 #######################

# --------------------------
# 2. 提取模型参数（同前）
# --------------------------
alpha1 <- coeffs["UTC500"]                  # -8.0401
alpha2 <- coeffs["UTC500_2"]                # 9.5740
alpha3 <- coeffs["UTC500:Precipitation:covidbefore"] # 0.0012
alpha4 <- coeffs["UTC500:Precipitation:covidduring"] # 0.0106

# 协方差矩阵（用于标准误，可选，若仅可视化效应值可省略）
var_names <- c("UTC500", "UTC500_2", "UTC500:Precipitation:covidbefore", "UTC500:Precipitation:covidduring")
V <- cov[var_names, var_names] 

# 模型自由度（用于置信区间，可选）
df <- df.residual(model_interaction1)


# --------------------------
# 3. 构建二维网格（UTC500 × PM2.5）
# --------------------------
# 3.1 设定两个变量的取值范围（基于原始数据，避免外推过多）
# UTC500：取原始数据的5%~95%分位数（排除极端值）

UTC_grid <- seq(0, 1, length.out = 500)  # 100个点

# PM2.5：取原始数据的0%~95%分位数（避免极端污染值）
Precipitation_range <- quantile(query_sample2$Precipitation, probs = c(0, 1), na.rm = TRUE)
Precipitation_grid <- seq(Precipitation_range[1], Precipitation_range[2], length.out = 500)  # 100个点

# 3.2 生成二维网格（100×100=10000个组合）
grid_data_Precipitation <- expand.grid(
  UTC500 = UTC_grid,
  Precipitation = Precipitation_grid
)


# --------------------------
# 5. 计算二维网格中每个点的边际效应（分疫情阶段）
# --------------------------
# 5.1 疫情前边际效应（covidbefore=1）
grid_data_Precipitation$marginal_pre <- alpha1 + 2 * alpha2 * grid_data_Precipitation$UTC500 + alpha3 * grid_data_Precipitation$Precipitation

# 5.2 疫情期间边际效应（covidduring=1）
grid_data_Precipitation$marginal_during <- alpha1 + 2 * alpha2 * grid_data_Precipitation$UTC500 + alpha4 * grid_data_Precipitation$Precipitation


# --------------------------
# 6. （可选）计算边际效应的标准误（误差传播）
# --------------------------
# 构建权重矩阵（每行对应一个网格点的权重）
grid_data_Precipitation <- grid_data_Precipitation %>%
  mutate(
    # 疫情前权重：[beta1, beta2, alpha3, alpha4]的权重
    w1_pre = 1,
    w2_pre = 2 * UTC500,
    w3_pre = Precipitation,
    w4_pre = 0,
    # 疫情期间权重
    w1_during = 1,
    w2_during = 2 * UTC500,
    w3_during = 0,
    w4_during = Precipitation
  )

# 计算疫情前标准误
se_pre <- apply(grid_data_Precipitation[, c("w1_pre", "w2_pre", "w3_pre", "w4_pre")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_Precipitation$se_pre <- se_pre

# 计算疫情期间标准误
se_during <- apply(grid_data_Precipitation[, c("w1_during", "w2_during", "w3_during", "w4_during")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_Precipitation$se_during <- se_during


# --------------------------
# 5. 计算95%置信区间
# --------------------------
t_low <- qt(0.025, df = df)
t_high <- qt(0.975, df = df)


grid_data_Precipitation$low_pre<-grid_data_Precipitation$marginal_pre + grid_data_Precipitation$se_pre*t_low
grid_data_Precipitation$up_pre<-grid_data_Precipitation$marginal_pre + grid_data_Precipitation$se_pre*t_high

grid_data_Precipitation$low_during<-grid_data_Precipitation$marginal_during + grid_data_Precipitation$se_during*t_low
grid_data_Precipitation$up_during<-grid_data_Precipitation$marginal_during + grid_data_Precipitation$se_during*t_high


############################################################

# 假设要获取"col1"、"col3"、"col5"这三列
grid_data_Precipitation$Precipitation<- grid_data_Precipitation$Precipitation+mean_Precipitation  ## 对去中心化的值进行还原
UTC_Precipitation_grid_before_during<- grid_data_Precipitation[, c("UTC500","Precipitation", "marginal_pre", "low_pre", "up_pre","marginal_during", "low_during", "up_during")]
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_Precipitation_grid_before_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_Precipitation_grid_before_during, save_file_path_before)

################# 获取等于mean_UTC500的数值位置 85##################

target_UTC500 <- grid_data_Precipitation$UTC500[85]### 选择固定阈值处的数据
# 筛选出UTC等于该值的所有行
sub_UTC_Precipitation_grid_before_during <- subset(UTC_Precipitation_grid_before_during, UTC500 == target_UTC500)

## before
UTC_Precipitation_grid_before<- sub_UTC_Precipitation_grid_before_during[, c("UTC500","Precipitation", "marginal_pre", "low_pre","up_pre")]
colnames(UTC_Precipitation_grid_before) <- c("UTC500","Precipitation", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_Precipitation_meanUTC_before', '.csv', sep="")  # 字符串连接
write.csv(UTC_Precipitation_grid_before, save_file_path_before)

## during 
UTC_Precipitation_grid_during<- sub_UTC_Precipitation_grid_before_during[, c("UTC500","Precipitation", "marginal_during", "low_during","up_during")]
colnames(UTC_Precipitation_grid_during) <- c("UTC500","Precipitation", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_Precipitation_meanUTC_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_Precipitation_grid_during, save_file_path_before)


###################



####################################UTC与疫情和 humidity 的交互##############################

model_interaction2 <- felm(sentiment ~ 1 + UTC500+UTC500_2+ UTC500:humidity:covid +NOUTC500+PM25+
                             Precipitation +Wind   + cloudcover + Surface_pressure+Skintemperature +
                             population500 + nightlight500+ settlement500+GDP
                           | yearmonthday + userid , 
                           data = query_sample2)

print(summary(model_interaction2))

#####################保存模型参数#######################
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model_interaction2))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)


##################################模型协方差计算#################################


# 提取模型系数
coeffs <-coef(model_interaction2) # 获取系数

##calculate and save marginal effects, with errors clustered regionally
cov <- vcovCR(model_interaction2,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解


####################before COVID19 #######################
####################before COVID19 边界效应 #######################

# --------------------------
# 2. 提取模型参数（同前）
# --------------------------
alpha1 <- coeffs["UTC500"]                  # -8.0401
alpha2 <- coeffs["UTC500_2"]                # 9.5740
alpha3 <- coeffs["UTC500:humidity:covidbefore"] # 0.0012
alpha4 <- coeffs["UTC500:humidity:covidduring"] # 0.0106

# 协方差矩阵（用于标准误，可选，若仅可视化效应值可省略）
var_names <- c("UTC500", "UTC500_2", "UTC500:humidity:covidbefore", "UTC500:humidity:covidduring")
V <- cov[var_names, var_names] 

# 模型自由度（用于置信区间，可选）
df <- df.residual(model_interaction2)


# --------------------------
# 3. 构建二维网格（UTC500 × PM2.5）
# --------------------------
# 3.1 设定两个变量的取值范围（基于原始数据，避免外推过多）
# UTC500：取原始数据的5%~95%分位数（排除极端值）

UTC_grid <- seq(0, 1, length.out = 500)  # 100个点

# PM2.5：取原始数据的0%~95%分位数（避免极端污染值）
humidity_range <- quantile(query_sample2$humidity, probs = c(0, 1), na.rm = TRUE)
humidity_grid <- seq(humidity_range[1], humidity_range[2], length.out = 500)  # 100个点

# 3.2 生成二维网格（100×100=10000个组合）
grid_data_humidity <- expand.grid(
  UTC500 = UTC_grid,
  humidity = humidity_grid
)


# --------------------------
# 5. 计算二维网格中每个点的边际效应（分疫情阶段）
# --------------------------
# 5.1 疫情前边际效应（covidbefore=1）
grid_data_humidity$marginal_pre <- alpha1 + 2 * alpha2 * grid_data_humidity$UTC500 + alpha3 * grid_data_humidity$humidity

# 5.2 疫情期间边际效应（covidduring=1）
grid_data_humidity$marginal_during <- alpha1 + 2 * alpha2 * grid_data_humidity$UTC500 + alpha4 * grid_data_humidity$humidity


# --------------------------
# 6. （可选）计算边际效应的标准误（误差传播）
# --------------------------
# 构建权重矩阵（每行对应一个网格点的权重）
grid_data_humidity <- grid_data_humidity %>%
  mutate(
    # 疫情前权重：[beta1, beta2, alpha3, alpha4]的权重
    w1_pre = 1,
    w2_pre = 2 * UTC500,
    w3_pre = humidity,
    w4_pre = 0,
    # 疫情期间权重
    w1_during = 1,
    w2_during = 2 * UTC500,
    w3_during = 0,
    w4_during = humidity
  )

# 计算疫情前标准误
se_pre <- apply(grid_data_humidity[, c("w1_pre", "w2_pre", "w3_pre", "w4_pre")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_humidity$se_pre <- se_pre

# 计算疫情期间标准误
se_during <- apply(grid_data_humidity[, c("w1_during", "w2_during", "w3_during", "w4_during")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_humidity$se_during <- se_during


# --------------------------
# 5. 计算95%置信区间
# --------------------------
t_low <- qt(0.025, df = df)
t_high <- qt(0.975, df = df)


grid_data_humidity$low_pre<-grid_data_humidity$marginal_pre + grid_data_humidity$se_pre*t_low
grid_data_humidity$up_pre<-grid_data_humidity$marginal_pre + grid_data_humidity$se_pre*t_high

grid_data_humidity$low_during<-grid_data_humidity$marginal_during + grid_data_humidity$se_during*t_low
grid_data_humidity$up_during<-grid_data_humidity$marginal_during + grid_data_humidity$se_during*t_high


############################################################

# 假设要获取"col1"、"col3"、"col5"这三列
grid_data_humidity$humidity<- grid_data_humidity$humidity+mean_humidity  ## 对去中心化的值进行还原
UTC_humidity_grid_before_during<- grid_data_humidity[, c("UTC500","humidity", "marginal_pre", "low_pre", "up_pre","marginal_during", "low_during", "up_during")]
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_humidity_grid_before_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_humidity_grid_before_during, save_file_path_before)

################# 获取等于mean_UTC500的数值位置 85##################

target_UTC500 <- grid_data_humidity$UTC500[85]### 选择固定阈值处的数据
# 筛选出UTC等于该值的所有行
sub_UTC_humidity_grid_before_during <- subset(UTC_humidity_grid_before_during, UTC500 == target_UTC500)

## before
UTC_humidity_grid_before<- sub_UTC_humidity_grid_before_during[, c("UTC500","humidity", "marginal_pre", "low_pre","up_pre")]
colnames(UTC_humidity_grid_before) <- c("UTC500","humidity", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_humidity_meanUTC_before', '.csv', sep="")  # 字符串连接
write.csv(UTC_humidity_grid_before, save_file_path_before)

## during 
UTC_humidity_grid_during<- sub_UTC_humidity_grid_before_during[, c("UTC500","humidity", "marginal_during", "low_during","up_during")]
colnames(UTC_humidity_grid_during) <- c("UTC500","humidity", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_humidity_meanUTC_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_humidity_grid_during, save_file_path_before)



####################################UTC与疫情和 Skintemperature 的交互##############################

model_interaction3 <- felm(sentiment ~ 1 + UTC500+UTC500_2+ UTC500:Skintemperature:covid + NOUTC500+PM25+
                             humidity +Precipitation + Wind + cloudcover + Surface_pressure +
                             population500 + nightlight500+ settlement500+GDP
                           | yearmonthday + userid , 
                           data = query_sample2)

print(summary(model_interaction3))

#####################保存模型参数#######################
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model_interaction3))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)


##################################模型协方差计算#################################


# 提取模型系数
coeffs <-coef(model_interaction3) # 获取系数

##calculate and save marginal effects, with errors clustered regionally
cov <- vcovCR(model_interaction3,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解


####################before COVID19 #######################
####################before COVID19 边界效应 #######################

# --------------------------
# 2. 提取模型参数（同前）
# --------------------------
alpha1 <- coeffs["UTC500"]                  # -8.0401
alpha2 <- coeffs["UTC500_2"]                # 9.5740
alpha3 <- coeffs["UTC500:Skintemperature:covidbefore"] # 0.0012
alpha4 <- coeffs["UTC500:Skintemperature:covidduring"] # 0.0106

# 协方差矩阵（用于标准误，可选，若仅可视化效应值可省略）
var_names <- c("UTC500", "UTC500_2", "UTC500:Skintemperature:covidbefore", "UTC500:Skintemperature:covidduring")
V <- cov[var_names, var_names] 

# 模型自由度（用于置信区间，可选）
df <- df.residual(model_interaction3)


# --------------------------
# 3. 构建二维网格（UTC500 × PM2.5）
# --------------------------
# 3.1 设定两个变量的取值范围（基于原始数据，避免外推过多）
# UTC500：取原始数据的5%~95%分位数（排除极端值）

UTC_grid <- seq(0, 1, length.out = 500)  # 100个点

# PM2.5：取原始数据的0%~95%分位数（避免极端污染值）
Skintemperature_range <- quantile(query_sample2$Skintemperature, probs = c(0, 1), na.rm = TRUE)
Skintemperature_grid <- seq(Skintemperature_range[1], Skintemperature_range[2], length.out = 500)  # 100个点

# 3.2 生成二维网格（100×100=10000个组合）
grid_data_Skintemperature <- expand.grid(
  UTC500 = UTC_grid,
  Skintemperature = Skintemperature_grid
)


# --------------------------
# 5. 计算二维网格中每个点的边际效应（分疫情阶段）
# --------------------------
# 5.1 疫情前边际效应（covidbefore=1）
grid_data_Skintemperature$marginal_pre <- alpha1 + 2 * alpha2 * grid_data_Skintemperature$UTC500 + alpha3 * grid_data_Skintemperature$Skintemperature

# 5.2 疫情期间边际效应（covidduring=1）
grid_data_Skintemperature$marginal_during <- alpha1 + 2 * alpha2 * grid_data_Skintemperature$UTC500 + alpha4 * grid_data_Skintemperature$Skintemperature


# --------------------------
# 6. （可选）计算边际效应的标准误（误差传播）
# --------------------------
# 构建权重矩阵（每行对应一个网格点的权重）
grid_data_Skintemperature <- grid_data_Skintemperature %>%
  mutate(
    # 疫情前权重：[beta1, beta2, alpha3, alpha4]的权重
    w1_pre = 1,
    w2_pre = 2 * UTC500,
    w3_pre = Skintemperature,
    w4_pre = 0,
    # 疫情期间权重
    w1_during = 1,
    w2_during = 2 * UTC500,
    w3_during = 0,
    w4_during = Skintemperature
  )

# 计算疫情前标准误
se_pre <- apply(grid_data_Skintemperature[, c("w1_pre", "w2_pre", "w3_pre", "w4_pre")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_Skintemperature$se_pre <- se_pre

# 计算疫情期间标准误
se_during <- apply(grid_data_Skintemperature[, c("w1_during", "w2_during", "w3_during", "w4_during")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_Skintemperature$se_during <- se_during


# --------------------------
# 5. 计算95%置信区间
# --------------------------
t_low <- qt(0.025, df = df)
t_high <- qt(0.975, df = df)


grid_data_Skintemperature$low_pre<-grid_data_Skintemperature$marginal_pre + grid_data_Skintemperature$se_pre*t_low
grid_data_Skintemperature$up_pre<-grid_data_Skintemperature$marginal_pre + grid_data_Skintemperature$se_pre*t_high

grid_data_Skintemperature$low_during<-grid_data_Skintemperature$marginal_during + grid_data_Skintemperature$se_during*t_low
grid_data_Skintemperature$up_during<-grid_data_Skintemperature$marginal_during + grid_data_Skintemperature$se_during*t_high


############################################################

# 假设要获取"col1"、"col3"、"col5"这三列
grid_data_Skintemperature$Skintemperature<- grid_data_Skintemperature$Skintemperature+mean_Skintemperature  ## 对去中心化的值进行还原
UTC_Skintemperature_grid_before_during<- grid_data_Skintemperature[, c("UTC500","Skintemperature", "marginal_pre", "low_pre", "up_pre","marginal_during", "low_during", "up_during")]
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_Skintemperature_grid_before_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_Skintemperature_grid_before_during, save_file_path_before)

################# 获取等于mean_UTC500的数值位置 85##################

target_UTC500 <- grid_data_Skintemperature$UTC500[85]### 选择固定阈值处的数据
# 筛选出UTC等于该值的所有行
sub_UTC_Skintemperature_grid_before_during <- subset(UTC_Skintemperature_grid_before_during, UTC500 == target_UTC500)

## before
UTC_Skintemperature_grid_before<- sub_UTC_Skintemperature_grid_before_during[, c("UTC500","Skintemperature", "marginal_pre", "low_pre","up_pre")]
colnames(UTC_Skintemperature_grid_before) <- c("UTC500","Skintemperature", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_Skintemperature_meanUTC_before', '.csv', sep="")  # 字符串连接
write.csv(UTC_Skintemperature_grid_before, save_file_path_before)

## during 
UTC_Skintemperature_grid_during<- sub_UTC_Skintemperature_grid_before_during[, c("UTC500","Skintemperature", "marginal_during", "low_during","up_during")]
colnames(UTC_Skintemperature_grid_during) <- c("UTC500","Skintemperature", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_Skintemperature_meanUTC_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_Skintemperature_grid_during, save_file_path_before)


###################




####################################UTC与疫情和 Surface_pressure 的交互##############################

model_interaction4 <- felm(sentiment ~ 1 + UTC500+UTC500_2+ UTC500:Surface_pressure:covid + NOUTC500+PM25+
                             humidity + Precipitation + Wind + cloudcover +Skintemperature +Surface_pressure+
                             population500 + nightlight500+ settlement500+GDP
                           | yearmonthday + userid , 
                           data = query_sample2)

print(summary(model_interaction4))

#####################保存模型参数#######################
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model_interaction4))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)


##################################模型协方差计算#################################


# 提取模型系数
coeffs <-coef(model_interaction4) # 获取系数

##calculate and save marginal effects, with errors clustered regionally
cov <- vcovCR(model_interaction4,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解


####################before COVID19 边界效应 #######################

# --------------------------
# 2. 提取模型参数（同前）
# --------------------------
alpha1 <- coeffs["UTC500"]                  # -8.0401
alpha2 <- coeffs["UTC500_2"]                # 9.5740
alpha3 <- coeffs["UTC500:Surface_pressure:covidbefore"] # 0.0012
alpha4 <- coeffs["UTC500:Surface_pressure:covidduring"] # 0.0106

# 协方差矩阵（用于标准误，可选，若仅可视化效应值可省略）
var_names <- c("UTC500", "UTC500_2", "UTC500:Surface_pressure:covidbefore", "UTC500:Surface_pressure:covidduring")
V <- cov[var_names, var_names] 

# 模型自由度（用于置信区间，可选）
df <- df.residual(model_interaction4)


# --------------------------
# 3. 构建二维网格（UTC500 × PM2.5）
# --------------------------
# 3.1 设定两个变量的取值范围（基于原始数据，避免外推过多）
# UTC500：取原始数据的5%~95%分位数（排除极端值）

UTC_grid <- seq(0, 1, length.out = 500)  # 100个点

# PM2.5：取原始数据的0%~95%分位数（避免极端污染值）
Surface_pressure_range <- quantile(query_sample2$Surface_pressure, probs = c(0, 1), na.rm = TRUE)
Surface_pressure_grid <- seq(Surface_pressure_range[1], Surface_pressure_range[2], length.out = 500)  # 100个点

# 3.2 生成二维网格（100×100=10000个组合）
grid_data_Surface_pressure <- expand.grid(
  UTC500 = UTC_grid,
  Surface_pressure = Surface_pressure_grid
)


# --------------------------
# 5. 计算二维网格中每个点的边际效应（分疫情阶段）
# --------------------------
# 5.1 疫情前边际效应（covidbefore=1）
grid_data_Surface_pressure$marginal_pre <- alpha1 + 2 * alpha2 * grid_data_Surface_pressure$UTC500 + alpha3 * grid_data_Surface_pressure$Surface_pressure

# 5.2 疫情期间边际效应（covidduring=1）
grid_data_Surface_pressure$marginal_during <- alpha1 + 2 * alpha2 * grid_data_Surface_pressure$UTC500 + alpha4 * grid_data_Surface_pressure$Surface_pressure


# --------------------------
# 6. （可选）计算边际效应的标准误（误差传播）
# --------------------------
# 构建权重矩阵（每行对应一个网格点的权重）
grid_data_Surface_pressure <- grid_data_Surface_pressure %>%
  mutate(
    # 疫情前权重：[beta1, beta2, alpha3, alpha4]的权重
    w1_pre = 1,
    w2_pre = 2 * UTC500,
    w3_pre = Surface_pressure,
    w4_pre = 0,
    # 疫情期间权重
    w1_during = 1,
    w2_during = 2 * UTC500,
    w3_during = 0,
    w4_during = Surface_pressure
  )

# 计算疫情前标准误
se_pre <- apply(grid_data_Surface_pressure[, c("w1_pre", "w2_pre", "w3_pre", "w4_pre")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_Surface_pressure$se_pre <- se_pre

# 计算疫情期间标准误
se_during <- apply(grid_data_Surface_pressure[, c("w1_during", "w2_during", "w3_during", "w4_during")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_Surface_pressure$se_during <- se_during


# --------------------------
# 5. 计算95%置信区间
# --------------------------
t_low <- qt(0.025, df = df)
t_high <- qt(0.975, df = df)


grid_data_Surface_pressure$low_pre<-grid_data_Surface_pressure$marginal_pre + grid_data_Surface_pressure$se_pre*t_low
grid_data_Surface_pressure$up_pre<-grid_data_Surface_pressure$marginal_pre + grid_data_Surface_pressure$se_pre*t_high

grid_data_Surface_pressure$low_during<-grid_data_Surface_pressure$marginal_during + grid_data_Surface_pressure$se_during*t_low
grid_data_Surface_pressure$up_during<-grid_data_Surface_pressure$marginal_during + grid_data_Surface_pressure$se_during*t_high


############################################################

# 假设要获取"col1"、"col3"、"col5"这三列


grid_data_Surface_pressure$Surface_pressure<- grid_data_Surface_pressure$Surface_pressure+mean_Surface_pressure  ## 对去中心化的值进行还原
UTC_Surface_pressure_grid_before_during<- grid_data_Surface_pressure[, c("UTC500","Surface_pressure", "marginal_pre", "low_pre", "up_pre","marginal_during", "low_during", "up_during")]
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_Surface_pressure_grid_before_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_Surface_pressure_grid_before_during, save_file_path_before)

################# 获取等于mean_UTC500的数值位置 85##################

target_UTC500 <- grid_data_Surface_pressure$UTC500[85]### 选择固定阈值处的数据
# 筛选出UTC等于该值的所有行
sub_UTC_Surface_pressure_grid_before_during <- subset(UTC_Surface_pressure_grid_before_during, UTC500 == target_UTC500)

## before
UTC_Surface_pressure_grid_before<- sub_UTC_Surface_pressure_grid_before_during[, c("UTC500","Surface_pressure", "marginal_pre", "low_pre","up_pre")]
colnames(UTC_Surface_pressure_grid_before) <- c("UTC500","Surface_pressure", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_Surface_pressure_meanUTC_before', '.csv', sep="")  # 字符串连接
write.csv(UTC_Surface_pressure_grid_before, save_file_path_before)

## during 
UTC_Surface_pressure_grid_during<- sub_UTC_Surface_pressure_grid_before_during[, c("UTC500","Surface_pressure", "marginal_during", "low_during","up_during")]
colnames(UTC_Surface_pressure_grid_during) <- c("UTC500","Surface_pressure", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_Surface_pressure_meanUTC_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_Surface_pressure_grid_during, save_file_path_before)


###################

####################################UTC与疫情和 population500 的交互##############################

model_interaction5 <- felm(sentiment ~ 1 + UTC500+UTC500_2+ UTC500:population500:covid + NOUTC500+PM25+
                             humidity +Precipitation + Wind + cloudcover + Surface_pressure+Skintemperature +
                             population500+nightlight500+ settlement500+GDP
                           | yearmonthday + userid , 
                           data = query_sample2)

print(summary(model_interaction5))

#####################保存模型参数#######################
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model_interaction5))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)


##################################模型协方差计算#################################


# 提取模型系数
coeffs <-coef(model_interaction5) # 获取系数

##calculate and save marginal effects, with errors clustered regionally
cov <- vcovCR(model_interaction5,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解


####################before COVID19 #######################
####################before COVID19 边界效应 #######################

# --------------------------
# 2. 提取模型参数（同前）
# --------------------------
alpha1 <- coeffs["UTC500"]                  # -8.0401
alpha2 <- coeffs["UTC500_2"]                # 9.5740
alpha3 <- coeffs["UTC500:population500:covidbefore"] # 0.0012
alpha4 <- coeffs["UTC500:population500:covidduring"] # 0.0106

# 协方差矩阵（用于标准误，可选，若仅可视化效应值可省略）
var_names <- c("UTC500", "UTC500_2", "UTC500:population500:covidbefore", "UTC500:population500:covidduring")
V <- cov[var_names, var_names] 

# 模型自由度（用于置信区间，可选）
df <- df.residual(model_interaction5)


# --------------------------
# 3. 构建二维网格（UTC500 × PM2.5）
# --------------------------
# 3.1 设定两个变量的取值范围（基于原始数据，避免外推过多）
# UTC500：取原始数据的5%~95%分位数（排除极端值）

UTC_grid <- seq(0, 1, length.out = 500)  # 100个点

# PM2.5：取原始数据的0%~95%分位数（避免极端污染值）
population500_range <- quantile(query_sample2$population500, probs = c(0, 1), na.rm = TRUE)
population500_grid <- seq(population500_range[1], population500_range[2], length.out = 500)  # 100个点

# 3.2 生成二维网格（100×100=10000个组合）
grid_data_population500 <- expand.grid(
  UTC500 = UTC_grid,
  population500 = population500_grid
)


# --------------------------
# 5. 计算二维网格中每个点的边际效应（分疫情阶段）
# --------------------------
# 5.1 疫情前边际效应（covidbefore=1）
grid_data_population500$marginal_pre <- alpha1 + 2 * alpha2 * grid_data_population500$UTC500 + alpha3 * grid_data_population500$population500

# 5.2 疫情期间边际效应（covidduring=1）
grid_data_population500$marginal_during <- alpha1 + 2 * alpha2 * grid_data_population500$UTC500 + alpha4 * grid_data_population500$population500


# --------------------------
# 6. （可选）计算边际效应的标准误（误差传播）
# --------------------------
# 构建权重矩阵（每行对应一个网格点的权重）
grid_data_population500 <- grid_data_population500 %>%
  mutate(
    # 疫情前权重：[beta1, beta2, alpha3, alpha4]的权重
    w1_pre = 1,
    w2_pre = 2 * UTC500,
    w3_pre = population500,
    w4_pre = 0,
    # 疫情期间权重
    w1_during = 1,
    w2_during = 2 * UTC500,
    w3_during = 0,
    w4_during = population500
  )

# 计算疫情前标准误
se_pre <- apply(grid_data_population500[, c("w1_pre", "w2_pre", "w3_pre", "w4_pre")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_population500$se_pre <- se_pre

# 计算疫情期间标准误
se_during <- apply(grid_data_population500[, c("w1_during", "w2_during", "w3_during", "w4_during")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_population500$se_during <- se_during


# --------------------------
# 5. 计算95%置信区间
# --------------------------
t_low <- qt(0.025, df = df)
t_high <- qt(0.975, df = df)


grid_data_population500$low_pre<-grid_data_population500$marginal_pre + grid_data_population500$se_pre*t_low
grid_data_population500$up_pre<-grid_data_population500$marginal_pre + grid_data_population500$se_pre*t_high

grid_data_population500$low_during<-grid_data_population500$marginal_during + grid_data_population500$se_during*t_low
grid_data_population500$up_during<-grid_data_population500$marginal_during + grid_data_population500$se_during*t_high


############################################################

# 假设要获取"col1"、"col3"、"col5"这三列

grid_data_population500$population500<- grid_data_population500$population500+mean_population500  ## 对去中心化的值进行还原
UTC_population500_grid_before_during<- grid_data_population500[, c("UTC500","population500", "marginal_pre", "low_pre", "up_pre","marginal_during", "low_during", "up_during")]
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_population500_grid_before_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_population500_grid_before_during, save_file_path_before)

################# 获取等于mean_UTC500的数值位置 85##################

target_UTC500 <- grid_data_population500$UTC500[85]### 选择固定阈值处的数据
# 筛选出UTC等于该值的所有行
sub_UTC_population500_grid_before_during <- subset(UTC_population500_grid_before_during, UTC500 == target_UTC500)

## before
UTC_population500_grid_before<- sub_UTC_population500_grid_before_during[, c("UTC500","population500", "marginal_pre", "low_pre","up_pre")]
colnames(UTC_population500_grid_before) <- c("UTC500","population500", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_population500_meanUTC_before', '.csv', sep="")  # 字符串连接
write.csv(UTC_population500_grid_before, save_file_path_before)

## during 
UTC_population500_grid_during<- sub_UTC_population500_grid_before_during[, c("UTC500","population500", "marginal_during", "low_during","up_during")]
colnames(UTC_population500_grid_during) <- c("UTC500","population500", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_population500_meanUTC_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_population500_grid_during, save_file_path_before)


###################


####################################UTC与疫情和 nightlight500 的交互##############################

model_interaction6 <- felm(sentiment ~ 1 + UTC500+UTC500_2+ UTC500:nightlight500:covid + NOUTC500+PM25+
                             humidity +Precipitation + Wind + cloudcover + Surface_pressure+Skintemperature +
                             nightlight500+ population500 + settlement500+GDP
                           | yearmonthday + userid , 
                           data = query_sample2)

print(summary(model_interaction6))

#####################保存模型参数#######################
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model_interaction6))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)


##################################模型协方差计算#################################


# 提取模型系数
coeffs <-coef(model_interaction6) # 获取系数

##calculate and save marginal effects, with errors clustered regionally
cov <- vcovCR(model_interaction6,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解


####################before COVID19 #######################
####################before COVID19 边界效应 #######################

# --------------------------
# 2. 提取模型参数（同前）
# --------------------------
alpha1 <- coeffs["UTC500"]                  # -8.0401
alpha2 <- coeffs["UTC500_2"]                # 9.5740
alpha3 <- coeffs["UTC500:nightlight500:covidbefore"] # 0.0012
alpha4 <- coeffs["UTC500:nightlight500:covidduring"] # 0.0106

# 协方差矩阵（用于标准误，可选，若仅可视化效应值可省略）
var_names <- c("UTC500", "UTC500_2", "UTC500:nightlight500:covidbefore", "UTC500:nightlight500:covidduring")
V <- cov[var_names, var_names] 

# 模型自由度（用于置信区间，可选）
df <- df.residual(model_interaction6)


# --------------------------
# 3. 构建二维网格（UTC500 × PM2.5）
# --------------------------
# 3.1 设定两个变量的取值范围（基于原始数据，避免外推过多）
# UTC500：取原始数据的5%~95%分位数（排除极端值）

UTC_grid <- seq(0, 1, length.out = 500)  # 100个点

# PM2.5：取原始数据的0%~95%分位数（避免极端污染值）
nightlight500_range <- quantile(query_sample2$nightlight500, probs = c(0, 1), na.rm = TRUE)
nightlight500_grid <- seq(nightlight500_range[1], nightlight500_range[2], length.out = 500)  # 100个点

# 3.2 生成二维网格（100×100=10000个组合）
grid_data_nightlight500 <- expand.grid(
  UTC500 = UTC_grid,
  nightlight500 = nightlight500_grid
)


# --------------------------
# 5. 计算二维网格中每个点的边际效应（分疫情阶段）
# --------------------------
# 5.1 疫情前边际效应（covidbefore=1）
grid_data_nightlight500$marginal_pre <- alpha1 + 2 * alpha2 * grid_data_nightlight500$UTC500 + alpha3 * grid_data_nightlight500$nightlight500

# 5.2 疫情期间边际效应（covidduring=1）
grid_data_nightlight500$marginal_during <- alpha1 + 2 * alpha2 * grid_data_nightlight500$UTC500 + alpha4 * grid_data_nightlight500$nightlight500


# --------------------------
# 6. （可选）计算边际效应的标准误（误差传播）
# --------------------------
# 构建权重矩阵（每行对应一个网格点的权重）
grid_data_nightlight500 <- grid_data_nightlight500 %>%
  mutate(
    # 疫情前权重：[beta1, beta2, alpha3, alpha4]的权重
    w1_pre = 1,
    w2_pre = 2 * UTC500,
    w3_pre = nightlight500,
    w4_pre = 0,
    # 疫情期间权重
    w1_during = 1,
    w2_during = 2 * UTC500,
    w3_during = 0,
    w4_during = nightlight500
  )

# 计算疫情前标准误
se_pre <- apply(grid_data_nightlight500[, c("w1_pre", "w2_pre", "w3_pre", "w4_pre")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_nightlight500$se_pre <- se_pre

# 计算疫情期间标准误
se_during <- apply(grid_data_nightlight500[, c("w1_during", "w2_during", "w3_during", "w4_during")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_nightlight500$se_during <- se_during


# --------------------------
# 5. 计算95%置信区间
# --------------------------
t_low <- qt(0.025, df = df)
t_high <- qt(0.975, df = df)


grid_data_nightlight500$low_pre<-grid_data_nightlight500$marginal_pre + grid_data_nightlight500$se_pre*t_low
grid_data_nightlight500$up_pre<-grid_data_nightlight500$marginal_pre + grid_data_nightlight500$se_pre*t_high

grid_data_nightlight500$low_during<-grid_data_nightlight500$marginal_during + grid_data_nightlight500$se_during*t_low
grid_data_nightlight500$up_during<-grid_data_nightlight500$marginal_during + grid_data_nightlight500$se_during*t_high


############################################################

# 假设要获取"col1"、"col3"、"col5"这三列

grid_data_nightlight500$nightlight500<- grid_data_nightlight500$nightlight500+mean_nightlight500  ## 对去中心化的值进行还原
UTC_nightlight500_grid_before_during<- grid_data_nightlight500[, c("UTC500","nightlight500", "marginal_pre", "low_pre", "up_pre","marginal_during", "low_during", "up_during")]
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_nightlight500_grid_before_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_nightlight500_grid_before_during, save_file_path_before)

################# 获取等于mean_UTC500的数值位置 85##################

target_UTC500 <- grid_data_nightlight500$UTC500[85]### 选择固定阈值处的数据
# 筛选出UTC等于该值的所有行
sub_UTC_nightlight500_grid_before_during <- subset(UTC_nightlight500_grid_before_during, UTC500 == target_UTC500)

## before
UTC_nightlight500_grid_before<- sub_UTC_nightlight500_grid_before_during[, c("UTC500","nightlight500", "marginal_pre", "low_pre","up_pre")]
colnames(UTC_nightlight500_grid_before) <- c("UTC500","nightlight500", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_nightlight500_meanUTC_before', '.csv', sep="")  # 字符串连接
write.csv(UTC_nightlight500_grid_before, save_file_path_before)

## during 
UTC_nightlight500_grid_during<- sub_UTC_nightlight500_grid_before_during[, c("UTC500","nightlight500", "marginal_during", "low_during","up_during")]
colnames(UTC_nightlight500_grid_during) <- c("UTC500","nightlight500", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_nightlight500_meanUTC_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_nightlight500_grid_during, save_file_path_before)


###################



####################################UTC与疫情和 settlement500 的交互##############################

model_interaction7 <- felm(sentiment ~ 1 + UTC500+UTC500_2+ UTC500:settlement500:covid + NOUTC500+PM25+
                             humidity +Precipitation + Wind + cloudcover + Surface_pressure+Skintemperature +settlement500+
                             population500 + nightlight500+GDP
                           | yearmonthday + userid , 
                           data = query_sample2)

print(summary(model_interaction7))

#####################保存模型参数#######################
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model_interaction7))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)


##################################模型协方差计算#################################


# 提取模型系数
coeffs <-coef(model_interaction7) # 获取系数

##calculate and save marginal effects, with errors clustered regionally
cov <- vcovCR(model_interaction7,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解


####################before COVID19 #######################
####################before COVID19 边界效应 #######################

# --------------------------
# 2. 提取模型参数（同前）
# --------------------------
alpha1 <- coeffs["UTC500"]                  # -8.0401
alpha2 <- coeffs["UTC500_2"]                # 9.5740
alpha3 <- coeffs["UTC500:settlement500:covidbefore"] # 0.0012
alpha4 <- coeffs["UTC500:settlement500:covidduring"] # 0.0106

# 协方差矩阵（用于标准误，可选，若仅可视化效应值可省略）
var_names <- c("UTC500", "UTC500_2", "UTC500:settlement500:covidbefore", "UTC500:settlement500:covidduring")
V <- cov[var_names, var_names] 

# 模型自由度（用于置信区间，可选）
df <- df.residual(model_interaction7)


# --------------------------
# 3. 构建二维网格（UTC500 × PM2.5）
# --------------------------
# 3.1 设定两个变量的取值范围（基于原始数据，避免外推过多）
# UTC500：取原始数据的5%~95%分位数（排除极端值）

UTC_grid <- seq(0, 1, length.out = 500)  # 100个点

# PM2.5：取原始数据的0%~95%分位数（避免极端污染值）
settlement500_range <- quantile(query_sample2$settlement500, probs = c(0, 1), na.rm = TRUE)
settlement500_grid <- seq(settlement500_range[1], settlement500_range[2], length.out = 500)  # 100个点

# 3.2 生成二维网格（100×100=10000个组合）
grid_data_settlement500 <- expand.grid(
  UTC500 = UTC_grid,
  settlement500 = settlement500_grid
)


# --------------------------
# 5. 计算二维网格中每个点的边际效应（分疫情阶段）
# --------------------------
# 5.1 疫情前边际效应（covidbefore=1）
grid_data_settlement500$marginal_pre <- alpha1 + 2 * alpha2 * grid_data_settlement500$UTC500 + alpha3 * grid_data_settlement500$settlement500

# 5.2 疫情期间边际效应（covidduring=1）
grid_data_settlement500$marginal_during <- alpha1 + 2 * alpha2 * grid_data_settlement500$UTC500 + alpha4 * grid_data_settlement500$settlement500


# --------------------------
# 6. （可选）计算边际效应的标准误（误差传播）
# --------------------------
# 构建权重矩阵（每行对应一个网格点的权重）
grid_data_settlement500 <- grid_data_settlement500 %>%
  mutate(
    # 疫情前权重：[beta1, beta2, alpha3, alpha4]的权重
    w1_pre = 1,
    w2_pre = 2 * UTC500,
    w3_pre = settlement500,
    w4_pre = 0,
    # 疫情期间权重
    w1_during = 1,
    w2_during = 2 * UTC500,
    w3_during = 0,
    w4_during = settlement500
  )

# 计算疫情前标准误
se_pre <- apply(grid_data_settlement500[, c("w1_pre", "w2_pre", "w3_pre", "w4_pre")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_settlement500$se_pre <- se_pre

# 计算疫情期间标准误
se_during <- apply(grid_data_settlement500[, c("w1_during", "w2_during", "w3_during", "w4_during")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_settlement500$se_during <- se_during


# --------------------------
# 5. 计算95%置信区间
# --------------------------
t_low <- qt(0.025, df = df)
t_high <- qt(0.975, df = df)


grid_data_settlement500$low_pre<-grid_data_settlement500$marginal_pre + grid_data_settlement500$se_pre*t_low
grid_data_settlement500$up_pre<-grid_data_settlement500$marginal_pre + grid_data_settlement500$se_pre*t_high

grid_data_settlement500$low_during<-grid_data_settlement500$marginal_during + grid_data_settlement500$se_during*t_low
grid_data_settlement500$up_during<-grid_data_settlement500$marginal_during + grid_data_settlement500$se_during*t_high


############################################################

# 假设要获取"col1"、"col3"、"col5"这三列

grid_data_settlement500$settlement500<- grid_data_settlement500$settlement500+mean_settlement500  ## 对去中心化的值进行还原
UTC_settlement500_grid_before_during<- grid_data_settlement500[, c("UTC500","settlement500", "marginal_pre", "low_pre", "up_pre","marginal_during", "low_during", "up_during")]
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_settlement500_grid_before_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_settlement500_grid_before_during, save_file_path_before)

################# 获取等于mean_UTC500的数值位置 85##################

target_UTC500 <- grid_data_settlement500$UTC500[85]### 选择固定阈值处的数据
# 筛选出UTC等于该值的所有行
sub_UTC_settlement500_grid_before_during <- subset(UTC_settlement500_grid_before_during, UTC500 == target_UTC500)

## before
UTC_settlement500_grid_before<- sub_UTC_settlement500_grid_before_during[, c("UTC500","settlement500", "marginal_pre", "low_pre","up_pre")]
colnames(UTC_settlement500_grid_before) <- c("UTC500","settlement500", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_settlement500_meanUTC_before', '.csv', sep="")  # 字符串连接
write.csv(UTC_settlement500_grid_before, save_file_path_before)

## during 
UTC_settlement500_grid_during<- sub_UTC_settlement500_grid_before_during[, c("UTC500","settlement500", "marginal_during", "low_during","up_during")]
colnames(UTC_settlement500_grid_during) <- c("UTC500","settlement500", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_settlement500_meanUTC_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_settlement500_grid_during, save_file_path_before)


###################



####################################UTC与疫情和 GDP 的交互##############################

model_interaction9 <- felm(sentiment ~ 1 + UTC500+UTC500_2+ UTC500:GDP:covid + NOUTC500+PM25+
                             humidity + Precipitation + Wind + cloudcover + + Surface_pressure+Skintemperature +
                             population500 + nightlight500+ settlement500
                           | yearmonthday + userid , 
                           data = query_sample2)

print(summary(model_interaction9))

#####################保存模型参数#######################
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model_interaction9))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)


##################################模型协方差计算#################################


# 提取模型系数
coeffs <-coef(model_interaction9) # 获取系数

##calculate and save marginal effects, with errors clustered regionally
cov <- vcovCR(model_interaction9,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解


####################before COVID19 #######################
####################before COVID19 边界效应 #######################

# --------------------------
# 2. 提取模型参数（同前）
# --------------------------
alpha1 <- coeffs["UTC500"]                  # -8.0401
alpha2 <- coeffs["UTC500_2"]                # 9.5740
alpha3 <- coeffs["UTC500:GDP:covidbefore"] # 0.0012
alpha4 <- coeffs["UTC500:GDP:covidduring"] # 0.0106

# 协方差矩阵（用于标准误，可选，若仅可视化效应值可省略）
var_names <- c("UTC500", "UTC500_2", "UTC500:GDP:covidbefore", "UTC500:GDP:covidduring")
V <- cov[var_names, var_names] 

# 模型自由度（用于置信区间，可选）
df <- df.residual(model_interaction9)


# --------------------------
# 3. 构建二维网格（UTC500 × PM2.5）
# --------------------------
# 3.1 设定两个变量的取值范围（基于原始数据，避免外推过多）
# UTC500：取原始数据的5%~95%分位数（排除极端值）

UTC_grid <- seq(0, 1, length.out = 500)  # 100个点

# PM2.5：取原始数据的0%~95%分位数（避免极端污染值）
GDP_range <- quantile(query_sample2$GDP, probs = c(0, 1), na.rm = TRUE)
GDP_grid <- seq(GDP_range[1], GDP_range[2], length.out = 500)  # 100个点

# 3.2 生成二维网格（100×100=10000个组合）
grid_data_GDP <- expand.grid(
  UTC500 = UTC_grid,
  GDP = GDP_grid
)


# --------------------------
# 5. 计算二维网格中每个点的边际效应（分疫情阶段）
# --------------------------
# 5.1 疫情前边际效应（covidbefore=1）
grid_data_GDP$marginal_pre <- alpha1 + 2 * alpha2 * grid_data_GDP$UTC500 + alpha3 * grid_data_GDP$GDP

# 5.2 疫情期间边际效应（covidduring=1）
grid_data_GDP$marginal_during <- alpha1 + 2 * alpha2 * grid_data_GDP$UTC500 + alpha4 * grid_data_GDP$GDP


# --------------------------
# 6. （可选）计算边际效应的标准误（误差传播）
# --------------------------
# 构建权重矩阵（每行对应一个网格点的权重）
grid_data_GDP <- grid_data_GDP %>%
  mutate(
    # 疫情前权重：[beta1, beta2, alpha3, alpha4]的权重
    w1_pre = 1,
    w2_pre = 2 * UTC500,
    w3_pre = GDP,
    w4_pre = 0,
    # 疫情期间权重
    w1_during = 1,
    w2_during = 2 * UTC500,
    w3_during = 0,
    w4_during = GDP
  )

# 计算疫情前标准误
se_pre <- apply(grid_data_GDP[, c("w1_pre", "w2_pre", "w3_pre", "w4_pre")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_GDP$se_pre <- se_pre

# 计算疫情期间标准误
se_during <- apply(grid_data_GDP[, c("w1_during", "w2_during", "w3_during", "w4_during")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_GDP$se_during <- se_during


# --------------------------
# 5. 计算95%置信区间
# --------------------------
t_low <- qt(0.025, df = df)
t_high <- qt(0.975, df = df)


grid_data_GDP$low_pre<-grid_data_GDP$marginal_pre + grid_data_GDP$se_pre*t_low
grid_data_GDP$up_pre<-grid_data_GDP$marginal_pre + grid_data_GDP$se_pre*t_high

grid_data_GDP$low_during<-grid_data_GDP$marginal_during + grid_data_GDP$se_during*t_low
grid_data_GDP$up_during<-grid_data_GDP$marginal_during + grid_data_GDP$se_during*t_high


############################################################

# 假设要获取"col1"、"col3"、"col5"这三列
## grid_data_GDP$GDP<- grid_data_GDP$GDP + mean_GDP ## 对去中心化的值进行还原
UTC_GDP_grid_before_during<- grid_data_GDP[, c("UTC500","GDP", "marginal_pre", "low_pre", "up_pre","marginal_during", "low_during", "up_during")]
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_GDP_grid_before_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_GDP_grid_before_during, save_file_path_before)

################# 获取等于mean_UTC500的数值位置 85##################

target_UTC500 <- grid_data_GDP$UTC500[85]### 选择固定阈值处的数据
# 筛选出UTC等于该值的所有行
sub_UTC_GDP_grid_before_during <- subset(UTC_GDP_grid_before_during, UTC500 == target_UTC500)

## before
UTC_GDP_grid_before<- sub_UTC_GDP_grid_before_during[, c("UTC500","GDP", "marginal_pre", "low_pre","up_pre")]
colnames(UTC_GDP_grid_before) <- c("UTC500","GDP", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_GDP_meanUTC_before', '.csv', sep="")  # 字符串连接
write.csv(UTC_GDP_grid_before, save_file_path_before)

## during 
UTC_GDP_grid_during<- sub_UTC_GDP_grid_before_during[, c("UTC500","GDP", "marginal_during", "low_during","up_during")]
colnames(UTC_GDP_grid_during) <- c("UTC500","GDP", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_GDP_meanUTC_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_GDP_grid_during, save_file_path_before)


###################



####################################UTC与疫情和温度 NOUTC500 的交互##############################

model_interaction10 <- felm(sentiment ~ 1 + UTC500+UTC500_2+ UTC500:NOUTC500:covid +NOUTC500 +PM25+
                              humidity +Precipitation + Wind + cloudcover + + Surface_pressure+Skintemperature +
                              population500 + nightlight500+ settlement500+GDP
                            | yearmonthday + userid , 
                            data = query_sample2)

print(summary(model_interaction10))

#####################保存模型参数#######################
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model_interaction10))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)


##################################模型协方差计算#################################


# 提取模型系数
coeffs <-coef(model_interaction10) # 获取系数

##calculate and save marginal effects, with errors clustered regionally
cov <- vcovCR(model_interaction10,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解


####################before COVID19 #######################
####################before COVID19 边界效应 #######################

# --------------------------
# 2. 提取模型参数（同前）
# --------------------------
alpha1 <- coeffs["UTC500"]                  # -8.0401
alpha2 <- coeffs["UTC500_2"]                # 9.5740
alpha3 <- coeffs["UTC500:NOUTC500:covidbefore"] # 0.0012
alpha4 <- coeffs["UTC500:NOUTC500:covidduring"] # 0.0106

# 协方差矩阵（用于标准误，可选，若仅可视化效应值可省略）
var_names <- c("UTC500", "UTC500_2", "UTC500:NOUTC500:covidbefore", "UTC500:NOUTC500:covidduring")
V <- cov[var_names, var_names] 

# 模型自由度（用于置信区间，可选）
df <- df.residual(model_interaction10)


# --------------------------
# 3. 构建二维网格（UTC500 × PM2.5）
# --------------------------
# 3.1 设定两个变量的取值范围（基于原始数据，避免外推过多）
# UTC500：取原始数据的5%~95%分位数（排除极端值）

UTC_grid <- seq(0, 1, length.out = 500)  # 100个点

# PM2.5：取原始数据的0%~95%分位数（避免极端污染值）
NOUTC500_range <- quantile(query_sample2$NOUTC500, probs = c(0, 1), na.rm = TRUE)
NOUTC500_grid <- seq(NOUTC500_range[1], NOUTC500_range[2], length.out = 500)  # 100个点

# 3.2 生成二维网格（100×100=10000个组合）
grid_data_NOUTC500 <- expand.grid(
  UTC500 = UTC_grid,
  NOUTC500 = NOUTC500_grid
)


# --------------------------
# 5. 计算二维网格中每个点的边际效应（分疫情阶段）
# --------------------------
# 5.1 疫情前边际效应（covidbefore=1）
grid_data_NOUTC500$marginal_pre <- alpha1 + 2 * alpha2 * grid_data_NOUTC500$UTC500 + alpha3 * grid_data_NOUTC500$NOUTC500

# 5.2 疫情期间边际效应（covidduring=1）
grid_data_NOUTC500$marginal_during <- alpha1 + 2 * alpha2 * grid_data_NOUTC500$UTC500 + alpha4 * grid_data_NOUTC500$NOUTC500


# --------------------------
# 6. （可选）计算边际效应的标准误（误差传播）
# --------------------------
# 构建权重矩阵（每行对应一个网格点的权重）
grid_data_NOUTC500 <- grid_data_NOUTC500 %>%
  mutate(
    # 疫情前权重：[beta1, beta2, alpha3, alpha4]的权重
    w1_pre = 1,
    w2_pre = 2 * UTC500,
    w3_pre = NOUTC500,
    w4_pre = 0,
    # 疫情期间权重
    w1_during = 1,
    w2_during = 2 * UTC500,
    w3_during = 0,
    w4_during = NOUTC500
  )

# 计算疫情前标准误
se_pre <- apply(grid_data_NOUTC500[, c("w1_pre", "w2_pre", "w3_pre", "w4_pre")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_NOUTC500$se_pre <- se_pre

# 计算疫情期间标准误
se_during <- apply(grid_data_NOUTC500[, c("w1_during", "w2_during", "w3_during", "w4_during")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_NOUTC500$se_during <- se_during


# --------------------------
# 5. 计算95%置信区间
# --------------------------
t_low <- qt(0.025, df = df)
t_high <- qt(0.975, df = df)


grid_data_NOUTC500$low_pre<-grid_data_NOUTC500$marginal_pre + grid_data_NOUTC500$se_pre*t_low
grid_data_NOUTC500$up_pre<-grid_data_NOUTC500$marginal_pre + grid_data_NOUTC500$se_pre*t_high

grid_data_NOUTC500$low_during<-grid_data_NOUTC500$marginal_during + grid_data_NOUTC500$se_during*t_low
grid_data_NOUTC500$up_during<-grid_data_NOUTC500$marginal_during + grid_data_NOUTC500$se_during*t_high


############################################################

# 假设要获取"col1"、"col3"、"col5"这三列
grid_data_NOUTC500$NOUTC500<- grid_data_NOUTC500$NOUTC500+mean_NOUTC500  ## 对去中心化的值进行还原
UTC_NOUTC500_grid_before_during<- grid_data_NOUTC500[, c("UTC500","NOUTC500", "marginal_pre", "low_pre", "up_pre","marginal_during", "low_during", "up_during")]
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_NOUTC500_grid_before_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_NOUTC500_grid_before_during, save_file_path_before)

################# 获取等于mean_UTC500的数值位置 85##################

target_UTC500 <- grid_data_NOUTC500$UTC500[85]### 选择固定阈值处的数据
# 筛选出UTC等于该值的所有行
sub_UTC_NOUTC500_grid_before_during <- subset(UTC_NOUTC500_grid_before_during, UTC500 == target_UTC500)

## before
UTC_NOUTC500_grid_before<- sub_UTC_NOUTC500_grid_before_during[, c("UTC500","NOUTC500", "marginal_pre", "low_pre","up_pre")]
colnames(UTC_NOUTC500_grid_before) <- c("UTC500","NOUTC500", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_NOUTC500_meanUTC_before', '.csv', sep="")  # 字符串连接
write.csv(UTC_NOUTC500_grid_before, save_file_path_before)

## during 
UTC_NOUTC500_grid_during<- sub_UTC_NOUTC500_grid_before_during[, c("UTC500","NOUTC500", "marginal_during", "low_during","up_during")]
colnames(UTC_NOUTC500_grid_during) <- c("UTC500","NOUTC500", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_NOUTC500_meanUTC_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_NOUTC500_grid_during, save_file_path_before)


###################



############################分层回归分析######################################

### 分别查看 UTC500 的系数在疫情前和疫情期间的差异

model_before <- felm(sentiment ~ 1 + UTC500 +UTC500_2+NOUTC500+
                       Precipitation + humidity + Wind + cloudcover + Surface_pressure +Skintemperature+
                       population500 + nightlight500+ settlement500+ GDP
                     | yearmonthday + userid , 
                     data = subset(query_sample2, covid == "before"))
print(summary(model_before))
# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model_before))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)
model_during <- felm(sentiment ~ 1 + UTC500  +UTC500_2 +NOUTC500+
                       Precipitation + humidity + Wind + cloudcover + Surface_pressure +Skintemperature+
                       population500 + nightlight500+ settlement500+ GDP
                     | yearmonthday + userid , 
                     data = subset(query_sample2, covid == "during"))

print(summary(model_during))

# 捕获第n个模型摘要的输出
model_summary <- capture.output(summary(model_during))
# 将第n个模型的输出追加到同一个txt文件中
cat(model_summary, file = output_file, sep = "\n", append = TRUE)

close(con)  # 关闭文件






############################################## UTC500 导出基础模型结果为各种所需表格#############################################

text_code1 <- stargazer(model_1, model_2, model_3, model_interaction10,  
                        type = "text", title = " Baseline Model")
write(text_code1, file = "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/secondary_data_utc500/Baseline_Results_tab_20250923.txt")


latex_code1 <- stargazer(model_1, model_2, model_3, model_interaction10,  
                         type = "latex", title = " Baseline Model")
write(latex_code1, file = "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/secondary_data_utc500/Baseline_Results_tab_latex_20250923.tex")


text_code2 <- stargazer(model_before, model_during,  
                        type = "text", title = " Hierarchical regression")
write(text_code1, file = "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/secondary_data_utc500/Hierarchical_Results_tab_20250923.txt")



latex_code2 <- stargazer(model_before, model_during,  
                         type = "latex", title = "Hierarchical regression")
write(latex_code1, file = "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/secondary_data_utc500/Hierarchical_Results_tab_latex_20250923.tex")



########### 导出交互项模型结果为各种所需表格#############

text_code31 <- stargazer(model_interaction1, model_interaction2, model_interaction3, 
                         type = "text", title = "Interaction model")
write(text_code31, file = "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/secondary_data_utc500/Interaction_Results1_tab_20250923.txt")


text_code311 <- stargazer(model_interaction4,
                          type = "text", title = "Interaction model")
write(text_code311, file = "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/secondary_data_utc500/Interaction_Results1-1_tab_20250923.txt")


text_code32 <- stargazer(model_interaction5, model_interaction6, model_interaction7,
                         type = "text", title = "Interaction model")
write(text_code32, file = "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/secondary_data_utc500/Interaction_Results2_tab_20250923.txt")


text_code322 <- stargazer( model_interaction9, 
                           type = "text", title = "Interaction model")
write(text_code322, file = "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/secondary_data_utc500/Interaction_Results2-2_tab_20250923.txt")


text_code323 <- stargazer( model_interaction0, 
                           type = "text", title = "Interaction model")
write(text_code323, file = "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/secondary_data_utc500/Interaction_Results2-3_tab_20250923.txt")


#########################################latex ########################

latex_code41 <- stargazer( model_interaction1, model_interaction2, model_interaction3,
                           type = "latex", title = "Interaction model")
write(latex_code41, file = "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/secondary_data_utc500/Interaction_Results1_tab_latex_20250923.tex")


latex_code411 <- stargazer( model_interaction4,
                            type = "latex", title = "Interaction model")
write(latex_code41, file = "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/secondary_data_utc500/Interaction_Results1-1_tab_latex_20250923.tex")


latex_code42 <- stargazer( model_interaction5, model_interaction6, model_interaction7, 
                           type = "latex", title = "Interaction model")
write(latex_code42, file = "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/secondary_data_utc500/Interaction_Results2_tab_latex_20250923.tex")

text_code422 <- stargazer( model_interaction9, 
                           type = "latex", title = "Interaction model")
write(text_code422, file = "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/secondary_data_utc500/Interaction_Results2-2_tab_latex_20250923.txt")

text_code423 <- stargazer( model_interaction0, 
                           type = "latex", title = "Interaction model")
write(text_code423, file = "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/secondary_data_utc500/Interaction_Results2-3_tab_latex_20250923.txt")










#################################################UTC1000 ##############################################################




#########################一次回归模型训练 ################################
model1000_1 <- felm(sentiment ~ 1+ UTC1000
                    |yearmonthday+userid,  data = query_sample2)


# 查看模型摘要
print(summary(model1000_1))

# 指定输出文件的路径和名称
output_file <- "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/ResultsUTC1000_fix_indivdual_city_day_20250923.txt"
con <- file(output_file, open = "a")# 打开文件连接，追加模式
writeLines("\n--- New Model Summary ---\n", con)# 写入一个分隔符（可选），以便区分不同的模型摘要
model1000_summary <- capture.output(summary(model1000_1))# 捕获模型摘要的输出
writeLines(model1000_summary, con = output_file)# 将模型摘要追加写入文件
# 关闭文件连接 close(con)


####################################UTC 常规##############################
model1000_2 <- felm(sentiment ~ 1 + UTC1000 + UTC1000_2+ NOUTC1000+ PM25+
                      Precipitation + humidity + Wind + cloudcover + Surface_pressure+Skintemperature +
                      settlement1000+population1000+ nightlight1000+GDP
                    | yearmonthday + userid, 
                    data = query_sample2)
print(summary(model1000_2))

####################### 计算VIF############################
vif_values <- vif(model1000_2)

# 打印VIF值；VIF值大于10才被认为存在严重的共线性问题;增加样本量可能有助于减轻共线性问题
print("VIF:")
print(vif_values)
#####################保存模型参数#######################
# 捕获第n个模型摘要的输出
model1000_summary <- capture.output(summary(model1000_2))
# 将第n个模型的输出追加到同一个txt文件中
cat(model1000_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)




####################################二次项模型中UTC1000 和UTC1000_2 与疫情的交互##############################
## 存在二次项不显著 UTC1000_2:covidduring -1.7748092  1.5852590  -1.120 0.262897 

model1000_03 <- felm(sentiment ~ 1 + UTC1000 + UTC1000_2+ UTC1000_2:covid+ UTC1000:covid +  NOUTC1000+PM25+
                       Precipitation + humidity + Wind + cloudcover + Surface_pressure+Skintemperature +
                       settlement1000+population1000+ nightlight1000+GDP
                     | yearmonthday + userid, 
                     data = query_sample2)
print(summary(model1000_03))

#####################保存模型参数#######################
# 捕获第n个模型摘要的输出
model1000_summary <- capture.output(summary(model1000_03))
# 将第n个模型的输出追加到同一个txt文件中
cat(model1000_summary, file = output_file, sep = "\n", append = TRUE)


####################################一次项模型中UTC1000 疫情的交互##############################

model1000_3 <- felm(sentiment ~ 1 + UTC1000 + UTC1000_2+ UTC1000:covid +  NOUTC1000+PM25+
                      Precipitation + humidity + Wind + cloudcover + Surface_pressure+Skintemperature +
                      settlement1000+population1000+ nightlight1000+GDP
                    | yearmonthday + userid, 
                    data = query_sample2)
print(summary(model1000_3))

#####################保存模型参数#######################
# 捕获第n个模型摘要的输出
model1000_summary <- capture.output(summary(model1000_3))
# 将第n个模型的输出追加到同一个txt文件中
cat(model1000_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)


##################################模型协方差计算#################################
# 提取模型系数
coeffs <-coef(model1000_3) # 获取系数

##calculate and save marginal effects, with errors clustered regionally

cov<- vcovCR(model1000_3,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解
#cov_simple <- vcov(model1000_3)  # 协方差矩阵,



############################ UTC effects_before ###########################################

# 1. 提取UTC1000和UTC1000_2的方差-协方差矩阵（仅保留这两个变量）
# 确保仅包含目标变量的协方差，避免其他变量干扰
V <- cov[c("UTC1000", "UTC1000_2"), c("UTC1000", "UTC1000_2")]

# 2. 提取UTC1000和UTC1000_2的系数（确认UTC1000_2是UTC1000的平方项）
beta <- c(coeffs["UTC1000"], coeffs["UTC1000_2"])

# 3. 获取模型残差自由度（用于计算t分布分位数）
residual_df <- df.residual(model1000_3)

# 4. 构建UTC1000的取值范围（0到1之间生成200个点）
UTC <- seq(0, 1, length.out = 200)
# 构建设计矩阵（包含一次项和二次项）
X <- cbind(UTC, UTC^2)  # 确保与模型中的UTC1000和UTC1000_2对应

# 5. 计算平均效应
mean_effect <- X %*% beta 

# 6. 计算效应的标准误差
# 利用方差-协方差矩阵计算每个点的标准误
se <- sqrt(rowSums(X %*% V * X))

# 7. 计算95%置信区间（t分布）
# 注意：qt(0.025, df)为负值，因此下限=均值+se*负值，上限=均值-se*负值
ci_low <- mean_effect + se * qt(0.025, df = residual_df)
ci_high <- mean_effect - se * qt(0.025, df = residual_df)

# 8. 整理结果并保存
# 构建数据框（包含UTC值、平均效应及置信区间）
Data_effects_UTC1000_UTC_before <- data.frame(
  UTC = UTC,
  mean_value = as.vector(mean_effect),  # 转换为向量便于存储
  Low_value = as.vector(ci_low),
  UP_value = as.vector(ci_high)
)

save_file_path= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1000/',  'UTC_effects_before', '.csv', sep="")  # 字符串连接
write.csv(Data_effects_UTC1000_UTC_before,save_file_path)




############################ UTC effects during ###########################################


####################### 1. 提取疫情期间效应所需的系数与协方差 ##############################
# （1）提取关键系数：UTC1000主效应、UTC1000二次项、UTC1000:covidduring交互项
# 提取所有系数（明确命名便于后续调用）
beta1 <- coeffs["UTC1000"]          # UTC1000主效应系数（β₁）
beta2 <- coeffs["UTC1000_2"]        # UTC1000二次项系数（β₂）
beta3 <- coeffs["UTC1000:covidduring"]  # 交互项系数（β₃）

# （2）提取上述3个系数的协方差矩阵（含三者间的协方差，影响标准误计算）
# 协方差矩阵需包含：UTC1000、UTC1000_2、UTC1000:covidduring
# （2）提取上述3个系数的协方差矩阵（含三者间的协方差，影响标准误计算）
# 协方差矩阵需包含：UTC1000、UTC1000_2、UTC1000:covidduring

cov_matrix <- as.matrix(cov)  # 转换为矩阵格式，便于提取子集

V_original <- cov_matrix[
  rownames(cov_matrix) %in% c("UTC1000", "UTC1000_2", "UTC1000:covidduring"),
  colnames(cov_matrix) %in% c("UTC1000", "UTC1000_2", "UTC1000:covidduring")
]

# 重命名行/列，避免混淆
rownames(V_original) <- colnames(V_original) <- c("beta1", "beta2", "beta3")

####################### 2. 构建匹配维度的系数与协方差矩阵 ##############################
# （1）疫情期间的效应系数（2个元素，与X的列数匹配）
beta <- c(
  beta_linear = beta1 + beta3,  # 合并后的线性项系数（β₁+β₃）
  beta_quad = beta2             # 二次项系数（β₂）
)

# （2）构建2×2协方差矩阵（与beta、X维度匹配）
V <- matrix(
  nrow = 2, ncol = 2,
  dimnames = list(c("beta_linear", "beta_quad"), c("beta_linear", "beta_quad"))
)
# 填充协方差矩阵（基于方差运算规则）
V["beta_linear", "beta_linear"] <- V_original["beta1","beta1"] + V_original["beta3","beta3"] + 2*V_original["beta1","beta3"]  # Var(β₁+β₃)
V["beta_linear", "beta_quad"]   <- V_original["beta1","beta2"] + V_original["beta3","beta2"]  # Cov(β₁+β₃, β₂)
V["beta_quad", "beta_linear"]   <- V["beta_linear", "beta_quad"]  # 协方差矩阵对称
V["beta_quad", "beta_quad"]     <- V_original["beta2","beta2"]  # Var(β₂)




####################### 3. 计算疫情期间UTC1000的效应及置信区间 ##############################
# （1）构建UTC1000的取值范围（与疫情前一致，便于对比，0到1生成200个点）
UTC <- seq(0, 1, length.out = 200)

# （2）构建设计矩阵X：对应疫情期间的效应公式
# X第一列：UTC1000（对应线性项总系数 beta_linear）
# X第二列：UTC1000²（对应二次项系数 beta_quad）
X <- cbind(
  UTC = UTC,
  UTC_sq = UTC^2
)

# （3）计算疫情期间UTC1000的平均效应
mean_effect <- X %*% beta  # 矩阵乘法，得到每个UTC值对应的效应

# （4）计算效应的聚类稳健标准误（关键：考虑3个系数的协方差）
# 公式逻辑：se = sqrt( X × V × X' )，rowSums(X %*% V * X) 等价于对角线元素提取
se <- sqrt(rowSums(X %*% V * X))

# （5）计算95%置信区间（t分布，使用模型残差自由度）
residual_df <- df.residual(model1000_3)  # 模型输出中为 4256966
ci_low <- mean_effect + se * qt(0.025, df = residual_df)  # 下限
ci_high <- mean_effect - se * qt(0.025, df = residual_df)  # 上限


####################### 4. 整理结果并保存 ##############################
Data_effects_UTC1000_UTC_during <- data.frame(
  UTC = UTC,
  mean_value = as.vector(mean_effect),  # 转换为向量便于存储
  Low_value = as.vector(ci_low),            # 95%置信区间下限
  UP_value = as.vector(ci_high)           # 95%置信区间上限
)

#步骤 7:  数据保存
save_file_path= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1000/',  'UTC_effects_during', '.csv', sep="")  # 字符串连接
write.csv(Data_effects_UTC1000_UTC_during,save_file_path)




######################### marginal effects################################


######################### 1. 提取模型系数#####################
beta1 <- coeffs["UTC1000"]          # β_UTC1000
beta2 <- coeffs["UTC1000_2"]        # β_UTC1000_2
beta3 <- coeffs["UTC1000:covidduring"]  # β_UTC1000:covidduring

cov_matrix <- as.matrix(cov)  # 转换为矩阵格式，便于提取子集
# 提取聚类稳健协方差矩阵（3个关键系数）
V <- cov_matrix[
  c("UTC1000", "UTC1000_2", "UTC1000:covidduring"),
  c("UTC1000", "UTC1000_2", "UTC1000:covidduring")
]
colnames(V) <- rownames(V) <- c("beta1", "beta2", "beta3")  # 重命名便于引用


####################### 2. 构建UTC1000的取值范围 ##############################
UTC <- seq(0, 1, length.out = 200)  # 与之前一致的取值范围


####################### 3. 疫情前的边际效应 ##############################
# （1）边际效应值
me_before <- as.vector(beta1 + 2 * beta2 * UTC)

# （2）边际效应的标准误（基于方差性质）
# 方差 = Var(beta1) + (2*UTC)^2*Var(beta2) + 2*(2*UTC)*Cov(beta1, beta2)
var_me_before <- V["beta1","beta1"] + (2*UTC)^2 * V["beta2","beta2"] + 2*(2*UTC)*V["beta1","beta2"]
se_before <- sqrt(var_me_before)

# （3）95%置信区间
residual_df <- df.residual(model1000_3)
ci_low_before <- me_before + se_before * qt(0.025, df = residual_df)
ci_high_before <- me_before - se_before * qt(0.025, df = residual_df)


####################### 4. 疫情期间的边际效应 ##############################
# （1）边际效应值
me_during <- as.vector( (beta1 + beta3) + 2 * beta2 * UTC )

# （2）边际效应的标准误
# 方差 = Var(beta1+beta3) + (2*UTC)^2*Var(beta2) + 2*(2*UTC)*Cov(beta1+beta3, beta2)
# 其中：Var(beta1+beta3) = Var(beta1) + Var(beta3) + 2*Cov(beta1, beta3)
#       Cov(beta1+beta3, beta2) = Cov(beta1, beta2) + Cov(beta3, beta2)
var_me_during <- (V["beta1","beta1"] + V["beta3","beta3"] + 2*V["beta1","beta3"]) + 
  (2*UTC)^2 * V["beta2","beta2"] + 
  2*(2*UTC)*(V["beta1","beta2"] + V["beta3","beta2"])
se_during <- sqrt(var_me_during)

# （3）95%置信区间
ci_low_during <- me_during + se_during * qt(0.025, df = residual_df)
ci_high_during <- me_during - se_during * qt(0.025, df = residual_df)


####################### 5. 边际效应保存 ##############################
# 构建数据框,保存疫情前边界效应数据
marginaleffects_UTC1000_before <- data.frame(UTC1000 = UTC, mean_value = me_before, 
                                             UP_value = ci_high_before, Low_value = ci_low_before)
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1000/',  'UTC_UTC1000_before', '.csv', sep="")  # 字符串连接

write.csv(marginaleffects_UTC1000_before, save_file_path_before)


# 构建数据框,保存疫情期间边界效应数据

marginaleffects_UTC1000_during <- data.frame(UTC1000 = UTC, mean_value = me_during, 
                                             UP_value = ci_high_during, Low_value = ci_low_during)
save_file_path_during= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1000/',  'UTC_UTC1000_during', '.csv', sep="")  # 字符串连接

write.csv(marginaleffects_UTC1000_during, save_file_path_during)










####################################UTC与疫情和 PM25 的交互##############################

model1000_interaction0 <- felm(sentiment ~ 1 + UTC1000+UTC1000_2+ UTC1000:PM25:covid + NOUTC1000+ 
                                 Precipitation+humidity+ Wind + cloudcover + Surface_pressure+Skintemperature +
                                 population1000 + nightlight1000+ settlement1000+GDP
                               | yearmonthday + userid , 
                               data = query_sample2)

print(summary(model1000_interaction0))


#####################保存模型参数#######################
# 捕获第n个模型摘要的输出
model1000_summary <- capture.output(summary(model1000_interaction0))
# 将第n个模型的输出追加到同一个txt文件中
cat(model1000_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)

# 提取模型系数
coeffs <-coef(model1000_interaction0) # 获取系数

##calculate and save marginal effects, with errors clustered regionally
cov <- vcovCR(model1000_interaction0,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解


####################before COVID19 边界效应 #######################

# --------------------------
# 2. 提取模型参数（同前）
# --------------------------
alpha1 <- coeffs["UTC1000"]                  # -8.0401
alpha2 <- coeffs["UTC1000_2"]                # 9.5740
alpha3 <- coeffs["UTC1000:PM25:covidbefore"] # 0.0012
alpha4 <- coeffs["UTC1000:PM25:covidduring"] # 0.0106

# 协方差矩阵（用于标准误，可选，若仅可视化效应值可省略）
var_names <- c("UTC1000", "UTC1000_2", "UTC1000:PM25:covidbefore", "UTC1000:PM25:covidduring")
V <- cov[var_names, var_names] 

# 模型自由度（用于置信区间，可选）
df <- df.residual(model1000_interaction0)


# --------------------------
# 3. 构建二维网格（UTC1000 × PM2.5）
# --------------------------
# 3.1 设定两个变量的取值范围（基于原始数据，避免外推过多）
# UTC1000：取原始数据的5%~95%分位数（排除极端值）

UTC_grid <- seq(0, 1, length.out = 1000)  # 100个点

# PM2.5：取原始数据的0%~95%分位数（避免极端污染值）
pm25_range <- quantile(query_sample2$PM25, probs = c(0, 1), na.rm = TRUE)
PM25_grid <- seq(pm25_range[1], pm25_range[2], length.out = 1000)  # 100个点

# 3.2 生成二维网格（100×100=10000个组合）
grid_data_PM25 <- expand.grid(
  UTC1000 = UTC_grid,
  PM25 = PM25_grid
)


# --------------------------
# 5. 计算二维网格中每个点的边际效应（分疫情阶段）
# --------------------------
# 5.1 疫情前边际效应（covidbefore=1）
grid_data_PM25$marginal_pre <- alpha1 + 2 * alpha2 * grid_data_PM25$UTC1000 + alpha3 * grid_data_PM25$PM25

# 5.2 疫情期间边际效应（covidduring=1）
grid_data_PM25$marginal_during <- alpha1 + 2 * alpha2 * grid_data_PM25$UTC1000 + alpha4 * grid_data_PM25$PM25


# --------------------------
# 6. （可选）计算边际效应的标准误（误差传播）
# --------------------------
# 构建权重矩阵（每行对应一个网格点的权重）
grid_data_PM25 <- grid_data_PM25 %>%
  mutate(
    # 疫情前权重：[beta1, beta2, alpha3, alpha4]的权重
    w1_pre = 1,
    w2_pre = 2 * UTC1000,
    w3_pre = PM25,
    w4_pre = 0,
    # 疫情期间权重
    w1_during = 1,
    w2_during = 2 * UTC1000,
    w3_during = 0,
    w4_during = PM25
  )

# 计算疫情前标准误
se_pre <- apply(grid_data_PM25[, c("w1_pre", "w2_pre", "w3_pre", "w4_pre")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_PM25$se_pre <- se_pre

# 计算疫情期间标准误
se_during <- apply(grid_data_PM25[, c("w1_during", "w2_during", "w3_during", "w4_during")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_PM25$se_during <- se_during


# --------------------------
# 5. 计算95%置信区间
# --------------------------
t_low <- qt(0.025, df = df)
t_high <- qt(0.975, df = df)


grid_data_PM25$low_pre<-grid_data_PM25$marginal_pre + grid_data_PM25$se_pre*t_low
grid_data_PM25$up_pre<-grid_data_PM25$marginal_pre + grid_data_PM25$se_pre*t_high

grid_data_PM25$low_during<-grid_data_PM25$marginal_during + grid_data_PM25$se_during*t_low
grid_data_PM25$up_during<-grid_data_PM25$marginal_during + grid_data_PM25$se_during*t_high


############################################################

# 假设要获取"col1"、"col3"、"col5"这三列

grid_data_PM25$PM25<- grid_data_PM25$PM25+mean_PM25  ## 对去中心化的值进行还原
UTC_PM25_grid_before_during<- grid_data_PM25[, c("UTC1000","PM25", "marginal_pre", "low_pre", "up_pre","marginal_during", "low_during", "up_during")]

save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1000/',  'UTC_PM25_grid_before_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_PM25_grid_before_during, save_file_path_before)

################# 获取等于mean_UTC1000的数值位置 85##################

target_UTC1000 <- grid_data_PM25$UTC1000[85]### 选择固定阈值处的数据
# 筛选出UTC等于该值的所有行
sub_UTC_PM25_grid_before_during <- subset(UTC_PM25_grid_before_during, UTC1000 == target_UTC1000)

## before
UTC_PM25_grid_before<- sub_UTC_PM25_grid_before_during[, c("UTC1000","PM25", "marginal_pre", "low_pre","up_pre")]
colnames(UTC_PM25_grid_before) <- c("UTC1000","PM25", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1000/',  'UTC_PM25_meanUTC_before', '.csv', sep="")  # 字符串连接
write.csv(UTC_PM25_grid_before, save_file_path_before)

## during 
UTC_PM25_grid_during<- sub_UTC_PM25_grid_before_during[, c("UTC1000","PM25", "marginal_during", "low_during","up_during")]
colnames(UTC_PM25_grid_during) <- c("UTC1000","PM25", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1000/',  'UTC_PM25_meanUTC_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_PM25_grid_during, save_file_path_before)


###################





# # 7. 可视化：二维热力图（分疫情阶段）
# # --------------------------
# # 7.1 数据整理：将宽格式转为长格式（便于分面）
# plot_data <- UTC_PM25_grid_before_during %>%
#   select(UTC1000, PM25, marginal_pre, marginal_during) %>%
#   pivot_longer(
#     cols = starts_with("marginal"),
#     names_to = "period",
#     values_to = "marginal_effect",
#     names_prefix = "marginal_"
#   ) %>%
#   mutate(
#     period = case_when(
#       period == "pre" ~ "Before Covid-19",
#       period == "during" ~ "During Covid-19"
#     )
#   )
# 
# 
# # 7.2 绘制热力图（填充色=边际效应大小，等高线=效应等值线）
# ggplot(plot_data, aes(x = UTC1000, y = PM25)) +
#   geom_tile(aes(fill = marginal_effect), alpha = 1.0) +
#   geom_contour(aes(z = marginal_effect), color = "white", size = 1.0, breaks = seq(-10, 10, 5)) +
#   metR::geom_text_contour(aes(z = marginal_effect), color = "white", size = 3, breaks = seq(-10, 10, 5)) +
#   facet_wrap(~period, ncol =2) +
#   # 移除midpoint参数，使用默认viridis色阶
#   scale_fill_viridis_c(
#     option = "inferno",  # 可选："viridis", "magma", "inferno", "plasma",  "rocket", "mako", "turbo"
#     name = "Marginal Effects",
#     limits = c(min(plot_data$marginal_effect), max(plot_data$marginal_effect))
#   ) +
#   labs(
#     x = "UTC1000",
#     y = "PM2.5（μg/m³）",
#     title = "The marginal effects of trees varying based on levels of PM2.5 (buffer of 1000 m)"
#   ) +
#   theme_bw() +
#   theme(
#     plot.title = element_text(hjust = 0.5),
#     legend.position = "bottom",
#     legend.key.width = unit(3, "cm")
#   )
# 


####################################UTC二次项与疫情和 Precipitation 的交互##############################

model1000_interaction1 <- felm(sentiment ~ 1 + UTC1000+UTC1000_2+ UTC1000:Precipitation:covid  + NOUTC1000+PM25+
                                 humidity+ Wind + cloudcover + Surface_pressure+Skintemperature +
                                 population1000 + nightlight1000+ settlement1000+GDP
                               | yearmonthday + userid, 
                               data = query_sample2)

print(summary(model1000_interaction1))
#####################保存模型参数#######################
# 捕获第n个模型摘要的输出
model1000_summary <- capture.output(summary(model1000_interaction1))
# 将第n个模型的输出追加到同一个txt文件中
cat(model1000_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

##################################模型协方差计算#################################
# 提取模型系数
coeffs <-coef(model1000_interaction1) # 获取系数

##calculate and save marginal effects, with errors clustered regionally
cov <- vcovCR(model1000_interaction1,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解


####################before COVID19  生成效应面#######################

####################before COVID19 边界效应 #######################

# --------------------------
# 2. 提取模型参数（同前）
# --------------------------
alpha1 <- coeffs["UTC1000"]                  # -8.0401
alpha2 <- coeffs["UTC1000_2"]                # 9.5740
alpha3 <- coeffs["UTC1000:Precipitation:covidbefore"] # 0.0012
alpha4 <- coeffs["UTC1000:Precipitation:covidduring"] # 0.0106

# 协方差矩阵（用于标准误，可选，若仅可视化效应值可省略）
var_names <- c("UTC1000", "UTC1000_2", "UTC1000:Precipitation:covidbefore", "UTC1000:Precipitation:covidduring")
V <- cov[var_names, var_names] 

# 模型自由度（用于置信区间，可选）
df <- df.residual(model1000_interaction1)


# --------------------------
# 3. 构建二维网格（UTC1000 × PM2.5）
# --------------------------
# 3.1 设定两个变量的取值范围（基于原始数据，避免外推过多）
# UTC1000：取原始数据的5%~95%分位数（排除极端值）

UTC_grid <- seq(0, 1, length.out = 1000)  # 100个点

# PM2.5：取原始数据的0%~95%分位数（避免极端污染值）
Precipitation_range <- quantile(query_sample2$Precipitation, probs = c(0, 1), na.rm = TRUE)
Precipitation_grid <- seq(Precipitation_range[1], Precipitation_range[2], length.out = 1000)  # 100个点

# 3.2 生成二维网格（100×100=10000个组合）
grid_data_Precipitation <- expand.grid(
  UTC1000 = UTC_grid,
  Precipitation = Precipitation_grid
)


# --------------------------
# 5. 计算二维网格中每个点的边际效应（分疫情阶段）
# --------------------------
# 5.1 疫情前边际效应（covidbefore=1）
grid_data_Precipitation$marginal_pre <- alpha1 + 2 * alpha2 * grid_data_Precipitation$UTC1000 + alpha3 * grid_data_Precipitation$Precipitation

# 5.2 疫情期间边际效应（covidduring=1）
grid_data_Precipitation$marginal_during <- alpha1 + 2 * alpha2 * grid_data_Precipitation$UTC1000 + alpha4 * grid_data_Precipitation$Precipitation


# --------------------------
# 6. （可选）计算边际效应的标准误（误差传播）
# --------------------------
# 构建权重矩阵（每行对应一个网格点的权重）
grid_data_Precipitation <- grid_data_Precipitation %>%
  mutate(
    # 疫情前权重：[beta1, beta2, alpha3, alpha4]的权重
    w1_pre = 1,
    w2_pre = 2 * UTC1000,
    w3_pre = Precipitation,
    w4_pre = 0,
    # 疫情期间权重
    w1_during = 1,
    w2_during = 2 * UTC1000,
    w3_during = 0,
    w4_during = Precipitation
  )

# 计算疫情前标准误
se_pre <- apply(grid_data_Precipitation[, c("w1_pre", "w2_pre", "w3_pre", "w4_pre")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_Precipitation$se_pre <- se_pre

# 计算疫情期间标准误
se_during <- apply(grid_data_Precipitation[, c("w1_during", "w2_during", "w3_during", "w4_during")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_Precipitation$se_during <- se_during


# --------------------------
# 5. 计算95%置信区间
# --------------------------
t_low <- qt(0.025, df = df)
t_high <- qt(0.975, df = df)


grid_data_Precipitation$low_pre<-grid_data_Precipitation$marginal_pre + grid_data_Precipitation$se_pre*t_low
grid_data_Precipitation$up_pre<-grid_data_Precipitation$marginal_pre + grid_data_Precipitation$se_pre*t_high

grid_data_Precipitation$low_during<-grid_data_Precipitation$marginal_during + grid_data_Precipitation$se_during*t_low
grid_data_Precipitation$up_during<-grid_data_Precipitation$marginal_during + grid_data_Precipitation$se_during*t_high


############################################################

# 假设要获取"col1"、"col3"、"col5"这三列
grid_data_Precipitation$Precipitation<- grid_data_Precipitation$Precipitation+mean_Precipitation  ## 对去中心化的值进行还原
UTC_Precipitation_grid_before_during<- grid_data_Precipitation[, c("UTC1000","Precipitation", "marginal_pre", "low_pre", "up_pre","marginal_during", "low_during", "up_during")]
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1000/',  'UTC_Precipitation_grid_before_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_Precipitation_grid_before_during, save_file_path_before)

################# 获取等于mean_UTC1000的数值位置 85##################

target_UTC1000 <- grid_data_Precipitation$UTC1000[85]### 选择固定阈值处的数据
# 筛选出UTC等于该值的所有行
sub_UTC_Precipitation_grid_before_during <- subset(UTC_Precipitation_grid_before_during, UTC1000 == target_UTC1000)

## before
UTC_Precipitation_grid_before<- sub_UTC_Precipitation_grid_before_during[, c("UTC1000","Precipitation", "marginal_pre", "low_pre","up_pre")]
colnames(UTC_Precipitation_grid_before) <- c("UTC1000","Precipitation", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1000/',  'UTC_Precipitation_meanUTC_before', '.csv', sep="")  # 字符串连接
write.csv(UTC_Precipitation_grid_before, save_file_path_before)

## during 
UTC_Precipitation_grid_during<- sub_UTC_Precipitation_grid_before_during[, c("UTC1000","Precipitation", "marginal_during", "low_during","up_during")]
colnames(UTC_Precipitation_grid_during) <- c("UTC1000","Precipitation", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1000/',  'UTC_Precipitation_meanUTC_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_Precipitation_grid_during, save_file_path_before)


###################



####################################UTC与疫情和 humidity 的交互##############################

model1000_interaction2 <- felm(sentiment ~ 1 + UTC1000+UTC1000_2+ UTC1000:humidity:covid +NOUTC1000+PM25+
                                 Precipitation +Wind   + cloudcover + Surface_pressure+Skintemperature +
                                 population1000 + nightlight1000+ settlement1000+GDP
                               | yearmonthday + userid , 
                               data = query_sample2)

print(summary(model1000_interaction2))

#####################保存模型参数#######################
# 捕获第n个模型摘要的输出
model1000_summary <- capture.output(summary(model1000_interaction2))
# 将第n个模型的输出追加到同一个txt文件中
cat(model1000_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)


##################################模型协方差计算#################################


# 提取模型系数
coeffs <-coef(model1000_interaction2) # 获取系数

##calculate and save marginal effects, with errors clustered regionally
cov <- vcovCR(model1000_interaction2,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解


####################before COVID19 #######################
####################before COVID19 边界效应 #######################

# --------------------------
# 2. 提取模型参数（同前）
# --------------------------
alpha1 <- coeffs["UTC1000"]                  # -8.0401
alpha2 <- coeffs["UTC1000_2"]                # 9.5740
alpha3 <- coeffs["UTC1000:humidity:covidbefore"] # 0.0012
alpha4 <- coeffs["UTC1000:humidity:covidduring"] # 0.0106

# 协方差矩阵（用于标准误，可选，若仅可视化效应值可省略）
var_names <- c("UTC1000", "UTC1000_2", "UTC1000:humidity:covidbefore", "UTC1000:humidity:covidduring")
V <- cov[var_names, var_names] 

# 模型自由度（用于置信区间，可选）
df <- df.residual(model1000_interaction2)


# --------------------------
# 3. 构建二维网格（UTC1000 × PM2.5）
# --------------------------
# 3.1 设定两个变量的取值范围（基于原始数据，避免外推过多）
# UTC1000：取原始数据的5%~95%分位数（排除极端值）

UTC_grid <- seq(0, 1, length.out = 1000)  # 100个点

# PM2.5：取原始数据的0%~95%分位数（避免极端污染值）
humidity_range <- quantile(query_sample2$humidity, probs = c(0, 1), na.rm = TRUE)
humidity_grid <- seq(humidity_range[1], humidity_range[2], length.out = 1000)  # 100个点

# 3.2 生成二维网格（100×100=10000个组合）
grid_data_humidity <- expand.grid(
  UTC1000 = UTC_grid,
  humidity = humidity_grid
)


# --------------------------
# 5. 计算二维网格中每个点的边际效应（分疫情阶段）
# --------------------------
# 5.1 疫情前边际效应（covidbefore=1）
grid_data_humidity$marginal_pre <- alpha1 + 2 * alpha2 * grid_data_humidity$UTC1000 + alpha3 * grid_data_humidity$humidity

# 5.2 疫情期间边际效应（covidduring=1）
grid_data_humidity$marginal_during <- alpha1 + 2 * alpha2 * grid_data_humidity$UTC1000 + alpha4 * grid_data_humidity$humidity


# --------------------------
# 6. （可选）计算边际效应的标准误（误差传播）
# --------------------------
# 构建权重矩阵（每行对应一个网格点的权重）
grid_data_humidity <- grid_data_humidity %>%
  mutate(
    # 疫情前权重：[beta1, beta2, alpha3, alpha4]的权重
    w1_pre = 1,
    w2_pre = 2 * UTC1000,
    w3_pre = humidity,
    w4_pre = 0,
    # 疫情期间权重
    w1_during = 1,
    w2_during = 2 * UTC1000,
    w3_during = 0,
    w4_during = humidity
  )

# 计算疫情前标准误
se_pre <- apply(grid_data_humidity[, c("w1_pre", "w2_pre", "w3_pre", "w4_pre")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_humidity$se_pre <- se_pre

# 计算疫情期间标准误
se_during <- apply(grid_data_humidity[, c("w1_during", "w2_during", "w3_during", "w4_during")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_humidity$se_during <- se_during


# --------------------------
# 5. 计算95%置信区间
# --------------------------
t_low <- qt(0.025, df = df)
t_high <- qt(0.975, df = df)


grid_data_humidity$low_pre<-grid_data_humidity$marginal_pre + grid_data_humidity$se_pre*t_low
grid_data_humidity$up_pre<-grid_data_humidity$marginal_pre + grid_data_humidity$se_pre*t_high

grid_data_humidity$low_during<-grid_data_humidity$marginal_during + grid_data_humidity$se_during*t_low
grid_data_humidity$up_during<-grid_data_humidity$marginal_during + grid_data_humidity$se_during*t_high


############################################################

# 假设要获取"col1"、"col3"、"col5"这三列
grid_data_humidity$humidity<- grid_data_humidity$humidity+mean_humidity  ## 对去中心化的值进行还原
UTC_humidity_grid_before_during<- grid_data_humidity[, c("UTC1000","humidity", "marginal_pre", "low_pre", "up_pre","marginal_during", "low_during", "up_during")]
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1000/',  'UTC_humidity_grid_before_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_humidity_grid_before_during, save_file_path_before)

################# 获取等于mean_UTC1000的数值位置 85##################

target_UTC1000 <- grid_data_humidity$UTC1000[85]### 选择固定阈值处的数据
# 筛选出UTC等于该值的所有行
sub_UTC_humidity_grid_before_during <- subset(UTC_humidity_grid_before_during, UTC1000 == target_UTC1000)

## before
UTC_humidity_grid_before<- sub_UTC_humidity_grid_before_during[, c("UTC1000","humidity", "marginal_pre", "low_pre","up_pre")]
colnames(UTC_humidity_grid_before) <- c("UTC1000","humidity", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1000/',  'UTC_humidity_meanUTC_before', '.csv', sep="")  # 字符串连接
write.csv(UTC_humidity_grid_before, save_file_path_before)

## during 
UTC_humidity_grid_during<- sub_UTC_humidity_grid_before_during[, c("UTC1000","humidity", "marginal_during", "low_during","up_during")]
colnames(UTC_humidity_grid_during) <- c("UTC1000","humidity", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1000/',  'UTC_humidity_meanUTC_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_humidity_grid_during, save_file_path_before)



####################################UTC与疫情和 Skintemperature 的交互##############################

model1000_interaction3 <- felm(sentiment ~ 1 + UTC1000+UTC1000_2+ UTC1000:Skintemperature:covid + NOUTC1000+PM25+
                                 humidity +Precipitation + Wind + cloudcover + Surface_pressure +
                                 population1000 + nightlight1000+ settlement1000+GDP
                               | yearmonthday + userid , 
                               data = query_sample2)

print(summary(model1000_interaction3))

#####################保存模型参数#######################
# 捕获第n个模型摘要的输出
model1000_summary <- capture.output(summary(model1000_interaction3))
# 将第n个模型的输出追加到同一个txt文件中
cat(model1000_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)


##################################模型协方差计算#################################


# 提取模型系数
coeffs <-coef(model1000_interaction3) # 获取系数

##calculate and save marginal effects, with errors clustered regionally
cov <- vcovCR(model1000_interaction3,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解


####################before COVID19 #######################
####################before COVID19 边界效应 #######################

# --------------------------
# 2. 提取模型参数（同前）
# --------------------------
alpha1 <- coeffs["UTC1000"]                  # -8.0401
alpha2 <- coeffs["UTC1000_2"]                # 9.5740
alpha3 <- coeffs["UTC1000:Skintemperature:covidbefore"] # 0.0012
alpha4 <- coeffs["UTC1000:Skintemperature:covidduring"] # 0.0106

# 协方差矩阵（用于标准误，可选，若仅可视化效应值可省略）
var_names <- c("UTC1000", "UTC1000_2", "UTC1000:Skintemperature:covidbefore", "UTC1000:Skintemperature:covidduring")
V <- cov[var_names, var_names] 

# 模型自由度（用于置信区间，可选）
df <- df.residual(model1000_interaction3)


# --------------------------
# 3. 构建二维网格（UTC1000 × PM2.5）
# --------------------------
# 3.1 设定两个变量的取值范围（基于原始数据，避免外推过多）
# UTC1000：取原始数据的5%~95%分位数（排除极端值）

UTC_grid <- seq(0, 1, length.out = 1000)  # 100个点

# PM2.5：取原始数据的0%~95%分位数（避免极端污染值）
Skintemperature_range <- quantile(query_sample2$Skintemperature, probs = c(0, 1), na.rm = TRUE)
Skintemperature_grid <- seq(Skintemperature_range[1], Skintemperature_range[2], length.out = 1000)  # 100个点

# 3.2 生成二维网格（100×100=10000个组合）
grid_data_Skintemperature <- expand.grid(
  UTC1000 = UTC_grid,
  Skintemperature = Skintemperature_grid
)


# --------------------------
# 5. 计算二维网格中每个点的边际效应（分疫情阶段）
# --------------------------
# 5.1 疫情前边际效应（covidbefore=1）
grid_data_Skintemperature$marginal_pre <- alpha1 + 2 * alpha2 * grid_data_Skintemperature$UTC1000 + alpha3 * grid_data_Skintemperature$Skintemperature

# 5.2 疫情期间边际效应（covidduring=1）
grid_data_Skintemperature$marginal_during <- alpha1 + 2 * alpha2 * grid_data_Skintemperature$UTC1000 + alpha4 * grid_data_Skintemperature$Skintemperature


# --------------------------
# 6. （可选）计算边际效应的标准误（误差传播）
# --------------------------
# 构建权重矩阵（每行对应一个网格点的权重）
grid_data_Skintemperature <- grid_data_Skintemperature %>%
  mutate(
    # 疫情前权重：[beta1, beta2, alpha3, alpha4]的权重
    w1_pre = 1,
    w2_pre = 2 * UTC1000,
    w3_pre = Skintemperature,
    w4_pre = 0,
    # 疫情期间权重
    w1_during = 1,
    w2_during = 2 * UTC1000,
    w3_during = 0,
    w4_during = Skintemperature
  )

# 计算疫情前标准误
se_pre <- apply(grid_data_Skintemperature[, c("w1_pre", "w2_pre", "w3_pre", "w4_pre")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_Skintemperature$se_pre <- se_pre

# 计算疫情期间标准误
se_during <- apply(grid_data_Skintemperature[, c("w1_during", "w2_during", "w3_during", "w4_during")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_Skintemperature$se_during <- se_during


# --------------------------
# 5. 计算95%置信区间
# --------------------------
t_low <- qt(0.025, df = df)
t_high <- qt(0.975, df = df)


grid_data_Skintemperature$low_pre<-grid_data_Skintemperature$marginal_pre + grid_data_Skintemperature$se_pre*t_low
grid_data_Skintemperature$up_pre<-grid_data_Skintemperature$marginal_pre + grid_data_Skintemperature$se_pre*t_high

grid_data_Skintemperature$low_during<-grid_data_Skintemperature$marginal_during + grid_data_Skintemperature$se_during*t_low
grid_data_Skintemperature$up_during<-grid_data_Skintemperature$marginal_during + grid_data_Skintemperature$se_during*t_high


############################################################

# 假设要获取"col1"、"col3"、"col5"这三列
grid_data_Skintemperature$Skintemperature<- grid_data_Skintemperature$Skintemperature+mean_Skintemperature  ## 对去中心化的值进行还原
UTC_Skintemperature_grid_before_during<- grid_data_Skintemperature[, c("UTC1000","Skintemperature", "marginal_pre", "low_pre", "up_pre","marginal_during", "low_during", "up_during")]
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1000/',  'UTC_Skintemperature_grid_before_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_Skintemperature_grid_before_during, save_file_path_before)

################# 获取等于mean_UTC1000的数值位置 85##################

target_UTC1000 <- grid_data_Skintemperature$UTC1000[85]### 选择固定阈值处的数据
# 筛选出UTC等于该值的所有行
sub_UTC_Skintemperature_grid_before_during <- subset(UTC_Skintemperature_grid_before_during, UTC1000 == target_UTC1000)

## before
UTC_Skintemperature_grid_before<- sub_UTC_Skintemperature_grid_before_during[, c("UTC1000","Skintemperature", "marginal_pre", "low_pre","up_pre")]
colnames(UTC_Skintemperature_grid_before) <- c("UTC1000","Skintemperature", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1000/',  'UTC_Skintemperature_meanUTC_before', '.csv', sep="")  # 字符串连接
write.csv(UTC_Skintemperature_grid_before, save_file_path_before)

## during 
UTC_Skintemperature_grid_during<- sub_UTC_Skintemperature_grid_before_during[, c("UTC1000","Skintemperature", "marginal_during", "low_during","up_during")]
colnames(UTC_Skintemperature_grid_during) <- c("UTC1000","Skintemperature", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1000/',  'UTC_Skintemperature_meanUTC_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_Skintemperature_grid_during, save_file_path_before)


###################




####################################UTC与疫情和 Surface_pressure 的交互##############################

model1000_interaction4 <- felm(sentiment ~ 1 + UTC1000+UTC1000_2+ UTC1000:Surface_pressure:covid + NOUTC1000+PM25+
                                 humidity + Precipitation + Wind + cloudcover +Skintemperature +Surface_pressure+
                                 population1000 + nightlight1000+ settlement1000+GDP
                               | yearmonthday + userid , 
                               data = query_sample2)

print(summary(model1000_interaction4))

#####################保存模型参数#######################
# 捕获第n个模型摘要的输出
model1000_summary <- capture.output(summary(model1000_interaction4))
# 将第n个模型的输出追加到同一个txt文件中
cat(model1000_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)


##################################模型协方差计算#################################


# 提取模型系数
coeffs <-coef(model1000_interaction4) # 获取系数

##calculate and save marginal effects, with errors clustered regionally
cov <- vcovCR(model1000_interaction4,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解


####################before COVID19 边界效应 #######################

# --------------------------
# 2. 提取模型参数（同前）
# --------------------------
alpha1 <- coeffs["UTC1000"]                  # -8.0401
alpha2 <- coeffs["UTC1000_2"]                # 9.5740
alpha3 <- coeffs["UTC1000:Surface_pressure:covidbefore"] # 0.0012
alpha4 <- coeffs["UTC1000:Surface_pressure:covidduring"] # 0.0106

# 协方差矩阵（用于标准误，可选，若仅可视化效应值可省略）
var_names <- c("UTC1000", "UTC1000_2", "UTC1000:Surface_pressure:covidbefore", "UTC1000:Surface_pressure:covidduring")
V <- cov[var_names, var_names] 

# 模型自由度（用于置信区间，可选）
df <- df.residual(model1000_interaction4)


# --------------------------
# 3. 构建二维网格（UTC1000 × PM2.5）
# --------------------------
# 3.1 设定两个变量的取值范围（基于原始数据，避免外推过多）
# UTC1000：取原始数据的5%~95%分位数（排除极端值）

UTC_grid <- seq(0, 1, length.out = 1000)  # 100个点

# PM2.5：取原始数据的0%~95%分位数（避免极端污染值）
Surface_pressure_range <- quantile(query_sample2$Surface_pressure, probs = c(0, 1), na.rm = TRUE)
Surface_pressure_grid <- seq(Surface_pressure_range[1], Surface_pressure_range[2], length.out = 1000)  # 100个点

# 3.2 生成二维网格（100×100=10000个组合）
grid_data_Surface_pressure <- expand.grid(
  UTC1000 = UTC_grid,
  Surface_pressure = Surface_pressure_grid
)


# --------------------------
# 5. 计算二维网格中每个点的边际效应（分疫情阶段）
# --------------------------
# 5.1 疫情前边际效应（covidbefore=1）
grid_data_Surface_pressure$marginal_pre <- alpha1 + 2 * alpha2 * grid_data_Surface_pressure$UTC1000 + alpha3 * grid_data_Surface_pressure$Surface_pressure

# 5.2 疫情期间边际效应（covidduring=1）
grid_data_Surface_pressure$marginal_during <- alpha1 + 2 * alpha2 * grid_data_Surface_pressure$UTC1000 + alpha4 * grid_data_Surface_pressure$Surface_pressure


# --------------------------
# 6. （可选）计算边际效应的标准误（误差传播）
# --------------------------
# 构建权重矩阵（每行对应一个网格点的权重）
grid_data_Surface_pressure <- grid_data_Surface_pressure %>%
  mutate(
    # 疫情前权重：[beta1, beta2, alpha3, alpha4]的权重
    w1_pre = 1,
    w2_pre = 2 * UTC1000,
    w3_pre = Surface_pressure,
    w4_pre = 0,
    # 疫情期间权重
    w1_during = 1,
    w2_during = 2 * UTC1000,
    w3_during = 0,
    w4_during = Surface_pressure
  )

# 计算疫情前标准误
se_pre <- apply(grid_data_Surface_pressure[, c("w1_pre", "w2_pre", "w3_pre", "w4_pre")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_Surface_pressure$se_pre <- se_pre

# 计算疫情期间标准误
se_during <- apply(grid_data_Surface_pressure[, c("w1_during", "w2_during", "w3_during", "w4_during")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_Surface_pressure$se_during <- se_during


# --------------------------
# 5. 计算95%置信区间
# --------------------------
t_low <- qt(0.025, df = df)
t_high <- qt(0.975, df = df)


grid_data_Surface_pressure$low_pre<-grid_data_Surface_pressure$marginal_pre + grid_data_Surface_pressure$se_pre*t_low
grid_data_Surface_pressure$up_pre<-grid_data_Surface_pressure$marginal_pre + grid_data_Surface_pressure$se_pre*t_high

grid_data_Surface_pressure$low_during<-grid_data_Surface_pressure$marginal_during + grid_data_Surface_pressure$se_during*t_low
grid_data_Surface_pressure$up_during<-grid_data_Surface_pressure$marginal_during + grid_data_Surface_pressure$se_during*t_high


############################################################

# 假设要获取"col1"、"col3"、"col5"这三列


grid_data_Surface_pressure$Surface_pressure<- grid_data_Surface_pressure$Surface_pressure+mean_Surface_pressure  ## 对去中心化的值进行还原
UTC_Surface_pressure_grid_before_during<- grid_data_Surface_pressure[, c("UTC1000","Surface_pressure", "marginal_pre", "low_pre", "up_pre","marginal_during", "low_during", "up_during")]
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1000/',  'UTC_Surface_pressure_grid_before_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_Surface_pressure_grid_before_during, save_file_path_before)

################# 获取等于mean_UTC1000的数值位置 85##################

target_UTC1000 <- grid_data_Surface_pressure$UTC1000[85]### 选择固定阈值处的数据
# 筛选出UTC等于该值的所有行
sub_UTC_Surface_pressure_grid_before_during <- subset(UTC_Surface_pressure_grid_before_during, UTC1000 == target_UTC1000)

## before
UTC_Surface_pressure_grid_before<- sub_UTC_Surface_pressure_grid_before_during[, c("UTC1000","Surface_pressure", "marginal_pre", "low_pre","up_pre")]
colnames(UTC_Surface_pressure_grid_before) <- c("UTC1000","Surface_pressure", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1000/',  'UTC_Surface_pressure_meanUTC_before', '.csv', sep="")  # 字符串连接
write.csv(UTC_Surface_pressure_grid_before, save_file_path_before)

## during 
UTC_Surface_pressure_grid_during<- sub_UTC_Surface_pressure_grid_before_during[, c("UTC1000","Surface_pressure", "marginal_during", "low_during","up_during")]
colnames(UTC_Surface_pressure_grid_during) <- c("UTC1000","Surface_pressure", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1000/',  'UTC_Surface_pressure_meanUTC_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_Surface_pressure_grid_during, save_file_path_before)


###################

####################################UTC与疫情和 population1000 的交互##############################

model1000_interaction5 <- felm(sentiment ~ 1 + UTC1000+UTC1000_2+ UTC1000:population1000:covid + NOUTC1000+PM25+
                                 humidity +Precipitation + Wind + cloudcover + Surface_pressure+Skintemperature +
                                 population1000+nightlight1000+ settlement1000+GDP
                               | yearmonthday + userid , 
                               data = query_sample2)

print(summary(model1000_interaction5))

#####################保存模型参数#######################
# 捕获第n个模型摘要的输出
model1000_summary <- capture.output(summary(model1000_interaction5))
# 将第n个模型的输出追加到同一个txt文件中
cat(model1000_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)


##################################模型协方差计算#################################


# 提取模型系数
coeffs <-coef(model1000_interaction5) # 获取系数

##calculate and save marginal effects, with errors clustered regionally
cov <- vcovCR(model1000_interaction5,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解


####################before COVID19 #######################
####################before COVID19 边界效应 #######################

# --------------------------
# 2. 提取模型参数（同前）
# --------------------------
alpha1 <- coeffs["UTC1000"]                  # -8.0401
alpha2 <- coeffs["UTC1000_2"]                # 9.5740
alpha3 <- coeffs["UTC1000:population1000:covidbefore"] # 0.0012
alpha4 <- coeffs["UTC1000:population1000:covidduring"] # 0.0106

# 协方差矩阵（用于标准误，可选，若仅可视化效应值可省略）
var_names <- c("UTC1000", "UTC1000_2", "UTC1000:population1000:covidbefore", "UTC1000:population1000:covidduring")
V <- cov[var_names, var_names] 

# 模型自由度（用于置信区间，可选）
df <- df.residual(model1000_interaction5)


# --------------------------
# 3. 构建二维网格（UTC1000 × PM2.5）
# --------------------------
# 3.1 设定两个变量的取值范围（基于原始数据，避免外推过多）
# UTC1000：取原始数据的5%~95%分位数（排除极端值）

UTC_grid <- seq(0, 1, length.out = 1000)  # 100个点

# PM2.5：取原始数据的0%~95%分位数（避免极端污染值）
population1000_range <- quantile(query_sample2$population1000, probs = c(0, 1), na.rm = TRUE)
population1000_grid <- seq(population1000_range[1], population1000_range[2], length.out = 1000)  # 100个点

# 3.2 生成二维网格（100×100=10000个组合）
grid_data_population1000 <- expand.grid(
  UTC1000 = UTC_grid,
  population1000 = population1000_grid
)


# --------------------------
# 5. 计算二维网格中每个点的边际效应（分疫情阶段）
# --------------------------
# 5.1 疫情前边际效应（covidbefore=1）
grid_data_population1000$marginal_pre <- alpha1 + 2 * alpha2 * grid_data_population1000$UTC1000 + alpha3 * grid_data_population1000$population1000

# 5.2 疫情期间边际效应（covidduring=1）
grid_data_population1000$marginal_during <- alpha1 + 2 * alpha2 * grid_data_population1000$UTC1000 + alpha4 * grid_data_population1000$population1000


# --------------------------
# 6. （可选）计算边际效应的标准误（误差传播）
# --------------------------
# 构建权重矩阵（每行对应一个网格点的权重）
grid_data_population1000 <- grid_data_population1000 %>%
  mutate(
    # 疫情前权重：[beta1, beta2, alpha3, alpha4]的权重
    w1_pre = 1,
    w2_pre = 2 * UTC1000,
    w3_pre = population1000,
    w4_pre = 0,
    # 疫情期间权重
    w1_during = 1,
    w2_during = 2 * UTC1000,
    w3_during = 0,
    w4_during = population1000
  )

# 计算疫情前标准误
se_pre <- apply(grid_data_population1000[, c("w1_pre", "w2_pre", "w3_pre", "w4_pre")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_population1000$se_pre <- se_pre

# 计算疫情期间标准误
se_during <- apply(grid_data_population1000[, c("w1_during", "w2_during", "w3_during", "w4_during")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_population1000$se_during <- se_during


# --------------------------
# 5. 计算95%置信区间
# --------------------------
t_low <- qt(0.025, df = df)
t_high <- qt(0.975, df = df)


grid_data_population1000$low_pre<-grid_data_population1000$marginal_pre + grid_data_population1000$se_pre*t_low
grid_data_population1000$up_pre<-grid_data_population1000$marginal_pre + grid_data_population1000$se_pre*t_high

grid_data_population1000$low_during<-grid_data_population1000$marginal_during + grid_data_population1000$se_during*t_low
grid_data_population1000$up_during<-grid_data_population1000$marginal_during + grid_data_population1000$se_during*t_high


############################################################

# 假设要获取"col1"、"col3"、"col5"这三列

grid_data_population1000$population1000<- grid_data_population1000$population1000+mean_population1000  ## 对去中心化的值进行还原
UTC_population1000_grid_before_during<- grid_data_population1000[, c("UTC1000","population1000", "marginal_pre", "low_pre", "up_pre","marginal_during", "low_during", "up_during")]
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1000/',  'UTC_population1000_grid_before_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_population1000_grid_before_during, save_file_path_before)

################# 获取等于mean_UTC1000的数值位置 85##################

target_UTC1000 <- grid_data_population1000$UTC1000[85]### 选择固定阈值处的数据
# 筛选出UTC等于该值的所有行
sub_UTC_population1000_grid_before_during <- subset(UTC_population1000_grid_before_during, UTC1000 == target_UTC1000)

## before
UTC_population1000_grid_before<- sub_UTC_population1000_grid_before_during[, c("UTC1000","population1000", "marginal_pre", "low_pre","up_pre")]
colnames(UTC_population1000_grid_before) <- c("UTC1000","population1000", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1000/',  'UTC_population1000_meanUTC_before', '.csv', sep="")  # 字符串连接
write.csv(UTC_population1000_grid_before, save_file_path_before)

## during 
UTC_population1000_grid_during<- sub_UTC_population1000_grid_before_during[, c("UTC1000","population1000", "marginal_during", "low_during","up_during")]
colnames(UTC_population1000_grid_during) <- c("UTC1000","population1000", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1000/',  'UTC_population1000_meanUTC_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_population1000_grid_during, save_file_path_before)


###################


####################################UTC与疫情和 nightlight1000 的交互##############################

model1000_interaction6 <- felm(sentiment ~ 1 + UTC1000+UTC1000_2+ UTC1000:nightlight1000:covid + NOUTC1000+PM25+
                                 humidity +Precipitation + Wind + cloudcover + Surface_pressure+Skintemperature +
                                 nightlight1000+ population1000 + settlement1000+GDP
                               | yearmonthday + userid , 
                               data = query_sample2)

print(summary(model1000_interaction6))

#####################保存模型参数#######################
# 捕获第n个模型摘要的输出
model1000_summary <- capture.output(summary(model1000_interaction6))
# 将第n个模型的输出追加到同一个txt文件中
cat(model1000_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)


##################################模型协方差计算#################################


# 提取模型系数
coeffs <-coef(model1000_interaction6) # 获取系数

##calculate and save marginal effects, with errors clustered regionally
cov <- vcovCR(model1000_interaction6,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解


####################before COVID19 #######################
####################before COVID19 边界效应 #######################

# --------------------------
# 2. 提取模型参数（同前）
# --------------------------
alpha1 <- coeffs["UTC1000"]                  # -8.0401
alpha2 <- coeffs["UTC1000_2"]                # 9.5740
alpha3 <- coeffs["UTC1000:nightlight1000:covidbefore"] # 0.0012
alpha4 <- coeffs["UTC1000:nightlight1000:covidduring"] # 0.0106

# 协方差矩阵（用于标准误，可选，若仅可视化效应值可省略）
var_names <- c("UTC1000", "UTC1000_2", "UTC1000:nightlight1000:covidbefore", "UTC1000:nightlight1000:covidduring")
V <- cov[var_names, var_names] 

# 模型自由度（用于置信区间，可选）
df <- df.residual(model1000_interaction6)


# --------------------------
# 3. 构建二维网格（UTC1000 × PM2.5）
# --------------------------
# 3.1 设定两个变量的取值范围（基于原始数据，避免外推过多）
# UTC1000：取原始数据的5%~95%分位数（排除极端值）

UTC_grid <- seq(0, 1, length.out = 1000)  # 100个点

# PM2.5：取原始数据的0%~95%分位数（避免极端污染值）
nightlight1000_range <- quantile(query_sample2$nightlight1000, probs = c(0, 1), na.rm = TRUE)
nightlight1000_grid <- seq(nightlight1000_range[1], nightlight1000_range[2], length.out = 1000)  # 100个点

# 3.2 生成二维网格（100×100=10000个组合）
grid_data_nightlight1000 <- expand.grid(
  UTC1000 = UTC_grid,
  nightlight1000 = nightlight1000_grid
)


# --------------------------
# 5. 计算二维网格中每个点的边际效应（分疫情阶段）
# --------------------------
# 5.1 疫情前边际效应（covidbefore=1）
grid_data_nightlight1000$marginal_pre <- alpha1 + 2 * alpha2 * grid_data_nightlight1000$UTC1000 + alpha3 * grid_data_nightlight1000$nightlight1000

# 5.2 疫情期间边际效应（covidduring=1）
grid_data_nightlight1000$marginal_during <- alpha1 + 2 * alpha2 * grid_data_nightlight1000$UTC1000 + alpha4 * grid_data_nightlight1000$nightlight1000


# --------------------------
# 6. （可选）计算边际效应的标准误（误差传播）
# --------------------------
# 构建权重矩阵（每行对应一个网格点的权重）
grid_data_nightlight1000 <- grid_data_nightlight1000 %>%
  mutate(
    # 疫情前权重：[beta1, beta2, alpha3, alpha4]的权重
    w1_pre = 1,
    w2_pre = 2 * UTC1000,
    w3_pre = nightlight1000,
    w4_pre = 0,
    # 疫情期间权重
    w1_during = 1,
    w2_during = 2 * UTC1000,
    w3_during = 0,
    w4_during = nightlight1000
  )

# 计算疫情前标准误
se_pre <- apply(grid_data_nightlight1000[, c("w1_pre", "w2_pre", "w3_pre", "w4_pre")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_nightlight1000$se_pre <- se_pre

# 计算疫情期间标准误
se_during <- apply(grid_data_nightlight1000[, c("w1_during", "w2_during", "w3_during", "w4_during")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_nightlight1000$se_during <- se_during


# --------------------------
# 5. 计算95%置信区间
# --------------------------
t_low <- qt(0.025, df = df)
t_high <- qt(0.975, df = df)


grid_data_nightlight1000$low_pre<-grid_data_nightlight1000$marginal_pre + grid_data_nightlight1000$se_pre*t_low
grid_data_nightlight1000$up_pre<-grid_data_nightlight1000$marginal_pre + grid_data_nightlight1000$se_pre*t_high

grid_data_nightlight1000$low_during<-grid_data_nightlight1000$marginal_during + grid_data_nightlight1000$se_during*t_low
grid_data_nightlight1000$up_during<-grid_data_nightlight1000$marginal_during + grid_data_nightlight1000$se_during*t_high


############################################################

# 假设要获取"col1"、"col3"、"col5"这三列

grid_data_nightlight1000$nightlight1000<- grid_data_nightlight1000$nightlight1000+mean_nightlight1000  ## 对去中心化的值进行还原
UTC_nightlight1000_grid_before_during<- grid_data_nightlight1000[, c("UTC1000","nightlight1000", "marginal_pre", "low_pre", "up_pre","marginal_during", "low_during", "up_during")]
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1000/',  'UTC_nightlight1000_grid_before_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_nightlight1000_grid_before_during, save_file_path_before)

################# 获取等于mean_UTC1000的数值位置 85##################

target_UTC1000 <- grid_data_nightlight1000$UTC1000[85]### 选择固定阈值处的数据
# 筛选出UTC等于该值的所有行
sub_UTC_nightlight1000_grid_before_during <- subset(UTC_nightlight1000_grid_before_during, UTC1000 == target_UTC1000)

## before
UTC_nightlight1000_grid_before<- sub_UTC_nightlight1000_grid_before_during[, c("UTC1000","nightlight1000", "marginal_pre", "low_pre","up_pre")]
colnames(UTC_nightlight1000_grid_before) <- c("UTC1000","nightlight1000", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1000/',  'UTC_nightlight1000_meanUTC_before', '.csv', sep="")  # 字符串连接
write.csv(UTC_nightlight1000_grid_before, save_file_path_before)

## during 
UTC_nightlight1000_grid_during<- sub_UTC_nightlight1000_grid_before_during[, c("UTC1000","nightlight1000", "marginal_during", "low_during","up_during")]
colnames(UTC_nightlight1000_grid_during) <- c("UTC1000","nightlight1000", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1000/',  'UTC_nightlight1000_meanUTC_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_nightlight1000_grid_during, save_file_path_before)


###################



####################################UTC与疫情和 settlement1000 的交互##############################

model1000_interaction7 <- felm(sentiment ~ 1 + UTC1000+UTC1000_2+ UTC1000:settlement1000:covid + NOUTC1000+PM25+
                                 humidity +Precipitation + Wind + cloudcover + Surface_pressure+Skintemperature +settlement1000+
                                 population1000 + nightlight1000+GDP
                               | yearmonthday + userid , 
                               data = query_sample2)

print(summary(model1000_interaction7))

#####################保存模型参数#######################
# 捕获第n个模型摘要的输出
model1000_summary <- capture.output(summary(model1000_interaction7))
# 将第n个模型的输出追加到同一个txt文件中
cat(model1000_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)


##################################模型协方差计算#################################


# 提取模型系数
coeffs <-coef(model1000_interaction7) # 获取系数

##calculate and save marginal effects, with errors clustered regionally
cov <- vcovCR(model1000_interaction7,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解


####################before COVID19 #######################
####################before COVID19 边界效应 #######################

# --------------------------
# 2. 提取模型参数（同前）
# --------------------------
alpha1 <- coeffs["UTC1000"]                  # -8.0401
alpha2 <- coeffs["UTC1000_2"]                # 9.5740
alpha3 <- coeffs["UTC1000:settlement1000:covidbefore"] # 0.0012
alpha4 <- coeffs["UTC1000:settlement1000:covidduring"] # 0.0106

# 协方差矩阵（用于标准误，可选，若仅可视化效应值可省略）
var_names <- c("UTC1000", "UTC1000_2", "UTC1000:settlement1000:covidbefore", "UTC1000:settlement1000:covidduring")
V <- cov[var_names, var_names] 

# 模型自由度（用于置信区间，可选）
df <- df.residual(model1000_interaction7)


# --------------------------
# 3. 构建二维网格（UTC1000 × PM2.5）
# --------------------------
# 3.1 设定两个变量的取值范围（基于原始数据，避免外推过多）
# UTC1000：取原始数据的5%~95%分位数（排除极端值）

UTC_grid <- seq(0, 1, length.out = 1000)  # 100个点

# PM2.5：取原始数据的0%~95%分位数（避免极端污染值）
settlement1000_range <- quantile(query_sample2$settlement1000, probs = c(0, 1), na.rm = TRUE)
settlement1000_grid <- seq(settlement1000_range[1], settlement1000_range[2], length.out = 1000)  # 100个点

# 3.2 生成二维网格（100×100=10000个组合）
grid_data_settlement1000 <- expand.grid(
  UTC1000 = UTC_grid,
  settlement1000 = settlement1000_grid
)


# --------------------------
# 5. 计算二维网格中每个点的边际效应（分疫情阶段）
# --------------------------
# 5.1 疫情前边际效应（covidbefore=1）
grid_data_settlement1000$marginal_pre <- alpha1 + 2 * alpha2 * grid_data_settlement1000$UTC1000 + alpha3 * grid_data_settlement1000$settlement1000

# 5.2 疫情期间边际效应（covidduring=1）
grid_data_settlement1000$marginal_during <- alpha1 + 2 * alpha2 * grid_data_settlement1000$UTC1000 + alpha4 * grid_data_settlement1000$settlement1000


# --------------------------
# 6. （可选）计算边际效应的标准误（误差传播）
# --------------------------
# 构建权重矩阵（每行对应一个网格点的权重）
grid_data_settlement1000 <- grid_data_settlement1000 %>%
  mutate(
    # 疫情前权重：[beta1, beta2, alpha3, alpha4]的权重
    w1_pre = 1,
    w2_pre = 2 * UTC1000,
    w3_pre = settlement1000,
    w4_pre = 0,
    # 疫情期间权重
    w1_during = 1,
    w2_during = 2 * UTC1000,
    w3_during = 0,
    w4_during = settlement1000
  )

# 计算疫情前标准误
se_pre <- apply(grid_data_settlement1000[, c("w1_pre", "w2_pre", "w3_pre", "w4_pre")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_settlement1000$se_pre <- se_pre

# 计算疫情期间标准误
se_during <- apply(grid_data_settlement1000[, c("w1_during", "w2_during", "w3_during", "w4_during")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_settlement1000$se_during <- se_during


# --------------------------
# 5. 计算95%置信区间
# --------------------------
t_low <- qt(0.025, df = df)
t_high <- qt(0.975, df = df)


grid_data_settlement1000$low_pre<-grid_data_settlement1000$marginal_pre + grid_data_settlement1000$se_pre*t_low
grid_data_settlement1000$up_pre<-grid_data_settlement1000$marginal_pre + grid_data_settlement1000$se_pre*t_high

grid_data_settlement1000$low_during<-grid_data_settlement1000$marginal_during + grid_data_settlement1000$se_during*t_low
grid_data_settlement1000$up_during<-grid_data_settlement1000$marginal_during + grid_data_settlement1000$se_during*t_high


############################################################

# 假设要获取"col1"、"col3"、"col5"这三列

grid_data_settlement1000$settlement1000<- grid_data_settlement1000$settlement1000+mean_settlement1000  ## 对去中心化的值进行还原
UTC_settlement1000_grid_before_during<- grid_data_settlement1000[, c("UTC1000","settlement1000", "marginal_pre", "low_pre", "up_pre","marginal_during", "low_during", "up_during")]
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1000/',  'UTC_settlement1000_grid_before_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_settlement1000_grid_before_during, save_file_path_before)

################# 获取等于mean_UTC1000的数值位置 85##################

target_UTC1000 <- grid_data_settlement1000$UTC1000[85]### 选择固定阈值处的数据
# 筛选出UTC等于该值的所有行
sub_UTC_settlement1000_grid_before_during <- subset(UTC_settlement1000_grid_before_during, UTC1000 == target_UTC1000)

## before
UTC_settlement1000_grid_before<- sub_UTC_settlement1000_grid_before_during[, c("UTC1000","settlement1000", "marginal_pre", "low_pre","up_pre")]
colnames(UTC_settlement1000_grid_before) <- c("UTC1000","settlement1000", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1000/',  'UTC_settlement1000_meanUTC_before', '.csv', sep="")  # 字符串连接
write.csv(UTC_settlement1000_grid_before, save_file_path_before)

## during 
UTC_settlement1000_grid_during<- sub_UTC_settlement1000_grid_before_during[, c("UTC1000","settlement1000", "marginal_during", "low_during","up_during")]
colnames(UTC_settlement1000_grid_during) <- c("UTC1000","settlement1000", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1000/',  'UTC_settlement1000_meanUTC_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_settlement1000_grid_during, save_file_path_before)


###################



####################################UTC与疫情和 GDP 的交互##############################

model1000_interaction9 <- felm(sentiment ~ 1 + UTC1000+UTC1000_2+ UTC1000:GDP:covid + NOUTC1000+PM25+
                                 humidity + Precipitation + Wind + cloudcover + + Surface_pressure+Skintemperature +
                                 population1000 + nightlight1000+ settlement1000
                               | yearmonthday + userid , 
                               data = query_sample2)

print(summary(model1000_interaction9))

#####################保存模型参数#######################
# 捕获第n个模型摘要的输出
model1000_summary <- capture.output(summary(model1000_interaction9))
# 将第n个模型的输出追加到同一个txt文件中
cat(model1000_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)


##################################模型协方差计算#################################


# 提取模型系数
coeffs <-coef(model1000_interaction9) # 获取系数

##calculate and save marginal effects, with errors clustered regionally
cov <- vcovCR(model1000_interaction9,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解


####################before COVID19 #######################
####################before COVID19 边界效应 #######################

# --------------------------
# 2. 提取模型参数（同前）
# --------------------------
alpha1 <- coeffs["UTC1000"]                  # -8.0401
alpha2 <- coeffs["UTC1000_2"]                # 9.5740
alpha3 <- coeffs["UTC1000:GDP:covidbefore"] # 0.0012
alpha4 <- coeffs["UTC1000:GDP:covidduring"] # 0.0106

# 协方差矩阵（用于标准误，可选，若仅可视化效应值可省略）
var_names <- c("UTC1000", "UTC1000_2", "UTC1000:GDP:covidbefore", "UTC1000:GDP:covidduring")
V <- cov[var_names, var_names] 

# 模型自由度（用于置信区间，可选）
df <- df.residual(model1000_interaction9)


# --------------------------
# 3. 构建二维网格（UTC1000 × PM2.5）
# --------------------------
# 3.1 设定两个变量的取值范围（基于原始数据，避免外推过多）
# UTC1000：取原始数据的5%~95%分位数（排除极端值）

UTC_grid <- seq(0, 1, length.out = 1000)  # 100个点

# PM2.5：取原始数据的0%~95%分位数（避免极端污染值）
GDP_range <- quantile(query_sample2$GDP, probs = c(0, 1), na.rm = TRUE)
GDP_grid <- seq(GDP_range[1], GDP_range[2], length.out = 1000)  # 100个点

# 3.2 生成二维网格（100×100=10000个组合）
grid_data_GDP <- expand.grid(
  UTC1000 = UTC_grid,
  GDP = GDP_grid
)


# --------------------------
# 5. 计算二维网格中每个点的边际效应（分疫情阶段）
# --------------------------
# 5.1 疫情前边际效应（covidbefore=1）
grid_data_GDP$marginal_pre <- alpha1 + 2 * alpha2 * grid_data_GDP$UTC1000 + alpha3 * grid_data_GDP$GDP

# 5.2 疫情期间边际效应（covidduring=1）
grid_data_GDP$marginal_during <- alpha1 + 2 * alpha2 * grid_data_GDP$UTC1000 + alpha4 * grid_data_GDP$GDP


# --------------------------
# 6. （可选）计算边际效应的标准误（误差传播）
# --------------------------
# 构建权重矩阵（每行对应一个网格点的权重）
grid_data_GDP <- grid_data_GDP %>%
  mutate(
    # 疫情前权重：[beta1, beta2, alpha3, alpha4]的权重
    w1_pre = 1,
    w2_pre = 2 * UTC1000,
    w3_pre = GDP,
    w4_pre = 0,
    # 疫情期间权重
    w1_during = 1,
    w2_during = 2 * UTC1000,
    w3_during = 0,
    w4_during = GDP
  )

# 计算疫情前标准误
se_pre <- apply(grid_data_GDP[, c("w1_pre", "w2_pre", "w3_pre", "w4_pre")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_GDP$se_pre <- se_pre

# 计算疫情期间标准误
se_during <- apply(grid_data_GDP[, c("w1_during", "w2_during", "w3_during", "w4_during")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_GDP$se_during <- se_during


# --------------------------
# 5. 计算95%置信区间
# --------------------------
t_low <- qt(0.025, df = df)
t_high <- qt(0.975, df = df)


grid_data_GDP$low_pre<-grid_data_GDP$marginal_pre + grid_data_GDP$se_pre*t_low
grid_data_GDP$up_pre<-grid_data_GDP$marginal_pre + grid_data_GDP$se_pre*t_high

grid_data_GDP$low_during<-grid_data_GDP$marginal_during + grid_data_GDP$se_during*t_low
grid_data_GDP$up_during<-grid_data_GDP$marginal_during + grid_data_GDP$se_during*t_high


############################################################

# 假设要获取"col1"、"col3"、"col5"这三列
##grid_data_GDP$GDP<- grid_data_GDP$GDP +mean_GDP ## 对去中心化的值进行还原
UTC_GDP_grid_before_during<- grid_data_GDP[, c("UTC1000","GDP", "marginal_pre", "low_pre", "up_pre","marginal_during", "low_during", "up_during")]
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1000/',  'UTC_GDP_grid_before_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_GDP_grid_before_during, save_file_path_before)

################# 获取等于mean_UTC1000的数值位置 85##################

target_UTC1000 <- grid_data_GDP$UTC1000[85]### 选择固定阈值处的数据
# 筛选出UTC等于该值的所有行
sub_UTC_GDP_grid_before_during <- subset(UTC_GDP_grid_before_during, UTC1000 == target_UTC1000)

## before
UTC_GDP_grid_before<- sub_UTC_GDP_grid_before_during[, c("UTC1000","GDP", "marginal_pre", "low_pre","up_pre")]
colnames(UTC_GDP_grid_before) <- c("UTC1000","GDP", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1000/',  'UTC_GDP_meanUTC_before', '.csv', sep="")  # 字符串连接
write.csv(UTC_GDP_grid_before, save_file_path_before)

## during 
UTC_GDP_grid_during<- sub_UTC_GDP_grid_before_during[, c("UTC1000","GDP", "marginal_during", "low_during","up_during")]
colnames(UTC_GDP_grid_during) <- c("UTC1000","GDP", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1000/',  'UTC_GDP_meanUTC_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_GDP_grid_during, save_file_path_before)


###################



####################################UTC与疫情和温度 NOUTC1000 的交互##############################

model1000_interaction10 <- felm(sentiment ~ 1 + UTC1000+UTC1000_2+ UTC1000:NOUTC1000:covid +NOUTC1000 +PM25+
                                  humidity +Precipitation + Wind + cloudcover + + Surface_pressure+Skintemperature +
                                  population1000 + nightlight1000+ settlement1000+GDP
                                | yearmonthday + userid , 
                                data = query_sample2)

print(summary(model1000_interaction10))

#####################保存模型参数#######################
# 捕获第n个模型摘要的输出
model1000_summary <- capture.output(summary(model1000_interaction10))
# 将第n个模型的输出追加到同一个txt文件中
cat(model1000_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)


##################################模型协方差计算#################################


# 提取模型系数
coeffs <-coef(model1000_interaction10) # 获取系数

##calculate and save marginal effects, with errors clustered regionally
cov <- vcovCR(model1000_interaction10,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解


####################before COVID19 #######################
####################before COVID19 边界效应 #######################

# --------------------------
# 2. 提取模型参数（同前）
# --------------------------
alpha1 <- coeffs["UTC1000"]                  # -8.0401
alpha2 <- coeffs["UTC1000_2"]                # 9.5740
alpha3 <- coeffs["UTC1000:NOUTC1000:covidbefore"] # 0.0012
alpha4 <- coeffs["UTC1000:NOUTC1000:covidduring"] # 0.0106

# 协方差矩阵（用于标准误，可选，若仅可视化效应值可省略）
var_names <- c("UTC1000", "UTC1000_2", "UTC1000:NOUTC1000:covidbefore", "UTC1000:NOUTC1000:covidduring")
V <- cov[var_names, var_names] 

# 模型自由度（用于置信区间，可选）
df <- df.residual(model1000_interaction10)


# --------------------------
# 3. 构建二维网格（UTC1000 × PM2.5）
# --------------------------
# 3.1 设定两个变量的取值范围（基于原始数据，避免外推过多）
# UTC1000：取原始数据的5%~95%分位数（排除极端值）

UTC_grid <- seq(0, 1, length.out = 1000)  # 100个点

# PM2.5：取原始数据的0%~95%分位数（避免极端污染值）
NOUTC1000_range <- quantile(query_sample2$NOUTC1000, probs = c(0, 1), na.rm = TRUE)
NOUTC1000_grid <- seq(NOUTC1000_range[1], NOUTC1000_range[2], length.out = 1000)  # 100个点

# 3.2 生成二维网格（100×100=10000个组合）
grid_data_NOUTC1000 <- expand.grid(
  UTC1000 = UTC_grid,
  NOUTC1000 = NOUTC1000_grid
)


# --------------------------
# 5. 计算二维网格中每个点的边际效应（分疫情阶段）
# --------------------------
# 5.1 疫情前边际效应（covidbefore=1）
grid_data_NOUTC1000$marginal_pre <- alpha1 + 2 * alpha2 * grid_data_NOUTC1000$UTC1000 + alpha3 * grid_data_NOUTC1000$NOUTC1000

# 5.2 疫情期间边际效应（covidduring=1）
grid_data_NOUTC1000$marginal_during <- alpha1 + 2 * alpha2 * grid_data_NOUTC1000$UTC1000 + alpha4 * grid_data_NOUTC1000$NOUTC1000


# --------------------------
# 6. （可选）计算边际效应的标准误（误差传播）
# --------------------------
# 构建权重矩阵（每行对应一个网格点的权重）
grid_data_NOUTC1000 <- grid_data_NOUTC1000 %>%
  mutate(
    # 疫情前权重：[beta1, beta2, alpha3, alpha4]的权重
    w1_pre = 1,
    w2_pre = 2 * UTC1000,
    w3_pre = NOUTC1000,
    w4_pre = 0,
    # 疫情期间权重
    w1_during = 1,
    w2_during = 2 * UTC1000,
    w3_during = 0,
    w4_during = NOUTC1000
  )

# 计算疫情前标准误
se_pre <- apply(grid_data_NOUTC1000[, c("w1_pre", "w2_pre", "w3_pre", "w4_pre")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_NOUTC1000$se_pre <- se_pre

# 计算疫情期间标准误
se_during <- apply(grid_data_NOUTC1000[, c("w1_during", "w2_during", "w3_during", "w4_during")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_NOUTC1000$se_during <- se_during


# --------------------------
# 5. 计算95%置信区间
# --------------------------
t_low <- qt(0.025, df = df)
t_high <- qt(0.975, df = df)


grid_data_NOUTC1000$low_pre<-grid_data_NOUTC1000$marginal_pre + grid_data_NOUTC1000$se_pre*t_low
grid_data_NOUTC1000$up_pre<-grid_data_NOUTC1000$marginal_pre + grid_data_NOUTC1000$se_pre*t_high

grid_data_NOUTC1000$low_during<-grid_data_NOUTC1000$marginal_during + grid_data_NOUTC1000$se_during*t_low
grid_data_NOUTC1000$up_during<-grid_data_NOUTC1000$marginal_during + grid_data_NOUTC1000$se_during*t_high


############################################################

# 假设要获取"col1"、"col3"、"col5"这三列
grid_data_NOUTC1000$NOUTC1000<- grid_data_NOUTC1000$NOUTC1000+mean_NOUTC1000  ## 对去中心化的值进行还原
UTC_NOUTC1000_grid_before_during<- grid_data_NOUTC1000[, c("UTC1000","NOUTC1000", "marginal_pre", "low_pre", "up_pre","marginal_during", "low_during", "up_during")]
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1000/',  'UTC_NOUTC1000_grid_before_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_NOUTC1000_grid_before_during, save_file_path_before)

################# 获取等于mean_UTC1000的数值位置 85##################

target_UTC1000 <- grid_data_NOUTC1000$UTC1000[85]### 选择固定阈值处的数据
# 筛选出UTC等于该值的所有行
sub_UTC_NOUTC1000_grid_before_during <- subset(UTC_NOUTC1000_grid_before_during, UTC1000 == target_UTC1000)

## before
UTC_NOUTC1000_grid_before<- sub_UTC_NOUTC1000_grid_before_during[, c("UTC1000","NOUTC1000", "marginal_pre", "low_pre","up_pre")]
colnames(UTC_NOUTC1000_grid_before) <- c("UTC1000","NOUTC1000", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1000/',  'UTC_NOUTC1000_meanUTC_before', '.csv', sep="")  # 字符串连接
write.csv(UTC_NOUTC1000_grid_before, save_file_path_before)

## during 
UTC_NOUTC1000_grid_during<- sub_UTC_NOUTC1000_grid_before_during[, c("UTC1000","NOUTC1000", "marginal_during", "low_during","up_during")]
colnames(UTC_NOUTC1000_grid_during) <- c("UTC1000","NOUTC1000", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1000/',  'UTC_NOUTC1000_meanUTC_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_NOUTC1000_grid_during, save_file_path_before)


###################



############################分层回归分析######################################

### 分别查看 UTC1000 的系数在疫情前和疫情期间的差异

model1000_before <- felm(sentiment ~ 1 + UTC1000 +UTC1000_2+NOUTC1000+
                           Precipitation + humidity + Wind + cloudcover + Surface_pressure +Skintemperature+
                           population1000 + nightlight1000+ settlement1000+ GDP
                         | yearmonthday + userid , 
                         data = subset(query_sample2, covid == "before"))
print(summary(model1000_before))
# 捕获第n个模型摘要的输出
model1000_summary <- capture.output(summary(model1000_before))
# 将第n个模型的输出追加到同一个txt文件中
cat(model1000_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)
model1000_during <- felm(sentiment ~ 1 + UTC1000  +UTC1000_2 +NOUTC1000+
                           Precipitation + humidity + Wind + cloudcover + Surface_pressure +Skintemperature+
                           population1000 + nightlight1000+ settlement1000+ GDP
                         | yearmonthday + userid , 
                         data = subset(query_sample2, covid == "during"))

print(summary(model1000_during))

# 捕获第n个模型摘要的输出
model1000_summary <- capture.output(summary(model1000_during))
# 将第n个模型的输出追加到同一个txt文件中
cat(model1000_summary, file = output_file, sep = "\n", append = TRUE)

close(con)  # 关闭文件













############################################## UTC1000 导出基础模型结果为各种所需表格#############################################

text_code1 <- stargazer(model1000_1, model1000_2, model1000_3, model1000_interaction10,  
                        type = "text", title = " Baseline Model")
write(text_code1, file = "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/secondary_data_utc1000/Baseline_Results_tab_20250923.txt")


latex_code1 <- stargazer(model1000_1, model1000_2, model1000_3, model1000_interaction10,  
                         type = "latex", title = " Baseline Model")
write(latex_code1, file = "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/secondary_data_utc1000/Baseline_Results_tab_latex_20250923.tex")


text_code2 <- stargazer(model1000_before, model1000_during,  
                        type = "text", title = " Hierarchical regression")
write(text_code1, file = "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/secondary_data_utc1000/Hierarchical_Results_tab_20250923.txt")



latex_code2 <- stargazer(model1000_before, model1000_during,  
                         type = "latex", title = "Hierarchical regression")
write(latex_code1, file = "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/secondary_data_utc1000/Hierarchical_Results_tab_latex_20250923.tex")



########### 导出交互项模型结果为各种所需表格#############

text_code31 <- stargazer(model1000_interaction1, model1000_interaction2, model1000_interaction3, 
                         type = "text", title = "Interaction model")
write(text_code31, file = "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/secondary_data_utc1000/Interaction_Results1_tab_20250923.txt")


text_code311 <- stargazer(model1000_interaction4,
                          type = "text", title = "Interaction model")
write(text_code311, file = "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/secondary_data_utc1000/Interaction_Results1-1_tab_20250923.txt")


text_code32 <- stargazer(model1000_interaction5, model1000_interaction6, model1000_interaction7,
                         type = "text", title = "Interaction model")
write(text_code32, file = "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/secondary_data_utc1000/Interaction_Results2_tab_20250923.txt")


text_code322 <- stargazer( model1000_interaction9, 
                           type = "text", title = "Interaction model")
write(text_code322, file = "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/secondary_data_utc1000/Interaction_Results2-2_tab_20250923.txt")


text_code323 <- stargazer( model1000_interaction0, 
                           type = "text", title = "Interaction model")
write(text_code323, file = "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/secondary_data_utc1000/Interaction_Results2-3_tab_20250923.txt")


#########################################latex ########################

latex_code41 <- stargazer( model1000_interaction1, model1000_interaction2, model1000_interaction3,
                           type = "latex", title = "Interaction model")
write(latex_code41, file = "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/secondary_data_utc1000/Interaction_Results1_tab_latex_20250923.tex")


latex_code411 <- stargazer( model1000_interaction4,
                            type = "latex", title = "Interaction model")
write(latex_code41, file = "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/secondary_data_utc1000/Interaction_Results1-1_tab_latex_20250923.tex")


latex_code42 <- stargazer( model1000_interaction5, model1000_interaction6, model1000_interaction7, 
                           type = "latex", title = "Interaction model")
write(latex_code42, file = "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/secondary_data_utc1000/Interaction_Results2_tab_latex_20250923.tex")

text_code422 <- stargazer( model1000_interaction9, 
                           type = "latex", title = "Interaction model")
write(text_code422, file = "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/secondary_data_utc1000/Interaction_Results2-2_tab_latex_20250923.txt")

text_code423 <- stargazer( model1000_interaction0, 
                           type = "latex", title = "Interaction model")
write(text_code423, file = "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/secondary_data_utc1000/Interaction_Results2-3_tab_latex_20250923.txt")





rm(model1000_1, model1000_2, model1000_3, model1000_interaction10,
   model1000_before, model1000_during,model1000_interaction0,
   model1000_interaction5, model1000_interaction6, model1000_interaction7,
   model1000_interaction9)











#################################################UTC1500 ##############################################################




#########################一次回归模型训练 ################################
model1500_1 <- felm(sentiment ~ 1+ UTC1500
                    |yearmonthday+userid,  data = query_sample2)


# 查看模型摘要
print(summary(model1500_1))

# 指定输出文件的路径和名称
output_file <- "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/ResultsUTC1500_fix_indivdual_city_day_20250923.txt"
con <- file(output_file, open = "a")# 打开文件连接，追加模式
writeLines("\n--- New Model Summary ---\n", con)# 写入一个分隔符（可选），以便区分不同的模型摘要
model1500_summary <- capture.output(summary(model1500_1))# 捕获模型摘要的输出
writeLines(model1500_summary, con = output_file)# 将模型摘要追加写入文件
# 关闭文件连接 close(con)


####################################UTC 常规##############################
model1500_2 <- felm(sentiment ~ 1 + UTC1500 + UTC1500_2+ NOUTC1500+ PM25+
                      Precipitation + humidity + Wind + cloudcover + Surface_pressure+Skintemperature +
                      settlement1500+population1500+ nightlight1500+GDP
                    | yearmonthday + userid, 
                    data = query_sample2)
print(summary(model1500_2))

####################### 计算VIF############################
vif_values <- vif(model1500_2)

# 打印VIF值；VIF值大于10才被认为存在严重的共线性问题;增加样本量可能有助于减轻共线性问题
print("VIF:")
print(vif_values)
#####################保存模型参数#######################
# 捕获第n个模型摘要的输出
model1500_summary <- capture.output(summary(model1500_2))
# 将第n个模型的输出追加到同一个txt文件中
cat(model1500_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)




####################################二次项模型中UTC1500 和UTC1500_2 与疫情的交互##############################
## 存在二次项不显著 UTC1500_2:covidduring -1.7748092  1.5852590  -1.120 0.262897 

model1500_03 <- felm(sentiment ~ 1 + UTC1500 + UTC1500_2+ UTC1500_2:covid+ UTC1500:covid +  NOUTC1500+PM25+
                       Precipitation + humidity + Wind + cloudcover + Surface_pressure+Skintemperature +
                       settlement1500+population1500+ nightlight1500+GDP
                     | yearmonthday + userid, 
                     data = query_sample2)
print(summary(model1500_03))

#####################保存模型参数#######################
# 捕获第n个模型摘要的输出
model1500_summary <- capture.output(summary(model1500_03))
# 将第n个模型的输出追加到同一个txt文件中
cat(model1500_summary, file = output_file, sep = "\n", append = TRUE)


####################################一次项模型中UTC1500 疫情的交互##############################

model1500_3 <- felm(sentiment ~ 1 + UTC1500 + UTC1500_2+ UTC1500:covid +  NOUTC1500+PM25+
                      Precipitation + humidity + Wind + cloudcover + Surface_pressure+Skintemperature +
                      settlement1500+population1500+ nightlight1500+GDP
                    | yearmonthday + userid, 
                    data = query_sample2)
print(summary(model1500_3))

#####################保存模型参数#######################
# 捕获第n个模型摘要的输出
model1500_summary <- capture.output(summary(model1500_3))
# 将第n个模型的输出追加到同一个txt文件中
cat(model1500_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)


##################################模型协方差计算#################################
# 提取模型系数
coeffs <-coef(model1500_3) # 获取系数

##calculate and save marginal effects, with errors clustered regionally

cov<- vcovCR(model1500_3,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解
#cov_simple <- vcov(model1500_3)  # 协方差矩阵,



############################ UTC effects_before ###########################################

# 1. 提取UTC1500和UTC1500_2的方差-协方差矩阵（仅保留这两个变量）
# 确保仅包含目标变量的协方差，避免其他变量干扰
V <- cov[c("UTC1500", "UTC1500_2"), c("UTC1500", "UTC1500_2")]

# 2. 提取UTC1500和UTC1500_2的系数（确认UTC1500_2是UTC1500的平方项）
beta <- c(coeffs["UTC1500"], coeffs["UTC1500_2"])

# 3. 获取模型残差自由度（用于计算t分布分位数）
residual_df <- df.residual(model1500_3)

# 4. 构建UTC1500的取值范围（0到1之间生成200个点）
UTC <- seq(0, 1, length.out = 200)
# 构建设计矩阵（包含一次项和二次项）
X <- cbind(UTC, UTC^2)  # 确保与模型中的UTC1500和UTC1500_2对应

# 5. 计算平均效应
mean_effect <- X %*% beta 

# 6. 计算效应的标准误差
# 利用方差-协方差矩阵计算每个点的标准误
se <- sqrt(rowSums(X %*% V * X))

# 7. 计算95%置信区间（t分布）
# 注意：qt(0.025, df)为负值，因此下限=均值+se*负值，上限=均值-se*负值
ci_low <- mean_effect + se * qt(0.025, df = residual_df)
ci_high <- mean_effect - se * qt(0.025, df = residual_df)

# 8. 整理结果并保存
# 构建数据框（包含UTC值、平均效应及置信区间）
Data_effects_UTC1500_UTC_before <- data.frame(
  UTC = UTC,
  mean_value = as.vector(mean_effect),  # 转换为向量便于存储
  Low_value = as.vector(ci_low),
  UP_value = as.vector(ci_high)
)

save_file_path= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1500/',  'UTC_effects_before', '.csv', sep="")  # 字符串连接
write.csv(Data_effects_UTC1500_UTC_before,save_file_path)




############################ UTC effects during ###########################################


####################### 1. 提取疫情期间效应所需的系数与协方差 ##############################
# （1）提取关键系数：UTC1500主效应、UTC1500二次项、UTC1500:covidduring交互项
# 提取所有系数（明确命名便于后续调用）
beta1 <- coeffs["UTC1500"]          # UTC1500主效应系数（β₁）
beta2 <- coeffs["UTC1500_2"]        # UTC1500二次项系数（β₂）
beta3 <- coeffs["UTC1500:covidduring"]  # 交互项系数（β₃）

# （2）提取上述3个系数的协方差矩阵（含三者间的协方差，影响标准误计算）
# 协方差矩阵需包含：UTC1500、UTC1500_2、UTC1500:covidduring
# （2）提取上述3个系数的协方差矩阵（含三者间的协方差，影响标准误计算）
# 协方差矩阵需包含：UTC1500、UTC1500_2、UTC1500:covidduring

cov_matrix <- as.matrix(cov)  # 转换为矩阵格式，便于提取子集

V_original <- cov_matrix[
  rownames(cov_matrix) %in% c("UTC1500", "UTC1500_2", "UTC1500:covidduring"),
  colnames(cov_matrix) %in% c("UTC1500", "UTC1500_2", "UTC1500:covidduring")
]

# 重命名行/列，避免混淆
rownames(V_original) <- colnames(V_original) <- c("beta1", "beta2", "beta3")

####################### 2. 构建匹配维度的系数与协方差矩阵 ##############################
# （1）疫情期间的效应系数（2个元素，与X的列数匹配）
beta <- c(
  beta_linear = beta1 + beta3,  # 合并后的线性项系数（β₁+β₃）
  beta_quad = beta2             # 二次项系数（β₂）
)

# （2）构建2×2协方差矩阵（与beta、X维度匹配）
V <- matrix(
  nrow = 2, ncol = 2,
  dimnames = list(c("beta_linear", "beta_quad"), c("beta_linear", "beta_quad"))
)
# 填充协方差矩阵（基于方差运算规则）
V["beta_linear", "beta_linear"] <- V_original["beta1","beta1"] + V_original["beta3","beta3"] + 2*V_original["beta1","beta3"]  # Var(β₁+β₃)
V["beta_linear", "beta_quad"]   <- V_original["beta1","beta2"] + V_original["beta3","beta2"]  # Cov(β₁+β₃, β₂)
V["beta_quad", "beta_linear"]   <- V["beta_linear", "beta_quad"]  # 协方差矩阵对称
V["beta_quad", "beta_quad"]     <- V_original["beta2","beta2"]  # Var(β₂)




####################### 3. 计算疫情期间UTC1500的效应及置信区间 ##############################
# （1）构建UTC1500的取值范围（与疫情前一致，便于对比，0到1生成200个点）
UTC <- seq(0, 1, length.out = 200)

# （2）构建设计矩阵X：对应疫情期间的效应公式
# X第一列：UTC1500（对应线性项总系数 beta_linear）
# X第二列：UTC1500²（对应二次项系数 beta_quad）
X <- cbind(
  UTC = UTC,
  UTC_sq = UTC^2
)

# （3）计算疫情期间UTC1500的平均效应
mean_effect <- X %*% beta  # 矩阵乘法，得到每个UTC值对应的效应

# （4）计算效应的聚类稳健标准误（关键：考虑3个系数的协方差）
# 公式逻辑：se = sqrt( X × V × X' )，rowSums(X %*% V * X) 等价于对角线元素提取
se <- sqrt(rowSums(X %*% V * X))

# （5）计算95%置信区间（t分布，使用模型残差自由度）
residual_df <- df.residual(model1500_3)  # 模型输出中为 4256966
ci_low <- mean_effect + se * qt(0.025, df = residual_df)  # 下限
ci_high <- mean_effect - se * qt(0.025, df = residual_df)  # 上限


####################### 4. 整理结果并保存 ##############################
Data_effects_UTC1500_UTC_during <- data.frame(
  UTC = UTC,
  mean_value = as.vector(mean_effect),  # 转换为向量便于存储
  Low_value = as.vector(ci_low),            # 95%置信区间下限
  UP_value = as.vector(ci_high)           # 95%置信区间上限
)

#步骤 7:  数据保存
save_file_path= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1500/',  'UTC_effects_during', '.csv', sep="")  # 字符串连接
write.csv(Data_effects_UTC1500_UTC_during,save_file_path)




######################### marginal effects################################


######################### 1. 提取模型系数#####################
beta1 <- coeffs["UTC1500"]          # β_UTC1500
beta2 <- coeffs["UTC1500_2"]        # β_UTC1500_2
beta3 <- coeffs["UTC1500:covidduring"]  # β_UTC1500:covidduring

cov_matrix <- as.matrix(cov)  # 转换为矩阵格式，便于提取子集
# 提取聚类稳健协方差矩阵（3个关键系数）
V <- cov_matrix[
  c("UTC1500", "UTC1500_2", "UTC1500:covidduring"),
  c("UTC1500", "UTC1500_2", "UTC1500:covidduring")
]
colnames(V) <- rownames(V) <- c("beta1", "beta2", "beta3")  # 重命名便于引用


####################### 2. 构建UTC1500的取值范围 ##############################
UTC <- seq(0, 1, length.out = 200)  # 与之前一致的取值范围


####################### 3. 疫情前的边际效应 ##############################
# （1）边际效应值
me_before <- as.vector(beta1 + 2 * beta2 * UTC)

# （2）边际效应的标准误（基于方差性质）
# 方差 = Var(beta1) + (2*UTC)^2*Var(beta2) + 2*(2*UTC)*Cov(beta1, beta2)
var_me_before <- V["beta1","beta1"] + (2*UTC)^2 * V["beta2","beta2"] + 2*(2*UTC)*V["beta1","beta2"]
se_before <- sqrt(var_me_before)

# （3）95%置信区间
residual_df <- df.residual(model1500_3)
ci_low_before <- me_before + se_before * qt(0.025, df = residual_df)
ci_high_before <- me_before - se_before * qt(0.025, df = residual_df)


####################### 4. 疫情期间的边际效应 ##############################
# （1）边际效应值
me_during <- as.vector( (beta1 + beta3) + 2 * beta2 * UTC )

# （2）边际效应的标准误
# 方差 = Var(beta1+beta3) + (2*UTC)^2*Var(beta2) + 2*(2*UTC)*Cov(beta1+beta3, beta2)
# 其中：Var(beta1+beta3) = Var(beta1) + Var(beta3) + 2*Cov(beta1, beta3)
#       Cov(beta1+beta3, beta2) = Cov(beta1, beta2) + Cov(beta3, beta2)
var_me_during <- (V["beta1","beta1"] + V["beta3","beta3"] + 2*V["beta1","beta3"]) + 
  (2*UTC)^2 * V["beta2","beta2"] + 
  2*(2*UTC)*(V["beta1","beta2"] + V["beta3","beta2"])
se_during <- sqrt(var_me_during)

# （3）95%置信区间
ci_low_during <- me_during + se_during * qt(0.025, df = residual_df)
ci_high_during <- me_during - se_during * qt(0.025, df = residual_df)


####################### 5. 边际效应保存 ##############################
# 构建数据框,保存疫情前边界效应数据
marginaleffects_UTC1500_before <- data.frame(UTC1500 = UTC, mean_value = me_before, 
                                             UP_value = ci_high_before, Low_value = ci_low_before)
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1500/',  'UTC_UTC1500_before', '.csv', sep="")  # 字符串连接

write.csv(marginaleffects_UTC1500_before, save_file_path_before)


# 构建数据框,保存疫情期间边界效应数据

marginaleffects_UTC1500_during <- data.frame(UTC1500 = UTC, mean_value = me_during, 
                                             UP_value = ci_high_during, Low_value = ci_low_during)
save_file_path_during= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1500/',  'UTC_UTC1500_during', '.csv', sep="")  # 字符串连接

write.csv(marginaleffects_UTC1500_during, save_file_path_during)










####################################UTC与疫情和 PM25 的交互##############################

model1500_interaction0 <- felm(sentiment ~ 1 + UTC1500+UTC1500_2+ UTC1500:PM25:covid + NOUTC1500+ 
                                 Precipitation+humidity+ Wind + cloudcover + Surface_pressure+Skintemperature +
                                 population1500 + nightlight1500+ settlement1500+GDP
                               | yearmonthday + userid , 
                               data = query_sample2)

print(summary(model1500_interaction0))


#####################保存模型参数#######################
# 捕获第n个模型摘要的输出
model1500_summary <- capture.output(summary(model1500_interaction0))
# 将第n个模型的输出追加到同一个txt文件中
cat(model1500_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)

# 提取模型系数
coeffs <-coef(model1500_interaction0) # 获取系数

##calculate and save marginal effects, with errors clustered regionally
cov <- vcovCR(model1500_interaction0,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解


####################before COVID19 边界效应 #######################

# --------------------------
# 2. 提取模型参数（同前）
# --------------------------
alpha1 <- coeffs["UTC1500"]                  # -8.0401
alpha2 <- coeffs["UTC1500_2"]                # 9.5740
alpha3 <- coeffs["UTC1500:PM25:covidbefore"] # 0.0012
alpha4 <- coeffs["UTC1500:PM25:covidduring"] # 0.0106

# 协方差矩阵（用于标准误，可选，若仅可视化效应值可省略）
var_names <- c("UTC1500", "UTC1500_2", "UTC1500:PM25:covidbefore", "UTC1500:PM25:covidduring")
V <- cov[var_names, var_names] 

# 模型自由度（用于置信区间，可选）
df <- df.residual(model1500_interaction0)


# --------------------------
# 3. 构建二维网格（UTC1500 × PM2.5）
# --------------------------
# 3.1 设定两个变量的取值范围（基于原始数据，避免外推过多）
# UTC1500：取原始数据的5%~95%分位数（排除极端值）

UTC_grid <- seq(0, 1, length.out = 1500)  # 100个点

# PM2.5：取原始数据的0%~95%分位数（避免极端污染值）
pm25_range <- quantile(query_sample2$PM25, probs = c(0, 1), na.rm = TRUE)
PM25_grid <- seq(pm25_range[1], pm25_range[2], length.out = 1500)  # 100个点

# 3.2 生成二维网格（100×100=15000个组合）
grid_data_PM25 <- expand.grid(
  UTC1500 = UTC_grid,
  PM25 = PM25_grid
)


# --------------------------
# 5. 计算二维网格中每个点的边际效应（分疫情阶段）
# --------------------------
# 5.1 疫情前边际效应（covidbefore=1）
grid_data_PM25$marginal_pre <- alpha1 + 2 * alpha2 * grid_data_PM25$UTC1500 + alpha3 * grid_data_PM25$PM25

# 5.2 疫情期间边际效应（covidduring=1）
grid_data_PM25$marginal_during <- alpha1 + 2 * alpha2 * grid_data_PM25$UTC1500 + alpha4 * grid_data_PM25$PM25


# --------------------------
# 6. （可选）计算边际效应的标准误（误差传播）
# --------------------------
# 构建权重矩阵（每行对应一个网格点的权重）
grid_data_PM25 <- grid_data_PM25 %>%
  mutate(
    # 疫情前权重：[beta1, beta2, alpha3, alpha4]的权重
    w1_pre = 1,
    w2_pre = 2 * UTC1500,
    w3_pre = PM25,
    w4_pre = 0,
    # 疫情期间权重
    w1_during = 1,
    w2_during = 2 * UTC1500,
    w3_during = 0,
    w4_during = PM25
  )

# 计算疫情前标准误
se_pre <- apply(grid_data_PM25[, c("w1_pre", "w2_pre", "w3_pre", "w4_pre")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_PM25$se_pre <- se_pre

# 计算疫情期间标准误
se_during <- apply(grid_data_PM25[, c("w1_during", "w2_during", "w3_during", "w4_during")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_PM25$se_during <- se_during


# --------------------------
# 5. 计算95%置信区间
# --------------------------
t_low <- qt(0.025, df = df)
t_high <- qt(0.975, df = df)


grid_data_PM25$low_pre<-grid_data_PM25$marginal_pre + grid_data_PM25$se_pre*t_low
grid_data_PM25$up_pre<-grid_data_PM25$marginal_pre + grid_data_PM25$se_pre*t_high

grid_data_PM25$low_during<-grid_data_PM25$marginal_during + grid_data_PM25$se_during*t_low
grid_data_PM25$up_during<-grid_data_PM25$marginal_during + grid_data_PM25$se_during*t_high


############################################################

# 假设要获取"col1"、"col3"、"col5"这三列

grid_data_PM25$PM25<- grid_data_PM25$PM25+mean_PM25  ## 对去中心化的值进行还原
UTC_PM25_grid_before_during<- grid_data_PM25[, c("UTC1500","PM25", "marginal_pre", "low_pre", "up_pre","marginal_during", "low_during", "up_during")]

save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1500/',  'UTC_PM25_grid_before_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_PM25_grid_before_during, save_file_path_before)

################# 获取等于mean_UTC1500的数值位置 85##################

target_UTC1500 <- grid_data_PM25$UTC1500[85]### 选择固定阈值处的数据
# 筛选出UTC等于该值的所有行
sub_UTC_PM25_grid_before_during <- subset(UTC_PM25_grid_before_during, UTC1500 == target_UTC1500)

## before
UTC_PM25_grid_before<- sub_UTC_PM25_grid_before_during[, c("UTC1500","PM25", "marginal_pre", "low_pre","up_pre")]
colnames(UTC_PM25_grid_before) <- c("UTC1500","PM25", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1500/',  'UTC_PM25_meanUTC_before', '.csv', sep="")  # 字符串连接
write.csv(UTC_PM25_grid_before, save_file_path_before)

## during 
UTC_PM25_grid_during<- sub_UTC_PM25_grid_before_during[, c("UTC1500","PM25", "marginal_during", "low_during","up_during")]
colnames(UTC_PM25_grid_during) <- c("UTC1500","PM25", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1500/',  'UTC_PM25_meanUTC_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_PM25_grid_during, save_file_path_before)


###################





# # 7. 可视化：二维热力图（分疫情阶段）
# # --------------------------
# # 7.1 数据整理：将宽格式转为长格式（便于分面）
# plot_data <- UTC_PM25_grid_before_during %>%
#   select(UTC1500, PM25, marginal_pre, marginal_during) %>%
#   pivot_longer(
#     cols = starts_with("marginal"),
#     names_to = "period",
#     values_to = "marginal_effect",
#     names_prefix = "marginal_"
#   ) %>%
#   mutate(
#     period = case_when(
#       period == "pre" ~ "Before Covid-19",
#       period == "during" ~ "During Covid-19"
#     )
#   )
# 
# 
# # 7.2 绘制热力图（填充色=边际效应大小，等高线=效应等值线）
# ggplot(plot_data, aes(x = UTC1500, y = PM25)) +
#   geom_tile(aes(fill = marginal_effect), alpha = 1.0) +
#   geom_contour(aes(z = marginal_effect), color = "white", size = 1.0, breaks = seq(-10, 10, 5)) +
#   metR::geom_text_contour(aes(z = marginal_effect), color = "white", size = 3, breaks = seq(-10, 10, 5)) +
#   facet_wrap(~period, ncol =2) +
#   # 移除midpoint参数，使用默认viridis色阶
#   scale_fill_viridis_c(
#     option = "inferno",  # 可选："viridis", "magma", "inferno", "plasma",  "rocket", "mako", "turbo"
#     name = "Marginal Effects",
#     limits = c(min(plot_data$marginal_effect), max(plot_data$marginal_effect))
#   ) +
#   labs(
#     x = "UTC1500",
#     y = "PM2.5（μg/m³）",
#     title = "The marginal effects of trees varying based on levels of PM2.5 (buffer of 1500 m)"
#   ) +
#   theme_bw() +
#   theme(
#     plot.title = element_text(hjust = 0.5),
#     legend.position = "bottom",
#     legend.key.width = unit(3, "cm")
#   )
# 


####################################UTC二次项与疫情和 Precipitation 的交互##############################

model1500_interaction1 <- felm(sentiment ~ 1 + UTC1500+UTC1500_2+ UTC1500:Precipitation:covid  + NOUTC1500+PM25+
                                 humidity+ Wind + cloudcover + Surface_pressure+Skintemperature +
                                 population1500 + nightlight1500+ settlement1500+GDP
                               | yearmonthday + userid, 
                               data = query_sample2)

print(summary(model1500_interaction1))
#####################保存模型参数#######################
# 捕获第n个模型摘要的输出
model1500_summary <- capture.output(summary(model1500_interaction1))
# 将第n个模型的输出追加到同一个txt文件中
cat(model1500_summary, file = output_file, sep = "\n", append = TRUE)
#close(con)

##################################模型协方差计算#################################
# 提取模型系数
coeffs <-coef(model1500_interaction1) # 获取系数

##calculate and save marginal effects, with errors clustered regionally
cov <- vcovCR(model1500_interaction1,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解


####################before COVID19  生成效应面#######################

####################before COVID19 边界效应 #######################

# --------------------------
# 2. 提取模型参数（同前）
# --------------------------
alpha1 <- coeffs["UTC1500"]                  # -8.0401
alpha2 <- coeffs["UTC1500_2"]                # 9.5740
alpha3 <- coeffs["UTC1500:Precipitation:covidbefore"] # 0.0012
alpha4 <- coeffs["UTC1500:Precipitation:covidduring"] # 0.0106

# 协方差矩阵（用于标准误，可选，若仅可视化效应值可省略）
var_names <- c("UTC1500", "UTC1500_2", "UTC1500:Precipitation:covidbefore", "UTC1500:Precipitation:covidduring")
V <- cov[var_names, var_names] 

# 模型自由度（用于置信区间，可选）
df <- df.residual(model1500_interaction1)


# --------------------------
# 3. 构建二维网格（UTC1500 × PM2.5）
# --------------------------
# 3.1 设定两个变量的取值范围（基于原始数据，避免外推过多）
# UTC1500：取原始数据的5%~95%分位数（排除极端值）

UTC_grid <- seq(0, 1, length.out = 1500)  # 100个点

# PM2.5：取原始数据的0%~95%分位数（避免极端污染值）
Precipitation_range <- quantile(query_sample2$Precipitation, probs = c(0, 1), na.rm = TRUE)
Precipitation_grid <- seq(Precipitation_range[1], Precipitation_range[2], length.out = 1500)  # 100个点

# 3.2 生成二维网格（100×100=15000个组合）
grid_data_Precipitation <- expand.grid(
  UTC1500 = UTC_grid,
  Precipitation = Precipitation_grid
)


# --------------------------
# 5. 计算二维网格中每个点的边际效应（分疫情阶段）
# --------------------------
# 5.1 疫情前边际效应（covidbefore=1）
grid_data_Precipitation$marginal_pre <- alpha1 + 2 * alpha2 * grid_data_Precipitation$UTC1500 + alpha3 * grid_data_Precipitation$Precipitation

# 5.2 疫情期间边际效应（covidduring=1）
grid_data_Precipitation$marginal_during <- alpha1 + 2 * alpha2 * grid_data_Precipitation$UTC1500 + alpha4 * grid_data_Precipitation$Precipitation


# --------------------------
# 6. （可选）计算边际效应的标准误（误差传播）
# --------------------------
# 构建权重矩阵（每行对应一个网格点的权重）
grid_data_Precipitation <- grid_data_Precipitation %>%
  mutate(
    # 疫情前权重：[beta1, beta2, alpha3, alpha4]的权重
    w1_pre = 1,
    w2_pre = 2 * UTC1500,
    w3_pre = Precipitation,
    w4_pre = 0,
    # 疫情期间权重
    w1_during = 1,
    w2_during = 2 * UTC1500,
    w3_during = 0,
    w4_during = Precipitation
  )

# 计算疫情前标准误
se_pre <- apply(grid_data_Precipitation[, c("w1_pre", "w2_pre", "w3_pre", "w4_pre")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_Precipitation$se_pre <- se_pre

# 计算疫情期间标准误
se_during <- apply(grid_data_Precipitation[, c("w1_during", "w2_during", "w3_during", "w4_during")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_Precipitation$se_during <- se_during


# --------------------------
# 5. 计算95%置信区间
# --------------------------
t_low <- qt(0.025, df = df)
t_high <- qt(0.975, df = df)


grid_data_Precipitation$low_pre<-grid_data_Precipitation$marginal_pre + grid_data_Precipitation$se_pre*t_low
grid_data_Precipitation$up_pre<-grid_data_Precipitation$marginal_pre + grid_data_Precipitation$se_pre*t_high

grid_data_Precipitation$low_during<-grid_data_Precipitation$marginal_during + grid_data_Precipitation$se_during*t_low
grid_data_Precipitation$up_during<-grid_data_Precipitation$marginal_during + grid_data_Precipitation$se_during*t_high


############################################################

# 假设要获取"col1"、"col3"、"col5"这三列
grid_data_Precipitation$Precipitation<- grid_data_Precipitation$Precipitation+mean_Precipitation  ## 对去中心化的值进行还原
UTC_Precipitation_grid_before_during<- grid_data_Precipitation[, c("UTC1500","Precipitation", "marginal_pre", "low_pre", "up_pre","marginal_during", "low_during", "up_during")]
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1500/',  'UTC_Precipitation_grid_before_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_Precipitation_grid_before_during, save_file_path_before)

################# 获取等于mean_UTC1500的数值位置 85##################

target_UTC1500 <- grid_data_Precipitation$UTC1500[85]### 选择固定阈值处的数据
# 筛选出UTC等于该值的所有行
sub_UTC_Precipitation_grid_before_during <- subset(UTC_Precipitation_grid_before_during, UTC1500 == target_UTC1500)

## before
UTC_Precipitation_grid_before<- sub_UTC_Precipitation_grid_before_during[, c("UTC1500","Precipitation", "marginal_pre", "low_pre","up_pre")]
colnames(UTC_Precipitation_grid_before) <- c("UTC1500","Precipitation", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1500/',  'UTC_Precipitation_meanUTC_before', '.csv', sep="")  # 字符串连接
write.csv(UTC_Precipitation_grid_before, save_file_path_before)

## during 
UTC_Precipitation_grid_during<- sub_UTC_Precipitation_grid_before_during[, c("UTC1500","Precipitation", "marginal_during", "low_during","up_during")]
colnames(UTC_Precipitation_grid_during) <- c("UTC1500","Precipitation", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1500/',  'UTC_Precipitation_meanUTC_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_Precipitation_grid_during, save_file_path_before)


###################



####################################UTC与疫情和 humidity 的交互##############################

model1500_interaction2 <- felm(sentiment ~ 1 + UTC1500+UTC1500_2+ UTC1500:humidity:covid +NOUTC1500+PM25+
                                 Precipitation +Wind   + cloudcover + Surface_pressure+Skintemperature +
                                 population1500 + nightlight1500+ settlement1500+GDP
                               | yearmonthday + userid , 
                               data = query_sample2)

print(summary(model1500_interaction2))

#####################保存模型参数#######################
# 捕获第n个模型摘要的输出
model1500_summary <- capture.output(summary(model1500_interaction2))
# 将第n个模型的输出追加到同一个txt文件中
cat(model1500_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)


##################################模型协方差计算#################################


# 提取模型系数
coeffs <-coef(model1500_interaction2) # 获取系数

##calculate and save marginal effects, with errors clustered regionally
cov <- vcovCR(model1500_interaction2,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解


####################before COVID19 #######################
####################before COVID19 边界效应 #######################

# --------------------------
# 2. 提取模型参数（同前）
# --------------------------
alpha1 <- coeffs["UTC1500"]                  # -8.0401
alpha2 <- coeffs["UTC1500_2"]                # 9.5740
alpha3 <- coeffs["UTC1500:humidity:covidbefore"] # 0.0012
alpha4 <- coeffs["UTC1500:humidity:covidduring"] # 0.0106

# 协方差矩阵（用于标准误，可选，若仅可视化效应值可省略）
var_names <- c("UTC1500", "UTC1500_2", "UTC1500:humidity:covidbefore", "UTC1500:humidity:covidduring")
V <- cov[var_names, var_names] 

# 模型自由度（用于置信区间，可选）
df <- df.residual(model1500_interaction2)


# --------------------------
# 3. 构建二维网格（UTC1500 × PM2.5）
# --------------------------
# 3.1 设定两个变量的取值范围（基于原始数据，避免外推过多）
# UTC1500：取原始数据的5%~95%分位数（排除极端值）

UTC_grid <- seq(0, 1, length.out = 1500)  # 100个点

# PM2.5：取原始数据的0%~95%分位数（避免极端污染值）
humidity_range <- quantile(query_sample2$humidity, probs = c(0, 1), na.rm = TRUE)
humidity_grid <- seq(humidity_range[1], humidity_range[2], length.out = 1500)  # 100个点

# 3.2 生成二维网格（100×100=15000个组合）
grid_data_humidity <- expand.grid(
  UTC1500 = UTC_grid,
  humidity = humidity_grid
)


# --------------------------
# 5. 计算二维网格中每个点的边际效应（分疫情阶段）
# --------------------------
# 5.1 疫情前边际效应（covidbefore=1）
grid_data_humidity$marginal_pre <- alpha1 + 2 * alpha2 * grid_data_humidity$UTC1500 + alpha3 * grid_data_humidity$humidity

# 5.2 疫情期间边际效应（covidduring=1）
grid_data_humidity$marginal_during <- alpha1 + 2 * alpha2 * grid_data_humidity$UTC1500 + alpha4 * grid_data_humidity$humidity


# --------------------------
# 6. （可选）计算边际效应的标准误（误差传播）
# --------------------------
# 构建权重矩阵（每行对应一个网格点的权重）
grid_data_humidity <- grid_data_humidity %>%
  mutate(
    # 疫情前权重：[beta1, beta2, alpha3, alpha4]的权重
    w1_pre = 1,
    w2_pre = 2 * UTC1500,
    w3_pre = humidity,
    w4_pre = 0,
    # 疫情期间权重
    w1_during = 1,
    w2_during = 2 * UTC1500,
    w3_during = 0,
    w4_during = humidity
  )

# 计算疫情前标准误
se_pre <- apply(grid_data_humidity[, c("w1_pre", "w2_pre", "w3_pre", "w4_pre")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_humidity$se_pre <- se_pre

# 计算疫情期间标准误
se_during <- apply(grid_data_humidity[, c("w1_during", "w2_during", "w3_during", "w4_during")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_humidity$se_during <- se_during


# --------------------------
# 5. 计算95%置信区间
# --------------------------
t_low <- qt(0.025, df = df)
t_high <- qt(0.975, df = df)


grid_data_humidity$low_pre<-grid_data_humidity$marginal_pre + grid_data_humidity$se_pre*t_low
grid_data_humidity$up_pre<-grid_data_humidity$marginal_pre + grid_data_humidity$se_pre*t_high

grid_data_humidity$low_during<-grid_data_humidity$marginal_during + grid_data_humidity$se_during*t_low
grid_data_humidity$up_during<-grid_data_humidity$marginal_during + grid_data_humidity$se_during*t_high


############################################################

# 假设要获取"col1"、"col3"、"col5"这三列
grid_data_humidity$humidity<- grid_data_humidity$humidity+mean_humidity  ## 对去中心化的值进行还原
UTC_humidity_grid_before_during<- grid_data_humidity[, c("UTC1500","humidity", "marginal_pre", "low_pre", "up_pre","marginal_during", "low_during", "up_during")]
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1500/',  'UTC_humidity_grid_before_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_humidity_grid_before_during, save_file_path_before)

################# 获取等于mean_UTC1500的数值位置 85##################

target_UTC1500 <- grid_data_humidity$UTC1500[85]### 选择固定阈值处的数据
# 筛选出UTC等于该值的所有行
sub_UTC_humidity_grid_before_during <- subset(UTC_humidity_grid_before_during, UTC1500 == target_UTC1500)

## before
UTC_humidity_grid_before<- sub_UTC_humidity_grid_before_during[, c("UTC1500","humidity", "marginal_pre", "low_pre","up_pre")]
colnames(UTC_humidity_grid_before) <- c("UTC1500","humidity", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1500/',  'UTC_humidity_meanUTC_before', '.csv', sep="")  # 字符串连接
write.csv(UTC_humidity_grid_before, save_file_path_before)

## during 
UTC_humidity_grid_during<- sub_UTC_humidity_grid_before_during[, c("UTC1500","humidity", "marginal_during", "low_during","up_during")]
colnames(UTC_humidity_grid_during) <- c("UTC1500","humidity", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1500/',  'UTC_humidity_meanUTC_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_humidity_grid_during, save_file_path_before)



####################################UTC与疫情和 Skintemperature 的交互##############################

model1500_interaction3 <- felm(sentiment ~ 1 + UTC1500+UTC1500_2+ UTC1500:Skintemperature:covid + NOUTC1500+PM25+
                                 humidity +Precipitation + Wind + cloudcover + Surface_pressure +
                                 population1500 + nightlight1500+ settlement1500+GDP
                               | yearmonthday + userid , 
                               data = query_sample2)

print(summary(model1500_interaction3))

#####################保存模型参数#######################
# 捕获第n个模型摘要的输出
model1500_summary <- capture.output(summary(model1500_interaction3))
# 将第n个模型的输出追加到同一个txt文件中
cat(model1500_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)


##################################模型协方差计算#################################


# 提取模型系数
coeffs <-coef(model1500_interaction3) # 获取系数

##calculate and save marginal effects, with errors clustered regionally
cov <- vcovCR(model1500_interaction3,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解


####################before COVID19 #######################
####################before COVID19 边界效应 #######################

# --------------------------
# 2. 提取模型参数（同前）
# --------------------------
alpha1 <- coeffs["UTC1500"]                  # -8.0401
alpha2 <- coeffs["UTC1500_2"]                # 9.5740
alpha3 <- coeffs["UTC1500:Skintemperature:covidbefore"] # 0.0012
alpha4 <- coeffs["UTC1500:Skintemperature:covidduring"] # 0.0106

# 协方差矩阵（用于标准误，可选，若仅可视化效应值可省略）
var_names <- c("UTC1500", "UTC1500_2", "UTC1500:Skintemperature:covidbefore", "UTC1500:Skintemperature:covidduring")
V <- cov[var_names, var_names] 

# 模型自由度（用于置信区间，可选）
df <- df.residual(model1500_interaction3)


# --------------------------
# 3. 构建二维网格（UTC1500 × PM2.5）
# --------------------------
# 3.1 设定两个变量的取值范围（基于原始数据，避免外推过多）
# UTC1500：取原始数据的5%~95%分位数（排除极端值）

UTC_grid <- seq(0, 1, length.out = 1500)  # 100个点

# PM2.5：取原始数据的0%~95%分位数（避免极端污染值）
Skintemperature_range <- quantile(query_sample2$Skintemperature, probs = c(0, 1), na.rm = TRUE)
Skintemperature_grid <- seq(Skintemperature_range[1], Skintemperature_range[2], length.out = 1500)  # 100个点

# 3.2 生成二维网格（100×100=15000个组合）
grid_data_Skintemperature <- expand.grid(
  UTC1500 = UTC_grid,
  Skintemperature = Skintemperature_grid
)


# --------------------------
# 5. 计算二维网格中每个点的边际效应（分疫情阶段）
# --------------------------
# 5.1 疫情前边际效应（covidbefore=1）
grid_data_Skintemperature$marginal_pre <- alpha1 + 2 * alpha2 * grid_data_Skintemperature$UTC1500 + alpha3 * grid_data_Skintemperature$Skintemperature

# 5.2 疫情期间边际效应（covidduring=1）
grid_data_Skintemperature$marginal_during <- alpha1 + 2 * alpha2 * grid_data_Skintemperature$UTC1500 + alpha4 * grid_data_Skintemperature$Skintemperature


# --------------------------
# 6. （可选）计算边际效应的标准误（误差传播）
# --------------------------
# 构建权重矩阵（每行对应一个网格点的权重）
grid_data_Skintemperature <- grid_data_Skintemperature %>%
  mutate(
    # 疫情前权重：[beta1, beta2, alpha3, alpha4]的权重
    w1_pre = 1,
    w2_pre = 2 * UTC1500,
    w3_pre = Skintemperature,
    w4_pre = 0,
    # 疫情期间权重
    w1_during = 1,
    w2_during = 2 * UTC1500,
    w3_during = 0,
    w4_during = Skintemperature
  )

# 计算疫情前标准误
se_pre <- apply(grid_data_Skintemperature[, c("w1_pre", "w2_pre", "w3_pre", "w4_pre")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_Skintemperature$se_pre <- se_pre

# 计算疫情期间标准误
se_during <- apply(grid_data_Skintemperature[, c("w1_during", "w2_during", "w3_during", "w4_during")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_Skintemperature$se_during <- se_during


# --------------------------
# 5. 计算95%置信区间
# --------------------------
t_low <- qt(0.025, df = df)
t_high <- qt(0.975, df = df)


grid_data_Skintemperature$low_pre<-grid_data_Skintemperature$marginal_pre + grid_data_Skintemperature$se_pre*t_low
grid_data_Skintemperature$up_pre<-grid_data_Skintemperature$marginal_pre + grid_data_Skintemperature$se_pre*t_high

grid_data_Skintemperature$low_during<-grid_data_Skintemperature$marginal_during + grid_data_Skintemperature$se_during*t_low
grid_data_Skintemperature$up_during<-grid_data_Skintemperature$marginal_during + grid_data_Skintemperature$se_during*t_high


############################################################

# 假设要获取"col1"、"col3"、"col5"这三列
grid_data_Skintemperature$Skintemperature<- grid_data_Skintemperature$Skintemperature+mean_Skintemperature  ## 对去中心化的值进行还原
UTC_Skintemperature_grid_before_during<- grid_data_Skintemperature[, c("UTC1500","Skintemperature", "marginal_pre", "low_pre", "up_pre","marginal_during", "low_during", "up_during")]
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1500/',  'UTC_Skintemperature_grid_before_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_Skintemperature_grid_before_during, save_file_path_before)

################# 获取等于mean_UTC1500的数值位置 85##################

target_UTC1500 <- grid_data_Skintemperature$UTC1500[85]### 选择固定阈值处的数据
# 筛选出UTC等于该值的所有行
sub_UTC_Skintemperature_grid_before_during <- subset(UTC_Skintemperature_grid_before_during, UTC1500 == target_UTC1500)

## before
UTC_Skintemperature_grid_before<- sub_UTC_Skintemperature_grid_before_during[, c("UTC1500","Skintemperature", "marginal_pre", "low_pre","up_pre")]
colnames(UTC_Skintemperature_grid_before) <- c("UTC1500","Skintemperature", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1500/',  'UTC_Skintemperature_meanUTC_before', '.csv', sep="")  # 字符串连接
write.csv(UTC_Skintemperature_grid_before, save_file_path_before)

## during 
UTC_Skintemperature_grid_during<- sub_UTC_Skintemperature_grid_before_during[, c("UTC1500","Skintemperature", "marginal_during", "low_during","up_during")]
colnames(UTC_Skintemperature_grid_during) <- c("UTC1500","Skintemperature", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1500/',  'UTC_Skintemperature_meanUTC_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_Skintemperature_grid_during, save_file_path_before)


###################




####################################UTC与疫情和 Surface_pressure 的交互##############################

model1500_interaction4 <- felm(sentiment ~ 1 + UTC1500+UTC1500_2+ UTC1500:Surface_pressure:covid + NOUTC1500+PM25+
                                 humidity + Precipitation + Wind + cloudcover +Skintemperature +Surface_pressure+
                                 population1500 + nightlight1500+ settlement1500+GDP
                               | yearmonthday + userid , 
                               data = query_sample2)

print(summary(model1500_interaction4))

#####################保存模型参数#######################
# 捕获第n个模型摘要的输出
model1500_summary <- capture.output(summary(model1500_interaction4))
# 将第n个模型的输出追加到同一个txt文件中
cat(model1500_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)


##################################模型协方差计算#################################


# 提取模型系数
coeffs <-coef(model1500_interaction4) # 获取系数

##calculate and save marginal effects, with errors clustered regionally
cov <- vcovCR(model1500_interaction4,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解


####################before COVID19 边界效应 #######################

# --------------------------
# 2. 提取模型参数（同前）
# --------------------------
alpha1 <- coeffs["UTC1500"]                  # -8.0401
alpha2 <- coeffs["UTC1500_2"]                # 9.5740
alpha3 <- coeffs["UTC1500:Surface_pressure:covidbefore"] # 0.0012
alpha4 <- coeffs["UTC1500:Surface_pressure:covidduring"] # 0.0106

# 协方差矩阵（用于标准误，可选，若仅可视化效应值可省略）
var_names <- c("UTC1500", "UTC1500_2", "UTC1500:Surface_pressure:covidbefore", "UTC1500:Surface_pressure:covidduring")
V <- cov[var_names, var_names] 

# 模型自由度（用于置信区间，可选）
df <- df.residual(model1500_interaction4)


# --------------------------
# 3. 构建二维网格（UTC1500 × PM2.5）
# --------------------------
# 3.1 设定两个变量的取值范围（基于原始数据，避免外推过多）
# UTC1500：取原始数据的5%~95%分位数（排除极端值）

UTC_grid <- seq(0, 1, length.out = 1500)  # 100个点

# PM2.5：取原始数据的0%~95%分位数（避免极端污染值）
Surface_pressure_range <- quantile(query_sample2$Surface_pressure, probs = c(0, 1), na.rm = TRUE)
Surface_pressure_grid <- seq(Surface_pressure_range[1], Surface_pressure_range[2], length.out = 1500)  # 100个点

# 3.2 生成二维网格（100×100=15000个组合）
grid_data_Surface_pressure <- expand.grid(
  UTC1500 = UTC_grid,
  Surface_pressure = Surface_pressure_grid
)


# --------------------------
# 5. 计算二维网格中每个点的边际效应（分疫情阶段）
# --------------------------
# 5.1 疫情前边际效应（covidbefore=1）
grid_data_Surface_pressure$marginal_pre <- alpha1 + 2 * alpha2 * grid_data_Surface_pressure$UTC1500 + alpha3 * grid_data_Surface_pressure$Surface_pressure

# 5.2 疫情期间边际效应（covidduring=1）
grid_data_Surface_pressure$marginal_during <- alpha1 + 2 * alpha2 * grid_data_Surface_pressure$UTC1500 + alpha4 * grid_data_Surface_pressure$Surface_pressure


# --------------------------
# 6. （可选）计算边际效应的标准误（误差传播）
# --------------------------
# 构建权重矩阵（每行对应一个网格点的权重）
grid_data_Surface_pressure <- grid_data_Surface_pressure %>%
  mutate(
    # 疫情前权重：[beta1, beta2, alpha3, alpha4]的权重
    w1_pre = 1,
    w2_pre = 2 * UTC1500,
    w3_pre = Surface_pressure,
    w4_pre = 0,
    # 疫情期间权重
    w1_during = 1,
    w2_during = 2 * UTC1500,
    w3_during = 0,
    w4_during = Surface_pressure
  )

# 计算疫情前标准误
se_pre <- apply(grid_data_Surface_pressure[, c("w1_pre", "w2_pre", "w3_pre", "w4_pre")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_Surface_pressure$se_pre <- se_pre

# 计算疫情期间标准误
se_during <- apply(grid_data_Surface_pressure[, c("w1_during", "w2_during", "w3_during", "w4_during")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_Surface_pressure$se_during <- se_during


# --------------------------
# 5. 计算95%置信区间
# --------------------------
t_low <- qt(0.025, df = df)
t_high <- qt(0.975, df = df)


grid_data_Surface_pressure$low_pre<-grid_data_Surface_pressure$marginal_pre + grid_data_Surface_pressure$se_pre*t_low
grid_data_Surface_pressure$up_pre<-grid_data_Surface_pressure$marginal_pre + grid_data_Surface_pressure$se_pre*t_high

grid_data_Surface_pressure$low_during<-grid_data_Surface_pressure$marginal_during + grid_data_Surface_pressure$se_during*t_low
grid_data_Surface_pressure$up_during<-grid_data_Surface_pressure$marginal_during + grid_data_Surface_pressure$se_during*t_high


############################################################

# 假设要获取"col1"、"col3"、"col5"这三列


grid_data_Surface_pressure$Surface_pressure<- grid_data_Surface_pressure$Surface_pressure+mean_Surface_pressure  ## 对去中心化的值进行还原
UTC_Surface_pressure_grid_before_during<- grid_data_Surface_pressure[, c("UTC1500","Surface_pressure", "marginal_pre", "low_pre", "up_pre","marginal_during", "low_during", "up_during")]
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1500/',  'UTC_Surface_pressure_grid_before_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_Surface_pressure_grid_before_during, save_file_path_before)

################# 获取等于mean_UTC1500的数值位置 85##################

target_UTC1500 <- grid_data_Surface_pressure$UTC1500[85]### 选择固定阈值处的数据
# 筛选出UTC等于该值的所有行
sub_UTC_Surface_pressure_grid_before_during <- subset(UTC_Surface_pressure_grid_before_during, UTC1500 == target_UTC1500)

## before
UTC_Surface_pressure_grid_before<- sub_UTC_Surface_pressure_grid_before_during[, c("UTC1500","Surface_pressure", "marginal_pre", "low_pre","up_pre")]
colnames(UTC_Surface_pressure_grid_before) <- c("UTC1500","Surface_pressure", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1500/',  'UTC_Surface_pressure_meanUTC_before', '.csv', sep="")  # 字符串连接
write.csv(UTC_Surface_pressure_grid_before, save_file_path_before)

## during 
UTC_Surface_pressure_grid_during<- sub_UTC_Surface_pressure_grid_before_during[, c("UTC1500","Surface_pressure", "marginal_during", "low_during","up_during")]
colnames(UTC_Surface_pressure_grid_during) <- c("UTC1500","Surface_pressure", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1500/',  'UTC_Surface_pressure_meanUTC_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_Surface_pressure_grid_during, save_file_path_before)


###################

####################################UTC与疫情和 population1500 的交互##############################

model1500_interaction5 <- felm(sentiment ~ 1 + UTC1500+UTC1500_2+ UTC1500:population1500:covid + NOUTC1500+PM25+
                                 humidity +Precipitation + Wind + cloudcover + Surface_pressure+Skintemperature +
                                 population1500+nightlight1500+ settlement1500+GDP
                               | yearmonthday + userid , 
                               data = query_sample2)

print(summary(model1500_interaction5))

#####################保存模型参数#######################
# 捕获第n个模型摘要的输出
model1500_summary <- capture.output(summary(model1500_interaction5))
# 将第n个模型的输出追加到同一个txt文件中
cat(model1500_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)


##################################模型协方差计算#################################


# 提取模型系数
coeffs <-coef(model1500_interaction5) # 获取系数

##calculate and save marginal effects, with errors clustered regionally
cov <- vcovCR(model1500_interaction5,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解


####################before COVID19 #######################
####################before COVID19 边界效应 #######################

# --------------------------
# 2. 提取模型参数（同前）
# --------------------------
alpha1 <- coeffs["UTC1500"]                  # -8.0401
alpha2 <- coeffs["UTC1500_2"]                # 9.5740
alpha3 <- coeffs["UTC1500:population1500:covidbefore"] # 0.0012
alpha4 <- coeffs["UTC1500:population1500:covidduring"] # 0.0106

# 协方差矩阵（用于标准误，可选，若仅可视化效应值可省略）
var_names <- c("UTC1500", "UTC1500_2", "UTC1500:population1500:covidbefore", "UTC1500:population1500:covidduring")
V <- cov[var_names, var_names] 

# 模型自由度（用于置信区间，可选）
df <- df.residual(model1500_interaction5)


# --------------------------
# 3. 构建二维网格（UTC1500 × PM2.5）
# --------------------------
# 3.1 设定两个变量的取值范围（基于原始数据，避免外推过多）
# UTC1500：取原始数据的5%~95%分位数（排除极端值）

UTC_grid <- seq(0, 1, length.out = 1500)  # 100个点

# PM2.5：取原始数据的0%~95%分位数（避免极端污染值）
population1500_range <- quantile(query_sample2$population1500, probs = c(0, 1), na.rm = TRUE)
population1500_grid <- seq(population1500_range[1], population1500_range[2], length.out = 1500)  # 100个点

# 3.2 生成二维网格（100×100=15000个组合）
grid_data_population1500 <- expand.grid(
  UTC1500 = UTC_grid,
  population1500 = population1500_grid
)


# --------------------------
# 5. 计算二维网格中每个点的边际效应（分疫情阶段）
# --------------------------
# 5.1 疫情前边际效应（covidbefore=1）
grid_data_population1500$marginal_pre <- alpha1 + 2 * alpha2 * grid_data_population1500$UTC1500 + alpha3 * grid_data_population1500$population1500

# 5.2 疫情期间边际效应（covidduring=1）
grid_data_population1500$marginal_during <- alpha1 + 2 * alpha2 * grid_data_population1500$UTC1500 + alpha4 * grid_data_population1500$population1500


# --------------------------
# 6. （可选）计算边际效应的标准误（误差传播）
# --------------------------
# 构建权重矩阵（每行对应一个网格点的权重）
grid_data_population1500 <- grid_data_population1500 %>%
  mutate(
    # 疫情前权重：[beta1, beta2, alpha3, alpha4]的权重
    w1_pre = 1,
    w2_pre = 2 * UTC1500,
    w3_pre = population1500,
    w4_pre = 0,
    # 疫情期间权重
    w1_during = 1,
    w2_during = 2 * UTC1500,
    w3_during = 0,
    w4_during = population1500
  )

# 计算疫情前标准误
se_pre <- apply(grid_data_population1500[, c("w1_pre", "w2_pre", "w3_pre", "w4_pre")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_population1500$se_pre <- se_pre

# 计算疫情期间标准误
se_during <- apply(grid_data_population1500[, c("w1_during", "w2_during", "w3_during", "w4_during")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_population1500$se_during <- se_during


# --------------------------
# 5. 计算95%置信区间
# --------------------------
t_low <- qt(0.025, df = df)
t_high <- qt(0.975, df = df)


grid_data_population1500$low_pre<-grid_data_population1500$marginal_pre + grid_data_population1500$se_pre*t_low
grid_data_population1500$up_pre<-grid_data_population1500$marginal_pre + grid_data_population1500$se_pre*t_high

grid_data_population1500$low_during<-grid_data_population1500$marginal_during + grid_data_population1500$se_during*t_low
grid_data_population1500$up_during<-grid_data_population1500$marginal_during + grid_data_population1500$se_during*t_high


############################################################

# 假设要获取"col1"、"col3"、"col5"这三列

grid_data_population1500$population1500<- grid_data_population1500$population1500+mean_population1500  ## 对去中心化的值进行还原
UTC_population1500_grid_before_during<- grid_data_population1500[, c("UTC1500","population1500", "marginal_pre", "low_pre", "up_pre","marginal_during", "low_during", "up_during")]
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1500/',  'UTC_population1500_grid_before_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_population1500_grid_before_during, save_file_path_before)

################# 获取等于mean_UTC1500的数值位置 85##################

target_UTC1500 <- grid_data_population1500$UTC1500[85]### 选择固定阈值处的数据
# 筛选出UTC等于该值的所有行
sub_UTC_population1500_grid_before_during <- subset(UTC_population1500_grid_before_during, UTC1500 == target_UTC1500)

## before
UTC_population1500_grid_before<- sub_UTC_population1500_grid_before_during[, c("UTC1500","population1500", "marginal_pre", "low_pre","up_pre")]
colnames(UTC_population1500_grid_before) <- c("UTC1500","population1500", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1500/',  'UTC_population1500_meanUTC_before', '.csv', sep="")  # 字符串连接
write.csv(UTC_population1500_grid_before, save_file_path_before)

## during 
UTC_population1500_grid_during<- sub_UTC_population1500_grid_before_during[, c("UTC1500","population1500", "marginal_during", "low_during","up_during")]
colnames(UTC_population1500_grid_during) <- c("UTC1500","population1500", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1500/',  'UTC_population1500_meanUTC_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_population1500_grid_during, save_file_path_before)


###################


####################################UTC与疫情和 nightlight1500 的交互##############################

model1500_interaction6 <- felm(sentiment ~ 1 + UTC1500+UTC1500_2+ UTC1500:nightlight1500:covid + NOUTC1500+PM25+
                                 humidity +Precipitation + Wind + cloudcover + Surface_pressure+Skintemperature +
                                 nightlight1500+ population1500 + settlement1500+GDP
                               | yearmonthday + userid , 
                               data = query_sample2)

print(summary(model1500_interaction6))

#####################保存模型参数#######################
# 捕获第n个模型摘要的输出
model1500_summary <- capture.output(summary(model1500_interaction6))
# 将第n个模型的输出追加到同一个txt文件中
cat(model1500_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)


##################################模型协方差计算#################################


# 提取模型系数
coeffs <-coef(model1500_interaction6) # 获取系数

##calculate and save marginal effects, with errors clustered regionally
cov <- vcovCR(model1500_interaction6,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解


####################before COVID19 #######################
####################before COVID19 边界效应 #######################

# --------------------------
# 2. 提取模型参数（同前）
# --------------------------
alpha1 <- coeffs["UTC1500"]                  # -8.0401
alpha2 <- coeffs["UTC1500_2"]                # 9.5740
alpha3 <- coeffs["UTC1500:nightlight1500:covidbefore"] # 0.0012
alpha4 <- coeffs["UTC1500:nightlight1500:covidduring"] # 0.0106

# 协方差矩阵（用于标准误，可选，若仅可视化效应值可省略）
var_names <- c("UTC1500", "UTC1500_2", "UTC1500:nightlight1500:covidbefore", "UTC1500:nightlight1500:covidduring")
V <- cov[var_names, var_names] 

# 模型自由度（用于置信区间，可选）
df <- df.residual(model1500_interaction6)


# --------------------------
# 3. 构建二维网格（UTC1500 × PM2.5）
# --------------------------
# 3.1 设定两个变量的取值范围（基于原始数据，避免外推过多）
# UTC1500：取原始数据的5%~95%分位数（排除极端值）

UTC_grid <- seq(0, 1, length.out = 1500)  # 100个点

# PM2.5：取原始数据的0%~95%分位数（避免极端污染值）
nightlight1500_range <- quantile(query_sample2$nightlight1500, probs = c(0, 1), na.rm = TRUE)
nightlight1500_grid <- seq(nightlight1500_range[1], nightlight1500_range[2], length.out = 1500)  # 100个点

# 3.2 生成二维网格（100×100=15000个组合）
grid_data_nightlight1500 <- expand.grid(
  UTC1500 = UTC_grid,
  nightlight1500 = nightlight1500_grid
)


# --------------------------
# 5. 计算二维网格中每个点的边际效应（分疫情阶段）
# --------------------------
# 5.1 疫情前边际效应（covidbefore=1）
grid_data_nightlight1500$marginal_pre <- alpha1 + 2 * alpha2 * grid_data_nightlight1500$UTC1500 + alpha3 * grid_data_nightlight1500$nightlight1500

# 5.2 疫情期间边际效应（covidduring=1）
grid_data_nightlight1500$marginal_during <- alpha1 + 2 * alpha2 * grid_data_nightlight1500$UTC1500 + alpha4 * grid_data_nightlight1500$nightlight1500


# --------------------------
# 6. （可选）计算边际效应的标准误（误差传播）
# --------------------------
# 构建权重矩阵（每行对应一个网格点的权重）
grid_data_nightlight1500 <- grid_data_nightlight1500 %>%
  mutate(
    # 疫情前权重：[beta1, beta2, alpha3, alpha4]的权重
    w1_pre = 1,
    w2_pre = 2 * UTC1500,
    w3_pre = nightlight1500,
    w4_pre = 0,
    # 疫情期间权重
    w1_during = 1,
    w2_during = 2 * UTC1500,
    w3_during = 0,
    w4_during = nightlight1500
  )

# 计算疫情前标准误
se_pre <- apply(grid_data_nightlight1500[, c("w1_pre", "w2_pre", "w3_pre", "w4_pre")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_nightlight1500$se_pre <- se_pre

# 计算疫情期间标准误
se_during <- apply(grid_data_nightlight1500[, c("w1_during", "w2_during", "w3_during", "w4_during")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_nightlight1500$se_during <- se_during


# --------------------------
# 5. 计算95%置信区间
# --------------------------
t_low <- qt(0.025, df = df)
t_high <- qt(0.975, df = df)


grid_data_nightlight1500$low_pre<-grid_data_nightlight1500$marginal_pre + grid_data_nightlight1500$se_pre*t_low
grid_data_nightlight1500$up_pre<-grid_data_nightlight1500$marginal_pre + grid_data_nightlight1500$se_pre*t_high

grid_data_nightlight1500$low_during<-grid_data_nightlight1500$marginal_during + grid_data_nightlight1500$se_during*t_low
grid_data_nightlight1500$up_during<-grid_data_nightlight1500$marginal_during + grid_data_nightlight1500$se_during*t_high


############################################################

# 假设要获取"col1"、"col3"、"col5"这三列

grid_data_nightlight1500$nightlight1500<- grid_data_nightlight1500$nightlight1500+mean_nightlight1500  ## 对去中心化的值进行还原
UTC_nightlight1500_grid_before_during<- grid_data_nightlight1500[, c("UTC1500","nightlight1500", "marginal_pre", "low_pre", "up_pre","marginal_during", "low_during", "up_during")]
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1500/',  'UTC_nightlight1500_grid_before_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_nightlight1500_grid_before_during, save_file_path_before)

################# 获取等于mean_UTC1500的数值位置 85##################

target_UTC1500 <- grid_data_nightlight1500$UTC1500[85]### 选择固定阈值处的数据
# 筛选出UTC等于该值的所有行
sub_UTC_nightlight1500_grid_before_during <- subset(UTC_nightlight1500_grid_before_during, UTC1500 == target_UTC1500)

## before
UTC_nightlight1500_grid_before<- sub_UTC_nightlight1500_grid_before_during[, c("UTC1500","nightlight1500", "marginal_pre", "low_pre","up_pre")]
colnames(UTC_nightlight1500_grid_before) <- c("UTC1500","nightlight1500", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1500/',  'UTC_nightlight1500_meanUTC_before', '.csv', sep="")  # 字符串连接
write.csv(UTC_nightlight1500_grid_before, save_file_path_before)

## during 
UTC_nightlight1500_grid_during<- sub_UTC_nightlight1500_grid_before_during[, c("UTC1500","nightlight1500", "marginal_during", "low_during","up_during")]
colnames(UTC_nightlight1500_grid_during) <- c("UTC1500","nightlight1500", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1500/',  'UTC_nightlight1500_meanUTC_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_nightlight1500_grid_during, save_file_path_before)


###################



####################################UTC与疫情和 settlement1500 的交互##############################

model1500_interaction7 <- felm(sentiment ~ 1 + UTC1500+UTC1500_2+ UTC1500:settlement1500:covid + NOUTC1500+PM25+
                                 humidity +Precipitation + Wind + cloudcover + Surface_pressure+Skintemperature +settlement1500+
                                 population1500 + nightlight1500+GDP
                               | yearmonthday + userid , 
                               data = query_sample2)

print(summary(model1500_interaction7))

#####################保存模型参数#######################
# 捕获第n个模型摘要的输出
model1500_summary <- capture.output(summary(model1500_interaction7))
# 将第n个模型的输出追加到同一个txt文件中
cat(model1500_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)


##################################模型协方差计算#################################


# 提取模型系数
coeffs <-coef(model1500_interaction7) # 获取系数

##calculate and save marginal effects, with errors clustered regionally
cov <- vcovCR(model1500_interaction7,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解


####################before COVID19 #######################
####################before COVID19 边界效应 #######################

# --------------------------
# 2. 提取模型参数（同前）
# --------------------------
alpha1 <- coeffs["UTC1500"]                  # -8.0401
alpha2 <- coeffs["UTC1500_2"]                # 9.5740
alpha3 <- coeffs["UTC1500:settlement1500:covidbefore"] # 0.0012
alpha4 <- coeffs["UTC1500:settlement1500:covidduring"] # 0.0106

# 协方差矩阵（用于标准误，可选，若仅可视化效应值可省略）
var_names <- c("UTC1500", "UTC1500_2", "UTC1500:settlement1500:covidbefore", "UTC1500:settlement1500:covidduring")
V <- cov[var_names, var_names] 

# 模型自由度（用于置信区间，可选）
df <- df.residual(model1500_interaction7)


# --------------------------
# 3. 构建二维网格（UTC1500 × PM2.5）
# --------------------------
# 3.1 设定两个变量的取值范围（基于原始数据，避免外推过多）
# UTC1500：取原始数据的5%~95%分位数（排除极端值）

UTC_grid <- seq(0, 1, length.out = 1500)  # 100个点

# PM2.5：取原始数据的0%~95%分位数（避免极端污染值）
settlement1500_range <- quantile(query_sample2$settlement1500, probs = c(0, 1), na.rm = TRUE)
settlement1500_grid <- seq(settlement1500_range[1], settlement1500_range[2], length.out = 1500)  # 100个点

# 3.2 生成二维网格（100×100=15000个组合）
grid_data_settlement1500 <- expand.grid(
  UTC1500 = UTC_grid,
  settlement1500 = settlement1500_grid
)


# --------------------------
# 5. 计算二维网格中每个点的边际效应（分疫情阶段）
# --------------------------
# 5.1 疫情前边际效应（covidbefore=1）
grid_data_settlement1500$marginal_pre <- alpha1 + 2 * alpha2 * grid_data_settlement1500$UTC1500 + alpha3 * grid_data_settlement1500$settlement1500

# 5.2 疫情期间边际效应（covidduring=1）
grid_data_settlement1500$marginal_during <- alpha1 + 2 * alpha2 * grid_data_settlement1500$UTC1500 + alpha4 * grid_data_settlement1500$settlement1500


# --------------------------
# 6. （可选）计算边际效应的标准误（误差传播）
# --------------------------
# 构建权重矩阵（每行对应一个网格点的权重）
grid_data_settlement1500 <- grid_data_settlement1500 %>%
  mutate(
    # 疫情前权重：[beta1, beta2, alpha3, alpha4]的权重
    w1_pre = 1,
    w2_pre = 2 * UTC1500,
    w3_pre = settlement1500,
    w4_pre = 0,
    # 疫情期间权重
    w1_during = 1,
    w2_during = 2 * UTC1500,
    w3_during = 0,
    w4_during = settlement1500
  )

# 计算疫情前标准误
se_pre <- apply(grid_data_settlement1500[, c("w1_pre", "w2_pre", "w3_pre", "w4_pre")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_settlement1500$se_pre <- se_pre

# 计算疫情期间标准误
se_during <- apply(grid_data_settlement1500[, c("w1_during", "w2_during", "w3_during", "w4_during")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_settlement1500$se_during <- se_during


# --------------------------
# 5. 计算95%置信区间
# --------------------------
t_low <- qt(0.025, df = df)
t_high <- qt(0.975, df = df)


grid_data_settlement1500$low_pre<-grid_data_settlement1500$marginal_pre + grid_data_settlement1500$se_pre*t_low
grid_data_settlement1500$up_pre<-grid_data_settlement1500$marginal_pre + grid_data_settlement1500$se_pre*t_high

grid_data_settlement1500$low_during<-grid_data_settlement1500$marginal_during + grid_data_settlement1500$se_during*t_low
grid_data_settlement1500$up_during<-grid_data_settlement1500$marginal_during + grid_data_settlement1500$se_during*t_high


############################################################

# 假设要获取"col1"、"col3"、"col5"这三列

grid_data_settlement1500$settlement1500<- grid_data_settlement1500$settlement1500+mean_settlement1500  ## 对去中心化的值进行还原
UTC_settlement1500_grid_before_during<- grid_data_settlement1500[, c("UTC1500","settlement1500", "marginal_pre", "low_pre", "up_pre","marginal_during", "low_during", "up_during")]
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1500/',  'UTC_settlement1500_grid_before_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_settlement1500_grid_before_during, save_file_path_before)

################# 获取等于mean_UTC1500的数值位置 85##################

target_UTC1500 <- grid_data_settlement1500$UTC1500[85]### 选择固定阈值处的数据
# 筛选出UTC等于该值的所有行
sub_UTC_settlement1500_grid_before_during <- subset(UTC_settlement1500_grid_before_during, UTC1500 == target_UTC1500)

## before
UTC_settlement1500_grid_before<- sub_UTC_settlement1500_grid_before_during[, c("UTC1500","settlement1500", "marginal_pre", "low_pre","up_pre")]
colnames(UTC_settlement1500_grid_before) <- c("UTC1500","settlement1500", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1500/',  'UTC_settlement1500_meanUTC_before', '.csv', sep="")  # 字符串连接
write.csv(UTC_settlement1500_grid_before, save_file_path_before)

## during 
UTC_settlement1500_grid_during<- sub_UTC_settlement1500_grid_before_during[, c("UTC1500","settlement1500", "marginal_during", "low_during","up_during")]
colnames(UTC_settlement1500_grid_during) <- c("UTC1500","settlement1500", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1500/',  'UTC_settlement1500_meanUTC_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_settlement1500_grid_during, save_file_path_before)


###################



####################################UTC与疫情和 GDP 的交互##############################

model1500_interaction9 <- felm(sentiment ~ 1 + UTC1500+UTC1500_2+ UTC1500:GDP:covid + NOUTC1500+PM25+
                                 humidity + Precipitation + Wind + cloudcover + + Surface_pressure+Skintemperature +
                                 population1500 + nightlight1500+ settlement1500
                               | yearmonthday + userid , 
                               data = query_sample2)

print(summary(model1500_interaction9))

#####################保存模型参数#######################
# 捕获第n个模型摘要的输出
model1500_summary <- capture.output(summary(model1500_interaction9))
# 将第n个模型的输出追加到同一个txt文件中
cat(model1500_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)


##################################模型协方差计算#################################


# 提取模型系数
coeffs <-coef(model1500_interaction9) # 获取系数

##calculate and save marginal effects, with errors clustered regionally
cov <- vcovCR(model1500_interaction9,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解


####################before COVID19 #######################
####################before COVID19 边界效应 #######################

# --------------------------
# 2. 提取模型参数（同前）
# --------------------------
alpha1 <- coeffs["UTC1500"]                  # -8.0401
alpha2 <- coeffs["UTC1500_2"]                # 9.5740
alpha3 <- coeffs["UTC1500:GDP:covidbefore"] # 0.0012
alpha4 <- coeffs["UTC1500:GDP:covidduring"] # 0.0106

# 协方差矩阵（用于标准误，可选，若仅可视化效应值可省略）
var_names <- c("UTC1500", "UTC1500_2", "UTC1500:GDP:covidbefore", "UTC1500:GDP:covidduring")
V <- cov[var_names, var_names] 

# 模型自由度（用于置信区间，可选）
df <- df.residual(model1500_interaction9)


# --------------------------
# 3. 构建二维网格（UTC1500 × PM2.5）
# --------------------------
# 3.1 设定两个变量的取值范围（基于原始数据，避免外推过多）
# UTC1500：取原始数据的5%~95%分位数（排除极端值）

UTC_grid <- seq(0, 1, length.out = 1500)  # 100个点

# PM2.5：取原始数据的0%~95%分位数（避免极端污染值）
GDP_range <- quantile(query_sample2$GDP, probs = c(0, 1), na.rm = TRUE)
GDP_grid <- seq(GDP_range[1], GDP_range[2], length.out = 1500)  # 100个点

# 3.2 生成二维网格（100×100=15000个组合）
grid_data_GDP <- expand.grid(
  UTC1500 = UTC_grid,
  GDP = GDP_grid
)


# --------------------------
# 5. 计算二维网格中每个点的边际效应（分疫情阶段）
# --------------------------
# 5.1 疫情前边际效应（covidbefore=1）
grid_data_GDP$marginal_pre <- alpha1 + 2 * alpha2 * grid_data_GDP$UTC1500 + alpha3 * grid_data_GDP$GDP

# 5.2 疫情期间边际效应（covidduring=1）
grid_data_GDP$marginal_during <- alpha1 + 2 * alpha2 * grid_data_GDP$UTC1500 + alpha4 * grid_data_GDP$GDP


# --------------------------
# 6. （可选）计算边际效应的标准误（误差传播）
# --------------------------
# 构建权重矩阵（每行对应一个网格点的权重）
grid_data_GDP <- grid_data_GDP %>%
  mutate(
    # 疫情前权重：[beta1, beta2, alpha3, alpha4]的权重
    w1_pre = 1,
    w2_pre = 2 * UTC1500,
    w3_pre = GDP,
    w4_pre = 0,
    # 疫情期间权重
    w1_during = 1,
    w2_during = 2 * UTC1500,
    w3_during = 0,
    w4_during = GDP
  )

# 计算疫情前标准误
se_pre <- apply(grid_data_GDP[, c("w1_pre", "w2_pre", "w3_pre", "w4_pre")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_GDP$se_pre <- se_pre

# 计算疫情期间标准误
se_during <- apply(grid_data_GDP[, c("w1_during", "w2_during", "w3_during", "w4_during")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_GDP$se_during <- se_during


# --------------------------
# 5. 计算95%置信区间
# --------------------------
t_low <- qt(0.025, df = df)
t_high <- qt(0.975, df = df)


grid_data_GDP$low_pre<-grid_data_GDP$marginal_pre + grid_data_GDP$se_pre*t_low
grid_data_GDP$up_pre<-grid_data_GDP$marginal_pre + grid_data_GDP$se_pre*t_high

grid_data_GDP$low_during<-grid_data_GDP$marginal_during + grid_data_GDP$se_during*t_low
grid_data_GDP$up_during<-grid_data_GDP$marginal_during + grid_data_GDP$se_during*t_high


############################################################

# 假设要获取"col1"、"col3"、"col5"这三列
##grid_data_GDP$GDP<- grid_data_GDP$GDP+mean_GDP  ## 对去中心化的值进行还原
UTC_GDP_grid_before_during<- grid_data_GDP[, c("UTC1500","GDP", "marginal_pre", "low_pre", "up_pre","marginal_during", "low_during", "up_during")]
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1500/',  'UTC_GDP_grid_before_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_GDP_grid_before_during, save_file_path_before)

################# 获取等于mean_UTC1500的数值位置 85##################

target_UTC1500 <- grid_data_GDP$UTC1500[85]### 选择固定阈值处的数据
# 筛选出UTC等于该值的所有行
sub_UTC_GDP_grid_before_during <- subset(UTC_GDP_grid_before_during, UTC1500 == target_UTC1500)

## before
UTC_GDP_grid_before<- sub_UTC_GDP_grid_before_during[, c("UTC1500","GDP", "marginal_pre", "low_pre","up_pre")]
colnames(UTC_GDP_grid_before) <- c("UTC1500","GDP", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1500/',  'UTC_GDP_meanUTC_before', '.csv', sep="")  # 字符串连接
write.csv(UTC_GDP_grid_before, save_file_path_before)

## during 
UTC_GDP_grid_during<- sub_UTC_GDP_grid_before_during[, c("UTC1500","GDP", "marginal_during", "low_during","up_during")]
colnames(UTC_GDP_grid_during) <- c("UTC1500","GDP", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1500/',  'UTC_GDP_meanUTC_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_GDP_grid_during, save_file_path_before)


###################



####################################UTC与疫情和温度 NOUTC1500 的交互##############################

model1500_interaction10 <- felm(sentiment ~ 1 + UTC1500+UTC1500_2+ UTC1500:NOUTC1500:covid +NOUTC1500 +PM25+
                                  humidity +Precipitation + Wind + cloudcover + + Surface_pressure+Skintemperature +
                                  population1500 + nightlight1500+ settlement1500+GDP
                                | yearmonthday + userid , 
                                data = query_sample2)

print(summary(model1500_interaction10))

#####################保存模型参数#######################
# 捕获第n个模型摘要的输出
model1500_summary <- capture.output(summary(model1500_interaction10))
# 将第n个模型的输出追加到同一个txt文件中
cat(model1500_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)


##################################模型协方差计算#################################


# 提取模型系数
coeffs <-coef(model1500_interaction10) # 获取系数

##calculate and save marginal effects, with errors clustered regionally
cov <- vcovCR(model1500_interaction10,cluster=query_sample2$userid, type='CR0') #子集数据进行协方差求解


####################before COVID19 #######################
####################before COVID19 边界效应 #######################

# --------------------------
# 2. 提取模型参数（同前）
# --------------------------
alpha1 <- coeffs["UTC1500"]                  # -8.0401
alpha2 <- coeffs["UTC1500_2"]                # 9.5740
alpha3 <- coeffs["UTC1500:NOUTC1500:covidbefore"] # 0.0012
alpha4 <- coeffs["UTC1500:NOUTC1500:covidduring"] # 0.0106

# 协方差矩阵（用于标准误，可选，若仅可视化效应值可省略）
var_names <- c("UTC1500", "UTC1500_2", "UTC1500:NOUTC1500:covidbefore", "UTC1500:NOUTC1500:covidduring")
V <- cov[var_names, var_names] 

# 模型自由度（用于置信区间，可选）
df <- df.residual(model1500_interaction10)


# --------------------------
# 3. 构建二维网格（UTC1500 × PM2.5）
# --------------------------
# 3.1 设定两个变量的取值范围（基于原始数据，避免外推过多）
# UTC1500：取原始数据的5%~95%分位数（排除极端值）

UTC_grid <- seq(0, 1, length.out = 1500)  # 100个点

# PM2.5：取原始数据的0%~95%分位数（避免极端污染值）
NOUTC1500_range <- quantile(query_sample2$NOUTC1500, probs = c(0, 1), na.rm = TRUE)
NOUTC1500_grid <- seq(NOUTC1500_range[1], NOUTC1500_range[2], length.out = 1500)  # 100个点

# 3.2 生成二维网格（100×100=15000个组合）
grid_data_NOUTC1500 <- expand.grid(
  UTC1500 = UTC_grid,
  NOUTC1500 = NOUTC1500_grid
)


# --------------------------
# 5. 计算二维网格中每个点的边际效应（分疫情阶段）
# --------------------------
# 5.1 疫情前边际效应（covidbefore=1）
grid_data_NOUTC1500$marginal_pre <- alpha1 + 2 * alpha2 * grid_data_NOUTC1500$UTC1500 + alpha3 * grid_data_NOUTC1500$NOUTC1500

# 5.2 疫情期间边际效应（covidduring=1）
grid_data_NOUTC1500$marginal_during <- alpha1 + 2 * alpha2 * grid_data_NOUTC1500$UTC1500 + alpha4 * grid_data_NOUTC1500$NOUTC1500


# --------------------------
# 6. （可选）计算边际效应的标准误（误差传播）
# --------------------------
# 构建权重矩阵（每行对应一个网格点的权重）
grid_data_NOUTC1500 <- grid_data_NOUTC1500 %>%
  mutate(
    # 疫情前权重：[beta1, beta2, alpha3, alpha4]的权重
    w1_pre = 1,
    w2_pre = 2 * UTC1500,
    w3_pre = NOUTC1500,
    w4_pre = 0,
    # 疫情期间权重
    w1_during = 1,
    w2_during = 2 * UTC1500,
    w3_during = 0,
    w4_during = NOUTC1500
  )

# 计算疫情前标准误
se_pre <- apply(grid_data_NOUTC1500[, c("w1_pre", "w2_pre", "w3_pre", "w4_pre")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_NOUTC1500$se_pre <- se_pre

# 计算疫情期间标准误
se_during <- apply(grid_data_NOUTC1500[, c("w1_during", "w2_during", "w3_during", "w4_during")], 1, function(w) {
  sqrt(as.numeric(t(w) %*% V %*% w))
})
grid_data_NOUTC1500$se_during <- se_during


# --------------------------
# 5. 计算95%置信区间
# --------------------------
t_low <- qt(0.025, df = df)
t_high <- qt(0.975, df = df)


grid_data_NOUTC1500$low_pre<-grid_data_NOUTC1500$marginal_pre + grid_data_NOUTC1500$se_pre*t_low
grid_data_NOUTC1500$up_pre<-grid_data_NOUTC1500$marginal_pre + grid_data_NOUTC1500$se_pre*t_high

grid_data_NOUTC1500$low_during<-grid_data_NOUTC1500$marginal_during + grid_data_NOUTC1500$se_during*t_low
grid_data_NOUTC1500$up_during<-grid_data_NOUTC1500$marginal_during + grid_data_NOUTC1500$se_during*t_high


############################################################

# 假设要获取"col1"、"col3"、"col5"这三列
grid_data_NOUTC1500$NOUTC1500<- grid_data_NOUTC1500$NOUTC1500+mean_NOUTC1500  ## 对去中心化的值进行还原
UTC_NOUTC1500_grid_before_during<- grid_data_NOUTC1500[, c("UTC1500","NOUTC1500", "marginal_pre", "low_pre", "up_pre","marginal_during", "low_during", "up_during")]
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1500/',  'UTC_NOUTC1500_grid_before_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_NOUTC1500_grid_before_during, save_file_path_before)

################# 获取等于mean_UTC1500的数值位置 85##################

target_UTC1500 <- grid_data_NOUTC1500$UTC1500[85]### 选择固定阈值处的数据
# 筛选出UTC等于该值的所有行
sub_UTC_NOUTC1500_grid_before_during <- subset(UTC_NOUTC1500_grid_before_during, UTC1500 == target_UTC1500)

## before
UTC_NOUTC1500_grid_before<- sub_UTC_NOUTC1500_grid_before_during[, c("UTC1500","NOUTC1500", "marginal_pre", "low_pre","up_pre")]
colnames(UTC_NOUTC1500_grid_before) <- c("UTC1500","NOUTC1500", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1500/',  'UTC_NOUTC1500_meanUTC_before', '.csv', sep="")  # 字符串连接
write.csv(UTC_NOUTC1500_grid_before, save_file_path_before)

## during 
UTC_NOUTC1500_grid_during<- sub_UTC_NOUTC1500_grid_before_during[, c("UTC1500","NOUTC1500", "marginal_during", "low_during","up_during")]
colnames(UTC_NOUTC1500_grid_during) <- c("UTC1500","NOUTC1500", "mean_value", "Low_value", "UP_value")
save_file_path_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc1500/',  'UTC_NOUTC1500_meanUTC_during', '.csv', sep="")  # 字符串连接
write.csv(UTC_NOUTC1500_grid_during, save_file_path_before)


###################



############################分层回归分析######################################

### 分别查看 UTC1500 的系数在疫情前和疫情期间的差异

model1500_before <- felm(sentiment ~ 1 + UTC1500 +UTC1500_2+NOUTC1500+
                           Precipitation + humidity + Wind + cloudcover + Surface_pressure +Skintemperature+
                           population1500 + nightlight1500+ settlement1500+ GDP
                         | yearmonthday + userid , 
                         data = subset(query_sample2, covid == "before"))
print(summary(model1500_before))
# 捕获第n个模型摘要的输出
model1500_summary <- capture.output(summary(model1500_before))
# 将第n个模型的输出追加到同一个txt文件中
cat(model1500_summary, file = output_file, sep = "\n", append = TRUE)

#close(con)
model1500_during <- felm(sentiment ~ 1 + UTC1500  +UTC1500_2 +NOUTC1500+
                           Precipitation + humidity + Wind + cloudcover + Surface_pressure +Skintemperature+
                           population1500 + nightlight1500+ settlement1500+ GDP
                         | yearmonthday + userid , 
                         data = subset(query_sample2, covid == "during"))

print(summary(model1500_during))

# 捕获第n个模型摘要的输出
model1500_summary <- capture.output(summary(model1500_during))
# 将第n个模型的输出追加到同一个txt文件中
cat(model1500_summary, file = output_file, sep = "\n", append = TRUE)

close(con)  # 关闭文件










############################################## UTC1500 导出基础模型结果为各种所需表格#############################################

text_code1 <- stargazer(model1500_1, model1500_2, model1500_3, model1500_interaction10,  
                        type = "text", title = " Baseline Model")
write(text_code1, file = "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/secondary_data_utc1500/Baseline_Results_tab_20250923.txt")


latex_code1 <- stargazer(model1500_1, model1500_2, model1500_3, model1500_interaction10,  
                         type = "latex", title = " Baseline Model")
write(latex_code1, file = "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/secondary_data_utc1500/Baseline_Results_tab_latex_20250923.tex")


text_code2 <- stargazer(model1500_before, model1500_during,  
                        type = "text", title = " Hierarchical regression")
write(text_code1, file = "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/secondary_data_utc1500/Hierarchical_Results_tab_20250923.txt")



latex_code2 <- stargazer(model1500_before, model1500_during,  
                         type = "latex", title = "Hierarchical regression")
write(latex_code1, file = "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/secondary_data_utc1500/Hierarchical_Results_tab_latex_20250923.tex")



########### 导出交互项模型结果为各种所需表格#############

text_code31 <- stargazer(model1500_interaction1, model1500_interaction2, model1500_interaction3, 
                         type = "text", title = "Interaction model")
write(text_code31, file = "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/secondary_data_utc1500/Interaction_Results1_tab_20250923.txt")


text_code311 <- stargazer(model1500_interaction4,
                          type = "text", title = "Interaction model")
write(text_code311, file = "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/secondary_data_utc1500/Interaction_Results1-1_tab_20250923.txt")


text_code32 <- stargazer(model1500_interaction5, model1500_interaction6, model1500_interaction7,
                         type = "text", title = "Interaction model")
write(text_code32, file = "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/secondary_data_utc1500/Interaction_Results2_tab_20250923.txt")


text_code322 <- stargazer( model1500_interaction9, 
                           type = "text", title = "Interaction model")
write(text_code322, file = "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/secondary_data_utc1500/Interaction_Results2-2_tab_20250923.txt")


text_code323 <- stargazer( model1500_interaction0, 
                           type = "text", title = "Interaction model")
write(text_code323, file = "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/secondary_data_utc1500/Interaction_Results2-3_tab_20250923.txt")


#########################################latex ########################

latex_code41 <- stargazer( model1500_interaction1, model1500_interaction2, model1500_interaction3,
                           type = "latex", title = "Interaction model")
write(latex_code41, file = "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/secondary_data_utc1500/Interaction_Results1_tab_latex_20250923.tex")


latex_code411 <- stargazer( model1500_interaction4,
                            type = "latex", title = "Interaction model")
write(latex_code41, file = "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/secondary_data_utc1500/Interaction_Results1-1_tab_latex_20250923.tex")


latex_code42 <- stargazer( model1500_interaction5, model1500_interaction6, model1500_interaction7, 
                           type = "latex", title = "Interaction model")
write(latex_code42, file = "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/secondary_data_utc1500/Interaction_Results2_tab_latex_20250923.tex")

text_code422 <- stargazer( model1500_interaction9, 
                           type = "latex", title = "Interaction model")
write(text_code422, file = "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/secondary_data_utc1500/Interaction_Results2-2_tab_latex_20250923.txt")

text_code423 <- stargazer( model1500_interaction0, 
                           type = "latex", title = "Interaction model")
write(text_code423, file = "E:/Sentiment_Brazil/R_codes/Negative_Sentiment_Rcodes_covid19/secondary_data_utc1500/Interaction_Results2-3_tab_latex_20250923.txt")





rm(model1500_1, model1500_2, model1500_3, model1500_interaction10,
   model1500_before, model1500_during,model1500_interaction0,
   model1500_interaction5, model1500_interaction6, model1500_interaction7,
   model1500_interaction9)





