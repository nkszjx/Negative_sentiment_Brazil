

rm(list=ls())  # 这行代码是清空当前工作环境中的所有对象，确保你开始一个干净的工作空间
library(ggpmisc)
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
library(pdftools)
library(magick)       # 处理PDF图片提取和合并
library(cowplot)      # 处理图形组合
library(grid)         # 图形布局处理
library(tidyr)
############################## 读取数据############################################


####################

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

query_sample2 = subset(result0, (freq>30) ) # 过滤tweet文本过少的数据阈值设为：50, 100, 150, 200, 250, 300,
query_sample2 <- na.omit(query_sample2)


unique_users <- n_distinct(query_sample2$userid)
cat("总用户数:", unique_users, "\n")


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
period <- ifelse(query_sample2$year < '2020', "Before Covid-19",
                 ifelse(query_sample2$year == '2020' & (query_sample2$month == '01' | query_sample2$month == '02'), "Before Covid-19", "During Covid-19"))
period <- data.frame(period)  # 将向量转换为数据框
names(period) <- "period"  # 重命名列为 "covid"
query_sample2 <- cbind(query_sample2, period)  # 合并covid列到数据框




# 增加疫情前期中期标记变量 
#query_sample2_meanUTC_before <- subset(query_sample2, covid == "before")
#query_sample2_meanUTC_during <- subset(query_sample2, covid == "during")

#query_sample2_meanUTC_before$period <- "Before Covid-19"
#query_sample2_meanUTC_during$period <- "During Covid-19"

#combined_data <- rbind(query_sample2_meanUTC_before, query_sample2_meanUTC_during)

#################################绘制图形###########################################


befor_covid<-subset(query_sample2, covid == "before")
during_covid<-subset(query_sample2, covid == "during")

# 添加 sentiment 的直方图
ggplot() +
  geom_histogram(data = befor_covid, aes(x = sentiment), fill = "#FFB5C5", color = "black", alpha = 0.85, binwidth = 2)+
  theme_minimal()+ # 使用简洁的主题
  #theme(panel.grid = element_blank()) +  # 去除网格线
  xlab("Negative sentiment values ") +   # X轴标签
  ylab("Counts")+ # Y轴标签
  ggtitle("Before COVID-19 ") +  # 标题
  theme(plot.title = element_text(size = 15, hjust = 0.5, vjust = -1),  # 设置文字的大小和标题居中
        axis.text = element_text(size = 15, color="black"),    # 设置经纬网文字的大小
        axis.title = element_text(size = 15))+
  scale_y_continuous(breaks = seq(0, 2000000, by = 30000),labels = scales::scientific)+
  scale_x_continuous(breaks = seq(0, 100, by = 20)) ->his_tree1819  
print(his_tree1819)



# 添加 sentiment 的直方图
ggplot() +
  geom_histogram(data = during_covid, aes(x = sentiment), fill = "#48D1CC", color = "black", alpha = 0.85, binwidth = 2) +
  theme_minimal()+ # 使用简洁的主题
  #theme(panel.grid = element_blank()) +  # 去除网格线
  xlab("Negative sentiment values") +   # X轴标签
  ylab("Counts")+ # Y轴标签
  ggtitle("During COVID-19") +  # 标题
  theme(plot.title = element_text(size = 15, hjust = 0.5, vjust = -1),  # 设置文字的大小和标题居中
        axis.text = element_text(size = 15, color="black"),    # 设置经纬网文字的大小
        axis.title = element_text(size = 15))+
  scale_y_continuous(breaks = seq(0, 1000000, by = 15000), labels = scales::scientific)+
  scale_x_continuous(breaks = seq(0, 100, by = 20)) ->his_tree202122    # 设置坐标轴文字的大小
print(his_tree202122)



dd <- cowplot::plot_grid(
  cowplot::plot_grid(his_tree1819, labels = " a ", label_size = 21, label_x = -0.01, label_y = 1.0, scale = 1),
  cowplot::plot_grid( his_tree202122 , labels = " b ", label_size = 21, label_x = 0.0, label_y = 1.0, scale = 1),
  nrow = 1, ncol = 2, rel_widths = c( 1, 1))

print(dd)


save_image_path= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'tree_sentiment_histogram20182022', '.pdf', sep="")  # 字符串连接

ggsave(save_image_path, plot = dd, width = 300, height = 100, units = "mm",dpi=200)





##################################################################################
theme0<-theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.text.x = element_text(size = 15),
              axis.text.y = element_text(size = 15),
              axis.title = element_text(size = 15),
              strip.text.x = element_text(size = 15),
              strip.text.y = element_text(),
              strip.background = element_rect( fill="white",colour = "black"))
###############################


################################# UTC-tree relationship before ##################################

# 创建数据框架
data <- data.frame(tree = query_sample2$UTC500, sentiment = query_sample2$sentiment, period=query_sample2$period)
## 获取样本对应的子集数据框
data_meanUTC_before <- subset(data, period == "Before Covid-19")
data_meanUTC_during <- subset(data, period == "During Covid-19")
# 绘制散点图
#mapping=aes(x= tree ,y=sentiment)

###############################

ggplot(data_meanUTC_before, mapping=aes(x= tree, y=sentiment))+
  geom_smooth(
    method = 'lm', #线性回归
    formula = 'y ~ x',
    se=T,#添加置信区间，默认就是T
    lwd=1,#线条宽度
    color = "black", #拟合曲线颜色
    fill="#FFB5C5",alpha=0.5)+  #置信区间颜色
  stat_poly_eq(
    formula = formula(y ~ x),  # 
    aes(label = paste(..eq.label.., sep = "~~~")),
    parse = TRUE,
    label.x = 0.85, label.y = 0.78, #设置位置
    size=5, color = "black"  # 设置字体大小和颜色
  )+
  
  annotate("text",  x=0.3,   y=23.75, size=5,color="black",hjust=0,label = "italic(P[before]) < 0.001", parse = TRUE, parse = TRUE)+
  theme_classic()+theme0+
  
  labs(title = "",y=" ",x="")+
  xlab("") +   # X轴标签
  ylab("Negative sentiment values \n (95% CI)")+ # Y轴标签
  scale_x_continuous(name = " ", limits = c(0, 0.6), breaks = seq(0, 0.6, by =0.15), 
                     labels = scales::number_format(accuracy = 0.01)) +
  theme(plot.margin = margin(t=0.0,b=-1.2,l=0.2,r=0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
        axis.text.x = element_text(color = "black", size = 15)  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
  )->treesentiment_meanUTC_before ## 可视范围限制

print(treesentiment_meanUTC_before)
########
ggplot(data = data_meanUTC_before)+
  geom_histogram(aes(x = tree, y = ..density..), binwidth = 0.0020, fill = "#FFB5C5", color = "#FFB5C5", alpha = 0.5) +
  #geom_density(aes(x=UTC500),fill="#002C5B66", color="white",show.legend = F, adjust = 1.8)+
  scale_x_continuous(name = "UTC coverage", limits = c(0, 0.6), breaks = seq(0, 0.6, by =0.15), 
                     labels = scales::number_format(accuracy = 0.01)) +
  scale_y_continuous(name = "  ", limits = c(0.0, 11.0),
                     breaks = seq(0.0, 11.0, by = 3.0), 
                     labels = scales::number_format(accuracy = 0.1)) +
  #labs(x=expression("UTC500_2"),y="")+
  theme_classic()+theme0+
  theme(plot.margin = margin(t=-0.35,b=0,l=0.2,r=0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
        axis.text.x = element_text(color = "black", size = 15)  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
  )->phUTC_meanUTC_before # 可视范围限制在 0 到 0.5 的范围内

print(phUTC_meanUTC_before)

ptreesentiment_meanUTC_before<-ggarrange(treesentiment_meanUTC_before,phUTC_meanUTC_before,ncol = 1,align = "v",heights = c(2,1))
#cairo_pdf("figure3.pdf",width = 11,height = 4,family = "Calibri")
print(ptreesentiment_meanUTC_before)


save_image_path= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'tree_sentiment_meanUTC_before', '.pdf', sep="")  # 字符串连接

ggsave(save_image_path, plot = ptreesentiment_meanUTC_before, width = 150, height = 150, units = "mm",dpi=200)



################################# UTC-tree relationship during ##################################


ggplot(data_meanUTC_during, mapping=aes(x= tree, y=sentiment))+
  geom_smooth(
    method = 'lm', #线性回归
    formula = 'y ~ x',
    se=T,#添加置信区间，默认就是T
    lwd=1,#线条宽度
    color = "black", #拟合曲线颜色
    fill="#48D1CC",alpha=0.5)+  #置信区间颜色
  stat_poly_eq(
    formula = formula(y ~ x),  # 
    aes(label = paste(..eq.label.., sep = "~~~")),
    parse = TRUE,
    label.x = 0.8, label.y = 0.77, #设置位置
    size=5, color = "black"  # 设置字体大小和颜色
  )+
  annotate("text",  x=0.3, y=35.5, size=5,color="black",hjust=0,label = "italic(P[during]) < 0.001", parse = TRUE, parse = TRUE)+
  theme_classic()+theme0+
  
  labs(title = "",y=" ",x="")+
  xlab("") +   # X轴标签
  ylab("Negative sentiment values \n (95% CI)")+ # Y轴标签
  scale_x_continuous(name = " ", limits = c(0, 0.6), breaks = seq(0, 0.6, by =0.15), 
                     labels = scales::number_format(accuracy = 0.01)) +
  theme(plot.margin = margin(t=0.0,b=-1.2,l=0.2,r=0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
        axis.text.x = element_text(color = "black", size = 15)  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
  )->treesentiment_meanUTC_during ## 可视范围限制

print(treesentiment_meanUTC_during)


########
ggplot(data = data_meanUTC_during)+
  geom_histogram(aes(x = tree, y = ..density..), binwidth = 0.0020, fill = "#48D1CC", color = "#48D1CC", alpha = 0.5) +
  #geom_density(aes(x=UTC500),fill="#002C5B66", color="white",show.legend = F, adjust = 1.8)+
  scale_x_continuous(name = "UTC coverage", limits = c(0, 0.6), breaks = seq(0, 0.6, by =0.15), 
                     labels = scales::number_format(accuracy = 0.01)) +
  scale_y_continuous(name = "  ", limits = c(0.0, 11.0),
                     breaks = seq(0.0, 11.0, by = 3.0), 
                     labels = scales::number_format(accuracy = 0.1)) +
  #labs(x=expression("UTC500_2"),y="")+
  theme_classic()+theme0+
  theme(plot.margin = margin(t=-0.35,b=0,l=0.2,r=0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
        axis.text.x = element_text(color = "black", size = 15)  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
  )->phUTC_meanUTC_during # 可视范围限制在 0 到 0.5 的范围内

print(phUTC_meanUTC_during)

ptreesentiment_meanUTC_during<-ggarrange(treesentiment_meanUTC_during,phUTC_meanUTC_during,ncol = 1,align = "v",heights = c(2,1))
#cairo_pdf("figure3.pdf",width = 11,height = 4,family = "Calibri")
print(ptreesentiment_meanUTC_during)


save_image_path= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'tree_sentiment_meanUTC_during', '.pdf', sep="")  # 字符串连接

ggsave(save_image_path, plot = ptreesentiment_meanUTC_during, width = 150, height = 150, units = "mm",dpi=200)




################################################合并图形###############################################################
#a2 <-  cowplot::plot_grid(ptreesentiment_meanUTC_during,nrow = 2, ncol = 1, labels = c("(b)"),
#label_size = 15, scale=1,label_x = 0.53, label_y = -0.01, rel_heights = c(1.0, 0.1), rel_widths = c(1, 1))

#print(a2)      


a1 <-  cowplot::plot_grid(ptreesentiment_meanUTC_before, nrow = 1, ncol = 1, labels = c("b"),
                          label_size = 18, scale=1, label_x = 0.01, label_y = 1.01,  rel_heights = c(1.0), rel_widths = c(1.0))

print(a1)

a2 <-  cowplot::plot_grid(ptreesentiment_meanUTC_during,nrow = 1, ncol = 1, labels = c("c "),
                          label_size = 18, scale=1,label_x = 0.01, label_y = 1.01, rel_heights = c(1.0), rel_widths = c( 1))

print(a2)                          



pa <- cowplot::plot_grid(a1, NULL, a2, nrow = 1, ncol = 3, align = "h", rel_heights = c(1.0, 1.0, 1.0), rel_widths = c(1, 0.1, 1.0)) # rel_heights 参数图形高度设置
print(pa)


save_image_path= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'tree_sentiment_meanUTC_before_meanUTC_during', '.pdf', sep="")  # 字符串连接

ggsave(save_image_path, plot = pa, width = 300, height = 150, units = "mm",dpi=300)



################################################合并图形###############################################################

dd1 <- cowplot::plot_grid(
  cowplot::plot_grid(his_tree1819, labels = " a ", label_size = 18, label_x = -0.01, label_y = 1.0, scale = 1),
  cowplot::plot_grid( his_tree202122 , labels = " b ", label_size = 18, label_x = 0.0, label_y = 1.0, scale = 1),
  nrow = 1, ncol = 2, rel_widths = c( 1, 1))

print(dd1)


dd2 <- cowplot::plot_grid(
  cowplot::plot_grid(ptreesentiment_meanUTC_before, labels = " c ", label_size = 18, label_x = -0.01, label_y = 1.0, scale = 1),
  cowplot::plot_grid( ptreesentiment_meanUTC_during , labels = " d ", label_size = 18, label_x = 0.0, label_y = 1.0, scale = 1),
  nrow = 1, ncol = 2, rel_widths = c( 1, 1))

print(dd2)


m <- cowplot::plot_grid(dd1, dd2, nrow = 2, ncol = 1, align = "h", rel_heights = c(1.0,  1.0), rel_widths = c(1,  1.0)) # rel_heights 参数图形高度设置
print(m)

save_image_path= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'satatistic_results', '.pdf', sep="")  # 字符串连接

ggsave(save_image_path, plot = m, width = 250, height = 250, units = "mm",dpi=300)




#########################UTC_effects ################################



input_file_name_meanUTC_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_effects_before', '.csv', sep="")  # 字符串连接
effects_UTC500_UTC_meanUTC_before = read.csv(input_file_name_meanUTC_before, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                                     strip.white = FALSE)

input_file_name_meanUTC_during= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_effects_during', '.csv', sep="")  # 字符串连接
effects_UTC500_UTC_meanUTC_during = read.csv(input_file_name_meanUTC_during, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                                     strip.white = FALSE)


###############################绘图################################
###########绘图#################

theme0<-theme(  theme_minimal(base_family = "Arial"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                legend.background = element_rect(fill = "transparent", color = NA),  # 背景透明，无边框
                axis.text.x = element_text(size = 15),
                axis.text.y = element_text(size = 15),
                axis.title = element_text(size = 15),
                strip.text.x = element_text(size = 15),
                strip.text.y = element_text(),
                strip.background = element_rect( fill="white",colour = "black"))
##################################
# 合并数据集，增加一个标记变量 "period" 表示疫情前后
effects_UTC500_UTC_meanUTC_before$period <- "Before Covid-19"
effects_UTC500_UTC_meanUTC_during$period <- "During Covid-19"

combined_data1 <- rbind(effects_UTC500_UTC_meanUTC_before,
                        effects_UTC500_UTC_meanUTC_during)

###################################### 绘制边际效应图和数据分布特征###########################################
#ggplot(data = combined_data1)+
#geom_ribbon(aes(x=UTC, y=mean_value, ymin=Low_value, ymax=UP_value),fill="#ED0021",alpha=0.3)+
#geom_line(aes(x=UTC, y=mean_value),colour="black",size=0.7)+

### 绘图
ggplot(data = combined_data1, aes(x = UTC, y = mean_value, color = period, fill = period)) +
  geom_ribbon(aes(ymin = Low_value, ymax = UP_value), alpha = 0.25, color = NA) +  # 去掉边界线
  geom_line(size = 0.9) +  # 平均值线
  annotate("text",  x=0.3,   y=-0.3, size=6, color="black",hjust=0,label = "italic(P[before]) < 0.001", parse = TRUE, parse = TRUE)+
  annotate("text",  x=0.3, y = -0.7, size=6, color = "black", hjust = 0, label = "italic(P[during]) < 0.001", parse = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # 添加一条红色虚线
  scale_x_continuous(name = " ", limits = c(0, 0.6), breaks = seq(0, 0.6, by =0.15), 
                     labels = scales::number_format(accuracy = 0.01)) +
  scale_y_continuous(name = "Effect on negative sentiment \n  values (95% CI)", 
                     limits = c(-3.5, 1),
                     breaks = seq(-3.5, 0, by = 1), 
                     labels = scales::number_format(accuracy = 0.1)) +
  #scale_color_manual(values = c("Before Covid-19" = "#FFB5C5", "During Covid-19" = "#48D1CC")) + # 填充颜色  # 自定义颜色
  #scale_fill_manual(values = c("Before Covid-19" = "#1F78B466", "During Covid-19" = "#33A02C66")) +
  labs(title = "",y=" ",x="")+
  theme_classic()+theme0+
  theme(plot.margin = margin(t = -0.3, b = -1.2, l = 0.2, r = 0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),
        axis.text.x = element_text(color = "black", size = 15),
        legend.position = c(0.3, 0.9),  # 图例位置
        #legend.position = "none", #设置无legend
        legend.title = element_blank(),  # 隐藏图例标题
        legend.text = element_text(size = 15)
  )->pUTC1 ## 可视范围限制

print(pUTC1)

#######################

# 绘制直方图，边框颜色与填充颜色不同
ggplot(data = query_sample2, aes(x = UTC500, fill = period)) +
  geom_histogram(aes(y = ..density.., color = period), 
                 position = "identity", binwidth = 0.001, size = 0.8, alpha = 0.4, 
                 color = NA) +  # 设置边框颜色为透明
  scale_x_continuous(name = "UTC coverage", 
                     limits = c(0, 0.6), breaks = seq(0, 0.6, by = 0.15), 
                     labels = scales::number_format(accuracy = 0.01)) +
  scale_y_continuous(name = " ", limits = c(0.0, 15), 
                     breaks = seq(0.0, 16, by = 4), 
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_fill_manual(values = c("Before Covid-19" = "#FFB5C5", "During Covid-19" = "#48D1CC")) + # 填充颜色
  #scale_color_manual(values = c("Before Covid-19" = "#08459440",  # 添加透明度（80 示 50% 透明）
  #"During Covid-19" = "#00640040")) + # 添加透明度
  theme_classic() + theme0 +
  theme(plot.margin = margin(t = -0.35, b = 0, l = 0.2, r = 0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),
        axis.text.x = element_text(color = "black", size = 15),
        legend.position = "none")->pUTC2

print(pUTC2)


pUTC<-ggarrange(pUTC1,pUTC2,ncol = 1,align = "v",heights = c(2,1))
#cairo_pdf("figure3.pdf",width = 11,height = 4,family = "Calibri")
print(pUTC)


save_image_path= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_effects', '.pdf', sep="")  # 字符串连接

ggsave(save_image_path, plot = pUTC, width = 150, height = 130, units = "mm",dpi=200)







#########################Marginal effect UTC500 ################################

##疫情前数据集
input_file_name_meanUTC_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_UTC500_before', '.csv', sep="")  # 字符串连接

marginaleffects_UTC_UTC500_meanUTC_before = read.csv(input_file_name_meanUTC_before, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                                             strip.white = FALSE)


##疫情后数据集
input_file_name_meanUTC_during= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_UTC500_during', '.csv', sep="")  # 字符串连接

marginaleffects_UTC_UTC500_meanUTC_during = read.csv(input_file_name_meanUTC_during, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                                             strip.white = FALSE)



###########绘图#################

theme0<-theme(  theme_minimal(base_family = "Arial"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                legend.background = element_rect(fill = "transparent", color = NA),  # 背景透明，无边框
                axis.text.x = element_text(size = 15),
                axis.text.y = element_text(size = 15),
                axis.title = element_text(size = 15),
                strip.text.x = element_text(size = 15),
                strip.text.y = element_text(),
                strip.background = element_rect( fill="white",colour = "black"))

##################################
# 合并数据集，增加一个标记变量 "period" 表示疫情前后
marginaleffects_UTC_UTC500_meanUTC_before$period <- "Before Covid-19"
marginaleffects_UTC_UTC500_meanUTC_during$period <- "During Covid-19"

combined_data1 <- rbind(marginaleffects_UTC_UTC500_meanUTC_before,
                        marginaleffects_UTC_UTC500_meanUTC_during)




### 绘图
ggplot(data = combined_data1, aes(x = UTC500, y = mean_value, color = period, fill = period)) +
  geom_ribbon(aes(ymin = Low_value, ymax = UP_value), alpha = 0.25, color = NA) +  # 去掉边界线
  geom_line(size = 0.9) +  # 平均值线
  annotate("text", x = 0.1, y = 1.0, size = 6, color = "black", hjust = 0, label = "italic(P[before]) < 0.001", parse = TRUE) +
  annotate("text", x = 0.1, y = 2.5, size = 6, color = "black", hjust = 0, label = "italic(P[during]) < 0.001", parse = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # 添加红色虚线
  scale_x_continuous(name = " ", limits = c(0, 0.6), breaks = seq(0, 0.6, by = 0.15),
                     labels = scales::number_format(accuracy = 0.01)) +
  scale_y_continuous(name = "Marginal effect on negative \n sentiment values (95% CI)",
                     limits = c(-12.0, 7), breaks = seq(-12.0, 7, by = 4.0),
                     labels = scales::number_format(accuracy = 0.1)) +
  #scale_color_manual(values = c("Before Covid-19" = "#FFB5C5", "During Covid-19" = "#48D1CC")) + # 填充颜色
  #scale_fill_manual(values = c("Before Covid-19" = "#1F78B466", "During Covid-19" = "#33A02C66")) +
  labs(title = "", y = "Marginal effect on negative \n sentiment values (95% CI)", x = "Skin Temperature") +
  theme_classic() + theme0 +
  theme(plot.margin = margin(t = -0.3, b = -1.2, l = 0.2, r = 0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),
        axis.text.x = element_text(color = "black", size = 15),
        #legend.position = c(0.3, 0.9),  # 图例位置
        legend.position = "none" #设置无legend
        #legend.title = element_blank(),  # 隐藏图例标题
        #legend.text = element_text(size = 15)
  )->pUTC5001  # 图例字体大小

print(pUTC5001)

#############################################################

# 绘制直方图，边框颜色与填充颜色不同
ggplot(data = query_sample2, aes(x = UTC500, fill = period)) +
  geom_histogram(aes(y = ..density.., color = period), 
                 position = "identity", binwidth = 0.001, size = 0.8, alpha = 0.4, 
                 color = NA) +  # 设置边框颜色为透明
  scale_x_continuous(name = "UTC coverage", 
                     limits = c(0, 0.6), breaks = seq(0, 0.6, by = 0.15),
                     labels = scales::number_format(accuracy = 0.01)) +
  scale_y_continuous(name = " ", limits = c(0.0, 15), 
                     breaks = seq(0.0, 15, by = 4), 
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_fill_manual(values = c("Before Covid-19" = "#FFB5C5", "During Covid-19" = "#48D1CC")) + # 填充颜色
  #scale_color_manual(values = c("Before Covid-19" = "#08459440",  # 添加透明度（80 表示 50% 透明）
  #"During Covid-19" = "#00640040")) + # 添加透明度
  theme_classic() + theme0 +
  theme(plot.margin = margin(t = -0.35, b = 0, l = 0.2, r = 0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),
        axis.text.x = element_text(color = "black", size = 15),
        legend.position = "none")->pUTC5002

print(pUTC5002)


pUTC500<-ggarrange(pUTC5001, pUTC5002,ncol = 1,align = "v",heights = c(2,1))
print(pUTC500)


save_image_path= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_UTC500', '.pdf', sep="")  # 字符串连接

ggsave(save_image_path, plot = pUTC500, width = 150, height = 150, units = "mm",dpi=200)






################################################合并图形###############################################################
a1 <-  cowplot::plot_grid(pUTC,nrow = 1, ncol = 1, labels = c("a"),
                          label_size = 15, scale=1, label_x = 0.1, label_y = 1.0,  rel_heights = c(1.0, 0.1), rel_widths = c(1, 1))

print(a1)

a2 <-  cowplot::plot_grid(pUTC500,nrow = 1, ncol = 1, labels = c("b"),
                          label_size = 15, scale=1,label_x = 0.1, label_y = 1.0, rel_heights = c(1.0, 0.1), rel_widths = c(1, 1))

print(a2)                          



pa <- cowplot::plot_grid(a1, NULL, a2, nrow = 1, ncol = 3, align = "h", rel_heights = c(1.0, 1.0, 1.0), rel_widths = c(1, 0.1, 1.0)) # rel_heights 参数图形高度设置
print(pa)


save_image_path= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_effect_Marginaleffect', '.pdf', sep="")  # 字符串连接

ggsave(save_image_path, plot = pa, width = 300, height = 150, units = "mm",dpi=300)







#########################Marginal effect NOUTC500 ################################

##疫情前数据集
input_file_name_meanUTC_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_NOUTC500_meanUTC_before', '.csv', sep="")  # 字符串连接

marginaleffects_UTC_NOUTC500_meanUTC_before = read.csv(input_file_name_meanUTC_before, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                                               strip.white = FALSE)


##疫情后数据集
input_file_name_meanUTC_during= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_NOUTC500_meanUTC_during', '.csv', sep="")  # 字符串连接

marginaleffects_UTC_NOUTC500_meanUTC_during = read.csv(input_file_name_meanUTC_during, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                                               strip.white = FALSE)



###########绘图#################

theme0<-theme(  theme_minimal(base_family = "Arial"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                legend.background = element_rect(fill = "transparent", color = NA),  # 背景透明，无边框
                axis.text.x = element_text(size = 15),
                axis.text.y = element_text(size = 15),
                axis.title = element_text(size = 15),
                strip.text.x = element_text(size = 15),
                strip.text.y = element_text(),
                strip.background = element_rect( fill="white",colour = "black"))

##################################
# 合并数据集，增加一个标记变量 "period" 表示疫情前后
marginaleffects_UTC_NOUTC500_meanUTC_before$period <- "Before Covid-19"
marginaleffects_UTC_NOUTC500_meanUTC_during$period <- "During Covid-19"

combined_data1 <- rbind(marginaleffects_UTC_NOUTC500_meanUTC_before,
                        marginaleffects_UTC_NOUTC500_meanUTC_during)

### 绘图
ggplot(data = combined_data1, aes(x = NOUTC500, y = mean_value, color = period, fill = period)) +
  geom_ribbon(aes(ymin = Low_value, ymax = UP_value), alpha = 0.25, color = NA) +  # 去掉边界线
  geom_line(size = 0.9) +  # 平均值线 
  annotate("text", x = 0.1, y = -1.0, size = 6, color = "black", hjust = 0, label = "italic(P[before]) < 0.001", parse = TRUE) +
  annotate("text", x = 0.1, y = -3.5, size = 6, color = "black", hjust = 0, label = "italic(P[during]) < 0.001", parse = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # 添加红色虚线
  scale_x_continuous(name = " ", limits = c(0, 0.6), breaks = seq(0, 0.6, by = 0.15),
                     labels = scales::number_format(accuracy = 0.01)) +
  scale_y_continuous(name = "Marginal effect on negative \n sentiment values (95% CI)",
                     limits = c(-20.0, 3), breaks = seq(-20.0, 3, by = 5.0),
                     labels = scales::number_format(accuracy = 0.1)) +
  #scale_color_manual(values = c("Before Covid-19" = "#FFB5C5", "During Covid-19" = "#48D1CC")) + # 填充颜色  # 自定义颜色
  #scale_fill_manual(values = c("Before Covid-19" = "#1F78B466", "During Covid-19" = "#33A02C66")) +
  labs(title = "", y = "Marginal effect on negative \n sentiment values (95% CI)", x = "Skin Temperature") +
  theme_classic() + theme0 +
  theme(plot.margin = margin(t = -0.3, b = -1.2, l = 0.2, r = 0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),
        axis.text.x = element_text(color = "black", size = 15),
        #legend.position = c(0.3, 0.9),  # 图例位置
        legend.position = "none", #设置无legend
        legend.title = element_blank(),  # 隐藏图例标题
        legend.text = element_text(size = 15)
  )->pNOUTC5001  # 图例字体大小


print(pNOUTC5001)

#############################################################

# 绘制直方图，边框颜色与填充颜色不同
ggplot(data = query_sample2, aes(x = NOUTC500, fill = period)) +
  geom_histogram(aes(y = ..density.., color = period), 
                 position = "identity", binwidth = 0.001, size = 0.8, alpha = 0.4, 
                 color = NA) +  # 设置边框颜色为透明
  scale_x_continuous(name = "NTGS coverage", 
                     limits = c(0, 0.6), breaks = seq(0, 0.6, by = 0.15), 
                     labels = scales::number_format(accuracy = 0.01)) +
  scale_y_continuous(name = " ", limits = c(0.0, 15), 
                     breaks = seq(0.0, 15, by = 4), 
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_fill_manual(values = c("Before Covid-19" = "#FFB5C5", "During Covid-19" = "#48D1CC")) + # 填充颜色 # 填充颜色
  #scale_color_manual(values = c("Before Covid-19" = "#08459440",  # 添加透明度（80 表示 50% 透明）
  #"During Covid-19" = "#00640040")) + # 添加透明度
  theme_classic() + theme0 +
  theme(plot.margin = margin(t = -0.35, b = 0, l = 0.2, r = 0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),
        axis.text.x = element_text(color = "black", size = 15),
        legend.position = "none")->pNOUTC5002

print(pNOUTC5002)


pNOUTC500<-ggarrange(pNOUTC5001, pNOUTC5002,ncol = 1,align = "v",heights = c(2,1))
print(pNOUTC500)


save_image_path= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_NOUTC500', '.pdf', sep="")  # 字符串连接

ggsave(save_image_path, plot = pNOUTC500, width = 150, height = 150, units = "mm",dpi=200)







################################################合并图形###############################################################


aa <-  cowplot::plot_grid(pUTC,nrow = 1, ncol = 1, labels = c("a"),
                          label_size = 18, scale=1,label_x = 0.02, label_y = 1.0, rel_widths = c(1, 1))
bb <-  cowplot::plot_grid(pUTC500,nrow = 1, ncol = 1, labels = c("b"),
                          label_size = 18, scale=1,label_x = 0.02, label_y = 1.0, rel_widths = c(1, 1))
cc <-  cowplot::plot_grid(pNOUTC500,nrow = 1, ncol = 1, labels = c("c"),
                          label_size = 18, scale=1,label_x = 0.02, label_y = 1.0, rel_widths = c(1, 1))

p1 <- cowplot::plot_grid(bb, NULL, cc, nrow = 1, ncol = 3, align = "h", rel_heights = c(1.0, 1.0, 1.0), rel_widths = c(1, 0.1, 1.0)) # rel_heights 参数图形高度设置
print(p1)
p2 <- cowplot::plot_grid(NULL, aa, NULL, nrow = 1, ncol = 3, align = "h", rel_heights = c(1.0, 1.0, 1.0), rel_widths = c(0.4, 1.0, 0.4)) # rel_heights 参数图形高度设置
print(p2)
p3 <- cowplot::plot_grid(p2, p1, nrow = 2, ncol = 1, align = "h", rel_heights = c(1.0), rel_widths = c(1.0)) # rel_heights 参数图形高度设置
print(p3)

save_image_path= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC500_effect_Marginal_effect', '.pdf', sep="")  # 字符串连接
ggsave(save_image_path, plot = p3, width = 250, height = 250, units = "mm",dpi=200)


##################################################################################################






#########################grid Marginal effect NOUTC500 ################################

##疫情前数据集
input_file_name= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_NOUTC500_grid_before_during', '.csv', sep="")  # 字符串连接

marginaleffects_UTC_NOUTC500_grid_before_during = read.csv(input_file_name, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                                                       strip.white = FALSE)

# 7. 可视化：二维热力图（分疫情阶段）
# --------------------------
# 7.1 数据整理：将宽格式转为长格式（便于分面）
plot_data <- marginaleffects_UTC_NOUTC500_grid_before_during %>%
  select(UTC500, NOUTC500, marginal_pre, marginal_during) %>%
  pivot_longer(
    cols = starts_with("marginal"),
    names_to = "period",
    values_to = "marginal_effect",
    names_prefix = "marginal_"
  ) %>%
  mutate(
    period = case_when(
      period == "pre" ~ "Before Covid-19",
      period == "during" ~ "During Covid-19"
    )
  ) 


# 7.2 绘制热力图（填充色=边际效应大小，等高线=效应等值线）
ggplot(plot_data, aes(x = UTC500, y = NOUTC500)) +
  geom_tile(aes(fill = marginal_effect), alpha = 1.0) +
  geom_contour(aes(z = marginal_effect), color = "white", size = 1.0, breaks = seq(-10, 10, 5)) +
  metR::geom_text_contour(aes(z = marginal_effect), color = "white", size = 3, breaks = seq(-10, 10, 5)) +
  facet_wrap(~period, ncol =2) +
  # 移除midpoint参数，使用默认viridis色阶
  scale_fill_viridis_c(
    option = "inferno",  # 可选："viridis", "magma", "inferno", "plasma",  "rocket", "mako", "turbo"
    name = "Marginal Effects",
    limits = c(min(plot_data$marginal_effect), max(plot_data$marginal_effect))
  ) +
  labs(
    x = "UTC500",
    y = "NOUTC500",
    title =""
    #title = "The marginal effects of trees varying based on levels of NOUTC500 (buffer of 500 m)"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.key.width = unit(3, "cm")
  )

















################################### climatic factors ##############################################



#########################Marginal effect Precipitation ################################

##疫情前数据集
input_file_name_meanUTC_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_Precipitation_meanUTC_before', '.csv', sep="")  # 字符串连接

marginaleffects_UTC_Precipitation_meanUTC_before = read.csv(input_file_name_meanUTC_before, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                                                    strip.white = FALSE)


##疫情后数据集
input_file_name_meanUTC_during= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_Precipitation_meanUTC_during', '.csv', sep="")  # 字符串连接

marginaleffects_UTC_Precipitation_meanUTC_during = read.csv(input_file_name_meanUTC_during, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                                                    strip.white = FALSE)



###########绘图#################

theme0<-theme(  theme_minimal(base_family = "Arial"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                legend.background = element_rect(fill = "transparent", color = NA),  # 背景透明，无边框
                axis.text.x = element_text(size = 15),
                axis.text.y = element_text(size = 15),
                axis.title = element_text(size = 15),
                strip.text.x = element_text(size = 15),
                strip.text.y = element_text(),
                strip.background = element_rect( fill="white",colour = "black"))

##################################
# 合并数据集，增加一个标记变量 "period" 表示疫情前后
marginaleffects_UTC_Precipitation_meanUTC_before$period <- "Before Covid-19"
marginaleffects_UTC_Precipitation_meanUTC_during$period <- "During Covid-19"

combined_data1 <- rbind(marginaleffects_UTC_Precipitation_meanUTC_before,
                        marginaleffects_UTC_Precipitation_meanUTC_during)

### 绘图
ggplot(data = combined_data1, aes(x = Precipitation, y = mean_value, color = period, fill = period)) +
  geom_ribbon(aes(ymin = Low_value, ymax = UP_value), alpha = 0.25, color = NA) +  # 去掉边界线
  geom_line(size = 0.9) +  # 平均值线
  annotate("text", x = 5, y = -0.5, size = 6, color = "black", hjust = 0, label = "italic(P[before]) < 0.001", parse = TRUE) +
  annotate("text", x = 5, y = -1.5, size = 6, color = "black", hjust = 0, label = "italic(P[during])< 0.001", parse = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # 添加红色虚线
  scale_x_continuous(name = " ", limits = c(0, 45), breaks = seq(0, 45, by =11.0),
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(name = "Marginal effect on negative \n sentiment values (95% CI)",
                     limits = c(-6.0, 1), breaks = seq(-6.0, 1, by = 1.5),
                     labels = scales::number_format(accuracy = 0.1)) +
  #scale_color_manual(values = c("Before Covid-19" = "#FFB5C5", "During Covid-19" = "#48D1CC")) + # 填充颜色  # 自定义颜色
  #scale_fill_manual(values = c("Before Covid-19" = "#1F78B466", "During Covid-19" = "#33A02C66")) +
  labs(title = "", y = "Marginal effect on negative \n sentiment values (95% CI)", x = "Skin Temperature") +
  theme_classic() + theme0 +
  theme(plot.margin = margin(t = -0.3, b = -1.2, l = 0.2, r = 0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),
        axis.text.x = element_text(color = "black", size = 15),
        legend.position = c(0.3, 0.9),  # 图例位置
        #legend.position = "none", #设置无legend
        legend.title = element_blank(),  # 隐藏图例标题
        legend.text = element_text(size = 15)
  )->pPrecipitation1  # 图例字体大小


print(pPrecipitation1)

#############################################################

# 绘制直方图，边框颜色与填充颜色不同
ggplot(data = query_sample2, aes(x = Precipitation, fill = period)) +
  geom_histogram(aes(y = ..density.., color = period), 
                 position = "identity", binwidth = 0.05, size = 0.8, alpha = 0.4, 
                 color = NA) +  # 设置边框颜色为透明
  scale_x_continuous(name = expression(Precipitation(mm/day)), 
                     limits = c(0, 45), breaks = seq(0, 45, by =11.0),
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(name = " ", limits = c(0.0, 0.35), 
                     breaks = seq(0.0, 0.35, by = 0.1), 
                     labels = scales::number_format(accuracy = 0.01)) +
  scale_fill_manual(values = c("Before Covid-19" = "#FFB5C5", "During Covid-19" = "#48D1CC")) + # 填充颜色 # 填充颜色
  #scale_color_manual(values = c("Before Covid-19" = "#08459440",  # 添加透明度（80 表示 50% 透明）
  #"During Covid-19" = "#00640040")) + # 添加透明度
  theme_classic() + theme0 +
  theme(plot.margin = margin(t = -0.35, b = 0, l = 0.2, r = 0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),
        axis.text.x = element_text(color = "black", size = 15),
        legend.position = "none")->pPrecipitation2

print(pPrecipitation2)


pPrecipitation<-ggarrange(pPrecipitation1, pPrecipitation2,ncol = 1,align = "v",heights = c(2,1))
print(pPrecipitation)


save_image_path= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_Precipitation', '.pdf', sep="")  # 字符串连接

ggsave(save_image_path, plot = pPrecipitation, width = 150, height = 150, units = "mm",dpi=200)






#########################Marginal effect humidity ################################

##疫情前数据集
input_file_name_meanUTC_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_humidity_meanUTC_before', '.csv', sep="")  # 字符串连接

marginaleffects_UTC_humidity_meanUTC_before = read.csv(input_file_name_meanUTC_before, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                                               strip.white = FALSE)


##疫情后数据集
input_file_name_meanUTC_during= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_humidity_meanUTC_during', '.csv', sep="")  # 字符串连接

marginaleffects_UTC_humidity_meanUTC_during = read.csv(input_file_name_meanUTC_during, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                                               strip.white = FALSE)



###########绘图#################

theme0<-theme(  theme_minimal(base_family = "Arial"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                legend.background = element_rect(fill = "transparent", color = NA),  # 背景透明，无边框
                axis.text.x = element_text(size = 15),
                axis.text.y = element_text(size = 15),
                axis.title = element_text(size = 15),
                strip.text.x = element_text(size = 15),
                strip.text.y = element_text(),
                strip.background = element_rect( fill="white",colour = "black"))

##################################
# 合并数据集，增加一个标记变量 "period" 表示疫情前后
marginaleffects_UTC_humidity_meanUTC_before$period <- "Before Covid-19"
marginaleffects_UTC_humidity_meanUTC_during$period <- "During Covid-19"

combined_data1 <- rbind(marginaleffects_UTC_humidity_meanUTC_before,
                        marginaleffects_UTC_humidity_meanUTC_during)

### 绘图
ggplot(data = combined_data1, aes(x = humidity, y = mean_value, color = period, fill = period)) +
  geom_ribbon(aes(ymin = Low_value, ymax = UP_value), alpha = 0.25, color = NA) +  # 去掉边界线
  geom_line(size = 0.9) +  # 平均值线
  annotate("text", x = 30, y = -1.0, size = 6, color = "black", hjust = 0, label = "italic(P[before]) > 0.1", parse = TRUE) +
  annotate("text", x = 30, y = -2.5, size = 6, color = "black", hjust = 0, label = "italic(P[during]) < 0.001", parse = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # 添加红色虚线
  scale_x_continuous(name = " ", limits = c(15, 100), breaks = seq(15, 100, by =21),   
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(name = "Marginal effect on negative \n sentiment values (95% CI)",
                     limits = c(-12.0, 2), breaks = seq(-12.0, 2, by = 3.0),
                     labels = scales::number_format(accuracy = 0.1)) +
  #scale_color_manual(values = c("Before Covid-19" = "#FFB5C5", "During Covid-19" = "#48D1CC")) + # 填充颜色  # 自定义颜色
  #scale_fill_manual(values = c("Before Covid-19" = "#1F78B466", "During Covid-19" = "#33A02C66")) +
  labs(title = "", y = "Marginal effect on negative \n sentiment values (95% CI)", x = "Skin Temperature") +
  theme_classic() + theme0 +
  theme(plot.margin = margin(t = -0.3, b = -1.2, l = 0.2, r = 0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),
        axis.text.x = element_text(color = "black", size = 15),
        #legend.position = c(0.3, 0.9),  # 图例位置
        legend.position = "none", #设置无legend
        legend.title = element_blank(),  # 隐藏图例标题
        legend.text = element_text(size = 15)
  )->phumidity1  # 图例字体大小


print(phumidity1)

#############################################################

# 绘制直方图，边框颜色与填充颜色不同
ggplot(data = query_sample2, aes(x = humidity, fill = period)) +
  geom_histogram(aes(y = ..density.., color = period), 
                 position = "identity", binwidth = 0.003, size = 0.8, alpha = 0.4, 
                 color = NA) +  # 设置边框颜色为透明
  scale_x_continuous(name = "Humidity(%)", limits = c(15, 100), breaks = seq(15, 100, by = 21), 
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(name = " ", limits = c(0.0, 0.2), 
                     breaks = seq(0.0, 0.2, by = 0.08), 
                     labels = scales::number_format(accuracy = 0.01)) +
  scale_fill_manual(values = c("Before Covid-19" = "#FFB5C5", "During Covid-19" = "#48D1CC")) + # 填充颜色 # 填充颜色
  #scale_color_manual(values = c("Before Covid-19" = "#08459440",  # 添加透明度（80 表示 50% 透明）
  #"During Covid-19" = "#00640040")) + # 添加透明度
  theme_classic() + theme0 +
  theme(plot.margin = margin(t = -0.35, b = 0, l = 0.2, r = 0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),
        axis.text.x = element_text(color = "black", size = 15),
        legend.position = "none")->phumidity2

print(phumidity2)


phumidity<-ggarrange(phumidity1, phumidity2,ncol = 1,align = "v",heights = c(2,1))
print(phumidity)


save_image_path= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_humidity', '.pdf', sep="")  # 字符串连接

ggsave(save_image_path, plot = phumidity, width = 150, height = 150, units = "mm",dpi=200)




#########################Marginal effect Surface_pressure ################################

##疫情前数据集
input_file_name_meanUTC_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_Surface_pressure_meanUTC_before', '.csv', sep="")  # 字符串连接

marginaleffects_UTC_Surface_pressure_meanUTC_before = read.csv(input_file_name_meanUTC_before, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                                                       strip.white = FALSE)


##疫情后数据集
input_file_name_meanUTC_during= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_Surface_pressure_meanUTC_during', '.csv', sep="")  # 字符串连接

marginaleffects_UTC_Surface_pressure_meanUTC_during = read.csv(input_file_name_meanUTC_during, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                                                       strip.white = FALSE)



###########绘图#################

theme0<-theme(  theme_minimal(base_family = "Arial"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                legend.background = element_rect(fill = "transparent", color = NA),  # 背景透明，无边框
                axis.text.x = element_text(size = 15),
                axis.text.y = element_text(size = 15),
                axis.title = element_text(size = 15),
                strip.text.x = element_text(size = 15),
                strip.text.y = element_text(),
                strip.background = element_rect( fill="white",colour = "black"))

##################################
# 合并数据集，增加一个标记变量 "period" 表示疫情前后
marginaleffects_UTC_Surface_pressure_meanUTC_before$period <- "Before Covid-19"
marginaleffects_UTC_Surface_pressure_meanUTC_during$period <- "During Covid-19"

combined_data1 <- rbind(marginaleffects_UTC_Surface_pressure_meanUTC_before,
                        marginaleffects_UTC_Surface_pressure_meanUTC_during)

### 绘图
ggplot(data = combined_data1, aes(x = Surface_pressure, y = mean_value, color = period, fill = period)) +
  geom_ribbon(aes(ymin = Low_value, ymax = UP_value), alpha = 0.25, color = NA) +  # 去掉边界线
  geom_line(size = 0.9) +  # 平均值线
  annotate("text", x = 91.5, y = -1.0, size = 6, color = "black", hjust = 0, label = "italic(P[before]) < 0.001", parse = TRUE) +
  annotate("text", x = 91.5, y = -2.5, size = 6, color = "black", hjust = 0, label = "italic(P[during]) < 0.001", parse = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # 添加红色虚线
  scale_x_continuous(name = " ", limits =  c(90, 102), breaks = seq(90, 102, by =3), 
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(name = "Marginal effect on negative \n sentiment values (95% CI)",
                     limits = c(-9.0, 0.5), breaks = seq(-9.0, 0.5, by = 2.5),
                     labels = scales::number_format(accuracy = 0.1)) +
  #scale_color_manual(values = c("Before Covid-19" = "#FFB5C5", "During Covid-19" = "#48D1CC")) + # 填充颜色  # 自定义颜色
  #scale_fill_manual(values = c("Before Covid-19" = "#1F78B466", "During Covid-19" = "#33A02C66")) +
  labs(title = "", y = "Marginal effect on negative \n sentiment values (95% CI)", x = "Skin Temperature") +
  theme_classic() + theme0 +
  theme(plot.margin = margin(t = -0.3, b = -1.2, l = 0.2, r = 0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),
        axis.text.x = element_text(color = "black", size = 15),
        #legend.position = c(0.3, 0.9),  # 图例位置
        legend.position = "none", #设置无legend
        legend.title = element_blank(),  # 隐藏图例标题
        legend.text = element_text(size = 15)
  )->pSurface_pressure1  # 图例字体大小


print(pSurface_pressure1)

#############################################################

# 绘制直方图，边框颜色与填充颜色不同
ggplot(data = query_sample2, aes(x = Surface_pressure, fill = period)) +
  geom_histogram(aes(y = ..density.., color = period), 
                 position = "identity", binwidth = 0.001, size = 0.8, alpha = 0.4, 
                 color = NA) +  # 设置边框颜色为透明
  scale_x_continuous(expression(Surface~Pressure~(Pa~"×"~1000)),
                     limits = c(90, 102), breaks = seq(90, 102, by =3), 
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(name = " ", limits = c(0.0, 1), 
                     breaks = seq(0.0, 1, by = 0.3), 
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_fill_manual(values = c("Before Covid-19" = "#FFB5C5", "During Covid-19" = "#48D1CC")) + # 填充颜色 # 填充颜色
  #scale_color_manual(values = c("Before Covid-19" = "#08459440",  # 添加透明度（80 表示 50% 透明）
  #"During Covid-19" = "#00640040")) + # 添加透明度
  theme_classic() + theme0 +
  theme(plot.margin = margin(t = -0.35, b = 0, l = 0.2, r = 0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),
        axis.text.x = element_text(color = "black", size = 15),
        legend.position = "none")->pSurface_pressure2

print(pSurface_pressure2)


pSurface_pressure<-ggarrange(pSurface_pressure1, pSurface_pressure2,ncol = 1,align = "v",heights = c(2,1))
print(pSurface_pressure)


save_image_path= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_Surface_pressure', '.pdf', sep="")  # 字符串连接

ggsave(save_image_path, plot = pSurface_pressure, width = 150, height = 150, units = "mm",dpi=200)






#########################Marginal effect Skintemperature ################################

##疫情前数据集
input_file_name_meanUTC_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_Skintemperature_meanUTC_before', '.csv', sep="")  # 字符串连接

marginaleffects_UTC_Skintemperature_meanUTC_before = read.csv(input_file_name_meanUTC_before, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                                                      strip.white = FALSE)


##疫情后数据集
input_file_name_meanUTC_during= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_Skintemperature_meanUTC_during', '.csv', sep="")  # 字符串连接

marginaleffects_UTC_Skintemperature_meanUTC_during = read.csv(input_file_name_meanUTC_during, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                                                      strip.white = FALSE)



###########绘图#################

theme0<-theme(  theme_minimal(base_family = "Arial"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                legend.background = element_rect(fill = "transparent", color = NA),  # 背景透明，无边框
                axis.text.x = element_text(size = 15),
                axis.text.y = element_text(size = 15),
                axis.title = element_text(size = 15),
                strip.text.x = element_text(size = 15),
                strip.text.y = element_text(),
                strip.background = element_rect( fill="white",colour = "black"))

##################################
# 合并数据集，增加一个标记变量 "period" 表示疫情前后
marginaleffects_UTC_Skintemperature_meanUTC_before$period <- "Before Covid-19"
marginaleffects_UTC_Skintemperature_meanUTC_during$period <- "During Covid-19"

combined_data1 <- rbind(marginaleffects_UTC_Skintemperature_meanUTC_before,
                        marginaleffects_UTC_Skintemperature_meanUTC_during)

### 绘图
ggplot(data = combined_data1, aes(x = Skintemperature, y = mean_value, color = period, fill = period)) +
  geom_ribbon(aes(ymin = Low_value, ymax = UP_value), alpha = 0.25, color = NA) +  # 去掉边界线
  geom_line(size = 0.9) +  # 平均值线
  annotate("text", x = 10, y = -1.0, size = 6, color = "black", hjust = 0, label = "italic(P[before]) < 0.001", parse = TRUE) +
  annotate("text", x = 10, y = -2.5, size = 6, color = "black", hjust = 0, label = "italic(P[during]) < 0.001", parse = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # 添加红色虚线
  scale_x_continuous(name = " ", limits = c(5, 45), breaks = seq(5, 45, by =10),  
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(name = "Marginal effect on negative \n sentiment values (95% CI)",
                     limits = c(-12.0, 2), breaks = seq(-12.0, 2, by = 3.0),
                     labels = scales::number_format(accuracy = 0.1)) +
  #scale_color_manual(values = c("Before Covid-19" = "#FFB5C5", "During Covid-19" = "#48D1CC")) + # 填充颜色  # 自定义颜色
  #scale_fill_manual(values = c("Before Covid-19" = "#1F78B466", "During Covid-19" = "#33A02C66")) +
  labs(title = "", y = "Marginal effect on negative \n sentiment values (95% CI)", x = "Skin Temperature") +
  theme_classic() + theme0 +
  theme(plot.margin = margin(t = -0.3, b = -1.2, l = 0.2, r = 0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),
        axis.text.x = element_text(color = "black", size = 15),
        #legend.position = c(0.3, 0.9),  # 图例位置
        legend.position = "none", #设置无legend
        legend.title = element_blank(),  # 隐藏图例标题
        legend.text = element_text(size = 15)
  )->pSkintemperature1  # 图例字体大小


print(pSkintemperature1)

#############################################################

# 绘制直方图，边框颜色与填充颜色不同
ggplot(data = query_sample2, aes(x = Skintemperature, fill = period)) +
  geom_histogram(aes(y = ..density.., color = period), 
                 position = "identity", binwidth = 0.001, size = 0.8, alpha = 0.4, 
                 color = NA) +  # 设置边框颜色为透明
  scale_x_continuous(name =expression(Temperature~(degree*C)), 
                     limits = c(5, 45), breaks = seq(5, 45, by =10), 
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(name = " ", limits = c(0.0, 0.32), 
                     breaks = seq(0.0, 0.32, by = 0.1), 
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_fill_manual(values = c("Before Covid-19" = "#FFB5C5", "During Covid-19" = "#48D1CC")) + # 填充颜色 # 填充颜色
  #scale_color_manual(values = c("Before Covid-19" = "#08459440",  # 添加透明度（80 表示 50% 透明）
  #"During Covid-19" = "#00640040")) + # 添加透明度
  theme_classic() + theme0 +
  theme(plot.margin = margin(t = -0.35, b = 0, l = 0.2, r = 0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),
        axis.text.x = element_text(color = "black", size = 15),
        legend.position = "none")->pSkintemperature2

print(pSkintemperature2)


pSkintemperature<-ggarrange(pSkintemperature1, pSkintemperature2,ncol = 1,align = "v",heights = c(2,1))
print(pSkintemperature)


save_image_path= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_Skintemperature', '.pdf', sep="")  # 字符串连接

ggsave(save_image_path, plot = pSkintemperature, width = 150, height = 150, units = "mm",dpi=200)





################################################合并图形###############################################################
cc <-  cowplot::plot_grid(pPrecipitation,nrow = 1, ncol = 1, labels = c("a"),
                          label_size = 18, scale=1,label_x = 0.02, label_y = 1.0, rel_widths = c(1, 1))

dd <-  cowplot::plot_grid(phumidity,nrow = 1, ncol = 1, labels = c("b"),
                          label_size = 18, scale=1,label_x = 0.02, label_y = 1.0, rel_widths = c(1, 1))


ee <-  cowplot::plot_grid(pSurface_pressure,nrow = 1, ncol = 1, labels = c("c"),
label_size = 18, scale=1,label_x = 0.02, label_y = 1.0, rel_widths = c(1, 1))

ff <-  cowplot::plot_grid(pSkintemperature,nrow = 1, ncol = 1, labels = c("d"),
                          label_size = 18, scale=1,label_x = 0.02, label_y = 1.0, rel_widths = c(1, 1))




p3 <- cowplot::plot_grid(cc, NULL, dd, nrow = 1, ncol = 3, align = "h", rel_heights = c(1.0, 1.0, 1.0), rel_widths = c(1.0, 0.02, 1.0)) # rel_heights 参数图形高度设置
print(p3)

p4 <- cowplot::plot_grid(ee,NULL, ff, nrow = 1, ncol = 3, align = "h", rel_heights = c(1.0, 1.0, 1.0), rel_widths = c(1.0, 0.02, 1.0)) # rel_heights 参数图形高度设置
print(p4)


p5 <- cowplot::plot_grid(p3, p4, nrow = 2, ncol = 1, align = "h", rel_heights = c(1.0, 1.0), rel_widths = c(1, 1.0)) # rel_heights 参数图形高度设置
print(p5)


save_image_path= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC500_climate_Marginal_effect', '.pdf', sep="")  # 字符串连接

ggsave(save_image_path, plot = p5, width = 250, height = 250, units = "mm",dpi=200)







#############################################Urban environment factors ###################################################################





#########################Marginal effect population500 ################################

##疫情前数据集
input_file_name_meanUTC_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_population500_meanUTC_before', '.csv', sep="")  # 字符串连接

marginaleffects_UTC_population500_meanUTC_before = read.csv(input_file_name_meanUTC_before, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                                                            strip.white = FALSE)


##疫情后数据集
input_file_name_meanUTC_during= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_population500_meanUTC_during', '.csv', sep="")  # 字符串连接

marginaleffects_UTC_population500_meanUTC_during = read.csv(input_file_name_meanUTC_during, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                                                            strip.white = FALSE)



###########绘图#################

theme0<-theme(  theme_minimal(base_family = "Arial"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                legend.background = element_rect(fill = "transparent", color = NA),  # 背景透明，无边框
                axis.text.x = element_text(size = 15),
                axis.text.y = element_text(size = 15),
                axis.title = element_text(size = 15),
                strip.text.x = element_text(size = 15),
                strip.text.y = element_text(),
                strip.background = element_rect( fill="white",colour = "black"))

##################################
# 合并数据集，增加一个标记变量 "period" 表示疫情前后
marginaleffects_UTC_population500_meanUTC_before$period <- "Before Covid-19"
marginaleffects_UTC_population500_meanUTC_during$period <- "During Covid-19"

combined_data1 <- rbind(marginaleffects_UTC_population500_meanUTC_before,
                        marginaleffects_UTC_population500_meanUTC_during)

### 绘图
ggplot(data = combined_data1, aes(x = population500, y = mean_value, color = period, fill = period)) +
  geom_ribbon(aes(ymin = Low_value, ymax = UP_value), alpha = 0.25, color = NA) +  # 去掉边界线
  geom_line(size = 0.9) +  # 平均值线
  annotate("text", x = 0.13, y = 2.5, size = 6, color = "black", hjust = 0, label = "italic(P[before]) < 0.001", parse = TRUE) +
  annotate("text", x = 0.13, y = 1.0, size = 6, color = "black", hjust = 0, label = "italic(P[during]) < 0.001", parse = TRUE) + 
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # 添加红色虚线
  scale_x_continuous(name = " ", limits = c(0, 6), breaks = seq(0, 6, by =1.5), 
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(name = "Marginal effect on negative \n sentiment values (95% CI)",
                     limits = c(-9.0, 6), breaks = seq(-9.0, 6, by = 4),
                     labels = scales::number_format(accuracy = 0.1)) +
  #scale_color_manual(values = c("Before Covid-19" = "#FFB5C5", "During Covid-19" = "#48D1CC")) + # 填充颜色  # 自定义颜色
  #scale_fill_manual(values = c("Before Covid-19" = "#1F78B466", "During Covid-19" = "#33A02C66")) +
  labs(title = "", y = "Marginal effect on negative \n sentiment values (95% CI)", x = "Skin Temperature") +
  theme_classic() + theme0 +
  theme(plot.margin = margin(t = -0.3, b = -1.2, l = 0.2, r = 0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),
        axis.text.x = element_text(color = "black", size = 15),
        legend.position = c(0.3, 0.9),  # 图例位置
        #legend.position = "none", #设置无legend
        legend.title = element_blank(),  # 隐藏图例标题
        legend.text = element_text(size = 15)
  )->ppopulation5001  # 图例字体大小


print(ppopulation5001)

#############################################################

# 绘制直方图，边框颜色与填充颜色不同
ggplot(data = query_sample2, aes(x = population500, fill = period)) +
  geom_histogram(aes(y = ..density.., color = period), 
                 position = "identity", binwidth = 0.001, size = 0.8, alpha = 0.4, 
                 color = NA) +  # 设置边框颜色为透明
  scale_x_continuous(name =  expression(Population~density~(~"×"~1000)), 
                     limits = c(0, 6), breaks = seq(0, 6, by =1.5), 
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(name = " ", limits = c(0.0, 2), 
                     breaks = seq(0.0, 2, by = 0.6), 
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_fill_manual(values = c("Before Covid-19" = "#FFB5C5", "During Covid-19" = "#48D1CC")) + # 填充颜色 # 填充颜色
  #scale_color_manual(values = c("Before Covid-19" = "#08459440",  # 添加透明度（80 表示 50% 透明）
  #"During Covid-19" = "#00640040")) + # 添加透明度
  theme_classic() + theme0 +
  theme(plot.margin = margin(t = -0.35, b = 0, l = 0.2, r = 0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),
        axis.text.x = element_text(color = "black", size = 15),
        legend.position = "none")->ppopulation5002

print(ppopulation5002)


ppopulation500<-ggarrange(ppopulation5001, ppopulation5002,ncol = 1,align = "v",heights = c(2,1))
print(ppopulation500)


save_image_path= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_population500', '.pdf', sep="")  # 字符串连接

ggsave(save_image_path, plot = ppopulation500, width = 150, height = 150, units = "mm",dpi=200)


################################################urban environment factors##################################################################






#########################Marginal effect nightlight500 ################################

##疫情前数据集
input_file_name_meanUTC_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_nightlight500_meanUTC_before', '.csv', sep="")  # 字符串连接

marginaleffects_UTC_nightlight500_meanUTC_before = read.csv(input_file_name_meanUTC_before, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                                                            strip.white = FALSE)


##疫情后数据集
input_file_name_meanUTC_during= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_nightlight500_meanUTC_during', '.csv', sep="")  # 字符串连接

marginaleffects_UTC_nightlight500_meanUTC_during = read.csv(input_file_name_meanUTC_during, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                                                            strip.white = FALSE)



###########绘图#################

theme0<-theme(  theme_minimal(base_family = "Arial"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                legend.background = element_rect(fill = "transparent", color = NA),  # 背景透明，无边框
                axis.text.x = element_text(size = 15),
                axis.text.y = element_text(size = 15),
                axis.title = element_text(size = 15),
                strip.text.x = element_text(size = 15),
                strip.text.y = element_text(),
                strip.background = element_rect( fill="white",colour = "black"))

##################################
# 合并数据集，增加一个标记变量 "period" 表示疫情前后
marginaleffects_UTC_nightlight500_meanUTC_before$period <- "Before Covid-19"
marginaleffects_UTC_nightlight500_meanUTC_during$period <- "During Covid-19"

combined_data1 <- rbind(marginaleffects_UTC_nightlight500_meanUTC_before,
                        marginaleffects_UTC_nightlight500_meanUTC_during)

### 绘图
ggplot(data = combined_data1, aes(x = nightlight500, y = mean_value, color = period, fill = period)) +
  geom_ribbon(aes(ymin = Low_value, ymax = UP_value), alpha = 0.25, color = NA) +  # 去掉边界线
  geom_line(size = 0.9) +  # 平均值线
  
  annotate("text", x = 50, y = -1.0, size = 6, color = "black", hjust = 0, label = "italic(P[before]) < 0.001", parse = TRUE) +
  annotate("text", x = 50, y = -2.5, size = 6, color = "black", hjust = 0, label = "italic(P[during]) < 0.001", parse = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # 添加红色虚线
  scale_x_continuous(name = " ", limits = c(0, 120), breaks = seq(0, 120, by =30),  
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(name = "Marginal effect on negative \n sentiment values (95% CI)",
                     limits = c(-13.0, 3), breaks = seq(-13.0, 3, by = 5.0),
                     labels = scales::number_format(accuracy = 0.1)) +
  #scale_color_manual(values = c("Before Covid-19" = "#FFB5C5", "During Covid-19" = "#48D1CC")) + # 填充颜色  # 自定义颜色
  #scale_fill_manual(values = c("Before Covid-19" = "#1F78B466", "During Covid-19" = "#33A02C66")) +
  labs(title = "", y = "Marginal effect on negative \n sentiment values (95% CI)", x = "Skin Temperature") +
  theme_classic() + theme0 +
  theme(plot.margin = margin(t = -0.3, b = -1.2, l = 0.2, r = 0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),
        axis.text.x = element_text(color = "black", size = 15),
        #legend.position = c(0.3, 0.9),  # 图例位置
        legend.position = "none", #设置无legend
        legend.title = element_blank(),  # 隐藏图例标题
        legend.text = element_text(size = 15)
  )->pnightlight5001  # 图例字体大小


print(pnightlight5001)

#############################################################

# 绘制直方图，边框颜色与填充颜色不同
ggplot(data = query_sample2, aes(x = nightlight500, fill = period)) +
  geom_histogram(aes(y = ..density.., color = period), 
                 position = "identity", binwidth = 0.01, size = 0.8, alpha = 0.4, 
                 color = NA) +  # 设置边框颜色为透明
  scale_x_continuous(name = "Nightlight value", limits = c(0, 120), breaks = seq(0, 120, by =30),    
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(name = " ", limits = c(0.0, 0.15), 
                     breaks = seq(0.0, 0.15, by = 0.04), 
                     labels = scales::number_format(accuracy = 0.01)) +
  scale_fill_manual(values = c("Before Covid-19" = "#FFB5C5", "During Covid-19" = "#48D1CC")) + # 填充颜色 # 填充颜色
  #scale_color_manual(values = c("Before Covid-19" = "#08459440",  # 添加透明度（80 表示 50% 透明）
  #"During Covid-19" = "#00640040")) + # 添加透明度
  theme_classic() + theme0 +
  theme(plot.margin = margin(t = -0.35, b = 0, l = 0.2, r = 0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),
        axis.text.x = element_text(color = "black", size = 15),
        legend.position = "none")->pnightlight5002

print(pnightlight5002)


pnightlight500<-ggarrange(pnightlight5001, pnightlight5002,ncol = 1,align = "v",heights = c(2,1))
print(pnightlight500)


save_image_path= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_nightlight500', '.pdf', sep="")  # 字符串连接

ggsave(save_image_path, plot = pnightlight500, width = 150, height = 150, units = "mm",dpi=200)







#########################Marginal effect settlement500 ################################

##疫情前数据集
input_file_name_meanUTC_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_settlement500_meanUTC_before', '.csv', sep="")  # 字符串连接

marginaleffects_UTC_settlement500_meanUTC_before = read.csv(input_file_name_meanUTC_before, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                                                            strip.white = FALSE)


##疫情后数据集
input_file_name_meanUTC_during= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_settlement500_meanUTC_during', '.csv', sep="")  # 字符串连接

marginaleffects_UTC_settlement500_meanUTC_during = read.csv(input_file_name_meanUTC_during, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                                                            strip.white = FALSE)



###########绘图#################

theme0<-theme(  theme_minimal(base_family = "Arial"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                legend.background = element_rect(fill = "transparent", color = NA),  # 背景透明，无边框
                axis.text.x = element_text(size = 15),
                axis.text.y = element_text(size = 15),
                axis.title = element_text(size = 15),
                strip.text.x = element_text(size = 15),
                strip.text.y = element_text(),
                strip.background = element_rect( fill="white",colour = "black"))

##################################
# 合并数据集，增加一个标记变量 "period" 表示疫情前后
marginaleffects_UTC_settlement500_meanUTC_before$period <- "Before Covid-19"
marginaleffects_UTC_settlement500_meanUTC_during$period <- "During Covid-19"

combined_data1 <- rbind(marginaleffects_UTC_settlement500_meanUTC_before,
                        marginaleffects_UTC_settlement500_meanUTC_during)

### 绘图
ggplot(data = combined_data1, aes(x = settlement500, y = mean_value, color = period, fill = period)) +
  geom_ribbon(aes(ymin = Low_value, ymax = UP_value), alpha = 0.25, color = NA) +  # 去掉边界线
  geom_line(size = 0.9) +  # 平均值线
  #annotate("text", x = 10, y = -2.0, size = 6, color = "black", hjust = 0, label = "italic(P) < 0.001", parse = TRUE) +
  
  annotate("text", x = 10, y = -1, size = 6, color = "black", hjust = 0,
           label = "italic(P[before]) < 0.001", parse = TRUE) +
  annotate("text", x = 10, y = -2.0, size = 6, color = "black", hjust = 0,
           label = "italic(P[during]) < 0.001", parse = TRUE) +
  
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # 添加红色虚线
  scale_x_continuous(name = " ", limits = c(0, 100), breaks = seq(0, 100, by =25), 
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(name = "Marginal effect on negative \n sentiment values (95% CI)",
                     limits = c(-9.0, 0), breaks = seq(-9.0, 0, by = 2.5),
                     labels = scales::number_format(accuracy = 0.1)) +
  #scale_color_manual(values = c("Before Covid-19" = "#FFB5C5", "During Covid-19" = "#48D1CC")) + # 填充颜色  # 自定义颜色
  #scale_fill_manual(values = c("Before Covid-19" = "#1F78B466", "During Covid-19" = "#33A02C66")) +
  labs(title = "", y = "Marginal effect on negative \n sentiment values (95% CI)", x = " ") +
  theme_classic() + theme0 +
  theme(plot.margin = margin(t = -0.3, b = -1.2, l = 0.2, r = 0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),
        axis.text.x = element_text(color = "black", size = 15),
        #legend.position = c(0.3, 0.9),  # 图例位置
        legend.position = "none", #设置无legend
        legend.title = element_blank(),  # 隐藏图例标题
        legend.text = element_text(size = 15)
  )->psettlement5001  # 图例字体大小


print(psettlement5001)

#############################################################

# 绘制直方图，边框颜色与填充颜色不同
ggplot(data = query_sample2, aes(x = settlement500, fill = period)) +
  geom_histogram(aes(y = ..density.., color = period), 
                 position = "identity", binwidth = 0.005, size = 0.8, alpha = 0.4, 
                 color = NA) +  # 设置边框颜色为透明
  scale_x_continuous(name = expression(Total~built-up~surface~(m^2 ~"×"~1000)), limits = c(0, 100), breaks = seq(0, 100, by = 25),  
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(name = " ", limits = c(0.0, 0.25), 
                     breaks = seq(0.0, 0.25, by = 0.08), 
                     labels = scales::number_format(accuracy = 0.01)) +
  scale_fill_manual(values = c("Before Covid-19" = "#FFB5C5", "During Covid-19" = "#48D1CC")) + # 填充颜色 # 填充颜色
  #scale_color_manual(values = c("Before Covid-19" = "#08459440",  # 添加透明度（80 表示 50% 透明）
  #"During Covid-19" = "#00640040")) + # 添加透明度
  theme_classic() + theme0 +
  theme(plot.margin = margin(t = -0.35, b = 0, l = 0.2, r = 0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),
        axis.text.x = element_text(color = "black", size = 15),
        legend.position = "none")->psettlement5002

print(psettlement5002)


psettlement500<-ggarrange(psettlement5001, psettlement5002,ncol = 1,align = "v",heights = c(2,1))
print(psettlement500)


save_image_path= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_settlement500', '.pdf', sep="")  # 字符串连接

ggsave(save_image_path, plot = psettlement500, width = 150, height = 150, units = "mm",dpi=200)




#########################Marginal effect GDP ################################

##疫情前数据集
input_file_name_meanUTC_before= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_GDP_meanUTC_before', '.csv', sep="")  # 字符串连接

marginaleffects_UTC_GDP_meanUTC_before = read.csv(input_file_name_meanUTC_before, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                                                  strip.white = FALSE)


##疫情后数据集
input_file_name_meanUTC_during= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_GDP_meanUTC_during', '.csv', sep="")  # 字符串连接

marginaleffects_UTC_GDP_meanUTC_during = read.csv(input_file_name_meanUTC_during, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                                                  strip.white = FALSE)



###########绘图#################

theme0<-theme(  theme_minimal(base_family = "Arial"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                legend.background = element_rect(fill = "transparent", color = NA),  # 背景透明，无边框
                axis.text.x = element_text(size = 15),
                axis.text.y = element_text(size = 15),
                axis.title = element_text(size = 15),
                strip.text.x = element_text(size = 15),
                strip.text.y = element_text(),
                strip.background = element_rect( fill="white",colour = "black"))

##################################
# 合并数据集，增加一个标记变量 "period" 表示疫情前后
marginaleffects_UTC_GDP_meanUTC_before$period <- "Before Covid-19"
marginaleffects_UTC_GDP_meanUTC_during$period <- "During Covid-19"

combined_data1 <- rbind(marginaleffects_UTC_GDP_meanUTC_before,
                        marginaleffects_UTC_GDP_meanUTC_during)

### 绘图
ggplot(data = combined_data1, aes(x = GDP, y = mean_value, color = period, fill = period)) +
  geom_ribbon(aes(ymin = Low_value, ymax = UP_value), alpha = 0.25, color = NA) +  # 去掉边界线
  geom_line(size = 0.9) +  # 平均值线
  #annotate("text", x = 5, y = -1.5, size = 6, color = "black", hjust = 0, label = "italic(P) < 0.001", parse = TRUE) +
  
  annotate("text", x = 8, y = -0.5, size = 6, color = "black", hjust = 0,
           label = "italic(P[before]) < 0.001", parse = TRUE) +
  annotate("text", x = 8, y = -1.5, size = 6, color = "black", hjust = 0,
           label = "italic(P[during]) < 0.001", parse = TRUE) + 
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # 添加红色虚线
  scale_x_continuous(name = " ", limits = c(3, 23), breaks = seq(3, 23, by =5), 
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(name = "Marginal effect on negative \n sentiment values (95% CI)",
                     limits = c(-8, 0), breaks = seq(-8, 0, by = 2.5),
                     labels = scales::number_format(accuracy = 0.1)) +
  #scale_color_manual(values = c("Before Covid-19" = "#FFB5C5", "During Covid-19" = "#48D1CC")) + # 填充颜色  # 自定义颜色
  #scale_fill_manual(values = c("Before Covid-19" = "#1F78B466", "During Covid-19" = "#33A02C66")) +
  labs(title = "", y = "Marginal effect on negative \n sentiment values (95% CI)", x = "Skin Temperature") +
  theme_classic() + theme0 +
  theme(plot.margin = margin(t = -0.3, b = -1.2, l = 0.2, r = 0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),
        axis.text.x = element_text(color = "black", size = 15),
        #legend.position = c(0.3, 0.9),  # 图例位置
        legend.position = "none", #设置无legend
        legend.title = element_blank(),  # 隐藏图例标题
        legend.text = element_text(size = 15)
  )->pGDP1  # 图例字体大小


print(pGDP1)

#############################################################

# 绘制直方图，边框颜色与填充颜色不同
ggplot(data = query_sample2, aes(x = GDP, fill = period)) +
  geom_histogram(aes(y = ..density.., color = period), 
                 position = "identity", binwidth = 0.001, size = 0.8, alpha = 0.4, 
                 color = NA) +  # 设置边框颜色为透明
  scale_x_continuous(name = expression(GDP~(~"$"~"×"~1000)), limits = c(3, 23), breaks = seq(3, 23, by = 5), 
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(name = "  ", limits = c(0, 1.7),
                     breaks = seq(0, 1.7, by = 0.5), 
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_fill_manual(values = c("Before Covid-19" = "#FFB5C5", "During Covid-19" = "#48D1CC")) + # 填充颜色 # 填充颜色
  #scale_color_manual(values = c("Before Covid-19" = "#08459440",  # 添加透明度（80 表示 50% 透明）
  #"During Covid-19" = "#00640040")) + # 添加透明度
  theme_classic() + theme0 +
  theme(plot.margin = margin(t = -0.35, b = 0, l = 0.2, r = 0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),
        axis.text.x = element_text(color = "black", size = 15),
        legend.position = "none")->pGDP2

print(pGDP2)


pGDP<-ggarrange(pGDP1, pGDP2,ncol = 1,align = "v",heights = c(2,1))
print(pGDP)


save_image_path= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC_GDP', '.pdf', sep="")  # 字符串连接

ggsave(save_image_path, plot = pGDP, width = 150, height = 150, units = "mm",dpi=200)






################################################合并图形###############################################################
ii <-  cowplot::plot_grid(ppopulation500,nrow = 1, ncol = 1, labels = c("a"),
                          label_size = 18, scale=1,label_x = 0.02, label_y = 1.0, rel_widths = c(1, 1))
jj <-  cowplot::plot_grid(pnightlight500,nrow = 1, ncol = 1, labels = c("b"),
                          label_size = 18, scale=1,label_x = 0.02, label_y = 1.0, rel_widths = c(1, 1))


kk <-  cowplot::plot_grid(psettlement500,nrow = 1, ncol = 1, labels = c("c"),
                          label_size = 18, scale=1,label_x = 0.02, label_y = 1.0, rel_widths = c(1, 1))



mm <-  cowplot::plot_grid(pGDP,nrow = 1, ncol = 1, labels = c("d"),
                          label_size = 18, scale=1,label_x = 0.02, label_y = 1.0, rel_widths = c(1, 1))



p6 <- cowplot::plot_grid(ii, NULL, jj, nrow = 1, ncol = 3, align = "h", rel_heights = c(1.0, 1.0, 1.0), rel_widths = c(1, 0.1, 1.0)) # rel_heights 参数图形高度设置
print(p6)

p7 <- cowplot::plot_grid(kk, NULL, mm, nrow = 1, ncol = 3, align = "h", rel_heights = c(1.0, 1.0, 1.0), rel_widths = c(1, 0.1, 1.0)) # rel_heights 参数图形高度设置
print(p7)


p8 <- cowplot::plot_grid(p6, p7, nrow = 2, ncol = 1, align = "h", rel_heights = c(1.0, 1.0), rel_widths = c(1, 1.0)) # rel_heights 参数图形高度设置
print(p8)

save_image_path= paste(str0, 'Negative_Sentiment_Rcodes_covid19', '/secondary_data_utc500/',  'UTC500_envrionments_Marginal_effect', '.pdf', sep="")  # 字符串连接

ggsave(save_image_path, plot = p8, width = 260, height = 250, units = "mm",dpi=200)




