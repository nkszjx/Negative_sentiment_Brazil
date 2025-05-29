


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
############################## 读取数据############################################
## 数据路径

sentiment_UTC_data='dataset2018_2019_numbermorethan20_unique'

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

query_sample2 = subset(result0, (freq>50) ) # 过滤tweet文本过少的数据阈值设为：50, 100, 150, 200, 250, 300,
query_sample2 <- na.omit(query_sample2)


unique_users <- n_distinct(query_sample2$userid)
cat("总用户数:", unique_users, "\n")









#########################UTC_effects ################################



input_file_name= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1500/',  'UTC_effects', '.csv', sep="")  # 字符串连接

effects_UTC1500_UTC = read.csv(input_file_name, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                               strip.white = FALSE)

###############################绘图################################

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
########NDVI+LBW


###################################### 绘制边际效应图和数据分布特征###########################################
ggplot(data = effects_UTC1500_UTC)+
  geom_ribbon(aes(x=UTC, y=mean_value, 
                  ymin=Low_value, ymax=UP_value),
              fill="#ED0021",alpha=0.3)+
  geom_line(aes(x=UTC, y=mean_value),colour="black",size=0.7)+
  annotate("text",x=0.4, y=-0.5, size=5,color="grey20",hjust=0,label=expression(" "*italic(P)*" < 0.001" ))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # 添加一条红色虚线
  scale_x_continuous(name = " ", limits = c(0, 0.65), breaks = seq(0, 0.65, by =0.15), 
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(name = "Effect on negative sentiment \n  values (95% CI)", 
                     limits = c(-3.0, 1.0),
                     breaks = seq(-3.0, 1.0, by = 0.75), 
                     labels = scales::number_format(accuracy = 0.1)) +
  labs(title = "",y=" ",x="")+
  theme_classic()+theme0+
  theme(plot.margin = margin(t=-0.3,b=-1.2,l=0.2,r=0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
        axis.text.x = element_text(color = "black", size = 15)  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
  )->pUTC1 ## 可视范围限制

print(pUTC1)

#######################

ggplot(data = query_sample2) +
  geom_histogram(aes(x = UTC1500, y = ..density..), binwidth = 0.002, fill = "#002C5B66", color = "#002C5B66", alpha = 0.5) +
  #geom_density(aes(x=UTC1500),fill="#002C5B66", color="white",show.legend = F, adjust = 2.5)+
  scale_x_continuous(name = "UTC coverage", limits = c(0, 0.65), breaks = seq(0, 0.65, by = 0.2), 
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(name = "  ", limits = c(0, 11),
                     breaks = seq(0, 11, by = 3), 
                     labels = scales::number_format(accuracy = 0.1)) +
  #labs(x=expression("UTC1500"),y="")+
  theme_classic()+theme0+
  theme(plot.margin = margin(t=-0.35,b=0,l=0.2,r=0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
        axis.text.x = element_text(color = "black", size = 15)  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
  )->pUTC2 # 可视范围限制在 0 到 0.5 的范围内

print(pUTC2)             


pUTC<-ggarrange(pUTC1,pUTC2,ncol = 1,align = "v",heights = c(2,1))
#cairo_pdf("figure3.pdf",width = 11,height = 4,family = "Calibri")
print(pUTC)


save_image_path= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1500/',  'UTC_effects', '.pdf', sep="")  # 字符串连接

ggsave(save_image_path, plot = pUTC, width = 150, height = 150, units = "mm",dpi=200)




#########################Marginal effect UTC1500 ################################


input_file_name= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1500/',  'UTC_UTC1500', '.csv', sep="")  # 字符串连接


marginaleffects_UTC1500_UTC1500 = read.csv(input_file_name, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                                           strip.white = FALSE)


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
########NDVI+LBW


###################################### 绘制边际效应图和数据分布特征###########################################
ggplot(data = marginaleffects_UTC1500_UTC1500)+
  geom_ribbon(aes(x=UTC1500, y=mean_value, ymin=Low_value, ymax=UP_value),fill="#ED0021",alpha=0.3)+
  geom_line(aes(x=UTC1500, y=mean_value),colour="black",size=0.70)+
  annotate("text",x=0.15, y=-2.0, size=5,color="grey20",hjust=0,label=expression(" "*italic(P)*" < 0.001" ))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # 添加一条红色虚线
  scale_x_continuous(name = " ", limits = c(0, 0.65), breaks = seq(0.0, 0.65, by =0.2), 
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(name = "Marginal effect on negative \n sentiment values (95% CI)", limits = c(-18.0, 2.5),
                     breaks = seq(-18.0, 2.5, by = 4.0), 
                     labels = scales::number_format(accuracy = 0.1)) +
  labs(title = "",y=" ",x="")+
  theme_classic()+theme0+
  theme(plot.margin = margin(t=-0.3,b=-1.2,l=0.2,r=0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
        axis.text.x = element_text(color = "black", size = 15)  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
  )->pUTC15001 ## 可视范围限制

print(pUTC15001)


##这种现象可能是由于在高人口密度的环境中，树木覆盖的效果会受到拥挤、交通或噪音等其他环境因素的抵消。
##这可能导致在较高人口密度下，树木覆盖对情绪的积极作用没有低密度地区明显，甚至消极情绪可能不明显下降。



#######################
ggplot(data = query_sample2)+
  geom_histogram(aes(x = UTC1500, y = ..density..), binwidth = 0.0020, fill = "#002c5B66", color = "#002c5B66", alpha = 0.5) +
  #geom_density(aes(x=UTC1500),fill="#002C5B66", color="white",show.legend = F, adjust = 1.8)+
  scale_x_continuous(name = "UTC coverage", limits = c(0, 0.65), breaks = seq(0, 0.65, by = 0.2), 
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(name = "  ", limits = c(0.0, 11.0),
                     breaks = seq(0.0, 11.0, by = 3.0), 
                     labels = scales::number_format(accuracy = 0.1)) +
  #labs(x=expression("UTC1500_2"),y="")+
  theme_classic()+theme0+
  theme(plot.margin = margin(t=-0.35,b=0,l=0.2,r=0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
        axis.text.x = element_text(color = "black", size = 15)  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
  )->pUTC15002 # 可视范围限制在 0 到 0.5 的范围内

print(pUTC15002)


pUTC1500<-ggarrange(pUTC15001,pUTC15002,ncol = 1,align = "v",heights = c(2,1))
#cairo_pdf("figure3.pdf",width = 11,height = 4,family = "Calibri")
print(pUTC1500)


save_image_path= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1500/',  'UTC_UTC1500', '.pdf', sep="")  # 字符串连接

ggsave(save_image_path, plot = pUTC1500, width = 150, height = 150, units = "mm",dpi=200)


################################################合并图形###############################################################
aa <-  cowplot::plot_grid(pUTC,nrow = 1, ncol = 1, labels = c("a"),
                          label_size = 18, scale=1,label_x = 0.02, label_y = 1.0, rel_widths = c(1, 1))



bb <-  cowplot::plot_grid(pUTC1500,nrow = 1, ncol = 1, labels = c("b"),
                          label_size = 18, scale=1,label_x = 0.02, label_y = 1.0, rel_widths = c(1, 1))

p1 <- cowplot::plot_grid(aa, NULL, bb, nrow = 1, ncol = 3, align = "h", rel_heights = c(1.0, 1.0, 1.0), rel_widths = c(1, 0.1, 1.0)) # rel_heights 参数图形高度设置
print(p1)

save_image_path= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1500/',  'UTC_effect_UTC1500_Marginal_effect', '.pdf', sep="")  # 字符串连接

ggsave(save_image_path, plot = p1, width = 260, height = 120, units = "mm",dpi=200)



########################################Marginal effect NOUTC1500 #########################################


input_file_name= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1500/',  'UTC_NOUTC1500', '.csv', sep="")  # 字符串连接


marginaleffects_UTC1500_NOUTC1500 = read.csv(input_file_name, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                                             strip.white = FALSE)


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
########NDVI+LBW


###################################### 绘制边际效应图和数据分布特征###########################################
ggplot(data = marginaleffects_UTC1500_NOUTC1500)+
  geom_ribbon(aes(x=NOUTC1500, y=mean_value, ymin=Low_value, ymax=UP_value),fill="#ED0021",alpha=0.3)+
  geom_line(aes(x=NOUTC1500, y=mean_value),colour="black",size=0.7)+
  annotate("text",x=0.55, y=-2.7, size=5,color="grey20",hjust=0,label=expression(" "*italic(P)*" < 0.001" ))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # 添加一条红色虚线
  scale_x_continuous(name = " ", limits = c(0, 0.8), breaks = seq(0, 0.8, by =0.2), 
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(name = "Marginal effect on negative \n sentiment values (95% CI)", limits = c(-16, 0),
                     breaks = seq(-16, 0, by = 3), 
                     labels = scales::number_format(accuracy = 0.1)) +
  labs(title = "",y=" ",x="")+
  theme_classic()+theme0+
  theme(plot.margin = margin(t=-0.3,b=-1.2,l=0.2,r=0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
        axis.text.x = element_text(color = "black", size = 15)  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
  )->pNOUTC15001 ## 可视范围限制

print(pNOUTC15001)


##这种现象可能是由于在高人口密度的环境中，树木覆盖的效果会受到拥挤、交通或噪音等其他环境因素的抵消。
##这可能导致在较高人口密度下，树木覆盖对情绪的积极作用没有低密度地区明显，甚至消极情绪可能不明显下降。



#######################
ggplot(data = query_sample2)+
  geom_histogram(aes(x = NOUTC1500, y = ..density..), binwidth = 0.0015, fill = "#002C5B66", color = "#002C5B66", alpha = 0.5) +
  #geom_density(aes(x=NOUTC1500),fill="#002C5B66", color="white",show.legend = F, adjust = 2.5)+
  scale_x_continuous(name = "No-tree green space coverage", limits = c(0, 0.8), breaks = seq(0, 0.8, by = 0.2), 
                     labels = scales::number_format(accuracy = 0.2)) +
  scale_y_continuous(name = "  ", limits = c(0, 11),
                     breaks = seq(0, 11, by = 3), 
                     labels = scales::number_format(accuracy = 0.1)) +
  #labs(x=expression("NOUTC1500"),y="")+
  theme_classic()+theme0+
  theme(plot.margin = margin(t=-0.35,b=0,l=0.2,r=0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
        axis.text.x = element_text(color = "black", size = 15)  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
  )->pNOUTC15002 # 可视范围限制在 0 到 0.5 的范围内

print(pNOUTC15002)


pNOUTC1500<-ggarrange(pNOUTC15001,pNOUTC15002,ncol = 1,align = "v",heights = c(2,1))
#cairo_pdf("figure3.pdf",width = 11,height = 4,family = "Calibri")
print(pNOUTC1500)


save_image_path= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1500/',  'UTC_NOUTC1500', '.pdf', sep="")  # 字符串连接

ggsave(save_image_path, plot = pNOUTC1500, width = 150, height = 150, units = "mm",dpi=200)


################################################合并图形###############################################################
dd <-  cowplot::plot_grid(pUTC1500,nrow = 1, ncol = 1, labels = c("b"),
                          label_size = 18, scale=1,label_x = 0.02, label_y = 1.0, rel_widths = c(1, 1))



cc <-  cowplot::plot_grid(pNOUTC1500,nrow = 1, ncol = 1, labels = c("a"),
                          label_size = 18, scale=1,label_x = 0.02, label_y = 1.0, rel_widths = c(1, 1))

p2 <- cowplot::plot_grid(cc, NULL, dd, nrow = 1, ncol = 3, align = "h", rel_heights = c(1.0, 1.0, 1.0), rel_widths = c(1, 0.1, 1.0)) # rel_heights 参数图形高度设置
print(p2)

save_image_path= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1500/',  'UTC1500_NOUTC1500_Marginal_effect', '.pdf', sep="")  # 字符串连接

ggsave(save_image_path, plot = p2, width = 260, height = 120, units = "mm",dpi=200)




##################################################################################################


################################### climatic factors ##############################################


#########################################Precipitation#############################################



input_file_name= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1500/',  'UTC_Precipitation', '.csv', sep="")  # 字符串连接

marginaleffects_UTC1500_Precipitation = read.csv(input_file_name, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                                                 strip.white = FALSE)


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
########NDVI+LBW


###################################### 绘制边际效应图和数据分布特征###########################################
ggplot(data = marginaleffects_UTC1500_Precipitation)+
  geom_ribbon(aes(x=Precipitation, y=mean_value, ymin=Low_value, ymax=UP_value),fill="#ED0021",alpha=0.3)+
  geom_line(aes(x=Precipitation, y=mean_value),colour="black",size=0.7)+
  annotate("text",x=11, y= -1.4, size=5,color="grey20",hjust=0,label=expression(" "*italic(P)*" < 0.001" ))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # 添加一条红色虚线
  scale_x_continuous(name = " ", limits = c(0, 45), breaks = seq(0, 45, by =11.0), 
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(name = "Marginal effect on negative \n sentiment values (95% CI)", limits = c(-9, 0.3),
                     breaks = seq(-9, 0.3, by = 3), 
                     labels = scales::number_format(accuracy = 0.1)) +
  labs(title = "",y=" ",x="")+
  theme_classic()+theme0+
  theme(plot.margin = margin(t=-0.3,b=-1.2,l=0.2,r=0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
        axis.text.x = element_text(color = "black", size = 15)  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
  )->pPrecipitation1 ## 可视范围限制

print(pPrecipitation1)


##这种现象可能是由于在高人口密度的环境中，树木覆盖的效果会受到拥挤、交通或噪音等其他环境因素的抵消。
##这可能导致在较高人口密度下，树木覆盖对情绪的积极作用没有低密度地区明显，甚至消极情绪可能不明显下降。



#######################
ggplot(data = query_sample2)+
  geom_histogram(aes(x = Precipitation, y = ..density..), binwidth = 0.08, 
                 fill = "#002C5B66", color = "#002C5B66", alpha = 0.5) +
  #geom_density(aes(x=Precipitation),fill="#002C5B66", color="white",show.legend = F, adjust = 2.5)+
  scale_x_continuous(name = expression(Precipitation(mm/day)), limits = c(0, 45), breaks = seq(0, 45, by = 11.0), 
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(name = "  ", limits = c(0, 0.33),
                     breaks = seq(0, 0.33, by = 0.1), 
                     labels = scales::number_format(accuracy = 0.1)) +
  #labs(x=expression("Precipitation"),y="")+
  theme_classic()+theme0+
  theme(plot.margin = margin(t=-0.35,b=0,l=0.2,r=0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
        axis.text.x = element_text(color = "black", size = 15)  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
  )->pPrecipitation2 # 可视范围限制在 0 到 0.5 的范围内

print(pPrecipitation2)


pPrecipitation<-ggarrange(pPrecipitation1,pPrecipitation2,ncol = 1,align = "v",heights = c(2,1))
print(pPrecipitation)


save_image_path= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1500/',  'UTC_Precipitation', '.pdf', sep="")  # 字符串连接

ggsave(save_image_path, plot = pPrecipitation, width = 150, height = 150, units = "mm",dpi=200)











#########################################humidity########################################



input_file_name= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1500/',  'UTC_humidity', '.csv', sep="")  # 字符串连接



marginaleffects_UTC1500_humidity= read.csv(input_file_name, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                                           strip.white = FALSE)


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
########NDVI+LBW


###################################### 绘制边际效应图和数据分布特征###########################################0.0117
ggplot(data = marginaleffects_UTC1500_humidity)+
  geom_ribbon(aes(x=humidity, y=mean_value, ymin=Low_value, ymax=UP_value),fill="#ED0021",alpha=0.3)+
  geom_line(aes(x=humidity, y=mean_value),colour="black",size=0.7)+
  annotate("text",x=36, y=-1.5, size=5,color="grey20",hjust=0,label=expression(" "*italic(P)*" = 0.0117" ))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # 添加一条红色虚线
  scale_x_continuous(name = " ", limits = c(15, 100), breaks = seq(15, 100, by =21), 
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(name = "Marginal effect on negative \n sentiment values (95% CI)", limits = c(-9, 0.0),
                     breaks = seq(-9, 0.0, by = 1.5), 
                     labels = scales::number_format(accuracy = 0.1)) +
  labs(title = "",y=" ",x="")+
  theme_classic()+theme0+
  theme(plot.margin = margin(t=-0.3,b=-1.2,l=0.2,r=0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
        axis.text.x = element_text(color = "black", size = 15)  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
  )->phumidity1 ## 可视范围限制

print(phumidity1)


##这种现象可能是由于在高人口密度的环境中，树木覆盖的效果会受到拥挤、交通或噪音等其他环境因素的抵消。
##这可能导致在较高人口密度下，树木覆盖对情绪的积极作用没有低密度地区明显，甚至消极情绪可能不明显下降。



#######################
ggplot(data = query_sample2)+
  geom_histogram(aes(x = humidity, y = ..density..), binwidth = 0.16, fill = "#002C5B66", color = "#002C5B66", alpha = 0.5) +
  #geom_density(aes(x=humidity),fill="#002C5B66", color="white",show.legend = F, adjust = 1.5)+
  scale_x_continuous(name = "Humidity(%)", limits = c(15, 100), breaks = seq(15, 100, by = 21), 
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(name = "  ", limits = c(0, 0.05),
                     breaks = seq(0, 0.05, by = 0.015), 
                     labels = scales::number_format(accuracy = 0.01)) +
  #labs(x=expression("humidity"),y="")+
  theme_classic()+theme0+
  theme(plot.margin = margin(t=-0.35,b=0,l=0.2,r=0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
        axis.text.x = element_text(color = "black", size = 15)  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
  )->phumidity2 # 可视范围限制在 0 到 0.5 的范围内

print(phumidity2)


phumidity<-ggarrange(phumidity1,phumidity2,ncol = 1,align = "v",heights = c(2,1))
#cairo_pdf("figure3.pdf",width = 11,height = 4,family = "Calibri")
print(phumidity)


save_image_path= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1500/',  'UTC_humidity', '.pdf', sep="")  # 字符串连接

ggsave(save_image_path, plot = phumidity, width = 150, height = 150, units = "mm",dpi=200)



#########################################Wind########################################



input_file_name= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1500/',  'UTC_Wind', '.csv', sep="")  # 字符串连接


marginaleffects_UTC1500_Wind = read.csv(input_file_name, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                                        strip.white = FALSE)


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
########NDVI+LBW


###################################### 绘制边际效应图和数据分布特征###########################################
ggplot(data = marginaleffects_UTC1500_Wind)+
  geom_ribbon(aes(x=Wind, y=mean_value, ymin=Low_value, ymax=UP_value),fill="#ED0021",alpha=0.3)+
  geom_line(aes(x=Wind, y=mean_value),colour="black",size=0.7)+
  annotate("text",x=1.5, y=-1.5, size=5,color="grey20",hjust=0,label=expression(" "*italic(P)*" < 0.001" ))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # 添加一条红色虚线
  scale_x_continuous(name = " ", limits = c(0, 8), breaks = seq(0, 8, by =2), 
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(name = "Marginal effect on negative \n sentiment values (95% CI)", limits = c(-8, 0.0),
                     breaks = seq(-8, 0.0, by = 1.5), 
                     labels = scales::number_format(accuracy = 0.1)) +
  labs(title = "",y=" ",x="")+
  theme_classic()+theme0+
  theme(plot.margin = margin(t=-0.3,b=-1.2,l=0.2,r=0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
        axis.text.x = element_text(color = "black", size = 15)  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
  )->pWind1 ## 可视范围限制

print(pWind1)


##这种现象可能是由于在高人口密度的环境中，树木覆盖的效果会受到拥挤、交通或噪音等其他环境因素的抵消。
##这可能导致在较高人口密度下，树木覆盖对情绪的积极作用没有低密度地区明显，甚至消极情绪可能不明显下降。



#######################
ggplot(data = query_sample2)+
  geom_histogram(aes(x = Wind, y = ..density..), binwidth = 0.015, fill = "#002C5B66", color = "#002C5B66", alpha = 0.5, szie=4.7) +
  #geom_density(aes(x=Wind),fill="#002C5B66", color="white",show.legend = F, adjust = 2.5)+
  scale_x_continuous(name = "Wind speed (m/s)", limits = c(0, 8), breaks = seq(0, 8, by = 2.0), 
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(name = "  ", limits = c(0, 0.67),
                     breaks = seq(0, 0.67, by = 0.2), 
                     labels = scales::number_format(accuracy = 0.1)) +
  #labs(x=expression("Wind"),y="")+
  theme_classic()+theme0+
  theme(plot.margin = margin(t=-0.35,b=0,l=0.2,r=0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
        axis.text.x = element_text(color = "black", size = 15)  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
  )->pWind2 # 可视范围限制在 0 到 0.5 的范围内

print(pWind2)


pWind<-ggarrange(pWind1,pWind2,ncol = 1,align = "v",heights = c(2,1))
#cairo_pdf("figure3.pdf",width = 11,height = 4,family = "Calibri")
print(pWind)


save_image_path= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1500/',  'UTC_Wind', '.pdf', sep="")  # 字符串连接

ggsave(save_image_path, plot = pWind, width = 150, height = 150, units = "mm",dpi=200)












#########################################cloudcover########################################


input_file_name= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1500/',  'UTC_cloudcover', '.csv', sep="")  # 字符串连接


marginaleffects_UTC1500_cloudcover = read.csv(input_file_name, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                                              strip.white = FALSE)

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
########NDVI+LBW


###################################### 绘制边际效应图和数据分布特征###########################################
ggplot(data = marginaleffects_UTC1500_cloudcover)+
  geom_ribbon(aes(x=cloudcover, y=mean_value, ymin=Low_value, ymax=UP_value),fill="#ED0021",alpha=0.3)+
  geom_line(aes(x=cloudcover, y=mean_value),colour="black",size=0.7)+
  annotate("text",x=20, y=-1.5, size=5,color="grey20",hjust=0,label=expression(" "*italic(P)*" = 0.8152" ))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # 添加一条红色虚线
  scale_x_continuous(name = " ", limits = c(0, 80), breaks = seq(0, 80, by =20.0), 
                     labels = scales::number_format(accuracy =0.1)) +
  scale_y_continuous(name = "Marginal effect on negative \n sentiment values (95% CI)", limits = c(-8.0, 0.0),
                     breaks = seq(-8.0, 0.0, by = 1.5), 
                     labels = scales::number_format(accuracy = 0.1)) +
  labs(title = "",y=" ",x="")+
  theme_classic()+theme0+
  theme(plot.margin = margin(t=-0.3,b=-1.2,l=0.2,r=0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
        axis.text.x = element_text(color = "black", size = 15)  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
  )->pcloudcover1 ## 可视范围限制

print(pcloudcover1)


##这种现象可能是由于在高人口密度的环境中，树木覆盖的效果会受到拥挤、交通或噪音等其他环境因素的抵消。
##这可能导致在较高人口密度下，树木覆盖对情绪的积极作用没有低密度地区明显，甚至消极情绪可能不明显下降。



#######################
ggplot(data = query_sample2)+
  geom_histogram(aes(x = cloudcover, y = ..density..), binwidth = 0.015, fill = "#002C5B66", color = "#002C5B66", alpha = 0.5, size = 0.7) +
  #geom_density(aes(x=cloudcover),fill="#002C5B66", color="white",show.legend = F, adjust = 0.15)+
  scale_x_continuous(name = "Cloud Coverage (%)", limits = c(0, 80), breaks = seq(0, 80, by = 20.0), 
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(name = "  ", limits = c(0, 2),
                     breaks = seq(0, 2, by = 0.6), 
                     labels = scales::number_format(accuracy = 0.6)) +
  #labs(x=expression("cloudcover"),y="")+
  theme_classic()+theme0+
  theme(plot.margin = margin(t=-0.35,b=0,l=0.2,r=0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
        axis.text.x = element_text(color = "black", size = 15)  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
  )->pcloudcover2 # 可视范围限制在 0 到 0.5 的范围内

print(pcloudcover2)


pcloudcover<-ggarrange(pcloudcover1,pcloudcover2,ncol = 1,align = "v",heights = c(2,1))
#cairo_pdf("figure3.pdf",width = 11,height = 4,family = "Calibri")
print(pcloudcover)


save_image_path= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1500/',  'UTC_cloudcover', '.pdf', sep="")  # 字符串连接

ggsave(save_image_path, plot = pcloudcover, width = 150, height = 150, units = "mm",dpi=200)




#########################################Surface_pressure########################################




input_file_name= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1500/',  'UTC_Surface_pressure', '.csv', sep="")  # 字符串连接

marginaleffects_UTC1500_Surface_pressure = read.csv(input_file_name, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                                                    strip.white = FALSE)


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
########NDVI+LBW


###################################### 绘制边际效应图和数据分布特征###########################################
ggplot(data = marginaleffects_UTC1500_Surface_pressure)+
  geom_ribbon(aes(x=Surface_pressure, y=mean_value, ymin=Low_value, ymax=UP_value),fill="#ED0021",alpha=0.3)+
  geom_line(aes(x=Surface_pressure, y=mean_value),colour="black",size=0.7)+
  annotate("text",x=92, y=-2.0, size=5,color="grey20",hjust=0,label=expression(" "*italic(P)*" < 0.001" ))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # 添加一条红色虚线
  scale_x_continuous(name = " ", limits = c(88, 104), breaks = seq(88, 104, by =4), 
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(name = "Marginal effect on negative \n sentiment values (95% CI)", limits = c(-11,0),
                     breaks = seq(-11, 0, by = 2.5), 
                     labels = scales::number_format(accuracy = 0.1)) +
  labs(title = "",y=" ",x="")+
  theme_classic()+theme0+
  theme(plot.margin = margin(t=-0.3,b=-1.2,l=0.2,r=0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
        axis.text.x = element_text(color = "black", size = 15)  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
  )->pSurface_pressure1 ## 可视范围限制

print(pSurface_pressure1)


##这种现象可能是由于在高人口密度的环境中，树木覆盖的效果会受到拥挤、交通或噪音等其他环境因素的抵消。
##这可能导致在较高人口密度下，树木覆盖对情绪的积极作用没有低密度地区明显，甚至消极情绪可能不明显下降。



#######################
ggplot(data = query_sample2)+
  geom_histogram(aes(x = Surface_pressure, y = ..density..), binwidth = 0.02, fill = "#002C5B66", color = "#002C5B66", alpha = 0.5) +
  #geom_density(aes(x=Surface_pressure),fill="#002C5B66", color="white",show.legend = F, adjust = 2.5)+
  scale_x_continuous(name =  expression(Surface~Pressure~(Pa~"×"~1000)), limits = c(88, 104), breaks = seq(88, 104, by = 4.0), 
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(name = "  ", limits = c(0, 0.35),
                     breaks = seq(0, 0.35, by = 0.1), 
                     labels = scales::number_format(accuracy = 0.1)) +
  #labs(x=expression("Surface_pressure"),y="")+
  theme_classic()+theme0+
  theme(plot.margin = margin(t=-0.35,b=0,l=0.2,r=0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
        axis.text.x = element_text(color = "black", size = 15)  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
  )->pSurface_pressure2 # 可视范围限制在 0 到 0.5 的范围内

print(pSurface_pressure2)


pSurface_pressure<-ggarrange(pSurface_pressure1,pSurface_pressure2,ncol = 1,align = "v",heights = c(2,1))
#cairo_pdf("figure3.pdf",width = 11,height = 4,family = "Calibri")
print(pSurface_pressure)


save_image_path= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1500/',  'UTC_Surface_pressure', '.pdf', sep="")  # 字符串连接

ggsave(save_image_path, plot = pSurface_pressure, width = 150, height = 150, units = "mm",dpi=200)





#########################################Skintemperature########################################


input_file_name= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1500/',  'UTC_Skintemperature', '.csv', sep="")  # 字符串连接


marginaleffects_UTC1500_Skintemperature = read.csv(input_file_name, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                                                   strip.white = FALSE)


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
########NDVI+LBW


###################################### 绘制边际效应图和数据分布特征###########################################
ggplot(data = marginaleffects_UTC1500_Skintemperature)+
  geom_ribbon(aes(x=Skintemperature, y=mean_value, ymin=Low_value, ymax=UP_value),fill="#ED0021",alpha=0.3)+
  geom_line(aes(x=Skintemperature, y=mean_value),colour="black",size=0.7)+
  annotate("text",x=15, y=-1.8, size=5,color="grey20",hjust=0,label=expression(" "*italic(P)*" < 0.001" ))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # 添加一条红色虚线
  scale_x_continuous(name = " ", limits = c(5, 45), breaks = seq(5, 45, by =10), 
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(name = "Marginal effect on negative \n sentiment values (95% CI)", limits = c(-10, 0),
                     breaks = seq(-10, 0, by = 1.7), 
                     labels = scales::number_format(accuracy = 0.1)) +
  labs(title = "",y=" ",x="")+
  theme_classic()+theme0+
  theme(plot.margin = margin(t=-0.3,b=-1.2,l=0.2,r=0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
        axis.text.x = element_text(color = "black", size = 15)  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
  )->pSkintemperature1 ## 可视范围限制

print(pSkintemperature1)


##这种现象可能是由于在高人口密度的环境中，树木覆盖的效果会受到拥挤、交通或噪音等其他环境因素的抵消。
##这可能导致在较高人口密度下，树木覆盖对情绪的积极作用没有低密度地区明显，甚至消极情绪可能不明显下降。



#######################
ggplot(data = query_sample2)+
  geom_histogram(aes(x = Skintemperature, y = ..density..), binwidth = 0.05, fill = "#002C5B66", color = "#002C5B66", alpha = 0.5) +
  #geom_density(aes(x=Skintemperature),fill="#002C5B66", color="white",show.legend = F, adjust = 1.5)+
  scale_x_continuous(name =expression(Temperature~(degree*C)), limits = c(5, 45), breaks = seq(5, 45, by = 10), 
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(name = "  ", limits = c(0, 0.10),
                     breaks = seq(0, 0.10, by = 0.03), 
                     labels = scales::number_format(accuracy = 0.03)) +
  #labs(x=expression("Skintemperature"),y="")+
  theme_classic()+theme0+
  theme(plot.margin = margin(t=-0.35,b=0,l=0.2,r=0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
        axis.text.x = element_text(color = "black", size = 15)  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
  )->pSkintemperature2 # 可视范围限制在 0 到 0.5 的范围内

print(pSkintemperature2)


pSkintemperature<-ggarrange(pSkintemperature1,pSkintemperature2,ncol = 1,align = "v",heights = c(2,1))
#cairo_pdf("figure3.pdf",width = 11,height = 4,family = "Calibri")
print(pSkintemperature)


save_image_path= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1500/',  'UTC_Skintemperature', '.pdf', sep="")  # 字符串连接

ggsave(save_image_path, plot = pSkintemperature, width = 150, height = 150, units = "mm",dpi=200)




################################################合并图形###############################################################
ee <-  cowplot::plot_grid(pPrecipitation,nrow = 1, ncol = 1, labels = c("a"),
                          label_size = 18, scale=1,label_x = 0.02, label_y = 1.0, rel_widths = c(1, 1))

ff <-  cowplot::plot_grid(phumidity,nrow = 1, ncol = 1, labels = c("b"),
                          label_size = 18, scale=1,label_x = 0.02, label_y = 1.0, rel_widths = c(1, 1))

gg <-  cowplot::plot_grid(pSurface_pressure,nrow = 1, ncol = 1, labels = c("c"),
                          label_size = 18, scale=1,label_x = 0.02, label_y = 1.0, rel_widths = c(1, 1))

hh <-  cowplot::plot_grid(pSkintemperature,nrow = 1, ncol = 1, labels = c("d"),
                          label_size = 18, scale=1,label_x = 0.02, label_y = 1.0, rel_widths = c(1, 1))







p3 <- cowplot::plot_grid(ee, NULL, ff, nrow = 1, ncol = 3, align = "h", rel_heights = c(1.0, 1.0, 1.0), rel_widths = c(1, 0.1, 1.0)) # rel_heights 参数图形高度设置
print(p3)

p4 <- cowplot::plot_grid(gg, NULL, hh, nrow = 1, ncol = 3, align = "h", rel_heights = c(1.0, 1.0, 1.0), rel_widths = c(1, 0.1, 1.0)) # rel_heights 参数图形高度设置
print(p4)


p5 <- cowplot::plot_grid(p3, p4, nrow = 2, ncol = 1, align = "h", rel_heights = c(1.0, 1.0), rel_widths = c(1, 1.0)) # rel_heights 参数图形高度设置
print(p5)


save_image_path= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1500/',  'UTC1500_climate_Marginal_effect', '.pdf', sep="")  # 字符串连接

ggsave(save_image_path, plot = p5, width = 320, height = 240, units = "mm",dpi=200)




#############################################Urban environment factors ###################################################################








#########################################population1500########################################


input_file_name= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1500/',  'UTC_population1500', '.csv', sep="")  # 字符串连接


marginaleffects_UTC1500_population1500 = read.csv(input_file_name, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                                                  strip.white = FALSE)



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
########NDVI+LBW


###################################### 绘制边际效应图和数据分布特征###########################################
ggplot(data = marginaleffects_UTC1500_population1500)+
  geom_ribbon(aes(x=population1500, y=mean_value, ymin=Low_value, ymax=UP_value),fill="#ED0021",alpha=0.3)+
  geom_line(aes(x=population1500, y=mean_value),colour="black",size=0.7)+
  annotate("text",x= 2.5, y= -1.2, size=5,color="grey20",hjust=0,label=expression(" "*italic(P)*" < 0.01" ))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # 添加一条红色虚线
  scale_x_continuous(name = " ", limits = c(0, 7.5), breaks = seq(0, 7.5, by =2.5), 
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(name = "Marginal effect on negative \n sentiment values (95% CI)", limits = c(-10, 0),
                     breaks = seq(-10, 0, by =3), 
                     labels = scales::number_format(accuracy = 0.1)) +
  labs(title = "",y=" ",x="")+
  theme_classic()+theme0+
  theme(plot.margin = margin(t=-0.3,b=-1.2,l=0.2,r=0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
        axis.text.x = element_text(color = "black", size = 15)  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
  )->ppopulation15001 ## 可视范围限制

print(ppopulation15001)


##这种现象可能是由于在高人口密度的环境中，树木覆盖的效果会受到拥挤、交通或噪音等其他环境因素的抵消。
##这可能导致在较高人口密度下，树木覆盖对情绪的积极作用没有低密度地区明显，甚至消极情绪可能不明显下降。



#######################
ggplot(data = query_sample2)+
  geom_histogram(aes(x = population1500, y = ..density..), binwidth = 0.015, fill = "#002C5B66", color = "#002C5B66", alpha = 0.5) +
  #geom_density(aes(x=population1500),fill="#002C5B66", color="white",show.legend = F, adjust = 1.5)+
  scale_x_continuous(name =  expression(Population~density~(~"×"~1000)), limits = c(0, 7.5), breaks = seq(0, 7.5, by =2.5), 
                     labels = scales::number_format(accuracy = 2.5)) +
  scale_y_continuous(name = "  ", limits = c(0, 0.65),
                     breaks = seq(0, 0.65, by = 0.2), 
                     labels = scales::number_format(accuracy = 0.2)) +
  #labs(x=expression("population1500"),y="")+
  theme_classic()+theme0+
  theme(plot.margin = margin(t=-0.35,b=0,l=0.2,r=0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
        axis.text.x = element_text(color = "black", size = 15)  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
  )->ppopulation15002 # 可视范围限制在 0 到 0.5 的范围内

print(ppopulation15002)


ppopulation1500<-ggarrange(ppopulation15001,ppopulation15002,ncol = 1,align = "v",heights = c(2,1))
#cairo_pdf("figure3.pdf",width = 11,height = 4,family = "Calibri")
print(ppopulation1500)


save_image_path= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1500/',  'UTC_population1500', '.pdf', sep="")  # 字符串连接

ggsave(save_image_path, plot = ppopulation1500, width = 150, height = 150, units = "mm",dpi=200)



#########################################nightlight1500########################################

input_file_name= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1500/',  'UTC_nightlight1500', '.csv', sep="")  # 字符串连接


marginaleffects_UTC1500_nightlight1500 = read.csv(input_file_name, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                                                  strip.white = FALSE)



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
########NDVI+LBW


###################################### 绘制边际效应图和数据分布特征###########################################
ggplot(data = marginaleffects_UTC1500_nightlight1500)+
  geom_ribbon(aes(x=nightlight1500, y=mean_value, ymin=Low_value, ymax=UP_value),fill="#ED0021",alpha=0.3)+
  geom_line(aes(x=nightlight1500, y=mean_value),colour="black",size=0.7)+
  annotate("text",x=50, y=-1.5, size=5,color="grey20",hjust=0,label=expression(" "*italic(P)*" < 0.001" ))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # 添加一条红色虚线
  scale_x_continuous(name = " ", limits = c(0, 150), breaks = seq(0, 150, by =35), 
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(name = "Marginal effect on negative \n sentiment values (95% CI)", limits = c(-9, 0),
                     breaks = seq(-9, 0, by = 1.5), 
                     labels = scales::number_format(accuracy = 0.1)) +
  labs(title = "",y=" ",x="")+
  theme_classic()+theme0+
  theme(plot.margin = margin(t=-0.3,b=-1.2,l=0.2,r=0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
        axis.text.x = element_text(color = "black", size = 15)  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
  )->pnightlight15001 ## 可视范围限制

print(pnightlight15001)


##这种现象可能是由于在高人口密度的环境中，树木覆盖的效果会受到拥挤、交通或噪音等其他环境因素的抵消。
##这可能导致在较高人口密度下，树木覆盖对情绪的积极作用没有低密度地区明显，甚至消极情绪可能不明显下降。



#######################
ggplot(data = query_sample2)+
  geom_histogram(aes(x = nightlight1500, y = ..density..), binwidth = 0.1, fill = "#002C5B66", color = "#002C5B66", alpha = 0.5) +
  #geom_density(aes(x=nightlight1500),fill="#002C5B66", color="white",show.legend = F, adjust = 1.5)+
  scale_x_continuous(name = "Nightlight value", limits = c(0, 150), breaks = seq(0, 150, by = 35), 
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(name = "  ", limits = c(0, 0.16),
                     breaks = seq(0, 0.16, by = 0.05), 
                     labels = scales::number_format(accuracy = 0.05)) +
  #labs(x=expression("nightlight1500"),y="")+
  theme_classic()+theme0+
  theme(plot.margin = margin(t=-0.35,b=0,l=0.2,r=0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
        axis.text.x = element_text(color = "black", size = 15)  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
  )->pnightlight15002 # 可视范围限制在 0 到 0.5 的范围内

print(pnightlight15002)


pnightlight1500<-ggarrange(pnightlight15001,pnightlight15002,ncol = 1,align = "v",heights = c(2,1))
#cairo_pdf("figure3.pdf",width = 11,height = 4,family = "Calibri")
print(pnightlight1500)


save_image_path= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1500/',  'UTC_nightlight1500', '.pdf', sep="")  # 字符串连接

ggsave(save_image_path, plot = pnightlight1500, width = 150, height = 150, units = "mm",dpi=200)



#########################################settlement1500########################################



input_file_name= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1500/',  'UTC_settlement1500', '.csv', sep="")  # 字符串连接

marginaleffects_UTC1500_settlement1500 = read.csv(input_file_name, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                                                  strip.white = FALSE)


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
########NDVI+LBW


###################################### 绘制边际效应图和数据分布特征###########################################
ggplot(data = marginaleffects_UTC1500_settlement1500)+
  geom_ribbon(aes(x=settlement1500, y=mean_value, ymin=Low_value, ymax=UP_value),fill="#ED0021",alpha=0.3)+
  geom_line(aes(x=settlement1500, y=mean_value),colour="black",size=0.7)+
  annotate("text",x=25, y=-1.2, size=5,color="grey20",hjust=0,label=expression(" "*italic(P)*" < 0.01" ))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # 添加一条红色虚线
  scale_x_continuous(name = " ", limits = c(0, 100), breaks = seq(0, 100, by =25), 
                     labels = scales::number_format(accuracy = 30.0)) +
  scale_y_continuous(name = "Marginal effect on negative \n sentiment values (95% CI)", limits = c(-8, 0.0),
                     breaks = seq(-8, 0.0, by = 2), 
                     labels = scales::number_format(accuracy = 0.1)) +
  labs(title = "",y=" ",x="")+
  theme_classic()+theme0+
  theme(plot.margin = margin(t=-0.3,b=-1.2,l=0.2,r=0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
        axis.text.x = element_text(color = "black", size = 15)  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
  )->psettlement15001 ## 可视范围限制

print(psettlement15001)


##这种现象可能是由于在高人口密度的环境中，树木覆盖的效果会受到拥挤、交通或噪音等其他环境因素的抵消。
##这可能导致在较高人口密度下，树木覆盖对情绪的积极作用没有低密度地区明显，甚至消极情绪可能不明显下降。




#######################
ggplot(data = query_sample2)+
  geom_histogram(aes(x = settlement1500, y = ..density..), binwidth = 0.1, fill = "#002C5B66", color = "#002C5B66", alpha = 0.5) +
  #geom_density(aes(x=settlement1500),fill="#002C5B66", color="white",show.legend = F, adjust = 1.5)+
  scale_x_continuous(name = expression(Total~built-up~surface~(m^2 ~"×"~1000)), limits = c(0, 100), breaks = seq(0, 100, by = 25), 
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(name = "  ", limits = c(0, 0.26),
                     breaks = seq(0, 0.26, by = 0.08), 
                     labels = scales::number_format(accuracy = 0.01)) +
  #labs(x=expression("settlement1500"),y="")+
  theme_classic()+theme0+
  theme(plot.margin = margin(t=-0.35,b=0,l=0.2,r=0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
        axis.text.x = element_text(color = "black", size = 15)  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
  )->psettlement15002 # 可视范围限制在 0 到 0.5 的范围内

print(psettlement15002)


psettlement1500<-ggarrange(psettlement15001,psettlement15002,ncol = 1,align = "v",heights = c(2,1))
#cairo_pdf("figure3.pdf",width = 11,height = 4,family = "Calibri")
print(psettlement1500)


save_image_path= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1500/',  'UTC_settlement1500', '.pdf', sep="")  # 字符串连接

ggsave(save_image_path, plot = psettlement1500, width = 150, height = 150, units = "mm",dpi=200)






#########################################impervious1500########################################


input_file_name= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1500/',  'UTC_impervious1500', '.csv', sep="")  # 字符串连接


marginaleffects_UTC1500_impervious1500 = read.csv(input_file_name, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                                                  strip.white = FALSE)


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
########NDVI+LBW


###################################### 绘制边际效应图和数据分布特征###########################################
ggplot(data = marginaleffects_UTC1500_impervious1500)+
  geom_ribbon(aes(x=impervious1500, y=mean_value, ymin=Low_value, ymax=UP_value),fill="#ED0021",alpha=0.3)+
  geom_line(aes(x=impervious1500, y=mean_value),colour="black",size=0.7)+
  annotate("text",x=0.4, y=-1.5, size=5,color="grey20",hjust=0,label=expression(" "*italic(P)*" < 0.001" ))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # 添加一条红色虚线
  scale_x_continuous(name = " ", limits = c(0.2, 1), breaks = seq(0.2, 1, by =0.20), 
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(name = "Marginal effect on negative \n sentiment values (95% CI)", limits = c(-11, 0.0),
                     breaks = seq(-11, 0.0, by = 3), 
                     labels = scales::number_format(accuracy = 0.1)) +
  labs(title = "",y=" ",x="")+
  theme_classic()+theme0+
  theme(plot.margin = margin(t=-0.3,b=-1.2,l=0.2,r=0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
        axis.text.x = element_text(color = "black", size = 15)  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
  )->pimpervious15001 ## 可视范围限制

print(pimpervious15001)


##这种现象可能是由于在高人口密度的环境中，树木覆盖的效果会受到拥挤、交通或噪音等其他环境因素的抵消。
##这可能导致在较高人口密度下，树木覆盖对情绪的积极作用没有低密度地区明显，甚至消极情绪可能不明显下降。



#######################
ggplot(data = query_sample2)+
  geom_histogram(aes(x = impervious1500, y = ..density..), binwidth = 0.002, fill = "#002C5B66", color = "#002C5B66", alpha = 0.5) +
  #geom_density(aes(x=impervious1500),fill="#002C5B66", color="white",show.legend = F, adjust = 1.5)+
  scale_x_continuous(name = "Impervious", limits = c(0.2, 1), breaks = seq(0.2, 1, by = 0.20), 
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(name = "  ", limits = c(0, 8),
                     breaks = seq(0, 8, by = 2.5), 
                     labels = scales::number_format(accuracy = 0.1)) +
  #labs(x=expression("impervious1500"),y="")+
  theme_classic()+theme0+
  theme(plot.margin = margin(t=-0.35,b=0,l=0.2,r=0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
        axis.text.x = element_text(color = "black", size = 15)  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
  )->pimpervious15002 # 可视范围限制在 0 到 0.5 的范围内

print(pimpervious15002)


pimpervious1500<-ggarrange(pimpervious15001,pimpervious15002,ncol = 1,align = "v",heights = c(2,1))
#cairo_pdf("figure3.pdf",width = 11,height = 4,family = "Calibri")
print(pimpervious1500)


save_image_path= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1500/',  'UTC_impervious1500', '.pdf', sep="")  # 字符串连接

ggsave(save_image_path, plot = pimpervious1500, width = 150, height = 150, units = "mm",dpi=200)



#########################################GDP########################################


# 构建数据框并直接命名


input_file_name= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1500/',  'UTC_GDP', '.csv', sep="")  # 字符串连接


marginaleffects_UTC1500_GDP = read.csv(input_file_name, header=TRUE, sep=',',blank.lines.skip=FALSE, encoding="UTF-8", 
                                       strip.white = FALSE)


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
########NDVI+LBW


###################################### 绘制边际效应图和数据分布特征###########################################
ggplot(data = marginaleffects_UTC1500_GDP)+
  geom_ribbon(aes(x=GDP, y=mean_value, ymin=Low_value, ymax=UP_value),fill="#ED0021",alpha=0.3)+
  geom_line(aes(x=GDP, y=mean_value),colour="black",size=0.7)+
  annotate("text",x=7, y=-1.6, size=5,color="grey20",hjust=0,label=expression(" "*italic(P)*" < 0.001" ))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # 添加一条红色虚线
  scale_x_continuous(name = " ", limits = c(0, 20), breaks = seq(0, 20, by =5), 
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(name = "Marginal effect on negative \n sentiment values (95% CI)", limits = c(-9, 0),
                     breaks = seq(-9, 0, by = 2.5), 
                     labels = scales::number_format(accuracy = 0.1)) +
  labs(title = "",y=" ",x="")+
  theme_classic()+theme0+
  theme(plot.margin = margin(t=-0.3,b=-1.2,l=0.2,r=0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
        axis.text.x = element_text(color = "black", size = 15)  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
  )->pGDP1 ## 可视范围限制

print(pGDP1)


##这种现象可能是由于在高人口密度的环境中，树木覆盖的效果会受到拥挤、交通或噪音等其他环境因素的抵消。
##这可能导致在较高人口密度下，树木覆盖对情绪的积极作用没有低密度地区明显，甚至消极情绪可能不明显下降。



#######################
ggplot(data = query_sample2)+
  geom_histogram(aes(x = GDP, y = ..density..), binwidth = 0.01, fill = "#002C5B66", color = "#002C5B66", alpha = 0.5) +
  #geom_density(aes(x=GDP),fill="#002C5B66", color="white",show.legend = F, adjust = 1.5)+
  scale_x_continuous(name = expression(GDP~(~"$"~"×"~1000)), limits = c(0, 20), breaks = seq(0, 20, by = 5), 
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(name = "  ", limits = c(0, 1.7),
                     breaks = seq(0, 1.7, by = 0.5), 
                     labels = scales::number_format(accuracy = 0.1)) +
  #labs(x=expression("GDP"),y="")+
  theme_classic()+theme0+
  theme(plot.margin = margin(t=-0.35,b=0,l=0.2,r=0.2, unit = "cm"),
        axis.text.y = element_text(color = "black", size = 15),  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
        axis.text.x = element_text(color = "black", size = 15)  # 设置 Y 轴字体颜色为蓝色，字体大小为 12
  )->pGDP2 # 可视范围限制在 0 到 0.5 的范围内

print(pGDP2)


pGDP<-ggarrange(pGDP1,pGDP2,ncol = 1,align = "v",heights = c(2,1))
#cairo_pdf("figure3.pdf",width = 11,height = 4,family = "Calibri")
print(pGDP)


save_image_path= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1500/',  'UTC_GDP', '.pdf', sep="")  # 字符串连接

ggsave(save_image_path, plot = pGDP, width = 150, height = 150, units = "mm",dpi=200)

#############################################################################################################



################################################合并图形###############################################################



ii <-  cowplot::plot_grid(pimpervious1500,nrow = 1, ncol = 1, labels = c("a"),
                          label_size = 18, scale=1,label_x = 0.02, label_y = 1.0, rel_widths = c(1, 1))



jj <-  cowplot::plot_grid(pGDP,nrow = 1, ncol = 1, labels = c("b"),
                          label_size = 18, scale=1,label_x = 0.02, label_y = 1.0, rel_widths = c(1, 1))


p6 <- cowplot::plot_grid(ii, NULL, jj, nrow = 1, ncol = 3, align = "h", rel_heights = c(1.0, 1.0, 1.0), rel_widths = c(1, 0.1, 1.0)) # rel_heights 参数图形高度设置
print(p6)




save_image_path= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1500/',  'UTC1500_imperv_gdp_Marginal_effect', '.pdf', sep="")  # 字符串连接

ggsave(save_image_path, plot = p6, width = 320, height = 120, units = "mm",dpi=200)





################################################合并图形###############################################################
ii <-  cowplot::plot_grid(ppopulation1500,nrow = 1, ncol = 1, labels = c("a"),
                          label_size = 18, scale=1,label_x = 0.02, label_y = 1.0, rel_widths = c(1, 1))
jj <-  cowplot::plot_grid(pnightlight1500,nrow = 1, ncol = 1, labels = c("b"),
                          label_size = 18, scale=1,label_x = 0.02, label_y = 1.0, rel_widths = c(1, 1))


kk <-  cowplot::plot_grid(pimpervious1500,nrow = 1, ncol = 1, labels = c("c"),
                          label_size = 18, scale=1,label_x = 0.02, label_y = 1.0, rel_widths = c(1, 1))



mm <-  cowplot::plot_grid(pGDP,nrow = 1, ncol = 1, labels = c("d"),
                          label_size = 18, scale=1,label_x = 0.02, label_y = 1.0, rel_widths = c(1, 1))


p6 <- cowplot::plot_grid(ii, NULL, jj, nrow = 1, ncol = 3, align = "h", rel_heights = c(1.0, 1.0, 1.0), rel_widths = c(1, 0.1, 1.0)) # rel_heights 参数图形高度设置
print(p6)

p7 <- cowplot::plot_grid(kk, NULL, mm, nrow = 1, ncol = 3, align = "h", rel_heights = c(1.0, 1.0, 1.0), rel_widths = c(1, 0.1, 1.0)) # rel_heights 参数图形高度设置
print(p7)


p8 <- cowplot::plot_grid(p6, p7, nrow = 2, ncol = 1, align = "h", rel_heights = c(1.0, 1.0), rel_widths = c(1, 1.0)) # rel_heights 参数图形高度设置
print(p8)


save_image_path= paste(str0, 'Negative_Sentiment_Rcodes', '/secondary_data_utc1500/',  'UTC1500_envrionments_Marginal_effect', '.pdf', sep="")  # 字符串连接

ggsave(save_image_path, plot = p8, width = 320, height = 240, units = "mm",dpi=200)





##############################################################################################################

