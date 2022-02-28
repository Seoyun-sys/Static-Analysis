#install.packages("mice")
#install.packages("VIM")
#install.packages("Hmisc")
#install.packages("gridExtra")
#install.packages("ggpubr")
#install.packages("remotes")
#remotes::install_github("coolbutuseless/ggpattern")
#library(tidyverse)
#library(Hmisc)
#library(VIM)
#library(mice)
#library(reshape)
#library(stringr)
#install.packages("nortest")

# library(multcompView)
# library(MASS)
library(nortest)


library(ggpubr)
library(gridExtra)
library(dplyr)
library(ggplot2)
library(ggpattern)
library(moonBook)

library(multcomp)
#library(Rserve)
require(graphics)

library(ggpubr)

#library(rcompanion)
library(psych) 


#MICE 전 파일 : CBC_origin.csv
#MICE 환자 제외 : MICE_CBC_remove.csv
#MICE 모든 환자 : MICE_CBC_all.csv
df <- read.csv("CBC_220218_수정.csv",encoding = "UTF-8-BOM",check.names = FALSE)
str(df)
View(df)


col <- colnames(df[5:11])

df$group <- factor(df$group, 
                        c('1', '2', '3'),
                        labels = c("Group1","Group2","Group3"))

group1 <- df %>%  filter(group =="Group1")
group2 <- df %>%  filter(group =="Group2")
group3 <- df %>%  filter(group =="Group3")


attach(df)

# 평균 및 표준편차
# 반올림 및 평균 테스트
des1 <- round(mean(CRP),3)


#각 검사의 평균 및 표준편차 구해서 "test_summary_220224.txt"에 붙임
for(i in 5:length(df)){
  str <-paste("---- group1 mean sd : ",colnames(df[i]),"\n",sep="")
  write.table(str, file="test_summary_220224.txt", append=T, row.names = F, col.names = F)
  me <- round(mean(group1[[i]]),2)
  capture.output(me,file="test_summary_220224.txt", append=T)
  s <- round(sd(group1[[i]]),2)
  capture.output(s,file="test_summary_220224.txt", append=T)
  
  
  str <-paste("---- group2 mean sd : ",colnames(df[i]),"\n",sep="")
  write.table(str, file="test_summary_220224.txt", append=T, row.names = F, col.names = F)
  me <- round(mean(group2[[i]]),2)
  capture.output(me,file="test_summary_220224.txt", append=T)
  s <- round(sd(group2[[i]]),2)
  capture.output(s,file="test_summary_220224.txt", append=T)
  
  
  str <-paste("---- group3 mean sd : ",colnames(df[i]),"\n",sep="")
  write.table(str, file="test_summary_220224.txt", append=T, row.names = F, col.names = F)
  me <- round(mean(group3[[i]]),2)
  capture.output(me,file="test_summary_220224.txt", append=T)
  s <- round(sd(group3[[i]]),2)
  capture.output(s,file="test_summary_220224.txt", append=T)


  }



#정규성 선형그래프 만들어 보기 -> 눈으로만 봐서는 알 수 없음
qqnorm(CRP) 
qqline(CRP)

qqnorm(WBC)
qqline(WBC)

qqnorm(Neutrophil_count)
qqline(Neutrophil_count)

qqnorm(Lymphocyte_count)
qqline(Lymphocyte_count)

qqnorm(Platelet_count)
qqline(Platelet_count)

qqnorm(Neutrophil_to_Lymphocyte_ratio)
qqline(Neutrophil_to_Lymphocyte_ratio)

qqnorm(Platelet_to_lymphocyte_ratio)
qqline(Platelet_to_lymphocyte_ratio)



# Anderson-Darling nomality test를 통해 정규분포 검정 후 P-value 추출
# 추출된 결과 "test_summary_220224.txt"에 붙임
for(i in 5:length(df)){
  
  str <-paste("---- Anderson-Darling normality test: ", colnames(df[i]), "\n", sep="")
  write.table(str, file="test_summary_220224.txt", append=T, row.names = F, col.names = F)
  
  AD <- ad.test(df[[i]])
  capture.output(AD, file = "test_summary_220224.txt", append=T)
 
  
  
  
  str2 <-paste("---- Anova test (정규성 따르면): ", colnames(df[i]), "\n", sep="")
  write.table(str2, file="test_summary_220224.txt", append=T, row.names = F, col.names = F)
  
  aov_res <- aov(Neutrophil_to_Lymphocyte_ratio~group,data=df)
  aov_sum <- summary(aov_res)
  capture.output(aov_sum, file = "test_summary_220224.txt", append=T)
  
  
  

  str3 <-paste("---- Kruskal-Wallis rank sum test (정규성 따르지않으면) : ", colnames(df[i]), "\n", sep="")
  write.table(str3, file="test_summary_220224.txt", append=T, row.names = F, col.names = F)
  
  krus <- kruskal.test(df[[i]]~df[[1]])
  capture.output(krus, file = "test_summary_220224.txt", append=T)
  
  
  

}




# pairwise wilcox test를 통해 정규분포 검정 후 Group1-Group2, Group1-Group3, Group2-Group3 P-value 추출
# 추출된 결과 "test_summary_220224.txt"에 붙임
for(i in 5:length(df)){
  

str <-paste("---- pairwise.wilcox.test: ", colnames(df[i]), "\n", sep="")
write.table(str, file="test_summary_220224.txt", append=T, row.names = F, col.names = F)

pt <- pairwise.wilcox.test(df[[i]], df[[1]], p.adjust.method = "BH")
capture.output(pt, file = "test_summary_220224.txt", append=T)

pt_p <- pt$p.value
str <-paste("---- pairwise.wilcox.test full table: ", colnames(df[i]), "\n", sep="")
write.table(str, file="test_summary_220224.txt", append=T, row.names = F, col.names = F)
pt_table <- fullPTable(pt_p)


capture.output(pt_table, file = "test_summary_220224.txt", append=T)

}




###########그냥 한 번 해봄,,,,
mytable(group~.,data=df,method=3,catMethod = 1,show.all=T)
age_mean <- round(mean(as.numeric(unlist(group3$age)) , na.rm =T),3)
age_sd <-round(sd(as.numeric(unlist(group3$age)) , na.rm =T),3)





######## 그래프 제작 돌리지 마시오##############################################
################################################################################
df_len <- length(df)

lab_name <- df[,5:df_len] %>% names()
CBC_df <- df[,5:df_len]

# lab_name <- df[,3:df_len] %>% names()
# CBC_df <- df[,3:df_len]
new_df <- data.frame()

#result <- md.pattern(CBC_df)
#write.csv(result,file="result_MICE2.csv",row.names= TRUE)

#result_VIM <- aggr(CBC_df, prop = FALSE, numbers = TRUE)
# file_name <- paste("result_VIM2.png")
# hist_png <- png(filename = file_name)
# plot(result_VIM)
# dev.off()

#sum(is.na(CBC_df)) # missing data의 개수 확인

#mean(is.na(CBC_df)) # missing data가 차지하는 비율

#mean(!complete.cases(CBC_df)) # missing data가 있는 행이 전체에서 차지하는 비율


CBC_df$group <- df$group


group1 <- CBC_df %>%  filter(group =="Group1")
group2 <- CBC_df %>%  filter(group =="Group2")
group3 <- CBC_df %>%  filter(group =="Group3")




g1_mean <- c()
g1_sd <- c()
g2_mean <- c()
g2_sd <- c()
g3_mean <- c()
g3_sd <- c()


# MICE 사용하지 않고 결측값에 mean 또는 중앙값 삽입
# for(col in lab_name){
# group1[col][is.na(group1[col])] <- median(group1[col], na.rm = T)
# group1[col][is.na(group1[col])] <- mean(group1[col], na.rm= T)
# 
# group2[col][is.na(group1[col])] <- median(group2[col], na.rm = T)
# group2[col][is.na(group1[col])] <- mean(group2[col], na.rm= T)
# 
# group3[col][is.na(group1[col])] <- median(group3[col], na.rm = T)
# group3[col][is.na(group1[col])] <- mean(group3[col], na.rm= T)
# }


for(labs in lab_name){
  
  #group1 <- impute(group1[labs],mean)
  #group2 <- impute(group2[labs],mean)
  #group3 <- impute(group3[labs],mean)
  
  g1_mean <- round(mean(as.numeric(unlist(group1[labs])) , na.rm =T),3)
  g1_sd <-round(sd(as.numeric(unlist(group1[labs])) , na.rm =T),3)
  
  group1_value <- data.frame(group=c("Group1"),lab = c(labs), n=c(length(group1$group)),mean_data= c(g1_mean),sd_data= c(g1_sd))
  new_df <- rbind(new_df,group1_value)
  
  
  
  g2_mean <- round(mean(as.numeric(unlist(group2[labs])) , na.rm =T),3) 
  g2_sd <- round(sd(as.numeric(unlist(group2[labs])) , na.rm =T),3) 
  
  group2_value <- data.frame(group=c("Group2"),lab = c(labs), n=c(length(group2$group)), mean_data= c(g2_mean),sd_data= c(g2_sd))
  new_df <- rbind(new_df,group2_value)
  
  g3_mean <- round(mean(as.numeric(unlist(group3[labs])) , na.rm =T),3) 
  g3_sd <-round(sd(as.numeric(unlist(group3[labs])) , na.rm =T),3) 
  
  group3_value <- data.frame(group=c("Group3"),lab = c(labs), n=c(length(group3$group)), mean_data= c(g3_mean),sd_data= c(g3_sd))
  new_df <- rbind(new_df,group3_value)
  
}

#new_df <- new_df %>% mutate(se_data= sd_data/sqrt(n))
new_df$se_data <- c(round((new_df$sd_data/sqrt(new_df$n)),3))
#write.csv(new_df,file="CBC_calculate_220218_2.csv",row.names= FALSE)



i <- 1
for(CBC in lab_name){
  
  lab_result_data <- new_df %>% filter(lab == CBC)
  
  color_palette <- c("Group3"= "grey")

  
  WBC_graph <- ggplot(data =lab_result_data,aes(x= group, y=mean_data,fill=as.factor(group))) + 
    geom_col_pattern(position = "dodge",aes(pattern_fill = group), pattern = c(Group1="none",Group2="stripe",Group3="none"), alpha=0.0, colour  = 'grey',size= 1.0,width = 0.7, pattern_fill = 'grey',pattern_angle = 45,pattern_density = 0.1, pattern_spacing = 0.04) +
    geom_bar(stat = 'identity',colour="black",alpha= c(0.0,0.0,0.8),size= 1.0,width = 0.7) + 
    geom_errorbar(aes(x=group, ymin=mean_data-se_data, ymax=mean_data + se_data), width=0.6, alpha=0.9, size=1.0) +
    ggtitle(CBC) + 
    scale_fill_manual(values= color_palette) +
    theme_classic() +
    labs(fill = "group", y= element_blank(), x= element_blank()) + 
    theme(plot.title = element_text(hjust = 0.5, size = 30,face='bold' ),axis.text.x = element_text(size=25,face='bold'), axis.text.y = element_text(size =25),legend.title = element_blank(),legend.position = 'none')

  
  
  #그래프 저장
  file_name <- paste(CBC,"origin.tiff")
  hist_png <- png(filename = file_name,width = 600,height = 600)
  plot(WBC_graph)
  dev.off()
  # # 
  assign(paste0('a',i),WBC_graph)
  i<-i+1
  
}


total <- ggarrange(a5,a4,a2,a6,a1,a3,a7,a8, nrow=2, ncol=4)

#그래프 저장
# file_name <- paste("CBC_total.tiff")
# hist_png <- png(filename = "CBC_total")
# plot(total)
# dev.off()


