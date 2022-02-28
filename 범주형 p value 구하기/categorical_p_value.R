##install.packages("moonBook")
library(moonBook)


df <- read.csv("group_final_220218.csv")
str(df)

# 1,2,3으로 되어있는 그룹구분을 Group1,2,3으로 바꿈
df$group <- factor(df$group, 
                   c('1', '2', '3'),
                   labels = c("Group1","Group2","Group3"))

################## 테스트 용 ###################################################
ab <- table(df)
chisq.test(ab)
attach(df)
summary(df)
mytable(group~FANA,data=df,method=3, catMethod=0)
################################################################################

# 범주형 : 빈도를 통해 분석할 테이블 생성
M <- matrix(c(12585,0, 0, 0,1755,40,0,1023,62,0,401,30,0,225,37,0,376,80), ncol=3, byrow=T)
dimnames(M) <- list("type"=c("a","b","c","d","e","f"), "group"=c("group1", "group2","group3"))
M


# kruskal.test(M)
# M1_chisq <- chisq.test(M)
# M1_chisq$expected


# 분석하고자 하는 값 중 0 (<5)이 존재함 -> fisher test 수행
fisher.test(M,simulate.p.value=TRUE)


# 범주형 : 빈도를 통해 분석할 테이블 생성 -> 빈도 중에 0 (<5)가 존재 -> fisher test
M1 <- matrix(c(12585,0, 0, 0,3780,249), ncol=3, byrow=T)
dimnames(M1) <- list("type"=c("1","non_1"), "group"=c("group1", "group2","group3"))
M1

fisher.test(M1)

# 카이제곱은 그냥 해봤음
M1_chisq <- chisq.test(M1)
M1_chisq$expected

#아래는 모두 위와 같음
M2 <- matrix(c(0,1755, 40, 12585,2025,209), ncol=3, byrow=T)
dimnames(M2) <- list("type"=c("2","non_2"), "group"=c("group1", "group2","group3"))
M2
chisq.test(M2)

fisher.test(M2)





M3 <- matrix(c(0,1023, 62, 12585, 2757, 187), ncol=3, byrow=T)
dimnames(M3) <- list("type"=c("3","non_3"), "group"=c("group1", "group2","group3"))
M3
chisq.test(M3)

fisher.test(M3)





M4 <- matrix(c(0,401, 30, 12585, 3379, 219), ncol=3, byrow=T)
dimnames(M4) <- list("type"=c("4","non_4"), "group"=c("group1", "group2","group3"))

fisher.test(M4)




M5 <- matrix(c(0,225, 37, 12585, 3555, 212), ncol=3, byrow=T)
dimnames(M5) <- list("type"=c("5","non_5"), "group"=c("group1", "group2","group3"))

fisher.test(M5)





M6 <- matrix(c(0, 376, 80, 12585, 3404, 169), ncol=3, byrow=T)
dimnames(M6) <- list("type"=c("6","non_6"), "group"=c("group1", "group2","group3"))

fisher.test(M6)




Idopathic_inflammatory <- matrix(c(29, 31, 3, 12556, 3749, 246), ncol=3, byrow=T)
dimnames(Idopathic_inflammatory) <- list("type"=c("Y","N"), "group"=c("group1", "group2","group3"))

chisq.test(Idopathic_inflammatory)
fisher.test(Idopathic_inflammatory)




Juvenile_rheumatoid_arthritis <- matrix(c(31, 5, 0, 12554, 3775, 249), ncol=3, byrow=T)
dimnames(Juvenile_rheumatoid_arthritis) <- list("type"=c("Y","N"), "group"=c("group1", "group2","group3"))

chisq.test(Juvenile_rheumatoid_arthritis)
fisher.test(Juvenile_rheumatoid_arthritis)





Overlap_syndrome <- matrix(c(5, 31, 5, 12580, 3749, 244), ncol=3, byrow=T)
dimnames(Overlap_syndrome) <- list("type"=c("Y","N"), "group"=c("group1", "group2","group3"))

chisq.test(Overlap_syndrome)
fisher.test(Overlap_syndrome)





Sjogren_syndrome <- matrix(c(95, 186, 13, 12490, 3594, 236), ncol=3, byrow=T)
dimnames(Sjogren_syndrome) <- list("type"=c("Y","N"), "group"=c("group1", "group2","group3"))

chisq.test(Sjogren_syndrome)
fisher.test(Sjogren_syndrome)





Systemic_sclerosis <- matrix(c(28, 126, 6, 12557, 3654, 243), ncol=3, byrow=T)
dimnames(Systemic_sclerosis) <- list("type"=c("Y","N"), "group"=c("group1", "group2","group3"))

chisq.test(Systemic_sclerosis)
fisher.test(Systemic_sclerosis)





SLE <- matrix(c(0, 0, 249, 12585, 3780, 0), ncol=3, byrow=T)
dimnames(SLE) <- list("type"=c("Y","N"), "group"=c("group1", "group2","group3"))

chisq.test(SLE)
fisher.test(SLE)





Ah <- matrix(c(104, 207, 4, 12481, 3573, 245), ncol=3, byrow=T)
dimnames(Ah) <- list("type"=c("Y","N"), "group"=c("group1", "group2","group3"))

chisq.test(Ah)
fisher.test(Ah)





