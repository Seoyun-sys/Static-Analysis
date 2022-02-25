#install.packages('mice')
#install.packages('VIM')

#install.packages('mice')
#install.packages('VIM')
library(mice)
library(MASS)
library(nnet)
library(nlme)
library(VIM)

# input data
#CBC_data_all_for_mice.csv : 모든 CBC  검사를 받지 않은 환자 포함
#CBC_data_remove_for_mice.csv : 모든 CBC  검사를 받지 않은 환자 제외
cbc_df <- read.csv("CBC_data_all.csv", sep = ",", header = TRUE)

#missing data pattern
#nhanes
#md.pattern(nhanes)

# Remove label and patid
imputing_df <- cbc_df[,3:10]

md.pattern(imputing_df)
p <- md.pairs(imputing_df)
p
write.csv(p, "observed_pairs2.csv")


marginplot(imputing_df[,c("WBC", "Platelet")], col=c("blue","red","orange"), cex=0.5, cex.lab=0.5, cex.numbers=1.3, pch=19)
pbox(imputing_df,pos=1,int=FALSE,cex=1.2)

#Creating imputations
#imp <- mice(nhanes)
#imp <- mice(df)
#imp_data <- mice(df, method = "norm")

# Run imputation
imp <- mice(imputing_df, m=1, maxit=500, method='cart', seed=500)
imp
imp$imp$WBC
imputed_df <- complete(imp)

md.pattern(imputed_df)

write.csv(imputed_df, "completely_imputed_data_CBC2.csv", row.names = FALSE)
