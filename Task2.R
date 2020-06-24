library(tidyverse)

library(modelr)

library(rpart)



df = read.csv("Data/ANZ synthesised transaction dataset.csv")



# Dataframe of estimated annual salary grouped by customer,where txn_description is PAY/SALARY.

ann_sal <- df[df$txn_description=='PAY/SALARY',c("amount","customer_id")] %>%
  
  group_by(customer_id) %>%
  
  summarise(annual_salary= sum(amount)/3 *12)



#Histogram of annual salary of customers          

hist(ann_sal$annual_salary[!ann_sal$annual_salary %in% boxplot.stats(ann_sal$annual_salary)$out], breaks=10, main = "Histogram of annual salary of customers", xlab= 'Income(AUD)')





# df_was: merge df with ann_sal

df_was <- df %>% merge(ann_sal) 



# df_was_se: select the columns to see the relationships

df_was_se <- df_was %>%
  
  select("gender","annual_salary","age","merchant_state","balance")

plot(df_was_se) #matrix plot of selected columns





##Linear model:

fit1 <- lm(annual_salary ~ age+balance+gender, data=df_was)

summary(fit1)

rmse(fit1,df_was)  #root-mean-square-error

plot(fit1$residuals, ylab = 'Residual')



##Decision Tree model:

# split into train and test datasets

smp_size <- floor(0.75 * nrow(df_was))

set.seed(1234) 

train_ind <- sample(seq_len(nrow(df_was)), size = smp_size)



#train datasets

df_was_train <- df_was[train_ind, ]

#test datasets

df_was_test <- df_was[-train_ind, ]  



#Decision Tree Plot

fit2 <- rpart(annual_salary ~ age+ balance+gender+merchant_state,method="anova",data=df_was)

plot(fit2,uniform=TRUE,
     
     main="Decision Tree for Annual Salary ")

text(fit2,use.n=TRUE, all=TRUE, cex=.55)

rmse(fit2, df_was_test)  #Root-mean-square-error