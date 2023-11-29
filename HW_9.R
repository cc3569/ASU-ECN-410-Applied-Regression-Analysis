#QUESTION 2
data = S22PRATE211

#creating a subset of data
subset = data[which(data$RV %in% c(0,1,2,3,8,9,10)),]

#OLS regression
model = lm(subset$ENDPCT70~subset$E1+subset$E2+subset$RV)
summary(model)

#Using RV as an indicator variable
model_ind = lm(subset$ENDPCT70~factor(subset$RV)+subset$E1+subset$E2)
summary(model_ind)

#OLS without RV
model_noRV = lm(subset$ENDPCT70~subset$E1+subset$E2)
summary(model_noRV)

#logistic regression
logistic_model = glm(ENDPCT70~E1+E2, family=binomial(), data=subset)
summary(logistic_model)

#concordance
library(survival)
concordance(logistic_model)

#G statistic calculation
##log likelihood with all coefficients = 0
subset$ll0 = log(abs(subset$ENDPCT70-mean(subset$ENDPCT70)))
##log likelihood of fitted model
subset$ll = ifelse(subset$ENDPCT70==1, log(fitted(logistic_model)), log(1-fitted(logistic_model)))
#difference in log likelihoods
diff_log_likelihood = sum(subset$ll0)-sum(subset$ll)
#G statistic
g_stat = -2*diff_log_likelihood

#finding probabilities
new_d = data.frame(E1=50,E2=50)
predict(logistic_model,newdata=new_d,type="response")

#QUESTION 3
data2 = NCAAM2022R12

#Logistic regression
logistic_model_2 = glm(HighWon~High+HWPCT+LWPCT, family=binomial(), data=data2)
summary(logistic_model_2)

#concordance
concordance(logistic_model_2)

#prediction values
new_d_2 = data.frame(High=4,HWPCT=.80,LWPCT=.70)
predict(logistic_model_2,newdata=new_d_2, data=data2, type="response")

new_d_3 = data.frame(High=4,HWPCT=.80,LWPCT=.90)
predict(logistic_model_2,newdata=new_d_3, data=data2, type="response")