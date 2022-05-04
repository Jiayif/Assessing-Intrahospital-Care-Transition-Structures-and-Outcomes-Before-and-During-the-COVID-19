library(MASS)
setwd("~/Downloads/trauma-selected")
readfile <- read.csv('pre_outcomes.csv')
#head(readfile)
data <- data.frame(readfile)

#model<- polr(as.factor(los) ~ path_len + pimd_iss + diagnose + Age + indegree_average + outdegree_average + betweenness_average + closeness_average + indegree_bs + outdegree_bs + betweenness_bs + closeness_bs + indegree_min + outdegree_min + betweenness_min + closeness_min + indegree_first + outdegree_first + betweenness_first + closeness_first + indegree_median + outdegree_median + betweenness_median + closeness_median + indegree_third + outdegree_third + betweenness_third + closeness_third + indegree_max + outdegree_max + betweenness_max + closeness_max, data = data, Hess = TRUE, control = list(maxit = 50))  # vaginal, remove gender
#c(indegree_average,outdegree_average,betweenness_average,closeness_average,indegree_bs,outdegree_bs,betweenness_bs,closeness_bs,indegree_min,outdegree_min,betweenness_min,closeness_min,indegree_first,outdegree_first,betweenness_first,closeness_first,indegree_median,outdegree_median,betweenness_median,closeness_median,indegree_third,outdegree_third,betweenness_third,closeness_third,indegree_max,outdegree_max,betweenness_max,closeness_max)

#model<- polr(as.factor(pimd_los) ~ pimd_iss + diagnose + Age + outdegree_average, data = data, Hess = TRUE, control = list(maxit = 50))
#options(scipen = 0)
#ctable <- coef(summary(model))
#p = pnorm(abs(ctable[,'t value']), lower.tail=FALSE) *2
#ctable <- cbind(ctable, 'p value'=p)
#ci <- confint(model)
#exp_ci = exp(cbind(OR=coef(model),ci))
#exp_ci_p = cbind(exp_ci,p)
#print(exp_ci_p)

for(n in 16:50){
  print(colnames(data)[n])
  model<- polr(as.factor(data$pimd_los) ~ data$GENDER_CONCEPT_ID + data$pimd_iss + data$diagnose + data$Age + data[,n], Hess = TRUE) 
  options(scipen = 0)
  ctable <- coef(summary(model))
  p = pnorm(abs(ctable[,'t value']), lower.tail=FALSE) *2
  exp_ci_p = cbind(exp(coef(model)),p[1:5])
  print(exp_ci_p)
}

library(MASS)
setwd("~/Downloads/trauma-selected")
readfile <- read.csv('intra_outcomes.csv')
#head(readfile)
data <- data.frame(readfile)

for(n in 44:50){
  print(colnames(data)[n])
  model<- polr(as.factor(data$pimd_los) ~ data$GENDER_CONCEPT_ID + data$pimd_iss + data$diagnose + data$Age + data[,n], Hess = TRUE) 
  options(scipen = 0)
  ctable <- coef(summary(model))
  p = pnorm(abs(ctable[,'t value']), lower.tail=FALSE) *2
  exp_ci_p = cbind(exp(coef(model)),p[1:5])
  print(exp_ci_p)
}

print(colnames(data)[51])
model<- polr(as.factor(data$pimd_los) ~ data$GENDER_CONCEPT_ID + data$pimd_iss + data$diagnose + data$Age + data[,51], Hess = TRUE) 
options(scipen = 0)
ctable <- coef(summary(model))
p = pnorm(abs(ctable[,'t value']), lower.tail=FALSE) *2
exp_ci_p = cbind(exp(coef(model)),p[1:5])
print(exp_ci_p)