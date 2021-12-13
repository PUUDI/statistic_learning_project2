


boxplot(train$EDUC~train$Group,data=train)
boxplot(train$MMSE~train$Group,data=train)







A=lm(train$CDR~train$MR.Delay+train$Visit)

library(mctest)
imcdiag(A)


A=lm(train$CDR~train$eTIV+train$ASF)

library(mctest)
imcdiag(A)


train$X=NULL
train$Subject.ID=NULL
train$MRI.ID = NULL
train$Group = NULL
train$Hand = NULL
train$M.F = NULL
train$CDR = NULL
train$SES = NULL


mahalanobis(train, colMeans(train), cov(train))
train$mahal = mahalanobis(train, colMeans(train), cov(train))
train$p <- pchisq(train$mahal, df=3, lower.tail=FALSE)
train$out=seq(1,298)
for(i in 1:298){
  if(train$p[i]<0.001){
    train$out[i]=1
  }else{
    train$out[i]=0
  }
}

sum(train$out)
