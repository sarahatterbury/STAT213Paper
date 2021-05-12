#DESCRIPTIVE
##class
class.freq <- table(law_resume$class)
class.freq

length(law_resume$class)

class.relfreq<- class.freq/length(law_resume$class)
class.relfreq
##interview
interview.freq <- table(law_resume$outcome)
interview.freq

length(law_resume$outcome)

interview.relfreq<- interview.freq/length(law_resume$outcome)
interview.relfreq

##bivariate
classint <-table(law_resume$class, law_resume$outcome)
classint

prop.table(classint)
prop.table(classint, margin = 1)

prop.table(classint, margin = 2)
barplot(prop.table(classint, margin=2),legend=TRUE,col=c("green","blue"),main = "Class Given Interview Outcome")

#hypothesis testing two proportions
x1 <- 16
n1 <- 159
x2 <- 6
n2 <- 157
p1 <- x1/n1
p2 <- x2/n2

pc <- (n1*p1+n2*p2)/(n1+n2)
z <- (p1 - p2)/sqrt(pc*(1-pc)*(1/n1+1/n2))
z

pval <- 1-pnorm(z,0,1)
pval

#test of independence
classout <- matrix(c(16,6,143,151), nrow = 2, ncol = 2, byrow = TRUE)
classout

classouttest <- chisq.test(classout, correct = TRUE)
classouttest

classouttest$residuals^2

