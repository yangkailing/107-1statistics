
#setwd("")
load("UCBAdmissions.Rdata")

# Q1: One population proportion --------------------------------------------------------------------
head(A)
str(A)
names(A)

B<-A[A$Dept== "B", ]
alpha=0.005
z_half_alpha = qnorm(alpha, lower.tail = F)

x = xtabs(Freq ~ Admit, data = A)
x

Suc = x[1]
Rej = x[2]
p_hat = Suc / sum(x)

z_score = (p_hat-0.4) / sqrt(0.4*0.6/sum(x))
z_score

#From z-score
if (abs(z_score) >= abs(z_half_alpha)) {
  CHECK = "Reject NULL"
} else {
  CHECK = "Can NOT reject NULL"
}

#From p-value and alpha
p.value = pnorm(abs(z_score),lower.tail = F)*2
p.value

if(p.value<=alpha){
  CHECK2 = "Reject NULL"
}else {
  CHECK2 = "Can NOT reject NULL"
}

print(CHECK)
print(CHECK2)


#  Q2: Two population proportions --------------------------------------------------------------------

z_half_alpha = qnorm(0.005, lower.tail = F)

x2 = xtabs(Freq ~ Gender + Admit, data = A)
x2

x_Male = x2[1,1]
x_Female = x2[2,1]

Total_Male = sum(x2[1,])
Total_Female = sum(x2[2,])

Male_Rate = x_Male / Total_Male
Female_Rate = x_Female / Total_Female

#common population proportion
Total_Rate = (x2[1,1]+x2[2,1]) / sum(x2)

se2 = sqrt(Total_Rate * (1-Total_Rate) * (1/Total_Male+1/Total_Female))
z2_score = ((Male_Rate - Female_Rate) - 0) / se2
z2_score


#From z-score
if (abs(z2_score) >= abs(z_half_alpha)) {
  CHECK = "Reject NULL"
} else {
  CHECK = "Can NOT reject NULL"
}

#From p-value and alpha
p.value = (1-pnorm(z2_score))
p.value

if(p.value<=alpha){
  CHECK2 = "Reject NULL"
}else {
  CHECK2 = "Can NOT reject NULL"
}

print(CHECK)
print(CHECK2)


# prop.test() -------------------------------------------------------------

## Q1: one population proportion
prop.test(x = x, n = sum(x), 
          p = 0.4, alternative = "two.sided", 
          conf.level = 0.99, correct = F)
z_score11 = sqrt(2.8255)
z_score11

## Q2: two population proportions
prop.test(x = c(x_Male, x_Female), n = c(Total_Male, Total_Female), 
          alternative = "two.sided", 
          conf.level = 0.99, correct = F)

z_score22 = sqrt(92.205)
z_score22







