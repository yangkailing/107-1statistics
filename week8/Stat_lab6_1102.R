
# 107-1 Statistics Lab6: Estimating proportions with confidence
#已知樣本比例模擬信賴區間
Prop_CI = function(n, p, confd) {
 
  all.phat = c()
  for (i in 1:10000) {
    phat = sum(sample(c(0,1), n, replace = T, prob = c(1-p,p)))/n
    all.phat[i] = phat
  }
  
  all.phat = sort(all.phat)
  
  low.per = ceiling(((1-confd)/2)*10000)
  low.vlue = all.phat[low.per]
  high.per = ceiling((1-((1-confd)/2))*10000)
  high.vlue = all.phat[high.per]
  
  results = c(paste((1-confd)/2*100,"%"), paste((1-((1-confd)/2))*100,"%"), low.vlue, high.vlue)
  
  mean.p = mean(all.phat)
  sd.p = sd(all.phat)
  
  CI = list(CI = matrix(results, nrow = 2, byrow = T), mean = mean.p, sd = sd.p )
  
  return(CI)
}

Prop_CI(883,0.68,0.95)


# By calculation
#手動計算信賴區間
n = 883
p = 0.68

z = qnorm((1-0.95)/2, lower.tail = F)
se = sqrt(p*(1-p)/n)

high.c = p + z*se; high.c
low.c = p - z*se; low.c


