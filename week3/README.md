# random circumstance
結果無法預測

# probability
1. 在0與1之間
2. 所有事件機率總合為1

# determining probability
1. 長時間的資料蒐集
2. 推估、估算
*在機率描述的時候不可以針對個人<br/>
ex. 乘客有0.004的機率遺失包裹 or 0.4%的乘客有機會遺失包裹  vs. 你出國的話有0.004的機率遺失包裹<br/>
*error for estimated probability is calculated as 1/sqrt(n) <br/>
有標準差和信賴區間的概念

# defination
單一問題 ex.一周喝幾次酒
## simple space
所有可能出現事件的集合{1,2,3,4,5,6,7}
## event
### simple event
簡單事件{1},{4}
### compound event
合成事件(多個簡單事件組合起來){1,3,4}
## complementary events(互補事件)
彼此沒交集，合起來為1
## mutually exclusive events(互斥事件)
彼此沒交集，但不是互補事件
## independent and dependent events
兩個事件不互相影響即為獨立事件
## conditional probabilities(條件機率)
在某事件發生下，另一事件發生的機率  <br/>
ex.P(B|A)=  probability of B given A

# basic rules
1. P(A<sup>C</sup>)=1-P(A)<br/>
2. P(A or B)=P(A)+P(B)-P(A and B)<br />
3. P(A and B)=P(A)*P(B|A)<br/>
*if A and Bare independent P(A and B)=P(A)*P(B)

# flawed intuitive judgments about probability
1. confusion of the inverse<br/>
P(A|B) vs.P(B|A)
2. specific people versus random individuals<br/>是不是equal likely<br/>
你有50%的機率會離婚 vs. 在結婚時，隨機選取的婚姻有50%可能會離婚 or 長期來看有550%的婚姻會以離婚收場<br/>
3. coincidences(巧合)
兩次事件看起來有關係，實際上卻沒有連結，每個事件是獨立的，因此有人連贏兩次樂透是可能的，即使機率很低
4. gambler's fallacy
誤將long-run frequency 視為 short-run<br/>
個個事件是獨立的，而沒有記憶性<br/>
ex.連續賭輸九次不代表第十次會變好
