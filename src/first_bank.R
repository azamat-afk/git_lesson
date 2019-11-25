library(earth)
data(etitanic)
summary(etitanic)
?etitanic

head(etitanic)
tail(etitanic)

library(dplyr)
tot <- tally(etitanic)
tot
titanic_df <- tbl_df(etitanic)
survivors <- filter(etitanic, sex == "male")
length(sex)
summary(survivors)

surv_male <- tally(survivors)
surv_f <- filter(titanic_df, sex == "female")
surv_female <- tally(surv_f)

total <- surv_f+survivors

rate_m <- surv_male/tot
rate_m

rate_f <- surv_female/tot
rate_f

titanic_df %>%
  group_by (sex, survived) %>%
  summarise (number = n()) %>%
  mutate (freq = (number / sum(number)*100))



library(sqldf)

sqldf('select sex, 100 * avg(survived) [%Surv] from etitanic group by sex order by sex desc')

surv_m <- nrow(subset(etitanic, sex == "male" & survived == 1))
total_m <- nrow(subset(etitanic, sex == "male"))
rate_m <- surv_m/total_m*100
rate_m

surv_f <- nrow(subset(etitanic, sex == "female" & survived == 1))
total_f <- nrow(subset(etitanic, sex == "female"))
rate_f <- surv_f/total_f*100
rate_f

counts <- table(etitanic$survived, etitanic$sex)


barplot(counts, ylim=c(0, 1000), xlab="Gender",ylab="Count",main="Survival Rate by Gender", col=c("red","darkblue"))
text(.71, 420, paste(as.character(round(rate_f, 1) ),"% Survived"))
text(1.9, 690, paste(as.character(round(rate_m, 1) ),"% Survived"))
legend("bottomright", fill=c("red", "blue"), legend=c("Perished", "Survived"))

library(sqldf)
sqldf('select LoanID, orig from table_1 
inner join ( select LoanID, max(date) MaxDate
  from table_1 group by LoanID ) tm
  on table_1.LoanID = tm.LoanID and table_1.date = tm.MaxDate')

sqldf('select LoanID, max(date) MaxDate
  from table_1
      group by LoanID')      

sqldf('select LoanID, orig
from table_1 where (LoanID, date) in
(
  select LoanID, max(date)
  from table_1
  group by LoanID
)')

sqldf('select LoanID, max(date) date, orig from table_1 group by LoanID')
sqldf('select LoanID, orig from table_1 where date=(select Max(date) from table_1 group by LoanID)')


#logistic regression
library(earth)
data(etitanic)

set.seed(123)
train.index <- sample(1:nrow(etitanic), size=0.7*nrow(etitanic))

train.data <- etitanic[train.index,]
test.data <- etitanic[-train.index,]


### Two Competing Models
logit.mod.1 <- glm(survived ~ ., data=train.data, family=binomial)
logit.mod.2 <- glm(survived ~ sex, data=train.data, family=binomial)

summary(logit.mod.1)
logit.mod.1$aic
summary(logit.mod.2)
logit.mod.2$aic

logit.mod.3 <- glm(survived ~ ., data=test.data, family=binomial)
logit.mod.4 <- glm(survived ~ sex, data=test.data, family=binomial)
summary(logit.mod.3)
logit.mod.3$aic
summary(logit.mod.4)
logit.mod.4$aic

sqldf('select count(LoanID) Statements from table_1 where year(date) = 2005')
