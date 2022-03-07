#OUR ASSIGNMENT 2

library(RODBC)
library(tidyverse)

# Connect to MySQL 
db =odbcConnect("monserv",uid="root",pwd="eliott00!")
sqlQuery(db, "USE ma_charity_full")

# Merge Assignment 2 and donors metrics  from database

query = "SELECT a.contact_id, a.calibration, a.donation, a.amount, c.recency, c.frequency, c.avgamount, c.maxamount, d.gender, d.active
        FROM assignment2 a
        LEFT JOIN (SELECT contact_id,
                  DATEDIFF(20180626, MAX(act_date)) / 365 AS 'recency',
                  COUNT(amount) AS 'frequency',
                  AVG(amount) AS 'avgamount',
                  MAX(amount) AS 'maxamount'
                  FROM acts
                  WHERE act_type_id = 'DO'
                  group by contact_id) as c
        ON a.contact_id = c.contact_id
        LEFT JOIN (SELECT prefix_id as 'gender', 
                  active,
                  id
                  FROM contacts) as d
        ON a.contact_id = d.id
        group by a.contact_id"

data = sqlQuery(db, query)


#Add new predictors

#AVG/Donation by month (here July)

query="SELECT a.contact_id, c.avgamount as m7
FROM assignment2 a
LEFT JOIN (SELECT contact_id,
           AVG(amount)  AS 'avgamount'
           FROM acts
           WHERE MONTH(act_date) = 7
           group by contact_id) as c
ON a.contact_id = c.contact_id
order by a.contact_id"

avg_m7 = sqlQuery(db, query)
avg_m7[is.na(avg_m7)] <- 0

data <- left_join(data,avg_m7)

#Amount per campaign 

query="SELECT a.contact_id, c.do_per_camp
FROM assignment2 a
LEFT JOIN (SELECT contact_id,
           SUM(amount)/COUNT(message_id) as do_per_camp
           FROM acts
           WHERE act_type_id = 'DO'
           group by contact_id) as c
ON a.contact_id = c.contact_id
Order by a.contact_id"

do_per_camp = sqlQuery(db, query)
do_per_camp[is.na(do_per_camp)] <- 0

data <- left_join(data,do_per_camp)

#Response rate per campaign

query="SELECT a.contact_id, d.answer/c.solicitation as answer_rate
FROM assignment2 a
LEFT JOIN (SELECT contact_id,
           COUNT(contact_id) as solicitation
           FROM actions
           group by contact_id) as c
ON a.contact_id = c.contact_id
LEFT JOIN (SELECT contact_id,
           count(campaign_id) as answer
           FROM acts
           WHERE act_type_id = 'DO' AND campaign_id IS NOT NULL
           GROUP BY 1) AS d
ON a.contact_id = d.contact_id
Order by a.contact_id"

answer_rate = sqlQuery(db, query)
data <- left_join(data,answer_rate)

#Amount donated last year

query="SELECT a.contact_id, c.last_year_do
FROM assignment2 a
LEFT JOIN (SELECT contact_id,
           SUM(amount) as last_year_do
           FROM acts
           WHERE act_type_id ='DO' AND act_date > 20170628
           group by contact_id) as c
ON a.contact_id = c.contact_id
Order by a.contact_id"

last_year_do = sqlQuery(db, query)
last_year_do[is.na(last_year_do)] <- 0

data <- left_join(data,last_year_do)


#Dealing with gender:
data$MME = ifelse(data$gender == 'MME', 1, 0)
data$MR = ifelse(data$gender == 'MR', 1, 0)
data$MLLE = ifelse(data$gender == 'MLLE', 1, 0)

#Select the training data with calibration
train_data = data %>% filter(calibration == 1)

#Dealing with missing values:
#The one having no donation we put a recency of 25  years(quantile 0.9 recency observed)
train_data$recency[is.na(train_data$recency)] <- quantile(train_data$recency[!is.na(train_data$frequency)],0.9)

#The one having no donation we put a frequency of 0.5  years(avoid problem with log)
train_data$frequency[is.na(train_data$frequency)] <- 0.5

#Set all null values to 0 (expect donation which is the target).
train_data[,-4][is.na(train_data[,-4])] <- 0

summary(train_data)

library(nnet)

prob.model = multinom(formula = donation ~ (recency * frequency) + log(recency) + log(frequency) + m7 + answer_rate + do_per_camp,
                      data = train_data)

#print(summary(prob.model))

#Gain Chart

# # Obtain predictions (on the same data set)
# probs  = predict(object = prob.model, newdata = train_data, type = "probs")
# # Rank order target variable in decreasing order of (predicted) probability
# target = train_data$donation[order(probs, decreasing=TRUE)] / sum(train_data$donation)
# gainchart = c(0, cumsum(target))
# # Create a random selection sequence
# random = seq(0, to = 1, length.out = length(train_data$donation))
# # Create the "perfect" selection sequence
# perfect = train_data$donation[order(train_data$donation, decreasing=TRUE)] / sum(train_data$donation)
# perfect = c(0, cumsum(perfect))
# # Plot gain chart, add random line
# plot(gainchart)
# lines(random)
# lines(perfect)

#Lift/Improvement
# Compute 1%, 5%, 10%, and 25% lift and improvement
q = c(0.01, 0.05, 0.10, 0.25)
x = quantile(gainchart, probs = q)
z = quantile(perfect,   probs = q)
print("Hit rate:")
print(x)
print("Lift:")
print(x/q)
print("Improvement:")
print((x-q)/(z-q))


# In-sample, donation amount model

z = which(!is.na(train_data$amount))
amount.model = lm(formula = log(amount) ~ sqrt(avgamount) + sqrt(maxamount) + sqrt(m7) + do_per_camp + sqrt(last_year_do) ,
                  data = train_data[z, ])

#print(summary(amount.model))

#Gain Chart

# Obtain predictions (on the same data set)
probs  = exp(predict(object = amount.model, newdata = train_data[z,]))
# Rank order target variable in decreasing order of (predicted) probability
target = train_data[z,]$amount[order(probs, decreasing=TRUE)] / sum(train_data[z,]$amount)
gainchart = c(0, cumsum(target))
# Create a random selection sequence
random = seq(0, to = 1, length.out = length(train_data[z,]$amount))
# Create the "perfect" selection sequence
perfect = train_data[z,]$amount[order(train_data[z,]$amount, decreasing=TRUE)] / sum(train_data[z,]$amount)
perfect = c(0, cumsum(perfect))
# Plot gain chart, add random line
# lines(random)
# lines(perfect)
# plot(gainchart)

#Lift/Improvement
# Compute 1%, 5%, 10%, and 25% lift and improvement

q = c(0.01, 0.05, 0.10, 0.25)
x = quantile(gainchart, probs = q)
z = quantile(perfect,   probs = q)
print("Hit rate:")
print(x)
print("Lift:")
print(x/q)
print("Improvement:")
print((x-q)/(z-q)) 

#Prediction DataFrame
newdata = data %>% filter(calibration == 0)

#Dealing with missing values:

#The one having no donation we put a recency of 25  years(max recency observed)
newdata$recency[is.na(newdata$recency)] <- quantile(newdata$recency[!is.na(newdata$recency)],0.9)

#The one having no donation we put a frequency of 0.5  years(avoid problem with log)
newdata$frequency[is.na(newdata$frequency)] <- 0.5

#Set all null values to 0 (expect donation which is the target).
newdata[c(-3,-4)][is.na(newdata[c(-3,-4)])] <- 0

out=data.frame(contact_id=newdata$contact_id)
out$probs  = predict(object = prob.model, newdata = newdata, type = "probs")
out$amount = exp(predict(object = amount.model, newdata = newdata))
out$score  = out$probs * out$amount
out$output = ifelse(out$score >=2, 1, 0)

z = which(out$score > 2)
print(length(z)/length(out$score))
print(length(z))

#Final Output

text = data.frame(contact_id = out$contact_id, solicit = out$output)
write.table(text, file = "final.txt", sep = "\t", row.names =FALSE, col.names = FALSE)
