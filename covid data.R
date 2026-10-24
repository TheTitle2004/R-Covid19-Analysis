data <- read.csv("C:/Users/royna/Downloads/COVID19_line_list_data.csv")
install.packages('Hmisc')
library(Hmisc)
describe(data)
View(data)

# cleaned up death column
data$death_dummy <- as.integer(data$death != 0)
unique(data$death_dummy)

# death rate
sum(data$death_dummy) / nrow(data)

# AGE
# claim: people who die are older than people who survive
dead = subset(data, death_dummy == 1)
alive = subset(data, death_dummy == 0)
num_dead <- sum(data$death_dummy, na.rm = TRUE)
num_dead
num_alive <- sum(data$death_dummy == 0, na.rm = TRUE)
num_alive

mean(dead$age, na.rm = TRUE)
mean(alive$age, na.rm = TRUE)
# is this statistically significant?
t.test(alive$age, dead$age, alternative="two.sided", conf.level = 0.99)
# normally, if p-value < 0.05, we reject null hypothesis
# here, p-value ~ 0, so we reject the null hypothesis and 
# conclude that this is statistically significant

# GENDER
# claim: gender has no effect
men = subset(data, gender == "male")
women = subset(data, gender == "female")
mean(men$death_dummy, na.rm = TRUE) #8.5%!
mean(women$death_dummy, na.rm = TRUE) #3.7%
# is this statistically significant?
t.test(men$death_dummy, women$death_dummy, alternative="two.sided", conf.level = 0.99)
# 99% confidence: men have from 0.8% to 8.8% higher chance
# of dying.
# p-value = 0.002 < 0.05, so this is statistically
# significant
