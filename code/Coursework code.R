# MATH5741 Coursework code

# Load in the data
mysample1 <- read.csv("data/coursework data.csv")
mysample1 <- mysample1[, -c(1)]

# Assigning names for columns for easier analysis
agefirst <- mysample1$AgeFirstKill
agelast <- mysample1$AgeLastKill
career <- mysample1$CareerDuration

# Summary statistics of AgeFirstKill, AgeLastKill and CareerDuration
summary_of_firstkill <- summary(agefirst)
summary_of_lastkill <- summary(agelast)
summary_of_careerduartion <- summary(career)

## Looking at histograms to find distributions.

# First Kill
par(mfrow = c(1,1))
hist(agefirst, breaks = 50, freq = FALSE, xlim=c(10,70),
     main = "Histogram for Age at First Kill", xlab = "Age at First Kill")
age_first_seq <- seq(from=min(agefirst), to=max(agefirst), by=0.01)
lines(age_first_seq, dnorm(age_first_seq, mean = mean(agefirst)-2.5, sd = sd(agefirst)-1),
      lwd=2, col='red')

# Last Kill
hist(agelast, breaks = 50, freq = FALSE, xlim=c(10,70),
     main = "Historgram for Age at Last Kill", xlab = "Age at Last Kill")
age_last_seq <- seq(from=min(agelast), to=max(agelast), by=0.01)
lines(age_last_seq, dnorm(age_last_seq, mean=mean(agelast)-2, sd=sd(agelast)-1),
      lwd=2, col='red')

# Career Duration
hist(career, breaks = 50, freq = FALSE, xlim=c(0,40),
     main = "Histogram for Career Duration", xlab = "Career Duration") 
career_seq <- seq(from=min(career), to=max(career), by=0.01)
lines(career_seq, dexp(career_seq, rate = 2/5), lwd = 2, col='red')

# Boxplot for age at first kill with each motive.
boxplot(AgeFirstKill ~ Motive, data=mysample1, las=1, 
        names = c("Anger", "Mental illness", "Revenge"))

# Creating data frames for each Motive
anger <- mysample1[which(mysample1$Motive == "Anger (including mission-oriented killers)"),]
revenge <- mysample1[which(mysample1$Motive == "Revenge or vigilante justice"),]
mental_illness <- mysample1[which(mysample1$Motive == 
                                    "Mental illness (including paranoia, visionary or Munchausen's syndrome)"),]

# Defining the AgeFirstKill variable in each of the motive dataframes
agefirst_anger <- anger$AgeFirstKill
agefirst_revenge <- revenge$AgeFirstKill
agefirst_mental <- mental_illness$AgeFirstKill

# Working with AgeFirstKill for anger
summary(agefirst_anger)

par(mfrow = c(1,2))
hist(agefirst_anger, breaks = 20, freq = FALSE, xlim=c(10,70),
main='Histogram for the Age at First Kill for killers \n with Anger as their motive',
  xlab = 'Age at First Kill for the Anger motive')
agefirst_anger_seq <- seq(from=min(agefirst_anger), to=max(agefirst_anger), by=0.01)
lines(agefirst_anger_seq, dnorm(agefirst_anger_seq, mean=mean(agefirst_anger),
                                sd=sd(agefirst_anger)), lwd=2, col='red')
qqnorm(agefirst_anger, pch=1, frame = FALSE, main="Normal Q-Q Plot for the Age at First
       Kill for killers with Anger as their motive")
qqline(agefirst_anger, col='steelblue', lwd=2)

# Hypothesis test for anger (z-test)
anger_n <- length(agefirst_anger)
Z <- abs((mean(agefirst_anger) - 27)/(sqrt(var(agefirst_anger)/anger_n)))
Z > abs(qnorm(p=0.025))

# Confidence interval and p-value for anger
CI <- mean(agefirst_anger) + c(-1,1)*abs(qnorm(p=0.025))*sqrt(var(agefirst_anger)/anger_n)
p_value <- 2*pnorm(-Z)*100

# Working with AgeFirstKill for revenge
summary(agefirst_revenge)

par(mfrow = c(1,2))
hist(agefirst_revenge, breaks = 20, freq = FALSE,
     main='Histogram for the Age at First Kill for killers \n with Revenge as their motive',
     xlab = 'Age at First Kill for the Revenge motive')
agefirst_revenge_seq <- seq(from=min(agefirst_revenge), to=max(agefirst_revenge), by=0.01)
lines(agefirst_revenge_seq, dnorm(agefirst_revenge_seq, mean=mean(agefirst_revenge),
                                sd=sd(agefirst_revenge)), lwd=2, col='red')
qqnorm(agefirst_revenge, pch=1, frame = FALSE, main="Normal Q-Q Plot for the Age at First
       Kill for killers with revenge as their motive")
qqline(agefirst_revenge, col='steelblue', lwd=2)

# Hypothesis test for revenge (z-test)
revenge_n <- length(agefirst_revenge)
Z <- abs((mean(agefirst_revenge) - 27)/(sqrt(var(agefirst_revenge)/revenge_n)))
Z > abs(qnorm(p=0.025))

# Confidence interval and p-value for anger
CI <- mean(agefirst_revenge) + c(-1,1)*abs(qnorm(p=0.025))*sqrt(var(agefirst_revenge)/revenge_n)
p_value <- 2*pnorm(-Z)*100

# Working with AgeFirstKill for revenge
summary(agefirst_mental)

par(mfrow = c(1,2))
hist(agefirst_mental, breaks = 15, freq = FALSE,
     main='Histogram for the Age at First Kill for killers \n with Mental Illness as their motive',
     xlab = 'Age at First Kill for the Mental Illness motive')
agefirst_mental_seq <- seq(from=min(agefirst_mental), to=max(agefirst_mental), by=0.01)
lines(agefirst_mental_seq, dnorm(agefirst_mental_seq, mean=mean(agefirst_mental),
                                  sd=sd(agefirst_mental)), lwd=2, col='red')
qqnorm(agefirst_mental, pch=1, frame = FALSE, main="Normal Q-Q Plot for the Age at First
       Kill for killers with Mental Illness as their motive")
qqline(agefirst_mental, col='steelblue', lwd=2)

### Hypothesis test for Mental Illness (t-test)
# sample variance for mental illness
mental_n <- length(agefirst_mental)
TT <- abs((mean(agefirst_mental) - 27)/(sqrt(s_squared_mental/mental_n)))
TT > abs(qt(0.975, df=(mental_n-1)))

# Confidence interval and p-value for Mental Illness
CI <- mean(agefirst_mental) + c(-1,1)*abs(qt(0.975, df=(mental_n-1)))*sqrt(var(agefirst_mental)/mental_n)
p_value <- 2*pt(-TT, df=(mental_n-1))*100

### Paired test between anger and revenge
# Checking which test to use
ratio1 <- sd(agefirst_anger)/sd(agefirst_revenge)
1/3 < ratio1
ratio1 < 3

# Variances are equal, perform test
meandiff1 <- mean(agefirst_anger) - mean(agefirst_revenge)
t.test(agefirst_anger, agefirst_revenge, mu = 0, paired=FALSE, var.equal = TRUE,
      conf.level=0.95)
# Fail to reject H0 here as 0 is in the interval, get p-value
p_value <- 200*pt(-0.70097, df=266)

### Paired test between anger and mental illness
# Check which test to use
sd(agefirst_anger)/sd(agefirst_mental) 

# Variances are equal, perform test
meandiff2 <- mean(agefirst_anger) - mean(agefirst_mental)
t.test(agefirst_anger, agefirst_mental, mu=0, paired=FALSE, var.equal = TRUE, 
       conf.level = 0.95)

# Fail to reject H0 as 0 is in the interval, get p-value
p_value <- 200*pt(-0.87917, df=226)

### Paired test between revenge and mental illness
# Check which test to use
sd(agefirst_revenge)/sd(agefirst_mental)

# Variances are equal, perform test
meandiff3 <- mean(agefirst_revenge)-mean(agefirst_mental)
t.test(agefirst_revenge, agefirst_mental, mu=0, paired=FALSE, var.equal = TRUE,
       conf.level = 0.95)
# Fail to reject H0 as 0 is in the interval, get p-value
p_value <- 200*pt(-1.0787, df=80)