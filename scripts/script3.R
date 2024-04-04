
# Load packages -----------------------------------------------------------

library(tidyverse)
library(afex)
library(emmeans)
library(skimr)
library(effectsize)

# Load data ---------------------------------------------------------------

weight_df <- read_csv("data/weight.csv")

# average height and standard dev of height
summarise(weight_df, 
          avg_height = mean(height), 
          std_height = sd(height))

options(pillar.sigfig = 8) # 8 significant figures

summarise(group_by(weight_df, gender),
          avg_height = mean(height), 
          std_height = sd(height))

# boxplots

ggplot(weight_df, aes(x = gender, y = height)) + geom_boxplot()


# independent samples t - test --------------------------------------------

result_1 <- t.test(height ~ gender, data = weight_df, var.equal = TRUE)

# one sample t-test -------------------------------------------------------

weight_male_df <- filter(weight_df, gender == 'Male')

result_2 <- t.test(height ~ 1, mu = 175, data = weight_male_df)

format(7.474e-09, scientific = FALSE)

options(scipen = 50) # higher the scipen, the higher the number needs to be before scientific notation 

# Paired samples t-test ---------------------------------------------------

sleep_df <- as_tibble(sleep) # convert base R data frame to tibble

result_3 <- t.test(extra ~ group, data = sleep, paired = TRUE)

# Correlation analysis ----------------------------------------------------

result_4 <- cor.test(~ height + weight, data = weight_df)
result_5 <- cor.test(~ height + weight, data = weight_male_df)

result_6 <- cor.test(~ height + weight, data = weight_df, method = 'spearman')


# Simple linear regression ------------------------------------------------

# some data viz for starters
ggplot(weight_male_df, aes(x = height, y = weight)) + 
  geom_point(size = 0.75, colour = 'blue', shape = 23) +
  geom_smooth(method = 'gam', colour = 'red') +
  geom_smooth(method = 'lm')

result_7 <- lm(weight ~ height, data = weight_male_df)
summary(result_7)


weight_male_df2 <- mutate(weight_male_df, norm_height = height - mean(height))

result_8 <- lm(weight ~ norm_height, data = weight_male_df2)
summary(result_8)

# One way Anova -----------------------------------------------------------

laptop_df <- as_tibble(laptop_urry)
skim(laptop_df)

ggplot(laptop_df, aes(x = talk, y = overall)) + geom_boxplot()

result_9 <- aov(overall ~ talk, data = laptop_df)
summary(result_9)

# F(4, 137) = 7.425, p = 0.0000192

# effect sizes
eta_squared(result_9, partial = FALSE)
cohens_f(result_9)

emmeans(result_9, specs = pairwise ~ talk, adjust = 'bonf')

# Factorial ANOVA ---------------------------------------------------------

ggplot(laptop_df, 
       aes(x = talk, y = overall, fill = condition)) + 
  geom_boxplot()


result_10 <- aov(overall ~ talk * condition, 
                 data = laptop_df)
result_10a <- aov(overall ~ talk + condition + talk:condition, 
                  data = laptop_df)

summary(result_10)

# oneway anova with condition as IV
result_11 <- aov(overall ~ condition, data = laptop_df)

ggplot(laptop_df, 
       aes(x = condition, y = overall)) + 
  geom_boxplot()

eta_squared(result_10)
cohens_f(result_10)

emmeans(result_10, specs = pairwise ~ talk | condition)
emmeans(result_10, specs = pairwise ~ condition | talk)


# Multiple linear regression ----------------------------------------------

result_12 <- lm(weight ~ height + age, data = weight_df)

summary(result_12)

# null model
result_13 <- lm(weight ~ 1, data = weight_df)

# model comparison
anova(result_13, result_12) # comparing the null model with model 12


result_14 <- lm(weight ~ height + age + gender, data = weight_df)

# re code the gender variable as numeric 0 and 1
# where 1 is for Male and 0 is for female
mutate(weight_df, gender = if_else(gender == 'Male', 1, 0))

# repeated measures Anova -------------------------------------------------

self_esteem <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntupsychology-data/main/data-sets/selfesteem_long.csv")

result_15 <- aov_car(esteem ~ Error(id/time), data = self_esteem)
summary(result_15)

emmeans(result_15, specs = pairwise ~ time, adjust = 'bonf')
