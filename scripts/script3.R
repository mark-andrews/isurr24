
# Load packages -----------------------------------------------------------

library(tidyverse)


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
