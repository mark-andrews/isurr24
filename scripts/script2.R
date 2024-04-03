# A script that reads in data files and does some data wrangling.


# Load up my packages -----------------------------------------------------

library(tidyverse) # loads up the tidyverse package 
library(skimr)     # load up skimr
library(readxl)    # load up the read_excel commands etc etc

# Read in the data set ----------------------------------------------------

# read in the weight.csv file
weight_df <- read_csv("data/weight.csv")

# Exploratory analysis ----------------------------------------------------

# to view more of the data, use glimpse
glimpse(weight_df)

# quick summary of the data with skim
skim(weight_df)

# Read in an Excel --------------------------------------------------------

era_df <- read_excel('data/ERA_DATA_ALL.xlsx', sheet = 'Channel_Averages_by_Category')
# same as previous command because "Channel_Averages_by_Category" is first sheet
era_df <- read_excel('data/ERA_DATA_ALL.xlsx')

# reads in another sheet
era_df2 <- read_excel('data/ERA_DATA_ALL.xlsx', sheet = 'Channel Information')


# Read in blp data --------------------------------------------------------

blp_df <- read_csv('data/blp-trials-short.txt')

# Selecting columns with select -------------------------------------------

select(blp_df, participant, spell, rt)
select(blp_df, lex:rt)
select(blp_df, participant, resp:rt.raw)
# drop columns
select(blp_df, -prev.rt, -rt.raw)
# select by column index
select(blp_df, 2:5)

select(blp_df, starts_with('r'))
select(blp_df, ends_with('t'))
select(blp_df, contains('rt'))
select(blp_df, matches('^rt'))     # starts with 'rt'
select(blp_df, matches('rt$'))     # ends with 'rt'
select(blp_df, matches('^rt|rt$')) # begins with or ends with 'rt'

# Filtering rows with filter ----------------------------------------------

filter(blp_df, lex == 'W') # select all rows where lex is equal to 'W'

# select rows where rt is less than 1000 ms
filter(blp_df, rt <= 1000)

# select accurate trials
filter(blp_df, lex == resp)

# select accurate trial AND where rt is not slow (i.e. not > 1000)
filter(blp_df, lex == resp, rt <= 1000)

# select accurate trial AND where rt is not slow (i.e. not > 1000)
# AND where rt is not too fast (i.e. where rt is not < 450)
filter(blp_df, lex == resp, rt <= 1000, rt >= 450)


# Create new columns with mutate ------------------------------------------

mutate(blp_df, accuracy = lex == resp)
mutate(blp_df, accuracy = lex == resp, fast_rt = rt < 450)
mutate(blp_df, accuracy = lex == resp, good_rt = rt > 450 & rt < 1000)

# column indicating stimulus length 
mutate(blp_df, length = str_length(spell))

# Renaming columns --------------------------------------------------------

rename(blp_df, stimulus = spell, reaction_time = rt)

# Sorting ----------------------------------------------------------------

arrange(blp_df, rt) # ascending order
arrange(blp_df, desc(rt)) # descending order
arrange(blp_df, spell)
arrange(blp_df, desc(spell))
