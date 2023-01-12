
library(tidyverse)

# generate fake dataset, which different types of explanatory variable data types: categorical and numerics
df = tibble(`birth year` = runif(100, min = 1900, max = 1960) %>% round()) %>% 
  mutate(age = if_else(`birth year` < 1930, 
                       rnorm(100, 50, 5) %>% round(), 
                       rnorm(100, 60, 5) %>% round()),
         `in war` = case_when(`birth year` < 1920 ~ "yes",
                              `birth year` > 1950 ~ "kind of",
                              TRUE ~ "no"),
         sex = sample(c("male", "female"), 100, replace = TRUE)
  )



df %>% 
  ggplot(aes(y = `birth year`, x = age)) + 
  geom_point()
         
df %>% 
  ggplot(aes(y = `birth year`, x = `in war`)) + 
  geom_boxplot()

df %>% 
  ggplot(aes(y = `birth year`, x = sex)) + 
  geom_boxplot()

write.csv(df, file = 'fake data.csv', row.names = F)
