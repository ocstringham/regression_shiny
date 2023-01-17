
library(tidyverse)
library(patchwork)

# generate fake dataset, which different types of explanatory variable data types: categorical and numerics
df = tibble(`birth year` = runif(100, min = 1900, max = 1960) %>% round()) %>% 
  mutate(`age at death` = if_else(`birth year` < 1930, 
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





df = read.csv('fake data.csv')
mod = lm(age.at.death ~ birth.year + sex, df)
smod = summary(mod)
smod$coefficients

make_stars <- function(pval) {
  stars = ""
  if(pval <= 0.001)
    stars = "***"
  if(pval > 0.001 & pval <= 0.01)
    stars = "**"
  if(pval > 0.01 & pval <= 0.05)
    stars = "*"
  if(pval > 0.05 & pval <= 0.1)
    stars = "."
  stars
}

broom::tidy(mod) %>% mutate(estimate = round(estimate, 2), std.error = round(std.error, 2), 
                            statistic = round(statistic, 2)) %>% 
  mutate(signif = sapply(p.value, function(x) make_stars(x))) %>% 
  select(-p.value) %>% 
  rename(p.value = signif)

sum(smod$residuals ^ 2)
df
is.numeric(df$age)


ggplot(df, aes(group = sex, x = in.war, fill = sex)) + 
  geom_bar(position = position_dodge2(preserve = "single")) + 
  scale_fill_brewer(palette = 1, type = 'qual')

ggplot(df, aes(x = in.war, y = age, fill = "gray")) + 
  geom_boxplot() + 
  scale_fill_brewer(palette = 3, type = 'qual')


# p1 <- ggplot(mtcars) + geom_point(aes(mpg, disp))
# p2 <- ggplot(mtcars) + geom_boxplot(aes(gear, disp, group = gear))
# p3 <- ggplot(mtcars) + geom_bar(aes(gear)) + facet_wrap(~cyl)
# p4 <- ggplot(mtcars) + geom_bar(aes(carb))
# p5 <- ggplot(mtcars) + geom_violin(aes(cyl, mpg, group = cyl))
# 
# # Either add the plots as single arguments
# wrap_plots(p1, p2, p3, p4, p5)
# 
# # Or add them as a list...
# plots <- list(p1, p2, p3, p4, p5)
# wrap_plots(plots)

x =  "test"

case_when(x == "1" ~ "3",
          x == "test" ~ "ok", 
          TRUE ~ "")

