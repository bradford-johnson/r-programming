# anova analysis
library(tidyverse)

# get data
df <- msleep %>%
  select(vore, sleep_rem) %>%
  drop_na()

# density plots of rem sleep -> facet by eating category
df %>%
  ggplot(aes(x = sleep_rem, y = ..density..)) +
  geom_density(fill = "#6195CC", alpha = .5) +
  facet_wrap(~vore) +
  labs(title = "Distribution of REM Sleep by Eating Category",
       x = "Density",
       y = "REM Sleep") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

## null hypothesis: no difference in the avg rem sleep across eating categories

# model 1 - anova analysis

model_1 <- aov(sleep_rem ~ vore, data = df)

summary(model_1)

## p < .05 -> reject null that there is no difference
## statistically significant

# all of the above in one step
msleep %>%
  select(vore, sleep_rem) %>%
  drop_na() %>%
  aov(sleep_rem ~ vore, data = .) %>%
  summary()