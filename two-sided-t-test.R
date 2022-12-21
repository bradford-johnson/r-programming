# two sided t-test
library(tidyverse)
library(gapminder)

# get data
df <- gapminder %>%
  filter(continent %in% c("Africa", "Europe"))

## null hypothesis: no difference in the means
## alternative hypothesis: true difference in the means

# t-test
df %>%
  t.test(lifeExp ~ continent, data = ., alternative = "two.sided")

## p < .05 -> reject null that there is no difference
## statistically significant, true difference

# density plot
## get means
means_df <- df %>%
  group_by(continent) %>%
  summarize(mean = mean(lifeExp))

df %>%
  ggplot(aes(x = lifeExp, y = after_stat(density), fill = continent)) +
  geom_density(alpha = .45) +
  geom_vline(data = means_df, linetype = 2,
             aes(xintercept = mean, color = continent)) +
  labs(title = "Density plot of life expectancy in Africa and Europe",
       x = "Life Expectancy",
       y = "") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        panel.grid = element_blank())