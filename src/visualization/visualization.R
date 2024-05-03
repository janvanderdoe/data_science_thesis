#import clean data
df <- read.csv("../../gen/input/amazon_search_hedonic_clean.csv", sep = ";")
library(ggplot2)
library(dplyr)

#join hedonic_utilitarian and search_experience together with a plus sign
df$product_type <- paste(df$hedonic_utilitarian, df$search_experience, sep = " + ")

#plot number of pictures vs helpfulness
ggplot(df, aes(x = n_pictures, y = helpful, color = product_type, linetype = product_type)) +
  geom_smooth(method = "lm", se = TRUE, aes(group = product_type), span = 0.5) +
  labs(title = "Number of pictures vs helpfulness", x = "Number of pictures", y = "Helpfulness") +
  theme_minimal() +
  scale_color_grey(name = "Product Type") +
  scale_linetype_discrete(name = "Product Type") +
  #xlim(0, 5) +
  #ylim(0, 4) +
  #put legend on top of graph on right
  theme(legend.position = c(0.75, 0.8)) +
  #increase font of everything
  theme(text = element_text(size = 15))

#days_diff versus helpfulness
ggplot(df, aes(x = days_diff, y = helpful)) +
  geom_smooth(se = TRUE, span = 0.5) +
  labs(title = "Days difference vs helpfulness", x = "Days since review written", y = "Helpfulness") +
  theme_minimal() +
  #xlim(0, 1000) +
  #ylim(0, 1) +
  #put legend on top of graph on right
  theme(legend.position = c(0.75, 0.8)) +
  #increase font of everything
  theme(text = element_text(size = 15)) +
  #center title
  theme(plot.title = element_text(hjust = 0.5)) +
  #change line to black
  geom_smooth(color = "black")

names(df)
#plot rating vs helpfulness in a bar plot
df %>% 
  group_by(rating) %>% 
  summarise(mean_helpful = mean(helpful)) %>% 
  ggplot(aes(x = rating, y = mean_helpful)) +
  geom_bar(stat = "identity", fill = "gray", color = "black") +
  labs(title = "Rating vs helpfulness", x = "Rating", y = "Mean helpfulness") +
  theme_minimal() +
  #put legend on top of graph on right
  theme(legend.position = c(0.75, 0.8)) +
  #increase font of everything
  theme(text = element_text(size = 15)) +
  #center title
  theme(plot.title = element_text(hjust = 0.5))
