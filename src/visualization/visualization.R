setwd("~/Git projects/dss_thesis/src/visualization")

#import clean data
df <- read.csv("../../gen/output/amazon_search_hedonic_clean_merged.csv", sep = ";")
library(ggplot2)
library(dplyr)

#join hedonic_utilitarian and search_experience together with a plus sign
df$product_type <- paste(df$hedonic_utilitarian, df$search_experience, sep = " + ")

#create dummy for helpful
df$helpful_dummy <- ifelse(df$helpful > 0, 1, 0)

#do number of pictures in buckets
df$n_pictures_cat <- ifelse(df$n_pictures == 0, 0, ifelse(df$n_pictures == 1, 1, ifelse(df$n_pictures == 2, 2, 3)))

#make a bar plot of n_pictures_cat by helpfulness grouped by product type
df %>% 
  group_by(n_pictures_cat, product_type) %>% 
  summarise(mean_helpful = mean(helpful_dummy)) %>% 
  ggplot(aes(x = factor(n_pictures_cat, c("0", "1", "2", "3")), y = mean_helpful, fill = product_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number of pictures vs helpfulness", x = "Number of pictures", y = "Mean helpfulness") +
  theme_minimal() +
  scale_fill_manual(values = c("darkgrey", "lightgrey", "black", "grey30")) + 
  #increase font of everything
  theme(text = element_text(size = 15)) +
  #center title
  theme(plot.title = element_text(hjust = 0.5)) +
  #y axis from 0 to 1
  ylim(0, 1) +
  #change x labels
  scale_x_discrete(labels = c("0", "1", "2", "3+")) +
  #change legend title
  labs(fill = "Product Type")

#make a bar plot of n_pictures_cat by helpfulness
df %>% 
  group_by(n_pictures_cat) %>% 
  summarise(mean_helpful = mean(helpful_dummy)) %>% 
  ggplot(aes(x = factor(n_pictures_cat, c("0", "1", "2", "3", "4")), y = mean_helpful)) +
  geom_bar(stat = "identity", fill = "gray") +
  labs(title = "Number of pictures vs helpfulness", x = "Number of pictures", y = "Mean helpfulness") +
  theme_minimal() +
  #put legend on top of graph on right
  theme(legend.position = c(0.75, 0.8)) +
  #increase font of everything
  theme(text = element_text(size = 15)) +
  #center title
  theme(plot.title = element_text(hjust = 0.5)

#plot number of pictures vs helpfulness
ggplot(df, aes(x = n_pictures, y = helpful_dummy, color = product_type, linetype = product_type)) +
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
ggplot(df, aes(x = days_diff, y = helpful_dummy)) +
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
  summarise(mean_helpful = mean(helpful_dummy)) %>% 
  ggplot(aes(x = rating, y = mean_helpful)) +
  geom_bar(stat = "identity", fill = "gray") +
  labs(title = "Rating vs helpfulness", x = "Rating", y = "Mean helpfulness") +
  theme_minimal() +
  #put legend on top of graph on right
  theme(legend.position = c(0.75, 0.8)) +
  #increase font of everything
  theme(text = element_text(size = 15)) +
  #center title
  theme(plot.title = element_text(hjust = 0.5))

#plot BERT performance
#make dataframe with BERT performance
bert_performance <- data.frame(
  Model = c("64", "128", "256", "512", "64", "128", "256", "512"),
  Split = c("validation", "validation", "validation", "validation",
          "training", "training", "training", "training"),
  Loss = c(0.7509, 0.7479, 0.7479, 1.516540647,
               0.7274, 0.7228, 0.7202, 1.557145596
               )
)


ggplot(bert_performance, aes(x = factor(Model, c("64", "128", "256", "512")), y = Loss, fill = Split)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "BERT performance", x = "Sequence Length", y = "Loss") +
  theme_minimal() +
  scale_fill_manual(values = c("darkgrey", "lightgrey")) +  # Define column colors
  #increase font of everything
  theme(text = element_text(size = 15)) +
  #center title
  theme(plot.title = element_text(hjust = 0.5))

bert_performance <- data.frame(
  Data = c("Control", "Control", "Control",
           "Control + Text", "Control + Text", "Control + Text",
           "Control + CP", "Control + CP", "Control + CP",
           "Control + CP + PF", "Control  + CP + PF", "Control + CP + PF"),
  Model = c("1 layer", "Random forest", "Decision tree",
            "1 layer", "Random forest", "Decision tree",
            "1 layer", "Random forest", "Decision tree",
            "1 layer", "Random forest", "Decision tree"),
  Accuracy = c(0.6857213377952576, 0.7275456489554203, 0.698593518670834,
           0.6365355849266052, 0.6582085869386413, 0.572791577562099,
           0.689504861831665, 0.6650764928442178, 0.600222075999342,
           0.5338871479034424, 0.6624444810001645, 0.6137933870702418)
  )

#make a bar chart of the BERT performance
ggplot(bert_performance, aes(x = Data, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Model comparison", x = "Model", y = "Accuracy") +
  theme_minimal() +
  scale_fill_manual(values = c("darkgrey", "lightgrey", "black", "grey30", "grey70")) +  # Define column colors
  #increase font of everything
  theme(text = element_text(size = 15)) +
  #center title
  theme(plot.title = element_text(hjust = 0.5)) +
  
