# Load required libraries.
library(ggplot2)
library(scales)
library(tidyr)

# Clear environment variables.
rm(list = ls())

# Read in the data file.
payments <- read.csv("./data/payments.csv", header = TRUE, stringsAsFactors = FALSE)

# Select only the New York payments.
ny_payments = subset(payments, Provider.State == "NY")

# What is the relationship between mean covered charges (Average.Covered.Charges)
# and mean total payments (Average.Total.Payments) in New York?
ggplot(data = ny_payments, aes(x = Average.Covered.Charges, y = Average.Total.Payments)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = lm) +
  xlab("Average Covered Charges") +
  ylab("Average Total Payments") +
  ggtitle("Charges vs. Payments") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  ggsave("./output/file1.pdf", width = 8.5, height = 11, units = "in")

# How does the relationship between mean covered charges (Average.Covered.Charges)
# and mean total payments (Average.Total.Payments) vary by medical condition
# (DRG.Definition) and the state in which care was received (Provider.State)?
payments <- separate(payments, col = DRG.Definition, into = c("DRG.Definition.1", "DRG.Definition.2"), sep = " - ")
ggplot(data = payments, aes(x = Average.Covered.Charges, y = Average.Total.Payments)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = lm) +
  ggtitle("Charges vs. Payments by Condition and State") +
  facet_grid(DRG.Definition.1 ~ Provider.State) +
  scale_y_continuous(labels = comma) +
  theme(strip.text.x = element_text(size = 6), strip.text.y = element_text(size = 6)) +
  ggsave("./output/file2.pdf", width = 8.5, height = 11, units = "in")
