library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)

penguins <- read_excel("C:/Users/Adedayo/Downloads/DTS_204_Dataset.xlsx")
colnames(penguins) <- trimws(tolower(colnames(penguins)))

head(penguins, 6)
colSums(is.na(penguins))

sapply(penguins[, c("bill_len", "bill_dep", "flipper_len", "body_mass")], function(x) {
  c(
    mean = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE)
  )
})

adelie_gentoo <- penguins[penguins$species %in% c("Adelie", "Gentoo") & !is.na(penguins$body_mass), ]
t_test_result <- t.test(body_mass ~ species, data = adelie_gentoo)
print(t_test_result)
names(penguins)
head(penguins)
unique(penguins$species)
aadelie_gentoo <- subset(penguins, species %in% c("Adelie", "Gentoo"))
adelie_gentoo <- na.omit(adelie_gentoo[, c("species", "body_mass")])
table(adelie_gentoo$species)
t.test(body_mass ~ species, data = adelie_gentoo)

sex_data <- subset(penguins, !is.na(sex))
male_count <- sum(sex_data$sex == "male")
total_count <- nrow(sex_data)
prop.test(male_count, total_count, p = 0.5)

num_vars <- penguins[, c("bill_len", "bill_dep", "flipper_len", "body_mass")]
head(num_vars)
corr_matrix <- cor(num_vars, use = "complete.obs")
corr_matrix

ggplot(melt(corr_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#4575b4", high = "#d73027", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), name = "Correlation") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +
  theme_minimal() +
  labs(title = "Correlation Heatmap of Penguin Variables", x = "", y = "")


lm(body_mass ~ flipper_len, data = penguins)
simple_model <- lm(body_mass ~ flipper_len, data = penguins)
summary(simple_model)

lm(body_mass ~ flipper_len + bill_len + bill_dep, data = penguins)
multi_model <- lm(body_mass ~ flipper_len + bill_len + bill_dep, data = penguins)
summary(multi_model)

ggplot(penguins, aes(x = flipper_len, y = body_mass)) +
  geom_point(color = "steelblue", alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  theme_minimal() +
  labs(
    title = "Scatter Plot of Body Mass vs. Flipper Length",
    x = "Flipper Length (mm)",
    y = "Body Mass (g)"
  )

ggplot(penguins, aes(x = species, y = body_mass, fill = species)) +
  geom_boxplot(alpha = 0.8) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "Body Mass Distribution across Penguin Species",
    x = "Penguin Species",
    y = "Body Mass (g)"
  )

ggplot(penguins, aes(x = body_mass)) +
  geom_histogram(aes(y = after_stat(density)), bins = 25, fill = "lightblue", color = "black", alpha = 0.7) +
  stat_function(
    fun = dnorm, 
    args = list(mean = mean(penguins$body_mass, na.rm = TRUE), 
                sd = sd(penguins$body_mass, na.rm = TRUE)), 
    color = "red", 
    linewidth = 1
  ) +
  theme_minimal() +
  labs(
    title = "Histogram of Body Mass with Normal Curve Overlay",
    x = "Body Mass (g)",
    y = "Density"
  )
prop.test(male_count, total_count, p = 0.5)
summary(multi_model)
