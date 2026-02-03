setwd("/Users/viktoria/Desktop/")
library(dplyr)
library(lme4)
library(lmerTest)
library(ggplot2)

#TWO BUTTONS DATA

VISUALdata <- read.csv("LD_participants_data.csv")

VISUALdata$word[VISUALdata$word == "полет"] <- "полёт"
VISUALdata$word[VISUALdata$word == "щетка"] <- "щётка"
VISUALdata$word[VISUALdata$word == "тетя"] <- "тётя"

VISUALdata <- VISUALdata %>%
  filter(modality == 0) %>%
  filter(is_test == 1) %>%
  filter(is_word == 1) %>%
  filter(is_correct == 1) %>%
  rename(RT = rt) %>%
  rename(Index = subject) %>%
  rename(Word = word)

DATA_cleanV_two <- VISUALdata %>%
  filter(RT >= 0.170) %>%  # First exclude fast RTs (170 ms)
  group_by(Index) %>%  # Group by subject
  mutate(
    Q1 = quantile(RT, 0.25, na.rm = TRUE),  # 25th percentile
    Q3 = quantile(RT, 0.75, na.rm = TRUE),  # 75th percentile
    IQR = Q3 - Q1,                          # Interquartile range
    lower_bound = Q1 - 1.5 * IQR,           # Lower cutoff
    upper_bound = Q3 + 1.5 * IQR            # Upper cutoff
  ) %>%
  filter(RT >= lower_bound & RT <= upper_bound) %>%  # Keep only non-outliers
  dplyr::select(-Q1, -Q3, -IQR, -lower_bound, -upper_bound)  # Remove temporary columns

# z-scores
DATA_cleanV_two <- DATA_cleanV_two %>%
  group_by(Index) %>%
  mutate(zRT = scale(RT)) %>%
  ungroup()


DATA_cleanV_two$Button <- "two"

big_data <- DATA_cleanV_two %>%
  select("Index", "Word", "RT", "zRT", "Button")


#ONE BUTTON DATA

VISUALdata <- read.csv("visual_data.csv")

VISUALdata <- VISUALdata %>%
  filter(If_word == "yes")

DATA_cleanV_one <- VISUALdata %>%
  filter(RT >= 0.170) %>%  # First exclude fast RTs (170 ms)
  group_by(Index) %>%  # Group by subject
  mutate(
    Q1 = quantile(RT, 0.25, na.rm = TRUE),  # 25th percentile
    Q3 = quantile(RT, 0.75, na.rm = TRUE),  # 75th percentile
    IQR = Q3 - Q1,                          # Interquartile range
    lower_bound = Q1 - 1.5 * IQR,           # Lower cutoff
    upper_bound = Q3 + 1.5 * IQR            # Upper cutoff
  ) %>%
  filter(RT >= lower_bound & RT <= upper_bound) %>%  # Keep only non-outliers
  dplyr::select(-Q1, -Q3, -IQR, -lower_bound, -upper_bound)  # Remove temporary columns

# z-scores
DATA_cleanV_one <- DATA_cleanV_one %>%
  group_by(Index) %>%
  mutate(zRT = scale(RT)) %>%
  ungroup()


DATA_cleanV_one$Button <- "one"

common_cols <- intersect(names(big_data), names(DATA_cleanV_one))
big_data <- bind_rows(
  big_data %>% select(all_of(common_cols)),
  DATA_cleanV_one %>% select(all_of(common_cols))
)




#REACTION TIME
model_visual <- lmer(RT ~ Button + (1 | Index) +  (1 | Word), data = big_data)
summary(model_visual)


big_data$Button <- factor(big_data$Button,
                   levels = c("one", "two"),
                   labels = c("Одна", "Две")
)

graph <- ggplot(big_data, aes(x = Button, y = RT)) +
  geom_violin(aes(fill = Button), trim = FALSE, linewidth = 0.3) +
  scale_fill_grey(start = 0.6, end = 0.8) +
  theme_bw() +
  geom_boxplot(outlier.shape = NA,
    width = 0.1, 
    linewidth = 0.3,        
    fill = 'white', 
    coef = 1.5                 
  ) +
  theme(text = element_text(family = "Times New Roman", size = 12)) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  xlab('Количество кнопок') + 
  ylab('Время реакции (с)') +
  labs(title = 'Время реакции на слова и количество кнопок (зрительная модальность)')
graph

ggsave("button_rt_rus.jpeg", plot = graph, width = 25, height = 15, units = c("cm"), dpi = 700)


big_data$Button <- factor(big_data$Button,
                          levels = c("Одна", "Две"),
                          labels = c("One", "Two")
)

graph <- ggplot(big_data, aes(x = Button, y = RT)) +
  geom_violin(aes(fill = Button), trim = FALSE, linewidth = 0.3) +
  scale_fill_grey(start = 0.6, end = 0.8) +
  theme_bw() +
  geom_boxplot(outlier.shape = NA,
               width = 0.1, 
               linewidth = 0.3,        
               fill = 'white', 
               coef = 1.5                 
  ) +
  theme(text = element_text(family = "Times New Roman", size = 12)) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  xlab('Buttons') + 
  ylab('Reaction time (s)') +
  labs(title = 'Reaction time on words and buttons (visual modality)')
graph

ggsave("button_rt_eng.jpeg", plot = graph, width = 25, height = 15, units = c("cm"), dpi = 700)






#CORRELATION (LENGTH CORRECTED)

DATA_cleanV_two$Word_length <- DATA_cleanV_one$Word_length[match(DATA_cleanV_two$Word, DATA_cleanV_one$Word)]

model_visual1 <- lm(zRT ~ Word_length , data = DATA_cleanV_two) 
summary(model_visual1)

DATA_cleanV_two$residuals_visual <- residuals(model_visual1)

meanZRT_by_wordV_two <- DATA_cleanV_two %>%
  group_by(Word) %>%
  summarise(mean_zRT = mean(residuals_visual, na.rm = TRUE)) %>%
  ungroup()
head(meanZRT_by_wordV_two)

meanZRT_by_wordV <- meanZRT_by_wordV_two

meanZRT_by_wordV <- meanZRT_by_wordV %>%
  rename(mean_zRT_two = mean_zRT)

model_visual1 <- lm(zRT ~ Word_length , data = DATA_cleanV_one) 
summary(model_visual1)

DATA_cleanV_one$residuals_visual <- residuals(model_visual1)

meanZRT_by_wordV_one <- DATA_cleanV_one %>%
  group_by(Word) %>%
  summarise(mean_zRT = mean(residuals_visual, na.rm = TRUE)) %>%
  ungroup()
head(meanZRT_by_wordV_one)

meanZRT_by_wordV$mean_zRT_one <- meanZRT_by_wordV_one$mean_zRT[match(meanZRT_by_wordV$Word, meanZRT_by_wordV_one$Word)]




shapiro.test(meanZRT_by_wordV$mean_zRT_one)
shapiro.test(meanZRT_by_wordV$mean_zRT_two)

cor_result <- cor.test(meanZRT_by_wordV$mean_zRT_one, meanZRT_by_wordV$mean_zRT_two, method = "spearman")
cor_result

cor_val <- round(cor_result$estimate, 3)



graph <- ggplot(meanZRT_by_wordV, aes(x = mean_zRT_one, y = mean_zRT_two)) +
  geom_point(fill = "grey80", color = "black", shape = 21) +
  geom_smooth(method = "lm", se = FALSE, color = "grey30", linewidth = 0.8) +
  annotate("text", 
           x = min(meanZRT_by_wordV$mean_zRT_one, na.rm = TRUE), 
           y = max(meanZRT_by_wordV$mean_zRT_two, na.rm = TRUE), 
           label = paste0("ρ = ", cor_val, "\np < 0.001"),
           hjust = 0, vjust = 1, size = 4, family = "Times New Roman") +
  labs(x = "Одна кнопка", 
       y = "Две кнопки",
       title = "Корреляция средних остатков регрессии z-оценок ВР по длине слова (ЗМ)") +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman", size = 11)) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
graph

ggsave("button_correlation_rus.jpeg", plot = graph, width = 25, height = 15, units = c("cm"), dpi = 700)


graph <- ggplot(meanZRT_by_wordV, aes(x = mean_zRT_one, y = mean_zRT_two)) +
  geom_point(fill = "grey80", color = "black", shape = 21) +
  geom_smooth(method = "lm", se = FALSE, color = "grey30", linewidth = 0.8) +
  annotate("text", 
           x = min(meanZRT_by_wordV$mean_zRT_one, na.rm = TRUE), 
           y = max(meanZRT_by_wordV$mean_zRT_two, na.rm = TRUE), 
           label = paste0("ρ = ", cor_val, "\np < 0.001"),
           hjust = 0, vjust = 1, size = 4, family = "Times New Roman") +
  labs(x = "One button", 
       y = "Two buttons",
       title = "Correlation of mean residuals from regression of RT z-scores on word length (VM)") +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman", size = 11)) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
graph

ggsave("button_correlation_eng.jpeg", plot = graph, width = 25, height = 15, units = c("cm"), dpi = 700)
