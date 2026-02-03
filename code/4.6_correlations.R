setwd("/Users/viktoria/Desktop/")
library(dplyr)
library(lme4)
library(lmerTest)
library(ggplot2)


#VISUAL DATA
VISUALdata <- read.csv("visual_data.csv")

VISUALdata <- VISUALdata %>%
  filter(If_word == "yes")

DATA_cleanV <- VISUALdata %>%
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
DATA_cleanV <- DATA_cleanV %>%
  group_by(Index) %>%
  mutate(zRT = scale(RT)) %>%
  ungroup()

model_visual1 <- lm(zRT ~ Word_length , data = DATA_cleanV) 
summary(model_visual1)

DATA_cleanV$residuals_visual <- residuals(model_visual1)

meanZRT_by_wordV <- DATA_cleanV %>%
  group_by(Word) %>%
  summarise(mean_zRT = mean(residuals_visual, na.rm = TRUE)) %>%
  ungroup()
head(meanZRT_by_wordV)

meanZRT_by_word <- meanZRT_by_wordV

meanZRT_by_word <- meanZRT_by_word %>%
  rename(mean_zRT_visual = mean_zRT)

#AUDIO ONSET DATA

AUDIOdata <- read.csv("audio_data.csv")
AUDIOdata <- AUDIOdata %>%
  filter(If_word == "yes")

delay = 0.200

AUDIOdata <- AUDIOdata %>%
  mutate(RT_onset =  RT - delay)

DATA_cleanA <- AUDIOdata %>%
  filter(RT_onset >= 0.170) %>%  # First exclude fast RTs (170 ms)
  group_by(Index) %>%  # Group by subject
  mutate(
    Q1 = quantile(RT_onset, 0.25, na.rm = TRUE),  # 25th percentile
    Q3 = quantile(RT_onset, 0.75, na.rm = TRUE),  # 75th percentile
    IQR = Q3 - Q1,                          # Interquartile range
    lower_bound = Q1 - 1.5 * IQR,           # Lower cutoff
    upper_bound = Q3 + 1.5 * IQR            # Upper cutoff
  ) %>%
  filter(RT_onset >= lower_bound & RT_onset <= upper_bound) %>%  # Keep only non-outliers
  dplyr::select(-Q1, -Q3, -IQR, -lower_bound, -upper_bound)  # Remove temporary columns

DATA_cleanA <- DATA_cleanA %>%
  group_by(Index) %>%
  mutate(zRT_onset = scale(RT_onset)) %>%
  ungroup()

model_audio1 <- lm(zRT_onset ~ Word_length, data = DATA_cleanA)
summary(model_audio1)

DATA_cleanA$residuals_audio <- residuals(model_audio1)

meanZRT_by_wordA <- DATA_cleanA %>%
  group_by(Word) %>%
  summarise(mean_zRT = mean(residuals_audio, na.rm = TRUE)) %>%
  ungroup()
head(meanZRT_by_wordA)

meanZRT_by_word$mean_zRT_audio_onset <- meanZRT_by_wordA$mean_zRT[match(meanZRT_by_wordA$Word, meanZRT_by_word$Word)]


#VISUAL AND AUDIO ONSET CORRELATION

shapiro.test(meanZRT_by_word$mean_zRT_visual)
shapiro.test(meanZRT_by_word$mean_zRT_audio_onset)

cor_result <- cor.test(meanZRT_by_word$mean_zRT_audio_onset, meanZRT_by_word$mean_zRT_visual, method = "spearman")
cor_result

cor_val <- round(cor_result$estimate, 3)
p_val <- round(cor_result$p.value, 3)

graph <- ggplot(meanZRT_by_word, aes(x = mean_zRT_audio_onset, y = mean_zRT_visual)) +
  geom_point(fill = "grey80", color = "black", shape = 21) +
  geom_smooth(method = "lm", se = FALSE, color = "grey30", linewidth = 0.8) +
  annotate("text", 
           x = min(meanZRT_by_word$mean_zRT_audio_onset, na.rm = TRUE), 
           y = max(meanZRT_by_word$mean_zRT_visual, na.rm = TRUE), 
           label = paste0("ρ = ", cor_val, "\np = ", p_val),
           hjust = 0, vjust = 1, size = 4, family = "Times New Roman") +
  labs(x = "Слуховая модальность (word onset)", 
       y = "Зрительная модальность",
       title = "Корреляция средних остатков регрессии z-оценок ВР по длине слова (одна кнопка)") +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman", size = 11)) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
graph

ggsave("visual_onset_correlation_rus.jpeg", plot = graph, width = 25, height = 15, units = c("cm"), dpi = 700)


graph <- ggplot(meanZRT_by_word, aes(x = mean_zRT_audio_onset, y = mean_zRT_visual)) +
  geom_point(fill = "grey80", color = "black", shape = 21) +
  geom_smooth(method = "lm", se = FALSE, color = "grey30", linewidth = 0.8) +
  annotate("text", 
           x = min(meanZRT_by_word$mean_zRT_audio_onset, na.rm = TRUE), 
           y = max(meanZRT_by_word$mean_zRT_visual, na.rm = TRUE), 
           label = paste0("ρ = ", cor_val, "\np = ", p_val),
           hjust = 0, vjust = 1, size = 4, family = "Times New Roman") +
  labs(x = "Audio modality (word onset)", 
       y = "Visual modality",
       title = "Correlation of mean residuals from regression of RT z-scores on word length (one button)") +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman", size = 11)) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
graph

ggsave("visual_onset_correlation_eng.jpeg", plot = graph, width = 25, height = 15, units = c("cm"), dpi = 700)





#AUDIO OFFSET DATA

AUDIOdata <- read.csv("audio_data.csv")
AUDIOdata <- AUDIOdata %>%
  filter(If_word == "yes")

AUDIOdata <- AUDIOdata %>%
  mutate(RT_offset = RT - delay - Word_length) 

DATA_cleanA <- AUDIOdata %>%
  group_by(Index) %>%  # Group by subject
  mutate(
    Q1 = quantile(RT_offset, 0.25, na.rm = TRUE),  # 25th percentile
    Q3 = quantile(RT_offset, 0.75, na.rm = TRUE),  # 75th percentile
    IQR = Q3 - Q1,                          # Interquartile range
    lower_bound = Q1 - 1.5 * IQR,           # Lower cutoff
    upper_bound = Q3 + 1.5 * IQR            # Upper cutoff
  ) %>%
  filter(RT_offset >= lower_bound & RT_offset <= upper_bound) %>%  # Keep only non-outliers
  dplyr::select(-Q1, -Q3, -IQR, -lower_bound, -upper_bound)  # Remove temporary columns

DATA_cleanA <- DATA_cleanA %>%
  group_by(Index) %>%
  mutate(zRT_offset = scale(RT_offset)) %>%
  ungroup()

meanZRT_by_wordA <- DATA_cleanA %>%
  group_by(Word) %>%
  summarise(mean_zRT = mean(zRT_offset, na.rm = TRUE)) %>%
  ungroup()
head(meanZRT_by_wordA)

meanZRT_by_word$mean_zRT_audio_offset <- meanZRT_by_wordA$mean_zRT[match(meanZRT_by_wordA$Word, meanZRT_by_word$Word)]


#VISUAL AND AUDIO OFFSET CORRELATION

shapiro.test(meanZRT_by_word$mean_zRT_audio_offset)

cor_result <- cor.test(meanZRT_by_word$mean_zRT_audio_offset, meanZRT_by_word$mean_zRT_visual, method = "pearson")
cor_result

cor_val <- round(cor_result$estimate, 3)
p_val <- round(cor_result$p.value, 3)

graph <- ggplot(meanZRT_by_word, aes(x = mean_zRT_audio_offset, y = mean_zRT_visual)) +
  geom_point(fill = "grey80", color = "black", shape = 21) +
  geom_smooth(method = "lm", se = FALSE, color = "grey30", linewidth = 0.8) +
  annotate("text", 
           x = min(meanZRT_by_word$mean_zRT_audio_offset, na.rm = TRUE), 
           y = max(meanZRT_by_word$mean_zRT_visual, na.rm = TRUE), 
           label = paste0("r = ", cor_val, "\np = ", p_val),
           hjust = 0, vjust = 1, size = 4, family = "Times New Roman") +
  labs(x = "Слуховая модальность (word offset)", 
       y = "Зрительная модальность",
       title = "Корреляция средних z-оценок ВР (СМ) и средних остатков регрессии z-оценок ВР по длине слова (ЗМ) (одна кнопка)") +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman", size = 11)) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
graph

ggsave("visual_offset_correlation_rus.jpeg", plot = graph, width = 25, height = 15, units = c("cm"), dpi = 700)


graph <- ggplot(meanZRT_by_word, aes(x = mean_zRT_audio_offset, y = mean_zRT_visual)) +
  geom_point(fill = "grey80", color = "black", shape = 21) +
  geom_smooth(method = "lm", se = FALSE, color = "grey30", linewidth = 0.8) +
  annotate("text", 
           x = min(meanZRT_by_word$mean_zRT_audio_offset, na.rm = TRUE), 
           y = max(meanZRT_by_word$mean_zRT_visual, na.rm = TRUE), 
           label = paste0("r = ", cor_val, "\np = ", p_val),
           hjust = 0, vjust = 1, size = 4, family = "Times New Roman") +
  labs(x = "Audio modality (word offset)", 
       y = "Visual modality",
       title = "Correlation of mean RT z-scores (AM) и mean residuals from regression of RT z-scores on word length (VM) (one button)") +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman", size = 11)) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
graph

ggsave("visual_offset_correlation_eng.jpeg", plot = graph, width = 25, height = 15, units = c("cm"), dpi = 700)







#AUDIO UNIQUENESS POINT DATA

AUDIOdata <- read.csv("audio_data.csv")
AUDIOdata <- AUDIOdata %>%
  filter(If_word == "yes")

AUDIOdata <- AUDIOdata %>%
  mutate(RT_unique = RT - delay - Uniqueness_point) 

DATA_cleanA <- AUDIOdata %>%
  group_by(Index) %>%  # Group by subject
  mutate(
    Q1 = quantile(RT_unique, 0.25, na.rm = TRUE),  # 25th percentile
    Q3 = quantile(RT_unique, 0.75, na.rm = TRUE),  # 75th percentile
    IQR = Q3 - Q1,                          # Interquartile range
    lower_bound = Q1 - 1.5 * IQR,           # Lower cutoff
    upper_bound = Q3 + 1.5 * IQR            # Upper cutoff
  ) %>%
  filter(RT_unique >= lower_bound & RT_unique <= upper_bound) %>%  # Keep only non-outliers
  dplyr::select(-Q1, -Q3, -IQR, -lower_bound, -upper_bound)  # Remove temporary columns

DATA_cleanA <- DATA_cleanA %>%
  group_by(Index) %>%
  mutate(zRT_unique = scale(RT_unique)) %>%
  ungroup()

meanZRT_by_wordA <- DATA_cleanA %>%
  group_by(Word) %>%
  summarise(mean_zRT = mean(zRT_unique, na.rm = TRUE)) %>%
  ungroup()
head(meanZRT_by_wordA)

meanZRT_by_word$mean_zRT_audio_UP <- meanZRT_by_wordA$mean_zRT[match(meanZRT_by_wordA$Word, meanZRT_by_word$Word)]


#VISUAL AND AUDIO UNIQUENESS POINT CORRELATION

shapiro.test(meanZRT_by_word$mean_zRT_audio_UP)

cor_result <- cor.test(meanZRT_by_word$mean_zRT_audio_UP, meanZRT_by_word$mean_zRT_visual, method = "spearman")
cor_result

cor_val <- round(cor_result$estimate, 3)
p_val <- round(cor_result$p.value, 3)

graph <- ggplot(meanZRT_by_word, aes(x = mean_zRT_audio_UP, y = mean_zRT_visual)) +
  geom_point(fill = "grey80", color = "black", shape = 21) +
  geom_smooth(method = "lm", se = FALSE, color = "grey30", linewidth = 0.8) +
  annotate("text", 
           x = min(meanZRT_by_word$mean_zRT_audio_UP, na.rm = TRUE), 
           y = max(meanZRT_by_word$mean_zRT_visual, na.rm = TRUE), 
           label = paste0("ρ = ", cor_val, "\np = ", p_val),
           hjust = 0, vjust = 1, size = 4, family = "Times New Roman") +
  labs(x = "Слуховая модальность (word uniqueness point)", 
       y = "Зрительная модальность",
       title = "Корреляция средних z-оценок ВР (СМ) и средних остатков регрессии z-оценок ВР по длине слова (ЗМ) (одна кнопка)") +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman", size = 11)) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
graph

ggsave("visual_UP_correlation_rus.jpeg", plot = graph, width = 25, height = 15, units = c("cm"), dpi = 700)


graph <- ggplot(meanZRT_by_word, aes(x = mean_zRT_audio_UP, y = mean_zRT_visual)) +
  geom_point(fill = "grey80", color = "black", shape = 21) +
  geom_smooth(method = "lm", se = FALSE, color = "grey30", linewidth = 0.8) +
  annotate("text", 
           x = min(meanZRT_by_word$mean_zRT_audio_UP, na.rm = TRUE), 
           y = max(meanZRT_by_word$mean_zRT_visual, na.rm = TRUE), 
           label = paste0("ρ = ", cor_val, "\np = ", p_val),
           hjust = 0, vjust = 1, size = 4, family = "Times New Roman") +
  labs(x = "Audio modality (word uniqueness point)", 
       y = "Visual modality",
       title = "Correlation of mean RT z-scores (AM) и mean residuals from regression of RT z-scores on word length (VM) (one button)") +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman", size = 11)) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
graph

ggsave("visual_UP_correlation_eng.jpeg", plot = graph, width = 25, height = 15, units = c("cm"), dpi = 700)
