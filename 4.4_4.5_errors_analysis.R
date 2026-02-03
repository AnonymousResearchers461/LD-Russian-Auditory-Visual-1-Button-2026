setwd("/Users/viktoria/Desktop/")
library(dplyr)
library(lme4)
library(lmerTest)


#gets SE from the model

get_se <- function(model, term) {
  if (!term %in% names(fixef(model))) {
    stop("Term '", term, "' is not found")
  }
  
  se <- sqrt(vcov(model)[term, term])
  return(se)
}





#DATASETS

AUDIOdata <- read.csv("LD_participants_data.csv")

AUDIOdata$word[AUDIOdata$word == "полет"] <- "полёт"
AUDIOdata$word[AUDIOdata$word == "щетка"] <- "щётка"
AUDIOdata$word[AUDIOdata$word == "тетя"] <- "тётя"

AUDIOdata <- AUDIOdata %>%
  filter(modality == 1) %>%
  filter(is_test == 1) %>%
  rename(RT = rt) %>%
  rename(Index = subject) %>%
  rename(Word = word) %>%
  rename(If_word = is_word) %>%
  rename(Answer = is_correct)

table(AUDIOdata$If_word, AUDIOdata$Answer)

AUDIOdata <- AUDIOdata %>%
  mutate(Answer = recode(Answer, `1` = "right", `0` = "wrong"))

AUDIOdata <- AUDIOdata %>%
  mutate(If_word = recode(If_word, `1` = "yes", `0` = "no"))

AUDIOdata_two <- AUDIOdata


VISUALdata <- read.csv("LD_participants_data.csv")

VISUALdata$word[VISUALdata$word == "полет"] <- "полёт"
VISUALdata$word[VISUALdata$word == "щетка"] <- "щётка"
VISUALdata$word[VISUALdata$word == "тетя"] <- "тётя"

VISUALdata <- VISUALdata %>%
  filter(modality == 0) %>%
  filter(is_test == 1) %>%
  rename(RT = rt) %>%
  rename(Index = subject) %>%
  rename(Word = word) %>%
  rename(If_word = is_word) %>%
  rename(Answer = is_correct)

table(VISUALdata$If_word, VISUALdata$Answer)

VISUALdata <- VISUALdata %>%
  mutate(Answer = recode(Answer, `1` = "right", `0` = "wrong"))

VISUALdata <- VISUALdata %>%
  mutate(If_word = recode(If_word, `1` = "yes", `0` = "no"))

VISUALdata_two <- VISUALdata


AUDIOdata_one <- read.csv("audio_data.csv")
table(AUDIOdata_one$If_word, AUDIOdata_one$Answer)

VISUALdata_one <- read.csv("visual_data.csv")
table(VISUALdata_one$If_word, VISUALdata_one$Answer)

AUDIOdata_two$Button <- "two"
VISUALdata_two$Button <- "two"
AUDIOdata_one$Button <- "one"
VISUALdata_one$Button <- "one"

AUDIOdata_two$Mod <- "audio"
VISUALdata_two$Mod <- "visual"
AUDIOdata_one$Mod <- "audio"
VISUALdata_one$Mod <- "visual"

AUDIOdata_two$Index <- paste(AUDIOdata_two$Mod, AUDIOdata_two$Button, AUDIOdata_two$Index, sep = "_")
VISUALdata_two$Index <- paste(VISUALdata_two$Mod, VISUALdata_two$Button, VISUALdata_two$Index, sep = "_")
AUDIOdata_one$Index <- paste(AUDIOdata_one$Mod, AUDIOdata_one$Button, AUDIOdata_one$Index, sep = "_")
VISUALdata_one$Index <- paste(VISUALdata_one$Mod, VISUALdata_one$Button, VISUALdata_one$Index, sep = "_")


two <- bind_rows(VISUALdata_two, AUDIOdata_two)
one <- bind_rows(VISUALdata_one, AUDIOdata_one)

two <- two %>% select(If_word, Word, Answer, Index, Button, Mod)
one <- one %>% select(If_word, Word, Answer, Index, Button, Mod)

one <- one %>%
  mutate(Mistake_type = case_when(
    If_word == "no" & Answer == "wrong" ~ "2 type",
    
    If_word == "yes" & Answer == "wrong" ~ "1 type",
    
    TRUE ~ "right"
  ))

two <- two %>%
  mutate(Mistake_type = case_when(
    If_word == "no" & Answer == "wrong" ~ "2 type",
    
    If_word == "yes" & Answer == "wrong" ~ "1 type",
    
    TRUE ~ "right"
  ))

video <- bind_rows(VISUALdata_two, VISUALdata_one)
audio <- bind_rows(AUDIOdata_two, AUDIOdata_one)

video <- video %>% select(If_word, Word, Answer, Index, Button, Mod)
audio <- audio %>% select(If_word, Word, Answer, Index, Button, Mod)

video <- video %>%
  mutate(Mistake_type = case_when(
    If_word == "no" & Answer == "wrong" ~ "2 type",
    
    If_word == "yes" & Answer == "wrong" ~ "1 type",
    
    TRUE ~ "right"
  ))

audio <- audio %>%
  mutate(Mistake_type = case_when(
    If_word == "no" & Answer == "wrong" ~ "2 type",
    
    If_word == "yes" & Answer == "wrong" ~ "1 type",
    
    TRUE ~ "right"
  ))

big <- bind_rows(audio, video)





#GENERAL MODEL

big <- big %>%
  mutate(
    Button = factor(Button, levels = c("one", "two")),
    Mod = factor(Mod, levels = c("visual", "audio")),
    
    correctness_num = as.numeric(Answer == "right"),
    correctness_fct = factor(Answer, levels = c("wrong", "right"))
  )

model_full <- glmer(
  correctness_num ~ Button * Mod + (1 | Index) + (1 | Word),
  data = big,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

summary(model_full)

OR <- exp(fixef(model_full)["Buttontwo"])
OR
se <- get_se(model_full, "Buttontwo")
se
OR_lower <- exp(fixef(model_full)["Buttontwo"] - 1.96 * se)
OR_lower
OR_upper <- exp(fixef(model_full)["Buttontwo"] + 1.96 * se)
OR_upper

OR <- exp(fixef(model_full)["Modaudio"])
OR
se <- get_se(model_full, "Modaudio")
se
OR_lower <- exp(fixef(model_full)["Modaudio"] - 1.96 * se)
OR_lower
OR_upper <- exp(fixef(model_full)["Modaudio"] + 1.96 * se)
OR_upper

OR <- exp(fixef(model_full)["Buttontwo:Modaudio"])
OR
se <- get_se(model_full, "Buttontwo:Modaudio")
se
OR_lower <- exp(fixef(model_full)["Buttontwo:Modaudio"] - 1.96 * se)
OR_lower
OR_upper <- exp(fixef(model_full)["Buttontwo:Modaudio"] + 1.96 * se)
OR_upper


errors <- big %>%
  filter(Answer == "wrong") %>%
  mutate(
    Type1 = as.numeric(Mistake_type == "1 type")
  )

model1 <- glmer(
  Type1 ~ Button * Mod + (1 | Index),
  data = errors,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

summary(model1)

OR <- exp(fixef(model1)["Buttontwo"])
OR
se <- get_se(model1, "Buttontwo")
se
OR_lower <- exp(fixef(model1)["Buttontwo"] - 1.96 * se)
OR_lower
OR_upper <- exp(fixef(model1)["Buttontwo"] + 1.96 * se)
OR_upper

OR <- exp(fixef(model1)["Modaudio"])
OR
se <- get_se(model1, "Modaudio")
se
OR_lower <- exp(fixef(model1)["Modaudio"] - 1.96 * se)
OR_lower
OR_upper <- exp(fixef(model1)["Modaudio"] + 1.96 * se)
OR_upper

OR <- exp(fixef(model1)["Buttontwo:Modaudio"])
OR
se <- get_se(model1, "Buttontwo:Modaudio")
se
OR_lower <- exp(fixef(model1)["Buttontwo:Modaudio"] - 1.96 * se)
OR_lower
OR_upper <- exp(fixef(model1)["Buttontwo:Modaudio"] + 1.96 * se)
OR_upper




#VISUAL

video <- video %>%
  mutate(
    Button = factor(Button, levels = c("one", "two")),
    correctness_num = as.numeric(Answer == "right"),  
    correctness_fct = factor(Answer, levels = c("wrong", "right"))
  )

model_full <- glmer(
  correctness_num ~ Button + (1 | Index) + (1 | Word),
  data = video,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

summary(model_full)

OR <- exp(fixef(model_full)["Buttontwo"])
OR
se <- get_se(model_full, "Buttontwo")
se
OR_lower <- exp(fixef(model_full)["Buttontwo"] - 1.96 * se)
OR_lower
OR_upper <- exp(fixef(model_full)["Buttontwo"] + 1.96 * se)
OR_upper



video_errors <- video %>%
  filter(Answer == "wrong") %>%
  mutate(
    Type1 = as.numeric(Mistake_type == "1 type")
  )

model1 <- glmer(
  Type1 ~ Button + (1 | Index),
  data = video_errors,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

summary(model1)

OR <- exp(fixef(model1)["Buttontwo"])
OR
se <- get_se(model1, "Buttontwo")
se
OR_lower <- exp(fixef(model1)["Buttontwo"] - 1.96 * se)
OR_lower
OR_upper <- exp(fixef(model1)["Buttontwo"] + 1.96 * se)
OR_upper




#AUDIO

audio <- audio %>%
  mutate(
    Button = factor(Button, levels = c("one", "two")),
    correctness_num = as.numeric(Answer == "right"), 
    correctness_fct = factor(Answer, levels = c("wrong", "right"))
  )

model_full <- glmer(
  correctness_num ~ Button + (1 | Index) + (1 | Word),
  data = audio,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

summary(model_full)

OR <- exp(fixef(model_full)["Buttontwo"])
OR
se <- get_se(model_full, "Buttontwo")
se
OR_lower <- exp(fixef(model_full)["Buttontwo"] - 1.96 * se)
OR_lower
OR_upper <- exp(fixef(model_full)["Buttontwo"] + 1.96 * se)
OR_upper



audio_errors <- audio %>%
  filter(Answer == "wrong") %>%
  mutate(
    Type1 = as.numeric(Mistake_type == "1 type")
  )

model1 <- glmer(
  Type1 ~ Button + (1 | Index),
  data = audio_errors,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

summary(model1)

OR <- exp(fixef(model1)["Buttontwo"])
OR
se <- get_se(model1, "Buttontwo")
se
OR_lower <- exp(fixef(model1)["Buttontwo"] - 1.96 * se)
OR_lower
OR_upper <- exp(fixef(model1)["Buttontwo"] + 1.96 * se)
OR_upper






# TWO BUTTONS

two <- two %>%
  mutate(
    Mod = factor(Mod, levels = c("visual", "audio")),
    correctness_num = as.numeric(Answer == "right"),
    correctness_fct = factor(Answer, levels = c("wrong", "right"))
  )

model_full <- glmer(
  correctness_num ~ Mod + (1 | Index) + (1 | Word),
  data = two,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

summary(model_full)

OR <- exp(fixef(model_full)["Modaudio"])
OR
se <- get_se(model_full, "Modaudio")
se
OR_lower <- exp(fixef(model_full)["Modaudio"] - 1.96 * se)
OR_lower
OR_upper <- exp(fixef(model_full)["Modaudio"] + 1.96 * se)
OR_upper

tab_model(
  model_full,
  show.se = TRUE,
  show.stat = TRUE,
  show.ci = FALSE,
  show.icc = TRUE,
  show.re.var = TRUE,
  pred.labels = c("(Intercept)", "Audio"),
  dv.labels = "Probability of correct answer")


two_errors <- two %>%
  filter(Answer == "wrong") %>%
  mutate(
    Type1 = as.numeric(Mistake_type == "1 type")
  )

model1 <- glmer(
  Type1 ~ Mod + (1 | Index),
  data = two_errors,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

summary(model1)

OR <- exp(fixef(model1)["Modaudio"])
OR
se <- get_se(model1, "Modaudio")
se
OR_lower <- exp(fixef(model1)["Modaudio"] - 1.96 * se)
OR_lower
OR_upper <- exp(fixef(model1)["Modaudio"] + 1.96 * se)
OR_upper





# ONE BUTTON

one <- one %>%
  mutate(
    Mod = factor(Mod, levels = c("visual", "audio")),
    correctness_num = as.numeric(Answer == "right"),
    correctness_fct = factor(Answer, levels = c("wrong", "right"))
  )

model_full <- glmer(
  correctness_num ~ Mod + (1 | Index) + (1 | Word),
  data = one,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

summary(model_full)

OR <- exp(fixef(model_full)["Modaudio"])
OR
se <- get_se(model_full, "Modaudio")
se
OR_lower <- exp(fixef(model_full)["Modaudio"] - 1.96 * se)
OR_lower
OR_upper <- exp(fixef(model_full)["Modaudio"] + 1.96 * se)
OR_upper



one_errors <- one %>%
  filter(Answer == "wrong") %>%
  mutate(
    Type1 = as.numeric(Mistake_type == "1 type")
  )

model1 <- glmer(
  Type1 ~ Mod + (1 | Index),
  data = one_errors,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

summary(model1)

OR <- exp(fixef(model1)["Modaudio"])
OR
se <- get_se(model1, "Modaudio")
se
OR_lower <- exp(fixef(model1)["Modaudio"] - 1.96 * se)
OR_lower
OR_upper <- exp(fixef(model1)["Modaudio"] + 1.96 * se)
OR_upper