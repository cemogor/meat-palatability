
# LOAD REVELANT PACKAGES --------------------------------------------------

rm(list=ls())
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggsci) 
library(tidyr)
library(stringr)
library(ggsci)
library(gghalves)
library(ggpubr)
library(performance)
library(ggeffects)
library(emmeans)


####################################################
####################################################
####################################################
#######                                     ########
#######             CATEGORY MODEL          ########
#######                                     ########
####################################################
####################################################
####################################################

#NOTE: variation in palatability scores across respondent categories and meat types

median.score <- read.csv("01. Data/median.score.csv")


count.cat <- median.score %>% 
  mutate(number = 1) %>% 
  group_by(category, type.respondent) %>% 
  summarise(count.cat = sum(number)) %>% 
  ungroup()

median.score <- median.score %>% 
  mutate(category = factor(category),
         type.respondent = factor(type.respondent)) %>% 
  left_join(count.cat)


boxplot.respondent <- ggplot(median.score, aes(x = type.respondent, y = median.score)) +
  geom_boxplot(fill='grey', color="black", alpha = 0.5)+
  theme_pubr(base_size = 15) +
  theme(axis.text = element_text(size = 17),
        axis.text.x = element_text(size = 17),
        axis.title = element_text(size = 20)) +
  labs(x="Respondent category", y= "Palatability") +
  scale_y_continuous(breaks = seq(0,10,2)) +
  scale_x_discrete(labels = c("Household", "Hunter", "Vendor"))

boxplot.respondent

boxplot.meat <- ggplot(median.score, aes(x = category, y = median.score)) +
  geom_boxplot(fill='grey', color="black", alpha = 0.5)+
  theme_pubr(base_size = 20) +
  theme(axis.text = element_text(size = 17),
        axis.text.x = element_text(size = 15),
        axis.title = element_text(size = 20)) +
  labs(x="Meat type", y= "Palatability") +
  scale_y_continuous(breaks = seq(0,10,2)) +
  scale_x_discrete(labels = c("Domestic", "Fish", "Invertebrate", "Wild"))

boxplot.meat

#median.score1 <- median.score %>% 
 # filter(type.respondent == "Market")


# FIT MODEL ---------------------------------------------------------------


model1a <- lm(median.score ~  type.respondent + category, data = median.score)
model1b <- lm(median.score ~  type.respondent * category, data = median.score)


# COMPARE MODELS ----------------------------------------------------------

plot(compare_performance(model1a, model1b))

compare_performance(model1a, model1b)


# MODEL DIAGNOSTICS -------------------------------------------------------

model1 <- model1a

check_model(model1)


summary(model1)

# QUICK VISUALISATION -----------------------------------------------------

plot(ggpredict(model1, c("category")))

plot(ggpredict(model1, c("type.respondent")))


# POST-HOC TEST -----------------------------------------------------------

emmeans.model1.meat = emmeans(model1, ~ category)
pairs(emmeans.model1.meat,
      adjust="tukey")


emmeans.model1.cat = emmeans(model1, ~ type.respondent)
pairs(emmeans.model1.cat,
      adjust="tukey")


# VISUALISE PREDICTIONS ---------------------------------------------------


model1a.plot <- ggpredict(model1, "category")

model1a.fig <- ggplot() + 
  geom_point(data=median.score, aes(x=category, y=median.score), alpha = .1, shape = 21, colour = "black", fill = "#E7D4B5", stroke=.1, size = 5)+
  geom_point(data = model1a.plot, aes(x=x, y = predicted), colour = "#169873", size = 4) +
  geom_errorbar(data = model1a.plot, aes(x=x, y = predicted, ymin = conf.low, ymax = conf.high), colour = "#169873", width =.15, size = 2, alpha = .8)+
  theme_pubr(base_size = 14) +
  theme(axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 10, angle = 0, vjust = 0.3)) +
  labs(x ="Meat type", y="Palatability") +
  scale_y_continuous(labels = seq(2,10,2)) +
  scale_x_discrete(labels = c("Domestic", "Fish", "Invertebrate", 'Wild'))

model1a.fig


model1b.plot <- ggpredict(model1, "type.respondent")

model1b.fig <- ggplot() + 
  geom_point(data=median.score, aes(x=type.respondent, y=median.score), alpha = .1, shape = 21, colour = "black", fill = "#E7D4B5", stroke=.1, size = 5)+
  geom_point(data = model1b.plot %>% filter(x %in% c("Market", "Household", "Hunter")), aes(x=x, y = predicted), colour = "#169873", size = 4) +
  geom_errorbar(data = model1b.plot %>% filter(x %in% c("Market", "Household", "Hunter")), aes(x=x, y = predicted, ymin = conf.low, ymax = conf.high), colour = "#169873", width =.12, size = 2, alpha = .8)+
  theme_pubr(base_size = 14) +
  theme(axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 10, angle = 0, vjust = 0.3)) +
  labs(x ="Respondent category", y="Palatability") +
  scale_y_continuous(labels = seq(2,10,2)) +
  scale_x_discrete(labels = c("Household", "Hunter", "Vendor"))

model1b.fig

plot(ggpredict(model1, "type.respondent"))



####################################################
####################################################
####################################################
#######                                     ########
#######            CLASS MODEL              ########
#######                                     ########
####################################################
####################################################
####################################################

#NOTE: variation in palatability across taxonomic classes (restricted to wild meat)

palatability <- read.csv("01. Data/palatability.model.csv")

median.score.all <- palatability %>%
  group_by(animal, category) %>% 
  summarise(median.score = median(score)) %>% 
  mutate(category = factor(category),
         animal = factor(animal)) %>% 
  filter(category  == "Wild meat") %>%
  mutate(order = "Artiodactyla",
         class = "Mammal")


# CREATE MAMMALIAN ORDER --------------------------------------------------

median.score.all$order [median.score.all$animal == "African brush-tailed porcupine"] <- "Rodentia"
median.score.all$order [median.score.all$animal == "African dwarf crocodile"] <- "Crocodilia"
median.score.all$order [median.score.all$animal == "Slender-snouted crocodile"] <- "Crocodilia"
median.score.all$order [median.score.all$animal == "Central African rock python"] <- "Squamata"
median.score.all$order [median.score.all$animal == "Gaboon viper"] <- "Squamata"
median.score.all$order [median.score.all$animal == "Greater cane rat"] <- "Rodentia"
median.score.all$order [median.score.all$animal == "Hornbills"] <- "Bucerotiformes"
median.score.all$order [median.score.all$animal == "Mona monkey"] <- "Primates"
median.score.all$order [median.score.all$animal == "Patas monkey"] <- "Primates"
median.score.all$order [median.score.all$animal == "Preuss's red colobus"] <- "Primates"
median.score.all$order [median.score.all$animal == "Small birds"] <- "Aves"
median.score.all$order [median.score.all$animal == "Turtles"] <- "Testudines"
median.score.all$order [median.score.all$animal == "African elephant"] <- "Proboscidea"
median.score.all$order [median.score.all$animal == "Ball python"] <- "Squamata"
median.score.all$order [median.score.all$animal == "Black-bellied pangolin"] <- "Pholidota"
median.score.all$order [median.score.all$animal == "Bushfowls"] <- "Galliformes"
median.score.all$order [median.score.all$animal == "Chameleons"] <- "Squamata"
median.score.all$order [median.score.all$animal == "Crested genet"] <- "Carnivora"
median.score.all$order [median.score.all$animal == "Drill"] <- "Primates"
median.score.all$order [median.score.all$animal == "Galagos"] <- "Primates"
median.score.all$order [median.score.all$animal == "Hawks and kites"] <- "Accipitriformes"
median.score.all$order [median.score.all$animal == "Hyraxes"] <- "Hyracoidea"
median.score.all$order [median.score.all$animal == "Pigeons"] <- "Columbidae"
median.score.all$order [median.score.all$animal == "Putty-nosed monkey"] <- "Primates"
median.score.all$order [median.score.all$animal == "Sclater's monkey"] <- "Primates"
median.score.all$order [median.score.all$animal == "Squirrels"] <- "Rodentia"
median.score.all$order [median.score.all$animal == "Vultures"] <- "Aves"
median.score.all$order [median.score.all$animal == "African civet"] <- "Carnivora"
median.score.all$order [median.score.all$animal == "African golden cat"] <- "Carnivora"
median.score.all$order [median.score.all$animal == "Black guineafowl"] <- "Galliformes"
median.score.all$order [median.score.all$animal == "Calabar angwantibo"] <- "Primates"
median.score.all$order [median.score.all$animal == "Chimpanzee"] <- "Primates"
median.score.all$order [median.score.all$animal == "Crested mona monkey"] <- "Primates"
median.score.all$order [median.score.all$animal == "Eagle"] <- "Accipitriformes"
median.score.all$order [median.score.all$animal == "Helmeted guineafowl"] <- "Galliformes"
median.score.all$order [median.score.all$animal == "Leopard"] <- "Carnivora"
median.score.all$order [median.score.all$animal == "Nile crocodile"] <- "Crocodilia"
median.score.all$order [median.score.all$animal == "Owls"] <- "Strigiformes"
median.score.all$order [median.score.all$animal == "Milne-Edwards's potto"] <- "Primates"
median.score.all$order [median.score.all$animal == "Rabbits"] <- "Rodentia"
median.score.all$order [median.score.all$animal == "Shrews"] <- "Rodentia"
median.score.all$order [median.score.all$animal == "Tortoises"] <- "Testudines"
median.score.all$order [median.score.all$animal == "African clawless otter"] <- "Carnivora"
median.score.all$order [median.score.all$animal == "African palm civet"] <- "Carnivora"
median.score.all$order [median.score.all$animal == "Bats"] <- "Chiroptera"
median.score.all$order [median.score.all$animal == "Cameroon red-eared monkey"] <- "Primates"
median.score.all$order [median.score.all$animal == "Cobra"] <- "Squamata"
median.score.all$order [median.score.all$animal == "Cross River gorilla"] <- "Primates"
median.score.all$order [median.score.all$animal == "Frogs and toads"] <- "Anura"
median.score.all$order [median.score.all$animal == "Gambian pouched rat"] <- "Rodentia"
median.score.all$order [median.score.all$animal == "Honey badger"] <- "Carnivora"
median.score.all$order [median.score.all$animal == "Mambas"] <- "Squamata"
median.score.all$order [median.score.all$animal == "Nile monitor"] <- "Crocodilia"
median.score.all$order [median.score.all$animal == "Parrots"] <- "Psittaciformes"
median.score.all$order [median.score.all$animal == "Preuss's monkey"] <- "Primates"
median.score.all$order [median.score.all$animal == "Red-capped mangabey"] <- "Primates"
median.score.all$order [median.score.all$animal == "Drill"] <- "Primates"
median.score.all$order [median.score.all$animal == "Drill"] <- "Primates"
median.score.all$order [median.score.all$animal == "Turaco"] <- "Musophagidae"
median.score.all$order [median.score.all$animal == "White-bellied pangolin"] <- "Pholidota"
median.score.all$order [median.score.all$animal == "Giant pangolin"] <- "Pholidota"
median.score.all$class [median.score.all$order == "Crocodilia"] <- "Reptile"
median.score.all$class [median.score.all$order == "Squamata"] <- "Reptile"
median.score.all$class [median.score.all$order == "Galliformes"] <- "Bird"
median.score.all$class [median.score.all$order == "Accipitriformes"] <- "Bird"
median.score.all$class [median.score.all$order == "Bucerotiformes"] <- "Bird"
median.score.all$class [median.score.all$order == "Strigiformes"] <- "Bird"
median.score.all$class [median.score.all$order == "Psittaciformes"] <- "Bird"
median.score.all$class [median.score.all$order == "Testudines"] <- "Reptile"
median.score.all$class [median.score.all$order == "Aves"] <- "Bird"
median.score.all$class [median.score.all$order == "Anura"] <- "Reptile"
median.score.all$class [median.score.all$order == "Columbidae"] <- "Bird"


count.class <- median.score.all %>% 
  mutate(number = 1) %>% 
  group_by(class) %>% 
  summarise(count.class = sum(number)) %>% 
  ungroup()

count.order <- median.score.all %>% 
  mutate(number = 1) %>% 
  group_by(order) %>% 
  summarise(count.order = sum(number)) %>% 
  ungroup()


median.score.all <- median.score.all %>% 
  mutate(class = factor(class),
         order = factor(order)) %>% 
  left_join(count.class) %>% 
  left_join(count.order) %>% 
  arrange(class)


# VISUALISE DATA ----------------------------------------------------------


boxplot.class <- ggplot(median.score.all, aes(x = class, y = median.score)) +
  geom_boxplot(fill='grey', color="black", alpha = 0.5)+
  theme_pubr(base_size = 15) +
  theme(axis.text = element_text(size = 17),
        axis.text.x = element_text(size = 17),
        axis.title = element_text(size = 20)) +
  labs(x="Taxonomic class", y= "Palatability") +
  scale_y_continuous(breaks = seq(0,10,2)) 

boxplot.class


# FIT MODEL ---------------------------------------------------------------

model2 <- lm(median.score ~  class, data = median.score.all)


# MODEL DIAGNOSTICS -------------------------------------------------------

plot(model2$fitted.values, model2$residuals)
abline(h=0, lty=2)


check_model(model2)


summary(model2)


# POST-HOC TEST -----------------------------------------------------------

emmeans.model2 = emmeans(model2, ~ class)

pairs(emmeans.model2,
      adjust="tukey")


# VISUALISE PREDICTIONS ---------------------------------------------------


model2.plot <- ggpredict(model2, "class[Bird, Mammal, Reptile]")

model2.fig <- ggplot() + 
  geom_point(data=median.score.all, aes(x=class, y=median.score), alpha = .2, shape = 21, colour = "black", fill = "#E7D4B5", stroke=.1, size = 5)+
  geom_point(data = model2.plot %>% filter(x %in% c("Bird", "Reptile", "Mammal")), aes(x=x, y = predicted), colour = "#169873", size = 4) +
  geom_errorbar(data = model2.plot %>% filter(x %in% c("Bird", "Reptile", "Mammal")), aes(x=x, y = predicted, ymin = conf.low, ymax = conf.high), colour = "#169873", width =.12, size = 2, alpha = .8)+
  #geom_errorbar(data = model2.plot %>% filter(x == c( "Mammal")), aes(x=x, y = predicted, ymin = conf.low, ymax = conf.high), colour = "#169873", width =.12, size = 2, alpha = .8)+
  theme_pubr(base_size = 14) +
  theme(axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 10, angle = 0, vjust = 0.3)) +
  labs(x ="Taxonomic class", y="Palatability") +
  scale_y_continuous(labels = seq(2,10,2))

model2.fig


####################################################
####################################################
####################################################
#######                                     ########
#######            ORDER MODEL              ########
#######                                     ########
####################################################
####################################################
####################################################

#NOTE: variation in palatability across mammalian orders (again, wild meat only)

order <- median.score.all %>% 
  filter(order %in% c("Primates", "Pholidota", "Carnivora", "Rodentia", "Artiodactyla")) %>% 
  arrange(order)


# VISUALISE DATA ----------------------------------------------------------


boxplot.order <- ggplot(order, aes(x = order, y = median.score)) +
  geom_boxplot(fill='grey', color="black", alpha = 0.5)+
  theme_pubr(base_size = 15) +
  theme(axis.text.x = element_text(size = 14.5, vjust = 0.3),
        axis.text.y = element_text(size = 17),
        axis.title = element_text(size = 20))+
  labs(x="Mammalian order", y= "Palatability") +
  scale_y_continuous(breaks = seq(0,10,2)) +
  scale_x_discrete(labels = c("Ungulate", "Carnivore", "Pangolin", "Primate", "Rodent"))

boxplot.order



# FIT MODEL ---------------------------------------------------------------


model3 <- lm(median.score ~ order, data = order)


# MODEL DIAGNOSTICS -------------------------------------------------------

check_model(model3)

summary(model3)

# POST-HOC TEST -----------------------------------------------------------

emmeans.model3 = emmeans(model3, ~ order)
pairs(emmeans.model3,
      adjust="tukey")


# VISUALISE PREDICTIONS ---------------------------------------------------


model3.plot <- ggpredict(model3, "order")

model3.fig <- ggplot() + 
  geom_point(data=order, aes(x=order, y=median.score), alpha = .4, shape = 21, colour = "black", fill = "#E7D4B5", stroke=.1, size = 5)+
  #geom_point(data = model3.plot %>% filter(x %in% c("Pholidota")), aes(x=x, y = predicted), colour = "#169873", size = 4) +
  geom_point(data = model3.plot %>% filter(x %in% c("Primates", "Carnivora", "Rodentia", "Artiodactyla", "Pholidota")), aes(x=x, y = predicted), colour = "#169873", size = 4) +
  #geom_errorbar(data = model3.plot %>% filter(x %in% c("Pholidota")), aes(x=x, y = predicted, ymin = conf.low, ymax = conf.high), colour = "#169873", width =.17, size = 2, alpha = .8)+
  geom_errorbar(data = model3.plot %>% filter(x %in% c("Primates", "Carnivora", "Rodentia", "Artiodactyla", "Pholidota")), aes(x=x, y = predicted, ymin = conf.low, ymax = conf.high), colour = "#169873", width =.17, size = 2, alpha = .8)+
  theme_pubr(base_size = 14) +
  theme(axis.text.x = element_text(size = 9.5, angle = 0, vjust = 0.3),
        axis.text.y = element_text(size = 11)) +
  labs(x ="Mammalian order", y="Palatability") +
  scale_y_continuous(labels = seq(4,12,2)) +
  scale_x_discrete(labels = c("Ungulate", "Carnivore", "Pangolin", "Primate", "Rodent"))


model3.fig


####################################################
####################################################
####################################################
#######                                     ########
#######            CATEGORY MODEL II        ########
#######                                     ########
####################################################
####################################################
####################################################

median.score2 <- read.csv("01. Data/median.score.category.csv")

threatened <- median.score2 %>% 
  filter(category == "Wild meat") %>% 
  mutate(iucn = case_when(animal == "African brush-tailed porcupine" ~ "LC",
                          animal == "African buffalo" ~ "NT",
                          animal == "African civet" ~ "LC",
                          animal == "African dwarf crocodile" ~ "VU",
                          animal == "African elephant" ~ "CR",
                          animal == "African golden cat" ~ "VU",
                          animal == "African palm civet" ~ "LC",
                          animal == "Ball python" ~ "NT",
                          animal == "Bates' pygmy antelope" ~ "LC",
                          animal == "Bay duiker" ~ "NT",
                          animal == "Black guineafowl" ~ "LC",
                          animal == "Black-bellied pangolin" ~ "VU",
                          animal == "Blue duiker" ~ "LC",
                          animal == "Bushbuck" ~ "LC",
                          animal == "Calabar angwantibo" ~ "NT",
                          animal == "Cameroon red-eared monkey" ~ "VU",
                          animal == "Central African rock python" ~ "NT",
                          animal == "Chimpanzee" ~ "EN",
                          animal == "Crested genet" ~ "VU",
                          animal == "Cross River gorilla" ~ "CR",
                          animal == "Drill" ~ "EN",
                          animal == "Gaboon viper" ~ "VU",
                          animal == "Giant pangolin" ~ "EN",
                          animal == "Giant pouched rat" ~ "LC",
                          animal == "Greater cane rat" ~ "LC",
                          animal == "Helmeted guineafowl" ~ "LC",
                          animal == "Honey badger" ~ "LC",
                          animal == "Mona monkey" ~ "NT",
                          animal == "Nile crocodile" ~ "LC",
                          animal == "Nile monito" ~ "LC",
                          animal == "Ogilby's duiker" ~ "VU",
                          animal == "Patas monkey" ~ "NT",
                          animal == "Preuss's red colobus" ~ "EN",
                          animal == "Putty-nosed monkey" ~ "NT",
                          animal == "Red river hog" ~ "LC",
                          animal == "Red-capped mangabey" ~ "EN",
                          animal == "Sclater's monkey" ~ "EN",
                          animal == "Sitatunga" ~ "LC",
                          animal == "Slender-snouted crocodile" ~ "CR",
                          animal == "Water chevrotain" ~ "LC",
                          animal == "White-bellied pangolin" ~ "EN",
                          animal == "Yellow-backed duiker" ~ "NT")) %>% 
  drop_na() %>% 
  mutate(threat.class = ifelse(iucn == "EN" | iucn == "VU" | iucn == "CR", "threatened", "not threatened")) %>% 
  filter(threat.class == "threatened") %>% 
  select(-c(threat.class,iucn))

threatened.df <- median.score2 %>% 
  filter(category != "Wild meat") %>% 
  bind_rows(threatened)


# VISUALISE DATA ----------------------------------------------------------

boxplot.wm.threatened <- ggplot(threatened.df, aes(x = category, y = median.score)) +
  geom_boxplot(fill='grey', color="black", alpha = 0.5)+
  theme_pubr(base_size = 20) +
  theme(axis.text = element_text(size = 17),
        axis.text.x = element_text(size = 15),
        axis.title = element_text(size = 20)) +
  labs(x="Meat type", y= "Palatability") +
  scale_y_continuous(breaks = seq(0,10,2)) +
  scale_x_discrete(labels = c("Domestic", "Fish", "Invertebrate", 'Wild\n(threatened sp)'))

boxplot.wm.threatened



# FIT MODEL ---------------------------------------------------------------

model4 <- lm(median.score ~ category, data = threatened.df)


# MODEL DIAGNOSTICS -------------------------------------------------------

check_model(model4)


summary(model4)

# QUICK VISUALISATION -----------------------------------------------------

plot(ggpredict(model4, c("category")))


# VISUALISE PREDICTIONS ---------------------------------------------------


model4.plot <- ggpredict(model4, "category")

model4.fig <- ggplot() + 
  geom_point(data=median.score2, aes(x=category, y=median.score), alpha = .1, shape = 21, colour = "black", fill = "#E7D4B5", stroke=.1, size = 5)+
  geom_point(data = model4.plot, aes(x=x, y = predicted), colour = "#169873", size = 4) +
  geom_errorbar(data = model4.plot, aes(x=x, y = predicted, ymin = conf.low, ymax = conf.high), colour = "#169873", width =.15, size = 2, alpha = .8)+
  theme_pubr(base_size = 14) +
  theme(axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 9, angle = 0, vjust = 0.3)) +
  labs(x ="Meat type", y="Palatability") +
  scale_y_continuous(labels = seq(2,10,2)) +
  scale_x_discrete(labels = c("Domestic", "Fish", "Invertebrate", 'Wild\n      (threatened)'))

model4.fig



####################################################
####################################################
####################################################
#######                                     ########
#######             EXPORT FIGURES          ########
#######                                     ########
####################################################
####################################################
####################################################

# DATA DISTRIBUTIONS -----------------------------------------------------------


boxplots <- ggarrange(boxplot.respondent, boxplot.meat,
                      boxplot.wm.threatened, boxplot.class,
                      labels = "auto",
                      align = "h",
                      font.label = list(size = 25))

boxplots2 <- ggarrange(boxplots, boxplot.order,
          nrow = 2,
          ncol = 1,
          labels = c("", "e"),
          heights = c(1,0.5),
          font.label = list(size = 25))

ggsave("Supplementary Fig. 1.tiff",
       plot = boxplots2,
       path = "..//figure",
       scale = 1,
       width = 25,
       height = 25,
       units = c("cm"),
       dpi = 300)


# MODEL PREDICTIONS -------------------------------------------------------

model.plot <- ggarrange(model1b.fig, model1a.fig, model2.fig, model3.fig,
                        ncol = 2,
                        nrow = 2,
                        labels = "auto",
                        font.label = list(size = 18),
                        align = "hv")


ggsave("model.tiff",
       plot = model.plot,
       path = "..//figure",
       scale = 1,
       width = 19,
       height = 17,
       units = c("cm"),
       dpi = 300)


# PLOT MODEL DIAGNOSTICS --------------------------------------------------


tiff(filename = "model check.tiff", width = 30, height = 30, units = "cm", res = 600)

par(mfrow = c(2,2), cex = 2)
plot(model1$fitted.values, model1$residuals, xlab = "Fitted values", ylab = "Pearson residuals")
abline(h=0, lty=2)
plot(model4$fitted.values, model4$residuals, xlab = "Fitted values", ylab = "Pearson residuals")
abline(h=0, lty=2)
plot(model2$fitted.values, model2$residuals, xlab = "Fitted values", ylab = "Pearson residuals")
abline(h=0, lty=2)
plot(model3$fitted.values, model3$residuals, xlab = "Fitted values", ylab = "Pearson residuals")
abline(h=0, lty=2)
dev.off()

