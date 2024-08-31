
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


# READ DATA ---------------------------------------------------------------

protein <- read.csv("01. Data/palatability.csv")


# WRANGLE DATA ------------------------------------------------------------

protein <- protein %>% 
  subset(select =- c(X1..Introduce.yourself..say.your.name.and.where.you.have.come.from..2..Introduce.the.project..stating.that.it.is.a.stuent.project.and.that.you.are.mainly.interested.in.their.preferences.for.different.meat..including.beef.and.chicken..3..Hand.out.the.consent.form..Continue.if.they.agree.to.partake.and.thus.sign.the.form.,
                     Unknown.diurnal.raptors:Olive.colobus,
                     X_uuid:X_index,
                     Be.ready.to.ask.some.pangolin.questions..Try.to.make.a.link.to.pangolins.from.the.previous.section.,
                     Respondent.s.ID)) %>% 
  rename(village = Select.village,
         type.respondent = What.category.of.respondent.are.you.interviewing.,
         name.respondent = Name.of.the.respondent,
         name.interviewer = Name.of.the.interviewer,
         sex = What.is.the.respondent.s.sex.,
         ethnicity = What.is.the.respondent.s.ethnicity.,
         dob = What.is.the.year.of.birth.of.the.respondent.,
         wbp.2010to2015 = X2010.2015,
         wbp.2016to2020 = X2016.2020,
         wbp.2021todate = X2021.date,
         bbp.2010to2015 = X2010.2015.1,
         bbp.2016to2020 = X2016.2020.1,
         bbp.2021todate = X2021.date.1,
         scale.wbp.2010to2015 = X2010.2015.2,
         scale.wbp.2016to2020 = X2016.2020.2,
         scale.wbp.2021todate = X2021.date.2,
         scale.bbp.2010to2015 = X2010.2015.3,
         scale.bbp.2016to2020 = X2016.2020.3,
         scale.bbp.2021todate = X2021.date.3,
         discard = Discard..bin.or.burn.,
         medicine = Use.for.medicine,
         stockpile = Stockpile.for.future.sale,
         no.access = I.ve.never.had.access.to.pangolin.scales,
         note.usage = Notes.on.usage,
         cultural.value = Describe.any.sentimental.or.cultural.value.associated.with.pangolin.meat.or.scales,
         moonlight.hunting = How.does.moonlight.affect.your.hunting.patterns.,
         moonlight.animal = How.does.moonlight.affect.animal.behaviour.at.night.)

#Convert dob to age
age <- protein %>% 
  subset(select = "dob") %>% 
  mutate(now = 2022,
         age = now-dob)

#Identify enclave communities 
enclave <- c("Okwa II", "Okwa I", "Okwangwo", "Mkpot 1")

palatability <- protein %>% 
  pivot_longer(African.brush.tailed.porcupine:Yellow.backed.duiker, names_to = "animal", values_to = "score") %>% 
  relocate(animal:score,.after = dob) %>% 
  mutate(comm.type = case_when(village %in% enclave ~ "Enclave", TRUE ~ "Border"))%>% 
  subset(select =-c(wbp.2010to2015:moonlight.animal)) %>% 
  mutate(animal = str_replace_all(animal,'[\\.,]',' ')) %>% 
  mutate(category = "Wild meat") %>% 
  mutate(
    animal = recode(animal,
                    "African brush tailed porcupine" = "African brush-tailed porcupine",
                    "Aquatic snails  nkonkor " = "Aquatic snail",
                    "Bat" = "Bats",
                    "Beef  cow " = "Beef",
                    "Black bellied pangolin" = "Black-bellied pangolin",
                    "Crowned monkey" = "Crested mona monkey",
                    "Dried anchovies" = "Anchovy",
                    "Freshwater shrimp and prawn" = "Freshwater shrimps",
                    "Frog and toads" = "Frogs and toads",
                    "Genet  crested and Cape "  = "Crested genet",
                    "Giant ground pangolin" = "Giant pangolin",
                    "Spotted necked otter" = "Otters",
                    "African slender snouted crocodile" = "Slender-snouted crocodile",
                    "Blue duiker  frutambo " = "Blue duiker",
                    "Cusimanse" = "Cusimanses",
                    "Lobster" = "Lobsters",
                    "Preuss s red colobus  Colobus monkey " = "Preuss's red colobus",
                    "Shrew  okornor " = "Shrews",
                    "Hornbill" = "Hornbills",
                    "Hawk and Kite" = "Hawks and kites",
                    "Small bird" = "Small birds",
                    "Ratel  honey badger " = "Honey badger",
                    "Yellow backed duiker" = "Yellow-backed duiker",
                    "Sclater s monkey" = "Sclater's monkey", 
                    "Eagle" = "Eagle",
                    "Chameleon" = "Chameleons",
                    "Parrot" = "Parrots",
                    "Turtle" = "Turtles",
                    "Hyrax" = "Hyraxes",
                    "African civet  bush dog " = "African civet",
                    "Squirrel" = "Squirrels",
                    "Putty nosed monkey" = "Putty-nosed monkey",
                    "Bushfowl"= "Bushfowls",
                    "Bates  pygmy antelope" = "Bates' pygmy antelope",
                    "White bellied pangolin" = "White-bellied pangolin",
                    "Harnessed bushbuck"  = "Bushbuck",
                    "Grasshopper"  = "Grasshoppers",
                    "Owl" = "Owls",
                    "Vulture" = "Vultures",
                    "Pigeon" = "Pigeons",
                    "Turaco" = "Turacos",
                    "Mongoose" = "Mongooses",
                    "Mamba" = "Mambas",
                    "West African mackerel  ice fish " = "West African mackerel",
                    "Preuss s monkey" = "Preuss's monkey", 
                    "Ogilby s duiker" = "Ogilby's duiker",
                    "Galago  or bushbaby " = "Galagos",
                    "Milne-Edwards's potto" =  "Milne-Edwards's Potto",
                    "Cameroon red eared monkey"  =  "Cameroon red-eared monkey",
                    "African palm civet  civet " = "African palm civet",
                    "Tortoise" = "Tortoises",
                    "Red capped mangabey" = "Red-capped mangabey",
                    "Greater cane rat  cutting grass " = "Greater cane rat",
                    "Rabbit"  = "Rabbits",
                    "Nile Monitor" = "Nile monitor"))



palatability$category [palatability$animal == "Beef"] <- "Domestic meat"

palatability$category [palatability$animal == "Chicken egg"] <- "Domestic meat"
palatability$category [palatability$animal == "Anchovy"] <- "Fish"
palatability$category [palatability$animal == "Sheep"] <- "Domestic meat"
palatability$category [palatability$animal == "Stock fish"] <- "Fish"

palatability$category [palatability$animal == "West African mackerel"] <- "Fish"
palatability$category [palatability$animal == "Aquatic snail"] <- "Invertebrate"
palatability$category [palatability$animal == "Carp"] <- "Fish"
palatability$category [palatability$animal == "Cricket"] <- "Invertebrate"
palatability$category [palatability$animal == "Freshwater crab"] <- "Invertebrate"
palatability$category [palatability$animal == "Goat"] <- "Domestic meat"
palatability$category [palatability$animal == "Guinea pig"] <- "Domestic meat"
palatability$category [palatability$animal == "Pork"] <- "Domestic meat"
palatability$category [palatability$animal == "Sardine and pilchard"] <- "Fish"
palatability$category [palatability$animal == "Termite"] <- "Invertebrate"
palatability$category [palatability$animal == "African palm weevil"] <- "Invertebrate"
palatability$category [palatability$animal == "Catfish"] <- "Fish"
palatability$category [palatability$animal == "Chicken"] <- "Domestic meat"
palatability$category [palatability$animal == "Dog"] <- "Domestic meat"
palatability$category [palatability$animal == "Duck"] <- "Domestic meat"
palatability$category [palatability$animal == "Common periwinkle"] <- "Invertebrate"
palatability$category [palatability$animal == "Freshwater shrimps"] <- "Invertebrate"
palatability$category [palatability$animal == "Giant land snail"] <- "Invertebrate"
palatability$category [palatability$animal == "Grasshoppers"] <- "Invertebrate"
palatability$category [palatability$animal == "Lobsters"] <- "Invertebrate"
palatability$category [palatability$animal == "Tilapia"] <- "Fish"
palatability$category [palatability$animal == "Turkey"] <- "Domestic meat"
palatability$category [palatability$animal == "Rabbits"] <- "Domestic meat"


# COUNT CALCULATIONS ------------------------------------------------------

#Remove columns with '999', representing Not Applicable during data collection
palatability <- palatability %>% 
  filter(score !=999) %>% 
  filter(animal != "Leopard") %>%  
  filter(animal != "African clawless otter")

palatability$number <- rep(1, nrow(palatability)) #create new column with 1s

#Calculate count per species 
count <- palatability %>% 
  group_by(animal, category) %>% 
  summarise(count = sum(number)) %>% 
  ungroup() %>% 
  mutate(animal.color = ifelse(animal %in% c("Giant pangolin", "Black-bellied pangolin", "White-bellied pangolin"), "Pangolin", "Other"))


################################################################################
################################################################################
###############                                                #################
###############                   FIGURES / MEAN               #################
###############                                                #################
################################################################################
################################################################################


# Means -------------------------------------------------------------------


mean.palatability <- palatability %>% 
  group_by(animal, category) %>% 
  summarise(mean = mean(score),
            stdv = sd(score))


# All ---------------------------------------------------------------------


ggplot(mean.palatability, aes(animal, mean, fill = "#009987")) +
  geom_col() +
  facet_wrap(~category,scales = "free_y") +
  coord_flip() +
  geom_errorbar(aes(ymin=mean-stdv, ymax=mean+stdv), width=.2,
                position=position_dodge(.9))

# Domestic meat -----------------------------------------------------------

dm.mean <- ggplot(mean.palatability %>% filter(category == "Domestic meat"),   aes(x = reorder(animal, mean), y = mean)) +
  geom_col(fill='#009987') +
  geom_text(data=count%>% filter(category == "Domestic meat"), aes(x=animal, y=11.5, label=count), size=3.4)+
  geom_errorbar(aes(ymin=mean-stdv, ymax=mean+stdv), width=.2,
                position=position_dodge(.9)) +
  theme_pubr(base_size = 12) +
  theme(
    axis.title = element_text(size = 20),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    #axis.title.y = element_blank()
  ) +
  labs(y="", x= "Domestic meat", face="bold") +
  coord_flip() +
  scale_y_continuous(breaks =seq(1,10,1))

dm.mean

# Fish  -------------------------------------------------------------------


fish.mean <- ggplot(mean.palatability %>% filter(category == "Fish"),   aes(x = reorder(animal, mean), y = mean)) +
  geom_col(fill='#009987') +
  geom_text(data=count%>% filter(category == "Fish"), aes(x=animal, y=11.5, label=count), size=3.4)+
  geom_errorbar(aes(ymin=mean-stdv, ymax=mean+stdv), width=.2,
                position=position_dodge(.9)) +
  theme_pubr(base_size = 12) +
  theme(
    axis.title = element_text(size = 20),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    #axis.title.y = element_blank()
  ) +
  labs(y="", x= "Fish", face="bold") +
  coord_flip() +
  scale_y_continuous(breaks =seq(1,10,1))

fish.mean



# Invertebrates -----------------------------------------------------------

invert.mean <- ggplot(mean.palatability %>% filter(category == "Invertebrate"),   aes(x = reorder(animal, mean), y = mean)) +
  geom_col(fill='#009987') +
  geom_text(data=count%>% filter(category == "Invertebrate"), aes(x=animal, y=11.5, label=count), size=3.4)+
  geom_errorbar(aes(ymin=mean-stdv, ymax=mean+stdv), width=.3,
                position=position_dodge(.9)) +
  theme_pubr(base_size = 12) +
  theme(
    axis.title = element_text(size = 20),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    #axis.title.y = element_blank()
  ) +
  labs(y="", x= "Invertebrate", face="bold") +
  coord_flip() +
  scale_y_continuous(breaks =seq(1,10,1))

invert.mean


# Wild meat ---------------------------------------------------------------


mean.palatability <- mean.palatability %>%
  mutate(animal.color = ifelse(animal %in% c("Giant pangolin", "Black-bellied pangolin", "White-bellied pangolin"), "Pangolin", "Other")) %>% 
  mutate(animal.color = factor(animal.color, levels = c("Pangolin", "Other")))

# Define colors for each animal type
animal.colors <- c("Pangolin" = "#804674", "Other" = "#009987") 


wild.mean <- ggplot(mean.palatability %>% filter(category == "Wild meat"),  aes(x = reorder(animal, mean), y = mean, fill = animal.color)) +
  geom_col(width = 0.85) +
  geom_text(data=count%>% filter(category == "Wild meat"), aes(x=animal, y=11.5, label=count), size=3.6)+
  geom_errorbar(aes(ymin=mean-stdv, ymax=mean+stdv), width=.3,
                position=position_dodge(.9)) +
  theme_pubr(base_size = 12) +
  theme(
    legend.text = element_text(size = 15),
    axis.title = element_text(size = 20),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    #axis.title.y = element_blank()
  ) +
  labs(y="", x= "Wild meat", face="bold", fill = "") +
  coord_flip() +
  scale_y_continuous(breaks =seq(1,10,1)) +
  scale_fill_manual(values = animal.colors,
  )


wild.mean

# SAVE PLOT ---------------------------------------------------------------

three.mean <- ggarrange(dm.mean, fish.mean, invert.mean, 
                        nrow = 3,
                        labels = "auto",
                        align = "v",
                        heights = c(1,.8,1),
                        font.label = list(size = 25))


all.mean <- ggarrange(three.mean, wild.mean,
                      #ncol = 2,
                      labels = c(" ", "d"),
                      common.legend = TRUE,
                      widths = c(1.1,1.4),
                      font.label = list(size = 25))

all.mean <- annotate_figure(all.mean, bottom = text_grob("Palatability", color = "black", rot = 0, size = 20))


all.mean

ggsave("pala.mean.tiff",
       plot = all.mean,
       path = "..//Figure",
       width = 28,
       height = 37,
       units = c("cm"),
       dpi = 600)


################################################################################
################################################################################
###############                                                #################
###############                 FIGURES / MEDIAN               #################
###############                                                #################
################################################################################
################################################################################


# DOMESTIC MEAT -----------------------------------------------------------

dm <- ggplot(palatability %>% filter(category == "Domestic meat"), aes(x=reorder(animal,score), y=score)) +
  geom_boxplot(fill='#898121', color="black", size = .5, alpha = .8, outlier.size = 1, outlier.fill = "grey", outlier.alpha = .4) +
  geom_text(data=count%>% filter(category == "Domestic meat"), aes(x=animal, y=10.8, label=count), size=2.5)+
  theme_pubr(base_size = 12) +
  theme(
    axis.title = element_text(size = 18),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    #axis.title.y = element_blank()
  ) +
  labs(y="", x= "Domestic meat", face="bold") +
  coord_flip() +
  scale_y_continuous(limits = c(1,10.9),breaks = c(1,2,3,4,5,6,7,8,9,10)) 

dm

# FISH --------------------------------------------------------------------

fish <- ggplot(palatability %>% filter(category == "Fish"), aes(x=reorder(animal,score), y=score)) +
  geom_boxplot(fill='#898121', color="black", size = .5, alpha = .8, outlier.size = 1, outlier.fill = "grey", outlier.alpha = .4)+
  geom_text(data=count%>% filter(category == "Fish"), aes(x=animal, y=10.8, label=count), size=2.5)+
  theme_pubr(base_size = 12) +
  theme(
    axis.title = element_text(size = 18),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank()) +
  labs(y="", x= "Fish") +
  coord_flip() +
  scale_y_continuous(limits = c(1,10.9),breaks = c(1,2,3,4,5,6,7,8,9,10)) 


fish


# INVERTEBRATES -----------------------------------------------------------

invert <- ggplot(palatability %>% filter(category == "Invertebrate"), aes(x=reorder(animal,score), y=score)) +
  geom_boxplot(fill='#898121', color="black", size = .5, alpha = .8, outlier.size = 1, outlier.fill = "grey", outlier.alpha = .4)+
  geom_text(data=count%>% filter(category == "Invertebrate"), aes(x=animal, y=10.8, label=count), size=2.5)+
  theme_pubr(base_size = 12) +
  theme(
    axis.title = element_text(size = 18),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank()) +
  labs(y="", x= "Invertebrate") +
  coord_flip() +
  scale_y_continuous(limits = c(1,10.9),breaks = c(1,2,3,4,5,6,7,8,9,10)) 

invert

#EE7214
# WILD MEAT ---------------------------------------------------------------

wm <- ggplot(palatability %>% filter(category == "Wild meat"), aes(x=reorder(animal,score), y=score)) +
  geom_boxplot(fill='#898121', color="black", size = .5, alpha = .8, outlier.size = 1, outlier.fill = "grey", outlier.alpha = .4)+
  geom_text(data=count%>% filter(category == "Wild meat"), aes(x=animal, y=10.8, label=count), size=2.5)+
  #facet_grid(~type.respondent, scales = "free_x") +
  theme_pubr(base_size = 10) +
  theme(
    axis.title = element_text(size = 18),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank()) +
  labs(y="", x= "Wild meat") +
  coord_flip() +
  scale_y_continuous(limits = c(1,10.9),breaks = c(1,2,3,4,5,6,7,8,9,10)) +
  scale_fill_aaas()


wm

three <- ggarrange(dm, fish, invert, 
                   nrow = 3,
                   labels = " ",
                   align = "v",
                   heights = c(1,.8,1),
                   font.label = list(size = 20))


all <- ggarrange(three, wm,
                 #ncol = 2,
                 labels = c(" ", " "),
                 widths = c(1,1.4),
                 font.label = list(size = 20))

all <- annotate_figure(all, bottom = text_grob("Palatability", color = "black", rot = 0, size = 18))


all

ggsave("pala.median.tiff",
       plot = all,
       path = "..//Figure",
       width = 26,
       height = 41,
       units = c("cm"),
       dpi = 600)


# MEDIAN PALATABILITY -------------------------------------------------------

median.score <- palatability %>%
    group_by(animal, type.respondent, category) %>% 
    summarise(median.score = median(score)) %>% 
  filter(animal != "Chicken egg")

#Calculate count per species and category 
count2 <- palatability %>% 
  group_by(animal, category, type.respondent) %>% 
  summarise(count = sum(number)) %>% 
  filter(animal != "Chicken egg") %>% 
  ungroup() 

sup.material <- median.score %>% 
  left_join(count2, by = c("animal", "category", "type.respondent"))

#sup.material <- median.score %>% 
 # pivot_wider(names_from = type.respondent, values_from = median.score)

write.csv(sup.material, "supplementary material.csv", row.names = F)

median.category <- median.score %>% 
  group_by(category) %>% 
  summarise(median = median(median.score))
  
  
score.export <- palatability %>%
    group_by(animal, category) %>% 
    summarise(median.score = median(score)) %>% 
    filter(category != "Wild meat")
  

write.csv(score.export, "other category export.csv", row.names = F)
  
# CORRELATION PLOTS -------------------------------------------------------

score <- pivot_wider(median.score, names_from = type.respondent, values_from = median.score) 

wm.cor <- score %>%
  filter(category  == "Wild meat") %>%
  ungroup() %>% 
  mutate(number = row_number()) %>% 
  mutate(label = paste0(number,"-", animal)) %>% 
  mutate(label = reorder(label, gtools::mixedorder(label))) %>% 
  mutate(order = "Artiodactyla",
         class = "Mammal")


wm.cor$order [wm.cor$animal == "African brush-tailed porcupine"] <- "Rodentia"
wm.cor$order [wm.cor$animal == "African dwarf crocodile"] <- "Crocodilia"
wm.cor$order [wm.cor$animal == "Slender-snouted crocodile"] <- "Crocodilia"
wm.cor$order [wm.cor$animal == "Central African rock python"] <- "Squamata"
wm.cor$order [wm.cor$animal == "Gaboon viper"] <- "Squamata"
wm.cor$order [wm.cor$animal == "Greater cane rat"] <- "Rodentia"
wm.cor$order [wm.cor$animal == "Hornbills"] <- "Bucerotiformes"
wm.cor$order [wm.cor$animal == "Mona monkey"] <- "Primates"
wm.cor$order [wm.cor$animal == "Patas monkey"] <- "Primates"
wm.cor$order [wm.cor$animal == "Preuss's red colobus"] <- "Bucerotiformes"
wm.cor$order [wm.cor$animal == "Small birds"] <- "Aves"
wm.cor$order [wm.cor$animal == "Turtles"] <- "Testudines"
wm.cor$order [wm.cor$animal == "African elephant"] <- "Proboscidea"
wm.cor$order [wm.cor$animal == "Ball python"] <- "Squamata"
wm.cor$order [wm.cor$animal == "Black-bellied pangolin"] <- "Pholidota"
wm.cor$order [wm.cor$animal == "Bushfowls"] <- "Galliformes"
wm.cor$order [wm.cor$animal == "Chameleons"] <- "Squamata"
wm.cor$order [wm.cor$animal == "Crested genet"] <- "Carnivora"
wm.cor$order [wm.cor$animal == "Drill"] <- "Primates"
wm.cor$order [wm.cor$animal == "Galagos"] <- "Primates"
wm.cor$order [wm.cor$animal == "Hawks and kites"] <- "Accipitriformes"
wm.cor$order [wm.cor$animal == "Hyraxes"] <- "Hyracoidea"
wm.cor$order [wm.cor$animal == "Pigeons"] <- "Columbidae"
wm.cor$order [wm.cor$animal == "Putty-nosed monkey"] <- "Primates"
wm.cor$order [wm.cor$animal == "Sclater's monkey"] <- "Primates"
wm.cor$order [wm.cor$animal == "Squirrels"] <- "Rodentia"
wm.cor$order [wm.cor$animal == "Vultures"] <- "Aves"
wm.cor$order [wm.cor$animal == "African civet"] <- "Carnivora"
wm.cor$order [wm.cor$animal == "African golden cat"] <- "Carnivora"
wm.cor$order [wm.cor$animal == "Black guineafowl"] <- "Galliformes"
wm.cor$order [wm.cor$animal == "Calabar angwantibo"] <- "Primates"
wm.cor$order [wm.cor$animal == "Chimpanzee"] <- "Primates"
wm.cor$order [wm.cor$animal == "Crested mona monkey"] <- "Primates"
wm.cor$order [wm.cor$animal == "Eagle"] <- "Accipitriformes"
wm.cor$order [wm.cor$animal == "Helmeted guineafowl"] <- "Galliformes"
wm.cor$order [wm.cor$animal == "Leopard"] <- "Carnivora"
wm.cor$order [wm.cor$animal == "Nile crocodile"] <- "Crocodilia"
wm.cor$order [wm.cor$animal == "Owls"] <- "Strigiformes"
wm.cor$order [wm.cor$animal == "Milne-Edwards's potto"] <- "Primates"
wm.cor$order [wm.cor$animal == "Rabbits"] <- "Rodentia"
wm.cor$order [wm.cor$animal == "Shrews"] <- "Rodentia"
wm.cor$order [wm.cor$animal == "Tortoises"] <- "Testudines"
wm.cor$order [wm.cor$animal == "African clawless otter"] <- "Carnivora"
wm.cor$order [wm.cor$animal == "African palm civet"] <- "Carnivora"
wm.cor$order [wm.cor$animal == "Bats"] <- "Chiroptera"
wm.cor$order [wm.cor$animal == "Cameroon red-eared monkey"] <- "Primates"
wm.cor$order [wm.cor$animal == "Cobra"] <- "Squamata"
wm.cor$order [wm.cor$animal == "Cross River gorilla"] <- "Primates"
wm.cor$order [wm.cor$animal == "Frogs and toads"] <- "Anura"
wm.cor$order [wm.cor$animal == "Gambian pouched rat"] <- "Rodentia"
wm.cor$order [wm.cor$animal == "Honey badger"] <- "Carnivora"
wm.cor$order [wm.cor$animal == "Mambas"] <- "Squamata"
wm.cor$order [wm.cor$animal == "Nile monitor"] <- "Crocodilia"
wm.cor$order [wm.cor$animal == "Parrots"] <- "Psittaciformes"
wm.cor$order [wm.cor$animal == "Preuss's monkey"] <- "Primates"
wm.cor$order [wm.cor$animal == "Red-capped mangabey"] <- "Primates"
wm.cor$order [wm.cor$animal == "Drill"] <- "Primates"
wm.cor$order [wm.cor$animal == "Drill"] <- "Primates"
wm.cor$order [wm.cor$animal == "Turaco"] <- "Musophagidae"
wm.cor$order [wm.cor$animal == "White-bellied pangolin"] <- "Pholidota"
wm.cor$order [wm.cor$animal == "Giant pangolin"] <- "Pholidota"
wm.cor$class [wm.cor$order == "Crocodilia"] <- "Reptile"
wm.cor$class [wm.cor$order == "Squamata"] <- "Reptile"
wm.cor$class [wm.cor$order == "Galliformes"] <- "Bird"
wm.cor$class [wm.cor$order == "Accipitriformes"] <- "Bird"
wm.cor$class [wm.cor$order == "Bucerotiformes"] <- "Bird"
wm.cor$class [wm.cor$order == "Strigiformes"] <- "Bird"
wm.cor$class [wm.cor$order == "Psittaciformes"] <- "Bird"
wm.cor$class [wm.cor$order == "Testudines"] <- "Reptile"
wm.cor$class [wm.cor$order == "Aves"] <- "Bird"
wm.cor$class [wm.cor$order == "Anura"] <- "Reptile"
wm.cor$class [wm.cor$order == "Columbidae"] <- "Bird"
wm.cor$class [wm.cor$order == "Musophagidae"] <- "Bird"
  
plot(score$Household, score$Hunter)

library(ggrepel)

#score <- score %>% filter(animal != "Vultures")

#Hunter vs household
a <- ggplot() +
  geom_point(data = score, aes(x= Household, y = Hunter), fill = "#169873", alpha = .3, shape = 21, size = 4.5) +
  #geom_text_repel(data = mammal, aes(x= Household, y = Hunter,label = number), size = 4, direction = "both", max.overlaps = getOption("ggrepel.max.overlaps", default = 25))+
  #facet_grid(~class) +
  theme_pubr(base_size = 20) +
  theme(axis.title = element_text(size = 25)) +
  scale_x_continuous(labels = c(2,4,6,8,10)) +
  scale_y_continuous(labels = c(2,4,6,8,10))

#Household vs market
b <- ggplot() +
  geom_point(data = score, aes(x= Household, y = Market), fill = "#169873", alpha = .3, shape = 21, size = 4.5) +
  #geom_text_repel(data = mammal, aes(x= Household, y = Market,label = number), size = 4, direction = "both", max.overlaps = getOption("ggrepel.max.overlaps", default = 25))+
  theme_pubr(base_size = 20) +
  theme(axis.title = element_text(size = 25)) +
  #facet_grid(~class) +
  scale_x_continuous(labels = c(2,4,6,8,10)) +
  scale_y_continuous(labels = c(2,4,6,8,10)) +
  labs( y = "Vendor")

#Hunter vs market
c <- ggplot() +
  geom_point(data = score, aes(x= Hunter, y = Market), fill = "#169873", alpha = .3, shape = 21, size = 4.5) +
  #geom_text_repel(data = mammal, aes(x= Hunter, y = Market,label = number), size = 4, direction = "both", max.overlaps = getOption("ggrepel.max.overlaps", default = 25))+
  theme_pubr(base_size = 20) +
  #facet_grid(~class) +
  theme(axis.title = element_text(size = 25)) +
  theme(legend.text = element_text(size = 10))+
  scale_x_continuous(labels = c(2,4,6,8,10)) +
  scale_y_continuous(labels = c(2,4,6,8,10)) +
  labs( y = "Vendor")

cor.plot <- ggarrange(a,b,c,
                         nrow = 1,
                         labels = "auto",
                         font.label = list(size = 30))

library(corrplot)
corrplot(cor(score %>%  subset(select = c(Household:Market) )))




ggsave("correlation plot.tiff",
        plot = cor.plot,
        path = "..//figure",
        scale = 1,
        width = 30,
        height = 10,
        units = c("cm"),
        dpi = 300)

library(GGally) 
ggpairs(score %>% drop_na(), c(3,4,5))+
  theme_pubclean()

score <- score %>% 
  drop_na()

cor(score [, c(3,4,5)]) #Correlation coefficient 

cor.test(score$Household, score$Hunter)
cor.test(score$Household, score$Market)
cor.test(score$Hunter, score$Market)




# MODELS ------------------------------------------------------------------

#CATEGORY ====

count.cat <- median.score %>% 
  mutate(number = 1) %>% 
  group_by(category, type.respondent, ethnicity) %>% 
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


boxplot.respondent <- ggplot(median.score, aes(x = comm.type, y = median.score)) +
  geom_boxplot(fill='grey', color="black", alpha = 0.5)+
  theme_pubr(base_size = 15) +
  theme(axis.text = element_text(size = 17),
        axis.text.x = element_text(size = 17),
        axis.title = element_text(size = 20)) +
  labs(x="Respondent category", y= "Palatability") +
  scale_y_continuous(breaks = seq(0,10,2)) 


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


median.score1 <- median.score %>% 
  filter(type.respondent == "Market")

model <- lm(median.score ~  type.respondent + category + ethnicity, data = median.score)
model2 <- lm(median.score ~  type.respondent * category + ethnicity, data = median.score)

compare_performance(model, model2)
summary(model2)

plot(ggpredict(model, c("type.respondent", "category")))

#EMEANS----
emmeans.model1.meat = emmeans(model, ~ category)
pairs(emmeans.model1.meat,
      adjust="tukey")


emmeans.model1.cat = emmeans(model, ~ type.respondent)
pairs(emmeans.model1.cat,
      adjust="tukey")


#Diagnostics
plot(model$fitted.values, model$residuals)
abline(h=0, lty=2)

check_model(model)

modelaa.plot <- ggpredict(model, "category")

model1aa <- ggplot() + 
  geom_point(data = modelaa.plot, aes(x=x, y = predicted), colour = "#805D93", size = 3) +
  geom_errorbar(data = modelaa.plot, aes(x=x, y = predicted, ymin = conf.low, ymax = conf.high), colour = "#805D93", width =.15, size = 2, alpha = .8)+
  geom_point(data=median.score, aes(x=category, y=median.score), alpha = .1, shape = 21, colour = "black", fill = "#169873", stroke=.1, size = 5)+
  theme_pubr(base_size=20.2) +
  theme(axis.text.y = element_text(size = 17),
        axis.text.x = element_text(size = 17, angle = 0, vjust = 0.3)) +
  labs(x ="Meat type", y="Palatability") +
  scale_y_continuous(labels = seq(2,10,2)) +
  scale_x_discrete(labels= c('Wild', 'Invertebrate', 'Fish', 'Domestic'))

model1aa


modela.plot <- ggpredict(model, "type.respondent")

model1a <- ggplot() + 
  geom_point(data = modela.plot %>% filter(x %in% c("Market", "Household")), aes(x=x, y = predicted), colour = "#805D93", size = 3) +
  geom_point(data = modela.plot %>% filter(x == c( "Hunter")), aes(x=x, y = predicted), colour = "#805D93", size = 3) +
  geom_errorbar(data = modela.plot %>% filter(x %in% c("Market", "Household")), aes(x=x, y = predicted, ymin = conf.low, ymax = conf.high), colour = "#805D93", width =.12, size = 2, alpha = .8)+
  geom_errorbar(data = modela.plot %>% filter(x == c( "Hunter")), aes(x=x, y = predicted, ymin = conf.low, ymax = conf.high), colour = "#805D93", width =.12, size = 2, alpha = .8)+
  geom_point(data=median.score, aes(x=type.respondent, y=median.score), alpha = .1, shape = 21, colour = "black", fill = "#169873", stroke=.1, size = 5)+
  theme_pubr(base_size=20.2) +
  theme(axis.text.y = element_text(size = 17),
        axis.text.x = element_text(size = 17, angle = 0, vjust = 0.3)) +
  labs(x ="Respondent category", y="Palatability") +
  scale_y_continuous(labels = seq(2,10,2))

model1a


#CLASS ====

median.score.all <- palatability %>%
  group_by(animal, category) %>% 
  summarise(median.score = median(score)) %>% 
  mutate(category = factor(category),
         animal = factor(animal)) %>% 
  filter(category  == "Wild meat") %>%
  mutate(order = "Artiodactyla",
         class = "Mammal")


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


#write.csv(median.score.all, "wildmeat export.csv", row.names = F)

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


boxplot.class <- ggplot(median.score.all, aes(x = class, y = median.score)) +
  geom_boxplot(fill='grey', color="black", alpha = 0.5)+
  theme_pubr(base_size = 15) +
  theme(axis.text = element_text(size = 17),
        axis.text.x = element_text(size = 17),
        axis.title = element_text(size = 20)) +
  labs(x="Taxonomic class", y= "Palatability") +
  scale_y_continuous(breaks = seq(0,10,2)) 

boxplot.class

#MODEL ====
model2 <- lm(median.score ~  class, data = median.score.all)
summary(model2)


#EMEANS----
emmeans.model2 = emmeans(model2, ~ class)

pairs(emmeans.model2,
      adjust="tukey")


#Model diagnostics ----
plot(model2$fitted.values, model2$residuals)
abline(h=0, lty=2)

check_model(model2)

modelb.plot <- ggpredict(model2, "class[Bird, Mammal, Reptile]")

model1b <- ggplot() + 
  geom_point(data = modelb.plot %>% filter(x %in% c("Bird", "Reptile")), aes(x=x, y = predicted), colour = "#805D93", size = 3) +
  geom_point(data = modelb.plot %>% filter(x == c( "Mammal")), aes(x=x, y = predicted), colour = "#805D93", size = 3) +
  geom_errorbar(data = modelb.plot %>% filter(x %in% c("Bird", "Reptile")), aes(x=x, y = predicted, ymin = conf.low, ymax = conf.high), colour = "#805D93", width =.12, size = 2, alpha = .8)+
  geom_errorbar(data = modelb.plot %>% filter(x == c( "Mammal")), aes(x=x, y = predicted, ymin = conf.low, ymax = conf.high), colour = "#805D93", width =.12, size = 2, alpha = .8)+
  geom_point(data=median.score.all, aes(x=class, y=median.score), alpha = .1, shape = 21, colour = "black", fill = "#169873", stroke=.1, size = 5)+
  theme_pubr(base_size=21) +
  theme(axis.text.y = element_text(size = 17),
        axis.text.x = element_text(size = 17, angle = 0, vjust = 0.3)) +
  labs(x ="Taxonomic class", y="Palatability") +
  scale_y_continuous(labels = seq(2,10,2))

model1b


#ORDER ====

order <- median.score.all %>% 
  filter(order %in% c("Primates", "Pholidota", "Carnivora", "Rodentia", "Artiodactyla")) %>% 
  arrange(order)


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


#Save boxplot ====

boxplots <- ggarrange(boxplot.respondent, boxplot.meat,
                      boxplot.class, boxplot.order,
                      labels = "auto",
                      font.label = list(size = 25))

#boxplots <- annotate_figure(boxplots,
#                              left = text_grob("Palatability", color = "black", rot = 90, size = 22, hjust = .1))



ggsave("Supplementary Fig. 1.tiff",
       plot = boxplots,
       path = "..//figure",
       scale = 1,
       width = 30,
       height = 20,
       units = c("cm"),
       dpi = 300)



#MODEL ====

model3 <- lm(median.score ~ order, data = order)
summary(model3)

emmeans.model3 = emmeans(model3, ~ order)
pairs(emmeans.model3,
      adjust="tukey")


#Model diagnostics ----
plot(model3$fitted.values, model3$residuals)
abline(h=0, lty=2)

check_model(model3)

modelc.plot <- ggpredict(model3, "order")

model1c <- ggplot() + 
  geom_point(data = modelc.plot %>% filter(x %in% c("Pholidota")), aes(x=x, y = predicted), colour = "#805D93", size = 3) +
  geom_point(data = modelc.plot %>% filter(x %in% c("Primates", "Carnivora", "Rodentia", "Artiodactyla")), aes(x=x, y = predicted), colour = "#805D93", size = 3) +
  geom_errorbar(data = modelc.plot %>% filter(x %in% c("Pholidota")), aes(x=x, y = predicted, ymin = conf.low, ymax = conf.high), colour = "#805D93", width =.17, size = 2, alpha = .8)+
  geom_errorbar(data = modelc.plot %>% filter(x %in% c("Primates", "Carnivora", "Rodentia", "Artiodactyla")), aes(x=x, y = predicted, ymin = conf.low, ymax = conf.high), colour = "#805D93", width =.17, size = 2, alpha = .8)+
  geom_point(data=order, aes(x=order, y=median.score), alpha = .2, shape = 21, colour = "black", fill = "#169873", stroke=.1, size = 5)+
  theme_pubr(base_size=21) +
  theme(axis.text.x = element_text(size = 13, angle = 0, vjust = 0.3),
        axis.text.y = element_text(size = 17)) +
  labs(x ="Mammalian order", y="Palatability") +
  scale_y_continuous(labels = seq(4,12,2)) +
  scale_x_discrete(labels = c("Ungulate", "Carnivore", "Pangolin", "Primate", "Rodent"))


model1c

model.plot <- ggarrange(model1a, model1aa, model1b, model1c,
                        ncol = 2,
                        nrow = 2,
                        labels = "auto",
                        font.label = list(size = 28),
                        align = "hv")

model.plot <- annotate_figure(model.plot,
                              left = text_grob("Palatability", color = "black", rot = 90, size = 22, hjust = .1))

model.plot

ggsave("model.tiff",
       plot = model1c,
       path = "..//figure",
       scale = 1,
       width = 20,
       height = 10,
       units = c("cm"),
       dpi = 300)



#PLOT MODEL DIAGNOSTICS =====

tiff(filename = "model check.tiff", width = 30, height = 30, units = "cm", res = 600)

par(mfrow = c(2,2), cex = 2)
plot(model$fitted.values, model$residuals, xlab = "Fitted values", ylab = "Pearson residuals")
abline(h=0, lty=2)
plot(model2$fitted.values, model2$residuals, xlab = "Fitted values", ylab = "Pearson residuals")
abline(h=0, lty=2)
plot(model3$fitted.values, model3$residuals, xlab = "Fitted values", ylab = "Pearson residuals")
abline(h=0, lty=2)

dev.off()



# PANGOLIN VS MOTHER PROTEIN TYPES ----------------------------------------

model5a <- median.score %>% 
  filter(category != "Wild meat") %>% 
  filter(type.respondent != "Household") %>% 
  group_by(animal, category) %>% 
  summarise(median.score = median(median.score)) 

model5b <- order %>% 
  filter(order == "Pholidota") %>% 
  subset(select = c(animal, category, median.score))

model5 <- rbind(model5a, model5b)


model5 <- lm(median.score ~ category, data = model5)
summary(model5)

emmeans.model5 = emmeans(model5, ~ category)
pairs(emmeans.model5,
      adjust="tukey")


plot(ggpredict(model5, "category")) +
  theme_pubr()


#PRICE PER KILO MODEL ====

ppkg <- read.csv("ppkg.csv")

ppkg <- ppkg %>% 
  rename(animal = species)


ppkg$animal <- recode(ppkg$animal,
                       "Harnessed bushbuck" = "Bushbuck",
                      "Cusimanse" = "Cusimanses",
                      "Unidentified eagl" = "Eagle",
                      "Freshwater shrimp and prawn" = "Freshwater shrimps",
                      "Unidentified galago" = "Galagos",
                      "Hornbill" = "Hornbills",
                      "Pel's fishing-owl" = "Owls",
                      "Preuss's red colobus" = "Preuss's red colobus monkey",
                      "Forest giant squirre" = "Squirrels",
                      "Home's hinge-back tortoise" = "Tortoises",
                      "Unidentified eagl" = "Eagle")



python <- data.frame(animal = "Ball python", ppkg = 1094)
b.guineafowl <- data.frame(animal = "Black guineafowl", ppkg = 2857)
bushfowls <- data.frame(animal = "Bushfowls", ppkg = 2857)
crested.mona <- data.frame(animal = "Crested mona monkey", ppkg = 1838)
hyraxes <- data.frame(animal = "Hyraxes", ppkg = 1104)
lobsters <- data.frame(animal = "Lobsters", ppkg = 553)
mongooses <- data.frame(animal = "Mongooses", ppkg = 2041)
otter <- data.frame(animal = "Otters", ppkg = 2593)
#potto <- data.frame(animal = "Pottos", ppkg = 2486)

ppkg<- rbind(ppkg, python, b.guineafowl, bushfowls, crested.mona, hyraxes, lobsters, mongooses, otter)



score.ppkg <- palatability %>%
  group_by(animal, category) %>% 
  summarise(median.score = median(score)) %>% 
  left_join(ppkg) %>% 
  ungroup() %>% 
drop_na() %>% 
  filter(category == "Wild meat")



model4 <- lm(ppkg ~ scale(median.score), data = score.ppkg)

summary(model4)

check_model(model4)

plot(ggpredict(model4, "median.score"))





# end of modelling --------------------------------------------------------



median.score$animal = dplyr::recode(median.score$animal, "African brush tailed porcupine" = "African brush-tailed porcupine",
                              "African civet  bush dog" = "African civet",
                              "African palm civet  civet" = "African palm civet",
                              "Aquatic snails  nkonkor " = "Aquatic snail",
                              "Bates  pygmy antelope" = "Bates's pygmy antelope",
                              "Beef  cow " = "Beef",
                              "Black bellied pangolin" = "Black-bellied pangolin",
                              "Blue duiker  frutambo " = "Blue duiker",
                              "Cameroon red eared monkey" = "Cameroon red-eared monkey",
                              "Chicken egg" = "Egg (chicken)",
                              "Galago  or bushbaby" = "Galago",
                              "Cuisimanse" = "Flat-headed cusimanse",
                              "West African mackerel  ice fish " = " West African mackerel",
                              "White bellied pangolin" = "White-bellied pangolin",
                              "Yellow backed duiker" = "Yellow-backed duiker",
                              "Spotted necked otter" = "Spotted-necked otter",
                              "Ratel  honey badger" = "Ratel",
                              "Sclater s monkey" = "Sclater's monkey",
                              "Shrew okornor " = "Shrew",
                              "Red capped mangabey" = "Red-capped mangabey",
                              "Putty nosed monkey" = "Putty-nosed monkey",
                              "Preuss s red colobus  Colobus monkey" = "Preuss's red colobus",
                              "Ogilby s duiker" = "Ogilby's duiker",
                              "Greater cane rat  cutting grass " = "Greater cane rat",
                              "Genet  crested and Cape" = "Crested genet",
                              "Squirrel" = "Forest giant squirrel",
                              "Turaco" =  "Great blue turaco",
                              "Tortoise" = "Home's hinge-back tortoise",
                              "Giant pouched rat" = "Gambian pouched rat",
                              "Nile crocodile" = "Nile crocodile")
    



ggplot(median.score, aes(animal, median.score)) +
  geom_bar(position="dodge", stat = "identity", width =.80) +
  coord_flip() +
  theme_bw() +
  theme_bw(base_size = 15)

write.csv(median.score, "../../../Consumption/Analysis/wrangling/main/palatability.score.csv", row.names=FALSE)
#write.csv(median.score, "../../../Offtake/General/Script/segments/wrangling and exploratory/palatability.score.csv", row.names=FALSE)



density <- ggplot(palatability, aes(score)) +
  geom_density(alpha = 0.6, fill="lightblue") +
  facet_wrap(~animal) +
  theme_pubr(base_size = 9)+
  labs(y = "Distribution", x = "Score")


ggsave("density.tiff",
       plot = density,
       height = 30,
       width = 42,
       path = "..//figure",
       dpi = 600,
       units = "cm")



# VISUALISATION (EXTENDED) ------------------------------------------------

library(ggdist)
library(gghalves)
#Subset the data

three <- palatability %>% 
  filter(animal %in% c("Goat","Turkey","African brush tailed porcupine","Black bellied pangolin", 
                       "Giant ground pangolin", "White bellied pangolin", "Catfish", "Bat"))

#pal.dist <- 
ggplot(three, aes(x=animal, y=score, fill=animal)) +
  ggdist::stat_halfeye(
    adjust =.5,
    width =.6,
    justification = -.3,
    .width = 0,
    point_colour = NA) +
  geom_boxplot(
    width = .2,
    outlier.colour = NA) +
  gghalves::geom_half_point(
    aes(fill=animal),
    side = "l",
    range_scale = .4,
    alpha =.3) +
  theme_bw(base_size = 20) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()) +
  labs(y="Palatability score", x=" ") +
  scale_y_continuous(limits = c(1,10),breaks = c(2,4,6,8,10)) +
  scale_fill_brewer(palette="Pastel1") +
  coord_flip()


library(ggbeeswarm)
quota_halves <- 
ggplot(three, aes(x=animal, y=score, fill=animal)) +
  geom_half_point(aes(color = animal), 
                  transformation = position_quasirandom(width = 0.1),
                  side = "l", size = 0.5, alpha = 0.5) +
  geom_half_boxplot(aes(fill = animal), side = "r") + 
  scale_fill_viridis_d(option = "plasma", end = 0.8) +
  scale_color_viridis_d(option = "plasma", end = 0.8) +
  guides(color = "none", fill = "none") +
  labs(x = "Quota", y = "Proportion of women in parliament") #+
theme_clean()

quota_densities <- 
  ggplot(three, aes(x=animal, y=score, fill=animal)) +
  geom_density(alpha = 0.6) #+
  scale_fill_viridis_d(option = "plasma", end = 0.8) +
  labs(x = "Proportion of women in parliament", y = "Density", fill = "Quota") +
  #theme_clean() +
  theme(legend.position = "bottom")

quota_halves | quota_densities


ggsave("pal.dist.png",
       plot = pal.dist,
       height = 30,
       width = 20,
       path = "..//figure",
       dpi = 300,
       units = "cm")



#https://r-graph-gallery.com/23-add-colors-to-specific-groups-of-a-boxplot.html

################## end ###################################





############################################################
#Extract scores for price model
############################################################


# PANGOLIN SCALE USES -----------------------------------------------------

pangolin <- protein %>% 
  select(village:dob, wbp.2010to2015:cultural.value, X_id)


pang.uses <- pangolin %>% 
  select(name.respondent, type.respondent, discard:no.access) %>%
  filter(type.respondent != "Household") %>% 
  filter(rowSums(select(.,discard:no.access)) >0) %>% 
  pivot_longer(cols = c(discard:no.access), names_to = "uses", values_to = "count") %>% #Pivot longer
  group_by(type.respondent, uses) %>% 
  summarise(median = IQR(count)) %>% 
  filter(type.respondent != "Household") %>% 
  mutate(prop = median/sum(median)) 

pang.uses$uses <- dplyr::recode(pang.uses$uses, 
                                "discard" = "Discard",
                                "medicine" = "Medicinal use",
                                "stockpile" = "Stockpile",
                                "no.access" = "No access")


#pang.uses.plot <- ggplot(pang.uses, aes(x = type.respondent, y = prop, fill = uses)) +
#  geom_col(position = "fill") +
#  labs(y = "Proportion", x = "Respondent category", fill = "Uses") +
#facet_grid(~type.respondent) +
#  theme_pubr(base_size = 25) +
#  scale_fill_npg() +
#  theme(legend.position = "top")


#pang.uses.plot

#ggsave("pang.uses.plot.tiff",
#       plot = pang.uses.plot,
#       path = "..//figure",
#       width = 30,
#       height = 20,
#       units = c("cm"),
#       dpi = 300)


# PANGOLIN PRICE ----------------------------------------------------------

pang.price <- pangolin %>% 
  #mutate(name.respondent = fct_relevel(name.respondent)) %>% 
  select(type.respondent, wbp.2010to2015:scale.bbp.2021todate, name.respondent) %>% 
  filter(rowSums(select(., wbp.2010to2015:scale.bbp.2021todate)) >0) %>% 
  pivot_longer(cols = c(wbp.2010to2015:scale.bbp.2021todate), names_to = "part", values_to = "price") %>% 
  filter(price >= 20 & price <= 10000) %>% 
  mutate(animal.part = if_else(str_detect(part, "scale"), "Scale", "Meat")) %>% 
  mutate(species = if_else(str_detect(part, "wbp"), "White-bellied", "Black-bellied")) %>% 
  mutate(timeline = case_when(str_detect(part, "2010to2015") ~ "2010-2015", 
                              str_detect(part, "2016to2020") ~ "2016-2020", 
                              str_detect(part, "2021todate") ~ "2021-2022")) %>% 
  ungroup()


unique.respondents <- unique(pang.price$name.respondent)     



pang.price$type.respondent <- as.factor(pang.price$type.respondent)
pang.price$animal.part <- as.factor(pang.price$animal.part)
pang.price$timeline <- as.factor(pang.price$timeline)
pang.price$species <- as.factor(pang.price$species)
pang.price$name.respondent <- factor(pang.price$name.respondent, levels = unique.respondents)

pang.price <- pang.price %>% 
  group_by(type.respondent, animal.part, timeline, species) %>% 
  summarise(price = median(price)) %>% 
  filter(type.respondent != "Household")

lm.price <- lm(price ~ type.respondent + animal.part * timeline + species, data = pang.price)

summary(lm.price)

tiff(filename = "model diagnostics.tiff", width = 20, height = 20, units = "cm", res = 300)
check_model(lm.price)
dev.off()


plot(lm.price$fitted.values, 
     lm.price$residual)


a <- plot(ggpredict(lm.price, c("timeline", "animal.part"))) +
  theme_pubr(base_size = 10) +
  labs (x = "Timeline", y = "Pirce (NGN)", col = " ", title = "") 


b <- plot(ggpredict(lm.price, "species")) +
  theme_pubr(base_size = 15) +
  labs (x = "Timeline", y = "Pirce (NGN)", title = "") 


ab <- ggarrange (a,b,
                 labels = "auto",
                 font.label = list(size = 20))


ggsave("rough model.tiff",
       plot = ab,
       path = "..//figure",
       scale = 1,
       width = 25,
       height = 10,
       units = c("cm"),
       dpi = 600)

ggsave("rough model.tiff",
       plot = a,
       path = "..//figure",
       scale = 1,
       width = 10,
       height = 7,
       units = c("cm"),
       dpi = 600)

#pang.price$number <- rep(1, nrow(pang.price)) #create new column with 1s


#Calculate count per species and category
#count.scale <- pang.price %>% 
#  filter(animal.part =="Scale") %>% 
#  group_by(species,type.respondent, part, timeline, animal.part) %>% 
#  summarise(count = sum(number)) %>% 
#  ungroup()

#count.meat <- pang.price %>% 
#filter(animal.part =="Meat") %>% 
#  group_by(species, part, timeline, animal.part) %>% 
#  summarise(count = sum(number)) %>% 
#  ungroup()

#pang.price.plot <- ggplot(pang.price, aes(timeline, price)) +
#  geom_boxplot(aes(colour = animal.part), size = 1) +
#geom_text(data=count.scale, aes(x=timeline, y= 0, label=count), hjust = 2, size=4)+
#geom_text(data=count.meat, aes(x=timeline, y= 0, label=count), hjust = -.5, size=4)+
#  theme_pubr(base_size = 30) +
#  theme(panel.grid = element_blank(),
#        strip.background = element_rect(fil = "white"),
#        axis.text.x = element_text(size = 25),
#        legend.position = "top") +
#facet_wrap(~ species, scales = "free_x") +
# scale_color_aaas() +
#  labs(colour = " ", x = "Timeline", y = "Price in Naira (adult individual)") #+
#scale_x_discrete(labels = c("2010\n-2015", "2016\n-2020", "2021-\n2022"))


#pang.price.plot


#uses.price <- ggarrange(pang.uses.plot, pang.price.plot,
#          labels = "AUTO",
#          widths = c(1.2,1.5),
#          font.label = list(size = 25))

#uses.price

#ggsave("uses.price.plot.png",
#       plot = uses.price,
#       path = "..//figure",
#       width = 45,
#       height = 20,
#       units = c("cm"),
#       dpi = 300)

#ggsave("pang.price.plot.tiff",
#       plot = pang.price.plot,
#       path = "..//figure",
#       width = 30,
#       height = 25,
#       units = c("cm"),
#       dpi = 300)

#xx <- respondent.number %>% 
#  group_by(Var1) %>% 
#  summarise(count = sum(Freq))


#write.csv(respondent.number, "respondent number.csv", row.names = F)



#moon <- protein %>% 
#  filter(type.respondent == "Hunter") %>% 
#  select(moonlight.hunting:moonlight.animal)

#write.csv(moon, "moon.csv")

meat <- read.csv("meat.csv")
meat <- meat %>% 
  dplyr::rename(protein = this_meat_label,
                score = What.is...this_meat_label..s.palatabilty.score.,) %>% 
  mutate(Species.ID = protein, .after = protein) %>% 
  subset(select = -c(this_meat_name, index, parent_index, ID, protein))
 

meat$Species.ID [meat$Species.ID == "African civet (bush dog)"] <- "African civet"
meat$Species.ID [meat$Species.ID == "African palm civet (civet)"] <- "African palm civet"
meat$Species.ID [meat$Species.ID == "Blue duiker (frutambo)"] <- "Blue duiker"
meat$Species.ID [meat$Species.ID == "Genet (crested and Cape)"] <- "Crested genet"
meat$Species.ID [meat$Species.ID == "Cusimanse"] <- "Flat-headed cusimanse"
meat$Species.ID [meat$Species.ID == "Squirrel"] <- "Forest giant squirrel"
meat$Species.ID [meat$Species.ID == "Turaco"] <- "Great blue turaco"
meat$Species.ID [meat$Species.ID == "Greater cane rat (cutting grass)"] <- "Greater cane rat"
meat$Species.ID [meat$Species.ID == "Tortoise"] <- "Home's hinge-back tortoise"
meat$Species.ID [meat$Species.ID == "Potto"] <- "Milne-Edwards's Potto"
meat$Species.ID [meat$Species.ID == "Nile Monitor"] <- "Nile monitor"
meat$Species.ID [meat$Species.ID == "Giant pouched rat"] <- "Gambian pouched rat"
meat$Species.ID [meat$Species.ID == "Red river hog"] <- "Red River hog"
meat$Species.ID [meat$Species.ID == "Beef (cow)"] <- "Rock hyrax"                #Change to hyrax later
meat$Species.ID [meat$Species.ID == "Bat"] <- "Western tree hyrax"        #Change to western tree hyrax later
meat$Species.ID [meat$Species.ID == "African dwarf crocodile"] <- "West African dwarf crocodile"

 
meat <- meat %>% 
  group_by(Species.ID) %>% 
  summarise(score = median(score)) %>% 
  ungroup()

meat <- meat %>% 
  filter(Species.ID %in% c("African brush-tailed porcupine",
"African civet",
"African golden cat",
"African palm civet",
"Bay duiker",
"Black-bellied pangolin",
"Blue duiker",
"Calabar angwantibo",
"Cameroon red-eared monkey",
"Central African rock python",
"Crested genet",
"Drill",
"Flat-headed cusimanse",
"Forest giant squirrel",
"Gaboon viper",
"Great blue turaco",
"Greater cane rat",
"Harnessed bushbuck",            
"Helmeted guineafowl",
"Home's hinge-back tortoise",
"Milne-Edwards's Potto",
"Mona monkey",
"Nile monitor",
"Ogilby's duiker",
"Gambian pouched rat",
"Putty-nosed monkey",
"Red River hog",
"Rock hyrax",
"Sitatunga",
"West African dwarf crocodile",
"Western tree hyrax",
"White-bellied pangolin",
"Yellow-backed duiker"))


write.csv(meat, "p.score.csv")
