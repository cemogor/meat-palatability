
# LOAD REVELANT PACKAGES --------------------------------------------------

rm(list=ls())
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(ggsci)
library(gghalves)
library(ggpubr)
library(corrplot)
library(GGally)
library(ggrepel)

# READ DATA ---------------------------------------------------------------

protein <- read.csv("01. Data/palatability.csv")

####################################################
####################################################
####################################################
#######                                     ########
#######           DATA CLEANING             ########
#######                                     ########
####################################################
####################################################
####################################################

# WRANGLE DATA ------------------------------------------------------------

protein <- protein %>% 
  subset(select =- c(X1..Introduce.yourself..say.your.name.and.where.you.have.come.from..2..Introduce.the.project..stating.that.it.is.a.stuent.project.and.that.you.are.mainly.interested.in.their.preferences.for.different.meat..including.beef.and.chicken..3..Hand.out.the.consent.form..Continue.if.they.agree.to.partake.and.thus.sign.the.form.,
                     X_uuid:X_index)) %>% 
  rename(village = Select.village,
         type.respondent = What.category.of.respondent.are.you.interviewing.,
         name.interviewer = Name.of.the.interviewer,
         sex = What.is.the.respondent.s.sex.,
         ethnicity = What.is.the.respondent.s.ethnicity.,
         dob = What.is.the.year.of.birth.of.the.respondent.)

#Convert dob to age
age <- protein %>% 
  subset(select = "dob") %>% 
  mutate(now = 2022,
         age = now-dob)

age1 <- age %>% 
  filter(age <81)

hist(age1$age)
median(age1$age)

#NOTE: DOB = 0 represent those who were unable to recall their dob but were clearly older than 18 years. 
      #DOB = 1879 was potentially 1979, making the respondent 43, not 143 years.

#Identify enclave communities 
enclave <- c("Okwa II", "Okwa I", "Okwangwo", "Mkpot 1")

palatability <- protein %>% 
  pivot_longer(African.brush.tailed.porcupine:Yellow.backed.duiker, names_to = "animal", values_to = "score") %>% 
  relocate(animal:score,.after = dob) %>% 
  mutate(comm.type = case_when(village %in% enclave ~ "Enclave", TRUE ~ "Border"))%>% 
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

write.csv(palatability, "01. Data/palatability.model.csv", row.names = F)


####################################################
####################################################
####################################################
#######                                     ########
####### CORRELATION ACROSS RESPONDENT GROUPS #######
#######                                     ########
####################################################
####################################################
####################################################


# MEDIAN PALATABILITY -------------------------------------------------------

median.score <- palatability %>%
  group_by(animal, type.respondent, category) %>% 
  summarise(median.score = median(score)) %>% 
  filter(animal != "Chicken egg")


median.score2 <- palatability %>%
  group_by(animal, category) %>% 
  summarise(median.score = median(score)) %>% 
  filter(animal != "Chicken egg")

write.csv(median.score, "01. Data/median.score.csv", row.names = F)
write.csv(median.score2, "01. Data/median.score.category.csv", row.names = F)

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

#write.csv(sup.material, "supplementary material.csv", row.names = F)

median.category <- median.score %>% 
  group_by(category) %>% 
  summarise(median = median(median.score))


score.export <- palatability %>%
  group_by(animal, category) %>% 
  summarise(median.score = median(score)) %>% 
  filter(category != "Wild meat")


#write.csv(score.export, "other category export.csv", row.names = F)

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



ggsave("correlation plot.tiff",
       plot = cor.plot,
       path = "..//figure",
       scale = 1,
       width = 30,
       height = 10,
       units = c("cm"),
       dpi = 300)


# CORRELATION COEF --------------------------------------------------------

#Heat map
corrplot(cor(score %>%  subset(select = c(Household:Market) )))

#View data distribution for each group
ggpairs(score %>% drop_na(), c(3,4,5))+
  theme_pubclean()


#Get correlation coefficient
score <- score %>% 
  drop_na() #Remove 'Vulture' with no response from hunter

cor(score [, c(3,4,5)]) 

cor.test(score$Household, score$Hunter)
cor.test(score$Household, score$Market)
cor.test(score$Hunter, score$Market)



# Heat map of prop eaten by respondents -----------------------------------

heat.map <- sup.material %>% 
  mutate(type.respondent = ifelse(type.respondent == "Market", "Vendor", type.respondent)) %>% 
  group_by(animal, category, type.respondent) %>% 
  summarise(number.eaten = sum(count)) %>% 
  ungroup() %>% 
  mutate(sum.across = 190,
         prop = number.eaten/sum.across) %>% 
  arrange(animal) %>% 
  mutate(numbering = as.integer(factor(animal))) %>%
  ungroup()



count3 <- count2 %>% 
  group_by(animal) %>% 
  summarise(number.respondents = sum(count)) %>% 
  mutate(prop = 1)


heat.map.fig <- ggplot(heat.map, aes(x = type.respondent, y = reorder(animal, -numbering), fill = prop, width = prop)) +
  geom_tile() + 
  scale_fill_continuous(low = "#E7D4B5", high = "#169873", name = "Proportion",
                        breaks = c(0.25, 0.50, 0.75), 
                        labels = c("0.25", "0.50", "0.75")) +
  geom_text(data=count3, aes(y=animal, x=3.55, label=number.respondents), size=1.7)+
    labs(x = "Respondent category",
       y = "Species (or species group)") +
  theme_minimal(base_size = 12) +
  
  theme(axis.title = element_text(size = 11),
        axis.text.y = element_text(size = 7),
        axis.text.x = element_text(size = 10)) 

heat.map.fig

ggsave("heat.map.tiff",
       plot = heat.map.fig,
       path = "..//figure",
       scale = 1,
       width = 15.5,
       height = 22,
       units = c("cm"),
       dpi = 300)

