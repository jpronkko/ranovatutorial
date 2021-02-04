# Tutorial: https://statsandr.com/blog/anova-in-r/#introduction
#install.packages("palmerpenguins")

library(palmerpenguins)
library(tidyverse)
library(plyr)
library(car) # qqPlot
library(lattice) # dotplot
library(multcomp)

dat <- penguins %>%
  select(species, flipper_length_mm)

dat <- filter(dat, complete.cases(dat))

ggplot(dat) +
  aes(x = species, y = flipper_length_mm, color = species) +
  geom_jitter() +
  theme(legend.position = "none")

# silmamaarainen normaaliustarkastelu ryhmille
ggplot(dat, aes(x=flipper_length_mm, fill=species)) + 
  geom_histogram(color="#e0ecef", alpha = 0.6, binwidth = 3, position='identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080", "#881010"))
# QQ ryhmille
qqPlot(dat$flipper_length_mm, groups = dat$species)

summary(dat)

# ANOVA
res_aov <- aov(flipper_length_mm ~ species, data = dat)

# normaalijakaumatestejä residuaaleille
ggplot() +
  aes(x = res_aov$residuals) +
  geom_histogram(bins = 10)

qqPlot(res_aov$residuals, id = FALSE)

# nollahypoteesi on, etta on normaalijakautunut
shapiro.test(res_aov$residuals)

# hajontojen tarkastelua visuaalisesti ryhmittain
ggplot(dat) + 
  aes(x = species, y = flipper_length_mm, color = species) +
  geom_boxplot()

dotplot(flipper_length_mm ~ species, data = dat)

# nollahypoteesi varianssit yhtasuuria
leveneTest(flipper_length_mm ~ species, data = dat)

par(mfrow = c(1,2))

plot(res_aov, which = 1)
plot(res_aov, which = 2)

ddply(dat, ~species, summarise, mean=mean(flipper_length_mm), sd=sd(flipper_length_mm))

# tai
#group_by(dat, species) %>%
#  summarise(
#    mean = mean(flipper_length_mm),
#    sd = sd(flipper_length_mm)
#  )

# Nollahypoteesi: ka:t yhtasuuria
oneway.test(flipper_length_mm ~ species, data = dat, var.equal = TRUE)

# Kayttaa Welch testia
oneway.test(flipper_length_mm ~ species, data = dat, var.equal = FALSE)

# vs (res_aov tehty ylla)
summary(res_aov)

###########
# Post-hoc, Tukey HSD, nollahypoteesi: ryhmien ka:t samoja

post_test <- glht(res_aov, linfct = mcp(species = "Tukey"))
summary(post_test)

par(mar = c(3, 8, 3, 3))
plot(post_test)

# tai:
plot(TukeyHSD(res_aov))

# Dunnett test:
post_test <- glht(res_aov, 
                  linfct = mcp(species = "Dunnet"))
summary(post_test)
plot(post_test)
