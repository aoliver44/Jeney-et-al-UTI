###################################
######### Alpha Diversity #########
###################################

# load libraries
library(vegan)
library(ggplot2)
library(cowplot)
library(tidyverse)
library(lmerTest)
library(reshape2)
library(ggpubr)

# set working directory & load files
setwd("~/Google Drive File Stream/My Drive/Other/Sarah_Steele/")
set.seed(999)

# Sequence Data
midas <- as.data.frame(t(read.csv("Bacteria_Composition_Summary.txt", check.names = FALSE, sep = "\t", row.names = 1)))

# Metadata
metadata <- read.csv("Final_Mapping_File for microbiomeanalyst.txt", sep = "\t", header = T)


# calculate alpha diveristy
shannon_div <- diversity(midas, "shannon") 
richness <- specnumber(midas)
Evenness <- shannon_div/log(richness)
all_diversity <- cbind(Evenness, richness, shannon_div)

# merge with Nutritional Metadata
alpha_diversity_data <- merge(all_diversity, metadata, by.x = "row.names", by.y = "X.NAME")
alpha_diversity_data_melt <- melt(data = alpha_diversity_data, id.vars = c("Row.names", "Treatment", "Individual"), measure.vars = c("shannon_div", "richness", "Evenness"))

alpha_diversity_data$Treatment <- ordered(alpha_diversity_data$Treatment, level = c("Donors", "Pre_FMT", "Post_FMT"))
richness <- ggplot(data = alpha_diversity_data) +
  aes(x = alpha_diversity_data$Treatment, y = alpha_diversity_data$richness, fill = Treatment) +
  geom_boxplot(outlier.shape = NA) + geom_point(position = position_jitterdodge(), alpha = 0.3) +
  annotate("text", x=1, y=2, label= "LME, p = 1.04e-07") +
  #geom_jitter(width = 0.15, alpha = 0.2) +
  scale_fill_brewer(palette = "Blues") +
  labs(x = '',
       y = 'Number of Species') +
  theme_bw(base_size = 14) + theme(legend.position = "none")

shapiro.test(alpha_diversity_data$richness)
fit <- lmer(richness ~ Treatment + (1|Individual), data = alpha_diversity_data)
summary(fit)

Evenness <- ggplot(data = alpha_diversity_data) +
  aes(x = alpha_diversity_data$Treatment, y = alpha_diversity_data$Evenness, fill = Treatment) +
  geom_boxplot(outlier.shape = NA) + geom_point(position = position_jitterdodge(), alpha = 0.3) +
  annotate("text", x=1, y=0.3, label= "LME, p = 1.04e-12") +
  #geom_jitter(width = 0.15, alpha = 0.2) +
  scale_fill_brewer(palette = "Greens") +
  labs(x = 'Intervention',
       y = 'Evenness (Pielou J)') +
  theme_bw(base_size = 14) + theme(legend.position = "none")

shapiro.test(alpha_diversity_data$Evenness)
fit2 <- lmer(Evenness ~ Treatment + (1|Individual), data = alpha_diversity_data)
summary(fit2)

#summary(aov(richness ~ Treatment + Error(Individual), data = alpha_diversity_data))
plot_grid(richness, Evenness, ncol = 1, labels = c("A", "B"))
