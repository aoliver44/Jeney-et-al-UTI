##################################
######### Beta Diversity #########
##################################

# load libraries
library(vegan)
library(labdsv)
library(ggplot2)
library(grid)

# set working directory & load files
setwd("~/Google Drive File Stream/My Drive/Other/Sarah_Steele/")
set.seed(999)

# Sequence Data
midas <- as.data.frame(t(read.csv("Bacteria_Composition_Summary.txt", check.names = FALSE, sep = "\t", row.names = 1)))

# Metadata
metadata <- read.csv("Final_Mapping_File for microbiomeanalyst.txt", sep = "\t", header = T)

# Beta-diversity analysis, NMDS and Permanovas
beta.mds <- metaMDS(midas, distance="bray", k=2)

stressplot(beta.mds)

sites <- as.data.frame(scores(beta.mds, display = "sites"))
species <- as.data.frame(scores(beta.mds, display = "species"))

nmds.sites <- merge(sites, metadata, by.x = "row.names", by.y = "X.NAME")
nmds.sites <- nmds.sites[order(nmds.sites$Timepoint),]
nmds.sites$Individual <- as.factor(nmds.sites$Individual)
cb_7 <- c("#d22154",
          "#007f36",
          "#670066",
          "#fdce5d",
          "#ccaaff",
          "#a65800",
          "#b25566")

ggplot() + 
  geom_point(data = nmds.sites, aes(NMDS1, NMDS2, color = Individual, shape = Treatment), alpha = 0.7, size = 5) + 
  geom_path(data=nmds.sites, aes(x=NMDS1,y=NMDS2,group=as.factor(Individual),colour=Individual), linetype = "dashed",size=0.5, arrow = arrow(), show.legend = FALSE) + 
  theme_classic(base_size = 20) + 
  geom_text(data = nmds.sites, aes(NMDS1, NMDS2, label = Individual), check_overlap = TRUE, size = 3.5) + 
  theme() +
  scale_color_manual(values = cb_7) + scale_fill_manual(values = cb_7) +guides(fill=guide_legend(title="Individual"))

# statistics: permanova
# Thank you Pedro Martinez Arbizu
source("parwise.adonis.r")
permanova_data <- merge(metadata, midas, by.x = "X.NAME", by.y = "row.names")

permanova_ind <- adonis(permanova_data[,8:NCOL(permanova_data)] ~ as.factor(Treatment), data = permanova_data, permutations = 999, parallel = 4, method = "bray")
permanova_ind

pairwise.adonis(x = permanova_data[,8:NCOL(permanova_data)], factors = permanova_data$Treatment, p.adjust.m = "BH")


temp_d_r <- subset(permanova_data, permanova_data$SAMPLETYPE == "Donors" | permanova_data$Treatment == "Post_FMT")
pairwise.adonis(x = temp_d_r[,8:NCOL(temp_d_r)], factors = temp_d_r$Individual_level, p.adjust.m = "BH")