#####################################
######### Fig. 1 UTI Counts #########
#####################################


# load libraries
library(reshape2)
library(ggplot2)

# set working directory & load files
setwd("~/Google Drive File Stream/My Drive/Other/Sarah_Steele/")
set.seed(999)

fig1_data <- read.csv("Fig1_data.txt", sep = "\t")
fig1_data_melt <- melt(fig1_data, id.vars = "Individual")
fig1_data_melt$Individual <- factor(fig1_data_melt$Individual, levels = c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", "P10"))

ggplot(data = fig1_data_melt) +
  aes(x = reorder(Individual, sort(as.numeric(Individual))), fill = variable, weight = value) +
  geom_bar(position = "dodge") +
  labs(title = 'Number of Urinary Tract Infections',
       y = 'Number of UTIs', x = "Individual") + scale_fill_manual(values = c("lightgoldenrod3",
                                                                              "steelblue3")) +
  theme_bw(base_size = 14) + guides(fill=guide_legend(title="Treatment"))
