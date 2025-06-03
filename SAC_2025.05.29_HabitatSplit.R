### Project: Making species accumulation curve for Agusan Marsh data, split by habitat
### Code Author: Justin M. Bernstein
### Last Modified Date: 05/29/2025

setwd("D:/Documents/Publications/Collaborations/Sanguila-et-al_Agusan-Marsh_Checklist/Agusan-Marsh_final-version_2025/")


# load packages
library(dplyr)
library(tidyr)
library(vegan)


# read data in for surveys.
spp.data <- read.csv("SAC_dataset_2025.05.29_HabitatSplit.csv")

# change the column names and double-check
colnames(spp.data) <- c("species", "data", "habitat")
head(spp.data)

# make subsets by habitat type
peat <- subset(spp.data, habitat == "peat_swamp") %>%
  select(-habitat)

fresh <- subset(spp.data, habitat == "freshwater_swamp") %>%
  select(-habitat)

mixed.term <- subset(spp.data, habitat == "mixed_swamp_terminalia") %>%
  select(-habitat)

mixed.dist <- subset(spp.data, habitat == "mixed_swamp_disturbed") %>%
  select(-habitat)

# we will also make a dataframe where we have both mixed swamps as once dataframe
mixed.swamps <- rbind(mixed.term, mixed.dist)

# check to make sure the rows were added correctly
nrow(mixed.term) # 51 rows
nrow(mixed.dist) # 87 rows
nrow(mixed.swamps) # 138 rows (51+87)

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Below are the Species Accumulation Curves. To keep the code succinct, make sure to replace the object for the incidence matrix
# with the appropriate dataframe you want a curve for (peat, fresh, mixed.term, mixed.dist, or mixed.swamps)
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# convert 
incidence.matrix <- mixed.swamps %>%
  mutate(date = as.Date(data)) %>%
  distinct(date, species) %>%
  pivot_wider(names_from = species, values_from = species,
              values_fn = length, values_fill = 0) %>%
  mutate(across(-date, ~ ifelse(. > 0, 1, 0)))

# now we must remove the date column (as it is a type date, not numeric, which is required by specaccum)
incidence.matrix.numeric <- incidence.matrix %>% select(-date)

# calculation the species accumulation curve (SAC)
spec.acc <- specaccum(incidence.matrix.numeric, method = "random")

# plot
pdf("SAC_mixed_swamps_disturbed.pdf")
plot(spec.acc,
     ci.type = "poly",        # Confidence interval shaded
     ci.col = "#ff9595",    # CI color
     col = "#7a0000",            # Line color
     lwd = 2,                 # Line width
     xlab = "Number of Survey Days",
     ylab = "Cumulative Species Richness",
     main = "Species Accumulation Curve (Mixed Swamps - Disturbed)")
dev.off()

# calculate total species richness
total.species <- sum(colSums(incidence.matrix.numeric) > 0)
print(paste("Total species richness:", total.species))
# [1] "Total species richness (peat swamps): 24"
# [1] "Total species richness (freshwater swamps): 27"
# [1] "Total species richness (mixed swamps total): 23"
# [1] "Total species richness (mixed swamps terminalia): 16"
# [1] "Total species richness (): 16"

# estimate species diversity indices
# For each survey (row), compute Shannon diversity:
shannon.div <- diversity(incidence.matrix.numeric, index = "shannon")
summary(shannon.div)
# peat swamps
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  0.0000  0.0000  0.8959  0.9454  1.4421  2.3979

# freshwater swamps
  #  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
  # 0.000   1.442   1.701   1.597   2.175   2.485

# mixed swamps (total)
  #  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
  # 1.386   1.609   1.792   1.829   2.009   2.303

# mixed swamps (terminalia)
  #  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
  # 1.386   1.386   1.498   1.645   1.756   2.197

# mixed swamps (disturbed)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  0.0000  0.8664  1.5890  1.3533  1.9074  2.3026