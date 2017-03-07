# https://druedin.com/2015/07/14/overcoming-warnings-when-importing-spss-files-to-r/
library(foreign)
library(dplyr)

ss2016 <- read.spss("15618_SuomiSyö_2016.sav", to.data.frame=TRUE, reencode = "macintosh")

# saving variable names to a diff dataset
dataset.labels <- as.data.frame(attr(ss2016, "variable.labels"), encoding="UTF-8")
str(dataset.labels)
levels(dataset.labels) <- gsub("\344", "ä", levels(dataset.labels))

#library(stringr)
#ss2016 <- str_replace(ss2016, pattern="<e4>", replace ="ä")
#keep <- c("k17.5", "k67_12", "k67_15", "k67_16", "suuralue", "supu", "ika") 
#ss2016-sub <- select(ss2016, one_of(keep))

ss16.sub <- select(ss16, k17.5, k67_12, k67_15, k67_16, suuralue, supu, ika, starts_with("k18a_"), starts_with("k63."), starts_with("k51."))
#levels(ss2016.sub) <- gsub("\344", "a", levels(ss2016.sub))
#ss2016.sub$ <- gsub("\344", "a", ss2016-sub$)

#ss2016.sub <- apply(ss2016.sub, 2, function(x) { 
#  gsub("<e4>'", "ä", x)
#})

ss16.sub$k67.comb <- with(ss16.sub,interaction(k67_12, k67_15, k67_16))

ss16.sub <- mutate(ss16.sub, lihaton = k67_12 == 12, kasvis = k67_15 == 15, vegaani = k67_16 == 16)
ss16.sub <- select(ss16.sub, -k67_12, -k67_15, -k67_16)

# print out a completeness indicator of the data
complete.cases(ss16.sub)

# print out the data along with a completeness indicator as the last column
data.frame(human[-1], comp = complete.cases(human))

# filter out all rows with NA values
human_ <- filter(human, complete.cases(human))

#deal with missing values


library(dplyr)
library(ggplot2)
library(GGally)
library(stringr)
library(tidyr)

# standardize the variables
ss16_std <- scale(ss16)

# perform principal component analysis (with the SVD method)
pca_1 <- prcomp(ss16)

# draw a biplot of the principal component representation and the original variables
biplot(pca_human1, choices = 1:2, cex = c(0.7, 1), col = c("grey40", "deeppink2"), main = " ", sub = "Figure 2.")
#fig.cap = "caption"