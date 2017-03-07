# https://druedin.com/2015/07/14/overcoming-warnings-when-importing-spss-files-to-r/
library(foreign)
library(dplyr)

ss2016 <- read.spss("15618_SuomiSyö_2016.sav", to.data.frame=TRUE, use.value.labels = T)#, reencode = "macintosh") piti tehdä jotain mutta eipä toimi

# saving variable names to a diff dataset just to make it easier to check at them
# encoding doesn't work
dataset.labels <- as.data.frame(attr(ss2016, "variable.labels"), encoding="macintosh")
str(dataset.labels)
#value.labels(ss2016) <- gsub("\344", "ä", value.labels(ss2016))

#library(stringr)
#ss2016 <- str_replace(ss2016, pattern="<e4>", replace ="ä")
#keep <- c("k17.5", "k67_12", "k67_15", "k67_16", "suuralue", "supu", "ika") 
#ss2016-sub <- select(ss2016, one_of(keep))

ss2016.sub <- dplyr::select(ss2016, k17.5, k67_12, k67_15, k67_16, suuralue, supu, ika, starts_with("k1."), starts_with("k18a_"), starts_with("k63."), starts_with("k51."))
#dataset.labels <- dplyr::selectrows(dataset.labels, k17.5, k67_12, k67_15, k67_16, suuralue, supu, ika, starts_with("k18a_"), starts_with("k63."), starts_with("k51."))

#levels(ss2016.sub) <- gsub("\344", "a", levels(ss2016.sub))
#ss2016.sub$ <- gsub("\344", "a", ss2016-sub$)

#ss2016.sub <- apply(ss2016.sub, 2, function(x) { 
#  gsub("<e4>'", "ä", x)
#})

#transforming ruokavalio questions to binary variables
#ss16.sub$k67.comb <- with(ss16.sub,interaction(k67_12, k67_15, k67_16))
ss2016.sub <- mutate(ss2016.sub, lihaton = k67_12 == 12, kasvis = k67_15 == 15, vegaani = k67_16 == 16)
ss2016.sub <- dplyr::select(ss2016.sub, -k67_12, -k67_15, -k67_16)

write.csv(ss2016.sub, "ss2016sub.csv")

#The Jungerian excursion, that is, went elsewhere to remove all ääkköset grrr
ss16 <- read.table("ss2016sub-ao.csv", sep=",", header = TRUE, row.names = 1 )

# dataset with attitude factors plus demo
ss16.atti <- dplyr::select(ss16, suuralue, supu, ika, kasvis, lihaton, vegaani, starts_with("k1."))
# dataset with attitude factors only
ss16.att <- dplyr::select(ss16, starts_with("k1."))
# dataset with viisi tärkeintä ominaisuutta ruoalla
ss16.attr <- dplyr::select(ss16, suuralue, supu, ika, kasvis, lihaton, vegaani, starts_with("k18a"))
# dataset with vastuullisuuskysymykset + demo
ss16.resp <- dplyr::select(ss16, suuralue, supu, ika, kasvis, lihaton, vegaani, starts_with("k51."))
# dataset with vastuullisuuskysymykset
ss16.res <- dplyr::select(ss16, starts_with("k51."))
# dataset with terveellisyyskysymykset + demo
ss16.heal <- dplyr::select(ss16, suuralue, supu, ika, kasvis, lihaton, vegaani, starts_with("k63."))
# dataset with terveellisyyskysymykset no demo
ss16.hea <- dplyr::select(ss16, starts_with("k63."))


library(dplyr)
library(ggplot2)
library(GGally)
library(stringr)
library(tidyr)




### PCA for food attributes questions

# transform all factors to numeric
for(i in c(1:ncol(ss16.att))) {
  ss16.att[,i] <- as.numeric(ss16.att[,i])
}

# deal with missing values here!!
# print out a completeness indicator of the data
complete.cases(ss16.att)

# print out the data along with a completeness indicator as the last column
data.frame(ss16.att[-1], comp = complete.cases(ss16.att))

# filter out all rows with NA values
ss16.att_ <- filter(ss16.att, complete.cases(ss16.att))

# standardize the variables
ss16.att_std <- scale(ss16.att_)

# perform principal component analysis (with the SVD method)
pca_1 <- prcomp(ss16.att_std)
summary(pca_1)

# draw a biplot of the principal component representation and the original variables
biplot(pca_1, choices = 1:2, cex = c(0.7, 1), col = c("grey40", "deeppink2"), main = " ", sub = "Figure 1")
#fig.cap = "caption"




### same stuff for responsibility questions

# transform all factors to numeric
for(i in c(1:ncol(ss16.res))) {
  ss16.res[,i] <- as.numeric(ss16.res[,i])
}

# deal with missing values here!!
# print out a completeness indicator of the data
complete.cases(ss16.res)

# print out the data along with a completeness indicator as the last column
data.frame(ss16.res[-1], comp = complete.cases(ss16.res))

# filter out all rows with NA values
ss16.res_ <- filter(ss16.res, complete.cases(ss16.res))

# standardize the variables
ss16.res_std <- scale(ss16.res_)

# perform principal component analysis (with the SVD method)
pca_2 <- prcomp(ss16.res_std)
summary(pca_2)

# draw a biplot of the principal component representation and the original variables
biplot(pca_2, choices = 1:2, cex = c(0.7, 1), col = c("grey40", "deeppink2"), main = " ", sub = "Figure 1")
#fig.cap = "caption"


### Trying out an MCA with the food attribute variables

library(FactoMineR)

# removing some un-interesting variables
tea_time2 <- dplyr::select(tea_time, -price, -How)

# multiple correspondence analysis
mca_sub <- MCA(ss16.attr, graph = F,  na.method="NA")

# summary of the model
summary(mca_sub)
dimdesc(mca_sub)

# visualize MCA
plot(mca_sub, invisible=c("ind"), habillage = "quali")
# plot(mca, invisible=c("ind","var"))