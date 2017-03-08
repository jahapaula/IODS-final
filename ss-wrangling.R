# https://druedin.com/2015/07/14/overcoming-warnings-when-importing-spss-files-to-r/
library(foreign)
library(dplyr)

ss2016 <- read.spss("15618_SuomiSyö_2016.sav", to.data.frame=TRUE, use.value.labels = T)#, reencode = "macintosh") piti tehdä jotain mutta eipä toimi

# saving variable names to a diff dataset just to make it easier to check at them
# encoding doesn't work
dataset.labels <- as.data.frame(attr(ss2016, "variable.labels"), encoding="macintosh")
str(dataset.labels)

ss2016.sub <- dplyr::select(ss2016, k17.5, k67_12, k67_15, k67_16, suuralue, supu, ika, starts_with("k1."), starts_with("k18a_"), starts_with("k63."), starts_with("k51."))
#dataset.labels <- dplyr::selectrows(dataset.labels, k17.5, k67_12, k67_15, k67_16, suuralue, supu, ika, starts_with("k18a_"), starts_with("k63."), starts_with("k51."))

#transforming ruokavalio questions to binary variables
ss2016.sub <- mutate(ss2016.sub, lihaton = k67_12 == 12, kasvis = k67_15 == 15, vegaani = k67_16 == 16)
ss2016.sub <- dplyr::select(ss2016.sub, -k67_12, -k67_15, -k67_16)

write.csv(ss2016.sub, "ss2016sub.csv")

#The Jungerian excursion, that is, went elsewhere to remove all ääkköset grrr
ss16 <- read.table("ss2016sub-ao.csv", sep=",", header = TRUE, row.names = 1 )


#### Generating the subset for PCA ####

# creating a sub dataset with attitude factors plus demo
ss16.atti <- dplyr::select(ss16, suuralue, supu, ika, kasvis, lihaton, vegaani, starts_with("k1."))
# dataset with attitude variables only
ss16.att <- dplyr::select(ss16, starts_with("k1."))
# dataset with viisi tärkeintä ominaisuutta ruoalla
# ss16.attr <- dplyr::select(ss16, suuralue, supu, ika, kasvis, lihaton, vegaani, starts_with("k18a"))

# https://www.r-bloggers.com/reorder-factor-levels/
# ss16.att$k1.1 = factor(ss16.att$k1.1,levels(ss16.att$k1.1)[c(4,2,1,3,5)])
# reorder all factor levels
for(i in c(1:ncol(ss16.att))) {
  ss16.att[,i] = factor(ss16.att[,i],levels(ss16.att[,i])[c(4,2,1,3,5)]) 
}

# save this version for likert scales
write.csv(ss16.att, "ss2016likerts.csv")

# transform all factors to numeric
for(i in c(1:ncol(ss16.att))) {
  ss16.att[,i] <- as.numeric(ss16.att[,i])
}

# deal with missing values
# print out a completeness indicator of the data
complete.cases(ss16.att)

# print out the data along with a completeness indicator as the last column
data.frame(ss16.att[-1], comp = complete.cases(ss16.att))

# filter out all rows with NA values
ss16.att_ <- filter(ss16.att, complete.cases(ss16.att))

# standardize the variables
ss16.att_std <- scale(ss16.att_)

# save the final, scaled and filtered dataset to a file
write.csv(ss16.att_std, "ss2016final.csv")