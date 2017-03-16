library(psych)

# using the master df from cms_randomforest.R and removing the target variable
master_pca <- master[, -53]
master_pca <- data.frame(scale(master_pca))
str(master_pca)


#PCA
cms.pca <- princomp(~ ., data = master_pca, cor = TRUE)
summary(cms.pca)
plot(cms.pca)

#Factor Analysis on entire data (all measures) to ascertain the validity of 
# 7 groups/latent variables/factors
cms.fa1 <- factanal(~., master_pca, factors=7)
cms.fa1
cms.fa1$loadings

cms.fa2 <- factanal(~., data=master_pca, factors=7, scores='regression')
cms.fa2
head(cms.fa2$scores)
cms.fa2$correlation


# Factor analysis for each of the seven groups separately
# effectiveness
str(effectiveness)
eff <- effectiveness[, -c(1, 20)]
str(eff)
sum(is.na(eff))

# observing correlations in the effectiveness measures
eff_cor <- lowerCor(eff)
eff_cor <- as.data.frame(eff_cor)
write.csv(eff_cor, "group_cor/eff_cor.csv")

# Some of the correlated variables are CAC_3_score-IMM_2_score, OP_29-OP_30, STK_6, 
# STK_8 etc. These should appear in the loading coefficeints as well. 

# Using fa from the psych package to find the loadings
eff.fa <- fa(eff, fm="ml", scores = "tenBerge")
eff.fa$loadings
eff.fa$scores
sum(is.na(eff.fa$scores))

# experience
str(experience)
exp <- experience[, -c(1, 13)]
str(exp)

# observing correlations in the effectiveness measures
exp_cor <- lowerCor(exp)
exp_cor <- as.data.frame(exp_cor)
write.csv(exp_cor, "group_cor/exp_cor.csv")

# Using fa to find the loadings
exp.fa <- fa(exp, fm="ml", scores = "tenBerge")
exp.fa$loadings
exp.fa$scores
sum(is.na(exp.fa$scores))


# 
str(medical)
med <- medical[, -c(1, 7)]
str(med)
sum(is.na(med))

# observing correlations in the effectiveness measures
med_cor <- lowerCor(med)
med_cor <- as.data.frame(med_cor)
write.csv(med_cor, "group_cor/med_cor.csv")

# Using fa to find the loadings
med.fa <- fa(med, fm="ml", scores = "tenBerge")
med.fa$loadings
med.fa$scores
sum(is.na(med.fa$scores))


# mortality
str(mortality)
mor <- mortality[, -c(1, 9)]
str(mor)
sum(is.na(mor))

# observing correlations in the effectiveness measures
mor_cor <- lowerCor(mor)
mor_cor <- as.data.frame(mor_cor)
write.csv(mor_cor, "group_cor/mor_cor.csv")

# Using fa to find the loadings
mor.fa <- fa(mor, fm="ml", scores = "tenBerge")
mor.fa$loadings
mor.fa$scores
sum(is.na(mor.fa$scores))


#readmission
str(readmission)
str(safety)
str(timely)

