library(psych)

# using the master df from cms_randomforest.R and removing the target variable
master_pca <- master[, -53]
master_pca <- data.frame(scale(master_pca))
str(master_pca)
cor.plot(master_pca)

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

# observing correlations in the effectiveness measures
pairs.panels(eff)
# using fa from the psych package
eff.fa <- fa(eff)
eff.fa

str(experience)
str(medical)
str(mortality)
str(readmission)
str(safety)
str(timely)

