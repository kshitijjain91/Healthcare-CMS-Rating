# Factor analysis for each of the seven groups separately
# effectiveness
effectiveness <- read.csv("effectiveness.csv") 
str(effectiveness)
eff <- effectiveness[, -c(1, 2)]
str(eff)
sum(is.na(eff))

# observing correlations in the effectiveness measures
eff_cor <- lowerCor(eff)

# Some of the correlated variables are CAC_3_score-IMM_2_score, OP_29-OP_30, STK_6, 
# STK_8 etc. These should appear in the loading coefficeints as well. 

# Using fa from the psych package to find the loadings
eff.fa <- fa(eff, fm="ml", scores = "tenBerge")
eff.fa$loadings
eff.fa$scores
sum(is.na(eff.fa$scores))

# experience
experience <- read.csv("experience.csv")
str(experience)
exp <- experience[, -c(1, 2)]
str(exp)

# observing correlations in the effectiveness measures
exp_cor <- lowerCor(exp)


# Using fa to find the loadings
exp.fa <- fa(exp, fm="ml", scores = "tenBerge")
exp.fa$loadings
exp.fa$scores
sum(is.na(exp.fa$scores))


# medical
medical <- read.csv("medical.csv")
str(medical)
med <- medical[, -c(1, 2)]
str(med)
sum(is.na(med))

# observing correlations in the effectiveness measures
med_cor <- lowerCor(med)

# Using fa to find the loadings
med.fa <- fa(med, fm="ml", scores = "tenBerge")
med.fa$loadings
med.fa$scores
sum(is.na(med.fa$scores))


# mortality
mortality <- read.csv("mortality.csv")
str(mortality)
mor <- mortality[, -c(1, 2)]
str(mor)
sum(is.na(mor))

# observing correlations in the effectiveness measures
mor_cor <- lowerCor(mor)

# Using fa to find the loadings
mor.fa <- fa(mor, fm="ml", scores = "tenBerge")
mor.fa$loadings
mor.fa$scores
sum(is.na(mor.fa$scores))


#readmission
readmission <- read.csv("readmission.csv")
str(readmission)
re <- readmission[, -c(1, 2)]
str(re)
sum(is.na(re))

# observing correlations in the effectiveness measures
re_cor <- lowerCor(re)

# Using fa to find the loadings
re.fa <- fa(re, fm="ml", scores = "tenBerge")
re.fa$loadings
re.fa$scores
sum(is.na(re.fa$scores))



# safety
safety <- read.csv("safety.csv")
str(safety)
safe <- safety[, -c(1, 2)]
str(safe)
sum(is.na(safe))

# observing correlations in the effectiveness measures
safe_cor <- lowerCor(safe)

# Using fa to find the loadings
safe.fa <- fa(safe, fm="ml", scores = "tenBerge")
safe.fa$loadings
safe.fa$scores
sum(is.na(safe.fa$scores))


# timely
timely <- read.csv("timely.csv")
str(timely)
time <- timely[, -c(1, 2)]
str(time)
sum(is.na(time))

# observing correlations in the effectiveness measures
time_cor <- lowerCor(time)

# Using fa to find the loadings
time.fa <- fa(time, fm="ml", scores = "tenBerge")
time.fa$loadings
time.fa$scores
sum(is.na(time.fa$scores))

# merging all scores with the master df
e <- read.csv("effectiveness.csv") 
id <- e$Provider.ID
id
scores <- as.data.frame(cbind(eff.fa$scores, exp.fa$scores, 
                              med.fa$scores, mor.fa$scores, re.fa$scores, safe.fa$scores, time.fa$scores)) 
colnames(scores) <- c("eff", "exp", "med", "mor", "re", "safe", "time")
str(scores)

# calculating weighted scores
weights <- c(0.04, 0.22, 0.04, 0.22, 0.22, 0.22, 0.04)
for (index in 1:length(weights)){
  scores[, index] <- scores[, index]*weights[index]
} 
scores$fscore <- scores$eff + scores$exp + scores$med + scores$mor + scores$re + scores$safe +scores$time
str(scores)
  
# clustering
score_cluster <- kmeans(scores$fscore, 5, nstart = 100)
score_cluster_1 <- data.frame(scores,score_cluster$cluster)
n=nrow(score_cluster_1);n
score_cluster_1$rating <- score_cluster_1$score_cluster.cluster


# ############re-assigning cluster for rating##################
avg <- c()
for (i in 1:5){
 avg[i] <- score_cluster$centers[i]
}
avg
#
# temp_matrix <- matrix(c(score_cluster$centers,sort(score_cluster$centers, decreasing=T),1,2,3,4,5),5,3)
temp_matrix <- matrix(c(avg,sort(avg, decreasing=T),1,2,3,4,5),5,3)
temp_old<-c()
temp_new<-c()
for(i in  1:5){
 for(j in 1:5){
   if(temp_matrix[i,1]== temp_matrix[j,2]){
     temp_new[i] <- j
     temp_old[i]<-i
     break
   }
  }
}
cluster_assign <- data.frame(temp_old,temp_new)
for(j in 1:n){
 for(i in 1:5){
 if(score_cluster_1$score_cluster.cluster[j] == cluster_assign[i,1]){
   score_cluster_1$rating[j] <- cluster_assign[i,2]
   break
   }
}
}

# ######################star_rating_validation######################  
rating_file <- read.csv("rating.csv")
ratings <- rating_file[, 3]
length(ratings)
summary(ratings)
score_cluster_1$rating <- as.factor(score_cluster_1$rating)
summary(score_cluster_1$rating)
t <- table(ratings, score_cluster_1$rating)
t <- as.matrix(t)
t


for (i in 1:5){
  s <- t[i, i]/sum(t[i, ])
  print(s)
}

