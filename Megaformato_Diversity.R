set.seed(343)
dat = MASS::mvrnorm(1000,mu=c(0,0),
                    Sigma=cbind(c(1,.50),c(.50,1)),
                    empirical=TRUE)

scoreX <- dat[,1]
coarse_scoreX <- as.numeric(cut(scoreX,breaks=5))

scoreY <- dat[,2]
coarse_scoreY <- as.numeric(cut(scoreY,breaks=5))

table(coarse_scoreX)
coarse_scoreX2 = as.integer(scoreX + .5)

table(coarse_scoreX2)
