library(tidyverse)
library(labelled)
library(ggcorrplot)
library(psych)


## Data -------
datafile<-'https://github.com/Neilblund/SDSB340-Spring-2026/raw/refs/heads/master/Data/polarization1.rds'
dat1<-readRDS(url(datafile))
labelfile<-'https://github.com/Neilblund/SDSB340-Spring-2026/raw/refs/heads/master/Data/polarization_labels.rds'
labels_frame<-readRDS(url(labelfile))


# Getting only the scale columns
scale_cols<-labels_frame$Variable
scale_data<-dat1|>
  select(all_of(scale_cols))




################################################################################

# Descriptive stats

# Correlations
cmat<-cor(scale_data)  # Get initial correlations (Why are these all NA?)

# Getting correlations for pairwise observations

cmat<-cor(scale_data, 
          use= 'pairwise.complete.obs')

# plotting results
ggcorrplot(cmat, 
           lab = TRUE,  # add labels
           lab_size = 2 # shrink them to make them fit
) 




# alpha (measure of scale reliability assuming tau equivalence and a single dimension)

## Here's how we could calculate alpha with base R code:
r_bar<-mean(cmat[lower.tri(cmat)])
k<-ncol(cmat)
alpha <- (k * r_bar) / (1 + (k - 1) * r_bar)
alpha

# or we can just use the alpha function from the psych package:
ca<-alpha(cmat)
ca$total   # alpha = .98 = 98% of the variation is explained by true score variation
ca$alpha.drop # there's no apparent benefit to dropping *any* of the items in terms of improving reliability


# Bootstrapping a confidence interval for alpha:

r <- 300
thetas<-numeric(r)
for(i  in 1:r){
  resampled_data<-scale_data[sample(1:nrow(scale_data), size=nrow(scale_data), replace=T),]
  cmat<-cor(resampled_data, use= 'pairwise.complete.obs')
  r_bar<-mean(cmat[lower.tri(cmat)])
  k<-ncol(cmat)
  alpha <- (k * r_bar) / (1 + (k - 1) * r_bar)
  thetas[i] <- alpha
}

moe<-sd(thetas) * qnorm(.975) # margin of error using the standard deviation of each bootstrap replicate times 1.96
mean(thetas)                  # average of all estimated results
mean(thetas) + moe            # upper bound of alpha
mean(thetas) - moe            # lower bound of alpha



# Creating a combined index


dat1$overall_score<-rowMeans(scale_data)
ggplot(dat1, aes(x=overall_score))  + geom_histogram()

# moralizing dimension only
dat1$moralizing<- scale_data|>
  select(starts_with("m"))|>
  rowMeans()
ggplot(dat1, aes(x=moralizing))  + geom_histogram()

# othering dimension only
dat1$othering<-scale_data|>
  select(starts_with("o"))|>
  rowMeans()
ggplot(dat1, aes(x=othering))  + geom_histogram()
# aversion dimension only
dat1$aversion<-scale_data|>
  select(starts_with("a"))|>
  rowMeans()
ggplot(dat1, aes(x=aversion))  + geom_histogram()





