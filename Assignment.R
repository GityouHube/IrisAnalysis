# Starting from github.com
# New (repositories) -> setting -> Create repository -> Copy URL (https://github.com/GityouHube/IrisAnalysis)

# in R
# New Project... -> Version Control -> Git -> Paste in 'Repository URL:' 

# Installing and Initializing Packages:
install.packages("tidyverse")
library(tidyverse)
renv::snapshot()
# > Lockfile written to "C:/Users/kctl226/Desktop/testrepo2/renv.lock".

# Loading and Exploring Data:
library(datasets)
data(iris)
head(iris,5)
summary(iris)
