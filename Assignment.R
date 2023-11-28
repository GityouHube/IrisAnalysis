# Starting from github.com
# New (repositories) -> setting -> Create repository -> Copy URL (https://github.com/GityouHube/IrisAnalysis)

# in R
# New Project... -> Version Control -> Git -> Paste in 'Repository URL:' 

# Installing and Initializing Packages:
library(renv)
# renv::init()
# renv::install(c("tidyverse","ggpubr"))
install.packages("tidyverse")
library(tidyverse)
renv::snapshot()

# > Lockfile written to "C:/Users/kctl226/Desktop/testrepo2/renv.lock".

# Loading and Exploring Data:
library(datasets)
data(iris)
head(iris,5)
summary(iris)

### Data Manipulation, https://rpubs.com/analystben/chapter-2
# Data set
iris <- as_tibble(iris)
iris
print(iris,n=3,width = Inf)

iris %>% summarise_if(is.numeric, mean)

library(GGally)
ggpairs(iris, aes(color = Species))

clean.data <- iris %>% drop_na() %>% unique()
summary(clean.data)        

# Aggregation
iris %>% group_by(Species) %>% summarize_all(mean)
iris %>% group_by(Species) %>% summarize_all(median)

# Sampling
sample(c("A","B","C"), size = 10, replace = T)
take <- sample(seq(nrow(iris)), size = 15)
iris[take,]
set.seed(1000)

s <- iris%>%slice_sample(n=15)
ggpairs(s,aes(color=Species))

# Stratified Sampling
library(sampling)
id2 <- strata(iris, stratanames = "Species", size = c(5,5,5), method = "srswor")
id2

s2 <- iris %>% slice(id2$ID_unit)
ggpairs(s2, aes(color=Species))

# Features
plotly::plot_ly(iris, x = ~Sepal.Length, y = ~Petal.Length, z = ~Sepal.Width,
                size = ~Petal.Width, color = ~Species, type = "scatter3d")
# > /session/viewhtml119fc1e6d2d60/index.html?viewer_pane=1&capabilities=1&host=http%3A%2F%2F127.0.0.1%3A62868 not found
# It's not open via a new window.

pc <- iris %>% select(-Species) %>% as.matrix() %>% prcomp()
summary(pc)
plot(pc, type = "line")
str(pc)
iris_protected <- as_tibble(pc$x) %>% add_column(Species = iris$Species)
ggplot(iris_protected, aes(x = PC1, y = PC2, color = Species)) + 
  geom_point()

ggplot(iris_protected,
       aes(x = PC1, y = 0, color = Species)) +
  geom_point() +
  scale_y_continuous(expand = c(0,0)) +
  theme(axis,text.y = element_blank(),
        axis.title.y = element_blank()
  )
# > Error in `plot_theme()`:
# ! Problem merging the `line` theme element

library(factoextra)
fviz_pca(pc)
fviz_pca_var(pc)

d <- iris %>% select(-Species) %>% dist()
fit <- cmdscale(d, k = 2)
colnames(fit) <- c("comp1", "comp2")
fit <- as_tibble(fit) %>% add_column(Species = iris$Species)

ggplot(fit, aes(x = comp1, y = comp2, color = Species)) + geom_point()

# Feature selection
ggplot(iris, aes(x = Petal.Width)) + geom_histogram(binwidth = .2)
iris %>% pull(Sepal.Width) %>% cut(breaks = 3)

library(arules)
iris %>% pull(Petal.Width) %>% discretize(method = "interval", breaks = 3)
iris %>% pull(Petal.Width) %>% discretize(method = "frequency", breaks = 3)
iris %>% pull(Petal.Width) %>% discretize(method = "cluster", breaks = 3)
ggplot(iris, aes(Petal.Width)) + geom_histogram(binwidth = .2) +
  geom_vline(xintercept =
               iris %>% pull(Petal.Width) %>% discretize(method = "interval", breaks = 3, onlycuts = TRUE),
             color = "blue") +
  labs(title = "Discretization: interval", subtitle = "Blue lines are boundaries")

ggplot(iris, aes(Petal.Width)) + geom_histogram(binwidth = .2) +
  geom_vline(xintercept =
               iris %>% pull(Petal.Width) %>% discretize(method = "frequency", breaks = 3, onlycuts = TRUE),
             color = "blue") +
  labs(title = "Discretization: frequency", subtitle = "Blue lines are boundaries")

ggplot(iris, aes(Petal.Width)) + geom_histogram(binwidth = .2) +
  geom_vline(xintercept =
               iris %>% pull(Petal.Width) %>% discretize(method = "cluster", breaks = 3, onlycuts = TRUE),
             color = "blue") +
  labs(title = "Discretization: cluster", subtitle = "Blue lines are boundaries")

# Standardized Data
scale_numeric <- function(x) x %>% mutate_if(is.numeric, function(y) as.vector(scale(y)))

iris.scaled <- iris %>% scale_numeric()
iris.scaled
summary(iris.scaled)

# Proximities
iris_sample <- iris.scaled %>% select(-Species) %>% slice(1:5)
iris_sample
dist(iris_sample, method = "euclidean")
dist(iris_sample, method = "manhattan")
dist(iris_sample, method = "maximum")

# Distances for Binary Data
b <- rbind(
  c(0,0,0,1,1,1,1,0,0,1),
  c(0,0,1,1,1,0,0,1,0,0)
)
b
b_logical <- apply(b, MARGIN = 2, as.logical)
b_logical
dist(b, method = "manhattan")
dist(b, method = "euclidean")^2
dist(b, method = "binary")
people <- tibble(
  height = c(      160,    185,    170),
  weight = c(       52,     90,     75),
  sex    = c( "female", "male", "male")
)
people

# library(proxy)
library(caret)
data_dummy <- dummyVars(~., people) %>% predict(people)
data_dummy

weight_matrix <- matrix(c(1, 1, 1/2, 1/2), ncol = 4, nrow = nrow(data_dummy), byrow = TRUE)
data_dummy_scaled <- scale(data_dummy) * weight_matrix

d_dummy <- dist(data_dummy_scaled)
d_dummy


library(proxy)
pr_DB$get_entry_names()

# Relationships Between Features
cc <- iris %>% select(-Species) %>% cor()
cc
ggplot(iris, aes(Petal.Length, Petal.Width)) + 
  geom_point() +
  geom_smooth(method = "lm")
with(iris, cor(Petal.Length, Petal.Width))
iris_ord <- iris %>% mutate_if(is.numeric,
                               function(x) cut(x, 3, labels = c("short", "medium", "long"), ordered = TRUE))

iris_ord
summary(iris_ord)
iris_ord %>% pull(Sepal.Length)
iris_ord %>% select(-Species) %>% sapply(xtfrm) %>% cor(method = "kendall")
iris_ord %>% select(-Species) %>% sapply(xtfrm) %>% cor(method = "spearman")
iris %>% select(-Species) %>% cor()

# Density Estimation
ggplot(iris, aes(x = Petal.Length, y = 0)) + geom_point()

ggplot(iris, aes(x = Petal.Length)) +
  geom_histogram() +
  geom_rug(alpha = 1/2)

ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_bin2d(bins = 10) +
  geom_jitter(color = "red")

ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_hex(bins = 10) +
  geom_jitter(color = "red")

ggplot(iris, aes(Petal.Length)) +
  geom_density(bw = .2) +
  geom_rug(alpha = 1/2)

ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_density_2d_filled() +
  geom_jitter()

# Exploring Data
summary(iris)
iris %>% pull(Sepal.Length) %>% mean()
iris %>% pull(Sepal.Length) %>% sd()
mean(c(1, 2, NA, 3, 4, 5))
mean(c(1, 2, NA, 3, 4, 5),  na.rm = TRUE)
iris %>% pull(Sepal.Length) %>% mean()
iris %>% pull(Sepal.Length) %>% mean(trim = .1)
iris %>% summarize_if(is.numeric, mean)
iris %>% summarize_if(is.numeric, sd)
iris %>% summarize_if(is.numeric, list(min = min, median = median, max = max))
iris %>% summarize_if(is.numeric, mad) # Mean Absolute Deviation
iris %>% group_by(Species) %>% summarize(across(Sepal.Length, mean))
iris %>% group_by(Species) %>% summarize_all(mean)
res.aov <- aov(Sepal.Length ~ Species, data = iris) # aov: ANOVA test
summary(res.aov)
TukeyHSD(res.aov)

# Tabulate data
iris %>% group_by(Species) %>% summarize(n())
iris_ord <- iris %>% mutate_if(is.numeric,
                               function(x) cut(x, 3, labels = c("short", "medium", "long"), ordered = TRUE))
iris_ord
summary(iris_ord)
tbl <- iris_ord %>% select(Sepal.Length, Species) %>% table()
tbl
iris_ord %>%
  select(Species, Sepal.Length) %>%
  ### Relationship Between Nominal and Ordinal Features
  pivot_longer(cols = Sepal.Length) %>%
  group_by(Species, value) %>% count() %>% ungroup() %>%
  pivot_wider(names_from = Species, values_from = n)

tbl %>% chisq.test()
fisher.test(tbl)
iris %>% pull(Petal.Length) %>% quantile()
iris %>% summarize(IQR = quantile(Petal.Length, probs = 0.75) - quantile(Petal.Length, probs = 0.25))

### Visualization
ggplot(iris, aes(Petal.Width)) + geom_histogram(bins = 20)
ggplot(iris, aes(Species, Sepal.Length)) + 
  geom_boxplot()
iris %>% group_by(Species) %>% summarize_if(is.numeric, median)

library(tidyr)
iris_long <- iris %>% mutate(id = row_number()) %>% pivot_longer(1:4)
ggplot(iris_long, aes(name, value)) + 
  geom_boxplot() +
  labs(y = "Original value")

library(tidyr)
iris_long_scaled <- iris %>% scale_numeric() %>% mutate(id = row_number()) %>% pivot_longer(1:4)
ggplot(iris_long_scaled, aes(name, value)) + 
  geom_boxplot() +
  labs(y = "Scaled value")

ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) + 
  geom_point()

library("GGally")
ggpairs(iris,  aes(color = Species))

iris_matrix <- iris %>% select(-Species) %>% as.matrix()

iris_long <- as_tibble(iris_matrix) %>% mutate(id = row_number()) %>% pivot_longer(1:4)
head(iris_long)
ggplot(iris_long,
       aes(x = name, y = id, fill = value)) + geom_tile()

library(seriation)
ggpimage(iris_matrix, prop = FALSE)
iris_scaled <- scale(iris_matrix)
ggpimage(iris_scaled, prop = FALSE)
ggpimage(iris_scaled, order = seriate(iris_scaled), prop = FALSE)


# Correlation Matrix
cm1 <- iris %>% select(-Species) %>% as.matrix %>% cor()
cm1
library(ggcorrplot)
ggcorrplot(cm1)
gghmap(cm1, prop = TRUE)
ggcorrplot(cm2)


# Parallel Coordinates Plot
library(GGally)
ggparcoord(iris, columns = 1:4, groupColumn = 5)
o <- seriate(as.dist(1-cor(iris[,1:4])), method = "BBURCG")
get_order(o)
ggparcoord(iris, columns = get_order(o), groupColumn = 5)



### Data Manupulation 2, Introduction to tidyverse, https://kelseyandersen.github.io/NetworksPlantPathology/APS2018_Tidyverse.html
library(tidyverse)
# load data
data(iris)
# provide you the first 6 rows of the data
head(iris)
# check the structure of data. Very handy function to get a basic information about the data.
str(iris)
# create tibble format table
df <- tbl_df(iris) 
df
# Filter rows with filter()
# here df is the object where we had store our tibble data
# yes, you need to use ==
filter(df, Species == "versicolor")
# Comparisons
filter(df, Petal.Length > 2)
# Logical operators
filter(df, Petal.Length > 6 & Sepal.Length > 7)
# arrange by sepal length then petal width. Default is ascending order. 
arrange(df, Sepal.Length, Petal.Width)
# allows to arrange in descending order. 
arrange(df, desc(Sepal.Length))
# from iris data, lets select only three columns - Species, Petal width and Petal length
select(df, Species, Petal.Width, Petal.Length)
mutate(df, log.Sepal.length = log(Sepal.Length))
# find mean of peteal length
summarise(df, mean(Petal.Length))
# find mean of petal length for each species
group_by(df, Species) %>% count(n())
df %>% 
  group_by(Species) %>%
  summarise(mean(Petal.Length))

# 1) Scatter PLot
ggplot(data=df, aes(x = Sepal.Length, y = Sepal.Width))+
  geom_point(aes(color=Species, shape=Species)) +
  xlab("Sepal Length") +
  ylab("Sepal Width") +
  ggtitle("Sepal Length-Width")

# 2) Box Plot

box <- ggplot(data=df, aes(x=Species, y=Sepal.Length))

box + 
  geom_boxplot(aes(fill=Species)) +
  ylab("Sepal Length") +
  ggtitle("Iris Boxplot") +
  stat_summary(fun.y=mean, geom="point", shape=5, size=4)

# 3) Histogram

histogram <- ggplot(data=df, aes(x=Sepal.Width))

histogram +
  geom_histogram(binwidth=0.2, color="black", aes(fill=Species)) +
  xlab("Sepal Width") + 
  ylab("Frequency") + 
  ggtitle("Histogram of Sepal Width")

# 4) bar plot
bar <- ggplot(data=df, aes(x=Species))

bar +
  geom_bar(aes(fill=Species)) + xlab("Species") + 
  ylab("Count") +
  ggtitle("Bar plot of Sepal Length") 

# 5) Faceting
facet <- ggplot(data=df, aes(Sepal.Length, y=Sepal.Width, color=Species)) +
  geom_point(aes(shape=Species), size=1.5) +
  xlab("Sepal Length") +
  ylab("Sepal Width") +
  ggtitle("Faceting") 
# Along columns
facet + facet_grid(. ~ Species)


### ggplot2, https://nbisweden.github.io/workshop-r/2011/lab_ggplot2.html
library(ggplot2)

# Bt species
ggplot(data=iris,mapping=aes(x=Petal.Length,y=Petal.Width))+
  geom_point(aes(color=Species))+
  geom_smooth(method="lm")+
  labs(title="Iris data",subtitle="ggplot2",x=" Petal Length", 
       y="Petal Width", caption="")+
  theme(legend.position="top",
        legend.justification="right")


# By Sepal.Width by Species
ggplot(data=iris,mapping=aes(x=Petal.Length,y=Petal.Width))+
  geom_point(aes(color=Species,size=Sepal.Width))+
  geom_smooth(method="lm")+
  scale_color_manual(values=c("red","blue","green"))+
  labs(title="Iris data",subtitle="ggplot2",x=" Petal Length", 
       y="Petal Width", caption="")+
  theme_bw()+
  theme(legend.position="top",
        legend.justification="right")


# By Sepal.Width
ggplot(data=iris,mapping=aes(x=Petal.Length,y=Petal.Width))+
  geom_point(aes(color=Sepal.Width))+
  geom_smooth(method="lm")+
  scale_x_continuous(breaks=1:8)+
  scale_color_continuous(name="Sepal.Width")+
  labs(title="Iris data",subtitle="ggplot2",x=" Petal Length", 
       y="Petal Width", caption="")+
  theme_bw()+
  theme(legend.position="top",
        legend.justification="right")


# Facet
ggplot(data=iris,mapping=aes(x=Petal.Length,y=Petal.Width))+
  geom_point(aes(color=Sepal.Width))+
  geom_smooth(method="lm")+
  scale_x_continuous(breaks=1:8)+
  scale_color_continuous(name="Sepal.Width")+
  labs(title="Iris data",subtitle="ggplot2",x=" Petal Length", 
       y="Petal Width", caption="") +
  facet_wrap(~Species)+
  theme_bw()+
  theme(legend.position="top",
        legend.justification="right")

### Saving Library State:
renv::snapshot()