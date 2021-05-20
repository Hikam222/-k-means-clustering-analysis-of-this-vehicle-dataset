library(pacman) 
pacman::p_load(tidyverse, readxl, NbClust, knitr, tidymodels,flexclust, funtimes, cluster, factoextra) 
theme_set(theme_light())
# Read in the original excel datafile
vehicles_original <- read_excel("C:\\Users\\Hikam\\Desktop\\Machine Learning Module\\CW\\vehicles.xlsx") %>%
  janitor::clean_names() %>%
  #https://www.rdocumentation.org/packages/janitor/versions/1.2.0/topics/clean_names
  mutate(class = as_factor(class))
# Get a birds eye view of how the dataset looks like
summary(vehicles_original)
vehicles_original %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "van") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Outlier Detection for class: 'van'")
vehicles_original %>%
  pivot_longer(2:19,names_to = "labels") %>% filter(class == "bus") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Outlier Detection for class: 'bus'")
vehicles_original %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "saab") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Outlier Detection for class: saab")
vehicles_original %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "opel") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Outlier Detection for class: opel")
vehicles_bus = vehicles_original %>%
  filter(class == "bus") %>% mutate(across(2:19, ~squish(.x, quantile(.x, c(.12, .73)))))
vehicles_van = vehicles_original %>%
  filter(class == "van") %>%
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.20, .82)))))
vehicles_opel = vehicles_original %>%
  filter(class == "opel") %>%
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.20, .83)))))
vehicles_saab = vehicles_original %>%
  filter(class == "saab") %>%
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.20, .83)))))
combined = bind_rows(list(vehicles_bus,vehicles_opel,vehicles_saab,vehicles_van)) %>%
  arrange(samples)
summary(combined)
combined %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "bus") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Transformed Outliers class: 'bus'")
combined %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "van") %>% mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Transformed Outliers class: 'van'")
combined %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "saab") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Transformed Outliers for class: saab")
combined %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "opel") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Transformed Outliers for class: opel")
# Remove the sample name and the class name. Both of these will be remove so that only 
# numerical data is left for the algorithm.
vehicles_data_points = combined %>%
  select(-samples, -class)# Now that we have the "vehicles_data_points" dataset, scaling is performed
vehicles_scaled = vehicles_data_points %>%
  mutate(across(everything(), scale))
set.seed(123)
# Perform the kmeans using the NbClust function
# Use Euclidean for distance
cluster_euclidean = NbClust(vehicles_scaled,distance="euclidean", 
                            min.nc=2,max.nc=10,method="kmeans",index="all")
# Use manhattan for distance
cluster_manhattan = NbClust(vehicles_scaled,distance="manhattan", 
                            min.nc=2,max.nc=15,method="kmeans",index="all")
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}
wssplot(vehicles_scaled)set.seed(123)
fit.km <- kmeans(vehicles_scaled, centers=2, nstart=25)
fit.km
table(fit.km$cluster,combined$class)
fit.km$centers
plot(vehicles_scaled[c("comp", "circ")], col=fit.km$cluster)
points(fit.km$centers[,c("comp", "circ")], col=1:3, pch=23, cex=3)
set.seed(123)
fit.km <- kmeans(vehicles_scaled, centers=3, nstart=25)
fit.km$size
table(fit.km$cluster,combined$class)
fit.km$centers
plot(vehicles_scaled[c("comp", "circ")], col=fit.km$cluster)
points(fit.km$centers[,c("comp", "circ")], col=1:3, pch=23, cex=3)
set.seed(123)
fit.km <- kmeans(vehicles_scaled, centers=4, nstart=25)
fit.km$size
table(fit.km$cluster,combined$class)
fit.km$centers
plot(vehicles_scaled[c("ra_gyr", "holl_ra")], col=fit.km$cluster)
points(fit.km$centers[,c("ra_gyr", "holl_ra")], col=1:3, pch=23, cex=3)set.seed(123)
fit.km <- kmeans(vehicles_scaled, centers=8, nstart=25)
fit.km$size
table(fit.km$cluster,combined$class)
fit.km$centers
plot(vehicles_scaled[c("ra_gyr", "holl_ra")], col=fit.km$cluster)
points(fit.km$centers[,c("ra_gyr", "holl_ra")], col=1:3, pch=23, cex=3)