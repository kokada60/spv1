library(tidymodels)
set.seed(27)

# Creating 3 centroids for 3-clusters. Their 2-D coordinates are set. Centroids are labeled 1:3, and number of obs for each
# centroids are specified.
centers <- tibble(
  centers = factor(1:3), 
  num_points = c(100, 150, 50), 
  x1 = c(5, 0, 03), 
  x2 = c(-1, 1, -2)
)

# Using the created centroids list, and the the centroid coordinate, will create normally distributed observation of 
# coordinate pairs. 
labelled_points <- 
  centers %>% 
  mutate(
    x1 = map2(num_points, x1, rnorm), 
    x2 = map2(num_points, x2, rnorm)
  ) %>% 
  select( -num_points) %>% 
  unnest(cols=c(x1, x2))

# Generated observations are plotted using "centroid" labels.
ggplot(labelled_points, aes(x=x1, y=x2, color=centers)) + geom_point()

# Build-in kmeans() function will be used on the same generated points groups. 
points_minus_cluster <- labelled_points %>% select(-centers) 
labelled_points

kclust <- kmeans(points_minus_cluster, centers = 3) 
kclust
summary(kclust)

typeof(kclust)

augment(kclust, points_minus_cluster)
tidy(kclust)

kclust$betweenss

kclusts <- tibble(k=1:9) %>% 
  mutate(
    kclust = map(k, ~ kmeans(points_minus_cluster, .x)), 
    tidied = map(kclust, tidy), 
    glanced = map(kclust, glance), 
    augmented = map(kclust, augment, points_minus_cluster) 
  )

clusters <- kclusts %>% unnest(cols = c(tidied))
assignments <- kclusts %>% unnest(cols = c(augmented)) 
clusterings <- kclusts %>% unnest(cols = c(glanced))

clusters # 45. For each k clusted selected... 
assignments   # 2700 = 300 * 9 sets of k-values.., 
clusterings   # 9 clustere.s. 


# now lets plot these clusters facetted by k-values... 
# Also add centroids for each cluster. 
ggplot(assignments, aes(x = x1, y = x2)) + 
  geom_point(aes(color=.cluster), alpha=0.8) + 
  geom_point(data=clusters, size=2, shape=15) + 
  facet_wrap(~ k)

# Estimating the optimal 
ggplot(clusterings, aes(x=k, y=tot.withinss)) + geom_line()+ geom_point() + geom_vline(xintercept=3)







