# Generate dataset
set.seed(4)
rndm_data <- rbind(matrix(rnorm(20*50, mean = 0), nrow = 20), 
           matrix(rnorm(20*50, mean=0.7), nrow = 20), 
           matrix(rnorm(20*50, mean=1.4), nrow = 20))
head(rndm_data)

# part a: Apply PCA
rndm_data.pca = prcomp(rndm_data)$x
plot(rndm_data.pca[,1:2], col=c(rep(1,20), rep(2,20), rep(3,20)),pch =19, xlab ="1st principal component", ylab="2nd principal component")

# part b: Run k-means witk k=3 
res = kmeans(rndm_data, centers = 3)
true_class = c(rep(1,20), rep(2,20), rep(3,20)) 
table(res$cluster, true_class)

# part c: Run k-means with k=4 and k=2
res = kmeans(rndm_data, centers = 2)
true = c(rep(1,20), rep(2,20), rep(3,20)) 
table(res$cluster, true_class)

res = kmeans(rndm_data, centers = 4)
true = c(rep(1,20), rep(2,20), rep(3,20)) 
table(res$cluster, true_class)

# part d: Apply kmeans with k=3 and PCA
res = kmeans(rndm_data.pca[,1:2], centers = 3) 
true = c(rep(1,20), rep(2,20), rep(3,20)) 
table(res$cluster, true_class)

# part e: Apply kmeans with k=3 and normalization
res = kmeans(scale(rndm_data), centers = 3)
true = c(rep(1,20), rep(2,20), rep(3,20)) 
table(res$cluster, true_class)
