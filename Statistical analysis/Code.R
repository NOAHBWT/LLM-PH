install.packages(c("tm", "wordcloud", "RColorBrewer", "rms", "segmented", "cluster", "factoextra"))
# R Code for SCI Paper - Code Availability Section
 
# This script provides R code snippets for the statistical analyses described in the paper.
# Please replace placeholder data with your actual dataset and adjust variable names as needed.
 
# Install and load necessary packages (if not already installed)
# install.packages(c("tm", "wordcloud", "RColorBrewer", "ggplot2", "cluster", "factoextra", "rms", "segmented"))
library(tm)
library(wordcloud)
library(RColorBrewer)
library(rms)
library(segmented)
library(cluster)
library(factoextra)
 
# 1. Term Frequency Analysis and Word Clouds
#    Packages: tm, wordcloud, RColorBrewer
 
# Placeholder for your text data (e.g., LLM inference keywords)
# Assuming 'keywords_data' is a character vector or a data frame column containing the keywords.
# Example: keywords_data <- c("medical history", "cardiovascular status", "chronic pain", "age", "insomnia", "mental health", "community connectedness", "standard of living", "full-time employment")
# For demonstration, let's create some dummy data
set.seed(123)
keywords_data <- sample(c("medical history", "cardiovascular status", "chronic pain", "age", "insomnia", "mental health", "community connectedness", "standard of living", "full-time employment", "pain severity", "physical activity", "sedentary behavior", "obesity", "diabetes", "hypertension"), 500, replace = TRUE, prob = c(0.1, 0.08, 0.12, 0.07, 0.05, 0.09, 0.06, 0.04, 0.08, 0.07, 0.06, 0.05, 0.04, 0.03, 0.06))
 
# Create a text corpus
corpus <- Corpus(VectorSource(keywords_data))
 
# Clean the corpus (optional, depending on data)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)
 
# Create a term-document matrix
tdm <- TermDocumentMatrix(corpus)
matrix <- as.matrix(tdm)
words <- sort(rowSums(matrix), decreasing = TRUE)
df <- data.frame(word = names(words), freq = words)
 
# Generate word cloud
# You might need to adjust min.freq and max.words based on your data
# For better visualization, consider using a larger plotting device or saving to a file.
# For example: png("wordcloud.png", width=800, height=600)
wordcloud(words = df$word, freq = df$freq, min.freq = 1, max.words = 100,
          random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2"))
# dev.off() # Close the plotting device if you saved to a file
 
 
# 2. Restricted Cubic Splines (RCS)
#    Package: rms
 
# Placeholder for your data (replace with your actual data loading)
# Assuming 'data' is a data frame with 'pain_intensity' (NRS) and 'deviation_variable' (e.g., Delta_V, GVD)
# For demonstration, let's create some dummy data
set.seed(456)
data_rcs <- data.frame(
  pain_intensity = rnorm(200, mean = 5, sd = 2) + rep(c(0, 1, 0, -1, 0), each = 40) * seq(-5, 5, length.out = 200)^2 / 25,
  deviation_variable = rnorm(200, mean = 0, sd = 2)
)
# Ensure pain_intensity is non-negative and within a reasonable range for NRS (e.g., 0-10)
data_rcs$pain_intensity <- pmax(0, pmin(10, data_rcs$pain_intensity))
 
# Define the number of knots and their positions (e.g., 5th, 35th, 65th, 95th percentiles)
# The 'rcs' function in 'rms' package automatically places knots at specified quantiles.
# Here, nk=4 means 3 knots (at 35th, 65th, 95th percentiles) plus the boundary knots (5th, 95th percentiles)
# The paper specifies knots at 5th, 35th, 65th, and 95th percentiles. For `rcs` function, 
# you specify the number of knots `nk`. If `nk=4`, it places knots at 0.05, 0.35, 0.65, 0.95 quantiles.
# If you want to explicitly define the quantiles, you can use `quantile(data_rcs$deviation_variable, c(0.05, 0.35, 0.65, 0.95))`
 
# Fit the RCS model
# 'ddist' is used to describe the distribution of variables for 'rcs' function
ddist <- datadist(data_rcs$deviation_variable)
options(datadist = "ddist")
 
# Fit the model using ols (ordinary least squares) for continuous outcome
# For other outcome types (e.g., binary), you might use `lrm` (logistic regression) or `cph` (Cox proportional hazards)
rcs_model <- ols(pain_intensity ~ rcs(deviation_variable, 4), data = data_rcs)
 
# Summarize the model (includes likelihood ratio test for non-linearity)
print(rcs_model)
 
# Plot the RCS curve
# You might need to adjust the range for prediction to cover your data's full range
plot(Predict(rcs_model, deviation_variable, fun = exp), xlab = "Physical Activity Deviation", ylab = "Pain Intensity (NRS)",
     main = "Restricted Cubic Spline of Pain Intensity vs. Deviation")
 
# Likelihood ratio test for non-linearity (already part of `print(rcs_model)` output)
# You can also extract it more directly if needed, but the `rms` package output is comprehensive.
 
 
# 3. Segmented Linear Regression
#    Package: segmented
 
# This step is performed for deviation dimensions exhibiting clear inflection points in RCS curves.
# First, identify the turning point from the RCS curve (e.g., visually or programmatically finding the minimum/maximum).
# For demonstration, let's assume a turning point at `deviation_variable = 0.5`.
 
# Fit a linear model first
linear_model <- lm(pain_intensity ~ deviation_variable, data = data_rcs)
 
# Fit segmented regression model
# You need to specify the `psi` argument, which is the initial guess for the breakpoint(s).
# The paper mentions identifying turning points from the RCS curve. Let's assume we found one.
# For demonstration, let's pick a breakpoint near the minimum of the dummy RCS curve.
# A more robust approach would be to find the minimum of the predicted values from the RCS model.
 
# Example of finding a turning point from RCS prediction (conceptual)
# pred_rcs <- Predict(rcs_model, deviation_variable=seq(min(data_rcs$deviation_variable), max(data_rcs$deviation_variable), length.out=100))
# turning_point_val <- pred_rcs$deviation_variable[which.min(pred_rcs$yhat)]
# For simplicity, let's use a fixed value for demonstration.
turning_point_val <- 0.5 # Replace with actual turning point identified from RCS
 
segmented_model <- segmented(linear_model, seg.Z = ~deviation_variable, psi = list(deviation_variable = turning_point_val))
 
# Summarize the segmented model
summary(segmented_model)
 
# Plot the segmented regression
plot(data_rcs$deviation_variable, data_rcs$pain_intensity, pch = 16, cex = 0.8, xlab = "Physical Activity Deviation", ylab = "Pain Intensity (NRS)")
plot(segmented_model, add = TRUE, col = "red", lwd = 2)
 
 
# 4. K-means Clustering and Silhouette Coefficient
#    Packages: stats (built-in), cluster, factoextra (for visualization)
 
# Placeholder for your four-dimensional deviation data (Delta_V, Delta_M, Delta_W, Delta_S)
# Assuming 'deviation_data' is a data frame with these four columns.
# For demonstration, let's create some dummy data.
set.seed(789)
deviation_data <- data.frame(
  Delta_V = rnorm(200, mean = 0, sd = 1.5),
  Delta_M = rnorm(200, mean = 0, sd = 1.2),
  Delta_W = rnorm(200, mean = 0, sd = 1.0),
  Delta_S = rnorm(200, mean = 0, sd = 0.8)
)
 
# Scale the data (important for clustering)
deviation_scaled <- scale(deviation_data)
 
# Determine optimal number of clusters using Silhouette coefficient
# The paper mentions evaluating Silhouette coefficient across K values.
# Let's test K from 2 to 10.
 
silhouette_scores <- c()
for (k in 2:10) {
  km.res <- kmeans(deviation_scaled, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(deviation_scaled))
  silhouette_scores <- c(silhouette_scores, mean(ss[, 3]))
}
 
# Plot silhouette scores to find optimal K
plot(2:10, silhouette_scores, type = "b", xlab = "Number of clusters (K)", ylab = "Average Silhouette Width",
     main = "Optimal K using Silhouette Method")
optimal_k <- which.max(silhouette_scores) + 1 # +1 because K starts from 2
cat("Optimal number of clusters (K) based on Silhouette coefficient: ", optimal_k, "\n")
 
# Perform K-means clustering with the optimal K
set.seed(123) # for reproducibility
final_kmeans_model <- kmeans(deviation_scaled, centers = optimal_k, nstart = 25)
 
# Add cluster assignments to the original data
deviation_data$cluster <- as.factor(final_kmeans_model$cluster)
 
# Visualize the clusters (e.g., using PCA for 2D visualization)
fviz_cluster(final_kmeans_model, data = deviation_scaled, geom = "point",
             main = paste0("K-means Clustering (K=", optimal_k, ")"))
 
 
# 5. One-way Analysis of Variance (ANOVA)
#    Package: stats (built-in)
 
# Assuming 'pain_intensity' is available in the data frame with cluster assignments.
# For demonstration, let's add pain intensity to the deviation_data_rcs for consistency.
# In a real scenario, you would merge your pain intensity data with the cluster assignments.
 
# Let's use the pain_intensity from data_rcs and assign clusters to it for demonstration.
# This is a simplified example; ensure your actual data alignment is correct.
combined_data <- data.frame(
  pain_intensity = data_rcs$pain_intensity,
  cluster = as.factor(final_kmeans_model$cluster[1:nrow(data_rcs)]) # Adjust indexing if data sizes differ
)
 
# Perform one-way ANOVA
anova_result <- aov(pain_intensity ~ cluster, data = combined_data)
 
# Summarize ANOVA results
summary(anova_result)
 
# Post-hoc tests (e.g., Tukey HSD) if ANOVA is significant
# TukeyHSD(anova_result)
 
# Boxplot to visualize differences
boxplot(pain_intensity ~ cluster, data = combined_data, 
        xlab = "Cluster", ylab = "Pain Intensity (NRS)",
        main = "Pain Intensity by K-means Cluster")
 
