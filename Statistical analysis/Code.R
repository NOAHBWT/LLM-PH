# 1. Converting Structured Data to Text Data

library(jsonlite)

create_system_prompt <- function() {
  system_prompt <- paste0(
    "Your Role\n",
    "You are a well-versed scholar in the study of pain and physical activity. You excel at prescribing personalized physical activity levels for people by integrating Biological, Psychological, and Social factors. \n\n",
    
    "Background Knowledge\n",
    "Vigorous physical activity refers to any activity lasting at least 10 minutes that makes you feel exerted and causes you to breathe much more heavily than usual. Examples include: running, walking uphill, swimming at a fast pace (excluding leisurely swimming, splashing about or floating), climbing stairs, aerobic exercise, aerobic dance, cycling at a fast pace, vigorous ball sports (such as those dominated by individual skill and involving a great deal of movement), examples include singles tennis, basketball and football, skipping, weight training, carrying heavy objects (over 10 kg), and shovelling mud. \n\n",
    "Moderate physical activity are those that last at least 10 minutes and make you feel slightly out of breath, with your breathing being a little heavier than usual. Examples include: walking downhill, swimming at a moderate pace, going down stairs, dancing (excluding aerobic dance, slow dancing and ballroom dancing), Tai Chi (excluding Wai Dan Gong), cycling at a moderate pace, walking whilst carrying moderately heavy items (such as shopping for groceries, carrying or holding a child); ‘Somewhat heavy’ refers to 4.5 to 9 kg: e.g. 2 reams of A4 paper, 2 family-size cartons of fresh milk (in plastic bottles), 1 mini watermelon, 3 pineapples, 5 kg of rice, 3 bricks, 7 bottles of beer, 24 tins of fizzy drink), tidying the balcony or back garden, strenuous household chores (such as cleaning windows, mopping the floor by hand, making the bed, hand-washing clothes, hand-washing the car), other ball sports, such as doubles tennis, badminton, table tennis, volleyball, baseball. \n\n",
    "Walking (for more than 10 consecutive minutes) includes walking to and from work, at home and when travelling, but does not include climbing stairs or hiking. \n\n",
    "Sitting refers to sitting or reclining whilst working, at home, doing homework or during leisure time. Examples include sitting at a desk, eating meals, using a computer whilst seated, visiting friends, reading, and watching television whilst seated or reclining; however, it does not include sleeping. \n\n",
    
    
    "Your Task\n",
    "It is important to avoid both ‘insufficient physical activity’ and ‘excessive physical activity’ in order to achieve the dual objectives of ‘health promotion’ and ‘pain relief’. Based on the information of a person below, recommend the ideal frequency (days per week) and duration (minutes per day) for three types of activity (Vigorous, Moderate, Walking) and duration (minutes per day) for sitting per day. The format of your answer is JSON. Please do not give any additional output. Please refer to the following format to give your answer:\n",
    "json\n",
    "{\n",
    "\"Inference process1\": string, // Please give your inference process of for recommending ideal level of vigorous physical activity. \n",
    "\"Vigorous physical activity (days_per_week)\": int\n",
    "\"Vigorous physical activity (minutes_on_those_day)\": int\n",
    
    "\"Inference process2\": string, // Please give your inference process of for recommending ideal level of moderate physical activity.\n",
    "\"Moderate physical activity (days_per_week)\": int\n",
    "\"Moderate physical activity (minutes_on_those_day)\": int\n",
    
    "\"Inference process3\": string, // Please give your inference process of for recommending ideal level of walking.\n",
    "\"Walking (days_per_week)\": int\n",
    "\"Walking (minutes_on_those_day)\": int\n",
    
    "\"Inference process4\": string, // Please give your inference process of for recommending ideal level of sitting.\n",
    "\"Sitting (minutes_per_day)\": int\n",
    "}"
  )
  
  return(system_prompt)
}

create_user_prompt <- function(patient_record) {
  # Extract patient information from the data frame row
  prompt <- paste0(
    "Please assess the pain intensity based on the following data:\n\n",
    "【Basic Information】\n",
    "- Age:", patient_record, " years\n",
    "- Gender: ", patient_record, "\n\n",
    "- Occupation: ", patient_record, "\n\n",
    "- Education level: ", patient_record, "\n",
    "【Medical history】\n",
    "- Insomnia: ", patient_record, "\n",
    "- Respiratory diseases: ", patient_record, "\n",
    "- Cardiovascular diseases: ", patient_record, "\n",
    "- Known psychiatric diseases: ", patient_record, "\n",
    "- Cancer: ", patient_record, "\n",
    "【Lifestyle】\n",
    "- Smoking: ", patient_record, "\n",
    "- Dringking: ", patient_record, "\n",
    "【Pain condition】\n",
    "- Self-reported chronic pain: ", patient_record, "\n",
    "- Pain duration: ", patient_record, "\n",
    "【Pain location】\n",
    "- Bone/joint pain: ", patient_record, "\n",
    "- Muscle pain: ", patient_record, "\n",
    "- Headache: ", patient_record, "\n",
    "- Neuralgia: ", patient_record, "\n",
    "- Stomach/belly pain: ", patient_record, "\n",
    "- Back pain: ", patient_record, "\n",
    "- Cardiac pain: ", patient_record, "\n",
    "- Chest pain: ", patient_record, "\n",
    "- Menstrual pain: ", patient_record, "\n",
    "- Toothache: ", patient_record, "\n",
    "- Cheek pain: ", patient_record, "\n",
    "- Leg pain: ", patient_record, "\n",
    "- Arm pain: ", patient_record, "\n",
    "- Skin pain: ", patient_record, "\n",
    "【Family status】\n",
    "Household size:",patient_record$`How many people live in your household in total? Please include yourself. If there are domestic workers in your household, please include them as well.`, "\n",
    "Presence of household members with special care needs:", patient_record, "\n",
    "Household income:", patient_record, " HKD\n",
    "【Personal well being (0 indicates “very dissatisfied” and 10 indicates “very satisfied”.)】\n",
    "Standard of living: ", patient_record, "\n",
    "Personal Health: ", patient_record, "\n",
    "Achieving in Life: ", patient_record, "\n",
    "Personal Relationships: ", patient_record, "\n",
    "Personal Safety: ", patient_record, "\n",
    "Community-Connectedness: ", patient_record, "\n",
    "Future Security: ", patient_record, "\n",
    "【Mental Health】\n",
    "Self-rated mental health: ", patient_record, "\n",
    "Based on the information above, your answer is:"
  )
  
  return(prompt)
}


convert_to_batch_jsonl <- function(patient_df, output_file = "patient_health_batch.jsonl") {

  system_msg <- create_system_prompt()

  json_objects <- list()

  for (i in 1:nrow(patient_df)) {

    patient_record <- patient_df[i, ]

    custom_id <- paste0("patient_", patient_record$SurveyCaseid)

    messages <- list(
      list(
        role = "system",
        content = system_msg
      ),
      list(
        role = "user",
        content = create_user_prompt(patient_record)
      )
    )
    

    json_obj <- list(
      custom_id = custom_id,
      method = "POST",
      url="/v4/chat/completions",
      body = list(
        model = "DSV3.1", 
        messages = messages,
        stream = T,
        temperature = 0,
        max_tokens = 2000
      )
    )
    

    json_objects[[i]] <- json_obj
  }
  
  json_lines <- sapply(json_objects, function(obj) {
    toJSON(obj, auto_unbox = TRUE, ensure_ascii = FALSE)
  })

  writeLines(json_lines, output_file)
  
  cat("Conversion Successful", length(json_objects), "patient record in JSONL format\n")
  cat("Output File：", output_file, "\n")
  
  return(invisible(json_objects))
}



# Install and load necessary packages (if not already installed)
# install.packages(c("ggplot2", "cluster", "factoextra", "rms", "segmented"))
library(rms)
library(segmented)
library(cluster)
library(factoextra)
library(ggplot2)

# 2. Restricted Cubic Splines (RCS)
 
# Fit the RCS model
# 'ddist' is used to describe the distribution of variables for 'rcs' function
ddist <- datadist(data_rcs$deviation_variable)
options(datadist = "ddist")
 
# Fit the model using ols (ordinary least squares) for continuous outcome
rcs_model <- ols(pain_intensity ~ rcs(deviation_variable, 3), data = data_rcs)
 
# Summarize the model (includes likelihood ratio test for non-linearity)
print(rcs_model)

# 3. Caculate the slope of each side
 
# This step is performed for deviation dimensions exhibiting clear inflection points in RCS curves.
# First, identify the turning point from the RCS curve (e.g., visually or programmatically finding the minimum/maximum).

pred_df <- as.data.frame(Predict(fit, deviation_variable))

min_row <- pred_df[which.min(pred_df$yhat), ]
turning_point <- round(min_row$deviation_variable, 1) 
y_min <- round(min_row$yhat, 1)

deviation_variable_min <- min(data$deviation_variable, na.rm = TRUE)
deviation_variable_max <- max(data$deviation_variable, na.rm = TRUE)

contrast_left <- contrast(fit, list(deviation_variable = turning_point), list(deviation_variable = deviation_variable_min))
slope_left_1k <- round((contrast_left$Contrast / (turning_point - deviation_variable_min)*1000), 3)
p_left <- contrast_left$Pvalue

contrast_right <- contrast(fit, list(deviation_variable = deviation_variable_max), list(deviation_variable = turning_point))
slope_right_1k <- round((contrast_right$Contrast / (deviation_variable_max - turning_point)*1000), 3)
p_right <- contrast_right$Pvalue

y_start <- pred_df$yhat[which.min(abs(pred_df$deviation_variable - deviation_variable_min))]
y_end   <- pred_df$yhat[which.min(abs(pred_df$deviation_variable - deviation_variable_max))]

# 3.plot
raw_breaks <- pretty(pred_df$yhat)
my_y_breaks <- sort(unique(c(raw_breaks[raw_breaks == floor(raw_breaks)], y_min)))

ggplot(pred_df, aes(x = deviation_variable, y = yhat)) +
  
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#1f77b4", alpha = 0.15) +
  geom_line(color = "#1f77b4", linewidth = 1) +
  
  annotate("segment", x = deviation_variable_min, y = y_start, xend = turning_point, yend = y_min, color = "grey", linetype = "longdash", linewidth = 0.8) +
  annotate("segment", x = turning_point, y = y_min, xend = deviation_variable_max, yend = y_end, color = "grey", linetype = "longdash",linewidth = 0.8) +
  
  annotate("text", x = (deviation_variable_min + turning_point)/2, y = max(pred_df$yhat) * 0.9, 
           label = paste0("Slope: ", slope_left_1k, "\n(per 1k unit)\nP ", ifelse(p_left < 0.001, "< 0.001", paste0("= ", round(p_left, 3)))),
           color = "#1f77b4", size = 3.5, fontface = "bold", hjust = 0.5) +
  
  annotate("text", x = (turning_point + deviation_variable_max)/2, y = max(pred_df$yhat) * 0.9, 
           label = paste0("Slope: +", slope_right_1k, "\n(per 1k unit)\nP ", ifelse(p_right < 0.001, "< 0.001", paste0("= ", round(p_right, 3)))),
           color = "#1f77b4", size = 3.5, fontface = "bold", hjust = 0.5) +
  
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_vline(xintercept = turning_point, linetype = "dotted", color = "purple4", linewidth = 1) +
  geom_hline(yintercept = y_min, linetype = "dotted", color = "purple4", linewidth = 1) +
  
  geom_rug(data = data, aes(x = deviation_variable), inherit.aes = FALSE, alpha = 0.25, sides = "b", color = "gray40") +
  annotate("text", x = -Inf, y = -Inf, label = "  ← Under-activity", hjust = -0.5, vjust = -2, size = 3.5, color = "gray50", fontface = "italic") +
  annotate("text", x = Inf, y = -Inf, label = "Over-activity →  ", hjust = 4.5, vjust = -2, size = 3.5, color = "gray50", fontface = "italic") +
  
  theme_classic(base_size = 14) +
  labs(
    x = "Deviation", 
    y = "Pain Intensity (NRS)",
    title = "PA"
  ) +
  
scale_x_continuous(breaks = my_x_breaks) +
  scale_y_continuous(breaks = my_y_breaks) +
  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 12))
  )

# 4. K-means Clustering and Silhouette Coefficient
#    Packages: stats (built-in), cluster, factoextra (for visualization)
 
# Placeholder for your four-dimensional deviation data (Delta_V, Delta_M, Delta_W, Delta_S)
# Assuming 'deviation_data' is a data frame with these four columns.
# For demonstration, we create some dummy data.
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
 
