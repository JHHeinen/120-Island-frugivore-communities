# Null models

# Install and load packages
sapply(c("vegan","ape","StatMatch","MASS"), require, character.only=TRUE)

# Load data
fauna <- read.csv("Fauna_PNAS.csv", stringsAsFactors=FALSE, fileEncoding="UTF-8")

# Set species names as row names
fauna <- as.data.frame(fauna, row.names=fauna$Species)

# Keep only the trait columns and log‐transform
data <- fauna[,c("Fruit.access","Fruit.in.diet","Diet","Seed.handling",
                 "Gape.width","Class","Body.mass")]
data[] <- lapply(data, function(x) if(is.character(x)) factor(x) else x)
data$Gape.width   <- log10(data$Gape.width)
data$Body.mass    <- log10(data$Body.mass)
data$Fruit.percent<- log10(data$Fruit.in.diet)
str(data)

# Transform categorical data to binary columns per category
dummy <- cbind(
  to.dummy(data$Fruit.access,   "Fruit.access"),
  to.dummy(data$Diet,           "Diet"),
  to.dummy(data$Seed.handling,  "Seed.handling"),
  to.dummy(data$Class,          "Class")
)
dummy <- as.data.frame(dummy, row.names=rownames(data))
dummy$Gape.width    <- data$Gape.width
dummy$Body.mass     <- data$Body.mass
dummy$Fruit.percent <- data$Fruit.percent
data <- dummy
str(data)

# Calculate weighted Gower distances and PCoA 
# (can take a bit of time to run)
weights     <- c(rep(1/3,12),1,1,1)
gowdist     <- gower.dist(data, var.weights=weights)
pcoa.result <- pcoa(gowdist)

# Build the pcoa.all / before / after objects
pcoa.axis <- as.data.frame(pcoa.result$vectors[,1:2])
colnames(pcoa.axis) <- c("Axis.1","Axis.2")
rownames(pcoa.axis) <- rownames(data)

pcoa.all <- cbind(pcoa.axis,AlienNativeExtinct=fauna$AlienNativeExtinct,Archip=fauna$Archipelago)
pcoa.before <- subset(pcoa.all, AlienNativeExtinct!="Alien")
pcoa.after  <- subset(pcoa.all, AlienNativeExtinct!="Extinct")

pcoa.all[] <- lapply(pcoa.all, function(x) if(is.character(x)) factor(x) else x)
pcoa.before[] <- lapply(pcoa.before, function(x) if(is.character(x)) factor(x) else x)
pcoa.after[] <- lapply(pcoa.after, function(x) if(is.character(x)) factor(x) else x)


#### Create function for null model for each archipelago. randomly changing the labels (extinct, native, alien). ####

run_label_randomization_null_model_archipelagos <- function(pcoa_data, n_permutations = 1000, plot_results = TRUE) {
  
  centroid_distance <- function(df1, df2) {
    if (nrow(df1) == 0 | nrow(df2) == 0) return(NA)
    c1 <- colMeans(df1[, c("Axis.1", "Axis.2")])
    c2 <- colMeans(df2[, c("Axis.1", "Axis.2")])
    sqrt(sum((c1 - c2)^2))
  }
  
  archipelagos <- unique(pcoa_data$Archip)
  summary_list <- list()
  plot_list <- list()
  
  for (arch in archipelagos) {
    message("Processing: ", arch)
    
    arch_data <- subset(pcoa_data, Archip == arch)
    
    label_counts <- table(arch_data$AlienNativeExtinct)
    n_native <- ifelse("Native" %in% names(label_counts), label_counts["Native"], 0)
    n_extinct <- ifelse("Extinct" %in% names(label_counts), label_counts["Extinct"], 0)
    n_alien <- ifelse("Alien" %in% names(label_counts), label_counts["Alien"], 0)
    
    if (n_native == 0 | (n_extinct == 0 & n_alien == 0)) {
      warning(paste("Skipping", arch, "- insufficient data"))
      next
    }
    
    native_ids <- rownames(subset(arch_data, AlienNativeExtinct == "Native"))
    extinct_ids <- rownames(subset(arch_data, AlienNativeExtinct == "Extinct"))
    alien_ids <- rownames(subset(arch_data, AlienNativeExtinct == "Alien"))
    
    obs_before <- pcoa_data[rownames(pcoa_data) %in% c(native_ids, extinct_ids), ]
    obs_after  <- pcoa_data[rownames(pcoa_data) %in% c(native_ids, alien_ids), ]
    
    obs_shift <- centroid_distance(obs_before, obs_after)
    null_dists <- numeric(n_permutations)
    
    species_ids <- rownames(arch_data)
    
    for (i in 1:n_permutations) {
      shuffled_labels <- sample(c(rep("Native", n_native), 
                                  rep("Extinct", n_extinct), 
                                  rep("Alien", n_alien)))
      names(shuffled_labels) <- sample(species_ids)  # randomize which species get each label
      
      before_sim_ids <- names(shuffled_labels)[shuffled_labels %in% c("Native", "Extinct")]
      after_sim_ids  <- names(shuffled_labels)[shuffled_labels %in% c("Native", "Alien")]
      
      before_sim <- pcoa_data[rownames(pcoa_data) %in% before_sim_ids, ]
      after_sim  <- pcoa_data[rownames(pcoa_data) %in% after_sim_ids, ]
      
      null_dists[i] <- centroid_distance(before_sim, after_sim)
    }
    
    p_val <- mean(null_dists >= obs_shift)
    
    summary_list[[arch]] <- data.frame(
      Archipelago = arch,
      n_native = n_native,
      n_extinct = n_extinct,
      n_alien = n_alien,
      ObservedShift = obs_shift,
      NullMean = mean(null_dists),
      NullSD = sd(null_dists),
      P_value = p_val
    )
    
    if (plot_results) {
      plot_obj <- ggplot(data.frame(null_dists), aes(null_dists)) +
        geom_histogram(bins = 30, fill = "lightblue", color = "gray30") +
        geom_vline(xintercept = obs_shift, color = "red", linetype = "dashed", size = 1) +
        labs(title = paste("Label-randomization null model -", arch),
             x = "Centroid distance", y = "Frequency") +
        annotate("text", x = obs_shift, y = max(table(cut(null_dists, 30))), 
                 label = paste0("Observed = ", round(obs_shift, 3)), hjust = -0.1, col = "red") +
        theme_classic()
      
      plot_list[[arch]] <- plot_obj
    }
  }
  
  results_df <- do.call(rbind, summary_list)
  return(list(summary = results_df, plots = plot_list))
}

# Run the null model
label_random_results <- run_label_randomization_null_model_archipelagos(pcoa.all, n_permutations = 1000)

# Prepare summary table from label randomization null model
summary_df <- label_random_results$summary
summary_df$Significance <- ifelse(summary_df$P_value < 0.05, "Non-random", "Random")

# Table S3
summary_df

# Prepare plotting data
plot_data <- do.call(rbind, lapply(names(label_random_results$plots), function(arch) {
  df <- data.frame(null_dists = label_random_results$plots[[arch]]$data$null_dists)
  df$Archipelago <- arch
  df }))

# Merge with summary for plotting
summary_for_plot <- summary_df[, c("Archipelago", "ObservedShift", "NullMean", "n_native", "n_extinct", "n_alien", "P_value", "Significance")]
plot_data <- merge(plot_data, summary_for_plot, by = "Archipelago")

# Calculate max histogram bar height per panel to position text
hist_heights <- plot_data |>
  group_by(Archipelago) |>
  summarise(max_y = max(tabulate(cut(null_dists, breaks = 30))))
plot_data <- merge(plot_data, hist_heights, by = "Archipelago")

# Create Figure S3
figureS3 <- ggplot(plot_data, aes(x = null_dists)) +
  geom_histogram(bins = 30, fill = "grey70", color = "black", size = 0.2) +
  geom_vline(aes(xintercept = ObservedShift), color = "blue", linetype = "dashed", linewidth = 0.5) +
  geom_vline(aes(xintercept = NullMean), color = "grey30", linetype = "solid", linewidth = 0.5) +
  facet_wrap(~ Archipelago, ncol = 5, labeller = labeller(Archipelago = label_wrap_gen(20))) +
  labs(x = "Centroid shift distance", y = "Frequency",
       title = "Label randomization null model of centroid shifts per archipelago") +
  theme_classic(base_size = 10) +
  theme(
    strip.text = element_text(size = 9, face = "plain"),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.line = element_line(size = 0.2),
    panel.spacing = unit(0.6, "lines"),
    strip.background = element_rect(fill = NA, color = "black", size = 0.2),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
  ) +
  geom_text(
    aes(
      x = Inf,
      y = Inf,
      label = paste0("obs=", round(ObservedShift, 3), 
                     "\nnull=", round(NullMean, 3),
                     "\np=", formatC(P_value, digits = 4, format = "f"),
                     "\n", Significance)
    ),
    hjust = 1.05, vjust = 1.5, size = 2.5, fontface = "plain"
  )

# Show plot
figureS3

# Save plot
CairoPDF("Figure S3.pdf", width = 12, height = 10)
figureS3
dev.off()


#### Create function for null model for whole dataset. randomly changing the labels (extinct, native, alien).####

run_label_randomization_global <- function(pcoa_data, n_permutations = 1000, seed = 42, plot_results = TRUE) {
  set.seed(seed)
  
  centroid_distance <- function(df1, df2) {
    if (nrow(df1) == 0 | nrow(df2) == 0) return(NA)
    c1 <- colMeans(df1[, c("Axis.1", "Axis.2")])
    c2 <- colMeans(df2[, c("Axis.1", "Axis.2")])
    sqrt(sum((c1 - c2)^2))
  }
  
  # Count original labels
  label_counts <- table(pcoa_data$AlienNativeExtinct)
  n_native  <- ifelse("Native"  %in% names(label_counts), label_counts["Native"], 0)
  n_extinct <- ifelse("Extinct" %in% names(label_counts), label_counts["Extinct"], 0)
  n_alien   <- ifelse("Alien"   %in% names(label_counts), label_counts["Alien"], 0)
  
  # Observed shift
  native_ids  <- rownames(pcoa_data[pcoa_data$AlienNativeExtinct == "Native", ])
  extinct_ids <- rownames(pcoa_data[pcoa_data$AlienNativeExtinct == "Extinct", ])
  alien_ids   <- rownames(pcoa_data[pcoa_data$AlienNativeExtinct == "Alien", ])
  
  obs_before <- pcoa_data[rownames(pcoa_data) %in% c(native_ids, extinct_ids), ]
  obs_after  <- pcoa_data[rownames(pcoa_data) %in% c(native_ids, alien_ids), ]
  obs_shift  <- centroid_distance(obs_before, obs_after)
  
  # Null model
  species_ids <- rownames(pcoa_data)
  null_dists <- numeric(n_permutations)
  
  for (i in 1:n_permutations) {
    # Random label assignment to same species
    new_labels <- sample(c(rep("Native", n_native), rep("Extinct", n_extinct), rep("Alien", n_alien)))
    df_sim <- pcoa_data
    df_sim$rand_label <- new_labels
    
    before_sim <- df_sim[df_sim$rand_label %in% c("Native", "Extinct"), ]
    after_sim  <- df_sim[df_sim$rand_label %in% c("Native", "Alien"), ]
    
    null_dists[i] <- centroid_distance(before_sim, after_sim)
  }
  
  null_mean <- mean(null_dists)
  p_val <- mean(null_dists >= obs_shift)
  
# Plot
if (plot_results) {
  library(ggplot2)
  library(Cairo)
  
  hist_df <- data.frame(null_dists = null_dists)
  max_y <- max(hist(hist_df$null_dists, breaks = 30, plot = FALSE)$counts)
  
  p <- ggplot(hist_df, aes(null_dists)) +
    geom_histogram(bins = 30, fill = "grey70", color = "black", size = 0.2) +
    geom_vline(xintercept = obs_shift, color = "blue", linetype = "dashed", linewidth = 0.5) +
    geom_vline(xintercept = null_mean, color = "grey30", linetype = "solid", linewidth = 0.5) +
    annotate("text", 
             x = Inf, y = Inf,
             hjust = 1.95, vjust = 1.5, size = 3.5, fontface = "plain",
             label = paste0("obs=", round(obs_shift, 3),
                            "\nnull=", round(null_mean, 3),
                            "\np=", formatC(p_val, digits = 4, format = "f"),
                            "\n", ifelse(p_val < 0.05, "Non-random", "Random"))) +
    labs(title = "All archipelagos",
         x = "Centroid shift distance", y = "Frequency") +
    theme_classic(base_size = 10) +
    theme(
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8),
      axis.line = element_line(size = 0.2),
      panel.spacing = unit(0.6, "lines"),
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      plot.background = element_rect(color = "black", fill = NA, size = 0.2)
    )
}

  # return summary and plot
    return(list(
            summary    = data.frame(
            ObservedShift = obs_shift,
            NullMean      = null_mean,
            NullSD        = sd(null_dists),
            P_value       = p_val,
            Significance  = ifelse(p_val<0.05,"Non-random","Random")
          ),
        null_dists = null_dists,
        plot       = p
      ))

}

# Run function
global_label_null <- run_label_randomization_global(pcoa.all, n_permutations = 1000)

# Show results
print(global_label_null)
global_label_null$summary

# Figure S2
print(global_label_null$plot)

# Save plot
CairoPDF("Figure S2.pdf", width = 6, height = 5)
print(global_label_null$plot)
dev.off()

##############
