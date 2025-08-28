# Figure 4 Gape width, Body mass, extinction
# Figure S5, Tables S2,S4,S5, Anova and Tukey tests

# load packages
sapply(c("ggplot2", "lme4", "varhandle", "grid","gridExtra","Cairo", "performance"), require, character.only=TRUE)
# Note: On macOS, Cairo requires XQuartz: https://www.xquartz.org/

# Load data
fauna <- read.csv("Fauna_PNAS.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8")
str(fauna)

# Convert character columns to factor
fauna[] <- lapply(fauna, function(x) if(is.character(x)) factor(x) else x)
str(fauna)

#### Figure 4A ####

# Only unique species for Figure 4a
fauna.u <- fauna[, c("Species", "Family", "Class", "Body.mass", "Gape.width")]
fauna.u <- unique(fauna.u)
str(fauna.u)
length(rownames(fauna.u)) # needs to be unique species list n=612

# Models for figure 4a (Information for Table S4)
m1 <- lm(log10(fauna.u$Body.mass)~log10(fauna.u$Gape.width))
summary(m1) 
m.2 <- lm(log10(fauna.u$Body.mass[fauna.u$Class=="Aves"])~log10(fauna.u$Gape.width[fauna.u$Class=="Aves"]))
summary(m.2)
m.3 <- lm(log10(fauna.u$Body.mass[fauna.u$Class=="Mammalia"])~log10(fauna.u$Gape.width[fauna.u$Class=="Mammalia"]))
summary(m.3)
m.4 <- lm(log10(fauna.u$Body.mass[fauna.u$Class=="Reptilia"])~log10(fauna.u$Gape.width[fauna.u$Class=="Reptilia"]))
summary(m.4)

# Model assumptions
par(mfrow=c(2,2))
hist(resid(m1))
hist(resid(m.2))
hist(resid(m.3))
hist(resid(m.4))
plot(m1, main="m1")
plot(m.2, main="m.2")
plot(m.3, main="m.3")
plot(m.4, main="m.4")
par(mfrow=c(1,1))

# Column with three categories to one binary column per category
class <- to.dummy(fauna.u$Class, prefix="class") 

# Figure 4a
figure4A <- ggplot(fauna.u, aes(x = log10(Body.mass), 
                         y = log10(Gape.width), 
                         colour = Class)) +
  geom_point(size = 6) +
  scale_shape_manual(values = c(17, 19)) +
  scale_colour_manual(values = c("#666666", "#7570B3", "#D95F02")) + 
  stat_smooth(data = subset(fauna.u, Class == "Aves"), 
              aes(x = log10(Body.mass), y = log10(Gape.width)), 
              method = "lm", size = 2, color = "#4D4D4D", se = TRUE) + # Darker contrast grey line for birds
  stat_smooth(data = subset(fauna.u, Class != "Aves"), 
              aes(x = log10(Body.mass), y = log10(Gape.width), colour = Class), 
              method = "lm", size = 2, se = TRUE, show.legend = FALSE) + # Mammals and reptiles
  stat_smooth(aes(group = 1), method = "lm", size = 2, col = "black", se = TRUE) + # Overall regression line
  labs(fill = NULL, 
       title = "a. Gape size and body mass") +
  xlab("Body mass (g)") +
  ylab("Gape width (mm)") +
  scale_x_continuous(
    breaks = c(1:5), 
    labels = c("10", "100", "1000", "10000", "100000")
  ) +
  scale_y_continuous(
    breaks = c(0.5,1,1.5,2), 
    labels = c("3","10","30","100")
  ) +
  theme(
    plot.title = element_text(size = 20), 
    axis.text = element_text(size = 15), 
    legend.text = element_text(size = 20), 
    legend.title = element_blank(),
    axis.title = element_text(size = 20), 
    legend.position = c(0.7, 0.2), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),  
    panel.border = element_rect(colour = "black", fill = NA, size = 1)
  )
figure4A

# Save plot as PNG file
# Save 4A
png("Figure4A.png", width = 7.53*300, height = 6*300, res = 300)
figure4A
dev.off()


#### Figure 4B ####

# Not only unique species but island-specific occurrences
length(rownames(fauna)) # n=3050

# Prepare data for model 4B
fauna$Gape.width_log10 <- log10(fauna$Gape.width) # New column with log10 transformed data
fauna$extinct_binary <- as.integer(ifelse(fauna$AlienNativeExtinct=="Extinct",1,0)) # New binary extinction column

# Model 4B (Information for Table S5)
model4 <- glmer(extinct_binary ~ Gape.width_log10 + Class + (1|Archipelago), family=binomial(link="logit"), na.action='na.omit', data=fauna )
nul4 <- glmer(extinct_binary ~ 1 + (1|Archipelago), family=binomial(link="logit"), na.action='na.omit', data=fauna )
AIC(model4, nul4)
cbind(fauna$extinct_binary,fitted(model4))
summary(model4)
hist(fauna$Gape.width_log10)

# Model assumptions
check_model(model4) # Number labels can not all be displayed because they overlap

# Create lists for model predictions
archipelagos <- levels(fauna$Archipelago)
predictions1 <- list()
predictions2 <- list()
predictions3 <- list()
new <- list()

# Assemble figure 4B in function
plot_figure4B <- function() {
# Create empty plot figure 4B to fill in with lines and points
plot(x = fauna$Gape.width_log10, y = fauna$extinct_binary, col="transparent", 
     xlab="Gape size (mm)", ylab="Island-specific extinction probability", 
     cex.axis=2, cex.lab=1.3, xaxt="n")

# Create x-axis
xticks <- c(0.5,1,1.5,2) # these are the log values that will give 0,10,100,1000,10000,100000 grams
xlab <- as.vector(c("3","10","30","100"))
axis(1, at= xticks, labels= xlab , col.axis="black", las=1, cex.axis=2)


# Predict mammal model prediction lines for each archipelago
(for(i in 1:length(archipelagos)){
  
  new[[i]] <- data.frame(Gape.width_log10=seq(from=0, to=2.5, by=0.1),
                         Class=as.factor(c('Mammalia')), extinct_binary=1, Archipelago=archipelagos[i])
  
  predictions1[[i]] <- as.vector( predict(model4, newdata=data.frame(Gape.width_log10=seq(from=0, to=2.5, by=0.1),
                                                                     Class=as.factor(c('Mammalia')), extinct_binary=1, Archipelago=archipelagos[i]),
                                          type="response", allow.new.levels = TRUE) )
  
  lines( as.vector(unlist(new[[i]][1])), unlist(predictions1[i]), col="#a5a2ce", lwd=0.5)
})

# Predict bird model prediction lines for each archipelago
(for(i in 1:length(archipelagos)){
  
  new[[i]] <- data.frame(Gape.width_log10=seq(from=0, to=2.5, by=0.1),
                         Class=as.factor(c('Aves')), extinct_binary=1, Archipelago=archipelagos[i])
  
  predictions2[[i]] <- as.vector( predict(model4, newdata=data.frame(Gape.width_log10=seq(from=0, to=2.5, by=0.1),
                                                                     Class=as.factor(c('Aves')), extinct_binary=1, Archipelago=archipelagos[i]),
                                          type="response", allow.new.levels = TRUE) )
  
  lines( as.vector(unlist(new[[i]][1])), unlist(predictions2[i]), col="#dcdcdc", lwd=1)
})

# Predict reptile model prediction lines for each Archipelago
(for(i in 1:length(archipelagos)){
  
  new[[i]] <- data.frame(Gape.width_log10=seq(from=0, to=2.5, by=0.1),
                         Class=as.factor(c('Reptilia')), extinct_binary=1, Archipelago=archipelagos[i])
  
  predictions3[[i]] <- as.vector( predict(model4, newdata=data.frame(Gape.width_log10=seq(from=0, to=2.5, by=0.1),
                                                                     Class=as.factor(c('Reptilia')), extinct_binary=1, Archipelago=archipelagos[i]),
                                          type="response", allow.new.levels = TRUE) )
  
  lines( as.vector(unlist(new[[i]][1])), unlist(predictions3[i]), col="#fed4b4", lwd=1)
})


# Add average model prediction lines for birds, mammals, reptiles 
lines(as.vector(unlist(new[[1]][1])), apply(X=as.data.frame(predictions2),MARGIN=1,FUN=mean), col="#666666", lwd=7)
lines(as.vector(unlist(new[[1]][1])), apply(X=as.data.frame(predictions1),MARGIN=1,FUN=mean), col="#7570B3", lwd=7)
lines(as.vector(unlist(new[[1]][1])), apply(X=as.data.frame(predictions3),MARGIN=1,FUN=mean), col="#D95F02", lwd=7)

# Add animal points to plot
points(fauna$Gape.width_log10[fauna$Class=="Aves"], fauna$extinct_binary[fauna$Class=="Aves"], col="#666666", pch=19, cex=3)
points(fauna$Gape.width_log10[fauna$Class=="Mammalia"], fauna$extinct_binary[fauna$Class=="Mammalia"], col="#7570B3", pch=19, cex=3)
points(fauna$Gape.width_log10[fauna$Class=="Reptilia"], fauna$extinct_binary[fauna$Class=="Reptilia"], col="#D95F02", pch=19, cex=3)
} # end plot function

# Plot figure 4B
plot_figure4B()

# To save as PNG:
png("Figure4B.png", width = 7.53*300, height = 6*300, res = 300)
plot_figure4B()
dev.off()

###


#### ANOVA ####
# Perform ANOVA to compare body mass across seed handling groups
anova_result <- aov(log10(Body.mass) ~ Seed.handling, data = fauna)

# Summary of the ANOVA results (Information for Table S7)
summary(anova_result)

# Perform Tukey's HSD test to see which groups are different (Information for Table S8)
TukeyHSD(anova_result)

###


#### Figure S5 body mass distribution ####

# Define colors for each panel
# Panel 1 colors (species class)
colors_panel1 <- c("Aves" = "#e41a1c", "Reptilia" = "#377eb8", "Mammalia" = "#4daf4a")  

# Panel 2 colors (Species Status)
colors_panel2 <- c("Alien" = "#984ea3", "Native" = "#66c2a5", "Extinct" = "#d95f02")  

# Panel 3 colors (Before vs After)
colors_panel3 <- c("Before" = "#7570b3", "After" = "#ff7f00")  

# Panel 4 colors (Seed Handling)
colors_panel4 <- c("SD" = "#e7298a", "SP" = "#66a61e", "DP" = "#e6ab02") 

# Panel 5 Archipelago colours
Archipelago_colors <- rainbow(22, alpha = 0.5)  # colors per archipelago

# Set x-axis limits manually
x_limits <- range(log10(fauna$Body.mass), na.rm = TRUE)

# Convert x-axis labels from log10 scale to actual gram values
log10_breaks <- seq(1, 5, by = 1)  
actual_gram_labels <- format(10^log10_breaks, scientific = FALSE, trim = TRUE)

# Create before and after groups to compare
fauna_grouped <- rbind(
  transform(fauna[fauna$AlienNativeExtinct %in% c("Native", "Extinct"), ], group = "Before"),
  transform(fauna[fauna$AlienNativeExtinct %in% c("Native", "Alien"), ], group = "After")
)
fauna_grouped$group <- factor(fauna_grouped$group, c("Before", "After"))

# Panel 1: Body Mass Distribution by Class (Aves, Reptilia, Mammalia)
p1 <- ggplot(fauna, aes(x = log10(Body.mass), fill = Class)) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = colors_panel1, labels = c("Aves" = "Birds", "Reptilia" = "Reptiles", "Mammalia" = "Mammals")) + 
  scale_x_continuous(breaks = log10_breaks, labels = actual_gram_labels, limits=x_limits) + 
  labs(fill = "Class", y = "Density", x = "Body Mass (log10 gram)") +  
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.title.x = element_blank(),  
    axis.title.y = element_text(size = 14),
    legend.position = c(0.85, 0.75),  
    legend.background = element_rect(fill = alpha('white', 0.6))  
  )
p1
# Panel 2: Body Mass Distribution by Species Status
p2 <- ggplot(fauna, aes(x = log10(Body.mass), fill = AlienNativeExtinct)) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = colors_panel2, labels = c("Alien" = "Non-Native", "Native" = "Native", "Extinct" = "Extinct")) + 
  scale_x_continuous(breaks = log10_breaks, labels = actual_gram_labels, limits=x_limits) +  
  labs(fill = "Species Status", y = "Density") + 
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.title.x = element_blank(),  
    axis.title.y = element_text(size = 14),
    legend.position = c(0.85, 0.75),  
    legend.background = element_rect(fill = alpha('white', 0.6))  
  )
p2
# Panel 3: Compare Before (without Alien) vs After (without Extinct)
p3 <- ggplot(fauna_grouped, aes(x = log10(Body.mass), fill = group)) + 
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = colors_panel3, labels = c("Before" = "Before extinctions", "After" = "After introductions")) + 
  scale_x_continuous(breaks = log10_breaks, labels = actual_gram_labels, limits=x_limits) + 
  labs(fill = "Time", y = "Density") +  
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.title.x = element_blank(),  
    axis.title.y = element_text(size = 14),
    legend.position = c(0.85, 0.75),  
    legend.background = element_rect(fill = alpha('white', 0.6)) 
  )
p3
# Panel 4: Body Mass Distribution by Seed Handling
p4 <- ggplot(fauna, aes(x = log10(Body.mass), fill = Seed.handling)) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = colors_panel4, labels = c("SD" = "Seed Disperser", "SP" = "Seed Predator", "DP" = "Seed Disperser/Predator")) + 
  scale_x_continuous(breaks = log10_breaks, labels = actual_gram_labels, limits=x_limits) +  
  labs(fill = "Seed Handling", y = "Density") +  
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.title.x = element_blank(),  
    axis.title.y = element_text(size = 14),
    legend.position = c(0.85, 0.75),  
    legend.background = element_rect(fill = alpha('white', 0.6)) 
  )
p4

# Panel 5: Body Mass Distribution per Archipelago
p5 <- ggplot(fauna, aes(x = log10(Body.mass), color = Archipelago)) +
  geom_density(aes(group = Archipelago), size = 1) +  # Group by Archipelago
  scale_x_continuous(breaks = log10_breaks, labels = actual_gram_labels, limits=x_limits) + 
  labs(x = "Body Mass (log10 gram)", y = "Density") + 
  scale_color_manual(values = Archipelago_colors) + 
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    legend.position = c(0.75, 0.5),  
    legend.direction = "vertical",  
    legend.box = "vertical", 
    legend.title = element_blank(),  
    legend.text = element_text(size = 7, face = "bold"),  
    legend.key.size = unit(0.2, "lines"), 
    legend.key = element_blank(), 
    legend.text.align = 0, 
    axis.title.y = element_text(size = 14)
  ) +
  guides(color = guide_legend(ncol = 5, override.aes = list(size = 1))) 
p5

# Define a common y-axis limit based on the maximum density value across all subsets
common_ylim <- c(0, 1.01)  

# Set same y-axis limits for all plots
p1 <- p1 + ylim(common_ylim)
p2 <- p2 + ylim(common_ylim)
p3 <- p3 + ylim(common_ylim)
p4 <- p4 + ylim(common_ylim)
p5 <- p5 + ylim(common_ylim)

# Combine all panels
combined_plot <- grid.arrange(
  p1, p2, p3, p4, p5,
  nrow = 5,  
  heights = c(1, 1, 1, 1, 1), 
  top = textGrob("Body mass distributions", gp = gpar(fontsize = 16, fontface = "bold"))  
)

# Show combined plot Figure S5
grid.draw(combined_plot)

# Save the plot
CairoPDF(file = "Body mass distributions.pdf", width = 12, height = 13)  
# Draw the combined plot 
grid.draw(combined_plot)
dev.off()  

###










