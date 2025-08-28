# Tortoise CCL and gape width

# Install and load packages
sapply(c("ggplot2"), require, character.only = TRUE)

# Load data
tortoises <- read.delim("Tortoises.txt")

# Linear model between tortoise curved carapace length (CCL) and gape width
m1 <- lm(gapewidth ~ CCL, data = tortoises)
summary(m1)

# Model assumptions
hist(resid(m1))
par(mfrow=c(2,2))  
plot(m1)
par(mfrow=c(1,1))

# Generating new CCL values based on the model and getting predictions with confidence intervals
new_CCL <- seq(min(tortoises$CCL), max(tortoises$CCL), length.out = 200)
newdata <- data.frame(CCL = new_CCL)
pred <- predict(m1, newdata = newdata, interval = "confidence")
pred_df <- data.frame(
  gapewidth = pred[, "fit"], # the predicted mean gape width at each CCL value.
  lwr       = pred[, "lwr"], # lower limit of the confidence interval for the mean prediction.
  upr       = pred[, "upr"], # upper limit of the confidence interval for the mean prediction.
  CCL       = new_CCL
)

# Create figure S7
Tor <- ggplot(tortoises, aes(x = CCL, y = gapewidth)) +
  geom_ribbon(
    data = pred_df,
    aes(x = CCL, ymin = lwr, ymax = upr), 
    fill = "grey80", alpha = 0.5,
    inherit.aes = FALSE
  ) +
  geom_line(
    data = pred_df,
    aes(x = CCL, y = gapewidth), 
    color = "black",
    inherit.aes = FALSE
  ) +
  labs(
    title = paste0(
      "Adj R² = ", signif(summary(m1)$adj.r.squared, 5),
      "   P = ", signif(summary(m1)$coef[2, 4], 5),
      "    Gape width (cm) = ",
      signif(m1$coef[[1]], 5), " + ",
      signif(m1$coef[[2]], 5), " * CCL"
    )
  ) +
  geom_point(size = 4) +
  ylab("Gape width (cm)") +
  xlab("Curved Carapace Length CCL (cm)\n       Aldabrachelys gigantea") +
  scale_x_continuous(breaks = c(75, 100, 125, 150), limits=c(75,165)) +
  theme(
    plot.title       = element_text(size = 15),
    axis.text        = element_text(size = 15),
    axis.title       = element_text(size = 15),
    legend.text      = element_text(size = 15),
    legend.title     = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line        = element_line(colour = "black"),
    axis.ticks       = element_line(colour = "black")   
  )

# show plot
Tor

# Save plot as PNG file
png("Tortoises.png", width=10*300, height=10*300, res=300) 
Tor
dev.off()

