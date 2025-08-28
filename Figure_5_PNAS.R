# Figure 5 seed and gape size distributions
# Figure S4,S8,S9, Tables S7, S8

# Load packages
sapply(c("ggplot2", "scales","dplyr", "ggforce", "Cairo"), require, character.only=TRUE)
# Note: On macOS, Cairo requires XQuartz: https://www.xquartz.org/

# Load fauna and flora data
flora <- read.csv("Flora_PNAS.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8")
fauna <- read.csv("Fauna_PNAS.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8")
str(flora)
str(fauna)

# Convert character columns to factor
fauna[] <- lapply(fauna, function(x) if(is.character(x)) factor(x) else x)
str(fauna)
flora[] <- lapply(flora, function(x) if(is.character(x)) factor(x) else x)
str(flora)

# Archipelago order needs to be alphabetical for flora and fauna datasets
levels(flora$Archipelago)
levels(fauna$Archipelago)

# Integrate Log10 transformation in column
fauna$Gape.width_log10 <- log10(fauna$Gape.width) # New column with log10 transformed data

# Subsets of fauna before and after extinctions and introductions
fauna.before <- subset(fauna,fauna$AlienNativeExtinct!="Alien") # subset before introductions
fauna.after  <- subset(fauna, fauna$AlienNativeExtinct!="Extinct") # subset after extinctions and introductions

# Prepare for plotting: data for each archipelago is displayed on a separate y-axis position. 
Lseed_data_y <- 100    
Lseed.sequence <- seq(from = 99.75, by = -0.1, length.out = length(levels(flora$Archipelago)))
Wseed_data_y <- min(Lseed.sequence)-0.6    
Wseed.sequence <- seq(from = Wseed_data_y-0.25, by = -0.1, length.out = length(levels(flora$Archipelago)))

Bgape_data_y <- min(Wseed.sequence)-0.6    
Bgape.sequence <- seq(from = Bgape_data_y-0.25, by = -0.1, length.out = length(levels(fauna.before$Archipelago)))
Agape_data_y <- min(Bgape.sequence)-0.6    
Agape.sequence <- seq(from = Agape_data_y-0.25, by = -0.1, length.out = length(levels(fauna.after$Archipelago)))

# Spacing between y-axis positions needs to be set 
Lseed.spacing<- setNames(Lseed.sequence, levels(flora$Archipelago))
Wseed.spacing<- setNames(Wseed.sequence, levels(flora$Archipelago))

Bgape.spacing<- setNames(Bgape.sequence, levels(fauna.before$Archipelago))
Agape.spacing<- setNames(Agape.sequence, levels(fauna.after$Archipelago))

# Creating new columns in databases to store y-axis positions for each type of data
flora$Lseed_y <- Lseed.spacing[as.character(flora$Archipelago)]
flora$Wseed_y <- Wseed.spacing[as.character(flora$Archipelago)]

fauna.before$Bgape_y <- Bgape.spacing[as.character(fauna.before$Archipelago)]
fauna.after$Agape_y <- Agape.spacing[as.character(fauna.after$Archipelago)]

# Create vector for Y-axis label text colours
labels<- c("Seed Length", names(Lseed.spacing), "Seed Width", names(Wseed.spacing), "Gape width before", names(Bgape.spacing), "Gape width after", names(Agape.spacing))
ytextcol<-ifelse(labels %in% c("Seed Length", "Seed Width", "Gape width before", "Gape width after"), "black", "grey65")

# Colours for bat carry limits
bcolbat<- ifelse(fauna.before$AlienNativeExtinct=="Extinct", "#cb181d","#edae49" )
acolbat<- ifelse(fauna.after$AlienNativeExtinct=="Extinct", "#cb181d","#edae49" )

# Gape width colours
bcolgape <- ifelse(fauna.before$AlienNativeExtinct == "Alien", "#08306B",
                   ifelse(fauna.before$AlienNativeExtinct == "Native", "#edae49",
                          ifelse(fauna.before$AlienNativeExtinct == "Extinct", "#cb181d", NA)))
acolgape <- ifelse(fauna.after$AlienNativeExtinct == "Alien", "#08306B",
                   ifelse(fauna.after$AlienNativeExtinct == "Native", "#edae49",
                          ifelse(fauna.after$AlienNativeExtinct == "Extinct", "#cb181d", NA)))

#### Create Figure 5 ####

figure5<- ggplot() + 
  # Add the density ridges for all data groups
  geom_density_ridges(data = flora, aes(x = log10(Seed_Length), y = Lseed_data_y), 
                      scale = 0.25, alpha = 0.8, col="transparent") +
  geom_density_ridges(data = flora, aes(x = log10(Seed_Width), y = Wseed_data_y), 
                      scale = 0.25, alpha = 0.8, col="transparent") +
  
  geom_density_ridges(data = fauna.before, aes(x = Gape.width_log10, y = Bgape_data_y), 
                      scale = 0.25, alpha = 0.8, col="transparent", fill="grey65") +
  geom_density_ridges(data = fauna.after, aes(x = Gape.width_log10, y = Agape_data_y), 
                      scale = 0.25, alpha = 0.8, col="transparent", fill="grey65") +
  
  # Add boxplots or all data groups
  geom_boxplot(data = flora, aes(x = log10(Seed_Length), y = Lseed_data_y), 
               width = .1, outlier.shape = NA, position = position_nudge(y = -0.1), color = "grey65") +
  geom_boxplot(data = flora, aes(x = log10(Seed_Width), y = Wseed_data_y), 
               width = .1, outlier.shape = NA, position = position_nudge(y = -0.1), color = "grey65") +
  geom_boxplot(data = fauna.before, aes(x = Gape.width_log10, y = Bgape_data_y), 
               width = .1, outlier.shape = NA, position = position_nudge(y = -0.1), color = "grey65") +
  geom_boxplot(data = fauna.after, aes(x = Gape.width_log10, y = Agape_data_y), 
               width = .1, outlier.shape = NA, position = position_nudge(y = -0.1), color = "grey65") +
  
  # Add points
  geom_point(data = flora, aes(x = log10(Seed_Length), y = Lseed_y), shape = '|', col = "grey65") +
  geom_point(data = flora, aes(x = log10(Seed_Width), y = Wseed_y), shape = '|', col = "grey65") +
  
  geom_point(data = fauna.before, aes(x = Gape.width_log10, y = Bgape_y), shape = '|', col = bcolgape) +
  geom_point(data = fauna.after, aes(x = Gape.width_log10, y = Agape_y), shape = '|', col = acolgape) +
  
  # Add bat carry limits for islands where there are fruit bats
  geom_point(data = fauna.before, aes(x = log10(Bat.carry.limit), y = Bgape_y), shape = 42,  col=bcolbat, size= 3, na.rm = TRUE) +
  geom_point(data = fauna.after, aes(x = log10(Bat.carry.limit), y = Agape_y), shape = 42,  col=acolbat, size= 3, na.rm = TRUE) +
  
  # Y-axis positions of points
  scale_y_continuous( limits=c(min(Agape.sequence) - 0, Lseed_data_y + 0.35),
                      breaks = c(Lseed_data_y, Lseed.spacing, Wseed_data_y, Wseed.spacing, Bgape_data_y,Bgape.spacing, Agape_data_y,Agape.spacing),
                      labels = c("Seed Length", names(Lseed.spacing), "Seed Width", names(Wseed.spacing), "Gape width before", names(Bgape.spacing), "Gape width after", names(Agape.spacing))
  ) +
  
  # Set theme and remove legend
  theme_ridges() +
  theme(legend.position = "none",axis.text.y = element_text(color = ytextcol, size=5), axis.title.x = element_text(size = 7), axis.text.x = element_text(size=10)) +
  
  # Add labels
  labs(y = "", x = "Millimeters")+
  scale_x_continuous(labels = c(0.1,1,10,100), limits=c(-1,2.1))
# Note: Warning refers to the colour of the y-axis text being determined via a vector. The y-axis text colour may not be the same in future ggplot2 versions.

# Show plot
figure5

# Save plot
CairoPDF("Figure5.pdf", width=6, height=7.53)
figure5
dev.off()


#### Figure S4 (sensitivity version of figure 5) ####

# Colour of seed data according to imputation status
flora <- flora %>%
  mutate(
    SL_imputed = SL_Imputed == "TRUE",
    SW_imputed = SW_Imputed == "TRUE",
    SL_color = ifelse(SL_imputed, "lightblue", "grey65"),
    SW_color = ifelse(SW_imputed, "lightblue", "grey65")
  )

# Create Figure S4 (sensitivity version of figure 5)
figureS4 <- ggplot() + 
  # Add the density ridge for all data groups
  geom_density_ridges(data = flora, aes(x = log10(Seed_Length), y = Lseed_data_y), 
                      scale = 0.25, alpha = 0.8, col="transparent") +
  geom_density_ridges(data = flora, aes(x = log10(Seed_Width), y = Wseed_data_y), 
                      scale = 0.25, alpha = 0.8, col="transparent") +
  
  geom_density_ridges(data = fauna.before, aes(x = Gape.width_log10, y = Bgape_data_y), 
                      scale = 0.25, alpha = 0.8, col="transparent", fill="grey65") +
  geom_density_ridges(data = fauna.after, aes(x = Gape.width_log10, y = Agape_data_y), 
                      scale = 0.25, alpha = 0.8, col="transparent", fill="grey65") +
  
  # Add boxplots or all data groups
  geom_boxplot(data = flora, aes(x = log10(Seed_Length), y = Lseed_data_y), 
               width = .1, outlier.shape = NA, position = position_nudge(y = -0.1), color = "grey65") +
  geom_boxplot(data = flora, aes(x = log10(Seed_Width), y = Wseed_data_y), 
               width = .1, outlier.shape = NA, position = position_nudge(y = -0.1), color = "grey65") +
  geom_boxplot(data = fauna.before, aes(x = Gape.width_log10, y = Bgape_data_y), 
               width = .1, outlier.shape = NA, position = position_nudge(y = -0.1), color = "grey65") +
  geom_boxplot(data = fauna.after, aes(x = Gape.width_log10, y = Agape_data_y), 
               width = .1, outlier.shape = NA, position = position_nudge(y = -0.1), color = "grey65") +
  
  # Add points
  geom_point(data = flora, aes(x = log10(Seed_Length), y = Lseed_y), shape = '|', col = flora$SL_color) +
  geom_point(data = flora, aes(x = log10(Seed_Width), y = Wseed_y), shape = '|', col = flora$SW_color) +
  
  geom_point(data = fauna.before, aes(x = Gape.width_log10, y = Bgape_y), shape = '|', col = bcolgape) +
  geom_point(data = fauna.after, aes(x = Gape.width_log10, y = Agape_y), shape = '|', col = acolgape) +

  # Add bat carry limits for islands where there are fruit bats
  geom_point(data = fauna.before, aes(x = log10(Bat.carry.limit), y = Bgape_y), shape = 42,  col=bcolbat, size= 3, na.rm = TRUE) +
  geom_point(data = fauna.after, aes(x = log10(Bat.carry.limit), y = Agape_y), shape = 42,  col=acolbat, size= 3, na.rm = TRUE) +
  
  # Y-axis positions of points
  scale_y_continuous( limits=c(min(Agape.sequence) - 0, Lseed_data_y + 0.35),
                      breaks = c(Lseed_data_y, Lseed.spacing, Wseed_data_y, Wseed.spacing, Bgape_data_y,Bgape.spacing, Agape_data_y,Agape.spacing),
                      labels = c("Seed Length", names(Lseed.spacing), "Seed Width", names(Wseed.spacing), "Gape width before", names(Bgape.spacing), "Gape width after", names(Agape.spacing))
  ) +
  
  # Set theme and remove legend
  theme_ridges() +
  theme(legend.position = "none",axis.text.y = element_text(color = ytextcol, size=5), axis.title.x = element_text(size = 7), axis.text.x = element_text(size=10)) +
  
  # Add labels
  labs(y = "", x = "Millimeters")+
  scale_x_continuous(labels = c(0.1,1,10,100), limits=c(-1,2.1))
# Note: Warning refers to the colour of the y-axis text being determined via a vector. The y-axis text colour may not be the same in future ggplot2 versions.

# Show plot
figureS4

# Save plot
CairoPDF("FigureS4.pdf", width=6, height=7.53)
figureS4
dev.off()


#### Figure S8 (Non-imputed seed shapes) #### 

# Only non-imputed data
df <- flora %>%
  filter(
    SL_Imputed == FALSE,
    SW_Imputed == FALSE,
    !is.na(Seed_Length),
    !is.na(Seed_Width) )

# Unique species
df_unique <- df %>% distinct(Species, .keep_all = TRUE)

# Calculate ellipse shapes at 10% scale
scale_factor <- 0.1
df_unique <- df_unique %>%
  mutate(
    x0    = Seed_Width,
    y0    = Seed_Length,
    a     = (Seed_Width  / 2) * scale_factor,
    b     = (Seed_Length / 2) * scale_factor,
    angle = 0
  )

# Create Figure S8
figureS8 <- ggplot(df_unique, aes(x = Seed_Width, y = Seed_Length)) +
  geom_point(alpha = 0.3, colour = "gray60") +
  geom_ellipse(aes(x0 = x0, y0 = y0, a = a, b = b, angle = angle),
               fill   = "grey85",
               colour = "black",
               alpha  = 0.5) +
  coord_fixed() +
  theme_minimal() +
  labs(
    title    = "Seed shapes",
    subtitle = "Subset of non-imputed data with both seed size measurements (scaled 10%)",
    x        = "Seed Width (mm)",
    y        = "Seed Length (mm)"
  )

# Show plot
figureS8

# Save plot
CairoPDF("FigureS8.pdf", width = 6, height = 6)
print(figureS8)
dev.off()


#### Figure S9 (Seed size imputations in blue) #### 

# Unique flora species
flora_unique <- flora %>%
  distinct(Seed_Length, Seed_Width, .keep_all = TRUE)

# Imputed or not
flora_unique <- flora_unique %>%
  mutate(
    Imputed = ifelse(
      SL_Imputed == "TRUE" | SW_Imputed == "TRUE",
      "Imputed",
      "Not imputed"
    ) %>% factor(levels = c("Not imputed","Imputed"))
  )

# Colours of points
imputation_colors <- c(
  "Not imputed" = "grey65",
  "Imputed"     = "lightblue"
)

# Plot Figure S9
figureS9 <- ggplot(flora_unique,
       aes(x = log10(Seed_Length),
           y = log10(Seed_Width),
           color = Imputed)) +
  geom_point(alpha = 0.7, size = 2) +
  scale_color_manual(values = imputation_colors,
                     guide = guide_legend(title = NULL)) +
  scale_x_continuous(breaks = log10(c(0.1, 1, 10, 100)),
                     labels = c(0.1, 1, 10, 100)) +
  scale_y_continuous(breaks = log10(c(0.1, 1, 10, 100)),
                     labels = c(0.1, 1, 10, 100)) +
  coord_fixed(ratio = 1) +
  theme_minimal() +
  labs(
    x     = "Seed Length (mm)   (25% imputed)",
    y     = "Seed Width (mm)   (37% imputed)",
    title = "Seed size imputations in blue"
  ) +
  theme(
    legend.position   = "none")

# Show plot
figureS9

# Save plot
CairoPDF("FigureS9.pdf", width=6, height=7.53)
figureS9
dev.off()

###




