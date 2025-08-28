# Figure 1 PNAS


# Run island summaries script first

# Install and load packages
sapply(c("ggplot2", "sf", "geodata", "terra", "viridis", "readr"), require, character.only = TRUE)

# Load data
islandsummary <- read.csv("Island Summary data.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8") # This is the output of the island summaries script (UTF-8 to maintain special characters in island names)
str(islandsummary)

# Calculate proportions extinction and introductions within communities
mean(islandsummary$Proportion.extinct) 
mean(islandsummary$Proportion.introduced) 

# Change island order to maximize data visualization
islandsummaryPE<- islandsummary[order(islandsummary$Proportion.extinct, decreasing=F),]
islandsummaryPNN<- islandsummary[order(islandsummary$Proportion.introduced, decreasing=F),]

### Create Figure 1a and 1B

# Load world map data
worldmap <- geodata::world(path = ".")
# Rotate the world map to Pacific orientation
pacificorientation <- rotate(worldmap, longitude = -35, split = TRUE, left = FALSE)
# Convert SpatVector to an sf object
sf_obj_pacificworldmap <- st_as_sf(pacificorientation)

# Figure 1A Before
figure1A <- ggplot() +
  geom_sf(data = sf_obj_pacificworldmap, color = "gray50", fill = "gray50") +
  coord_sf(xlim = c(-40, 320), ylim = c(-50, 50), expand = FALSE) +
  scale_color_viridis(option = "D") +
  scale_size_continuous(breaks = c(10, 20, 30, 40, 50, 60, 70, 80), range = c(4, 15)) +
  guides(colour = guide_legend(override.aes = list(size = 8)),
         size = guide_legend(override.aes = list(col = "grey50"))) +
  labs(x = "", y = "", colour = "Proportion", size = "Before", title = "Figure 1 A: Extinctions") +
  geom_point(aes(x = ifelse(Longitude < (-35), Longitude + 360, Longitude), y = Latitude,
                 colour = Proportion.introduced,
                 size = Before),
             shape = 1, stroke = 1.2,
             data = islandsummaryPE) +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

figure1A

# Figure 1B After
figure1B <- ggplot() +
  geom_sf(data = sf_obj_pacificworldmap, color = "gray50", fill = "gray50") +
  coord_sf(xlim = c(-40, 320), ylim = c(-50, 50), expand = FALSE) +  
  scale_color_viridis(option = "D") +
  scale_size_continuous(breaks = c(10, 20, 30, 40, 50, 60, 70, 80), range = c(4, 15)) +
  guides(colour = guide_legend(override.aes = list(size = 8)),
         size = guide_legend(override.aes = list(col = "grey50"))) +
  labs(x = "", y = "", colour = "Proportion", size = "After", title = "Figure 1 B: Introductions") +
  geom_point(aes(x = ifelse(Longitude < (-35), Longitude + 360, Longitude), y = Latitude,
                 colour = Proportion.introduced,
                 size = After),
             shape = 1, stroke = 1.2,
             data = islandsummaryPNN) +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

figure1B

# Save plots as PNG files
# Save 1A
png("Figure1A.png", width=16*300, height=10*300, res=300) 
figure1A
dev.off()

# Save 1B
png("Figure1B.png", width=16*300, height=10*300, res=300) 
figure1B
dev.off()

###
