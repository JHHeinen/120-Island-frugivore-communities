# Figure 6, S6.

# First run script "Changes in gape size" and use output file.

# Install and load packages
sapply(c("sf","geodata","terra","ggplot2"), require, character.only = TRUE)

# Load data
gapechanges <- read.csv("Changes in gape size.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8") # This is the output of the Changes in gape size script (UTF-8 to maintain special characters in island names)

# Convert character columns to factor
gapechanges[] <- lapply(gapechanges, function(x) if(is.character(x)) factor(x) else x)

### Figure 6 ###

# Colour palette for Panel 1
pal1 <- scale_color_gradient2(
  low = "#2D004B",      
  mid = "lightyellow",  
  high = "#fa6602",    
  midpoint = 0,
  na.value = "transparent",
  limits = c(min(gapechanges$change_gape, na.rm = TRUE), max(gapechanges$change_gape, na.rm = TRUE))
)

# Load world map data and rotate
w <- geodata::world(path = ".")
x <- rotate(w, long = -35, split = TRUE, left = FALSE)
sf_obj <- st_as_sf(x)

# Optimize data visualization order
gapechanges2 <- gapechanges[order(abs(gapechanges$change_gape), decreasing = FALSE),]

# Panel 1: Figure 6A
pan1 <- ggplot() +
  geom_sf(data = sf_obj, color = "gray50", fill = "gray50") +
  coord_sf(xlim = c(-40, 320), ylim = c(-50, 50), expand = FALSE) +  
  pal1 +
  labs(x = "", y = "", colour = "Change in max gape size", size = "Max gape size before", , title = "Figure 6A: Changes in gape size") +
  geom_point(aes(x = ifelse(Longitude < (-35), Longitude + 360, Longitude), y = Latitude,
                 colour = change_gape,
                 size = maxgapebefore),
             shape = 1, stroke = 2,
             data = gapechanges2, na.rm  = TRUE) +
  scale_size_continuous(range = c(1, 8)) + 
  theme(panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
print(pan1)

## Panel 2: Figure 6B
# Filter out data
filtered_gapechangesW <- gapechanges[gapechanges$nrseeds.before != 0,] # Only islands with plant data
filtered_gapechangesW <- filtered_gapechangesW[order(abs(filtered_gapechangesW$nrseeds.nolongerfitW), decreasing = FALSE),] # Optimize data visualization order


# Colour palette for Panel 2: Figure 6B
pal2 <- scale_color_gradient2(
  low = "#002D14",     
  mid = "lightyellow",  
  high =  "#67001F" ,   
  midpoint = 0,
  na.value = "transparent",
  limits = c(min(filtered_gapechangesW$nrseeds.nolongerfitW, na.rm = TRUE), max(filtered_gapechangesW$nrseeds.nolongerfitW, na.rm = TRUE))
)

# Panel 2: Figure 6B
pan2 <- ggplot() +
  geom_sf(data = sf_obj, color = "gray50", fill = "gray50") +
  coord_sf(xlim = c(-40, 320), ylim = c(-50, 50), expand = FALSE) +  
  pal2 +
  labs(x = "", y = "", colour = "Nr seeds no longer fit", size = "Nr seeds before", title = "Figure 6B: Number of seeds no longer fitting in gape (width-wise)") +
  geom_point(aes(x = ifelse(Longitude < (-35), Longitude + 360, Longitude), y = Latitude,
                 colour = nrseeds.nolongerfitW,
                 size = nrseeds.before),
             shape = 1, stroke = 2,
             data = filtered_gapechangesW, na.rm  = TRUE) +
  scale_size_continuous(range = c(1, 8)) +  
  theme(panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
print(pan2)

# Save plots
png("Figure6A.png", width=16*300, height=10*300, res=300) 
print(pan1)
dev.off()

png("Figure6B.png", width=16*300, height=10*300, res=300)
print(pan2)
dev.off()

###


### Sensitivity versions. Figure S6 ###

#### Figure S6A
# Filter out data
filtered_gapechangesL <- gapechanges[gapechanges$nrseeds.before != 0,] # Only islands with plant data
filtered_gapechangesL <- filtered_gapechangesL[order(abs(filtered_gapechangesL$nrseeds.nolongerfitL), decreasing = FALSE),] # Optimize data visualization order

# Colour palette Figure S6A
pal3 <- scale_color_gradient2(
  low = "#002D14",     
  mid = "lightyellow",  
  high =  "#67001F" ,   
  midpoint = 0,
  na.value = "transparent",
  limits = c(min(filtered_gapechangesL$nrseeds.nolongerfitL, na.rm = TRUE), max(filtered_gapechangesL$nrseeds.nolongerfitL, na.rm = TRUE))
)

# Figure S6A
pan3 <- ggplot() +
  geom_sf(data = sf_obj, color = "gray50", fill = "gray50") +
  coord_sf(xlim = c(-40, 320), ylim = c(-50, 50), expand = FALSE) +  
  pal2 +
  labs(x = "", y = "", colour = "Nr seeds no longer fit", size = "Nr seeds before", title ="Figure S6A: Number of seeds no longer fitting in gape (length-wise)" ) +
  geom_point(aes(x = ifelse(Longitude < (-35), Longitude + 360, Longitude), y = Latitude,
                 colour = nrseeds.nolongerfitL,
                 size = nrseeds.before),
             shape = 1, stroke = 2,
             data = filtered_gapechangesL, na.rm  = TRUE) +
  scale_size_continuous(range = c(1, 8)) +  
  theme(panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
print(pan3)


#### Figure S6B
# Filter out data
filtered_gapechangesL_NotImputed <- gapechanges[gapechanges$nrseeds.beforeL_NotImputed != 0,] # Only islands with plant data 
filtered_gapechangesL_NotImputed <- filtered_gapechangesL[order(abs(filtered_gapechangesL$nrseeds.nolongerfitL_NotImputed), decreasing = FALSE),] # Optimize data visualization order

# Colour palette Figure S6B
pal4 <- scale_color_gradient2(
  low = "#002D14",     
  mid = "lightyellow",  
  high =  "#67001F" ,   
  midpoint = 0,
  na.value = "transparent",
  limits = c(min(filtered_gapechangesL$nrseeds.nolongerfitL_NotImputed, na.rm = TRUE), max(filtered_gapechangesL$nrseeds.nolongerfitL_NotImputed, na.rm = TRUE))
)

# Figure S6B
pan4 <- ggplot() +
  geom_sf(data = sf_obj, color = "gray50", fill = "gray50") +
  coord_sf(xlim = c(-40, 320), ylim = c(-50, 50), expand = FALSE) +  
  pal2 +
  labs(x = "", y = "", colour = "Nr seeds no longer fit", size = "Nr seeds before", title="Figure S6B: Number of seeds no longer fitting in gape (length-wise, only non-imputed data)") +
  geom_point(aes(x = ifelse(Longitude < (-35), Longitude + 360, Longitude), y = Latitude,
                 colour = nrseeds.nolongerfitL_NotImputed,
                 size = nrseeds.beforeL_NotImputed),
             shape = 1, stroke = 2,
             data = filtered_gapechangesL_NotImputed, na.rm  = TRUE) +
  scale_size_continuous(range = c(1, 8)) +  
  theme(panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
print(pan4)


#### Figure S6C
# Filter out data
filtered_gapechangesW_NotImputed <- gapechanges[gapechanges$nrseeds.beforeW_NotImputed != 0,] # Only islands with plant data
filtered_gapechangesW_NotImputed <- filtered_gapechangesW[order(abs(filtered_gapechangesW$nrseeds.nolongerfitW_NotImputed), decreasing = FALSE),] # Optimize data visualization order

# Colour palette Figure S6C
pal5 <- scale_color_gradient2(
  low = "#002D14",     
  mid = "lightyellow",  
  high =  "#67001F" ,   
  midpoint = 0,
  na.value = "transparent",
  limits = c(min(filtered_gapechangesW$nrseeds.nolongerfitW_NotImputed, na.rm = TRUE), max(filtered_gapechangesW$nrseeds.nolongerfitW_NotImputed, na.rm = TRUE))
)

# Figure S6C
pan5 <- ggplot() +
  geom_sf(data = sf_obj, color = "gray50", fill = "gray50") +
  coord_sf(xlim = c(-40, 320), ylim = c(-50, 50), expand = FALSE) +  
  pal2 +
  labs(x = "", y = "", colour = "Nr seeds no longer fit", size = "Nr seeds before", title="Figure S6C: Number of seeds no longer fitting in gape (width-wise, only non-imputed data)") +
  geom_point(aes(x = ifelse(Longitude < (-35), Longitude + 360, Longitude), y = Latitude,
                 colour = nrseeds.nolongerfitW_NotImputed,
                 size = nrseedsW.before_NotImputed ),
             shape = 1, stroke = 2,
             data = filtered_gapechangesW_NotImputed, na.rm  = TRUE) +
  scale_size_continuous(range = c(1, 8)) +  
  theme(panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
print(pan5)


# Save plots
png("FigureS6A.png", width=16*300, height=10*300, res=300) 
print(pan3)
dev.off()

png("FigureS6B.png", width=16*300, height=10*300, res=300) 
print(pan4)
dev.off()

png("FigureS6C.png", width=16*300, height=10*300, res=300) 
print(pan5)
dev.off()

####
