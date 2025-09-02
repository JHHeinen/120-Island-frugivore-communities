# Figure 2 and 3: Principle Coordinate Analysis


# Install and load packages
sapply(c("Cairo","vegan","ape","StatMatch","MASS","ggplot2","varhandle",
         "stringr","RColorBrewer","scales","ggrepel"), require, character.only = TRUE)

# Load data
fauna <- read.csv("Fauna_PNAS.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8")

# Set species names are row names
fauna <- as.data.frame(fauna, row.names=fauna$Species)

# Save version of this data under different name for later use
data <- fauna

# Subset with traits only
data <- data[,c("Fruit.access","Fruit.in.diet", "Diet", "Seed.handling", "Gape.width","Class","Body.mass")]
str(data)

# Convert character columns to factor
data[] <- lapply(data, function(x) if(is.character(x)) factor(x) else x)
str(data)

# Log10 transformations
data$Gape.width <- log10(data$Gape.width)
data$Body.mass <- log10(data$Body.mass)
data$Fruit.in.diet <- log10(data$Fruit.in.diet)

# transform categorical data to binary columns per category
Fruit.access.dummy <- to.dummy(data$Fruit.access, prefix="Fruit.access")
Diet.dummy <- to.dummy(data$Diet, prefix="Diet")
Seed.handling.dummy <- to.dummy(data$Seed.handling, prefix="Seed.handling")
Class.dummy <- to.dummy(data$Class, prefix="Class")

dummy <- cbind(Fruit.access.dummy,Diet.dummy,Seed.handling.dummy,Class.dummy)
dummy <- as.data.frame(dummy, row.names = rownames(data))
dummy$Gape.width <- data$Gape.width
dummy$Body.mass <- data$Body.mass
dummy$Fruit.percent <- data$Fruit.in.diet
data <- dummy

# Set Gower distance weights proportionate to number of levels per category
str(data)
weights <- c(1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1/3,1,1,1) 

# Calculate Gower distance
str(data)
gowdist <- gower.dist(data, var.weights= weights) # takes a bit of time to run

# Run PCoA 
# NOTE: "No correction was applied to the negative eigenvalues"
pcoa.result <- pcoa(gowdist) # takes a bit of time to run

# Simple plot version
fit <- envfit(pcoa.result$vectors[,1:2], data)
plot(pcoa.result$vectors[,1:2], pch=19, asp=1, col="grey50",
     xlab="PCoA1", ylab="PCoA2", main="PCoA with envfit arrows")
plot(fit, add=TRUE, col="black")

# Save first two axis data
pcoa.axis <- as.data.frame(pcoa.result$vectors[,c(1,2)])
pcoa.axis

# Species names as row names for axis data
pcoa.axis <- as.data.frame(pcoa.axis, row.names=data$Species)
pcoa.axis

# Calculate the percentage of variation that each axis accounts for
perc.rel.eig <- round(pcoa.result$values$Relative_eig * 100, 2)
perc.rel.eig # first axis 41.30%, second axis 24.96% explained

# Table S2
with(pcoa.result$values,
     print(
       data.frame(
         Axis                = paste0("PCoA", 1:2),
         Eigenvalue          = Eigenvalues[1:2],
         Percent_Variance    = round(Relative_eig[1:2] * 100, 2),
         Cumulative_Variance = round(cumsum(Relative_eig)[1:2] * 100, 2) ),row.names = FALSE))

# Figure S1
# Plot relative eigenvalues per axis. 
plot(seq_along(perc.rel.eig[1:10]),           
  perc.rel.eig[1:10],                      
  type="b",pch=1,lty=1,bty="n",                                
  xlim=c(0, 10),ylim=c(0, 50),
  xlab="Axis number", ylab="Relative eigenvalues (%)",
  main="PCoA relative eigenvalues per axis" )

# Prepare to extract more data columns from previously saved data version
fauna[] <- lapply(fauna, function(x) if(is.character(x)) factor(x) else x) # Turn all character columns into factors
str(fauna)

# Create datasets of pcoa axis coordinates for total, before, and after
pcoa.axis$AlienNativeExtinct <- fauna$AlienNativeExtinct
pcoa.axis$Archipelago <- as.factor(fauna$Archipelago)
pcoa.axis$Island <- as.factor(fauna$Island)
pcoa.axis$Species <- as.factor(fauna$Species) 
pcoa.all <- pcoa.axis
pcoa.before <- subset(pcoa.axis, pcoa.axis$AlienNativeExtinct!="Alien") # Before
pcoa.after <- subset(pcoa.axis, pcoa.axis$AlienNativeExtinct!="Extinct") # After

# Create colour vectors
aliencol.C <- pcoa.after$AlienNativeExtinct
aliencol.C <- ifelse(aliencol.C=="Alien", "#08306B", "transparent")
nativecol.C <- pcoa.before$AlienNativeExtinct
nativecol.C <- ifelse(nativecol.C=="Native","#edae49" , "transparent") 
extinctcol.B <- pcoa.before$AlienNativeExtinct
extinctcol.B <- ifelse(extinctcol.B=="Extinct", "#d1495b", "transparent")

# Note: Figure 2 is created further down

### Figure 3: All Archipelagos combined
archippelagoplot <- ggplot(pcoa.before, aes(Axis.1, Axis.2)) +
  coord_fixed() +
  geom_density_2d_filled(contour_var = "ndensity", alpha = 1) +
  scale_fill_manual(values = colorRampPalette(brewer.pal(9,"Greys"))(10)) +
  facet_wrap(~ Archipelago, labeller = labeller(Archipelago = label_wrap_gen(10))) +
  geom_point(data= pcoa.before, aes(Axis.1, Axis.2), colour=nativecol.C) +
  geom_point(data= pcoa.after, aes(Axis.1, Axis.2), colour=aliencol.C) +
  geom_point(data= pcoa.before, aes(Axis.1, Axis.2), colour=extinctcol.B) +
  theme_classic() +  
  theme(strip.background = element_blank(),  
        strip.text = element_text(size = rel(0.7), face = "bold", margin = margin(0, 0, 0, 0)),  
        panel.border = element_rect(color = "black", fill = NA, size = 0.2),  
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(), 
        axis.line = element_blank(),
        legend.position = "none",
        panel.spacing = unit(0.1, "lines")
  )  

archippelagoplot <- archippelagoplot + 
  facet_wrap(~ str_replace_all(str_replace_all(Archipelago, "\\(", "\n("), " /", "\n/"), labeller = labeller(Archipelago = label_wrap_gen(10)))  

archippelagoplot

# Save plot (maintain special characters in names)
CairoPDF("Figure 3.pdf", width=6, height=7.53)
archippelagoplot
dev.off()


##### Figure 2: Changes in PCoA #####

# Calculate the common x and y range for axis 1 and axis 2
xrng = range(c(pcoa.all$Axis.1, pcoa.all$Axis.1))
yrng = range(c(pcoa.all$Axis.2, pcoa.all$Axis.2))

# Calculate the 2d density estimate over the common range
d1 = kde2d(pcoa.before$Axis.1, pcoa.before$Axis.2, lims=c(xrng, yrng), n=200) 
d2 = kde2d(pcoa.after$Axis.1, pcoa.after$Axis.2, lims=c(xrng, yrng), n=200) 

# Calculate the difference between the 2d density estimates
diff12 = d1 
diff12$z = d2$z - d1$z            

# Balanced colour palette
col10 <- c(rev(colorRampPalette(brewer.pal(9,"Reds"))(13)),colorRampPalette(brewer.pal(9,"Blues"))(13))
show_col(col10)
show_col(col10[5:26])
col10SUB <- col10[5:26]

# Point colours
nativecol.1 <- pcoa.all$AlienNativeExtinct
nativecol.1<- ifelse(nativecol.1=="Native", "#edae49", "transparent")
aliencol.1 <- pcoa.all$AlienNativeExtinct
aliencol.1<- ifelse(aliencol.1=="Alien", "#08306B", "transparent")
extinctcol.1 <- pcoa.all$AlienNativeExtinct
extinctcol.1<- ifelse(extinctcol.1=="Extinct", "#d1495b", "transparent")

# Function to rescale so the smallest value becomes 0 and the largest becomes 1
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

# Show figure 2 (cannot be saved into an object, arrows cannot be plotted in this type of figure directly, and there is a grid-like appearance)
filled.contour(diff12$z, asp = 1, col=col10SUB, 
                         plot.axes={points(range01(pcoa.all$Axis.1), range01(pcoa.all$Axis.2), cex=1, col = nativecol.1, pch=19)}, 
                         plot.axis={points(range01(pcoa.all$Axis.1), range01(pcoa.all$Axis.2), cex=1, col = extinctcol.1, pch=19)}, 
                         plot.axis={points(range01(pcoa.all$Axis.1), range01(pcoa.all$Axis.2), cex=1, col = aliencol.1, pch=19)},
                         frame.plot = FALSE) 

# Arrow preparations
fit<-envfit(pcoa.all[,c(1,2)], data)
spp.scrs <- as.data.frame(scores(fit, display = "vectors"))
arrow_factor <- 0.3

#### Create Figure 2: Changes in PCoA 
### Assembled version

grid <- expand.grid(x = d1$x, y = d1$y)
grid$z <- as.vector(diff12$z)

nbreaks <- 22
brks <- pretty(range(grid$z, finite = TRUE), n = nbreaks)
cols <- colorRampPalette(col10SUB)(length(brks) - 1)

fig2 <- ggplot(grid, aes(x, y, z = z)) +
  geom_contour_filled(breaks = brks, show.legend = FALSE) +
  scale_fill_manual(values = cols, drop = FALSE) +
  geom_point(data = pcoa.all,
             aes(Axis.1, Axis.2),
             inherit.aes = FALSE, colour = nativecol.1, size = 2.5, shape = 16) +
  geom_point(data = pcoa.all,
             aes(Axis.1, Axis.2),
             inherit.aes = FALSE, colour = extinctcol.1, size = 2.5, shape = 16) +
  geom_point(data = pcoa.all,
             aes(Axis.1, Axis.2),
             inherit.aes = FALSE, colour = aliencol.1,   size = 2.5, shape = 16) +
  geom_segment(data = spp.scrs,
               aes(x = 0, y = 0,
                   xend = Axis.1 * arrow_factor, yend = Axis.2 * arrow_factor),
               inherit.aes = FALSE,
               arrow = arrow(length = unit(0.25, "cm")),
               colour = "grey40", linewidth = 0.6) +
  coord_fixed() + theme_void()

fig2

# Save
ggsave("Figure 2.pdf", fig2, device = grDevices::cairo_pdf, width = 7, height = 7)

######
