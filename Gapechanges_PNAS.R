# Changes in gape size


# Install and load packages
sapply(c("readr"), require, character.only = TRUE)

# Load data
islandsummary <- read.csv("Island Summary data.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8") # This is the output of the island summaries script (UTF-8 to maintain special characters in island names)
flora <- read.csv("Flora_PNAS.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8")
fauna <- read.csv("Fauna_PNAS.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8")

# Convert character columns to factor
islandsummary[] <- lapply(islandsummary, function(x) if(is.character(x)) factor(x) else x)
flora[] <- lapply(flora, function(x) if(is.character(x)) factor(x) else x)
fauna[] <- lapply(fauna, function(x) if(is.character(x)) factor(x) else x)

# Subset data: seed dispersers only
faunaSD <- subset(fauna, fauna$Seed.handling=="SD") 

# Subset data as start of new database
gapechanges <- islandsummary[,c("Island.archipelago","Archipelago","Island","Latitude","Longitude")]

# unique island lists (island archipelago combination as unique identifiers)
islands <- levels(islandsummary$Island.archipelago) # all islands
islandsFL <- levels(flora$Island.archipelago) # only islands for which there is flora data

# Calculate changes in gape size for all islands
# Loop through all islands
for(i in 1:length(islands)) {
  # Number of animals before (excluding "Alien")
  gapechanges$number_SD_animals_before[gapechanges$Island.archipelago == islands[i]] <- sum(faunaSD$AlienNativeExtinct != "Alien" & faunaSD$Island.archipelago == islands[i])
  
  # Number of animals after (excluding "Extinct")
  gapechanges$number_SD_animals_after[gapechanges$Island.archipelago == islands[i]] <- sum(faunaSD$AlienNativeExtinct != "Extinct" & faunaSD$Island.archipelago == islands[i])
  
  # Maximum gape size before
  if (gapechanges$number_SD_animals_before[gapechanges$Island.archipelago == islands[i]] > 0) {
    gapechanges$maxgapebefore[gapechanges$Island.archipelago == islands[i]] <- max(faunaSD$Gape.width[faunaSD$AlienNativeExtinct != "Alien" & faunaSD$Island.archipelago == islands[i]])
  } else {
    gapechanges$maxgapebefore[gapechanges$Island.archipelago == islands[i]] <- NA
  }
  
  # Maximum gape size after
  if (gapechanges$number_SD_animals_after[gapechanges$Island.archipelago == islands[i]] > 0) {
    gapechanges$maxgapeafter[gapechanges$Island.archipelago == islands[i]] <- max(faunaSD$Gape.width[faunaSD$AlienNativeExtinct != "Extinct" & faunaSD$Island.archipelago == islands[i]])
  } else {
    gapechanges$maxgapeafter[gapechanges$Island.archipelago == islands[i]] <- NA
  }
  
  # Calculate the changes in max gape size
  gapechanges$change_gape[gapechanges$Island.archipelago == islands[i]] <- gapechanges$maxgapeafter[gapechanges$Island.archipelago == islands[i]] - gapechanges$maxgapebefore[gapechanges$Island.archipelago == islands[i]]
}

# calculate change in match between gape and seed sizes for 64 islands with flora data (islands without flora data get NA values)
# Loop through islands with flora data
for(i in 1:length(islandsFL)){ 

  # Seed length based calculations (these have "L" in the column name)
  gapechanges$maxseedL.before[gapechanges$Island.archipelago==islandsFL[i]] = 
    max(flora$Seed_Length[ flora$Island.archipelago==islandsFL[i]])
  
  gapechanges$nrseeds.before[gapechanges$Island.archipelago==islandsFL[i]] = 
    length(flora$Seed_Length[ flora$Island.archipelago==islandsFL[i]]) 
  
  x1 <- flora$Seed_Length[ flora$Island.archipelago==islandsFL[i]] < gapechanges$maxgapebefore[gapechanges$Island.archipelago==islandsFL[i]]
  xx1 <- table(x1)["TRUE"]
  
  gapechanges$nrseeds.smallergape.beforeL[gapechanges$Island.archipelago==islandsFL[i]] = xx1
  
  x2 <- flora$Seed_Length[ flora$Island.archipelago==islandsFL[i]] < gapechanges$maxgapeafter[gapechanges$Island.archipelago==islandsFL[i]]
  xx2 <- table(x2)["TRUE"]
  
  gapechanges$nrseeds.smallergape.afterL[gapechanges$Island.archipelago==islandsFL[i]] = xx2
  
  gapechanges$nrseeds.nolongerfitL[gapechanges$Island.archipelago==islandsFL[i]] = 
    gapechanges$nrseeds.smallergape.beforeL[gapechanges$Island.archipelago==islandsFL[i]] - gapechanges$nrseeds.smallergape.afterL[gapechanges$Island.archipelago==islandsFL[i]] 
  
  gapechanges$Prop.seeds.nolongerfitL[gapechanges$Island.archipelago==islandsFL[i]] = 
    gapechanges$nrseeds.nolongerfitL[gapechanges$Island.archipelago==islandsFL[i]] / gapechanges$nrseeds.smallergape.beforeL[gapechanges$Island.archipelago==islandsFL[i]]

  # Seed width based calculations (these have "W" in the column name)
  gapechanges$maxseedW.before[gapechanges$Island.archipelago==islandsFL[i]] = 
    max(flora$Seed_Width[ flora$Island.archipelago==islandsFL[i]])
  
  gapechanges$nrseedsW.before[gapechanges$Island.archipelago==islandsFL[i]] = 
    length(flora$Seed_Width[ flora$Island.archipelago==islandsFL[i]]) 
  
  y1 <- flora$Seed_Width[ flora$Island.archipelago==islandsFL[i]] < gapechanges$maxgapebefore[gapechanges$Island.archipelago==islandsFL[i]]
  yy1 <- table(y1)["TRUE"]
  
  gapechanges$nrseeds.smallergape.beforeW[gapechanges$Island.archipelago==islandsFL[i]] = yy1
  
  y2 <- flora$Seed_Width[ flora$Island.archipelago==islandsFL[i]] < gapechanges$maxgapeafter[gapechanges$Island.archipelago==islandsFL[i]]
  yy2 <- table(y2)["TRUE"]
  
  gapechanges$nrseeds.smallergape.afterW[gapechanges$Island.archipelago==islandsFL[i]] = yy2
  
  gapechanges$nrseeds.nolongerfitW[gapechanges$Island.archipelago==islandsFL[i]] = 
    gapechanges$nrseeds.smallergape.beforeW[gapechanges$Island.archipelago==islandsFL[i]] - gapechanges$nrseeds.smallergape.afterW[gapechanges$Island.archipelago==islandsFL[i]] 
  
  gapechanges$Prop.seeds.nolongerfitW[gapechanges$Island.archipelago==islandsFL[i]] = 
    gapechanges$nrseeds.nolongerfitW[gapechanges$Island.archipelago==islandsFL[i]] / gapechanges$nrseeds.smallergape.beforeW[gapechanges$Island.archipelago==islandsFL[i]]
}

### Version of plant data without imputed values

# Subsets of flora that do not include imputed seed sizes
floraNIL <- subset(flora, flora$SL_Imputed==FALSE) # Seed length not imputed
floraNIW <- subset(flora, flora$SW_Imputed==FALSE) # Seed width not imputed
  
# calculate change in match between gape and seed sizes for islands with flora data
# Loop through islands with flora data
for(i in 1:length(islandsFL)){ 
  
  # Non-imputed subset: Seed length based calculations (these have "L" and "_NotImputed" in the column name)
  gapechanges$maxseedL.before_NotImputed[gapechanges$Island.archipelago==islandsFL[i]] = 
    max(floraNIL$Seed_Length[ floraNIL$Island.archipelago==islandsFL[i]])
  
  gapechanges$nrseeds.beforeL_NotImputed[gapechanges$Island.archipelago==islandsFL[i]] = 
    length(floraNIL$Seed_Length[ floraNIL$Island.archipelago==islandsFL[i]]) 
  
  x1 <- floraNIL$Seed_Length[ floraNIL$Island.archipelago==islandsFL[i]] < gapechanges$maxgapebefore[gapechanges$Island.archipelago==islandsFL[i]]
  xx1 <- table(x1)["TRUE"]
  
  gapechanges$nrseeds.smallergape.beforeL_NotImputed[gapechanges$Island.archipelago==islandsFL[i]] = xx1
  
  x2 <- floraNIL$Seed_Length[ floraNIL$Island.archipelago==islandsFL[i]] < gapechanges$maxgapeafter[gapechanges$Island.archipelago==islandsFL[i]]
  xx2 <- table(x2)["TRUE"]
  
  gapechanges$nrseeds.smallergape.afterL_NotImputed[gapechanges$Island.archipelago==islandsFL[i]] = xx2
  
  gapechanges$nrseeds.nolongerfitL_NotImputed[gapechanges$Island.archipelago==islandsFL[i]] = 
    gapechanges$nrseeds.smallergape.beforeL_NotImputed[gapechanges$Island.archipelago==islandsFL[i]] - gapechanges$nrseeds.smallergape.afterL_NotImputed[gapechanges$Island.archipelago==islandsFL[i]] 
  
  gapechanges$Prop.seeds.nolongerfitL_NotImputed[gapechanges$Island.archipelago==islandsFL[i]] = 
    gapechanges$nrseeds.nolongerfitL_NotImputed[gapechanges$Island.archipelago==islandsFL[i]] / gapechanges$nrseeds.smallergape.beforeL_NotImputed[gapechanges$Island.archipelago==islandsFL[i]]
  
  # Non-imputed subset: Seed width based calculations (these have "W" and "_NotImputed" in the column name)
  gapechanges$maxseedW.before_NotImputed[gapechanges$Island.archipelago==islandsFL[i]] = 
    max(floraNIW$Seed_Width[ floraNIW$Island.archipelago==islandsFL[i]])
  
  gapechanges$nrseedsW.before_NotImputed[gapechanges$Island.archipelago==islandsFL[i]] = 
    length(floraNIW$Seed_Width[ floraNIW$Island.archipelago==islandsFL[i]]) 
  
  y1 <- floraNIW$Seed_Width[ floraNIW$Island.archipelago==islandsFL[i]] < gapechanges$maxgapebefore[gapechanges$Island.archipelago==islandsFL[i]]
  yy1 <- table(y1)["TRUE"]
  
  gapechanges$nrseeds.smallergape.beforeW_NotImputed[gapechanges$Island.archipelago==islandsFL[i]] = yy1
  
  y2 <- floraNIW$Seed_Width[ floraNIW$Island.archipelago==islandsFL[i]] < gapechanges$maxgapeafter[gapechanges$Island.archipelago==islandsFL[i]]
  yy2 <- table(y2)["TRUE"]
  
  gapechanges$nrseeds.smallergape.afterW_NotImputed[gapechanges$Island.archipelago==islandsFL[i]] = yy2
  
  gapechanges$nrseeds.nolongerfitW_NotImputed[gapechanges$Island.archipelago==islandsFL[i]] = 
    gapechanges$nrseeds.smallergape.beforeW_NotImputed[gapechanges$Island.archipelago==islandsFL[i]] - gapechanges$nrseeds.smallergape.afterW_NotImputed[gapechanges$Island.archipelago==islandsFL[i]] 
  
  gapechanges$Prop.seeds.nolongerfitW_NotImputed[gapechanges$Island.archipelago==islandsFL[i]] = 
    gapechanges$nrseeds.nolongerfitW_NotImputed[gapechanges$Island.archipelago==islandsFL[i]] / gapechanges$nrseeds.smallergape.beforeW_NotImputed[gapechanges$Island.archipelago==islandsFL[i]]
}

# Look at data (Information for Table S6)
gapechanges

# Save data as CSV file (UTF-8 encoded to keep special characters)
write_csv(gapechanges,"Changes in gape size.csv") 
