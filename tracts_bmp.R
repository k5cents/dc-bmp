# This code exists to apply census tract info to the BMP data set.
library("sp")
library("maptools")

# Download the KML and BMP data.
tract.raw <- getKMLcoordinates(kmlfile="https://opendata.arcgis.com/datasets/b78d164649ad487db0f2b20a9261fb8c_17.kml", ignoreAltitude = T)
BMP <- read.csv("https://opendata.arcgis.com/datasets/a973c2c7b7c14a918859f3e38bdffdd2_42.csv")
income <- read.csv("https://opendata.arcgis.com/datasets/b78d164649ad487db0f2b20a9261fb8c_17.csv")

# Create empty list, then fill with tract boundies as polygons.
tract.shapes <- as.list(rep(NA, length(tract.raw)))
for(i in 1:length(tract.raw)){
  tract.shapes[[i]] <- Polygon(tract.raw[[i]])
}

# Apply that list of tract "names" as the names for each neighborhood shape.
tract.vec <- rep("tract", length(tract.raw))
tract.nos <- tract.names <- as.character(read.csv("https://github.com/k5cents/dc-bmp/raw/master/fedtract.csv", header = F)$V1)
tract.no <-paste(tract.vec, tract.nos, sep = "_")
names(tract.shapes) <- tract.no

# Turn the list of tract polygon coordinates into individual objects for each neighborhood.
for(i in 1:length(tract.raw)){
  assign(names(tract.shapes)[i], tract.shapes[[i]])
}
# Create variable indicating presence in each neighborhood.
for(i in 1:length(tract.no)){
  BMP[tract.no[i]] <- point.in.polygon(
    BMP$LONGITUDE,
    BMP$LATITUDE,
    (get(tract.no[i]))@coords[,1],  
    (get(tract.no[i]))@coords[,2])
}

# Create single variable indicating neighborhood for each BMP entry.
BMP$tract <- NA
for(i in 1:length(tract.no)){
  BMP$tract[BMP[tract.no[i]] == 1] <- tract.no[i]
}

# Remove the neighborhood presence indicator variables
BMP[tract.no[1:length(tract.no)]] <- NULL

# Save modified dataset
head(BMP$tract)
