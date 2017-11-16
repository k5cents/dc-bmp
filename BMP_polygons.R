# This code uses the Google Maps KML file to apply neighborhood names onto the observations of our BMP dataset.
install.packages("sp")
install.packages("maptools")
install.packages("rgeos")
library("sp")
library("maptools")

# Download the KML and BMP data.
nbh.raw <- getKMLcoordinates(kmlfile="https://raw.githubusercontent.com/k5cents/dc-bmp/master/shapes.kml", ignoreAltitude = T)
BMP <- read.csv("https://opendata.arcgis.com/datasets/a973c2c7b7c14a918859f3e38bdffdd2_42.csv")

# Create empty list, then fill with neighborhood boundies as polygons.
nbh.shapes <- as.list(rep(NA, length(nbh.raw)))
for(i in 1:length(nbh.raw)){
  nbh.shapes[[i]] <- Polygon(nbh.raw[[i]])
}

# Ensure the resulting list is polygons and formatted right.
head(nbh.shapes, 1)

# Read in a list of neighborhood names, in the same order as the shapes.
nbh.names <- read.csv("https://raw.githubusercontent.com/k5cents/dc-bmp/master/names.csv", header = FALSE)$V1

# Apply that list of neighborhood names as the names for each neighborhood shape.
names(nbh.shapes) <- nbh.names

# Turn the list of neighborhood polygon coordinates into individual objects for each neighborhood.
for(i in 1:length(nbh.raw)){
  assign(names(nbh.shapes)[i], nbh.shapes[[i]])
}

# Create variable indicating presence in each neighborhood.
nbh.char <- as.character(nbh.names)
for(i in 1:length(nbh.char)){
  BMP[nbh.char[i]] <- point.in.polygon(
    BMP$LONGITUDE,
    BMP$LATITUDE,
    (get(nbh.char[i]))@coords[,1],  
    (get(nbh.char[i]))@coords[,2])
}

# Create single variable indicating neighborhood for each BMP entry.
BMP$Neighborhood <- NA
for(i in 1:length(nbh.char)){
  BMP$neighborhood[BMP[nbh.char[i]] == 1] <- nbh.char[i]
}

# Remove the neighborhood presence indicator variables

BMP[nbh.char[length(nbh.char)]] <- NULL

# Save modified dataset
write.csv(BMP, "BMP.csv")
