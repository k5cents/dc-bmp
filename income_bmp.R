# Load in the income and BMP data files and the census track KML
library("sp")
library("maptools")

income <- read.csv("https://opendata.arcgis.com/datasets/b78d164649ad487db0f2b20a9261fb8c_17.csv")
BMP <- read.csv("https://opendata.arcgis.com/datasets/a973c2c7b7c14a918859f3e38bdffdd2_42.csv")
tract.raw <- getKMLcoordinates(kmlfile="https://opendata.arcgis.com/datasets/b78d164649ad487db0f2b20a9261fb8c_17.kml", ignoreAltitude = T)
tract.names <- as.character(read.csv("https://github.com/k5cents/dc-bmp/raw/master/tract_names.csv", header = F)$V1)
nbh.raw <- getKMLcoordinates(kmlfile="https://raw.githubusercontent.com/k5cents/dc-bmp/master/nbh_shapes.kml", ignoreAltitude = T)

# Create empty list, then fill with neighborhood boundies as polygons.
nbh.shapes <- as.list(rep(NA, length(nbh.raw)))
for(i in 1:length(nbh.raw)){
  nbh.shapes[[i]] <- Polygon(nbh.raw[[i]])
}

# Read in a list of neighborhood names (in the same order as the shapes).
nbh.names <- read.csv("https://raw.githubusercontent.com/k5cents/dc-bmp/master/nbh_names.csv", header = FALSE)$V1

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
BMP$neighborhood <- NA
for(i in 1:length(nbh.char)){
  BMP$neighborhood[BMP[nbh.char[i]] == 1] <- nbh.char[i]
}

# Repeat this all with census tract info

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

# Remove the presence indicator variables and extra objects
BMP[nbh.char[1:length(nbh.char)]] <- NULL
BMP[tract.no[1:length(tract.no)]] <- NULL
rm(list = nbh.char)
rm(list = tract.no)
rm(tract.nos, tract.vec)
BMP$tract <- gsub("tract_", "", BMP$tract)

# Make a vector of the number of BMPs per tract
tract.bmp <- rep(NA, nrow(income))
for(i in 1:nrow(income)){
  tract.bmp[i] <- nrow(subset(BMP, tract == tract.names[i]))
}

# Make a vector of the median income per tract
tract.income <- rep(NA, nrow(income))
for(i in 1:nrow(income)){
  tract.income[i] <- income$FAGI_MEDIAN_2005[income$FEDTRACTNO == tract.names[i]]
}

# Combine the two vectors
final <- as.data.frame(cbind(tract.income, tract.bmp))
names(final) <- c("income", "installations")

# Make a scatterplot
plot(final)
abline(lm(installations ~ income, data = final))
