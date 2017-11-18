# This code formats the tax data set (eventually with region info).

library("sp")
library("maptools")
library(readr)

# Download the KML and BMP data.
nbh.raw <- getKMLcoordinates(kmlfile="https://raw.githubusercontent.com/k5cents/dc-bmp/master/shapes.kml", ignoreAltitude = T)
tax <- read.csv("https://opendata.arcgis.com/datasets/014f4b4f94ea461498bfeba877d92319_56.csv") # load property tax data

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

# Download the property addresses as a CSV file.
prop_address_list <- tax$PROPERTY_ADDRESS
prop_address_list <- as.data.frame(prop_address_list)
write_excel_csv(prop_address_list, path = "/home/kiernan/govt310/prop_address_list.csv")

# Use the DC MAR to assign Latitude, Longitude to property addresses.
tax.long <- read.csv("TAX_LONG.csv") # Created with MAR
tax.lat <- read.csv("TAX_LAT.csv") # Created with MAR
tax.coords <- cbind(tax.lat, tax.long)
nrow(tax$PROPERTY_ADDRESS) == nrow(tax.coords)
tax$LONGITUDE <- tax.long
tax$LATITUDE <- tax.lat

# Create variable indicating presence in each neighborhood.
nbh.char <- as.character(nbh.names)
for(i in 1:length(nbh.char)){
  tax[nbh.char[i]] <- point.in.polygon(
    tax$LONGITUDE,
    tax$LATITUDE,
    (get(nbh.char[i]))@coords[,1],  
    (get(nbh.char[i]))@coords[,2])
}

# Create single variable indicating neighborhood for each BMP entry.
tax$neighborhood <- NA
for(i in 1:length(nbh.char)){
  tax$neighborhood[tax[nbh.char[i]] == 1] <- nbh.char[i]
}

# Remove the neighborhood presence indicator variables
tax[nbh.char[1:length(nbh.char)]] <- NULL

# Save modified dataset
write.csv(tax, "BMP.csv")
head(tax$neighborhood)
