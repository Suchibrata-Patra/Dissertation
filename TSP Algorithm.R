# Define Haversine distance function
haversine_distance = function(lat1, lon1, lat2, lon2) {
  # Convert latitude and longitude from degrees to radians
  lat1 = lat1 * pi / 180
  lon1 = lon1 * pi / 180
  lat2 = lat2 * pi / 180
  lon2 = lon2 * pi / 180
 
  # Radius of the Earth in kilometers
  R = 6371
 
  # Haversine formula
  dlon = lon2 - lon1
  dlat = lat2 - lat1
  a = sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c = 2 * asin(sqrt(a))
  distance = R * c
 
  return(distance)
}

# Coordinates of delivery locations
coordinates = data.frame(
  Location = c("A", "B", "C", "D", "E"),
  Latitude = c(22.5458, 22.5794, 22.5448, 22.5705, 22.5108),
  Longitude = c(88.3521, 88.3476, 88.3425, 88.4172, 88.3714)
)

# Calculate aerial distance between each pair of locations
distances = matrix(NA, nrow = nrow(coordinates), ncol = nrow(coordinates))
rownames(distances) = coordinates$Location
colnames(distances) = coordinates$Location

for (i in 1:nrow(coordinates)) {
  for (j in 1:nrow(coordinates)) {
    distances[i, j] = haversine_distance(coordinates[i, "Latitude"], coordinates[i, "Longitude"],
                                         coordinates[j, "Latitude"], coordinates[j, "Longitude"])
  }
}

# Speed limits matrix (km/h)
speed_limits = matrix(c(
  NA, 30, 25, 20, 35,
  30, NA, 28, 22, 38,
  25, 28, NA, 18, 33,
  20, 22, 18, NA, 28,
  35, 38, 33, 28, NA
), nrow = 5, byrow = TRUE)

# Calculate time required between each pair of locations (hours)
time_required = distances / speed_limits

# Algorithm to optimize delivery route considering time constraints and speed limits
delivery_route = c("A")  # Starting point
remaining_locations = coordinates$Location[!coordinates$Location %in% delivery_route]

while (length(remaining_locations) > 0) {
  current_location = delivery_route[length(delivery_route)]
  next_location = remaining_locations[1]
  min_time = time_required[current_location, next_location]
 
  for (location in remaining_locations) {
    time = time_required[current_location, location]
    if (!is.na(time) && time < min_time) {
      next_location = location
      min_time = time
    }
  }
 
  delivery_route = c(delivery_route, next_location)
  remaining_locations = remaining_locations[remaining_locations != next_location]
}

print("Optimized Delivery Route considering time constraints and speed limits:")
print(delivery_route)
