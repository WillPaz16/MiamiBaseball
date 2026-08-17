library(tidyverse)
library(sportyR)
library(sp)  # For point-in-polygon functionality

# Load the data and rename columns
cutOff <- read_csv("cutOff.csv") %>% 
  rename(
    `Home to 1st` = `Home to 1B`,
    `Home to 2nd` = `Home to 2B`
  )

# Convert mph to fps
mph_to_fps <- function(mph) {
  return(mph * 5280 / 3600)
}

# Constants
d_1 <- 130  
d_2 <- 130
n <- 100  # Number of simulations
windSpeed <- 6  # Wind speed in mph
adjustmentFactor <- 1 + 0.015 * windSpeed
set.seed(166)

# Generate random samples for base velocities in fps
outfieldTransfer <- rnorm(n, mean = .75, sd = 0.1)  # Seconds
outfieldVelo <- mph_to_fps(rnorm(n, mean = 85, sd = 1.5)) / adjustmentFactor
infieldVelo <- mph_to_fps(rnorm(n, mean = 75, sd = 2)) / adjustmentFactor
infieldTransfer <- rnorm(n, mean = 1, sd = 0.15)  # Seconds

# Define a sequence of m values
m_values <- seq(0, 40, by = 0.25)  # m from 0 to 50 feet with step 0.25

# Function to calculate the result for each simulation and m value
calculate_result <- function(m, d_1, d_2) {
  # Calculate total time in seconds
  outfieldTransfer +
  (sqrt((d_1^2 + m^2)) / outfieldVelo) + 
    infieldTransfer + ifelse(m > 0, log(1 + m) * 0.05, 0) +
    (sqrt((d_2^2 + m^2)) / infieldVelo)
}

# Iterate over m values and calculate the average result for each m
results <- map_df(m_values, ~{
  tibble(
    m = .x,
    result = mean(calculate_result(.x, d_1, d_2))  # Mean across simulations
  )
})

# Plot the results
ggplot(results, aes(x = m, y = result)) +
  geom_line(color = "goldenrod") +
  labs(
    title = "The Effect of a Bad Throw on Time",
    x = "Feet Thrown Offline",
    y = "Average Time (seconds)"
  ) +
  theme_minimal()


################################################################################

# Generate random samples for distances based on your cutoff data
homeToFirst <- rnorm(n, mean = mean(na.omit(cutOff$`Home to 1st`)), sd = sd(na.omit(cutOff$`Home to 1st`)))
homeToSecond <- rnorm(n, mean = mean(na.omit(cutOff$`Home to 2nd`)), sd = sd(na.omit(cutOff$`Home to 2nd`)))
homeToThird <- rnorm(n, mean = mean(na.omit(cutOff$`Home to 3rd`)), sd = sd(na.omit(cutOff$`Home to 3rd`)))
firstToThird <- rnorm(n, mean = mean(na.omit(cutOff$`1st to 3rd`)), sd = sd(na.omit(cutOff$`1st to 3rd`)))
firstToHome <- rnorm(n, mean = mean(na.omit(cutOff$`1st to Home`)), sd = sd(na.omit(cutOff$`1st to Home`)))
secondToHome <- rnorm(n, mean = mean(na.omit(cutOff$`2nd to Home`)), sd = sd(na.omit(cutOff$`2nd to Home`)))
secondToThird <- rnorm(n, mean = mean(na.omit(cutOff$`2nd to 3rd (Tag Up)`)), sd = sd(na.omit(cutOff$`2nd to 3rd (Tag Up)`)))
thirdToHome <- rnorm(n, mean = mean(na.omit(cutOff$`3rd to Home (Tag Up)`)), sd = sd(na.omit(cutOff$`3rd to Home (Tag Up)`)))

################################################################################

# Define key fence points with angles and distances
fence_data <- data.frame(
  angle = c(45, 0, -45),  # Angles (in degrees)
  distance = c(332, 400, 345)  # Distances (in feet)
) %>%
  mutate(
    x = distance * sin(angle * pi / 180),  # Convert to x-coordinate
    y = distance * cos(angle * pi / 180)   # Convert to y-coordinate
  )

# Interpolate smooth points using spline with higher resolution
smooth_fence <- as.data.frame(spline(
  x = fence_data$x,
  y = fence_data$y,
  n = 1000  # Increase the number of points for a smoother curve
))

# Define key infield edge points with angles and distances
infield_edge <- data.frame(
  angle = c(45, 0, -45),  # Angles (in degrees)
  distance = c(130, 155, 130)  # Distances (in feet)
) %>%
  mutate(
    x = distance * sin(angle * pi / 180),  # Convert to x-coordinate
    y = distance * cos(angle * pi / 180)   # Convert to y-coordinate
  )

# Interpolate smooth points using spline with higher resolution
smooth_edge <- as.data.frame(spline(
  x = infield_edge$x,
  y = infield_edge$y,
  n = 1000  # Increase the number of points for a smoother curve
))

# Left field foul line
lf_foul <- data.frame(
  angle = c(45, 45),
  distance = c(130, 332)
) %>% 
  mutate(
    x = distance * sin(angle * pi / 180),  # Convert to x-coordinate
    y = distance * cos(angle * pi / 180)   # Convert to y-coordinate
  )

# Right field foul line
rf_foul <- data.frame(
  angle = c(-45, -45),
  distance = c(345, 130)
) %>% 
  mutate(
    x = distance * sin(angle * pi / 180),  # Convert to x-coordinate
    y = distance * cos(angle * pi / 180)   # Convert to y-coordinate
  )

# Assuming smooth_fence and smooth_edge are already defined
# Combine the data for the area between the two splines
combined_polygon <- rbind(
  data.frame(x = smooth_fence$x, y = smooth_fence$y),
  data.frame(x = rev(smooth_edge$x), y = rev(smooth_edge$y))  # Reverse the order of smooth_edge
)

################################################################################

# Set the range for the grid in inches (convert feet to inches)
x_range <- seq(min(combined_polygon$x) * 12, max(combined_polygon$x) * 12, by = 3)  # 3-inch increments
y_range <- seq(min(combined_polygon$y) * 12, max(combined_polygon$y) * 12, by = 3)  # 3-inch increments

################################################################################

# Create a grid of all possible points incremented by 3 inches
outfield_grid <- expand.grid(x_inch = x_range, y_inch = y_range) %>%
  mutate(
    x = x_inch / 12,  # Convert back to feet for visualization
    y = y_inch / 12
  )

# Use sp::point.in.polygon() to filter points inside the fence boundary
inside_fence <- sp::point.in.polygon(
  outfield_grid$x, outfield_grid$y, 
  combined_polygon$x, combined_polygon$y
) == 1

# Filter points inside the outfield fence
outfield_grid <- outfield_grid[inside_fence, ]

# Calculate distances from the origin for points inside the fence
outfield_grid <- outfield_grid %>% 
  mutate(distanceOrigin = sqrt(x^2 + y^2))

################################################################################

#---------------------------- THIRD TO HOME ------------------------------------

################################################################################

# Divide distances by 2 to create d_1 and d_2 for the outfield
outfield_grid <- outfield_grid %>% 
  mutate(
    d_1_origin = distanceOrigin / 2,  # Divide distance by 2 for d_1
    d_2_origin = distanceOrigin / 2   # Divide distance by 2 for d_2
  )

# Map the times based on distances using a linear interpolation for the outfield
outfield_grid <- outfield_grid %>% 
  mutate(
    time_origin = calculate_result(0, d_1_origin, d_2_origin)  # Linear interpolation of time
  )

################################################################################

plot_origin <- geom_baseball(league = "MLB") +  # Add the baseball field
  geom_path(data = smooth_edge, aes(x = x, y = y), color = "#395d33", linewidth = 0.2) + 
  
  # Points outside the gradient range, colored forest green
  geom_point(data = outfield_grid %>% filter(time_origin < min(thirdToHome) | time_origin > max(thirdToHome)), 
             aes(x = x, y = y), color = "#395d33", size = 0.5) +
  
  # Points colored with the gradient based on time
  geom_point(data = outfield_grid %>% filter(time_origin >= min(thirdToHome) & time_origin <= max(thirdToHome)), 
             aes(x = x, y = y, color = time_origin), size = 0.5) + 
  
  # Define the gradient color scale for valid points
  scale_color_gradient2(
    low = "lightblue", mid = "white", high = "firebrick1", 
    midpoint = mean(thirdToHome), limits = c(min(thirdToHome), max(thirdToHome))
  ) + 
  
  geom_path(data = smooth_fence, aes(x = x, y = y), color = "gray90", linewidth = 1) + 
  
  labs(
    title = "Baseball Field Points with Time Gradient (m = 0)", 
    x = "X (feet)", 
    y = "Y (feet)", 
    color = "Time"
  ) +
  
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text = element_blank(),
    legend.position = "right"
  )


# Save the plot
ggsave("cutOffThirdtoHome.png", plot = plot_origin, 
       width = 7, height = 6, dpi = 300)

set.seed(166)
sum(outfield_grid$time_origin < quantile(na.omit(cutOff$`3rd to Home (Tag Up)`), 0.25), na.rm = TRUE) / sum(!is.na(outfield_grid$time_origin)) * 100
sum(outfield_grid$time_origin < mean(na.omit(cutOff$`3rd to Home (Tag Up)`)), na.rm = TRUE) / sum(!is.na(outfield_grid$time_origin)) * 100
sum(outfield_grid$time_origin < quantile(na.omit(cutOff$`3rd to Home (Tag Up)`), 0.75), na.rm = TRUE) / sum(!is.na(outfield_grid$time_origin)) * 100

################################################################################

# Map the times based on distances using a linear interpolation for the outfield
outfield_grid <- outfield_grid %>% 
  mutate(
    time_origin10 = calculate_result(10, d_1_origin, d_2_origin)  # Linear interpolation of time
  )

################################################################################

plot_origin10 <- geom_baseball(league = "MLB") +  # Add the baseball field
  geom_path(data = smooth_edge, aes(x = x, y = y), color = "#395d33", linewidth = 0.2) + 
  
  # Points outside the gradient range, colored forest green
  geom_point(data = outfield_grid %>% filter(time_origin10 < min(thirdToHome) | time_origin10 > max(thirdToHome)), 
             aes(x = x, y = y), color = "#395d33", size = 0.5) +
  
  # Points colored with the gradient based on time
  geom_point(data = outfield_grid %>% filter(time_origin10 >= min(thirdToHome) & time_origin10 <= max(thirdToHome)), 
             aes(x = x, y = y, color = time_origin10), size = 0.5) + 
  
  # Define the gradient color scale for valid points
  scale_color_gradient2(
    low = "lightblue", mid = "white", high = "firebrick1", 
    midpoint = mean(thirdToHome), limits = c(min(thirdToHome), max(thirdToHome))
  ) + 
  
  geom_path(data = smooth_fence, aes(x = x, y = y), color = "gray90", linewidth = 1) + 
  
  labs(
    title = "Baseball Field Points with Time Gradient (m = 10)", 
    x = "X (feet)", 
    y = "Y (feet)", 
    color = "Time"
  ) +
  
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text = element_blank(),
    legend.position = "right"
  )


# Save the plot
ggsave("cutOffThirdtoHome10.png", plot = plot_origin10, 
       width = 7, height = 6, dpi = 300)

set.seed(166)
sum(outfield_grid$time_origin10 < quantile(na.omit(cutOff$`3rd to Home (Tag Up)`), 0.25), na.rm = TRUE) / sum(!is.na(outfield_grid$time_origin10)) * 100
sum(outfield_grid$time_origin10 < mean(na.omit(cutOff$`3rd to Home (Tag Up)`)), na.rm = TRUE) / sum(!is.na(outfield_grid$time_origin10)) * 100
sum(outfield_grid$time_origin10 < quantile(na.omit(cutOff$`3rd to Home (Tag Up)`), 0.75), na.rm = TRUE) / sum(!is.na(outfield_grid$time_origin10)) * 100

################################################################################

# Map the times based on distances using a linear interpolation for the outfield
outfield_grid <- outfield_grid %>% 
  mutate(
    time_origin20 = calculate_result(20, d_1_origin, d_2_origin)  # Linear interpolation of time
  )

################################################################################

plot_origin20 <- geom_baseball(league = "MLB") +  # Add the baseball field
  geom_path(data = smooth_edge, aes(x = x, y = y), color = "#395d33", linewidth = 0.2) + 
  
  # Points outside the gradient range, colored forest green
  geom_point(data = outfield_grid %>% filter(time_origin20 < min(thirdToHome) | time_origin20 > max(thirdToHome)), 
             aes(x = x, y = y), color = "#395d33", size = 0.5) +
  
  # Points colored with the gradient based on time
  geom_point(data = outfield_grid %>% filter(time_origin20 >= min(thirdToHome) & time_origin20 <= max(thirdToHome)), 
             aes(x = x, y = y, color = time_origin20), size = 0.5) + 
  
  # Define the gradient color scale for valid points
  scale_color_gradient2(
    low = "lightblue", mid = "white", high = "firebrick1", 
    midpoint = mean(thirdToHome), limits = c(min(thirdToHome), max(thirdToHome))
  ) + 
  
  geom_path(data = smooth_fence, aes(x = x, y = y), color = "gray90", linewidth = 1) + 
  
  labs(
    title = "Baseball Field Points with Time Gradient (m = 20)", 
    x = "X (feet)", 
    y = "Y (feet)", 
    color = "Time"
  ) +
  
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text = element_blank(),
    legend.position = "right"
  )


# Save the plot
ggsave("cutOffThirdtoHome20.png", plot = plot_origin20, 
       width = 7, height = 6, dpi = 300)

set.seed(166)
sum(outfield_grid$time_origin20 < quantile(na.omit(cutOff$`3rd to Home (Tag Up)`), 0.25), na.rm = TRUE) / sum(!is.na(outfield_grid$time_origin20)) * 100
sum(outfield_grid$time_origin20 < mean(na.omit(cutOff$`3rd to Home (Tag Up)`)), na.rm = TRUE) / sum(!is.na(outfield_grid$time_origin20)) * 100
sum(outfield_grid$time_origin20 < quantile(na.omit(cutOff$`3rd to Home (Tag Up)`), 0.75), na.rm = TRUE) / sum(!is.na(outfield_grid$time_origin20)) * 100

################################################################################

#-------------------------- SECOND TO THIRD ------------------------------------

################################################################################

# Calculate distances from the origin for points inside the fence
outfield_grid <- outfield_grid %>% 
  mutate(distanceThird = sqrt((x+63.63961)^2 + (y-63.63961)^2))

################################################################################

# Divide distances by 2 to create d_1 and d_2 for the outfield
outfield_grid <- outfield_grid %>% 
  mutate(
    d_1_third = distanceThird / 2,  # Divide distance by 2 for d_1
    d_2_third = distanceThird / 2   # Divide distance by 2 for d_2
  )

# Map the times based on distances using a linear interpolation for the outfield
outfield_grid <- outfield_grid %>% 
  mutate(
    time_third = calculate_result(0, d_1_third, d_2_third)  # Linear interpolation of time
  )

################################################################################

plot_third <- geom_baseball(league = "MLB") +  # Add the baseball field
  geom_path(data = smooth_edge, aes(x = x, y = y), color = "#395d33", linewidth = 0.2) + 
  
  # Points outside the gradient range, colored forest green
  geom_point(data = outfield_grid %>% filter(time_third < min(secondToThird) | time_third > max(secondToThird)), 
             aes(x = x, y = y), color = "#395d33", size = 0.5) +
  
  # Points colored with the gradient based on time
  geom_point(data = outfield_grid %>% filter(time_third >= min(secondToThird) & time_third <= max(secondToThird)), 
             aes(x = x, y = y, color = time_third), size = 0.5) + 
  
  geom_path(data = smooth_fence, aes(x = x, y = y), color = "gray90", linewidth = 0.2) +
  
  # Define the gradient color scale for valid points
  scale_color_gradient2(
    low = "lightblue", mid = "white", high = "firebrick1", 
    midpoint = mean(thirdToHome), limits = c(min(thirdToHome), max(thirdToHome))
  ) + 
  
  labs(
    title = "Baseball Field Points with Time Gradient (m = 0)", 
    x = "X (feet)", 
    y = "Y (feet)", 
    color = "Time"
  ) +
  
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text = element_blank(),
    legend.position = "right"
  )


# Save the plot
ggsave("cutOffSecondToThird.png", plot = plot_third, 
       width = 7, height = 6, dpi = 300)

################################################################################

# Map the times based on distances using a linear interpolation for the outfield
outfield_grid <- outfield_grid %>% 
  mutate(
    time_third10 = calculate_result(10, d_1_third, d_2_third)  # Linear interpolation of time
  )

################################################################################

plot_third10 <- geom_baseball(league = "MLB") +  # Add the baseball field
  geom_path(data = smooth_edge, aes(x = x, y = y), color = "#395d33", linewidth = 0.2) + 
  
  # Points outside the gradient range, colored forest green
  geom_point(data = outfield_grid %>% filter(time_third10 < min(secondToThird) | time_third10 > max(secondToThird)), 
             aes(x = x, y = y), color = "#395d33", size = 0.5) +
  
  # Points colored with the gradient based on time
  geom_point(data = outfield_grid %>% filter(time_third10 >= min(secondToThird) & time_third10 <= max(secondToThird)), 
             aes(x = x, y = y, color = time_third10), size = 0.5) + 
  
  geom_path(data = smooth_fence, aes(x = x, y = y), color = "gray90", linewidth = 0.2) +
  
  # Define the gradient color scale for valid points
  scale_color_gradient2(
    low = "lightblue", mid = "white", high = "firebrick1", 
    midpoint = mean(thirdToHome), limits = c(min(thirdToHome), max(thirdToHome))
  ) + 
  
  labs(
    title = "Baseball Field Points with Time Gradient (m = 10)", 
    x = "X (feet)", 
    y = "Y (feet)", 
    color = "Time"
  ) +
  
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text = element_blank(),
    legend.position = "right"
  )


# Save the plot
ggsave("cutOffSecondToThird10.png", plot = plot_third10, 
       width = 7, height = 6, dpi = 300)

################################################################################

# Map the times based on distances using a linear interpolation for the outfield
outfield_grid <- outfield_grid %>% 
  mutate(
    time_third20 = calculate_result(20, d_1_third, d_2_third)  # Linear interpolation of time
  )

################################################################################

plot_third20 <- geom_baseball(league = "MLB") +  # Add the baseball field
  geom_path(data = smooth_edge, aes(x = x, y = y), color = "#395d33", linewidth = 0.2) + 
  
  # Points outside the gradient range, colored forest green
  geom_point(data = outfield_grid %>% filter(time_third20 < min(secondToThird) | time_third20 > max(secondToThird)), 
             aes(x = x, y = y), color = "#395d33", size = 0.5) +
  
  # Points colored with the gradient based on time
  geom_point(data = outfield_grid %>% filter(time_third20 >= min(secondToThird) & time_third20 <= max(secondToThird)), 
             aes(x = x, y = y, color = time_third20), size = 0.5) + 
  
  geom_path(data = smooth_fence, aes(x = x, y = y), color = "gray90", linewidth = 0.2) + 
  
  # Define the gradient color scale for valid points
  scale_color_gradient2(
    low = "lightblue", mid = "white", high = "firebrick1", 
    midpoint = mean(thirdToHome), limits = c(min(thirdToHome), max(thirdToHome))
  ) + 
  
  labs(
    title = "Baseball Field Points with Time Gradient (m = 20)", 
    x = "X (feet)", 
    y = "Y (feet)", 
    color = "Time"
  ) +
  
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text = element_blank(),
    legend.position = "right"
  )


# Save the plot
ggsave("cutOffSecondToThird20.png", plot = plot_third20, 
       width = 7, height = 6, dpi = 300)

################################################################################

# Calculate distances from the origin for points inside the fence
outfield_grid <- outfield_grid %>% 
  mutate(distanceSecond = sqrt((x)^2 + (y-127.2792)^2))

################################################################################

# Divide distances by 2 to create d_1 and d_2 for the outfield
outfield_grid <- outfield_grid %>% 
  mutate(
    d_1_second = distanceSecond / 2,  # Divide distance by 2 for d_1
    d_2_second = distanceSecond / 2   # Divide distance by 2 for d_2
  )

# Map the times based on distances using a linear interpolation for the outfield
outfield_grid <- outfield_grid %>% 
  mutate(
    time_second = calculate_result(0, d_1_second, d_2_second)  # Linear interpolation of time
  )

################################################################################

plot_second <- geom_baseball(league = "MLB") +  # Add the baseball field
  geom_path(data = smooth_fence, aes(x = x, y = y), color = "gray90", linewidth = 0.2) + 
  geom_path(data = smooth_edge, aes(x = x, y = y), color = "#395d33", linewidth = 0.2) + 
  
  # Points outside the gradient range, colored forest green
  geom_point(data = outfield_grid %>% filter(time_second < min(thirdToHome) | time_second > max(thirdToHome)), 
             aes(x = x, y = y), color = "#395d33", size = 0.5) +
  
  # Points colored with the gradient based on time
  geom_point(data = outfield_grid %>% filter(time_second >= min(thirdToHome) & time_second <= max(thirdToHome)), 
             aes(x = x, y = y, color = time_second), size = 0.5) + 
  
  # Define the gradient color scale for valid points
  scale_color_gradient2(
    low = "lightblue", mid = "white", high = "firebrick1", 
    midpoint = mean(thirdToHome), limits = c(min(thirdToHome), max(thirdToHome))
  ) + 
  
  labs(
    title = "Baseball Field Points with Time Gradient", 
    x = "X (feet)", 
    y = "Y (feet)", 
    color = "Time"
  ) +
  
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text = element_blank(),
    legend.position = "right"
  )


# Save the plot
ggsave("cutOffGradient.png", plot = plot_second, 
       width = 10, height = 7, dpi = 300)

