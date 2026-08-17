library(tidyverse)
library(sportyR)
library(sp)  # For point-in-polygon functionality

# Load the data and rename columns
cutOff <- read_csv("cutOff.csv") %>% 
  rename(
    `Home to 1st` = `Home to 1B`,
    `Home to 2nd` = `Home to 2B`
  )

# Constants
d_1 <- 90  # Distance from the outfield to home plate
d_2 <- 120  # Distance from the infield to home plate
n <- 100  # Number of simulations

# Generate random samples for each relevant column
outfieldVelo <- rnorm(n, mean = 88, sd = 1.5)
infieldVelo <- rnorm(n, mean = 84, sd = 2)
infieldTransfer <- rnorm(n, mean = 1, sd = 0.12)

# Define a sequence of m values for distances
m_values <- seq(0, 20, by = 0.25)  # m from 0 to 20 with step 0.25

# Function to calculate ball flight time for each simulation and m value
calculate_ball_flight_time <- function(m) {
  sqrt((d_1^2 + m^2) / (outfieldVelo^2)) + 
    infieldTransfer + 
    sqrt((d_2^2 + m^2) / (infieldVelo^2))
}

# Calculate average ball flight time for each m
ball_flight_results <- map_df(m_values, ~{
  tibble(
    m = .x,
    ball_flight_time = mean(calculate_ball_flight_time(.x))  # Take mean across simulations
  )
})

# Generate random samples for runner times from home to home for comparison
homeToHome <- rnorm(n, mean = mean(na.omit(cutOff$`Home to 3rd`)), sd = sd(na.omit(cutOff$`Home to 3rd`)))

# Calculate out probability for each m
out_prob_results <- ball_flight_results %>%
  mutate(
    out_probability = map_dbl(m, ~mean(homeToHome > ball_flight_results$ball_flight_time[ball_flight_results$m == .x]))  # Calculate probability of out
  )

# Interpolate smooth points using spline with higher resolution
fence_data <- data.frame(
  angle = c(45, 0, -45),  # Angles (in degrees)
  distance = c(330, 400, 343)  # Distances (in feet)
) %>%
  mutate(
    x = distance * sin(angle * pi / 180),  # Convert to x-coordinate
    y = distance * cos(angle * pi / 180)   # Convert to y-coordinate
  )

smooth_fence <- as.data.frame(spline(
  x = fence_data$x,
  y = fence_data$y,
  n = 1000  # Increase the number of points for a smoother curve
))

# Set the range for the grid in inches (convert feet to inches) with 6-inch increments
x_range <- seq(min(smooth_fence$x) * 12, max(smooth_fence$x) * 12, by = 6)  # 6-inch increments
y_range <- seq(min(smooth_fence$y) * 12, max(smooth_fence$y) * 12, by = 6)  # 6-inch increments

# Create a grid of all possible points incremented by 6 inches
outfield_grid <- expand.grid(x_inch = x_range, y_inch = y_range) %>%
  mutate(
    x = x_inch / 12,  # Convert back to feet for visualization
    y = y_inch / 12
  )

# Use sp::point.in.polygon() to filter points inside the fence boundary
inside_fence <- sp::point.in.polygon(
  outfield_grid$x, outfield_grid$y, 
  smooth_fence$x, smooth_fence$y
) == 1

# Filter points inside the outfield fence
outfield_grid <- outfield_grid[inside_fence, ]

# Create heatmap data combining out probabilities and positions
heatmap_data <- outfield_grid %>%
  mutate(out_probability = map_dbl(1:nrow(outfield_grid), ~ {
    # Calculate m for the current point
    m_val <- sqrt((outfield_grid$x[.x])^2 + (outfield_grid$y[.x])^2)
    # Get the out probability for this m value
    out_prob_results$out_probability[out_prob_results$m == m_val]
  }))

# Plot the heatmap
ggplot(heatmap_data, aes(x = x, y = y, fill = out_probability)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red", na.value = "grey50") +
  labs(title = "Out Probability Heatmap from Third to Home",
       x = "X (feet)",
       y = "Y (feet)",
       fill = "Out Probability") +
  theme_minimal()

