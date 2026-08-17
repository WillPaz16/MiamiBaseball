library(tidyverse)
library(ggthemes)

###############################################################################

fallDF <- read_csv("fallDF.csv")
fallDF <- drop_na(fallDF, PlateLocSide, PlateLocHeight) # Remove NA values for relevant variables
glimpse(fallDF)

ahmad_full <- fallDF %>% 
  filter(Pitcher == "Harajli, Ahmad")

ahmad <- fallDF %>% 
  filter(Pitcher == "Harajli, Ahmad",
         TaggedPitchType == "Fastball")

write_csv(ahmad, "ahmad.csv")

###############################################################################

# Define rectangle bounds
rect_xmin <- -0.85
rect_xmax <- 0.85
rect_ymin <- 1.5
rect_ymax <- 3.5

# Define all possible regions
all_regions <- tibble(region = c("High Q1", "High Q2", "High Q3", "High and Inside", "High and Outside",
                                 "Inside Q1", "Inside Q2", "Inside Q3", "Inside Q1-1", "Inside Q1-2",
                                 "Inside Q1-3", "Inside Q2-1", "Inside Q2-2","Inside Q2-3", "Inside Q3-1",
                                 "Inside Q3-2", "Inside Q3-3", "Low Q1", "Low Q2", "Low Q3", "Low and Inside",
                                  "Low and Outside", "Outside Q1", "Outside Q2", "Outside Q3"))

# Define regions with clear mapping for inside and outside
ahmad <- ahmad %>%
  mutate(
    region = case_when(
      PlateLocSide >= rect_xmin & PlateLocSide <= rect_xmax &
        PlateLocHeight >= rect_ymax ~ paste0(
          "High Q",
          findInterval(PlateLocSide, seq(rect_xmin, rect_xmax, length.out = 4), rightmost.closed = TRUE)
        ),
      PlateLocSide >= rect_xmin & PlateLocSide <= rect_xmax &
        PlateLocHeight <= rect_ymin ~ paste0(
          "Low Q",
          findInterval(PlateLocSide, seq(rect_xmin, rect_xmax, length.out = 4), rightmost.closed = TRUE)
        ),
      PlateLocSide >= rect_xmin & PlateLocSide <= rect_xmax &
        PlateLocHeight >= rect_ymin & PlateLocHeight <= rect_ymax ~ paste0(
          "Inside Q",
          findInterval(PlateLocSide, seq(rect_xmin, rect_xmax, length.out = 4), rightmost.closed = TRUE),
          "-",
          findInterval(PlateLocHeight, seq(rect_ymin, rect_ymax, length.out = 4), rightmost.closed = TRUE)
        ),
      PlateLocHeight > rect_ymax & PlateLocSide < rect_xmin ~ "High and Inside",
      PlateLocHeight > rect_ymax & PlateLocSide > rect_xmax ~ "High and Outside",
      PlateLocHeight < rect_ymin & PlateLocSide < rect_xmin ~ "Low and Inside",
      PlateLocHeight < rect_ymin & PlateLocSide > rect_xmax ~ "Low and Outside",
      PlateLocSide < rect_xmin ~ paste0(
        "Inside Q",
        findInterval(PlateLocHeight, seq(rect_ymin, rect_ymax, length.out = 4), rightmost.closed = TRUE)
      ),
      PlateLocSide > rect_xmax ~ paste0(
        "Outside Q",
        findInterval(PlateLocHeight, seq(rect_ymin, rect_ymax, length.out = 4), rightmost.closed = TRUE)
      ),
      TRUE ~ "Unknown"
    )
  )

###############################################################################

# Calculate average VertApprAngle for each region
avg_vert_angle_ahmad <- ahmad %>%
  group_by(region) %>%
  summarise(avg_angle = mean(VertApprAngle, na.rm = TRUE)) %>%
  right_join(all_regions, by = "region") %>%
  replace_na(list(avg_angle = 0)) %>%
  mutate(
    x = case_when(
      # Outer positions
      region == "High Q1" ~ rect_xmin + (rect_xmax - rect_xmin) / 6,
      region == "High Q2" ~ rect_xmin + (rect_xmax - rect_xmin) / 2,
      region == "High Q3" ~ rect_xmin + 5 * (rect_xmax - rect_xmin) / 6,
      region == "High and Inside" ~ rect_xmin - 0.5,
      region == "High and Outside" ~ rect_xmax + 0.5,
      region == "Outside Q1" ~ rect_xmax + 0.5,
      region == "Outside Q2" ~ rect_xmax + 0.5,
      region == "Outside Q3" ~ rect_xmax + 0.5,
      region == "Inside Q1" ~ rect_xmin - 0.5,
      region == "Inside Q2" ~ rect_xmin - 0.5,
      region == "Inside Q3" ~ rect_xmin - 0.5,
      region == "Low Q1" ~ rect_xmin + (rect_xmax - rect_xmin) / 6,
      region == "Low Q2" ~ rect_xmin + (rect_xmax - rect_xmin) / 2,
      region == "Low Q3" ~ rect_xmin + 5 * (rect_xmax - rect_xmin) / 6,
      region == "Low and Inside" ~ rect_xmin - 0.5,
      region == "Low and Outside" ~ rect_xmax + 0.5,
      # Hard coded positions for inside quadrants
      region == "Inside Q1-1" ~ rect_xmin + (rect_xmax - rect_xmin) / 6,
      region == "Inside Q1-2" ~ rect_xmin + (rect_xmax - rect_xmin) / 6,
      region == "Inside Q1-3" ~ rect_xmin + (rect_xmax - rect_xmin) / 6,
      region == "Inside Q2-1" ~ rect_xmin + (rect_xmax - rect_xmin) / 2,
      region == "Inside Q2-2" ~ rect_xmin + (rect_xmax - rect_xmin) / 2,
      region == "Inside Q2-3" ~ rect_xmin + (rect_xmax - rect_xmin) / 2,
      region == "Inside Q3-1" ~ rect_xmin + 5 * (rect_xmax - rect_xmin) / 6,
      region == "Inside Q3-2" ~ rect_xmin + 5 * (rect_xmax - rect_xmin) / 6,
      region == "Inside Q3-3" ~ rect_xmin + 5 * (rect_xmax - rect_xmin) / 6,
      TRUE ~ mean(c(rect_xmin, rect_xmax)) # Fallback
    ),
    y = case_when(
      # Outer positions
      region == "High Q1" ~ rect_ymax + 0.5,
      region == "High Q2" ~ rect_ymax + 0.5,
      region == "High Q3" ~ rect_ymax + 0.5,
      region == "High and Inside" ~ rect_ymax + 0.5,
      region == "High and Outside" ~ rect_ymax + 0.5,
      region == "Outside Q1" ~ rect_ymin + (rect_ymax - rect_ymin) / 6,
      region == "Outside Q2" ~ rect_ymin + (rect_ymax - rect_ymin) / 2,
      region == "Outside Q3" ~ rect_ymin + 5 * (rect_ymax - rect_ymin) / 6,
      region == "Inside Q1" ~ rect_ymin + (rect_ymax - rect_ymin) / 6,
      region == "Inside Q2" ~ rect_ymin + (rect_ymax - rect_ymin) / 2,
      region == "Inside Q3" ~ rect_ymin + 5 * (rect_ymax - rect_ymin) / 6,
      region == "Low Q1" ~ rect_ymin - 0.5,
      region == "Low Q2" ~ rect_ymin - 0.5,
      region == "Low Q3" ~ rect_ymin - 0.5,
      region == "Low and Inside" ~ rect_ymin - 0.5,
      region == "Low and Outside" ~ rect_ymin - 0.5,
      # Hard coded positions for inside quadrants
      region == "Inside Q1-1" ~ rect_ymin + (rect_ymax - rect_ymin) / 6,
      region == "Inside Q1-2" ~ rect_ymin + (rect_ymax - rect_ymin) / 2,
      region == "Inside Q1-3" ~ rect_ymin + 5 * (rect_ymax - rect_ymin) / 6,
      region == "Inside Q2-1" ~ rect_ymin + (rect_ymax - rect_ymin) / 6,
      region == "Inside Q2-2" ~ rect_ymin + (rect_ymax - rect_ymin) / 2,
      region == "Inside Q2-3" ~ rect_ymin + 5 * (rect_ymax - rect_ymin) / 6,
      region == "Inside Q3-1" ~ rect_ymin + (rect_ymax - rect_ymin) / 6,
      region == "Inside Q3-2" ~ rect_ymin + (rect_ymax - rect_ymin) / 2,
      region == "Inside Q3-3" ~ rect_ymin + 5 * (rect_ymax - rect_ymin) / 6,
      TRUE ~ mean(c(rect_ymin, rect_ymax)) # Fallback
    )
  )

###############################################################################

# Define the percentile_rank function
percentile_rank <- function(data, value) {
  # Remove NA values
  data <- na.omit(data)
  
  # Calculate the percentile rank
  percentile <- ecdf(data)(value) * 100
  
  return(percentile)
}

percentile_rank(ncaa2024_fb$extension, 6.74)
percentile_rank(ncaa2024_fb$relspeed, 95.5)
percentile_rank(ncaa2024_fb$inducedvertbreak, 16.5)
percentile_rank(ncaa2024_fb$spinrate, 2150)
percentile_rank(ncaa2024_fb$relheight, 5.62)

# Calculate the percentile rank for each region in Ahmad's data compared to ncaa2024_fb data
avg_vert_angle_ahmad <- avg_vert_angle_ahmad %>%
  group_by(region) %>%
  mutate(pct_rank_ahmad = percentile_rank(ncaa2024_fb %>% filter(region == first(region)) %>% pull(vertapprangle), avg_angle)) %>%
  ungroup()

# Display the updated dataframe
print(avg_vert_angle_ahmad, n = 50)

###############################################################################

# Filter out regions with avg_angle == 0
avg_vert_angle_filtered_ahmad <- avg_vert_angle_ahmad %>%
  filter(avg_angle != 0)

###############################################################################

# Generate the plot for average VertApprAngle with red-blue shading
ggplot(data = ahmad, mapping = aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType)) +
  xlim(c(-2, 2)) +
  ylim(c(0, 5)) +
  coord_fixed(0.8) +
  annotate(
    'rect', xmin = rect_xmin, xmax = rect_xmax, ymin = rect_ymin,
    ymax = rect_ymax, color = "black", fill = "black", alpha = 0.2
  ) +
  geom_tile(data = avg_vert_angle_filtered_ahmad, aes(x = x, y = y, fill = avg_angle), inherit.aes = FALSE, alpha = 0.5) +
  geom_text(data = avg_vert_angle_filtered_ahmad, aes(x = x, y = y, label = sprintf("%.1f", avg_angle)),
            inherit.aes = FALSE, size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", midpoint = mean(avg_vert_angle_filtered$avg_angle), guide = "none") +
  labs(
    title = "Ahmad Harajli Average Fastball VAA",
    subtitle = "Pitcher's Perspective",
    x = NULL,
    y = NULL,
    color = "Pitch Type",
    fill = "VertApprAngle",
    caption = "Miami Baseball Fall 2024"
  ) +
  theme_classic() +
  theme(panel.grid.minor = element_blank())

##############################################################################

ggplot(data = ahmad, mapping = aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType)) +
  xlim(c(-2, 2)) +
  ylim(c(0, 5)) +
  coord_fixed(0.8) +
  annotate(
    'rect', xmin = rect_xmin, xmax = rect_xmax, ymin = rect_ymin,
    ymax = rect_ymax, color = "black", fill = "black", alpha = 0.2
  ) +
  geom_tile(data = avg_vert_angle_filtered_ahmad, aes(x = x, y = y, fill = pct_rank_ahmad), inherit.aes = FALSE, alpha = 0.5) +
  geom_text(data = avg_vert_angle_filtered_ahmad, aes(x = x, y = y, label = sprintf("%.1f", pct_rank_ahmad)),
            inherit.aes = FALSE, size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", midpoint = 50, guide = "none") +
  labs(
    title = "Ahmad Harajli VAA Percentile Rank",
    subtitle = "Pitcher's Perspective",
    x = NULL,
    y = NULL,
    color = "Pitch Type",
    fill = "VertApprAngle",
    caption = "Miami Baseball Fall 2024"
  ) +
  theme_classic() +
  theme(panel.grid.minor = element_blank())

##############################################################################

ahmad_sw_str <- ahmad %>% 
  filter(region %in% c("Inside Q2-1", "Inside Q2-2","Inside Q2-3", "Inside Q3-1",
                       "Inside Q3-2", "Inside Q3-3")) %>% 
  summarize(swStr = round(sum(PitchCall %in% c("StrikeSwinging"))/n()*100, 1))
ahmad_sw_str

##############################################################################

prostuff <- read_csv("prostuff.csv")

ahmad_pca <- ahmad %>% 
  filter(TaggedPitchType == "Fastball") %>% 
  rename(player_name = Pitcher,
         spin_rate = SpinRate,
         velocity = RelSpeed,
         release_extension = Extension,
         release_pos_z = RelHeight,
         release_pos_x = RelSide,
         pitcher_break_z_induced = InducedVertBreak,
         pitcher_break_x = HorzBreak) %>% 
  group_by(player_name) %>% 
  summarize(spin_rate = mean(spin_rate),
            velocity = 94.7,
            release_extension = mean(release_extension),
            release_pos_z = mean(release_pos_z),
            release_pos_x = mean(release_pos_x),
            pitcher_break_z_induced = mean(pitcher_break_z_induced, na.rm=T),
            pitcher_break_x = mean(pitcher_break_x, na.rm=T))

pro_movement <- read_csv("pitch_movement.csv") %>% 
  rename(player_name = `last_name, first_name`) %>% 
  select(player_name, pitcher_break_z_induced, pitcher_break_x)

pro <- right_join(prostuff, pro_movement) %>% 
  mutate(release_pos_x = abs(release_pos_x))

prostuff_with_ahmad <- bind_rows(pro, ahmad_pca)
write_csv(prostuff_with_ahmad, "ahmad_pca.csv")

##############################################################################

ggplot(ahmad_full, aes(x = HorzBreak,
                       y = InducedVertBreak,
                       color = TaggedPitchType)) +
  geom_point(size = 1.5, alpha = 0.7) +
  xlim(-22, 22) +
  ylim(-22, 22) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  theme_minimal() +
  labs(title = "Pitch Movement",
       x = "Horizontal Break",
       y = "Induced Vertical Break") +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"))

