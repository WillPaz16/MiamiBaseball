library(tidyverse)

cuts <- read_csv("~/Downloads/Miami University/Miami Baseball/Miami Shiny/games2025DF.csv") %>% 
  filter(Pitcher == "Cuthbertson, Hayden",
         Date != "2025-02-18")

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
cuts <- cuts %>%
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
avg_vert_angle_cuts <- cuts %>%
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

# Filter out regions with avg_angle == 0
avg_vert_angle_filtered_cuts <- avg_vert_angle_cuts %>%
  filter(avg_angle != 0)

###############################################################################

# Generate the plot for average VertApprAngle with red-blue shading
ggplot(data = cuts, mapping = aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType)) +
  xlim(c(-2, 2)) +
  ylim(c(0, 5)) +
  coord_fixed(0.8) +
  annotate(
    'rect', xmin = rect_xmin, xmax = rect_xmax, ymin = rect_ymin,
    ymax = rect_ymax, color = "black", fill = "black", alpha = 0.2
  ) +
  geom_tile(data = avg_vert_angle_filtered_cuts, aes(x = x, y = y, fill = avg_angle), inherit.aes = FALSE, alpha = 0.5) +
  geom_text(data = avg_vert_angle_filtered_cuts, aes(x = x, y = y, label = sprintf("%.1f", avg_angle)),
            inherit.aes = FALSE, size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", midpoint = mean(avg_vert_angle_filtered_cuts$avg_angle), guide = "none") +
  labs(
    title = "Cuthbertson Average Fastball VAA",
    subtitle = "Pitcher's Perspective",
    x = NULL,
    y = NULL,
    color = "Pitch Type",
    fill = "VertApprAngle",
    caption = "Miami Baseball 2025"
  ) +
  theme_classic() +
  theme(panel.grid.minor = element_blank())
