###############################################################################

# NCAA VAA Analysis
# Will Paz 
# 12.27.24

###############################################################################
# Load packages
library(tidyverse)

###############################################################################

# Read in the data
ncaa2024 <- read_csv("2024TMDataWhole.csv")
ncaa2024_fb <- ncaa2024 %>% 
  filter(taggedpitchtype %in% c("Fastball","FourSeamFastBall"),
         !is.na(vertapprangle))

###############################################################################

#----------------------- NCAA Avg VAA Location Adj ----------------------------

###############################################################################

# Define rectangle bounds
rect_xmin <- -0.85
rect_xmax <- 0.85
rect_ymin <- 1.5
rect_ymax <- 3.5

# Define all possible regions
all_regions <- tibble(region = c("High Q1", "High Q2", "High Q3", "High and In", "High and Out",
                                "High and Inside", "High and Outside", "Inside Q1", "Inside Q2",
                                "Inside Q3", "Inside Q1-1", "Inside Q1-2","Inside Q1-3", "Inside Q2-1",
                                "Inside Q2-2","Inside Q2-3", "Inside Q3-1", "Inside Q3-2", "Inside Q3-3",
                                "Low Q1", "Low Q2", "Low Q3", "Low and In", "Low and Out","Low and Inside",
                                "Low and Outside", "Outside Q1", "Outside Q2", "Outside Q3"))

# Define regions with clear mapping for inside and outside
ncaa2024_fb <- ncaa2024_fb %>%
  mutate(
    region = case_when(
      platelocside >= rect_xmin & platelocside <= rect_xmax &
        platelocheight >= rect_ymax ~ paste0(
          "High Q",
          findInterval(platelocside, seq(rect_xmin, rect_xmax, length.out = 4), rightmost.closed = TRUE)
        ),
      platelocside >= rect_xmin & platelocside <= rect_xmax &
        platelocheight <= rect_ymin ~ paste0(
          "Low Q",
          findInterval(platelocside, seq(rect_xmin, rect_xmax, length.out = 4), rightmost.closed = TRUE)
        ),
      platelocside >= rect_xmin & platelocside <= rect_xmax &
        platelocheight >= rect_ymin & platelocheight <= rect_ymax ~ paste0(
          "Inside Q",
          findInterval(platelocside, seq(rect_xmin, rect_xmax, length.out = 4), rightmost.closed = TRUE),
          "-",
          findInterval(platelocheight, seq(rect_ymin, rect_ymax, length.out = 4), rightmost.closed = TRUE)
        ),
      platelocheight > rect_ymax & platelocside < rect_xmin ~ "High and Inside",
      platelocheight > rect_ymax & platelocside > rect_xmax ~ "High and Outside",
      platelocheight < rect_ymin & platelocside < rect_xmin ~ "Low and Inside",
      platelocheight < rect_ymin & platelocside > rect_xmax ~ "Low and Outside",
      platelocside < rect_xmin ~ paste0(
        "Inside Q",
        findInterval(platelocheight, seq(rect_ymin, rect_ymax, length.out = 4), rightmost.closed = TRUE)
      ),
      platelocside > rect_xmax ~ paste0(
        "Outside Q",
        findInterval(platelocheight, seq(rect_ymin, rect_ymax, length.out = 4), rightmost.closed = TRUE)
      ),
      TRUE ~ "Unknown"
    )
  )


###############################################################################

# Calculate average VertApprAngle for each region
avg_vert_angle <- ncaa2024_fb %>%
  group_by(region) %>%
  summarize(avg_angle = mean(vertapprangle, na.rm = TRUE)) %>%
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

# Filter out regions with avg_angle == 0
avg_vert_angle_filtered <- avg_vert_angle %>%
  filter(avg_angle != 0)

###############################################################################

# Generate the plot for average VertApprAngle with red-blue shading
ggplot(data = ncaa2024_fb, mapping = aes(x = platelocside, y = platelocheight, color = taggedpitchtype)) +
  xlim(c(-2, 2)) +
  ylim(c(0, 5)) +
  coord_fixed(0.8) +
  annotate(
    'rect', xmin = rect_xmin, xmax = rect_xmax, ymin = rect_ymin,
    ymax = rect_ymax, color = "black", fill = "black", alpha = 0.2
  ) +
  geom_tile(data = avg_vert_angle_filtered, aes(x = x, y = y, fill = avg_angle), inherit.aes = FALSE, alpha = 0.5) +
  geom_text(data = avg_vert_angle_filtered, aes(x = x, y = y, label = sprintf("%.1f", avg_angle)),
            inherit.aes = FALSE, size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", midpoint = mean(avg_vert_angle_filtered$avg_angle), guide = "none") +
  labs(
    title = "NCAA Average Fastball VAA",
    subtitle = "Pitcher's Perspective",
    x = NULL,
    y = NULL,
    color = "Pitch Type",
    fill = "VertApprAngle",
    caption = "NCAA Baseball 2024 - Trackman"
  ) +
  theme_classic() +
  theme(panel.grid.minor = element_blank())

###############################################################################

ggplot(ncaa2024_fb, aes(x=vertapprangle)) +
  geom_density()

ggplot(ncaa2024_fb, aes(x=vertapprangle)) +
  geom_boxplot()

summary(ncaa2024_fb$vertapprangle)

percentile_rank(ncaa2024_fb$vertapprangle, mean(ncaa2024_fb$vertapprangle, na.rm = T))

quantile(ncaa2024_fb$vertapprangle, probs = .9, na.rm=T)

quantile(ncaa2024_fb$vertapprangle, probs = .1, na.rm=T)

sw_str <- ncaa2024_fb %>% 
  group_by(region) %>% 
  mutate(vaa_tier = case_when(
    vertapprangle > quantile(vertapprangle, probs = .9, na.rm=T) ~ "Elite",
    vertapprangle < quantile(vertapprangle, probs = .9, na.rm=T) &
      vertapprangle > quantile(vertapprangle, probs = .6, na.rm=T) ~ "Above Average",
    vertapprangle < quantile(vertapprangle, probs = .6, na.rm=T) &
      vertapprangle > quantile(vertapprangle, probs = .4, na.rm=T) ~ "Average",
    vertapprangle < quantile(vertapprangle, probs = .4, na.rm=T) &
      vertapprangle > quantile(vertapprangle, probs = .25, na.rm=T) ~ "Below Average",
    vertapprangle < quantile(vertapprangle, probs = .25, na.rm=T) ~ "Poor"
  )) %>%
  mutate(vaa_tier = factor(vaa_tier, levels = c("Elite", "Above Average", "Average", "Below Average", "Poor")))


sw_str_clean <- sw_str %>% 
  group_by(region, vaa_tier) %>% 
  filter(!is.na(vaa_tier)) %>% 
  summarize(swStr = round(sum(pitchcall %in% c("StrikeSwinging"))/n()*100, 1)) %>% 
  right_join(all_regions, by = "region") %>%
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

glimpse(sw_str_clean)

###############################################################################

# Elite

sw_str_filtered <- sw_str_clean %>% 
  filter(vaa_tier == "Elite")

# Generate the plot for average VertApprAngle with red-blue shading
ggplot(data = ncaa2024_fb, mapping = aes(x = platelocside, y = platelocheight, color = taggedpitchtype)) +
  xlim(c(-2, 2)) +
  ylim(c(0, 5)) +
  coord_fixed(0.8) +
  annotate(
    'rect', xmin = rect_xmin, xmax = rect_xmax, ymin = rect_ymin,
    ymax = rect_ymax, color = "black", fill = "black", alpha = 0.2
  ) +
  geom_tile(data = sw_str_filtered, aes(x = x, y = y, fill = swStr), inherit.aes = FALSE, alpha = 0.5) +
  geom_text(data = sw_str_filtered, aes(x = x, y = y, label = sprintf("%.1f", swStr)),
            inherit.aes = FALSE, size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", midpoint = mean(sw_str_filtered$swStr), guide = "none") +
  labs(
    title = "SwStr% for Elite NCAA VAA",
    subtitle = "Pitcher's Perspective",
    x = NULL,
    y = NULL,
    color = "Pitch Type",
    fill = "VertApprAngle",
    caption = "NCAA Baseball 2024 - Trackman"
  ) +
  theme_classic() +
  theme(panel.grid.minor = element_blank())

###############################################################################

# Above Average

sw_str_filtered <- sw_str_clean %>% 
  filter(vaa_tier == "Above Average")

# Generate the plot for average VertApprAngle with red-blue shading
ggplot(data = ncaa2024_fb, mapping = aes(x = platelocside, y = platelocheight, color = taggedpitchtype)) +
  xlim(c(-2, 2)) +
  ylim(c(0, 5)) +
  coord_fixed(0.8) +
  annotate(
    'rect', xmin = rect_xmin, xmax = rect_xmax, ymin = rect_ymin,
    ymax = rect_ymax, color = "black", fill = "black", alpha = 0.2
  ) +
  geom_tile(data = sw_str_filtered, aes(x = x, y = y, fill = swStr), inherit.aes = FALSE, alpha = 0.5) +
  geom_text(data = sw_str_filtered, aes(x = x, y = y, label = sprintf("%.1f", swStr)),
            inherit.aes = FALSE, size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", midpoint = mean(sw_str_filtered$swStr), guide = "none") +
  labs(
    title = "SwStr% for Above Average NCAA VAA",
    subtitle = "Pitcher's Perspective",
    x = NULL,
    y = NULL,
    color = "Pitch Type",
    fill = "VertApprAngle",
    caption = "NCAA Baseball 2024 - Trackman"
  ) +
  theme_classic() +
  theme(panel.grid.minor = element_blank())

###############################################################################

# Average

sw_str_filtered <- sw_str_clean %>% 
  filter(vaa_tier == "Average")

# Generate the plot for average VertApprAngle with red-blue shading
ggplot(data = ncaa2024_fb, mapping = aes(x = platelocside, y = platelocheight, color = taggedpitchtype)) +
  xlim(c(-2, 2)) +
  ylim(c(0, 5)) +
  coord_fixed(0.8) +
  annotate(
    'rect', xmin = rect_xmin, xmax = rect_xmax, ymin = rect_ymin,
    ymax = rect_ymax, color = "black", fill = "black", alpha = 0.2
  ) +
  geom_tile(data = sw_str_filtered, aes(x = x, y = y, fill = swStr), inherit.aes = FALSE, alpha = 0.5) +
  geom_text(data = sw_str_filtered, aes(x = x, y = y, label = sprintf("%.1f", swStr)),
            inherit.aes = FALSE, size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", midpoint = mean(sw_str_filtered$swStr), guide = "none") +
  labs(
    title = "SwStr% for Average NCAA VAA",
    subtitle = "Pitcher's Perspective",
    x = NULL,
    y = NULL,
    color = "Pitch Type",
    fill = "VertApprAngle",
    caption = "NCAA Baseball 2024 - Trackman"
  ) +
  theme_classic() +
  theme(panel.grid.minor = element_blank())

###############################################################################

# Below Average

sw_str_filtered <- sw_str_clean %>% 
  filter(vaa_tier == "Below Average")

# Generate the plot for average VertApprAngle with red-blue shading
ggplot(data = ncaa2024_fb, mapping = aes(x = platelocside, y = platelocheight, color = taggedpitchtype)) +
  xlim(c(-2, 2)) +
  ylim(c(0, 5)) +
  coord_fixed(0.8) +
  annotate(
    'rect', xmin = rect_xmin, xmax = rect_xmax, ymin = rect_ymin,
    ymax = rect_ymax, color = "black", fill = "black", alpha = 0.2
  ) +
  geom_tile(data = sw_str_filtered, aes(x = x, y = y, fill = swStr), inherit.aes = FALSE, alpha = 0.5) +
  geom_text(data = sw_str_filtered, aes(x = x, y = y, label = sprintf("%.1f", swStr)),
            inherit.aes = FALSE, size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", midpoint = mean(sw_str_filtered$swStr), guide = "none") +
  labs(
    title = "SwStr% for Below Average NCAA VAA",
    subtitle = "Pitcher's Perspective",
    x = NULL,
    y = NULL,
    color = "Pitch Type",
    fill = "VertApprAngle",
    caption = "NCAA Baseball 2024 - Trackman"
  ) +
  theme_classic() +
  theme(panel.grid.minor = element_blank())

###############################################################################

# Poor

sw_str_filtered <- sw_str_clean %>% 
  filter(vaa_tier == "Poor")

# Generate the plot for average VertApprAngle with red-blue shading
ggplot(data = ncaa2024_fb, mapping = aes(x = platelocside, y = platelocheight, color = taggedpitchtype)) +
  xlim(c(-2, 2)) +
  ylim(c(0, 5)) +
  coord_fixed(0.8) +
  annotate(
    'rect', xmin = rect_xmin, xmax = rect_xmax, ymin = rect_ymin,
    ymax = rect_ymax, color = "black", fill = "black", alpha = 0.2
  ) +
  geom_tile(data = sw_str_filtered, aes(x = x, y = y, fill = swStr), inherit.aes = FALSE, alpha = 0.5) +
  geom_text(data = sw_str_filtered, aes(x = x, y = y, label = sprintf("%.1f", swStr)),
            inherit.aes = FALSE, size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", midpoint = mean(sw_str_filtered$swStr), guide = "none") +
  labs(
    title = "SwStr% for Poor NCAA VAA",
    subtitle = "Pitcher's Perspective",
    x = NULL,
    y = NULL,
    color = "Pitch Type",
    fill = "VertApprAngle",
    caption = "NCAA Baseball 2024 - Trackman"
  ) +
  theme_classic() +
  theme(panel.grid.minor = element_blank())

###############################################################################

library(gt)

sw_str %>% 
  group_by(vaa_tier) %>% 
  filter(!is.na(vaa_tier),
         region %in% c("Inside Q2-1", "Inside Q2-2", "Inside Q2-3", 
                       "Inside Q3-1", "Inside Q3-2", "Inside Q3-3")) %>% 
  summarize(swStr = round(sum(pitchcall %in% c("StrikeSwinging")) / n() * 100, 1)) %>%
  mutate(vaa_tier = factor(vaa_tier, levels = c("Elite", "Above Average", "Average", "Below Average", "Poor"))) %>% # Reordering vaa_tier
  gt() %>%
  tab_header(
    title = "Swinging Strike Percentage by VAA Tier",
  ) %>%
  cols_label(
    vaa_tier = "VAA Tier",
    swStr = "Swinging Strike %"
  ) %>%
  fmt_number(
    columns = swStr,
    decimals = 1
  ) %>%
  tab_options(
    table.font.size = 12,
    heading.align = "center"
  )


