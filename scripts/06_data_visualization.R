# ==================================================
# COPUS Segmentation Framework
# 06_data_visualization.R
# Purpose: Generate manuscript figures from final segmentation outputs
# Figures generated:
#   - Figure 2: Example class session visualization
#   - Figure 3a: Segment duration by detector
#   - Figure 3b: Number of segments per class by detector
# ==================================================
source("scripts/00_setup.R")

# ==== LOAD FINAL SEGMENTATION CACHE ===========================================

cache_05_path <- "cache/05_final_outputs.rds"

if (!file.exists(cache_05_path)) {
  stop(
    "Missing cache file: ",
    cache_05_path,
    "\nRun scripts 01, 02, 04, and 05 before running script 06.",
    call. = FALSE
  )
}

cache_05 <- readRDS(cache_05_path)

required_cache_05_objects <- c(
  "master_data",
  "interval_labels_final_with_tertiary",
  "lecture_segments",
  "clicker_segments",
  "pi_segments",
  "tps_segments",
  "peer_lite_segments",
  "clicker_lite_segments",
  "admin_segments",
  "student_work_segments",
  "instructorQA_segments",
  "studentQA_segments",
  "transition_segments"
)

missing_cache_05_objects <- setdiff(
  required_cache_05_objects,
  names(cache_05)
)

if (length(missing_cache_05_objects) > 0L) {
  stop(
    "Cache 05 is missing required object(s): ",
    paste(missing_cache_05_objects, collapse = ", "),
    "\nRe-run scripts/05_label_tertiary_coverage.R.",
    call. = FALSE
  )
}

master_data <- cache_05$master_data
interval_labels_final_with_tertiary <-
  cache_05$interval_labels_final_with_tertiary

lecture_segments <- cache_05$lecture_segments
clicker_segments <- cache_05$clicker_segments
pi_segments <- cache_05$pi_segments
tps_segments <- cache_05$tps_segments
peer_lite_segments <- cache_05$peer_lite_segments
clicker_lite_segments <- cache_05$clicker_lite_segments
admin_segments <- cache_05$admin_segments
student_work_segments <- cache_05$student_work_segments

instructorQA_segments <- cache_05$instructorQA_segments
studentQA_segments <- cache_05$studentQA_segments
transition_segments <- cache_05$transition_segments

rm(
  cache_05,
  cache_05_path,
  required_cache_05_objects,
  missing_cache_05_objects
)

message(
  "Loaded final segmentation cache: ",
  "cache/05_final_outputs.rds"
)

# Ensure output directories exist before writing figures and tables.
dir.create(
  "figures",
  showWarnings = FALSE,
  recursive = TRUE
)

dir.create(
  "outputs",
  showWarnings = FALSE,
  recursive = TRUE
)

# ==== Figure 2 ====
# 1. Find the example class with the most distinct profiles --------------------
# 1.1 Summarize profile diversity + minutes per class ------
# uses: interval_labels_final_with_tertiary (id, time, profile)
# ranks by: n_labels (excluding Unlabeled), labeled_minutes, total_minutes
class_profile_summary <- interval_labels_final_with_tertiary %>%
  group_by(id) %>%
  summarise(
    total_intervals = n(),
    total_minutes   = total_intervals * 2,
    labeled_intervals = sum(profile != "Unlabeled"),
    labeled_minutes   = labeled_intervals * 2,
    n_labels = n_distinct(profile[profile != "Unlabeled"]),
    .groups = "drop"
  ) %>%
  arrange(desc(n_labels), desc(labeled_minutes), desc(total_minutes))

# 1.2 Choose the class id to plot ----
top_id <- class_profile_summary$id[30]
top_id

# 1.3 Shared setting color palette -----
profile_palette <- c(
  "TPS"             = "#009E73",
  "PeerInstruction" = "#E69F00",
  "Clicker"         = "#C44E52",
  "Lecture"         = "#0072B2",
  "PeerLite"        = "#FDB863",
  "ClickerLite"     = "#F4A3A3",
  "StudentWork"     = "#F0E442",
  "Admin"           = "#D65F9E",
  "StudentQA"       = "#6A51A3",
  "InstructorQA"    = "#B39DDB",
  "Transition"      = "#6E6E6E",
  "Unlabeled"       = "#D9D9D9"
)

profile_order <- names(profile_palette)

# 2. Build interval-level timeline data with segment-based x positions ---------
# set width for segments
gap_width <- 0.8 

# Create interval-level timeline data and assign each interval to a detected segment
timeline_base <- interval_labels_final_with_tertiary %>%
  filter(id == top_id) %>%
  arrange(time) %>%
  mutate(
    profile = trimws(profile),
    interval_num = row_number(),
    new_run = (row_number() == 1) |
      (time != lag(time) + 1) |
      (profile != lag(profile)),
    run_id = cumsum(new_run)
  )

# create one row per detected segment and 
# assign x-axis coordinates so segment bars align with COPUS code columns
segment_df <- timeline_base %>%
  group_by(id, run_id, profile) %>%
  summarise(
    start_time = min(time),
    end_time = max(time),
    start_interval = min(interval_num),
    end_interval = max(interval_num),
    n_intervals = n(),
    minutes = n_intervals * 2,
    .groups = "drop"
  ) %>%
  arrange(run_id) %>%
  mutate(
    segment_label = paste0("S", row_number()),
    start_x = cumsum(lag(n_intervals, default = 0)) +
      (row_number() - 1) * gap_width + 1,
    end_x = start_x + n_intervals - 1,
    xmin = start_x - 0.5,
    xmax = end_x + 0.5,
    xmid = (xmin + xmax) / 2,
    profile = factor(profile, levels = profile_order)
  )

# build plotting coordinate system: 
# place intervals within their corresponding segment 
# so the timeline and COPUS matrix align on the same x-axis
timeline_df <- timeline_base %>%
  group_by(run_id) %>%
  mutate(within_segment_index = row_number() - 1) %>%
  ungroup() %>%
  left_join(
    segment_df %>% select(run_id, start_x, segment_label),
    by = "run_id"
  ) %>%
  mutate(
    x_plot = start_x + within_segment_index
  )

# 3. Build COPUS code matrix data ----------------------------------------------
# COPUS code order shown in the matrix
instructor_codes <- c(
  "Instructor.Lec", "Instructor.RtW", "Instructor.FUp", "Instructor.PQ",
  "Instructor.CQ", "Instructor.AnQ", "Instructor.MG", "Instructor.1o1",
  "Instructor.DV", "Instructor.Adm", "Instructor.W", "Instructor.Other"
)

student_codes <- c(
  "Student.L", "Student.Ind", "Student.CG", "Student.WG", "Student.OG",
  "Student.AnQ", "Student.SQ", "Student.WC", "Student.Prd",
  "Student.SP", "Student.TQ", "Student.W", "Student.Other"
)

code_order <- c(instructor_codes, student_codes)

code_labels <- c(
  "Instructor.Lec" = "Lec",
  "Instructor.RtW" = "RtW",
  "Instructor.FUp" = "FUp",
  "Instructor.PQ" = "PQ",
  "Instructor.CQ" = "CQ",
  "Instructor.AnQ" = "AnQ",
  "Instructor.MG" = "MG",
  "Instructor.1o1" = "1o1",
  "Instructor.DV" = "D/V",
  "Instructor.Adm" = "Adm",
  "Instructor.W" = "W",
  "Instructor.Other" = "O",
  "Student.L" = "L",
  "Student.Ind" = "Ind",
  "Student.CG" = "CG",
  "Student.WG" = "WG",
  "Student.OG" = "OG",
  "Student.AnQ" = "AnQ",
  "Student.SQ" = "SQ",
  "Student.WC" = "WC",
  "Student.Prd" = "Prd",
  "Student.SP" = "SP",
  "Student.TQ" = "TQ",
  "Student.W" = "W",
  "Student.Other" = "O"
)

# join interval plotting positions to raw COPUS codes, 
# then reshape to one row per interval-code pair for geom_tile()
raw_code_long <- master_data %>%
  filter(id == top_id) %>%
  arrange(time) %>%
  left_join(
    timeline_df %>% 
      dplyr::select(time, interval_num, x_plot),
    by = "time"
  ) %>%
  dplyr::select(id, time, interval_num, x_plot, all_of(code_order)) %>%
  pivot_longer(
    cols = all_of(code_order),
    names_to = "code",
    values_to = "present"
  ) %>%
  mutate(
    present = as.numeric(present),
    code_short = code_labels[code],
    role = if_else(grepl("^Instructor", code), "Instructor", "Student")
  )

# 4. Add y-axis positions for matrix -------------------------------------------
# desired top-to-bottom order
code_order_top_to_bottom <- c(
  instructor_codes,
  student_codes
)

# create y-position lookup table for COPUS codes
# with a gap separating Instructor and Student codes

instructor_y <- rev(seq(15, 26))
student_y    <- rev(seq(1, 13))

code_y_df <- tibble(
  code = code_order_top_to_bottom,
  code_short = code_labels[code_order_top_to_bottom],
  y_pos = c(instructor_y, student_y)
)

raw_code_long <- raw_code_long %>%
  left_join(code_y_df, by = c("code", "code_short"))

# y positions for role labels
instructor_y_mid <- raw_code_long %>%
  filter(role == "Instructor") %>%
  summarise(y = mean(range(y_pos))) %>%
  pull(y)

student_y_mid <- raw_code_long %>%
  filter(role == "Student") %>%
  summarise(y = mean(range(y_pos))) %>%
  pull(y)

x_min <- min(timeline_df$x_plot) - 0.5
x_max <- max(timeline_df$x_plot) + 0.5

# 5. Combined : timeline + COPUS code matrix in one ggplot ---------------------
# fill color for COPUS matrix cells
raw_code_long <- raw_code_long %>%
  mutate(tile_fill = if_else(present == 1, "black", "white"))

# define vertical positions for the top timeline
matrix_top_y <- max(code_y_df$y_pos, na.rm = TRUE)

timeline_bar_ymin <- matrix_top_y + 4.0
timeline_bar_ymax <- matrix_top_y + 5.0
interval_num_y    <- matrix_top_y + 6.0
segment_label_y   <- matrix_top_y + 2.9
class_label_y     <- matrix_top_y + 7.2

# x-axis limits
x_min <- min(timeline_df$x_plot, na.rm = TRUE) - 0.5
x_max <- max(timeline_df$x_plot, na.rm = TRUE) + 0.5

# left-side label position
left_label_x <- x_min - 3.8      # Instructor/Student COPUS Codes
code_label_x <- x_min - 1.1      # Lec, RtW, FUp...

# only show detectors present in this class
present_profiles <- segment_df %>%
  pull(profile) %>%
  as.character() %>%
  unique()

present_profiles <- intersect(profile_order, present_profiles)

# draw detected segments and raw COPUS codes in a shared coordinate system
figure2_copus_timeline <- ggplot() +
  
  # ---- COPUS code matrix ----
geom_tile(
  data = raw_code_long,
  aes(x = x_plot, y = y_pos),
  fill = raw_code_long$tile_fill,
  color = "black",
  linewidth = 0.25,
  width = 1,
  height = 1
) +
  geom_text(
    data = code_y_df,
    aes(x = code_label_x, y = y_pos, label = code_short),
    hjust = 1,
    size = 3.5
  ) +
  
  # ---- Detected segment timeline ----
geom_rect(
  data = segment_df,
  aes(
    xmin = xmin,
    xmax = xmax,
    ymin = timeline_bar_ymin,
    ymax = timeline_bar_ymax,
    fill = profile
  ),
  color = "white",
  linewidth = 0.25
) +
  
  # ---- COPUS interval numbers ----
geom_text(
  data = timeline_df,
  aes(x = x_plot, y = interval_num_y, label = interval_num),
  size = 3.5,
  fontface = "bold"
) +
  
  # ---- Segment labels S1, S2, ... ----
geom_text(
  data = segment_df,
  aes(x = xmid, y = segment_label_y, label = segment_label),
  size = 4,
  fontface = "bold"
) +
  
  # ---- Beginning / end labels ----
annotate(
  "text",
  x = x_min,
  y = class_label_y,
  label = "| beginning of class",
  hjust = 0,
  size = 4,
  fontface = "bold"
) +
  annotate(
    "text",
    x = x_max,
    y = class_label_y,
    label = "end of class |",
    hjust = 1,
    size = 4,
    fontface = "bold"
  ) +
  
  # ---- Instructor / Student y-axis group labels ----
annotate(
  "text",
  x = left_label_x,
  y = instructor_y_mid,
  label = "Instructor COPUS Codes",
  angle = 90,
  fontface = "bold",
  size = 4
) +
  annotate(
    "text",
    x = left_label_x,
    y = student_y_mid,
    label = "Student COPUS Codes",
    angle = 90,
    fontface = "bold",
    size = 4
  ) +
  
  # ---- scales ----
scale_fill_manual(
  values = profile_palette,
  breaks = present_profiles,
  drop = TRUE,
  name = "Detector"
) +
  scale_y_continuous(
    breaks = NULL,
    limits = c(0, class_label_y + 1),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    limits = c(left_label_x - 0.5, x_max),
    breaks = NULL,
    expand = c(0, 0)
  ) +
  
  coord_cartesian(clip = "off") +
  
  labs(x = NULL, y = NULL) +
  
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.key.size = unit(0.8, "cm"),
    
    plot.margin = margin(10, 10, 10, 40)
  )

figure2_copus_timeline
# ==== Figure 3 Shared setting ====
detector_order <- c(
  "TPS",
  "PeerInstruction",
  "Clicker",
  "Lecture",
  "PeerLite",
  "ClickerLite",
  "StudentWork",
  "Admin",
  "StudentQA",
  "InstructorQA",
  "Transition"
)

detector_tier <- c(
  "TPS" = "Primary",
  "PeerInstruction" = "Primary",
  "Clicker" = "Primary",
  "Lecture" = "Primary",
  "PeerLite" = "Secondary",
  "ClickerLite" = "Secondary",
  "StudentWork" = "Secondary",
  "Admin" = "Secondary",
  "StudentQA" = "Tertiary",
  "InstructorQA" = "Tertiary",
  "Transition" = "Tertiary"
)

tier_shape <- c(
  "Primary" = 16,    # circle
  "Secondary" = 17,  # triangle
  "Tertiary" = 15    # square
)

profile_palette <- c(
  "TPS"             = "#009E73",       # green  
  "PeerInstruction" = "#E69F00",       # orange
  "Clicker"         = "#C44E52",       # red
  "Lecture"         = "#0072B2",       # blue
  
  "PeerLite"        = "#FDB863",       # light orange
  "ClickerLite"     = "#F4A3A3",       # light red
  "StudentWork"     = "#F0E442",       # yellow
  "Admin"           = "#D65F9E",       # purple pink
  
  "StudentQA"       = "#6A51A3",       # mid purple
  "InstructorQA"    = "#B39DDB",       # light purple
  "Transition"      = "#6E6E6E",       # mid grey
  
  "Unlabeled"       = "#D9D9D9"        # light grey
)

# prettier x-axis labels
detector_labels <- c(
  "TPS" = "TPS",
  "PeerInstruction" = "Peer\nInstruction",
  "Clicker" = "Clicker",
  "Lecture" = "Lecture",
  "PeerLite" = "Peer-Lite",
  "ClickerLite" = "Clicker-Lite",
  "StudentWork" = "Student\nWork",
  "Admin" = "Admin",
  "StudentQA" = "Student\nQA",
  "InstructorQA" = "Instructor\nQA",
  "Transition" = "Transition"
)

# Combine all segment tables
all_segments <- bind_rows(
  tps_segments              %>% mutate(detector = "TPS"),
  pi_segments               %>% mutate(detector = "PeerInstruction"),
  clicker_segments          %>% mutate(detector = "Clicker"),
  lecture_segments          %>% mutate(detector = "Lecture"),
  peer_lite_segments        %>% mutate(detector = "PeerLite"),
  clicker_lite_segments     %>% mutate(detector = "ClickerLite"),
  student_work_segments     %>% mutate(detector = "StudentWork"),
  admin_segments            %>% mutate(detector = "Admin"),
  studentQA_segments        %>% mutate(detector = "StudentQA"),
  instructorQA_segments     %>% mutate(detector = "InstructorQA"),
  transition_segments       %>% mutate(detector = "Transition")
) %>%
  mutate(
    detector = factor(detector, levels = detector_order),
    tier = detector_tier[as.character(detector)],
    tier = factor(tier, levels = c("Primary", "Secondary", "Tertiary"))
  )

# ==== Figure 3a ====
fig3a <- ggplot(all_segments, aes(x = detector, y = minutes, fill = detector)) +
  geom_boxplot(
    width = 0.65,
    outlier.shape = NA,
    alpha = 0.9,
    color = "grey25"
  ) +
  geom_jitter(
    aes(color = detector, shape = tier),
    width = 0.18,
    alpha = 0.15,
    size = 1.5,
    stroke = 0.2
  ) +
  scale_fill_manual(values = profile_palette, guide = "none") +
  scale_color_manual(values = profile_palette, guide = "none") +
  scale_shape_manual(values = tier_shape, name = "Detector Tier") +
  scale_x_discrete(labels = detector_labels, drop = FALSE) +
  labs(
    title = "Figure 3a. Segment Duration Statistics by Detector",
    x = NULL,
    y = "Segment duration (minutes)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "right",
    plot.title = element_text(face = "bold")
  ) +
  guides(shape = guide_legend(override.aes = list(size = 4))
  )

fig3a

# ==== Figure 3b ====
# 1. class length (min) for each class
class_length <- master_data %>%
  group_by(id) %>%
  summarise(
    total_intervals = n(),
    total_minutes = total_intervals * 2,
    .groups = "drop"
  )

# 2. number of segment per class per detector
segments_per_class <- all_segments %>%
  group_by(id, detector, tier) %>%
  summarise(
    n_segments = n(),
    .groups = "drop"
  )

# 3. full grid: keep classes with zero segments
class_detector_grid <- tidyr::expand_grid(
  id = unique(master_data$id),
  detector = factor(detector_order, levels = detector_order)
) %>%
  mutate(
    tier = detector_tier[as.character(detector)],
    tier = factor(tier, levels = c("Primary", "Secondary", "Tertiary"))
  )

segments_per_class_full <- class_detector_grid %>%
  left_join(segments_per_class, by = c("id", "detector", "tier")) %>%
  mutate(n_segments = if_else(is.na(n_segments), 0L, n_segments)) %>%
  left_join(class_length, by = "id") %>%
  mutate(
    segments_per_50min = (n_segments / total_minutes) * 50
  )

# 4. plot: normalized number of segments per class (per 50 min)
fig3b <- ggplot(segments_per_class_full, aes(x = detector, y = segments_per_50min, fill = detector)) +
  geom_boxplot(
    width = 0.65,
    outlier.shape = NA,
    alpha = 0.9,
    color = "grey25"
  ) +
  geom_jitter(
    aes(color = detector, shape = tier),
    width = 0.18,
    alpha = 0.15,
    size = 1.5,
    stroke = 0.2
  ) +
  scale_fill_manual(values = profile_palette, guide = "none") +
  scale_color_manual(values = profile_palette, guide = "none") +
  scale_shape_manual(values = tier_shape, name = "Detector Tier") +
  scale_x_discrete(labels = detector_labels, drop = FALSE) +
  labs(
    title = "Figure 3b. Class-Level Structural Metrics by Detector",
    x = NULL,
    y = "Number of segments per class (normalized to 50 minutes)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "right",
    plot.title = element_text(face = "bold")
  ) +
  guides(shape = guide_legend(override.aes = list(size = 4))
  )

fig3b
# ==== Median summary for figure 3a and b ====
# median segment duration y detector
fig3a_summary <- all_segments %>%
  group_by(detector, tier) %>%
  summarise(
    n_segments = n(),
    median_duration = median(minutes, na.rm = TRUE),
    mean_duration = mean(minutes, na.rm = TRUE),
    iqr_duration = IQR(minutes, na.rm = TRUE),
    min_duration = min(minutes, na.rm = TRUE),
    max_duration = max(minutes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(detector)

# median number of segments per class (per 50 min) by detector
fig3b_summary <- segments_per_class_full %>%
  group_by(detector, tier) %>%
  summarise(
    n_classes = n(),
    median_segments_per_50min = median(segments_per_50min, na.rm = TRUE),
    mean_segments_per_50min = mean(segments_per_50min, na.rm = TRUE),
    iqr_segments_per_50min = IQR(segments_per_50min, na.rm = TRUE),
    min_segments_per_50min = min(segments_per_50min, na.rm = TRUE),
    max_segments_per_50min = max(segments_per_50min, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(detector)

# supplementary table
detector_medians <- fig3a_summary %>%
  select(detector, tier, n_segments, median_duration) %>%
  left_join(
    fig3b_summary %>%
      select(detector, median_segments_per_50min),
    by = "detector"
  ) %>%
  arrange(detector)

# 6. Save outputs --------------------------------------------------------------
ggsave("figures/Figure2_example_session.png", figure2_copus_timeline, width = 14, height = 8, dpi = 300)
ggsave("figures/Figure3a_segment_duration.png", fig3a, width = 8, height = 6, dpi = 300)
ggsave("figures/Figure3b_segment_counts.png", fig3b, width = 8, height = 6, dpi = 300)
write_csv(detector_medians,"outputs/detector_medians.csv")