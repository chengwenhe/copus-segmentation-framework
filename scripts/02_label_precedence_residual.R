# ==================================================
# COPUS Segmentation Framework
# 02_label_precedence_residual.R
# Purpose: Apply precedence rules and create residual intervals
# ==================================================
source("scripts/00_setup.R")

# ==== LOAD PRIMARY/SECONDARY DETECTOR CACHE ===================================

cache_01_path <- "cache/01_primary_secondary_outputs.rds"

if (!file.exists(cache_01_path)) {
  stop(
    "Missing cache file: ",
    cache_01_path,
    "\nRun scripts/01_primary_secondary_detectors.R first.",
    call. = FALSE
  )
}

cache_01 <- readRDS(cache_01_path)

required_cache_01_objects <- c(
  "code_cols",
  "master_data",
  "lecture_segments",
  "clicker_segments",
  "pi_segments",
  "tps_segments",
  "peer_lite_segments",
  "clicker_lite_segments",
  "admin_segments",
  "student_work_segments"
)

missing_cache_01_objects <- setdiff(
  required_cache_01_objects,
  names(cache_01)
)

if (length(missing_cache_01_objects) > 0L) {
  stop(
    "Cache 01 is missing required object(s): ",
    paste(missing_cache_01_objects, collapse = ", "),
    "\nRe-run scripts/01_primary_secondary_detectors.R.",
    call. = FALSE
  )
}

code_cols <- cache_01$code_cols
master_data <- cache_01$master_data
lecture_segments <- cache_01$lecture_segments
clicker_segments <- cache_01$clicker_segments
pi_segments <- cache_01$pi_segments
tps_segments <- cache_01$tps_segments
peer_lite_segments <- cache_01$peer_lite_segments
clicker_lite_segments <- cache_01$clicker_lite_segments
admin_segments <- cache_01$admin_segments
student_work_segments <- cache_01$student_work_segments

rm(
  cache_01,
  cache_01_path,
  required_cache_01_objects,
  missing_cache_01_objects
)

message(
  "Loaded primary/secondary detector cache: ",
  "cache/01_primary_secondary_outputs.rds"
)

# ==== LABEL & COVERAGE ====
# 1. Convert detector segments to interval-level labels ------------------------
# covert ALL segments to interval-level ----------------------------------------
# convert segment table to interval-level rows
label_from_segments <- function(segs_tbl, label) {
  if (nrow(segs_tbl) == 0) {
    return(tibble(id=character(), time=integer(), label=character()))
  }
  segs_tbl %>%
    rowwise() %>%
    do({
      tibble(
        id   = .$id,
        time = seq(.$start_time, .$end_time),
        label = label
      )
    }) %>%
    ungroup()
}

# create interval labels for primary and secondary detectors -------------------
# primary segment labels
lab_lecture       <- label_from_segments(lecture_segments,       "Lecture")
lab_clicker       <- label_from_segments(clicker_segments,       "Clicker")
lab_pi            <- label_from_segments(pi_segments,            "PeerInstruction")
lab_tps           <- label_from_segments(tps_segments,           "TPS")
# secondary segment labels
lab_peer_lite     <- label_from_segments(peer_lite_segments,     "PeerLite")
lab_clicker_lite  <- label_from_segments(clicker_lite_segments,  "ClickerLite")
lab_admin         <- label_from_segments(admin_segments,         "Admin")
lab_student_work  <- label_from_segments(student_work_segments,  "StudentWork")

# 2. Apply precedence rules ----------------------------------------------------
# build labels_all_final with a precedence rule------------------
# rule: TPS > PeerInstruction > Clicker > PeerLite > ClickerLite > StudentWork > Lecture > Admin
labels_all_final <- bind_rows(
  lab_tps              %>% mutate(priority = 1L),  # highest priority
  lab_pi               %>% mutate(priority = 2L),
  lab_clicker          %>% mutate(priority = 3L),
  lab_peer_lite        %>% mutate(priority = 4L),
  lab_clicker_lite     %>% mutate(priority = 5L),
  lab_student_work     %>% mutate(priority = 6L),  
  lab_lecture          %>% mutate(priority = 7L),
  lab_admin            %>% mutate(priority = 8L)   # lowest labeled priority
) %>%
  arrange(id, time, priority) %>%
  group_by(id, time) %>%
  slice(1) %>%                     # keep highest-priority label per interval
  ungroup() %>%
  select(id, time, label)

# 3. Map final labels back to every interval -----------------------------------
# map final labels back to every interval --------------------------------------
interval_grid <- master_data %>%
  select(id, time) %>%
  distinct()

interval_labels_final <- interval_grid %>%
  left_join(labels_all_final, by = c("id", "time")) %>%
  mutate(profile = if_else(is.na(label), "Unlabeled", label)) %>%
  select(id, time, profile)
# 4. Summarize coverage by class session ---------------------------------------
# coverage summary per class (id) ----------------------------------------------
# 1. Basic counts: intervals per profile per class
coverage_counts <- interval_labels_final %>%
  group_by(id, profile) %>%
  summarise(
    n_intervals = n(),
    minutes     = n_intervals * 2,
    .groups = "drop"
  )

coverage_counts

# 2. Total intervals per class
total_intervals <- interval_labels_final %>%
  dplyr::count(id, name = "total_intervals") %>%
  dplyr::mutate(total_minutes = total_intervals * 2)

total_intervals

# 3. Merge + compute percentages
coverage_summary <- coverage_counts %>%
  left_join(total_intervals, by = "id") %>%
  mutate(
    pct_intervals = round(n_intervals / total_intervals * 100, 2),
    pct_minutes   = round(minutes / total_minutes * 100, 2)
  ) %>%
  arrange(id, desc(pct_intervals))

coverage_summary

# 4. clean output (WIDE)
# define the desired profile column order
profile_order <- c(
  "TPS",
  "PeerInstruction",
  "Clicker",
  "PeerLite",
  "ClickerLite",
  "StudentWork",  
  "Lecture",
  "Admin",
  "Unlabeled"
)

coverage_summary_wide <- coverage_summary %>%
  select(id, profile, pct_intervals) %>%
  mutate(profile = factor(profile, levels = profile_order)) %>%
  pivot_wider(
    names_from  = profile,
    values_from = pct_intervals,
    values_fill = 0
  ) %>%
  # ensure columns are returned in the specified order
  select(id, all_of(profile_order))

coverage_summary_wide

# ==== UNLABELED SEGMENT ====
# 5. Identify unlabeled intervals and residual segments ------------------------
# detect consecutive unlabeled intervals -------------------------------------
detect_unlabeled_segments <- function(interval_df) {
  
  unlabeled_df <- interval_df %>%
    arrange(id, time) %>%
    mutate(is_unlabeled = profile == "Unlabeled")
  
  unlabeled_df %>%
    group_by(id) %>%
    mutate(
      # start a new unlabeled run when:
      # - switching from labeled to unlabeled
      # - OR time is not consecutive
      new_run = is_unlabeled & (
        lag(is_unlabeled, default = FALSE) == FALSE |
          time != lag(time, default = first(time)) + 1
      ),
      run_id = cumsum(new_run)
    ) %>%
    ungroup() %>%
    # keep only unlabeled intervals
    filter(is_unlabeled) %>%
    group_by(id, run_id) %>%
    summarise(
      start_time  = min(time),
      end_time    = max(time),
      n_intervals = n(),
      minutes     = n_intervals * 2,
      type        = "Unlabeled",
      .groups = "drop"
    ) %>%
    arrange(id, start_time)
}

# run the detector
unlabeled_segments <- detect_unlabeled_segments(interval_labels_final)

unlabeled_segments

# 6. Save outputs --------------------------------------------------------------
write_csv(interval_labels_final, "outputs/interval_labels_final_primary_secondary.csv")
write_csv(unlabeled_segments, "outputs/unlabeled_segments_primary_secondary.csv")


# ==== SAVE CACHE FOR DOWNSTREAM SCRIPTS =======================================

# Save the primary/secondary labels, residual segments, and detector outputs
# required by scripts 03, 04, 05, and 06.
dir.create(
  "cache",
  showWarnings = FALSE,
  recursive = TRUE
)

cache_02 <- list(
  code_cols = code_cols,
  master_data = master_data,
  lecture_segments = lecture_segments,
  clicker_segments = clicker_segments,
  pi_segments = pi_segments,
  tps_segments = tps_segments,
  peer_lite_segments = peer_lite_segments,
  clicker_lite_segments = clicker_lite_segments,
  admin_segments = admin_segments,
  student_work_segments = student_work_segments,
  interval_labels_final = interval_labels_final,
  unlabeled_segments = unlabeled_segments,
  coverage_counts = coverage_counts,
  total_intervals = total_intervals,
  coverage_summary = coverage_summary,
  coverage_summary_wide = coverage_summary_wide
)

saveRDS(
  cache_02,
  "cache/02_primary_secondary_labels.rds"
)

message(
  "Saved primary/secondary label cache: ",
  "cache/02_primary_secondary_labels.rds"
)

rm(cache_02)
