# ==================================================
# COPUS Segmentation Framework
# 05_label_tertiary_coverage.R
# Purpose: Apply precedence rules for tertiary segments and compute coverage
# ==================================================
source("scripts/00_setup.R")
source("scripts/01_primary_secondary_detectors.R")
source("scripts/02_label_precedence_residual.R")
source("scripts/04_tertiary_detectors.R")
# ==== MAPPING & LABEL ====
# 1. Expand tertiary segment tables to interval labels -----
lab_studentQA    <- label_from_segments(studentQA_segments,    "StudentQA")
lab_instructorQA <- label_from_segments(instructorQA_segments, "InstructorQA")
lab_transition   <- label_from_segments(transition_segments,   "Transition")

# 2. Restrict tertiary labels to ONLY previously-unlabeled intervals -----
# identify unlabeled intervals from the strategy-level labeling
unlabeled_only_grid <- interval_labels_final %>%
  filter(profile == "Unlabeled") %>%
  select(id, time)

# 3. Keep tertiary labels only where strategy label is Unlabeled -----
lab_studentQA_u <- unlabeled_only_grid %>% inner_join(lab_studentQA,    by = c("id","time"))
lab_instructorQA_u <- unlabeled_only_grid %>% inner_join(lab_instructorQA, by = c("id","time"))
lab_transition_u <- unlabeled_only_grid %>% inner_join(lab_transition,   by = c("id","time"))

# 4. Apply tertiary precedence within the unlabeled-only space ----
tertiary_labels_final <- bind_rows(
  lab_studentQA_u    %>% mutate(priority = 1L),
  lab_instructorQA_u %>% mutate(priority = 2L),
  lab_transition_u   %>% mutate(priority = 3L)
) %>%
  arrange(id, time, priority) %>%
  group_by(id, time) %>%
  slice(1) %>%       # keep highest-priority tertiary label for each unlabeled interval
  ungroup() %>%
  select(id, time, tertiary_label = label)

# 5. Merge tertiary labels back into the final interval profiles -----
interval_labels_final_with_tertiary <- interval_labels_final %>%
  left_join(tertiary_labels_final, by = c("id","time")) %>%
  mutate(
    profile_aug = if_else(profile == "Unlabeled" & !is.na(tertiary_label),
                          tertiary_label,
                          profile)
  ) %>%
  select(id, time, profile = profile_aug)

# ==== COVERAGE ====
# 6. Compute coverage -----
# counts
coverage_counts_tertiary <- interval_labels_final_with_tertiary %>%
  group_by(id, profile) %>%
  summarise(
    n_intervals = n(),
    minutes     = n_intervals * 2,
    .groups = "drop"
  )

# total
total_intervals_tertiary <- interval_labels_final_with_tertiary %>%
  dplyr::count(id, name = "total_intervals") %>%
  dplyr::mutate(total_minutes = total_intervals * 2)

# percent
coverage_summary_tertiary <- coverage_counts_tertiary %>%
  left_join(total_intervals_tertiary, by = "id") %>%
  mutate(
    pct_intervals = round(n_intervals / total_intervals * 100, 2),
    pct_minutes   = round(minutes / total_minutes * 100, 2)
  ) %>%
  arrange(id, desc(pct_intervals))

coverage_summary_tertiary

# 7. Wide-format output including tertiary labels -----
profile_order_tertiary <- c(
  "TPS",
  "PeerInstruction",
  "Clicker",
  "PeerLite",
  "ClickerLite",
  "Lecture",
  "StudentWork",
  "Admin",
  "StudentQA",
  "InstructorQA",
  "Transition",
  "Unlabeled"
)

coverage_summary_wide_tertiary <- coverage_summary_tertiary %>%
  select(id, profile, pct_intervals) %>%
  mutate(profile = factor(profile, levels = profile_order_tertiary)) %>%
  pivot_wider(
    names_from  = profile,
    values_from = pct_intervals,
    values_fill = 0
  ) %>%
  select(id, all_of(profile_order_tertiary))

coverage_summary_wide_tertiary

# 8. Coverage improvement -----
# overall labeled percent before tertiary vs after tertiary detectors
overall_before <- interval_labels_final %>%
  summarise(pct_labeled = round(mean(profile != "Unlabeled") * 100, 2))

overall_after <- interval_labels_final_with_tertiary %>%
  summarise(pct_labeled = round(mean(profile != "Unlabeled") * 100, 2))

# newly labeled intervals by tertiary detectors
# intervals that were unlabeled before but labeled after
tertiary_fills <- interval_labels_final %>%
  select(id, time, before = profile) %>%
  inner_join(interval_labels_final_with_tertiary %>%
               select(id, time, after = profile),
             by = c("id","time")) %>%
  filter(before == "Unlabeled" & after != "Unlabeled")

# distribution by tertiary label
tertiary_distribution <- tertiary_fills %>%
  dplyr::count(after, name = "n_intervals") %>%
  dplyr::mutate(
    pct_of_total_time = round(n_intervals / nrow(interval_labels_final) * 100, 2),
    pct_of_filled     = round(n_intervals / sum(n_intervals) * 100, 2)
  ) %>%
  arrange(desc(n_intervals))

# 9. Save outputs -----
write_csv(interval_labels_final_with_tertiary, "outputs/interval_labels_final_primary_secondary_tertiary.csv")
write_csv(coverage_summary_wide_tertiary, "outputs/coverage_summary_wide_tertiary.csv")
write_csv(tertiary_distribution,"outputs/tertiary_distribution.csv")