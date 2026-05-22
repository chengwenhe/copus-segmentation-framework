# ==================================================
# COPUS Segmentation Framework
# 03_pam_clustering_analysis.R
# Purpose: Perform PAM clustering on residual intervals
# ==================================================
source("scripts/00_setup.R")
source("scripts/01_primary_secondary_detectors.R")
source("scripts/02_label_precedence_residual.R")
# Phase 1 Diagnostic profiling---------------------------------
# 1. extract 1-interval (2-minute) unlabeled chunks
unlabeled_1bin <- unlabeled_segments %>%
  filter(n_intervals == 1)

nrow(unlabeled_1bin)

# 2. pull COPUS codes for those bins
# expand (id, time) for 1-bin segments
unlabeled_1bin_rows <- unlabeled_1bin %>%
  transmute(id, time = start_time)

# join with raw COPUS data
unlabeled_1bin_data <- unlabeled_1bin_rows %>%
  left_join(master_data, by = c("id", "time")) %>%
  select(all_of(code_cols))

# checks
dim(unlabeled_1bin_data)

# 3. code frequency profile
code_freq_1bin <- unlabeled_1bin_data %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "code",
    values_to = "count"
  ) %>%
  filter(count > 0) %>%
  arrange(desc(count))

code_freq_1bin

# Phase 2 Clustering -----------------------------------------------------------
# 1. Prepare single-interval residual segments ---------------------------------
# 1.1 1-interval unlabeled segments only -----
unlabeled_1bin <- unlabeled_segments %>%
  filter(n_intervals == 1) %>%
  mutate(
    seg_row_id = row_number(),          # stable row id for this subset
    time = start_time                   # single bin
  ) %>%
  select(seg_row_id, id, time)

nrow(unlabeled_1bin)

# check
unlabeled_1bin %>% count(id) %>% arrange(desc(n)) %>% head()
unlabeled_1bin %>% count(id, time) %>% filter(n > 1)  # should be empty

# 1.2 join COPUS code columns -----
unlabeled_1bin_codes <- unlabeled_1bin %>%
  left_join(master_data %>% select(id, time, all_of(code_cols)),
            by = c("id","time")) %>%
  mutate(across(all_of(code_cols), ~ replace_na(., 0))) %>%
  mutate(across(all_of(code_cols), ~ as.integer(. >= 1)))  # make it binary; it converts the specified columns to 0/1 indicators where values ≥ 1 become 1, and values < 1 become 0

stopifnot(all(code_cols %in% names(unlabeled_1bin_codes))) #checks

# extra checks
# check join success rate
unlabeled_1bin_codes %>%
  summarise(any_missing_row = mean(is.na(.[[code_cols[1]]]))) # zero means all good

# verify master_data really has these bins
anti <- unlabeled_1bin %>%
  anti_join(master_data %>% distinct(id, time), by=c("id","time"))
nrow(anti); anti %>% head() # zero means all good

# 1.3 drop all-zero rows and ultra-rare codes ----------------------------------
# drop bins with no coded activity (all 25 codes = 0)
unlabeled_1bin_codes2 <- unlabeled_1bin_codes %>%
  mutate(row_sum = rowSums(across(all_of(code_cols)))) %>%
  filter(row_sum > 0) %>%
  select(-row_sum)

# drop ultra-rare codes (keep > 1% of bins)
min_prop <- 0.01 # we can tweak threshold 
code_freq <- unlabeled_1bin_codes2 %>%
  summarise(across(all_of(code_cols), ~ mean(. == 1))) %>%
  pivot_longer(everything(), names_to = "code", values_to = "prop")

kept_codes <- code_freq %>%
  filter(prop >= min_prop) %>%
  pull(code)

length(kept_codes)
kept_codes

# 2. Build COPUS feature matrix ------------------------------------------------
# 2.1 build feature matrix -----
X <- unlabeled_1bin_codes2 %>%
  select(seg_row_id, id, time, all_of(kept_codes))

# matrix for clustering
X_mat <- X %>% select(all_of(kept_codes)) %>% as.matrix()

# quick check: any non-binary?
summary(as.vector(X_mat))

# how many codes per bin?
rowSums(X_mat) %>% summary()

# which codes dominate?
colMeans(X_mat) %>% sort(decreasing = TRUE)

# 2.2 compute Gower distance ----- 
# Gower distance suited for binary data
d_gower <- daisy(
  X_mat,
  metric = "gower",
  type = list(symm = colnames(X_mat)) 
)

# 3. Evaluate number of clusters -----------------------------------------------
# 3.1 evaluate k = 2 to 12 using silhouette -----
sil_tbl <- map_dfr(2:12, function(k){
  fit <- pam(d_gower, k = k, diss = TRUE)
  tibble(k = k, silhouette = fit$silinfo$avg.width)
})

sil_tbl

ggplot(sil_tbl, aes(x = k, y = silhouette)) +
  geom_line() + geom_point() +
  scale_x_continuous(breaks = 2:12) +
  labs(
    title = "Choose K via Average Silhouette (1-bin unlabeled)",
    x = "Number of clusters (K)",
    y = "Average silhouette width"
  ) +
  theme_minimal(base_size = 13)

# 3.2 optional supplementary NbClust check -----
# NbClust to determine number of clusters (multiple indices)
set.seed(123) # for reproducible use and freezes randomness

# NbClust operates on a feature matrix (not a distance matrix)
# NbClust primarily supports hierarchical or k-means–based methods
# Use it as an independent diagnostic for plausible K values

nb_res <- NbClust(
  data = X_mat,
  distance = "binary",     # binary distance for 0/1 COPUS codes
  min.nc = 2,
  max.nc = 12,
  method = "ward.D2",      # commonly and widely used in NbClust
  index = "all"            # evaluate all available indices
)

# summary of how many indices recommend each K
nb_res$Best.nc

# 4. Fit final PAM model -------------------------------------------------------
# 4.1 fit PAM clustering for final k = 5 ------
# choose number of cluster
K_final <- 5 # switch to 4 to compare

# fit PAM using the Gower distance
pam_fit <- pam(d_gower, k = K_final , diss = TRUE)

# 4.2 attach cluster labels back to bins ------
clustered_1bin <- X %>%
  transmute(
    seg_row_id, id, time, cluster = factor(pam_fit$clustering)
  )

# check cluster sizes
cluster_sizes <- clustered_1bin %>%
  count(cluster, name = "n") %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(cluster)

cluster_sizes

# 5. Summarize cluster profiles ------------------------------------------------
# 5.1 cluster profiles -----
cluster_profiles <- clustered_1bin %>%
  left_join(
    unlabeled_1bin_codes2 %>% select(seg_row_id, all_of(kept_codes)),
    by = "seg_row_id"
  ) %>%
  group_by(cluster) %>%
  summarise(
    across(all_of(kept_codes), ~ mean(. == 1, na.rm = TRUE)),
    .groups = "drop"
  )

cluster_profiles

# reshape to long format (for inspection & plotting)
cluster_profiles_long <- cluster_profiles %>%
  pivot_longer(
    cols = all_of(kept_codes),
    names_to = "code",
    values_to = "prop"
  ) %>%
  arrange(cluster, desc(prop))

cluster_profiles_long

# top defining codes per cluster
top_codes_each_cluster <- cluster_profiles_long %>%
  group_by(cluster) %>%
  slice_max(prop, n = 5, with_ties = FALSE) %>%
  ungroup()

top_codes_each_cluster

# 5.2 compare cluster-specific code prevalence -----
# contrast with global prevalence
# global prevalence of each code (average global probability per code)
global_props <- unlabeled_1bin_codes2 %>%
  summarise(across(all_of(kept_codes), ~ mean(. == 1))) %>%
  pivot_longer(
    everything(),
    names_to = "code",
    values_to = "global_prop"
  )

# cluster-specific deviations
cluster_profiles_contrast <- cluster_profiles_long %>%
  left_join(global_props, by = "code") %>%
  mutate(delta = prop - global_prop) %>%
  arrange(cluster, desc(delta))

cluster_profiles_contrast

top_contrast_codes <- cluster_profiles_contrast %>%
  filter(delta > 0) %>%
  group_by(cluster) %>%
  slice_max(delta, n = 5, with_ties = FALSE) %>%
  ungroup()

# 5.3 dominant COPUS code summaries: heatmap for cluster profile -----
# aggregate of all bins
# join cluster labels with codes 
cluster_bins_codes <- clustered_1bin %>%
  left_join(
    unlabeled_1bin_codes2 %>% select(seg_row_id, all_of(kept_codes)),
    by = "seg_row_id"
  )

# aggregate prevalence within cluster
hm_df <- cluster_bins_codes %>%
  group_by(cluster) %>%
  summarise(across(all_of(kept_codes), ~ mean(. == 1, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(-cluster, names_to = "code", values_to = "prop") %>%
  mutate(
    cluster = fct_infreq(cluster),  # orders cluster factor levels by decreasing frequency
    code = fct_reorder(code, prop, .fun = max)  # reorders codes by the maximum prevalence across clusters
  ) # puts more discriminative codes together-ish

ggplot(hm_df, aes(x = cluster, y = code, fill = prop)) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_fill_gradient(low = "white", high = "black",
                      labels = percent_format(accuracy = 1)) +
  labs(
    title = "Cluster profiles (all 1-interval bins)",
    subtitle = "Cell = % of bins in cluster where code is present",
    x = "Cluster",
    y = "COPUS code",
    fill = "% present"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank())

# 6. Stability diagnostics for PAM clustering (optional) -----------------------
# 6.1 required inputs -----
# d_gower : dissimilarity from daisy(...)
# X_mat   : feature matrix used to build d_gower (same row order)
# checks (avoid silent row-order mismatch) 
stopifnot(attr(d_gower, "Size") == nrow(X_mat))

Ks <- c(4, 5)
R  <- 50
subsample_prop <- 0.80

# helper: full-data PAM labels
pam_full <- function(k) {
  pam(d_gower, k = k, diss = TRUE)$clustering
}

# helper: ARI
ari <- function(a, b) adjustedRandIndex(a, b)

# 6.2 reproducibility check -----
# same data, repeated runs; note: PAM with diss=TRUE is often deterministic -> ARI will be 1
repro_tbl_list <- lapply(Ks, function(k) {
  ref <- pam_full(k)
  
  aris <- sapply(1:R, function(seed) {
    set.seed(seed) # usually irrelevant here, but harmless
    cl <- pam(d_gower, k = k, diss = TRUE)$clustering
    ari(ref, cl)
  })
  
  tibble(k = k, check = "reproducibility", run = 1:R, ari = as.numeric(aris))
})

repro_tbl <- bind_rows(repro_tbl_list)

print(repro_tbl %>%
        group_by(check, k) %>%
        summarise(min = min(ari), q1 = quantile(ari, .25), median = median(ari),
                  mean = mean(ari), q3 = quantile(ari, .75), max = max(ari),
                  .groups = "drop"))

# 6.3 sub-sample stability using ARI -----
# fit on 80% sub-sample, compare to full-data ref on the SAME items via ARI
# meaningful robustness test
sub_tbl_list <- lapply(Ks, function(k) {
  ref_full <- pam_full(k)
  
  aris <- sapply(1:R, function(seed) {
    set.seed(seed)
    idx <- sample(seq_len(nrow(X_mat)),
                  size = floor(subsample_prop * nrow(X_mat)),
                  replace = FALSE)
    
    # subset distance matrix consistently
    d_sub <- as.dist(as.matrix(d_gower)[idx, idx])
    
    cl_sub <- pam(d_sub, k = k, diss = TRUE)$clustering
    
    ari(ref_full[idx], cl_sub)
  })
  
  tibble(k = k, check = paste0("subsample_", subsample_prop*100, "pct"),
         run = 1:R, ari = as.numeric(aris))
})

sub_tbl <- bind_rows(sub_tbl_list)

print(sub_tbl %>%
        group_by(check, k) %>%
        summarise(min = min(ari), q1 = quantile(ari, .25), median = median(ari),
                  mean = mean(ari), q3 = quantile(ari, .75), max = max(ari),
                  .groups = "drop"))

# 6.4 plot or summary table --------
# optional plot
plot_df <- bind_rows(repro_tbl, sub_tbl)

ggplot(plot_df, aes(x = factor(k), y = ari)) +
  geom_boxplot() +
  facet_wrap(~check, scales = "free_y") +
  labs(
    title = "PAM stability diagnostics (ARI)",
    subtitle = "Reproducibility (same data) + Subsample robustness (80% rows)",
    x = "Number of clusters (k)",
    y = "Adjusted Rand Index (vs full-data reference)"
  ) +
  theme_minimal(base_size = 13)

# 7. Save outputs --------------------------------------------------------------
write_csv(cluster_profiles, "outputs/pam_cluster_profiles.csv")
write_csv(sil_tbl, "outputs/pam_silhouette.csv")
write_csv(plot_df, "outputs/pam_stability_summary.csv")