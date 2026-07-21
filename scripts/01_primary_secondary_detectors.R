# ==================================================
# COPUS Segmentation Framework
# 01_primary_secondary_detectors.R
# Purpose: Run primary and secondary detectors
# ==================================================
source("scripts/00_setup.R")
# ensure COPUS code columns numeric ---------------------------
code_cols <- c(
  "Instructor.1o1","Instructor.Adm","Instructor.AnQ","Instructor.CQ","Instructor.DV",
  "Instructor.FUp","Instructor.Lec","Instructor.MG","Instructor.Other","Instructor.PQ",
  "Instructor.RtW","Instructor.W",
  "Student.AnQ","Student.CG","Student.Ind","Student.L","Student.OG",
  "Student.Other","Student.Prd","Student.SP","Student.SQ","Student.TQ",
  "Student.W","Student.WC","Student.WG"
)

master_data <- master_data %>%
  mutate(across(all_of(code_cols), ~ suppressWarnings(as.numeric(.)))) %>%  # "0"/"1" -> 0/1; non-numeric -> NA
  mutate(across(all_of(code_cols), ~ tidyr::replace_na(., 0)))              # NA -> 0

# ==== PRIMARY DETECTORS ====
# Lecture Detector -------------
# characterize Lec and Listen segments
# add a per-row flag:lecturing +listening
master_data <- master_data %>%
  mutate(
    lec_listen = (`Instructor.Lec` == 1) & (`Student.L` == 1)
  ) 

# within each session (id), sort by time and assign run IDs to consecutive blocks
master_data <- master_data %>%
  group_by(id) %>%
  arrange(time, .by_group = TRUE) %>% 
  mutate(
    # start a new run if: first row in session, time jumps, or the lec_listen state changes
    new_run = (row_number() == 1) |
      (time != lag(time) + 1) |
      (lec_listen != lag(lec_listen)),
    run_id = cumsum(new_run)
  ) %>%
  ungroup()

# collapse ONLY the lecture+listening runs into segments
lecture_segments <- master_data %>%
  filter(lec_listen) %>%
  group_by(id, run_id) %>%
  summarise(
    start_time  = min(time),
    end_time    = max(time),
    n_intervals = n(),              # number of 2-min rows
    minutes     = n_intervals * 2,  # segment length in minutes
    type        = "Lecture",  
    .groups = "drop"
  ) %>%
  arrange(id, start_time)

# view the result
lecture_segments

# Clicker Detector -------------
# 1) phase flags
build_clicker_flags <- function(df) {
  df %>%
    arrange(id, time) %>%
    mutate(
      prompt_raw  = (`Instructor.CQ` == 1),
      student_raw = (`Student.Ind` == 1) | (`Student.CG` == 1),
      wrap_raw    = (`Instructor.FUp` == 1),
      
      # phase flags (allow overlaps; we keep them “raw” and enforce adjacency in the scanner)
      phase_prompt  = prompt_raw,
      phase_student = student_raw,
      phase_wrap    = wrap_raw
    )
}

# 2) scanner for one session (nearest-prompt + adjacency/overlap)
scan_clicker_one_id <- function(dd,
                                min_prompt  = 1,
                                min_student = 1,
                                min_wrap    = 1,
                                max_gap_ps  = 0,  # Prompt→Student allowed gap (bins)
                                max_gap_sw  = 0   # Student→Wrap allowed gap
) {
  stopifnot(all(c("id","time","phase_prompt","phase_student","phase_wrap") %in% names(dd)))
  
  n <- nrow(dd)
  i <- 1
  out <- list()
  
  while (i <= n) {
    
    ## (1) Find first STUDENT block at/after i
    j <- i
    while (j <= n && !dd$phase_student[j]) j <- j + 1
    if (j > n) break
    stu_start <- j
    
    # extend student block (contiguous)
    cnt <- 0; k <- stu_start
    while (k <= n && dd$phase_student[k]) { cnt <- cnt + 1; k <- k + 1 }
    if (cnt < min_student) { i <- k; next }
    stu_end <- k - 1
    
    ## (2) NEAREST-PROMPT: pick CQ block immediately before (or overlapping) the student block
    p_end <- stu_start - 1
    if (dd$phase_prompt[stu_start]) p_end <- stu_start  # allow overlap
    while (p_end >= i && !dd$phase_prompt[p_end]) p_end <- p_end - 1
    if (p_end < i) { i <- stu_end + 1; next }  # no prompt before/at student
    
    p_start <- p_end
    while (p_start > i && dd$phase_prompt[p_start - 1]) p_start <- p_start - 1
    if ((p_end - p_start + 1) < min_prompt) { i <- stu_end + 1; next }
    
    # adjacency/overlap Prompt→Student
    if (stu_start > (p_end + 1 + max_gap_ps)) { i <- stu_end + 1; next }
    
    ## (3) Find WRAP at/after stu_start
    k <- stu_start
    while (k <= n && !dd$phase_wrap[k]) k <- k + 1
    if (k > n) break
    wrap_start <- k
    
    # extend wrap block (contiguous)
    cnt <- 0; m <- wrap_start
    while (m <= n && dd$phase_wrap[m]) { cnt <- cnt + 1; m <- m + 1 }
    if (cnt < min_wrap) { i <- m; next }
    wrap_end <- m - 1
    
    # adjacency/overlap Student→Wrap
    if (wrap_start > (stu_end + 1 + max_gap_sw)) { i <- wrap_end + 1; next }
    
    ## (4) Record segment (from prompt_start to wrap_end)
    seg_start_time <- dd$time[p_start]
    seg_end_time   <- dd$time[wrap_end]
    out[[length(out) + 1]] <- tibble::tibble(
      id          = dd$id[1],
      start_time  = seg_start_time,
      end_time    = seg_end_time,
      n_intervals = seg_end_time - seg_start_time + 1,
      minutes     = (seg_end_time - seg_start_time + 1) * 2,
      type        = "Clicker"
    )
    
    ## (5) Advance
    i <- wrap_end + 1
  }
  
  if (length(out) == 0) {
    tibble::tibble(
      id=character(), start_time=integer(), end_time=integer(),
      n_intervals=integer(), minutes=integer(), type=character()
    )
  } else dplyr::bind_rows(out)
}

# 3) detector wrapper that applies the scanner per class
detect_clicker_segments <- function(df,
                                    min_prompt  = 1,
                                    min_student = 1,
                                    min_wrap    = 1,
                                    max_gap_ps  = 0,
                                    max_gap_sw  = 0) {
  df2 <- build_clicker_flags(df)
  
  df2 %>%
    dplyr::group_by(id) %>%
    dplyr::group_split() %>%
    purrr::map_dfr(~ scan_clicker_one_id(.x,
                                         min_prompt  = min_prompt,
                                         min_student = min_student,
                                         min_wrap    = min_wrap,
                                         max_gap_ps  = max_gap_ps,
                                         max_gap_sw  = max_gap_sw)) %>%
    dplyr::arrange(id, start_time)
}

# 4) run it (strict adjacency/overlap by default) 
clicker_segments <- detect_clicker_segments(
  master_data,
  min_prompt  = 1,
  min_student = 1,
  min_wrap    = 1,
  max_gap_ps  = 0,  # Student must be adjacent to or overlap Prompt
  max_gap_sw  = 0   # Wrap must be adjacent to or overlap Student
)

clicker_segments

# Think-Pair-Share Detector -------------
# 1) phase flags
build_tps_flags <- function(df) {
  df %>%
    arrange(id, time) %>%
    mutate(
      prompt_raw = (`Instructor.PQ` == 1) | (`Instructor.CQ` == 1),
      indiv_raw  = (`Student.Ind`  == 1),
      group_raw  = (`Student.CG`   == 1) | (`Student.OG` == 1) | (`Student.WG` == 1),
      share_raw  = (`Instructor.FUp` == 1) & ( (`Student.L` == 1) | (`Student.AnQ` == 1) ),
      
      # phase flags (allow overlaps; we keep them “raw” and enforce adjacency in the scanner)
      phase_prompt = prompt_raw,
      phase_indiv  = indiv_raw,
      phase_group  = group_raw,
      phase_share  = share_raw
    )
}

#2) scanner for one session (nearest-prompt + adjacency)
scan_tps_one_id <- function(dd,
                            min_prompt = 1,
                            min_indiv  = 1,
                            min_group  = 1,
                            min_share  = 1,
                            max_gap_pi = 0,  # Prompt→Indiv gap allowed (in bins)
                            max_gap_ig = 0,  # Indiv→Group gap allowed
                            max_gap_gs = 0   # Group→Share gap allowed
) {
  stopifnot(all(c("id","time",
                  "phase_prompt","phase_indiv","phase_group","phase_share") %in% names(dd)))
  
  n <- nrow(dd)
  i <- 1
  out <- list()
  
  while (i <= n) {
    
    ## (1) Find first INDIV block at/after i
    j <- i
    while (j <= n && !dd$phase_indiv[j]) j <- j + 1
    if (j > n) break
    indiv_start <- j
    
    # extend indiv (contiguous)
    cnt <- 0; k <- indiv_start
    while (k <= n && dd$phase_indiv[k]) { cnt <- cnt + 1; k <- k + 1 }
    if (cnt < min_indiv) { i <- k; next }
    indiv_end <- k - 1
    
    ## (2) From indiv_start, find the NEAREST prompt block before (or overlapping) indiv
    p_end <- indiv_start - 1
    # if prompt overlaps indiv, allow p_end >= indiv_start
    if (dd$phase_prompt[indiv_start]) p_end <- indiv_start
    while (p_end >= i && !dd$phase_prompt[p_end]) p_end <- p_end - 1
    if (p_end < i) { i <- indiv_end + 1; next }   # no prompt before/at indiv
    
    p_start <- p_end
    while (p_start > i && dd$phase_prompt[p_start - 1]) p_start <- p_start - 1
    if ((p_end - p_start + 1) < min_prompt) { i <- indiv_end + 1; next }
    
    # adjacency/overlap Prompt→Indiv
    # valid if indiv_start <= p_end (overlap) OR gap <= max_gap_pi
    if (indiv_start > (p_end + 1 + max_gap_pi)) { i <- indiv_end + 1; next }
    
    ## (3) Find GROUP block at/after indiv_start
    k <- indiv_start
    while (k <= n && !dd$phase_group[k]) k <- k + 1
    if (k > n) break
    group_start <- k
    
    # extend group (contiguous)
    cnt <- 0; m <- group_start
    while (m <= n && dd$phase_group[m]) { cnt <- cnt + 1; m <- m + 1 }
    if (cnt < min_group) { i <- m; next }
    group_end <- m - 1
    
    # adjacency/overlap Indiv→Group
    if (group_start > (indiv_end + 1 + max_gap_ig)) { i <- group_end + 1; next }
    
    ## (4) Find SHARE block at/after group_start
    m <- group_start
    while (m <= n && !dd$phase_share[m]) m <- m + 1
    if (m > n) break
    share_start <- m
    
    # extend share (contiguous)
    cnt <- 0; q <- share_start
    while (q <= n && dd$phase_share[q]) { cnt <- cnt + 1; q <- q + 1 }
    if (cnt < min_share) { i <- q; next }
    share_end <- q - 1
    
    # adjacency/overlap Group→Share
    if (share_start > (group_end + 1 + max_gap_gs)) { i <- share_end + 1; next }
    
    ## (5) Record segment from prompt_start to share_end
    seg_start_time <- dd$time[p_start]
    seg_end_time   <- dd$time[share_end]
    out[[length(out) + 1]] <- tibble(
      id          = dd$id[1],
      start_time  = seg_start_time,
      end_time    = seg_end_time,
      n_intervals = seg_end_time - seg_start_time + 1,
      minutes     = (seg_end_time - seg_start_time + 1) * 2,
      type        = "TPS"
    )
    
    ## (6) Advance past this segment
    i <- share_end + 1
  }
  
  if (length(out) == 0) {
    tibble(id=character(), start_time=integer(), end_time=integer(),
           n_intervals=integer(), minutes=integer(), type=character())
  } else dplyr::bind_rows(out)
}

# 3) detector wrapper
detect_tps_segments <- function(df,
                                min_prompt = 1,
                                min_indiv  = 1,
                                min_group  = 1,
                                min_share  = 1,
                                max_gap_pi = 0,
                                max_gap_ig = 0,
                                max_gap_gs = 0) {
  df2 <- build_tps_flags(df)
  
  df2 %>%
    group_by(id) %>%
    group_split() %>%
    map_dfr(~ scan_tps_one_id(.x,
                              min_prompt = min_prompt,
                              min_indiv  = min_indiv,
                              min_group  = min_group,
                              min_share  = min_share,
                              max_gap_pi = max_gap_pi,
                              max_gap_ig = max_gap_ig,
                              max_gap_gs = max_gap_gs)) %>%
    arrange(id, start_time)
}


#4) run it (strict adjacency/overlap by default)
tps_segments <- detect_tps_segments(
  master_data,
  min_prompt = 1,
  min_indiv  = 1,
  min_group  = 1,
  min_share  = 1,
  max_gap_pi = 0,  # Prompt→Indiv must be adjacent or overlapping
  max_gap_ig = 0,  # Indiv→Group must be adjacent or overlapping
  max_gap_gs = 0   # Group→Share must be adjacent or overlapping
)

tps_segments

# Peer Instruction Detector -------------
# 1) phase flags
build_pi_flags <- function(df) {
  df %>%
    arrange(id, time) %>%
    mutate(
      phase_prompt  = (`Instructor.CQ` == 1) | (`Instructor.PQ` == 1),
      phase_discuss = (`Student.CG` == 1) | (`Student.OG` == 1) | (`Student.WG` == 1),
      phase_wrap    = (`Instructor.FUp` == 1)
    )
}

# 2) scanner for one session using “nearest-prompt + adjacency/overlap”
scan_pi_one_id <- function(dd,
                           min_prompt  = 1,
                           min_discuss = 1,
                           min_wrap    = 1,
                           max_gap_pd  = 0,  # max allowed gap (in 2-min bins) between Prompt end and Discuss start
                           max_gap_dw  = 0   # max allowed gap between Discuss end and Wrap start
) {
  stopifnot(all(c("id","time","phase_prompt","phase_discuss","phase_wrap") %in% names(dd)))
  n <- nrow(dd)
  i <- 1
  out <- list()
  
  while (i <= n) {
    
    ## 1) Find first DISCUSS block at/after i
    j <- i
    while (j <= n && !dd$phase_discuss[j]) j <- j + 1
    if (j > n) break
    discuss_start <- j
    
    # extend contiguous discuss block
    cnt <- 0; k <- discuss_start
    while (k <= n && dd$phase_discuss[k]) { cnt <- cnt + 1; k <- k + 1 }
    if (cnt < min_discuss) { i <- k; next }
    discuss_end <- k - 1
    
    ## 2) Find the NEAREST Prompt block that ends immediately before (or overlaps) this discuss block
    # search backward from discuss_start-1 for the last TRUE in phase_prompt
    p_end <- discuss_start - 1
    while (p_end >= i && !dd$phase_prompt[p_end]) p_end <- p_end - 1
    if (p_end < i) { i <- discuss_end + 1; next }  # no prompt before discuss
    
    # walk backward to get contiguous prompt block
    p_start <- p_end
    while (p_start > i && dd$phase_prompt[p_start - 1]) p_start <- p_start - 1
    if ((p_end - p_start + 1) < min_prompt) { i <- discuss_end + 1; next }
    
    # adjacency/overlap constraint Prompt→Discuss:
    # allow overlap (discuss_start <= p_end) OR a small gap <= max_gap_pd
    if (discuss_start > (p_end + 1 + max_gap_pd)) { i <- discuss_end + 1; next }
    
    ## 3) Find WRAP at/after discuss_start
    k <- discuss_start
    while (k <= n && !dd$phase_wrap[k]) k <- k + 1
    if (k > n) break
    wrap_start <- k
    
    # extend contiguous wrap block
    cnt <- 0; m <- wrap_start
    while (m <= n && dd$phase_wrap[m]) { cnt <- cnt + 1; m <- m + 1 }
    if (cnt < min_wrap) { i <- m; next }
    wrap_end <- m - 1
    
    # adjacency/overlap constraint Discuss→Wrap:
    if (wrap_start > (discuss_end + 1 + max_gap_dw)) { i <- wrap_end + 1; next }
    
    ## 4) Record segment from prompt_start to wrap_end
    seg_start_time <- dd$time[p_start]
    seg_end_time   <- dd$time[wrap_end]
    out[[length(out) + 1]] <- tibble(
      id          = dd$id[1],
      start_time  = seg_start_time,
      end_time    = seg_end_time,
      n_intervals = seg_end_time - seg_start_time + 1,
      minutes     = (seg_end_time - seg_start_time + 1) * 2,
      type        = "PeerInstruction"
    )
    
    ## 5) Advance cursor past this segment
    i <- wrap_end + 1
  }
  
  if (length(out) == 0) {
    tibble(id=character(), start_time=integer(), end_time=integer(),
           n_intervals=integer(), minutes=integer(), type=character())
  } else dplyr::bind_rows(out)
}

# 3) detector that applies the scanner per class
detect_pi_segments <- function(df,
                               min_prompt  = 1,
                               min_discuss = 1,
                               min_wrap    = 1,
                               max_gap_pd  = 0,
                               max_gap_dw  = 0) {
  df2 <- build_pi_flags(df)
  
  df2 %>%
    group_by(id) %>%
    group_split() %>%
    map_dfr(~ scan_pi_one_id(.x,
                             min_prompt  = min_prompt,
                             min_discuss = min_discuss,
                             min_wrap    = min_wrap,
                             max_gap_pd  = max_gap_pd,
                             max_gap_dw  = max_gap_dw)) %>%
    arrange(id, start_time)
}

# 4) run it (defaults: overlap/adjacent only)
pi_segments <- detect_pi_segments(
  master_data,
  min_prompt  = 1,
  min_discuss = 1,
  min_wrap    = 1,
  max_gap_pd  = 0,  # discuss must be adjacent to (or overlap) prompt
  max_gap_dw  = 0   # wrap must be adjacent to (or overlap) discuss
)

pi_segments

# ====SECONDARY DETECTORS ====
# Peer-Lite Detector -------------
# Prompt (PQ or CQ) as head →  Discussion (OG or WG or CG) as body
# MUST be adjacent or overlapping
# NO Instructor.FUp during or after the discussion block
# 1) Phase flags
build_peerlite_flags <- function(df) {
  df %>%
    arrange(id, time) %>%
    mutate(
      phase_prompt  = (`Instructor.PQ` == 1) | (`Instructor.CQ` == 1),
      phase_discuss = (`Student.OG` == 1) | (`Student.WG` == 1) | (`Student.CG` == 1),
      phase_FUp     = (`Instructor.FUp` == 1)
    )
}

# 2) Scanner for one class (prompt-anchored)
scan_peerlite_one_id <- function(dd,
                                 min_prompt  = 1,
                                 min_discuss = 1) {
  stopifnot(all(c("id","time","phase_prompt","phase_discuss","phase_FUp") %in% names(dd)))
  
  n   <- nrow(dd)
  i   <- 1
  out <- list()
  
  while (i <= n) {
    
    ## (A) Find the next PROMPT block (the "head")
    p <- i
    while (p <= n && !dd$phase_prompt[p]) p <- p + 1
    if (p > n) break
    
    p_start <- p
    p_end <- p
    while (p_end < n && dd$phase_prompt[p_end + 1]) p_end <- p_end + 1
    
    if ((p_end - p_start + 1) < min_prompt) {
      i <- p_end + 1
      next
    }
    
    ## (B) Body start rule:
    ##     Discussion can start within [p_start, p_end] OR at p_end + 1
    discuss_start <- NA_integer_
    
    # overlap anywhere in prompt block
    overlap_idx <- which(dd$phase_discuss[p_start:p_end])
    if (length(overlap_idx) > 0) {
      discuss_start <- p_start + min(overlap_idx) - 1
    } else if (p_end < n && dd$phase_discuss[p_end + 1]) {
      # adjacent immediately after prompt block
      discuss_start <- p_end + 1
    } else {
      # prompt not followed by discussion in/adjacent -> not PeerLite
      i <- p_end + 1
      next
    }
    
    ## (C) Extend contiguous discussion block
    k <- discuss_start
    cnt <- 0
    while (k <= n && dd$phase_discuss[k]) { cnt <- cnt + 1; k <- k + 1 }
    if (cnt < min_discuss) {
      i <- k
      next
    }
    discuss_end <- k - 1
    
    ## (D) Exclude if FUp occurs during discussion OR immediately after it
    if (any(dd$phase_FUp[discuss_start:discuss_end])) {
      i <- discuss_end + 1
      next
    }
    if (discuss_end < n && dd$phase_FUp[discuss_end + 1]) {
      i <- discuss_end + 1
      next
    }
    
    ## (E) Record the PeerLite episode
    seg_start_time <- dd$time[p_start]       # anchor start at prompt (head)
    seg_end_time   <- dd$time[discuss_end]   # end at discussion end (body)
    
    out[[length(out) + 1]] <- tibble(
      id          = dd$id[1],
      start_time  = seg_start_time,
      end_time    = seg_end_time,
      n_intervals = seg_end_time - seg_start_time + 1,
      minutes     = (seg_end_time - seg_start_time + 1) * 2,
      type        = "PeerLite"
    )
    
    ## (F) Move cursor forward:
    # This ensures we DO NOT capture multiple short episodes within the same
    # contiguous discussion run. We only keep the first prompt-anchored episode.
    i <- discuss_end + 1
  }
  
  if (length(out) == 0) {
    tibble(id=character(), start_time=integer(), end_time=integer(),
           n_intervals=integer(), minutes=integer(), type=character())
  } else {
    dplyr::bind_rows(out) %>% arrange(id, start_time)
  }
}

# 3) Detector applied across all classes
detect_peer_lite_segments <- function(df,
                                      min_prompt  = 1,
                                      min_discuss = 1) {
  df2 <- build_peerlite_flags(df)
  
  df2 %>%
    group_by(id) %>%
    group_split() %>%
    purrr::map_dfr(~ scan_peerlite_one_id(.x,
                                          min_prompt  = min_prompt,
                                          min_discuss = min_discuss)) %>%
    arrange(id, start_time)
}

# 4) Run it
peer_lite_segments <- detect_peer_lite_segments(
  master_data,
  min_prompt  = 1,
  min_discuss = 1
)

peer_lite_segments

# Clicker-Lite Detector -------------
# Prompt (CQ) as head →  Student (Ind or CG) as body
# MUST be adjacent/overlapping
# NO Instructor.FUp during or immediately after the student phase
# 1) Phase flags
build_clickerlite_flags <- function(df) {
  df %>%
    arrange(id, time) %>%
    mutate(
      phase_prompt  = (`Instructor.CQ` == 1),
      phase_student = (`Student.Ind` == 1) | (`Student.CG` == 1),
      phase_FUp     = (`Instructor.FUp` == 1)
    )
}

# 2) Scanner for one class (prompt-anchored)
scan_clickerlite_one_id <- function(dd,
                                    min_prompt  = 1,
                                    min_student = 1) {
  stopifnot(all(c("id","time","phase_prompt","phase_student","phase_FUp") %in% names(dd)))
  
  n   <- nrow(dd)
  i   <- 1
  out <- list()
  
  while (i <= n) {
    
    ## (A) Find the next PROMPT block (the "head": Instructor.CQ)
    p <- i
    while (p <= n && !dd$phase_prompt[p]) p <- p + 1
    if (p > n) break
    
    p_start <- p
    p_end <- p
    while (p_end < n && dd$phase_prompt[p_end + 1]) p_end <- p_end + 1
    
    if ((p_end - p_start + 1) < min_prompt) {
      i <- p_end + 1
      next
    }
    
    ## (B) Body start rule (v3):
    ##     Student phase can start within [p_start, p_end] OR at p_end + 1
    student_start <- NA_integer_
    
    # overlap anywhere in prompt block
    overlap_idx <- which(dd$phase_student[p_start:p_end])
    if (length(overlap_idx) > 0) {
      student_start <- p_start + min(overlap_idx) - 1
    } else if (p_end < n && dd$phase_student[p_end + 1]) {
      # adjacent immediately after prompt block
      student_start <- p_end + 1
    } else {
      # prompt not followed by student response in/adjacent -> not ClickerLite
      i <- p_end + 1
      next
    }
    
    ## (C) Extend contiguous student block
    k <- student_start
    cnt <- 0
    while (k <= n && dd$phase_student[k]) { cnt <- cnt + 1; k <- k + 1 }
    if (cnt < min_student) {
      i <- k
      next
    }
    student_end <- k - 1
    
    ## (D) Exclude if FUp occurs during student block OR immediately after it
    if (any(dd$phase_FUp[student_start:student_end])) {
      i <- student_end + 1
      next
    }
    if (student_end < n && dd$phase_FUp[student_end + 1]) {
      i <- student_end + 1
      next
    }
    
    ## (E) Record ClickerLite episode: head -> body end
    seg_start_time <- dd$time[p_start]       # start at prompt block start
    seg_end_time   <- dd$time[student_end]   # end at student phase end
    
    out[[length(out) + 1]] <- tibble(
      id          = dd$id[1],
      start_time  = seg_start_time,
      end_time    = seg_end_time,
      n_intervals = seg_end_time - seg_start_time + 1,
      minutes     = (seg_end_time - seg_start_time + 1) * 2,
      type        = "ClickerLite"
    )
    
    ## (F) Advance cursor: keep only the FIRST CQ-anchored episode
    ##     within this contiguous student-response run
    i <- student_end + 1
  }
  
  if (length(out) == 0) {
    tibble(id=character(), start_time=integer(), end_time=integer(),
           n_intervals=integer(), minutes=integer(), type=character())
  } else {
    dplyr::bind_rows(out) %>% arrange(id, start_time)
  }
}

# 3) Detector across all classes
detect_clicker_lite_segments <- function(df,
                                         min_prompt  = 1,
                                         min_student = 1) {
  df2 <- build_clickerlite_flags(df)
  
  df2 %>%
    group_by(id) %>%
    group_split() %>%
    purrr::map_dfr(~ scan_clickerlite_one_id(.x,
                                             min_prompt  = min_prompt,
                                             min_student = min_student)) %>%
    arrange(id, start_time)
}

# 4) Run it
clicker_lite_segments <- detect_clicker_lite_segments(
  master_data,
  min_prompt  = 1,
  min_student = 1
)

clicker_lite_segments

# Admin Detector -------------
# Admin segment = any block where Instructor.Adm == 1
# Other instructor/student codes are allowed
# Segments are contiguous by time (adjacent bins)
# No additional constraints
# 1) Phase flags 
build_admin_flags <- function(df) {
  df %>%
    arrange(id, time) %>%
    mutate(
      phase_admin = (`Instructor.Adm` == 1)
    )
}

# 2) Scanner for one class
scan_admin_one_id <- function(dd,
                              min_admin = 1) {
  
  stopifnot(all(c("id","time","phase_admin") %in% names(dd)))
  
  n   <- nrow(dd)
  i   <- 1
  out <- list()
  
  while (i <= n) {
    
    ## 1) Find first ADMIN block
    j <- i
    while (j <= n && !dd$phase_admin[j]) j <- j + 1
    if (j > n) break
    
    admin_start <- j
    
    # extend contiguous admin block
    cnt <- 0; k <- admin_start
    while (k <= n && dd$phase_admin[k]) { cnt <- cnt + 1; k <- k + 1 }
    if (cnt < min_admin) { i <- k; next }
    
    admin_end <- k - 1
    
    ## 2) Record Admin segment
    seg_start_time <- dd$time[admin_start]
    seg_end_time   <- dd$time[admin_end]
    
    out[[length(out) + 1]] <- tibble(
      id          = dd$id[1],
      start_time  = seg_start_time,
      end_time    = seg_end_time,
      n_intervals = seg_end_time - seg_start_time + 1,
      minutes     = (seg_end_time - seg_start_time + 1) * 2,
      type        = "Admin"
    )
    
    ## 3) Move forward
    i <- admin_end + 1
  }
  
  # Return output
  if (length(out) == 0) {
    tibble(
      id=character(), start_time=integer(), end_time=integer(),
      n_intervals=integer(), minutes=integer(), type=character()
    )
  } else dplyr::bind_rows(out)
}

# 3) Detector across all classes
detect_admin_segments <- function(df,
                                  min_admin = 1) {
  
  df2 <- build_admin_flags(df)
  
  df2 %>%
    group_by(id) %>%
    group_split() %>%
    map_dfr(~ scan_admin_one_id(.x,
                                min_admin = min_admin)) %>%
    arrange(id, start_time)
}

# 4) run it
admin_segments <- detect_admin_segments(
  master_data,
  min_admin = 1        # any length allowed
)

admin_segments

# Student Work Detector -------------
# Anchor/Continue: (MG/1o1) AND (OG/WG/Ind)
# STOP at first PQ/CQ inside the work run (cut segment before prompt)
# Exclude if PQ/CQ occurs RIGHT BEFORE segment start  --> implemented as "slide start forward"
# PQ/CQ bins cannot START student work (phase_work_start)

# 1) Build flags
build_student_work_flags <- function(df) {
  df %>%
    arrange(id, time) %>%
    mutate(
      phase_work =
        ( (`Instructor.MG` == 1) | (`Instructor.1o1` == 1) ) &
        ( (`Student.OG` == 1) | (`Student.WG` == 1) | (`Student.Ind` == 1) ),
      
      phase_prompt =
        (`Instructor.PQ` == 1) | (`Instructor.CQ` == 1),
      
      # IMPORTANT: prompt bins cannot START student work
      phase_work_start = phase_work & !phase_prompt
    )
}

# 2) Scanner for one class (one id) — CLEAN version with "sliding start"
scan_student_work_one_id <- function(dd) {
  needed <- c("id","time","phase_work","phase_work_start","phase_prompt")
  stopifnot(all(needed %in% names(dd)))
  
  dd <- dd %>% arrange(time)
  n <- nrow(dd)
  i <- 1
  out <- list()
  
  while (i <= n) {
    
    # (A) find first legal START (work but NOT prompt)
    j <- i
    while (j <= n && !dd$phase_work_start[j]) j <- j + 1
    if (j > n) break
    run_start <- j
    
    # (B) extend contiguous WORK run (still uses phase_work)
    k <- run_start
    while (k <= n && dd$phase_work[k]) k <- k + 1
    run_end <- k - 1
    
    # (C) RIGHT-BEFORE exclusion implemented as "slide start forward"
    # If the bin RIGHT BEFORE run_start is a prompt, we do NOT discard the whole run.
    # Instead, push start forward to the first bin inside the run whose previous bin is NOT a prompt.
    if (run_start > 1 && dd$phase_prompt[run_start - 1]) {
      s <- run_start + 1
      while (s <= run_end && dd$phase_prompt[s - 1]) s <- s + 1
      run_start <- s
    }
    
    # If sliding pushes start beyond run_end, skip this run
    if (run_start > run_end) {
      i <- run_end + 1
      next
    }
    
    # (D) STOP at first prompt inside run (cut BEFORE that bin)
    prompt_idx_rel <- which(dd$phase_prompt[run_start:run_end])
    if (length(prompt_idx_rel) > 0) {
      first_prompt_abs <- run_start + prompt_idx_rel[1] - 1
      seg_end <- first_prompt_abs - 1
    } else {
      first_prompt_abs <- NA_integer_
      seg_end <- run_end
    }
    
    # (E) record if segment is valid
    if (seg_end >= run_start) {
      out[[length(out) + 1]] <- tibble::tibble(
        id          = dd$id[1],
        start_time  = dd$time[run_start],
        end_time    = dd$time[seg_end],
        n_intervals = dd$time[seg_end] - dd$time[run_start] + 1,
        minutes     = (dd$time[seg_end] - dd$time[run_start] + 1) * 2,
        type        = "StudentWork"
      )
    }
    
    # (F) advance cursor
    if (!is.na(first_prompt_abs)) {
      i <- first_prompt_abs  # land on prompt bin and keep scanning from there
    } else {
      i <- run_end + 1
    }
  }
  
  if (length(out) == 0) {
    tibble::tibble(
      id=character(), start_time=integer(), end_time=integer(),
      n_intervals=integer(), minutes=integer(), type=character()
    )
  } else dplyr::bind_rows(out)
}

# 3) Apply across all classes
detect_student_work_segments <- function(df) {
  df2 <- build_student_work_flags(df)
  
  df2 %>%
    dplyr::group_by(id) %>%
    dplyr::group_split() %>%
    purrr::map_dfr(~ scan_student_work_one_id(.x)) %>%
    dplyr::arrange(id, start_time)
}

# 4) Run it
student_work_segments <- detect_student_work_segments(master_data)
student_work_segments

# ==== OPTIONAL DETECTOR-LEVEL CHECKS ===========================================

# Print a concise detector summary by default. Detailed heatmap inspections are
# retained for manual review but are not required for the detector pipeline.
RUN_DETECTOR_CHECKS <- FALSE

# Preferred COPUS code order for manual inspection
code_order <- c(
  "Instructor.Lec", "Instructor.CQ", "Instructor.PQ", "Instructor.FUp",
  "Instructor.1o1", "Instructor.Adm", "Instructor.AnQ", "Instructor.DV",
  "Instructor.MG", "Instructor.Other", "Instructor.RtW", "Instructor.W",
  "Student.L", "Student.Ind", "Student.CG", "Student.OG", "Student.WG",
  "Student.AnQ", "Student.Other", "Student.Prd", "Student.SP", "Student.SQ",
  "Student.TQ", "Student.W", "Student.WC"
)

# Heatmap inspector ------------------------------------------------------------

inspect_segment_heat <- function(
    df,
    id,
    start_time,
    end_time,
    plot = FALSE,
    profile = NULL,
    cols = code_order
) {
  df_f <- df %>%
    dplyr::filter(
      .data$id == !!id,
      .data$time >= !!start_time,
      .data$time <= !!end_time
    ) %>%
    dplyr::select(
      "time",
      dplyr::any_of(cols)
    )
  
  long <- df_f %>%
    tidyr::pivot_longer(
      cols = -"time",
      names_to = "Code",
      values_to = "Present"
    ) %>%
    dplyr::mutate(
      Present = suppressWarnings(as.numeric(.data$Present))
    ) %>%
    tidyr::replace_na(
      list(Present = 0)
    ) %>%
    dplyr::mutate(
      Code = factor(
        .data$Code,
        levels = cols
      )
    )
  
  if (isTRUE(plot)) {
    p1 <- ggplot2::ggplot(
      long,
      ggplot2::aes(
        x = .data$time,
        y = .data$Code,
        fill = factor(.data$Present)
      )
    ) +
      ggplot2::geom_tile(
        color = "grey85"
      ) +
      ggplot2::scale_fill_manual(
        values = c(
          "0" = "white",
          "1" = "grey20"
        ),
        guide = "none"
      ) +
      ggplot2::labs(
        title = paste0(
          ifelse(
            is.null(profile),
            "",
            paste0(profile, " — ")
          ),
          "Codes over time: ",
          id,
          " (",
          start_time,
          "–",
          end_time,
          ")"
        ),
        x = "Time (2-min bins)",
        y = NULL
      ) +
      ggplot2::theme_classic(
        base_size = 11
      )
    
    print(p1)
  }
  
  invisible(long)
}


# Inspect the first detected segment from one detector --------------------------

inspect_first <- function(
    seg_table,
    profile_name
) {
  if (nrow(seg_table) == 0L) {
    message(
      profile_name,
      ": No segments detected."
    )
    
    return(invisible(NULL))
  }
  
  inspect_segment_heat(
    df = master_data,
    id = seg_table$id[1],
    start_time = seg_table$start_time[1],
    end_time = seg_table$end_time[1],
    profile = profile_name,
    plot = TRUE
  )
}


# Concise detector summary -----------------------------------------------------

summarize_detector <- function(
    seg_table,
    detector_name
) {
  tibble::tibble(
    detector = detector_name,
    n_segments = nrow(seg_table),
    median_minutes = if (
      nrow(seg_table) > 0L &&
      "minutes" %in% names(seg_table)
    ) {
      stats::median(
        seg_table$minutes,
        na.rm = TRUE
      )
    } else {
      NA_real_
    }
  )
}

detector_summary <- dplyr::bind_rows(
  summarize_detector(
    lecture_segments,
    "Lecture"
  ),
  summarize_detector(
    clicker_segments,
    "Clicker"
  ),
  summarize_detector(
    tps_segments,
    "TPS"
  ),
  summarize_detector(
    pi_segments,
    "PeerInstruction"
  ),
  summarize_detector(
    peer_lite_segments,
    "PeerLite"
  ),
  summarize_detector(
    clicker_lite_segments,
    "ClickerLite"
  ),
  summarize_detector(
    admin_segments,
    "Admin"
  ),
  summarize_detector(
    student_work_segments,
    "StudentWork"
  )
)

print(detector_summary)


# Optional heatmap inspections -------------------------------------------------

if (isTRUE(RUN_DETECTOR_CHECKS)) {
  inspect_first(
    lecture_segments,
    "Lecture"
  )
  
  inspect_first(
    clicker_segments,
    "Clicker"
  )
  
  inspect_first(
    tps_segments,
    "TPS"
  )
  
  inspect_first(
    pi_segments,
    "Peer Instruction"
  )
  
  inspect_first(
    peer_lite_segments,
    "PeerLite"
  )
  
  inspect_first(
    clicker_lite_segments,
    "ClickerLite"
  )
  
  inspect_first(
    admin_segments,
    "Admin"
  )
  
  inspect_first(
    student_work_segments,
    "Student Work"
  )
} else {
  message(
    "Skipping optional detector-level plots. ",
    "Set RUN_DETECTOR_CHECKS <- TRUE to inspect example segments."
  )
}


# ==== SAVE CACHE FOR DOWNSTREAM SCRIPTS =======================================

# Save only the objects required by downstream scripts.
# This avoids saving the entire global environment with save.image().
dir.create(
  "cache",
  showWarnings = FALSE,
  recursive = TRUE
)

cache_01 <- list(
  code_cols = code_cols,
  master_data = master_data,
  lecture_segments = lecture_segments,
  clicker_segments = clicker_segments,
  pi_segments = pi_segments,
  tps_segments = tps_segments,
  peer_lite_segments = peer_lite_segments,
  clicker_lite_segments = clicker_lite_segments,
  admin_segments = admin_segments,
  student_work_segments = student_work_segments
)

saveRDS(
  cache_01,
  "cache/01_primary_secondary_outputs.rds"
)

message(
  "Saved primary/secondary detector cache: ",
  "cache/01_primary_secondary_outputs.rds"
)

rm(cache_01)
