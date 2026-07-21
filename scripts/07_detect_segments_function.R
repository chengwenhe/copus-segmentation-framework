# ================================================================
# COPUS Segmentation Framework
# 07_detect_segments_function.R
#
# Purpose:
#   Run the complete rule-based segmentation framework on one
#   de-identified COPUS classroom observation.
#
# Main function:
#   result <- detect_segments(dat)
#
# Main outputs:
#   result$segments          # final consecutive instructional segments
#   result$intervals         # one final label per COPUS interval
#   result$display_table     # final + alternative labels for presentation
#   result$alternatives      # detailed priority-masked alternatives
#
# Notes:
#   - This script defines functions only. It does not read data, run the
#     full manuscript pipeline, write files, or create global result objects.
#   - Tertiary detectors run only on intervals left Unlabeled after
#     primary/secondary precedence.
#   - Detector logic and priority rules reproduce scripts 01, 02, 04, and 05.
#   - COPUS intervals are treated as 2 minutes, matching the original pipeline.
# ================================================================

# ---- Minimal package requirements --------------------------------------------

.required_packages <- c("dplyr", "tidyr", "purrr", "tibble")
.missing_packages <- .required_packages[
  !vapply(.required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(.missing_packages) > 0) {
  stop(
    "Please install the following required packages before sourcing this script: ",
    paste(.missing_packages, collapse = ", "),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
})

rm(.required_packages, .missing_packages)


# ---- Constants ---------------------------------------------------------------

COPUS_CODE_COLUMNS <- c(
  "Instructor.1o1", "Instructor.Adm", "Instructor.AnQ", "Instructor.CQ",
  "Instructor.DV", "Instructor.FUp", "Instructor.Lec", "Instructor.MG",
  "Instructor.Other", "Instructor.PQ", "Instructor.RtW", "Instructor.W",
  "Student.AnQ", "Student.CG", "Student.Ind", "Student.L", "Student.OG",
  "Student.Other", "Student.Prd", "Student.SP", "Student.SQ", "Student.TQ",
  "Student.W", "Student.WC", "Student.WG"
)

PRIMARY_SECONDARY_PRIORITY <- tibble::tribble(
  ~label,             ~priority,
  "TPS",                       1L,
  "PeerInstruction",           2L,
  "Clicker",                   3L,
  "PeerLite",                  4L,
  "ClickerLite",               5L,
  "StudentWork",               6L,
  "Lecture",                   7L,
  "Admin",                     8L
)

TERTIARY_PRIORITY <- tibble::tribble(
  ~label,          ~priority,
  "StudentQA",              1L,
  "InstructorQA",           2L,
  "Transition",             3L
)


# ---- Input validation and preprocessing --------------------------------------

prepare_copus_input <- function(dat) {
  if (!is.data.frame(dat)) {
    stop("`dat` must be a data.frame or tibble.", call. = FALSE)
  }
  
  dat <- tibble::as_tibble(dat)
  
  # Accept common capitalization variants for the two identifier columns.
  if (!"time" %in% names(dat)) {
    time_matches <- which(tolower(names(dat)) == "time")
    if (length(time_matches) == 1L) {
      names(dat)[time_matches] <- "time"
    }
  }
  
  if (!"id" %in% names(dat)) {
    id_matches <- which(tolower(names(dat)) == "id")
    if (length(id_matches) == 1L) {
      names(dat)[id_matches] <- "id"
    } else {
      dat$id <- "Selected_Session"
    }
  }
  
  required_columns <- c("id", "time", COPUS_CODE_COLUMNS)
  missing_columns <- setdiff(required_columns, names(dat))
  
  if (length(missing_columns) > 0L) {
    stop(
      "`dat` is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }
  
  dat <- dat %>%
    dplyr::mutate(
      id = as.character(.data$id),
      time = suppressWarnings(as.numeric(.data$time)),
      dplyr::across(
        dplyr::all_of(COPUS_CODE_COLUMNS),
        ~ suppressWarnings(as.numeric(as.character(.x)))
      )
    ) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(COPUS_CODE_COLUMNS),
        ~ tidyr::replace_na(.x, 0)
      )
    )
  
  if (nrow(dat) == 0L) {
    stop("`dat` contains no COPUS intervals.", call. = FALSE)
  }
  
  if (anyNA(dat$time)) {
    stop("`time` must contain numeric interval numbers with no missing values.", call. = FALSE)
  }
  
  if (any(dat$time %% 1 != 0)) {
    stop("`time` must contain whole-number interval indices.", call. = FALSE)
  }
  
  dat <- dat %>%
    dplyr::mutate(time = as.integer(.data$time)) %>%
    dplyr::arrange(.data$id, .data$time)
  
  if (dplyr::n_distinct(dat$id) != 1L) {
    stop(
      "`detect_segments()` accepts exactly one classroom observation at a time. ",
      "Filter the full dataset to one `id` before calling the function.",
      call. = FALSE
    )
  }
  
  if (anyDuplicated(dat[c("id", "time")]) > 0L) {
    stop("`dat` contains duplicated `id` × `time` rows.", call. = FALSE)
  }
  
  if (nrow(dat) > 1L && any(diff(dat$time) != 1L)) {
    stop(
      "`time` must be consecutive within the selected classroom observation.",
      call. = FALSE
    )
  }
  
  dat
}


empty_segment_table <- function() {
  tibble::tibble(
    id = character(),
    start_time = integer(),
    end_time = integer(),
    n_intervals = integer(),
    minutes = integer(),
    type = character(),
    stage = character()
  )
}


# ---- Primary detector: Lecture -----------------------------------------------

detect_lecture_segments <- function(df) {
  df %>%
    dplyr::arrange(.data$id, .data$time) %>%
    dplyr::mutate(
      lec_listen = (`Instructor.Lec` == 1) & (`Student.L` == 1)
    ) %>%
    dplyr::group_by(.data$id) %>%
    dplyr::arrange(.data$time, .by_group = TRUE) %>%
    dplyr::mutate(
      new_run = (dplyr::row_number() == 1L) |
        (.data$time != dplyr::lag(.data$time) + 1L) |
        (.data$lec_listen != dplyr::lag(.data$lec_listen)),
      run_id = cumsum(.data$new_run)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$lec_listen) %>%
    dplyr::group_by(.data$id, .data$run_id) %>%
    dplyr::summarise(
      start_time = min(.data$time),
      end_time = max(.data$time),
      n_intervals = dplyr::n(),
      minutes = n_intervals * 2L,
      type = "Lecture",
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$id, .data$start_time)
}


# ---- Remaining primary and secondary detector functions ----------------------

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

build_pi_flags <- function(df) {
  df %>%
    arrange(id, time) %>%
    mutate(
      phase_prompt  = (`Instructor.CQ` == 1) | (`Instructor.PQ` == 1),
      phase_discuss = (`Student.CG` == 1) | (`Student.OG` == 1) | (`Student.WG` == 1),
      phase_wrap    = (`Instructor.FUp` == 1)
    )
}

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

build_peerlite_flags <- function(df) {
  df %>%
    arrange(id, time) %>%
    mutate(
      phase_prompt  = (`Instructor.PQ` == 1) | (`Instructor.CQ` == 1),
      phase_discuss = (`Student.OG` == 1) | (`Student.WG` == 1) | (`Student.CG` == 1),
      phase_FUp     = (`Instructor.FUp` == 1)
    )
}

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

build_clickerlite_flags <- function(df) {
  df %>%
    arrange(id, time) %>%
    mutate(
      phase_prompt  = (`Instructor.CQ` == 1),
      phase_student = (`Student.Ind` == 1) | (`Student.CG` == 1),
      phase_FUp     = (`Instructor.FUp` == 1)
    )
}

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

build_admin_flags <- function(df) {
  df %>%
    arrange(id, time) %>%
    mutate(
      phase_admin = (`Instructor.Adm` == 1)
    )
}

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

detect_student_work_segments <- function(df) {
  df2 <- build_student_work_flags(df)
  
  df2 %>%
    dplyr::group_by(id) %>%
    dplyr::group_split() %>%
    purrr::map_dfr(~ scan_student_work_one_id(.x)) %>%
    dplyr::arrange(id, start_time)
}

# ---- Collect primary and secondary candidates --------------------------------

detect_primary_secondary_candidates <- function(dat) {
  candidates <- dplyr::bind_rows(
    detect_lecture_segments(dat),
    detect_clicker_segments(dat),
    detect_tps_segments(dat),
    detect_pi_segments(dat),
    detect_peer_lite_segments(dat),
    detect_clicker_lite_segments(dat),
    detect_admin_segments(dat),
    detect_student_work_segments(dat)
  )
  
  if (nrow(candidates) == 0L) {
    return(empty_segment_table())
  }
  
  candidates %>%
    dplyr::mutate(stage = "PrimarySecondary") %>%
    dplyr::arrange(
      .data$id,
      match(.data$type, PRIMARY_SECONDARY_PRIORITY$label),
      .data$start_time,
      .data$end_time
    )
}


# ---- Generic segment-to-interval helpers -------------------------------------

expand_candidate_segments <- function(candidate_segments) {
  required <- c("id", "start_time", "end_time", "type", "stage")
  missing <- setdiff(required, names(candidate_segments))
  
  if (length(missing) > 0L) {
    stop(
      "`candidate_segments` is missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  
  if (nrow(candidate_segments) == 0L) {
    return(tibble::tibble(
      id = character(),
      time = integer(),
      label = character(),
      stage = character()
    ))
  }
  
  expanded <- lapply(seq_len(nrow(candidate_segments)), function(i) {
    start_i <- as.integer(candidate_segments$start_time[[i]])
    end_i <- as.integer(candidate_segments$end_time[[i]])
    
    if (is.na(start_i) || is.na(end_i) || start_i > end_i) {
      stop("A candidate segment has invalid start/end times.", call. = FALSE)
    }
    
    tibble::tibble(
      id = as.character(candidate_segments$id[[i]]),
      time = seq.int(start_i, end_i),
      label = as.character(candidate_segments$type[[i]]),
      stage = as.character(candidate_segments$stage[[i]])
    )
  })
  
  dplyr::bind_rows(expanded) %>%
    dplyr::distinct(.data$id, .data$time, .data$label, .data$stage)
}


rank_candidate_intervals <- function(candidate_intervals, priority_table) {
  if (nrow(candidate_intervals) == 0L) {
    return(tibble::tibble(
      id = character(),
      time = integer(),
      label = character(),
      stage = character(),
      priority = integer(),
      candidate_rank = integer(),
      is_selected = logical()
    ))
  }
  
  ranked <- candidate_intervals %>%
    dplyr::left_join(priority_table, by = "label")
  
  unknown_labels <- ranked %>%
    dplyr::filter(is.na(.data$priority)) %>%
    dplyr::distinct(.data$label) %>%
    dplyr::pull(.data$label)
  
  if (length(unknown_labels) > 0L) {
    stop(
      "No priority was defined for candidate label(s): ",
      paste(unknown_labels, collapse = ", "),
      call. = FALSE
    )
  }
  
  ranked %>%
    dplyr::arrange(.data$id, .data$time, .data$priority, .data$label) %>%
    dplyr::group_by(.data$id, .data$time) %>%
    dplyr::mutate(
      candidate_rank = dplyr::row_number(),
      is_selected = .data$candidate_rank == 1L
    ) %>%
    dplyr::ungroup()
}


detect_unlabeled_segments <- function(interval_df) {
  unlabeled_df <- interval_df %>%
    dplyr::arrange(.data$id, .data$time) %>%
    dplyr::mutate(is_unlabeled = .data$profile == "Unlabeled") %>%
    dplyr::filter(.data$is_unlabeled)
  
  # A fully labeled observation has no residual blocks for tertiary detection.
  # Return a correctly typed empty table rather than summarising an empty group.
  if (nrow(unlabeled_df) == 0L) {
    return(
      tibble::tibble(
        id = character(),
        start_time = integer(),
        end_time = integer(),
        n_intervals = integer(),
        minutes = integer(),
        type = character()
      )
    )
  }
  
  unlabeled_df %>%
    dplyr::group_by(.data$id) %>%
    dplyr::mutate(
      new_run = (dplyr::row_number() == 1L) |
        (.data$time != dplyr::lag(.data$time) + 1L),
      new_run = tidyr::replace_na(.data$new_run, TRUE),
      run_id = cumsum(.data$new_run)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$id, .data$run_id) %>%
    dplyr::summarise(
      start_time = min(.data$time),
      end_time = max(.data$time),
      n_intervals = dplyr::n(),
      minutes = n_intervals * 2L,
      type = "Unlabeled",
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$id, .data$start_time)
}

apply_primary_secondary_priority <- function(dat, candidate_segments) {
  candidate_intervals <- expand_candidate_segments(candidate_segments)
  ranked <- rank_candidate_intervals(
    candidate_intervals,
    PRIMARY_SECONDARY_PRIORITY
  )
  
  winners <- ranked %>%
    dplyr::filter(.data$is_selected) %>%
    dplyr::select(
      id,
      time,
      final_label = label,
      final_priority = priority
    )
  
  intervals <- dat %>%
    dplyr::distinct(.data$id, .data$time) %>%
    dplyr::left_join(winners, by = c("id", "time")) %>%
    dplyr::mutate(
      profile = dplyr::coalesce(.data$final_label, "Unlabeled")
    ) %>%
    dplyr::select(id, time, profile)
  
  alternatives <- ranked %>%
    dplyr::filter(!.data$is_selected) %>%
    dplyr::left_join(
      winners %>%
        dplyr::select(id, time, final_label),
      by = c("id", "time")
    ) %>%
    dplyr::transmute(
      id = .data$id,
      time = .data$time,
      final_label = .data$final_label,
      alternative_label = .data$label,
      alternative_priority = .data$priority,
      alternative_rank = .data$candidate_rank - 1L,
      stage = .data$stage
    )
  
  list(
    intervals = intervals,
    candidate_intervals = ranked,
    alternatives = alternatives,
    unlabeled_segments = detect_unlabeled_segments(intervals)
  )
}


# ---- Tertiary detector functions ---------------------------------------------

build_instructorQA_flags <- function(df_block) {
  df_block %>%
    arrange(time) %>%
    mutate(
      # anchor: instructor poses or follows up on a question; students listen or answer
      anchor_instructorQA =
        ((`Instructor.PQ` == 1) | (`Instructor.FUp` == 1) ) & ( (`Student.AnQ` == 1) | (`Student.L` == 1) ),
      
      # continue: PQ/FUp/RtW/AnQ allowed; students L or AnQ or asking questions
      cont_instructorQA =
        ( (`Instructor.PQ` == 1) | (`Instructor.FUp` == 1) | (`Instructor.RtW` == 1) |  (`Instructor.AnQ` == 1)  ) &
        ( (`Student.AnQ` == 1) | (`Student.SQ` == 1) |  (`Student.L` == 1) )
    )
}

scan_instructorQA_one_block <- function(dd_block,
                                        min_len = 1) {
  stopifnot(all(c("id","time","anchor_instructorQA","cont_instructorQA") %in% names(dd_block)))
  
  dd_block <- dd_block %>% arrange(time)
  n <- nrow(dd_block)
  i <- 1
  out <- list()
  
  while (i <= n) {
    # find next anchor
    j <- i
    while (j <= n && !dd_block$anchor_instructorQA[j]) j <- j + 1
    if (j > n) break
    
    seg_start_idx <- j
    
    # extend while continuation holds (including the anchor interval)
    k <- seg_start_idx
    while (k <= n && dd_block$cont_instructorQA[k]) k <- k + 1
    seg_end_idx <- k - 1
    
    # enforce min length (in intervals)
    if ((seg_end_idx - seg_start_idx + 1) >= min_len) {
      out[[length(out) + 1]] <- tibble::tibble(
        id          = dd_block$id[1],
        start_time  = dd_block$time[seg_start_idx],
        end_time    = dd_block$time[seg_end_idx],
        n_intervals = dd_block$time[seg_end_idx] - dd_block$time[seg_start_idx] + 1,
        minutes     = (dd_block$time[seg_end_idx] - dd_block$time[seg_start_idx] + 1) * 2,
        type        = "InstructorQA"
      )
    }
    
    # move cursor to first interval after this segment
    i <- seg_end_idx + 1
  }
  
  if (length(out) == 0) {
    tibble::tibble(
      id=character(), start_time=integer(), end_time=integer(),
      n_intervals=integer(), minutes=integer(), type=character()
    )
  } else dplyr::bind_rows(out)
}

detect_instructorQA_from_unlabeled <- function(master_data,
                                               unlabeled_segments,
                                               min_len = 1) {
  
  if (nrow(unlabeled_segments) == 0) {
    return(tibble::tibble(
      id=character(), start_time=integer(), end_time=integer(),
      n_intervals=integer(), minutes=integer(), type=character()
    ))
  }
  
  purrr::pmap_dfr(
    unlabeled_segments %>% select(id, start_time, end_time),
    function(id, start_time, end_time) {
      
      block <- master_data %>%
        filter(.data$id == !!id,
               .data$time >= !!start_time,
               .data$time <= !!end_time) %>%
        arrange(time)
      
      # Safety: skip empty blocks
      if (nrow(block) == 0) return(NULL)
      
      dd <- build_instructorQA_flags(block)
      
      scan_instructorQA_one_block(dd, min_len = min_len)
    }
  ) %>%
    arrange(id, start_time)
}

build_studentQA_flags <- function(df_block) {
  df_block %>%
    arrange(time) %>%
    mutate(
      # anchor: MUST have student question + instructor answer or followup
      anchor_studentQA =
        (`Student.SQ` == 1) & ((`Instructor.AnQ` == 1) | (`Instructor.FUp` == 1)),
      
      # continue: instructor answering/followup/writing + students asking, answering, or listening
      cont_studentQA =
        ( (`Instructor.AnQ` == 1) | (`Instructor.FUp` == 1) | (`Instructor.RtW` == 1) ) &
        ( (`Student.SQ` == 1) |  (`Student.AnQ` == 1) | (`Student.L` == 1) )
    )
}

scan_studentQA_one_block <- function(dd_block,
                                     min_len = 1) {
  stopifnot(all(c("id","time","anchor_studentQA","cont_studentQA") %in% names(dd_block)))
  
  dd_block <- dd_block %>% arrange(time)
  n <- nrow(dd_block)
  i <- 1
  out <- list()
  
  while (i <= n) {
    # find next anchor
    j <- i
    while (j <= n && !dd_block$anchor_studentQA[j]) j <- j + 1
    if (j > n) break
    
    seg_start_idx <- j
    
    # extend while continuation holds
    k <- seg_start_idx
    while (k <= n && dd_block$cont_studentQA[k]) k <- k + 1
    seg_end_idx <- k - 1
    
    # enforce min length (in intervals)
    if ((seg_end_idx - seg_start_idx + 1) >= min_len) {
      out[[length(out) + 1]] <- tibble::tibble(
        id          = dd_block$id[1],
        start_time  = dd_block$time[seg_start_idx],
        end_time    = dd_block$time[seg_end_idx],
        n_intervals = dd_block$time[seg_end_idx] - dd_block$time[seg_start_idx] + 1,
        minutes     = (dd_block$time[seg_end_idx] - dd_block$time[seg_start_idx] + 1) * 2,
        type        = "StudentQA"
      )
    }
    
    # advance cursor
    i <- seg_end_idx + 1
  }
  
  if (length(out) == 0) {
    tibble::tibble(
      id=character(), start_time=integer(), end_time=integer(),
      n_intervals=integer(), minutes=integer(), type=character()
    )
  } else dplyr::bind_rows(out)
}

detect_studentQA_from_unlabeled <- function(master_data,
                                            unlabeled_segments,
                                            min_len = 1) {
  
  if (nrow(unlabeled_segments) == 0) {
    return(tibble::tibble(
      id=character(), start_time=integer(), end_time=integer(),
      n_intervals=integer(), minutes=integer(), type=character()
    ))
  }
  
  purrr::pmap_dfr(
    unlabeled_segments %>% dplyr::select(id, start_time, end_time),
    function(id, start_time, end_time) {
      
      block <- master_data %>%
        dplyr::filter(.data$id == !!id,
                      .data$time >= !!start_time,
                      .data$time <= !!end_time) %>%
        dplyr::arrange(time)
      
      if (nrow(block) == 0) return(NULL)
      
      dd <- build_studentQA_flags(block)
      
      scan_studentQA_one_block(dd, min_len = min_len)
    }
  ) %>%
    dplyr::arrange(id, start_time)
}

build_transition_flags <- function(df_block) {
  
  # exclusion sets
  I_excl <- c("Instructor.Lec","Instructor.FUp","Instructor.PQ","Instructor.CQ",
              "Instructor.AnQ","Instructor.MG","Instructor.1o1")
  
  S_excl <- c("Student.Ind","Student.CG","Student.WG","Student.OG",
              "Student.AnQ","Student.SQ","Student.WC","Student.Prd",
              "Student.SP","Student.TQ")
  
  df_block %>%
    arrange(time) %>%
    mutate(
      # base requirement: in-between behavior only
      base_transition =
        ( (`Instructor.W` == 1) | (`Instructor.Other` == 1) ) &
        ( (`Student.W` == 1)    | (`Student.Other` == 1) ),
      
      # exclusion: no instructional codes present in the same interval
      instr_excluded_I = rowSums(dplyr::across(dplyr::all_of(I_excl))) > 0,
      instr_excluded_S = rowSums(dplyr::across(dplyr::all_of(S_excl))) > 0,
      
      # eligible if base holds and no excluded codes present
      eligible_transition = base_transition & (!instr_excluded_I) & (!instr_excluded_S),
      
      # anchor/continue are identical for this detector
      anchor_transition = eligible_transition,
      cont_transition   = eligible_transition
    )
}

scan_transition_one_block <- function(dd_block,
                                      min_len = 1) {
  stopifnot(all(c("id","time","anchor_transition","cont_transition") %in% names(dd_block)))
  
  dd_block <- dd_block %>% arrange(time)
  n <- nrow(dd_block)
  i <- 1
  out <- list()
  
  while (i <= n) {
    
    # find next anchor
    j <- i
    while (j <= n && !dd_block$anchor_transition[j]) j <- j + 1
    if (j > n) break
    
    seg_start_idx <- j
    
    # extend while continuation holds
    k <- seg_start_idx
    while (k <= n && dd_block$cont_transition[k]) k <- k + 1
    seg_end_idx <- k - 1
    
    # enforce min length
    if ((seg_end_idx - seg_start_idx + 1) >= min_len) {
      out[[length(out) + 1]] <- tibble::tibble(
        id          = dd_block$id[1],
        start_time  = dd_block$time[seg_start_idx],
        end_time    = dd_block$time[seg_end_idx],
        n_intervals = dd_block$time[seg_end_idx] - dd_block$time[seg_start_idx] + 1,
        minutes     = (dd_block$time[seg_end_idx] - dd_block$time[seg_start_idx] + 1) * 2,
        type        = "Transition"
      )
    }
    
    i <- seg_end_idx + 1
  }
  
  if (length(out) == 0) {
    tibble::tibble(
      id=character(), start_time=integer(), end_time=integer(),
      n_intervals=integer(), minutes=integer(), type=character()
    )
  } else dplyr::bind_rows(out)
}

detect_transition_from_unlabeled <- function(master_data,
                                             unlabeled_segments,
                                             min_len = 1) {
  
  if (nrow(unlabeled_segments) == 0) {
    return(tibble::tibble(
      id=character(), start_time=integer(), end_time=integer(),
      n_intervals=integer(), minutes=integer(), type=character()
    ))
  }
  
  purrr::pmap_dfr(
    unlabeled_segments %>% dplyr::select(id, start_time, end_time),
    function(id, start_time, end_time) {
      
      block <- master_data %>%
        dplyr::filter(.data$id == !!id,
                      .data$time >= !!start_time,
                      .data$time <= !!end_time) %>%
        dplyr::arrange(time)
      
      if (nrow(block) == 0) return(NULL)
      
      dd <- build_transition_flags(block)
      
      scan_transition_one_block(dd, min_len = min_len)
    }
  ) %>%
    dplyr::arrange(id, start_time)
}

# ---- Collect and prioritize tertiary candidates ------------------------------

detect_tertiary_candidates <- function(dat, unlabeled_segments) {
  if (nrow(unlabeled_segments) == 0L) {
    return(empty_segment_table())
  }
  
  candidates <- dplyr::bind_rows(
    detect_instructorQA_from_unlabeled(
      master_data = dat,
      unlabeled_segments = unlabeled_segments,
      min_len = 1
    ),
    detect_studentQA_from_unlabeled(
      master_data = dat,
      unlabeled_segments = unlabeled_segments,
      min_len = 1
    ),
    detect_transition_from_unlabeled(
      master_data = dat,
      unlabeled_segments = unlabeled_segments,
      min_len = 1
    )
  )
  
  if (nrow(candidates) == 0L) {
    return(empty_segment_table())
  }
  
  candidates %>%
    dplyr::mutate(stage = "Tertiary") %>%
    dplyr::arrange(
      .data$id,
      match(.data$type, TERTIARY_PRIORITY$label),
      .data$start_time,
      .data$end_time
    )
}


apply_tertiary_priority <- function(primary_intervals, candidate_segments) {
  residual_grid <- primary_intervals %>%
    dplyr::filter(.data$profile == "Unlabeled") %>%
    dplyr::select(id, time)
  
  candidate_intervals <- expand_candidate_segments(candidate_segments) %>%
    dplyr::inner_join(residual_grid, by = c("id", "time"))
  
  ranked <- rank_candidate_intervals(candidate_intervals, TERTIARY_PRIORITY)
  
  winners <- ranked %>%
    dplyr::filter(.data$is_selected) %>%
    dplyr::select(
      id,
      time,
      tertiary_label = label,
      tertiary_priority = priority
    )
  
  intervals <- primary_intervals %>%
    dplyr::left_join(winners, by = c("id", "time")) %>%
    dplyr::mutate(
      profile = dplyr::if_else(
        .data$profile == "Unlabeled" & !is.na(.data$tertiary_label),
        .data$tertiary_label,
        .data$profile
      )
    ) %>%
    dplyr::select(id, time, profile)
  
  alternatives <- ranked %>%
    dplyr::filter(!.data$is_selected) %>%
    dplyr::left_join(
      winners %>%
        dplyr::select(
          id,
          time,
          final_label = tertiary_label
        ),
      by = c("id", "time")
    ) %>%
    dplyr::transmute(
      id = .data$id,
      time = .data$time,
      final_label = .data$final_label,
      alternative_label = .data$label,
      alternative_priority = .data$priority,
      alternative_rank = .data$candidate_rank - 1L,
      stage = .data$stage
    )
  
  list(
    intervals = intervals,
    candidate_intervals = ranked,
    alternatives = alternatives
  )
}


# ---- Collapse final interval labels into consecutive segments ----------------

assign_final_segment_ids <- function(intervals) {
  intervals %>%
    dplyr::arrange(.data$id, .data$time) %>%
    dplyr::group_by(.data$id) %>%
    dplyr::mutate(
      new_segment = (dplyr::row_number() == 1L) |
        (.data$time != dplyr::lag(.data$time) + 1L) |
        (.data$profile != dplyr::lag(.data$profile)),
      new_segment = tidyr::replace_na(.data$new_segment, TRUE),
      Segment_ID = cumsum(.data$new_segment)
    ) %>%
    dplyr::ungroup()
}


collapse_final_segments <- function(intervals_with_segment_ids) {
  intervals_with_segment_ids %>%
    dplyr::group_by(.data$id, .data$Segment_ID) %>%
    dplyr::summarise(
      Start = min(.data$time),
      End = max(.data$time),
      Label = dplyr::first(.data$profile),
      n_intervals = dplyr::n(),
      Minutes = n_intervals * 2L,
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$id, .data$Segment_ID)
}


collapse_alternative_labels <- function(
    interval_alternatives,
    intervals_with_segment_ids
) {
  if (nrow(interval_alternatives) == 0L) {
    return(tibble::tibble(
      Segment_ID = integer(),
      Start = integer(),
      End = integer(),
      Final_Label = character(),
      Alternative_Label = character(),
      Alternative_Rank = integer(),
      Type = character(),
      Stage = character()
    ))
  }
  
  alternative_runs <- interval_alternatives %>%
    dplyr::inner_join(
      intervals_with_segment_ids %>%
        dplyr::select(
          id,
          time,
          Segment_ID,
          Final_Label = profile
        ),
      by = c("id", "time")
    ) %>%
    dplyr::filter(.data$final_label == .data$Final_Label) %>%
    dplyr::arrange(
      .data$id,
      .data$Segment_ID,
      .data$stage,
      .data$alternative_priority,
      .data$alternative_label,
      .data$time
    ) %>%
    dplyr::group_by(
      .data$id,
      .data$Segment_ID,
      .data$Final_Label,
      .data$stage,
      .data$alternative_label,
      .data$alternative_priority
    ) %>%
    dplyr::mutate(
      new_alt_run = (dplyr::row_number() == 1L) |
        (.data$time != dplyr::lag(.data$time) + 1L),
      new_alt_run = tidyr::replace_na(.data$new_alt_run, TRUE),
      alt_run_id = cumsum(.data$new_alt_run)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(
      .data$id,
      .data$Segment_ID,
      .data$Final_Label,
      .data$stage,
      .data$alternative_label,
      .data$alternative_priority,
      .data$alt_run_id
    ) %>%
    dplyr::summarise(
      Start = min(.data$time),
      End = max(.data$time),
      .groups = "drop"
    ) %>%
    dplyr::arrange(
      .data$id,
      .data$Segment_ID,
      .data$alternative_priority,
      .data$Start,
      .data$End,
      .data$alternative_label
    ) %>%
    dplyr::group_by(.data$id, .data$Segment_ID) %>%
    dplyr::mutate(
      Alternative_Rank = dplyr::row_number(),
      Type = paste("Alternative", .data$Alternative_Rank)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      Segment_ID = .data$Segment_ID,
      Start = .data$Start,
      End = .data$End,
      Final_Label = .data$Final_Label,
      Alternative_Label = .data$alternative_label,
      Alternative_Rank = .data$Alternative_Rank,
      Type = .data$Type,
      Stage = .data$stage
    )
  
  alternative_runs
}


build_display_table <- function(segments, alternatives) {
  final_rows <- segments %>%
    dplyr::transmute(
      Segment_ID = .data$Segment_ID,
      Start = .data$Start,
      End = .data$End,
      Label = .data$Label,
      Type = "Final",
      .display_order = 0L
    )
  
  if (nrow(alternatives) == 0L) {
    return(
      final_rows %>%
        dplyr::select(-".display_order")
    )
  }
  
  alternative_rows <- alternatives %>%
    dplyr::transmute(
      Segment_ID = .data$Segment_ID,
      Start = .data$Start,
      End = .data$End,
      Label = .data$Alternative_Label,
      Type = .data$Type,
      .display_order = .data$Alternative_Rank
    )
  
  dplyr::bind_rows(final_rows, alternative_rows) %>%
    dplyr::arrange(
      .data$Segment_ID,
      .data$.display_order,
      .data$Start,
      .data$End,
      .data$Label
    ) %>%
    dplyr::select(-".display_order")
}


# ---- Main user-facing function -----------------------------------------------

detect_segments <- function(dat) {
  dat_clean <- prepare_copus_input(dat)
  
  # Stage 1: primary and secondary detectors + precedence
  primary_secondary_segments <- detect_primary_secondary_candidates(dat_clean)
  
  primary_result <- apply_primary_secondary_priority(
    dat = dat_clean,
    candidate_segments = primary_secondary_segments
  )
  
  # Stage 2: tertiary detectors only on residual Unlabeled intervals
  tertiary_segments <- detect_tertiary_candidates(
    dat = dat_clean,
    unlabeled_segments = primary_result$unlabeled_segments
  )
  
  tertiary_result <- apply_tertiary_priority(
    primary_intervals = primary_result$intervals,
    candidate_segments = tertiary_segments
  )
  
  # Final interval labels and consecutive final segments
  intervals_with_segment_ids <- assign_final_segment_ids(
    tertiary_result$intervals
  )
  
  segment_details <- collapse_final_segments(intervals_with_segment_ids)
  
  segments <- segment_details %>%
    dplyr::select(
      Segment_ID,
      Start,
      End,
      Label
    )
  
  interval_alternatives <- dplyr::bind_rows(
    primary_result$alternatives,
    tertiary_result$alternatives
  )
  
  alternatives <- collapse_alternative_labels(
    interval_alternatives = interval_alternatives,
    intervals_with_segment_ids = intervals_with_segment_ids
  )
  
  candidate_segments <- dplyr::bind_rows(
    primary_secondary_segments,
    tertiary_segments
  ) %>%
    dplyr::arrange(
      factor(.data$stage, levels = c("PrimarySecondary", "Tertiary")),
      .data$start_time,
      .data$end_time,
      .data$type
    )
  
  candidate_intervals <- dplyr::bind_rows(
    primary_result$candidate_intervals,
    tertiary_result$candidate_intervals
  ) %>%
    dplyr::arrange(
      factor(.data$stage, levels = c("PrimarySecondary", "Tertiary")),
      .data$time,
      .data$priority,
      .data$label
    )
  
  result <- list(
    segments = segments,
    intervals = intervals_with_segment_ids %>%
      dplyr::transmute(
        id = .data$id,
        time = .data$time,
        Segment_ID = .data$Segment_ID,
        Label = .data$profile
      ),
    display_table = build_display_table(segments, alternatives),
    alternatives = alternatives,
    segment_details = segment_details,
    candidate_segments = candidate_segments,
    candidate_intervals = candidate_intervals,
    input_data = dat_clean
  )
  
  class(result) <- c("copus_segmentation_result", "list")
  result
}


# Print only the clean final segment table when the result object is displayed.
print.copus_segmentation_result <- function(x, ...) {
  print(x$segments, ...)
  invisible(x)
}

