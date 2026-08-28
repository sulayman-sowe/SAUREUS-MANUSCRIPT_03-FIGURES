 
# ....................................DATA CLEANING........................................
library(readxl)
DATA_03_ANALYSIS<- read_excel("~/WORKINGS/DATABASES/DATA_03_ANALYSIS.xlsx")
View(DATA_03_ANALYSIS)

getwd()
setwd("C:/Users/sulsowe/Documents/WORKINGS/PLOTS/Manuscript_03")

# PVS_PSP
library(dplyr)
PVS_PSP_ISOLATES <- FINALIZED_DATA %>%
  filter(Site != "MRCG")

setwd("C:/Users/sulsowe/Documents/WORKINGS/DATABASES")
write_xlsx(PVS_PSP_ISOLATES, "PVS_PSP_ISOLATES.xlsx")

# Extract key fields from SAMPLE_ID
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)


DATASET_MANUSCRIPT_03 <- read_excel("C:/Users/sulsowe/Documents/WORKINGS/DATABASES/DATASET_MANUSCRIPT_03.xlsx")

# Check 1: Each BC has a unique study_ID
bc_counts <- DATASET_MANUSCRIPT_03 %>%
  filter(Specimen_Type == "INVASIVE") %>%
  count(Study_ID)

bc_counts %>% filter(n > 1) # if multiple BC isolates per patient--must be justified in methods


# Check 2: NPS isolates carry a valid study_ID (corresponding BC)
DATASET_MANUSCRIPT_03_with_ORPHAN_NPS <- read_excel("C:/Users/sulsowe/Documents/WORKINGS/DATABASES/DATASET_MANUSCRIPT_03_with_ORPHAN_NPS.xlsx")

nps_ids <- DATASET_MANUSCRIPT_03_with_ORPHAN_NPS %>%
  filter(Specimen_Type == "NON-INVASIVE") %>%
  pull(Study_ID)

bc_ids <- DATASET_MANUSCRIPT_03_with_ORPHAN_NPS%>%
  filter(Specimen_Type == "INVASIVE") %>%
  pull(Study_ID)

invalid_nps_ids <- setdiff(nps_ids, bc_ids)

invalid_nps_ids


#Alternative Check 2: No Orphan NPS isolates

orphan_nps <- DATASET_MANUSCRIPT_03_with_ORPHAN_NPS %>%
  filter(Specimen_Type == "NON-INVASIVE") %>%
  anti_join(
   DATASET_MANUSCRIPT_03_with_ORPHAN_NPS%>% filter(Specimen_Type == "INVASIVE") %>% distinct(Study_ID),
    by = "Study_ID"
  )

orphan_nps

# Check 3: Reverse Check--BC Without NPS

bc_without_nps <- DATASET_MANUSCRIPT_03 %>%
  filter(Specimen_Type == "INVASIVE") %>%
  anti_join(
    DATASET_MANUSCRIPT_03 %>% filter(Specimen_Type == "NON-INVASIVE") %>% distinct(Study_ID),
    by = "Study_ID"
  )

bc_without_nps # NPS samples with no growth, or Missing carriage sampling




# FINALLY
# Restricting my  data set to matched pairs only (BC cases that have ≥1 valid NPS isolate)

valid_ids <- DATASET_MANUSCRIPT_03 %>%
  group_by(Study_ID) %>%
  summarise(
    has_BC = any(Specimen_Type == "INVASIVE"),
    has_NPS = any(Specimen_Type == "NON-INVASIVE"),
    .groups = "drop"
  ) %>%
  filter(has_BC & has_NPS) %>%
  pull(Study_ID)
# Then filter Dataset
DATA_03 <- DATASET_MANUSCRIPT_03 %>%
  filter(Study_ID %in% valid_ids)

# Check: Every Patient has BC
DATA_03 %>%
  group_by(Study_ID) %>%
  summarise(has_BC = any(Specimen_Type == "INVASIVE")) %>%
  filter(!has_BC)

# Every Patient has NPS
DATA_03 %>%
  group_by(Study_ID) %>%
  summarise(has_NPS = any(Specimen == "NPS")) %>%
  filter(!has_NPS)

# Count Paired Patients 
DATA_03 %>%
  distinct(Study_ID) %>%
  nrow()


library(writexl)
write_xlsx(DATA_03, "DATA_03.xlsx")


# ........................................FIGURES AND TABLES........................

# Table 1: Patient demographics and clinical characteristics
library(dplyr)
library(readxl)
DATASET_MANUSCRIPT_03 <- read_excel("C:/Users/sulsowe/Documents/WORKINGS/DATABASES/DATASET_MANUSCRIPT_03.xlsx")
  
Table_01 <- DATASET_MANUSCRIPT_03 %>%
  filter(Specimen_Type == "INVASIVE")

library(writexl)
write_xlsx(Table_01, "Table_01.xlsx")

# Table 2: Within-patient isolate matching table 

library(dplyr)

# BC table (one row per patient)
bc_table <- DATA_03 %>%
  filter(Specimen_Type == "INVASIVE") %>%
  select(Study_ID, BC_ST = ST)

# NPS table (collapse to set per patient)
nps_table <- DATA_03%>%
  filter(Specimen_Type == "NON-INVASIVE") %>%
  group_by(Study_ID) %>%
  summarise(
    NPS_STs = list(unique(ST)),
    n_NPS_isolates = n(),
    n_unique_STs = n_distinct(ST),
    .groups = "drop"
  )

# Merge and compute match
matching_table_02<- bc_table %>%
  left_join(nps_table, by = "Study_ID") %>%
  mutate(
    BC_in_NPS = mapply(function(bc, nps) bc %in% nps, BC_ST, NPS_STs)
  )

#Proportions
matching_table_02 %>%
  summarise(
    total_patients = n(),
    matches = sum(BC_in_NPS),
    non_matches = sum(!BC_in_NPS),
    proportion_match = mean(BC_in_NPS)
  )
# Publication 
library(knitr)

matching_table_02 %>%
  count(BC_in_NPS) %>%
  mutate(
    BC_in_NPS = ifelse(BC_in_NPS, "Match", "No Match")
  ) %>%
  kable(col.names = c("Concordance", "Number of Patients"))


# Alt 2: Cohort Age Breakdown Table

library(dplyr)

# 1. Standardize age units to months and fix missing sex data
cleaned_cohort <- DATA_03 %>%
  mutate(
    Age_In_Months = ifelse(Age_Type == "Days", round(Age / 30.44, 1), Age),
    Sex = ifelse(Sex == "" | is.na(Sex), "Unknown", Sex),
    Season = tools::toTitleCase(tolower(Season))
  )

# 2. Get quick count breakdowns
cleaned_cohort %>% count(Sex)
cleaned_cohort %>% count(Season)

# 3. Get median and range metrics for age
summary(cleaned_cohort$Age_In_Months)


# Table 3: 
library(dplyr)

# BC: one CC per patient
bc_cc <- DATASET_MANUSCRIPT_03%>%
  filter(Specimen_Type == "INVASIVE") %>%
  select(Study_ID, Clonal_Complex) %>%
  mutate(Source = "INVASIVE")

# NPS: collapse multiple colonies -- unique CCs per patient
nps_cc <- DATASET_MANUSCRIPT_03 %>%
  filter(Specimen == "NPS") %>%
  distinct(Study_ID, Clonal_Complex) %>%  
  mutate(Source = "NON-INVASIVE")

# Combine
cc_patient_level <- bind_rows(bc_cc, nps_cc)

# Table
cc_table <- cc_patient_level %>%
  count(Source, Clonal_Complex) %>%
  group_by(Source) %>%
  mutate(
    total = sum(n),
    proportion = n / total *100
  ) %>%
  ungroup()

library(tidyr)

cc_table_wide <- cc_table %>%
  select(Source, Clonal_Complex, n, proportion) %>%
  pivot_wider(
    names_from = Source,
    values_from = c(n, proportion),  # Proportions are within-source
    values_fill = 0
  )

cc_table_wide

# Paired comparision per CC
cc_presence <- cc_patient_level %>%
  mutate(present = 1) %>%
  pivot_wider(
    names_from = Source,
    values_from = present,
    values_fill = 0
  )

cc_comparison <- cc_presence %>%
  group_by(Clonal_Complex) %>%
  summarise(
    BC_only = sum(INVASIVE == 1 & `NON-INVASIVE` == 0),
    NPS_only = sum(INVASIVE == 0 & `NON-INVASIVE` == 1),
    Both = sum(INVASIVE == 1 & `NON-INVASIVE` == 1)
  )

# Alt

library(dplyr)
library(tidyr)
library(purrr)
library(broom)
library(glue)


# 1. STANDARDIZE DATA & COMPUTE EXPERIMENTAL DENOMINATORS

clean_data <- DATA_03 %>%
  filter(!is.na(ST),ST != "NA", ST!= "") %>%
  mutate(Group = ifelse(Specimen_Type == "INVASIVE", "BC", "NPS"))

# Global Denominators
total_bc_isolates  <- sum(clean_data$Group == "BC")
total_nps_isolates <- sum(clean_data$Group == "NPS")
total_patients     <- n_distinct(clean_data$Study_ID) # Fixed pool size (N = 35)


# 2. SEPARATE AGGREGATIONS (Isolate vs Patient Levels)


# A. BC Isolates (1 per patient)
bc_counts <- clean_data %>%
  filter(Group == "BC") %>%
  group_by(ST, Clonal_Complex) %>%
  summarise(n_BC = n(), .groups = "drop") %>%
  mutate(pct_BC = (n_BC / total_bc_isolates) * 100)

# B. Raw NPS Isolates (Biased by multiple picks per patient, but kept for sample reporting)
nps_iso_counts <- clean_data %>%
  filter(Group == "NPS") %>%
  group_by(ST) %>%
  summarise(n_NPS_iso = n(), .groups = "drop") %>%
  mutate(pct_NPS_iso = (n_NPS_iso / total_nps_isolates) * 100)

# C. Patient-Level NPS (RESOLVES COMPLEXITY: Unique STs per patient)
# If a patient has 4 colonies of ST5, this safely collapses it to n = 1 event.
nps_patient_counts <- clean_data %>%
  filter(Group == "NPS") %>%
  distinct(Study_ID, ST) %>% 
  group_by(ST) %>%
  summarise(n_NPS_pat = n(), .groups = "drop") %>%
  mutate(pct_NPS_pat = (n_NPS_pat / total_patients) * 100)

# 3. COMPENSATE FOR MISSED CLONAL COMPLEXES IN MERGE

# Gather a master list of all unique STs and map them safely to their CCs
st_cc_lookup <- clean_data %>% 
  distinct(ST, Clonal_Complex) %>% 
  filter(!is.na(Clonal_Complex))

table_3_base <- data.frame(ST = unique(clean_data$ST)) %>%
  left_join(st_cc_lookup, by = "ST") %>%
  left_join(bc_counts %>% select(-Clonal_Complex), by = "ST") %>%
  left_join(nps_iso_counts, by = "ST") %>%
  left_join(nps_patient_counts, by = "ST") %>%
  mutate(across(starts_with("n_"), ~replace_na(., 0)),
         across(starts_with("pct_"), ~replace_na(., 0)))


# 4. RESOLVE STATISTICAL COMPLEXITY (Un-biased Fisher's Test)

# We test: Is an ST significantly overrepresented in the Blood relative to its
# true background colonization rate among patients?
table_3_stats <- table_3_base %>%
  rowwise() %>%
  mutate(
    # Construct a 2x2 table using true patient-level event counts
    contingency_matrix = list(matrix(c(
      n_BC, n_NPS_pat,                                      # Target ST events
      total_bc_isolates - n_BC, total_patients - n_NPS_pat  # All other ST events
    ), nrow = 2)),
    
    # Run exact test (safe for small cell counts / zero values)
    test_results = list(tidy(fisher.test(contingency_matrix)))
  ) %>%
  unnest(test_results) %>%
  ungroup()


# 5. FINAL STRING LAYOUT FOR TEXT REPORTING

final_table_3 <- table_3_stats %>%
  transmute(
    `ST` = ST,
    `Clonal Complex (CC)` = Clonal_Complex,
    `BC Isolates n (%)` = glue("{n_BC} ({round(pct_BC, 1)}%)"),
    `Raw NPS Isolates n (%)` = glue("{n_NPS_iso} ({round(pct_NPS_iso, 1)}%)"),
    `Patient-level NPS n (%)` = glue("{n_NPS_pat} ({round(pct_NPS_pat, 1)}%)"),
    `Odds Ratio (95% CI)` = ifelse(is.nan(estimate), "Undetermined", 
                                   sprintf("%.2f (%.2f - %.2f)", estimate, conf.low, conf.high)),
    `p-value` = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
  ) %>%
  arrange(desc(readr::parse_number(`BC Isolates n (%)`)))

print(final_table_3, n = 25)


# ALT TABLE 4 ANALYSIS (VIRULENCE GENES OR AMR GENES)

library(dplyr)
library(tidyr)
library(lme4)
library(purrr)
library(broom.mixed)
library(knitr)


# STEP 0:  VIRULENCE GENE VECTOR

virulence_genes <- c(
  "clfA,clfB", "fnbA,fnbB", "spa", "cna", "ebpS", "coa,vwb", "sak", "hysA",
  "lip", "nuc", "hla/hly", "hlb", "hld", "hlg,lukS-PV,lukF-PV", "tst",
  "sea-e, seg-sei", "eta,etb", "cap operon", "chp", "isdA,isdB,isdC",
  "sfa,sfb", "psmα, psmβ", "agrA,agrB,agrC,agrD", "sarA", "saeR,saeS",
  "icaA,icaB,icaC,icaD", "bap", "aur", "lipoteichoic acid", "peptidoglycan"
)
 
amr_genes <- c("blaZ","dfrG","tet(K)","msr(A)","tet(M)","tet(L)","grlA","mecA",
               "aac(6')-aph(2'')","fusB","qacD","erm(C)","cat(pC233)","aph(3')-III",
               "ant(6)-Ia","ant(9)-Ia","erm(A)","gyrA","qacB","qacJ","rpoB","aadD",
               "bleO","mph(C)","vga(A)","dfrB","fusC","mecA1","sal(A)",
               "fosD","Isa(A)","vga(A)V","fosB","cat(pC221)","fusA")

# Identify matched patients using precise backticks to handle the hyphen in NON-INVASIVE
patient_st_matrix <- DATA_03 %>%
  filter(Specimen_Type %in% c("INVASIVE", "NON-INVASIVE")) %>%
  select(Study_ID, Specimen_Type, ST) %>%
  distinct() %>%
  pivot_wider(names_from = Specimen_Type, values_from = ST, values_fn = list) %>%
  rowwise()

matched_patients <- patient_st_matrix %>%
  filter(any(`INVASIVE` %in% `NON-INVASIVE`)) %>%
  pull(Study_ID)


# STEP 1: LONG FORMAT MASTER TRANSFORMATION

master_long_df <- DATA_03 %>%
  filter(Specimen_Type %in% c("INVASIVE", "NON-INVASIVE")) %>%
  select(Study_ID, Sample_ID, Specimen_Type, all_of(amr_genes)) %>%
  pivot_longer(
    cols = all_of(amr_genes),
    names_to = "Gene",
    values_to = "Presence"
  ) %>%
  mutate(
    Presence = ifelse(Presence %in% c("Yes", 1, "Y", TRUE), 1, 0),
    Is_Invasive = ifelse(Specimen_Type == "INVASIVE", 1, 0)
  )


# STEP 2: DEFINE GLMM PIPELINE CORE FUNCTION

run_within_patient_glmm <- function(target_gene, working_df) {
  gene_sub <- working_df %>% filter(Gene == target_gene)
  
  # Handle invariant features safely (all 0s or all 1s across the subset)
  if (length(unique(gene_sub$Presence)) < 2) {
    return(data.frame(Gene = target_gene, Odds_Ratio = "N/A", P_Value = "Invariant"))
  }
  
  # Check for complete separation anomalies
  counts <- table(gene_sub$Specimen_Type, gene_sub$Presence)
  if(nrow(counts) == 2 && ncol(counts) == 2) {
    if(counts[1,1] == 0 || counts[1,2] == 0 || counts[2,1] == 0 || counts[2,2] == 0) {
      return(data.frame(Gene = target_gene, Odds_Ratio = "Inf/Zero", P_Value = "Separation"))
    }
  }
  
  model <- tryCatch({
    glmer(Presence ~ Is_Invasive + (1 | Study_ID), data = gene_sub, family = binomial(link = "logit"))
  }, error = function(e) {
    # Fail-safe to standard logistic regression if the random intercept variance drops to zero
    suppressWarnings(glm(Presence ~ Is_Invasive, data = gene_sub, family = binomial(link = "logit")))
  })
  
  tidy_model <- suppressWarnings(tryCatch({
    broom.mixed::tidy(model, conf.int = TRUE, exponentiate = TRUE) %>%
      filter(term == "Is_Invasive")
  }, error = function(e) { NULL }))
  
  if (is.null(tidy_model) || nrow(tidy_model) == 0) {
    return(data.frame(Gene = target_gene, Odds_Ratio = "NC", P_Value = "No Conv."))
  }
  
  or <- round(tidy_model$estimate, 2)
  ci_lower <- round(tidy_model$conf.low, 2)
  ci_upper <- round(tidy_model$conf.high, 2)
  p_val <- tidy_model$p.value
  
  data.frame(
    Gene = target_gene,
    Odds_Ratio = paste0(or, " (", ci_lower, "-", ci_upper, ")"),
    P_Value = if(p_val < 0.001) "< 0.001" else as.character(round(p_val, 3))
  )
}

# Output formatter
build_output_table <- function(raw_long_data, model_output, title) {
  prev <- raw_long_data %>%
    group_by(Gene, Specimen_Type) %>%
    summarise(Total = n(), Present = sum(Presence), Pct = round((Present/Total)*100, 1), .groups = "drop") %>%
    mutate(Label = ifelse(Specimen_Type == "INVASIVE", "BC_Prev", "NPS_Prev")) %>%
    select(Gene, Label, Pct) %>%
    pivot_wider(names_from = Label, values_from = Pct)
  
  if(!"BC_Prev" %in% colnames(prev)) prev$BC_Prev <- 0
  if(!"NPS_Prev" %in% colnames(prev)) prev$NPS_Prev <- 0
  
  final_table <- prev %>%
    left_join(model_output, by = "Gene") %>%
    rename(
      `AMR gene` = Gene, `BC (%)` = BC_Prev, `NPS (%)` = NPS_Prev,
      `Odds Ratio (95% CI)` = Odds_Ratio, `P-Value` = P_Value
    )
  
  print(paste("===", title, "==="))
  print(kable(final_table, format = "markdown", align = "lcccc"))
  return(final_table)
}


# ANALYSIS 1: MATCHED PAIRS ONLY

matched_long_df <- master_long_df %>% filter(Study_ID %in% matched_patients)
matched_models <- map_df(amr_genes, run_within_patient_glmm, working_df = matched_long_df)

analysis1_table <- build_output_table(
  raw_long_data = matched_long_df, 
  model_output = matched_models, 
  title = "Analysis 1: Matched Pairs Only"
)
write.csv(analysis1_table, "Table_Analysis1_AMR-genes_Matched_Only.csv", row.names = FALSE)


# ANALYSIS 2: ALL PATIENT PAIRS (n=35 patients)

all_models <- map_df(amr_genes, run_within_patient_glmm, working_df = master_long_df)

analysis2_table <- build_output_table(
  raw_long_data = master_long_df, 
  model_output = all_models, 
  title = "Analysis 2: All Patient Pairs (n=35)"
)
write.csv(analysis2_table, "Table_Analysis2_amr genes_All_Pairs.csv", row.names = FALSE)


# Table 5:
library(dplyr)

# Identify AMR gene columns
amr_cols <- c(
  "blaZ", "dfrG", "tet(K)", "msr(A)", "tet(M)", "tet(L)", "grlA", "mecA",
  "aac(6')-aph(2'')", "fusB", "qacD", "erm(C)", "cat(pC233)", "aph(3')-III",
  "ant(6)-Ia", "ant(9)-Ia", "erm(A)", "gyrA", "qacB", "qacJ", "rpoB", "aadD",
  "bleO", "mph(C)", "vga(A)", "dfrB", "fusC", "mecA1", "sal(A)",
  "fosD", "Isa(A)", "vga(A)V", "fosB", "cat(pC221)", "fusA"
)

library(dplyr)
library(dplyr)
library(stringr)

df_amr<- DATASET_MANUSCRIPT_03%>%
  mutate(
    across(all_of(amr_cols), ~ case_when(
      str_to_lower(str_trim(.)) %in% "yes" ~ 1L,
      str_to_lower(str_trim(.)) %in% "no" ~ 0L,
      is.na(.) ~ NA_integer_,
      TRUE ~ NA_integer_   # anything unexpected → NA (forces you to notice)
    ))
  )

# BC (one row per patient)
bc_amr <-df_amr %>%
  filter(Specimen_Type == "INVASIVE") %>%
  select(Study_ID, all_of(amr_cols)) %>%
  mutate(Source = "BC")

# NPS (collapse multiple colonies)
nps_amr <- df_amr %>%
  filter(Specimen == "NPS") %>%
  group_by(Study_ID) %>%
  summarise(
    across(all_of(amr_cols), ~ as.integer(any(. == 1, na.rm = TRUE))),
    .groups = "drop"
  ) %>%
  mutate(Source = "NPS")

# Combine
amr_patient <- bind_rows(bc_amr, nps_amr)

# convert to long format--avoids headaches with col names
library(tidyr)

amr_long <- amr_patient %>%
  pivot_longer(
    cols = all_of(amr_cols),
    names_to = "Gene",
    values_to = "Present"
  )

# Compute summary
amr_summary <- amr_long %>%
  group_by(Source, Gene) %>%
  summarise(
    Count = sum(Present == 1),
    Total = n(),
    Prevalence = Count / Total,
    .groups = "drop"
  )

amr_table <- amr_summary %>%
  select(Source, Gene, Count, Prevalence) %>%
  pivot_wider(
    names_from = Source,
    values_from = c(Count, Prevalence),
    values_fill = 0
  )

amr_table <- amr_table %>%
  mutate(
    Prev_BC = round(Prevalence_BC * 100, 1),
    Prev_NPS = round(Prevalence_NPS * 100, 1)
  )


# FIGURE 1: Patient-level BC/NPS isolate matching by MLST

library(dplyr)
library(tidyr)
library(igraph)
library(ggraph)
library(ggplot2)
library(purrr)
library(stringr)


# CLEAN VARIABLES


DATA_03 <- DATA_03 %>%
  mutate(
    Study_ID = trimws(as.character(Study_ID)),
    Sample_ID = trimws(as.character(Sample_ID)),
    Specimen_Type = trimws(as.character(Specimen_Type)),
    ST = as.character(ST)
  )

# CREATE BC REFERENCE TABLE

bc_nodes <- DATA_03 %>%
  filter(Specimen_Type == "INVASIVE") %>%
  select(
    Study_ID,
    BC_ID = Sample_ID,
    BC_ST = ST
  )

# LINK NPS ISOLATES TO BC

network_df <- DATA_03 %>%
  filter(Specimen_Type == "NON-INVASIVE") %>%
  inner_join(bc_nodes, by = "Study_ID") %>%
  mutate(
    Match = ifelse(ST == BC_ST, "Match", "Non-match")
  )

# CREATE EDGE TABLE

edges <- network_df %>%
  transmute(
    Study_ID,
    from = BC_ID,
    to = Sample_ID,
    Match
  )

# CREATE NODE TABLE 
bc_vertices <- DATA_03 %>% 
  filter(Specimen_Type == "INVASIVE") %>% 
  transmute(name = Sample_ID, Study_ID, Specimen_Type, ST)

nps_vertices <- DATA_03 %>% 
  filter(Specimen_Type == "NON-INVASIVE") %>% 
  transmute(name = Sample_ID, Study_ID, Specimen_Type, ST)

nodes <- bind_rows(bc_vertices, nps_vertices) %>% 
  distinct()

# BUILD ONE GRAPH PER PATIENT
patient_graphs <- edges %>% 
  split(.$Study_ID) %>% 
  map(function(edg) {
    
    all_edge_nodes <- unique(c(edg$from, edg$to))
    
    verts <- nodes %>% 
      filter(name %in% all_edge_nodes) %>%
      distinct(name, .keep_all = TRUE)
    
    missing_nodes <- setdiff(all_edge_nodes, verts$name)
    if (length(missing_nodes) > 0) {
      extra_verts <- tibble(
        name = missing_nodes,
        Study_ID = unique(edg$Study_ID),
        Specimen_Type = "UNKNOWN",
        ST = "UNKNOWN"
      )
      verts <- bind_rows(verts, extra_verts)
    }
    
    # CRITICAL CHANGE HERE: 
    # Pass only (from, to, Match). Do not pass Study_ID as an attribute column.
    graph_edges <- edg %>% select(from, to, Match)
    
    graph_from_data_frame(
      d = graph_edges, 
      vertices = verts, 
      directed = FALSE
    )
  })

# EXTRACT LAYOUTS


plot_df <- map2_df(
  patient_graphs,
  names(patient_graphs),
  
  function(g, patient_id) {
    
    lay <- create_layout(g, layout = "stress")
    
    lay$Patient <- patient_id
    
    lay
  }
)

# EXTRACT EDGE DATA
edge_df <- map2_df(
  patient_graphs, names(patient_graphs), function(g, patient_id) {
    lay <- create_layout(g, layout = "stress")
    edg <- as_data_frame(g, what = "edges")
    coords <- lay %>% select(name, x, y)
    
    # Clean join using custom suffixes instead of manual renaming
    edg <- edg %>% 
      left_join(coords, by = c("from" = "name")) %>% 
      left_join(coords, by = c("to" = "name"), suffix = c("", "end"))
    
    edg$Patient <- patient_id
    edg
  }
)

# ORDER PATIENTS


patient_order <- sort(unique(plot_df$Patient))

plot_df$Patient <- factor(plot_df$Patient,
                          levels = patient_order)

edge_df$Patient <- factor(edge_df$Patient,
                          levels = patient_order)

# SUMMARY STATISTICS


n_patients <- n_distinct(DATA_03$Study_ID)

n_bc <- DATA_03 %>%
  filter(Specimen_Type == "INVASIVE") %>%
  nrow()

n_nps <- DATA_03 %>%
  filter(Specimen_Type == "NON-INVASIVE") %>%
  nrow()

match_summary <- network_df %>%
  count(Match)

n_match <- match_summary %>%
  filter(Match == "Match") %>%
  pull(n)

n_nonmatch <- match_summary %>%
  filter(Match == "Non-match") %>%
  pull(n)

patients_with_match <- network_df %>%
  group_by(Study_ID) %>%
  summarise(any_match = any(Match == "Match")) %>%
  filter(any_match) %>%
  nrow()

# PLOT

p <- ggplot() +
  
  # EDGES
  geom_segment(
    data = edge_df,
    aes(
      x = x,
      y = y,
      xend = xend,
      yend = yend,
      linetype = Match
    ),
    linewidth = 1.0,
    colour = "black"
  ) +
  
  # NODES
  geom_point(
    data = plot_df,
    aes(
      x = x,
      y = y,
      fill = Specimen_Type
    ),
    shape = 21,
    colour = "black",
    size = 10,
    stroke = 0.8
  ) +
  
  # LABELS
  geom_text(
    data = plot_df,
    aes(
      x = x,
      y = y,
      label = ST
    ),
    size = 3
  ) +
  
  # FACETS
  facet_wrap(
    ~ Patient,
    ncol = 7
  ) +
  
  # COLORS
  scale_fill_manual(
    values = c(
      "INVASIVE" = "#F8766D",
      "NON-INVASIVE" = "#00BFC4"
    )
  ) +
  
  scale_linetype_manual(
    values = c(
      "Match" = "solid",
      "Non-match" = "22"
    )
  ) +
  
  # TITLES
  labs(
    title = "Fig 1: Patient-level BC/NPS isolate matching by MLST",
    fill = "Specimen_Type",
    linetype = "Relationship"
  ) +
  
  # THEME
  theme_void(base_size = 12) +
  
  theme(
    plot.title = element_text(
      size = 22,
      face = "bold",
      hjust = 0.5
    ),
    
    strip.text = element_text(
      size = 11,
      face = "bold"
    ),
    
    legend.position = "right",
    
    panel.spacing = unit(1.2, "lines")
  )

# SAVE FIGURE
ggsave(
  "Fig 1.png",
  p,
  width = 18,
  height = 14,
  dpi = 800,
  bg = "white"
)



# FIGURE 2:Within-patient diversity plot 

library(dplyr)
library(ggplot2)
library(patchwork)

virulence_genes <- c("clfA,clfB","fnbA,fnbB","spa","cna","ebpS","coa,vwb","sak","hysA",
                     "lip","nuc","hla/hly","hlb","hld","hlg,lukS-PV,lukF-PV","tst",
                     "sea-e, seg-sei","eta,etb","cap operon","chp","isdA,isdB,isdC",
                     "sfa,sfb","psmα, psmβ","agrA,agrB,agrC,agrD","sarA","saeR,saeS",
                     "icaA,icaB,icaC,icaD","bap","aur","lipoteichoic acid","peptidoglycan")

amr_genes <- c("blaZ","dfrG","tet(K)","msr(A)","tet(M)","tet(L)","grlA","mecA",
               "aac(6')-aph(2'')","fusB","qacD","erm(C)","cat(pC233)","aph(3')-III",
               "ant(6)-Ia","ant(9)-Ia","erm(A)","gyrA","qacB","qacJ","rpoB","aadD",
               "bleO","mph(C)","vga(A)","dfrB","fusC","mecA1","sal(A)",
               
               "fosD","Isa(A)","vga(A)V","fosB","cat(pC221)","fusA")

# Restrict to NPS data only--My diversity comparision is based upon carriage isolates only
NPS_data <- DATA_03 %>%
  filter(Specimen_Type == "NON-INVASIVE")

# Create virulence and AMR profiles
NPS_data <- NPS_data %>%
  mutate(
    Virulence_Profile = apply(
      select(., all_of(virulence_genes)),
      1,
      paste,
      collapse = "_"))

NPS_data <- NPS_data %>%
  mutate(
    AMR_Profile = apply(
      select(., all_of(amr_genes)),
      1,
      paste,
      collapse = "_"))

# BC-NPS match status
BC_ST <- DATA_03 %>%
  filter(Specimen_Type == "INVASIVE") %>%
  select(Study_ID, BC_ST = ST)

# Joining BC back to NPS
NPS_data <- NPS_data %>%
  left_join(BC_ST, by = "Study_ID")


match_status <- NPS_data %>%
  group_by(Study_ID) %>%
  summarise(
    BC_group = ifelse(any(ST == BC_ST), "Matched", "Unmatched"),
    .groups = "drop")

# Patient-level diversity table
patient_summary <- NPS_data %>%
  group_by(Study_ID) %>%
  summarise(
    n_ST = n_distinct(ST),
    n_virulence = n_distinct(Virulence_Profile),
    n_amr = n_distinct(AMR_Profile),
    .groups = "drop"
  ) %>%
  left_join(match_status, by = "Study_ID")

# PANEL A: ST Diversity

pA <- ggplot(patient_summary,
             aes(x = BC_group,
                 y = n_ST,
                 fill = BC_group)) +
  
  geom_boxplot(width = 0.6,
               alpha = 0.7,
               outlier.shape = NA) +
  
  geom_jitter(width = 0.12,
              size = 2,
              alpha = 0.7) +
  
  labs(
    title = "A. MLST diversity within patients",
    x = NULL,
    y = "Unique STs per patient"
  ) +
  
  theme_classic() +
  theme(legend.position = "none")

# PANEL B: Single VS Multiple STs

panelB <- patient_summary %>%
  mutate(
    ST_category = ifelse(n_ST == 1,
                         "Single ST",
                         "Multiple STs")
  ) %>%
  count(BC_group, ST_category) %>%
  group_by(BC_group) %>%
  mutate(prop = n/sum(n))

pB <- ggplot(panelB,
             aes(x = BC_group,
                 y = prop,
                 fill = ST_category)) +
  
  geom_col(width = 0.7) +
  
  labs(
    title = "B. ST diversity structure",
    x = NULL,
    y = "Proportion of patients"
  ) +
  
  theme_classic()

# PANEL C: Virulence Diversity 

pC <- ggplot(patient_summary,
             aes(x = BC_group,
                 y = n_virulence,
                 fill = BC_group)) +
  
  geom_boxplot(alpha = 0.7,
               outlier.shape = NA) +
  
  geom_jitter(width = 0.12,
              alpha = 0.6) +
  
  labs(
    title = "C. Virulence gene diversity",
    x = NULL,
    y = "Unique virulence profiles"
  ) +
  
  theme_classic() +
  theme(legend.position = "none")

# PANEL D: AMR Diversity
pD <- ggplot(patient_summary,
             aes(x = BC_group,
                 y = n_amr,
                 fill = BC_group)) +
  
  geom_boxplot(alpha = 0.7,
               outlier.shape = NA) +
  
  geom_jitter(width = 0.12,
              alpha = 0.6) +
  
  labs(
    title = "D. AMR gene diversity",
    x = NULL,
    y = "Unique AMR profiles"
  ) +
  
  theme_classic() +
  theme(legend.position = "none")

# Combined Figure
library(patchwork) # A composer of plots

final_plot <- (pA | pB) /
  (pC | pD)

ggsave("Fig 2.png", width = 8, height = 6, dpi = 300)


# Alt Figure 2 (All Patients)
library(ggplot2)
library(dplyr)
library(patchwork)

# 1. Define your 30 virulence gene column names exactly as they appear in R

vir_gene_list <-c(
  "clfA,clfB", "fnbA,fnbB", "spa", "cna", "ebpS", "coa,vwb", "sak", "hysA",
  "lip", "nuc", "hla/hly", "hlb", "hld", "hlg,lukS-PV,lukF-PV", "tst",
  "sea-e, seg-sei", "eta,etb", "cap operon", "chp", "isdA,isdB,isdC",
  "sfa,sfb", "psmα, psmβ", "agrA,agrB,agrC,agrD", "sarA", "saeR,saeS",
  "icaA,icaB,icaC,icaD", "bap", "aur", "lipoteichoic acid", "peptidoglycan"
)

# --- PANEL A: TOTAL VIRULENCE LOAD (30 GENES) ---

# Calculate the total number of virulence genes switched "on" (Yes) per isolate
DATA_03_VIRULENCE <- DATA_03_ANALYSIS%>%
  mutate(Virulence_Load = rowSums(.[vir_gene_list] == "Yes", na.rm = TRUE))

# Run a quick Wilcoxon test to get a p-value for publication
stat_res <- wildox_test <- wildox_res <- wilcox.test(Virulence_Load ~ Specimen_Type, data = DATA_03_VIRULENCE)
p_val_text <- paste0("p = ", format.pval(stat_res$p.value, digits = 3))

# Generate Panel A Box Plot
# 1. Find your maximum value to dynamically scale the margin
max_genes <- max(DATA_03_VIRULENCE$Virulence_Load, na.rm = TRUE)

panel_A <- ggplot(DATA_03_VIRULENCE, aes(x = Specimen, y = Virulence_Load, fill = Specimen)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7, width = 0.4, color = "black") +
  geom_jitter(width = 0.15, alpha = 0.4, size = 2, color = "grey30") +
  # Match the primary theme colors of Panel B
  scale_fill_manual(values = c("Blood Culture" = "#D32F2F", "NPS" = "#1976D2")) +
  scale_y_continuous(limits = c(min(DATA_03_VIRULENCE$Virulence_Load) - 1, max_genes + 3)) +
  # Place the text safely inside that new expanded headspace
  annotate("text", x = 1.5, y = max_genes + 1.5, 
           label = "p = 0.0047", fontface = "bold.italic", size = 4.5) +
  labs(
    title = "PANEL A: Total Virulence Repertoire Size",
    subtitle = "Comparison of total detected virulence genes out of 30 surveyed",
    x = "Specimen Type",
    y = "Number of Virulence Genes Present"
  ) +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(), legend.position = "none")

# --- PANEL B: SPECIFIC TARGET SIGNATURES ---

# Classify the specific target signatures from your hypothesis
signature_data <- DATA_03_ANALYSIS %>%
  mutate(Signature = case_when(
    `cap operon` == "Yes" & cna == "Yes" & hld == "Yes" & sak == "Yes" ~ "cap+ / cna+ / hld+ / sak+ (Invasive Signature)",
    chp == "Yes" & hysA == "Yes"           ~ "chp+ / hysA+ (Carriage Signature)",
    TRUE                           ~ "Other 30-Gene Profiles"
  )) %>%
  group_by(Specimen, Signature) %>%
  tally() %>%
  mutate(Percentage = (n / sum(n)) * 100)

# Generate Panel B Bar Plot
panel_B <- ggplot(signature_data, aes(x = Specimen, y = Percentage, fill = Signature)) +
  geom_col(position = "dodge", color = "black", linewidth = 0.3, width = 0.6) +
  scale_fill_manual(values = c(
    "cap+ / cna+ / hld+ / sak+ (Invasive Signature)" = "#D32F2F", 
    "chp+ / hysA+ (Carriage Signature)" = "#1976D2", 
    "Other 30-Gene Profiles" = "#E0E0E0"
  )) +
  labs(
    title = "PANEL B: Distribution of Diagnostic Marker Signatures",
    subtitle = "Prevalence of targeted virulence profiles across types",
    x = "Specimen Type",
    y = "Percentage of Isolates (%)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 2))


# --- STITCH PANELS TOGETHER AND SAVE ---
combined_figure <- panel_A | panel_B

ggsave("Virulence_30Gene_Analysis.png",width = 12, height = 6, dpi = 300)



# Alt Fig 2 (All Patients) 


library(dplyr)
library(ggplot2)
library(patchwork)

# 1. Calculate the mean virulence load and dominant signature proportions PER patient
patient_summary <- DATA_03_VIRULENCE%>%
  group_by(Study_ID, Specimen) %>%
  summarise(
    # Average number of virulence genes active for this sample type in this patient
    Mean_Virulence_Load = mean(Virulence_Load, na.rm = TRUE),
    # Track presence of signatures as a proportion (0 to 1) within those 8 colonies
    Invasive_Sig_Prop = mean(`cap operon` == "Yes" & cna == "Yes" & hld == "Yes" & sak == "Yes", na.rm = TRUE),
    Carriage_Sig_Prop = mean(chp == "Yes" & hysA == "Yes", na.rm = TRUE),
    .groups = "drop"
  )

# 2. Keep ONLY patients who have BOTH a BC and at least one NPS isolate
paired_patients <- patient_summary %>%
  group_by(Study_ID) %>%
  filter(all(c("Blood Culture", "NPS") %in% Specimen)) %>%
  pull(Study_ID) %>%
  unique()

plot_data_consolidated <- patient_summary %>% 
  filter(Study_ID %in% paired_patients)

# 3. Correct Paired Statistical Test on Consolidated Averages
bc_vector <- plot_data_consolidated %>% filter(Specimen == "Blood Culture") %>% arrange(Study_ID) %>% pull(Mean_Virulence_Load)
nps_vector <- plot_data_consolidated %>% filter(Specimen == "NPS") %>% arrange(Study_ID) %>% pull(Mean_Virulence_Load)

paired_stat_nested <- wilcox.test(bc_vector, nps_vector, paired = TRUE)
p_val_nested_text <- paste0("Paired p = ", format.pval(paired_stat_nested$p.value, digits = 4))


# --- PANEL A: CONSOLIDATED REPERTOIRE SHIFTS ---
max_genes_c <- max(plot_data_consolidated$Mean_Virulence_Load, na.rm = TRUE)

panel_A_nested <- ggplot(plot_data_consolidated, aes(x = Specimen, y = Mean_Virulence_Load)) +
  geom_boxplot(aes(fill = Specimen), outlier.shape = NA, alpha = 0.5, width = 0.4, color = "black") +
  # Lines connect the single BC to the MEAN of the 8 NPS colonies for each patient
  geom_line(aes(group = Study_ID), color = "grey40", alpha = 0.6, linewidth = 0.6) +
  geom_point(aes(color = Specimen), size = 3, alpha = 0.8) +
  scale_fill_manual(values = c("Blood Culture" = "#D32F2F", "NPS" = "#1976D2")) +
  scale_color_manual(values = c("Blood Culture" = "#D32F2F", "NPS" = "#1976D2")) +
  
  scale_y_continuous(limits = c(0, 33)) +
  annotate("text", x = 1.5, y = max_genes_c + 2, 
           label = p_val_nested_text, fontface = "bold.italic", size = 4.5) +
  labs(
    x = "Specimen Type",
    y = "Mean Virulence Genes Present"
  ) +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(), legend.position = "none")


# --- PANEL B: COLONY POPULATION SIGNATURE PREVALENCE ---
# Melt proportions back into long format for a clean bar plot comparison
signature_melted <- plot_data_consolidated %>%
  group_by(Specimen) %>%
  summarise(
    `cap+/cna+/hld+/sak (Invasive)` = mean(Invasive_Sig_Prop) * 100,
    `chp+/hysA+ (Carriage)` = mean(Carriage_Sig_Prop) * 100,
    .groups = "drop"
  ) %>%
  tidyr::pivot_longer(cols = -Specimen, names_to = "Signature", values_to = "Percentage")

panel_B_nested <- ggplot(signature_melted, aes(x = Specimen, y = Percentage, fill = Signature)) +
  geom_col(position = "dodge", color = "black", linewidth = 0.3, width = 0.6) +
  scale_fill_manual(values = c(
    "cap+/cna+/hld+/sak (Invasive)" = "#D32F2F", 
    "chp+/hysA+ (Carriage)" = "#1976D2"
  )) +
  labs(
    x = "Specimen Type",
    y = "Mean Prevalence within Patient (%)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 2))

# --- STITCH AND EXPORT ---
final_nested_plot <- panel_A_nested | panel_B_nested
ggsave("Figure5_SideBySide.png", width = 12, height = 6, dpi = 300)


# Alt Fig 2 (Matched Patients) PATIENT POPULATION APPROACH -- a single mean value for the gene count and a single collective percentage for the signatures (This allows me to smoothly trace a single line from a patient's unique blood clone to their average nose colonizing population)     

library(dplyr)
library(ggplot2)
library(patchwork)


# STEP 1: STRICT CLONE-MATCHING FILTER (The Missing Step)

# 1. Pull out the exact ST that was found in each patient's Blood Culture
bc_st_lookup <- DATA_03_VIRULENCE %>%
  filter(Specimen == "Blood Culture") %>%
  select(Study_ID, BC_ST = ST) %>%
  distinct()

# 2. Filter your raw data to keep ONLY the records that match that blood ST
clone_matched_raw <- DATA_03_VIRULENCE %>%
  inner_join(bc_st_lookup, by = "Study_ID") %>%
  # Keep rows where the isolate's ST is identical to that patient's blood culture ST
  filter(ST == BC_ST)

# 3. Identify which patients STILL have BOTH a Blood Culture and at least one NPS 
# after dropping the mismatched strains (This will isolate your exact 9 patients)
clone_paired_patients <- clone_matched_raw %>%
  group_by(Study_ID) %>%
  filter(all(c("Blood Culture", "NPS") %in% Specimen)) %>%
  pull(Study_ID) %>%
  unique()

# 4. Create your final, clean, strictly filtered dataframe
final_9_patients_data <- clone_matched_raw %>%
  filter(Study_ID %in% clone_paired_patients)

# Check: This print statement should now show exactly 9 unique patients!
print(paste("Number of clone-matched patients isolated:", length(unique(final_9_patients_data$Study_ID))))


# STEP 2: SUBSEQUENT NESTED ANALYSIS ON THE 9 PATIENTS ONLY

# Calculate summaries using only our strictly filtered 9-patient data
patient_summary <- final_9_patients_data %>%
  group_by(Study_ID, Specimen) %>%
  summarise(
    Mean_Virulence_Load = mean(Virulence_Load, na.rm = TRUE),
    Invasive_Sig_Prop = mean(`cap operon` == "Yes" & cna == "Yes" & hld == "Yes", na.rm = TRUE),
    Carriage_Sig_Prop = mean(chp == "Yes", na.rm = TRUE),
    .groups = "drop"
  )

# Extract vectors for the paired test (guaranteed to be matched and sorted)
bc_vector <- patient_summary %>% filter(Specimen == "Blood Culture") %>% arrange(Study_ID) %>% pull(Mean_Virulence_Load)
nps_vector <- patient_summary %>% filter(Specimen == "NPS") %>% arrange(Study_ID) %>% pull(Mean_Virulence_Load)

paired_stat_nested <- wilcox.test(bc_vector, nps_vector, paired = TRUE)
p_val_nested_text <- paste0("Paired p = ", format.pval(paired_stat_nested$p.value, digits = 5))


# --- PANEL A: CONSOLIDATED REPERTOIRE SHIFTS ---
max_genes_c <- max(patient_summary$Mean_Virulence_Load, na.rm = TRUE)

panel_A_nested <- ggplot(patient_summary, aes(x = Specimen, y = Mean_Virulence_Load)) +
  geom_violin(aes(fill = Specimen), alpha = 0.3, width = 0.5, color = NA) +
  geom_line(aes(group = Study_ID), color = "grey50", alpha = 0.7, linewidth = 0.6) +
  geom_point(aes(color = Specimen), size = 3.5, alpha = 0.9) +
  scale_fill_manual(values = c("Blood Culture" = "#D32F2F", "NPS" = "#1976D2")) +
  scale_color_manual(values = c("Blood Culture" = "#D32F2F", "NPS" = "#1976D2")) +
  
  scale_y_continuous(limits = c(10, 32)) +
  annotate("text", x = 1.5, y = max_genes_c + 2, 
           label = p_val_nested_text, fontface = "bold.italic", size = 4.5) +
  labs(
    x = "Specimen Type",
    y = "Mean Virulence Genes Present"
  ) +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(), legend.position = "none")


# --- PANEL B: COLONY POPULATION SIGNATURE PREVALENCE ---
signature_melted <- patient_summary %>%
  group_by(Specimen) %>%
  summarise(
    `cap+/cna+/hld+ ` = mean(Invasive_Sig_Prop) * 100,
    `chp+ ` = mean(Carriage_Sig_Prop) * 100,
    .groups = "drop"
  ) %>%
  tidyr::pivot_longer(cols = -Specimen, names_to = "Signature", values_to = "Percentage")

panel_B_nested <- ggplot(signature_melted, aes(x = Specimen, y = Percentage, fill = Signature)) +
  geom_col(position = "dodge", color = "black", linewidth = 0.3, width = 0.6) +
  scale_fill_manual(values = c(
    "cap+/cna+/hld+ " = "#D32F2F", 
    "chp+ " = "#1976D2"
  )) +
  labs(
    x = "Specimen Type",
    y = "Mean Prevalence within Patient (%)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 2))

# Display plot
(panel_A_nested | panel_B_nested)
ggsave("Figure5_SideBySide4.png", width = 12, height = 6, dpi = 300)


# Alt Fig 2 (Matched Patients FOR AMR ) PATIENT POPULATION APPROACH -- a single mean value for the gene count and a single collective percentage for the signatures (This allows me to smoothly trace a single line from a patient's unique blood clone to their average nose colonizing population)     

library(dplyr)
library(ggplot2)
library(patchwork)


# Calculate the total number of virulence genes switched "on" (Yes) per isolate
DATA_03_AMR <- DATA_03_ANALYSIS %>%
  mutate(AMR_Load = rowSums(.[amr_genes] == "Yes", na.rm = TRUE))
# STEP 1: STRICT CLONE-MATCHING FILTER (The Missing Step)

# 1. Pull out the exact ST that was found in each patient's Blood Culture
bc_st_lookup <- DATA_03_AMR %>%
  filter(Specimen == "Blood Culture") %>%
  select(Study_ID, BC_ST = ST) %>%
  distinct()

# 2. Filter your raw data to keep ONLY the records that match that blood ST
clone_matched_raw <- DATA_03_AMR %>%
  inner_join(bc_st_lookup, by = "Study_ID") %>%
  # Keep rows where the isolate's ST is identical to that patient's blood culture ST
  filter(ST == BC_ST)

# 3. Identify which patients STILL have BOTH a Blood Culture and at least one NPS 
# after dropping the mismatched strains (This will isolate your exact 9 patients)
clone_paired_patients <- clone_matched_raw %>%
  group_by(Study_ID) %>%
  filter(all(c("Blood Culture", "NPS") %in% Specimen)) %>%
  pull(Study_ID) %>%
  unique()

# 4. Create your final, clean, strictly filtered dataframe
final_9_patients_data <- clone_matched_raw %>%
  filter(Study_ID %in% clone_paired_patients)

# Check: This print statement should now show exactly 9 unique patients!
print(paste("Number of clone-matched patients isolated:", length(unique(final_9_patients_data$Study_ID))))


# STEP 2: SUBSEQUENT NESTED ANALYSIS ON THE 9 PATIENTS ONLY

# Calculate summaries using only our strictly filtered 9-patient data
patient_summary <- final_9_patients_data %>%
  group_by(Study_ID, Specimen) %>%
  summarise(
    Mean_AMR_Load = mean(AMR_Load, na.rm = TRUE),
    .groups = "drop"
  )

# Extract vectors for the paired test (guaranteed to be matched and sorted)
bc_vector <- patient_summary %>% filter(Specimen == "Blood Culture") %>% arrange(Study_ID) %>% pull(Mean_AMR_Load)
nps_vector <- patient_summary %>% filter(Specimen == "NPS") %>% arrange(Study_ID) %>% pull(Mean_AMR_Load)

paired_stat_nested <- wilcox.test(bc_vector, nps_vector, paired = TRUE)
p_val_nested_text <- paste0("Paired p = ", format.pval(paired_stat_nested$p.value, digits = 5))


# --- PANEL A: CONSOLIDATED REPERTOIRE SHIFTS ---
max_genes_c <- max(patient_summary$Mean, na.rm = TRUE)

panel_A_nested <- ggplot(patient_summary, aes(x = Specimen, y = Mean_AMR_Load)) +
  geom_violin(aes(fill = Specimen), alpha = 0.3, width = 0.5, color = NA) +
  geom_line(aes(group = Study_ID), color = "grey50", alpha = 0.7, linewidth = 0.6) +
  geom_point(aes(color = Specimen), size = 3.5, alpha = 0.9) +
  scale_fill_manual(values = c("Blood Culture" = "#D32F2F", "NPS" = "#1976D2")) +
  scale_color_manual(values = c("Blood Culture" = "#D32F2F", "NPS" = "#1976D2")) +
  
  scale_y_continuous(limits = c(10, 32)) +
  annotate("text", x = 1.5, y = max_genes_c + 2, 
           label = p_val_nested_text, fontface = "bold.italic", size = 4.5) +
  labs(
    x = "Specimen Type",
    y = "Mean AMR Genes Present"
  ) +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(), legend.position = "none")




# Figure 4: Virulence gene heatmap (All Patients)


library(dplyr)
library(tidyr)
library(ComplexHeatmap)
library(circlize)

virulence_genes <- c("clfA,clfB","fnbA,fnbB","spa","cna","ebpS","coa,vwb","sak","hysA",
                     "lip","nuc","hla/hly","hlb","hld","hlg,lukS-PV,lukF-PV","tst",
                     "sea-e, seg-sei","eta,etb","cap operon","chp","isdA,isdB,isdC",
                     "sfa,sfb","psmα, psmβ","agrA,agrB,agrC,agrD","sarA","saeR,saeS",
                     "icaA,icaB,icaC,icaD","bap","aur","lipoteichoic acid","peptidoglycan")


# 1. DATA PREPARATION 


# Clean and format your original data
heatmap_df <- DATA_03 %>%
  select(Study_ID, Sample_ID, Specimen_Type, all_of(virulence_genes)) %>%
  pivot_longer(
    cols = all_of(virulence_genes),
    names_to = "Gene",
    values_to = "Presence"
  ) %>%
  mutate(
    Presence = ifelse(Presence %in% c("Yes", 1, "Y", TRUE), 1, 0),
    # Maintain your precise isolate grouping hierarchy
    Isolate_Label = paste(Specimen_Type, Sample_ID, sep = "_"),
    Isolate_Order = case_when(
      Specimen_Type == "INVASIVE" ~ 1,
      TRUE ~ 2
    )
  ) %>%
  arrange(Study_ID, Isolate_Order, Sample_ID)


# MAP GENES TO THEIR FUNCTIONAL CLASSES


# Ordered strictly by group category for visualization
ordered_genes <- c(
  # Adhesion
  "clfA,clfB", "fnbA,fnbB", "cna", "ebpS", "sfa,sfb", "lipoteichoic acid", "peptidoglycan",
  # Biofilm
  "icaA,icaB,icaC,icaD", "bap",
  # Iron acquisition
  "isdA,isdB,isdC",
  # Immune evasion
  "cap operon", "spa", "chp",
  # Toxins
  "hla/hly", "hlb", "hld", "hlg,lukS-PV,lukF-PV", "tst", "sea-e, seg-sei", "eta,etb", "psmα, psmβ",
  # Quorum sensing
  "agrA,agrB,agrC,agrD", "sarA", "saeR,saeS",
  # Exoenzymes / Overlapping Shared
  "aur", "coa,vwb", "lip", "nuc", "sak", "hysA"
)

# Parallel categories vector matching the ordered lengths above
gene_categories <- c(
  rep("Adhesion", 7),
  rep("Biofilm", 2),
  rep("Iron Acq.", 1),
  rep("Immune Evasion", 3),
  rep("Toxins", 8),
  rep("Quorum Sensing", 3),
  rep("Exoenzymes", 6)
)

# Convert the categorical vector into a factor to keep its order
gene_categories <- factor(gene_categories, levels = unique(gene_categories))

# 3. BUILD THE WIDE HEATMAP MATRIX & METADATA

# Reshape back to wide format using your exact established row arrangement
wide_heatmap_data <- heatmap_df %>%
  select(Study_ID, Isolate_Label, Isolate_Order, Gene, Presence) %>%
  pivot_wider(names_from = Gene, values_from = Presence) %>%
  # Ensure columns are arranged identically to our functional group ordering
  select(Study_ID, Isolate_Label, Isolate_Order, all_of(ordered_genes))

# Isolate the pure values numeric matrix
mat <- as.matrix(wide_heatmap_data[, ordered_genes])
rownames(mat) <- wide_heatmap_data$Isolate_Label

# Ensure row split vectors maintain historical ordering rules
patient_split <- factor(wide_heatmap_data$Study_ID, levels = unique(wide_heatmap_data$Study_ID))


# 4. DEFINE ANNOTATIONS AND COLOR PALETTES

# 1. DEFINE COLORS & STRUCTURE

# Colors for your 7 functional classes
class_colors <- c(
  "Adhesion"          = "#E41A1C", 
  "Biofilm"           = "#377EB8", 
  "Iron Acq."         = "#4DAF4A",
  "Immune Evasion"    = "#984EA3", 
  "Toxins"            = "#FF7F00", 
  "Quorum Sensing"    = "#FFFF33", 
  "Exoenzymes"        = "#A65628"
)

# Clean, thin top color bars (No text inside them to prevent overlapping)
col_anno <- HeatmapAnnotation(
  Class = gene_categories,
  col = list(Class = class_colors),
  show_annotation_name = FALSE,
  show_legend = FALSE # We will build a customized, beautiful legend manually instead
)

# Left-side annotation tracking Specimen Type
row_anno <- rowAnnotation(
  Type = gsub("_.*", "", rownames(mat)), 
  col = list(Type = c("INVASIVE" = "#2C3E50", "NON-INVASIVE" = "#BDC3C7")), 
  show_annotation_name = FALSE,
  annotation_legend_param = list(title = "Isolate Source")
)


# 2. CREATE THE SEPARATE FUNCTIONAL CLASS LEGEND

functional_legend <- Legend(
  labels = names(class_colors),
  title = "Functional Class",
  legend_gp = gpar(fill = class_colors),
  title_gp = gpar(fontface = "bold", fontsize = 10),
  labels_gp = gpar(fontsize = 9)
)

# 3. BUILD THE HEATMAP OBJECT

final_heatmap <- ComplexHeatmap::Heatmap(
  mat,
  name = "Gene detected",
  col = c("0" = "#F2F2F2", "1" = "steelblue"), 
  
  # Column Configuration (Genes)
  cluster_columns = FALSE,          
  column_split = gene_categories,   
  column_title = NULL,                   
  column_names_gp = gpar(fontface = "italic", fontsize = 10),
  column_names_rot = 45,                 
  top_annotation = col_anno,             
  
  # Row Configuration (Patients/Isolates)
  cluster_rows = FALSE,             
  row_split = patient_split,        
  row_title_rot = 0,                
  row_title_gp = gpar(fontsize = 10, fontface = "bold"),
  show_row_names = FALSE,                
  left_annotation = row_anno,
  
  # Structural Aesthetics
  rect_gp = gpar(col = "white", lwd = 0.5), 
  border = TRUE,
  
  # Explicitly map the labels to the 0 and 1 values
  heatmap_legend_param = list(
    at = c(0, 1),                 # Maps text specifically to 0 and 1 numbers
    labels = c("Absent", "Present"),
    title_gp = gpar(fontface = "bold", fontsize = 10)
  )
)

# 4. EXPORT TO A PERFECT HIGH-RES PNG

graphics.off() 

png("Fig 4.png", 
    width = 3400, # Added extra horizontal width to comfortably fit the new legend box
    height = 4200, 
    res = 300)

# Draw everything together and explicitly attach our new custom legend pack on the right side
ComplexHeatmap::draw(
  final_heatmap, 
  column_title = "Fig 4: Within-patient virulence gene diversity (BC + NPS isolates)",
  column_title_gp = gpar(fontface = "bold", fontsize = 16),
  annotation_legend_list = list(functional_legend) # Places our custom text box nicely next to the plot
)

dev.off()



# Figure 4: Virulence heatmap (Unmatched Patients only)

library(dplyr)
library(tidyr)
library(ComplexHeatmap)
library(circlize)

virulence_genes <- c("clfA,clfB","fnbA,fnbB","spa","cna","ebpS","coa,vwb","sak","hysA",
                     "lip","nuc","hla/hly","hlb","hld","hlg,lukS-PV,lukF-PV","tst",
                     "sea-e, seg-sei","eta,etb","cap operon","chp","isdA,isdB,isdC",
                     "sfa,sfb","psmα, psmβ","agrA,agrB,agrC,agrD","sarA","saeR,saeS",
                     "icaA,icaB,icaC,icaD","bap","aur","lipoteichoic acid","peptidoglycan")


# NEW STEP: IDENTIFY UNMATCHED PATIENTS AUTOMATICALLY

unmatched_patients <- DATA_03 %>%
  filter(Specimen_Type %in% c("INVASIVE", "NON-INVASIVE")) %>%
  select(Study_ID, Specimen_Type, ST) %>%
  distinct() %>%
  pivot_wider(names_from = Specimen_Type, values_from = ST, values_fn = list) %>%
  rowwise() %>%
  # Added backticks around column names to explicitly handle the hyphen
  filter(!any(`INVASIVE` %in% `NON-INVASIVE`)) %>%
  pull(Study_ID)


# 1. DATA PREPARATION (WITH FILTER APPLIED)

heatmap_df <- DATA_03 %>%
  # Filter to retain ONLY the 26 unmatched patients
  filter(Study_ID %in% unmatched_patients) %>%
  select(Study_ID, Sample_ID, Specimen_Type, all_of(virulence_genes)) %>%
  pivot_longer(
    cols = all_of(virulence_genes),
    names_to = "Gene",
    values_to = "Presence"
  ) %>%
  mutate(
    Presence = ifelse(Presence %in% c("Yes", 1, "Y", TRUE), 1, 0),
    Isolate_Label = paste(Specimen_Type, Sample_ID, sep = "_"),
    Isolate_Order = case_when(
      Specimen_Type == "INVASIVE" ~ 1,
      TRUE ~ 2
    )
  ) %>%
  arrange(Study_ID, Isolate_Order, Sample_ID)


# MAP GENES TO THEIR FUNCTIONAL CLASSES

ordered_genes <- c(
  # Adhesion
  "clfA,clfB", "fnbA,fnbB", "cna", "ebpS", "sfa,sfb", "lipoteichoic acid", "peptidoglycan",
  # Biofilm
  "icaA,icaB,icaC,icaD", "bap",
  # Iron acquisition
  "isdA,isdB,isdC",
  # Immune evasion
  "cap operon", "spa", "chp",
  # Toxins
  "hla/hly", "hlb", "hld", "hlg,lukS-PV,lukF-PV", "tst", "sea-e, seg-sei", "eta,etb", "psmα, psmβ",
  # Quorum sensing
  "agrA,agrB,agrC,agrD", "sarA", "saeR,saeS",
  # Exoenzymes / Overlapping Shared
  "aur", "coa,vwb", "lip", "nuc", "sak", "hysA"
)

gene_categories <- c(
  rep("Adhesion", 7),
  rep("Biofilm", 2),
  rep("Iron Acq.", 1),
  rep("Immune Evasion", 3),
  rep("Toxins", 8),
  rep("Quorum Sensing", 3),
  rep("Exoenzymes", 6)
)

gene_categories <- factor(gene_categories, levels = unique(gene_categories))

# 3. BUILD THE WIDE HEATMAP MATRIX & METADATA

wide_heatmap_data <- heatmap_df %>%
  select(Study_ID, Isolate_Label, Isolate_Order, Gene, Presence) %>%
  pivot_wider(names_from = Gene, values_from = Presence) %>%
  select(Study_ID, Isolate_Label, Isolate_Order, all_of(ordered_genes))

mat <- as.matrix(wide_heatmap_data[, ordered_genes])
rownames(mat) <- wide_heatmap_data$Isolate_Label

# Dropping unused levels ensures empty patient slots do not break row splitting
patient_split <- factor(wide_heatmap_data$Study_ID, levels = unique(wide_heatmap_data$Study_ID))


# 4. DEFINE ANNOTATIONS AND COLOR PALETTES

class_colors <- c(
  "Adhesion"          = "#E41A1C", 
  "Biofilm"           = "#377EB8", 
  "Iron Acq."         = "#4DAF4A",
  "Immune Evasion"    = "#984EA3", 
  "Toxins"            = "#FF7F00", 
  "Quorum Sensing"    = "#FFFF33", 
  "Exoenzymes"        = "#A65628"
)

col_anno <- HeatmapAnnotation(
  Class = gene_categories,
  col = list(Class = class_colors),
  show_annotation_name = FALSE,
  show_legend = FALSE 
)

row_anno <- rowAnnotation(
  Type = gsub("_.*", "", rownames(mat)), 
  col = list(Type = c("INVASIVE" = "#2C3E50", "NON-INVASIVE" = "#BDC3C7")), 
  show_annotation_name = FALSE,
  annotation_legend_param = list(title = "Isolate Source")
)

functional_legend <- Legend(
  labels = names(class_colors),
  title = "Functional Class",
  legend_gp = gpar(fill = class_colors),
  title_gp = gpar(fontface = "bold", fontsize = 10),
  labels_gp = gpar(fontsize = 9)
)

# 5. BUILD THE HEATMAP OBJECT

final_heatmap <- ComplexHeatmap::Heatmap(
  mat,
  name = "Gene detected",
  col = c("0" = "#F2F2F2", "1" = "steelblue"), 
  
  cluster_columns = FALSE,          
  column_split = gene_categories,   
  column_title = NULL,                   
  column_names_gp = gpar(fontface = "italic", fontsize = 10),
  column_names_rot = 45,                 
  top_annotation = col_anno,             
  
  cluster_rows = FALSE,             
  row_split = patient_split,        
  row_title_rot = 0,                
  row_title_gp = gpar(fontsize = 10, fontface = "bold"),
  show_row_names = FALSE,                
  left_annotation = row_anno,
  
  rect_gp = gpar(col = "white", lwd = 0.5), 
  border = TRUE,
  
  heatmap_legend_param = list(
    at = c(0, 1),                 
    labels = c("Absent", "Present"),
    title_gp = gpar(fontface = "bold", fontsize = 10)
  )
)

# 6. EXPORT TO A PERFECT HIGH-RES PNG

graphics.off() 

# Dynamically adjust height since we have fewer rows now (26 patients instead of 35)
png("Fig 4_unmatched.png", 
    width = 3400, 
    height = 3200, 
    res = 300)

ComplexHeatmap::draw(
  final_heatmap, 
  column_title = "Fig 4: Virulence Profiles of Unmatched Cohort Pairs",
  column_title_gp = gpar(fontface = "bold", fontsize = 16),
  annotation_legend_list = list(functional_legend) 
)

dev.off()


# Matched Patients

library(dplyr)
library(tidyr)
library(ComplexHeatmap)
library(circlize)

virulence_genes <- c("clfA,clfB","fnbA,fnbB","spa","cna","ebpS","coa,vwb","sak","hysA",
                     "lip","nuc","hla/hly","hlb","hld","hlg,lukS-PV,lukF-PV","tst",
                     "sea-e, seg-sei","eta,etb","cap operon","chp","isdA,isdB,isdC",
                     "sfa,sfb","psmα, psmβ","agrA,agrB,agrC,agrD","sarA","saeR,saeS",
                     "icaA,icaB,icaC,icaD","bap","aur","lipoteichoic acid","peptidoglycan")


# IDENTIFY THE 9 MATCHED PATIENTS & RETAIN BOTH NICHE STs

# This matrix tracks exactly what the invasive sequence type is per patient ID
invasive_st_mapping <- DATA_03 %>%
  filter(Specimen_Type == "INVASIVE") %>%
  select(Study_ID, Invasive_ST = ST) %>%
  distinct()

patient_st_matrix <- DATA_03 %>%
  filter(Specimen_Type %in% c("INVASIVE", "NON-INVASIVE")) %>%
  select(Study_ID, Specimen_Type, ST) %>%
  distinct() %>%
  pivot_wider(names_from = Specimen_Type, values_from = ST, values_fn = list) %>%
  rowwise()

# Isolate exclusively the 9 matching human hosts
matched_patients <- patient_st_matrix %>%
  filter(any(`INVASIVE` %in% `NON-INVASIVE`)) %>%
  pull(Study_ID)


# 1. DATA PREPARATION (WITH FILTER APPLIED)

heatmap_df <- DATA_03 %>%
  # Filter to retain ONLY the 26 unmatched patients
  filter(Study_ID %in% matched_patients) %>%
  left_join(invasive_st_mapping, by = "Study_ID") %>%
  select(Study_ID, Sample_ID, Specimen_Type, ST, Invasive_ST, all_of(virulence_genes)) %>%
  pivot_longer(
    cols = all_of(virulence_genes),
    names_to = "Gene",
    values_to = "Presence"
  ) %>%
  mutate(
    Presence = ifelse(Presence %in% c("Yes", 1, "Y", TRUE), 1, 0),
    Isolate_Label = paste(Specimen_Type, Sample_ID, sep = "_"),
    Isolate_Order = case_when(
      Specimen_Type == "INVASIVE" ~ 1,
      TRUE ~ 2
    ),
    # DYNAMIC CLASSIFICATION: Breaks rows into 3 explicit categories
    Sub_Type = case_when(
      Specimen_Type == "INVASIVE" ~ "Invasive Clones",
      Specimen_Type == "NON-INVASIVE" & ST == Invasive_ST ~ "NPS Clones (Matching ST)",
      TRUE ~ "NPS Clones (Mismatched ST)"
    )
  ) %>%
  arrange(Study_ID, Isolate_Order, Sample_ID)


# MAP GENES TO THEIR FUNCTIONAL CLASSES

ordered_genes <- c(
  # Adhesion
  "clfA,clfB", "fnbA,fnbB", "cna", "ebpS", "sfa,sfb", "lipoteichoic acid", "peptidoglycan",
  # Biofilm
  "icaA,icaB,icaC,icaD", "bap",
  # Iron acquisition
  "isdA,isdB,isdC",
  # Immune evasion
  "cap operon", "spa", "chp",
  # Toxins
  "hla/hly", "hlb", "hld", "hlg,lukS-PV,lukF-PV", "tst", "sea-e, seg-sei", "eta,etb", "psmα, psmβ",
  # Quorum sensing
  "agrA,agrB,agrC,agrD", "sarA", "saeR,saeS",
  # Exoenzymes / Overlapping Shared
  "aur", "coa,vwb", "lip", "nuc", "sak", "hysA"
)

gene_categories <- c(
  rep("Adhesion", 7),
  rep("Biofilm", 2),
  rep("Iron Acq.", 1),
  rep("Immune Evasion", 3),
  rep("Toxins", 8),
  rep("Quorum Sensing", 3),
  rep("Exoenzymes", 6)
)

gene_categories <- factor(gene_categories, levels = unique(gene_categories))

# 3. BUILD THE WIDE HEATMAP MATRIX & METADATA

wide_heatmap_data <- heatmap_df %>%
  select(Study_ID, Isolate_Label, Isolate_Order, Sub_Type, Gene, Presence) %>%
  distinct() %>%
  pivot_wider(names_from = Gene, values_from = Presence) %>%
  select(Study_ID, Isolate_Label, Isolate_Order, Sub_Type, all_of(ordered_genes))

mat <- as.matrix(wide_heatmap_data[, ordered_genes])
rownames(mat) <- wide_heatmap_data$Isolate_Label

# Isolate metadata labels exactly aligned with rownames(mat) order
row_subtypes <- wide_heatmap_data$Sub_Type
patient_split <- factor(wide_heatmap_data$Study_ID, levels = unique(wide_heatmap_data$Study_ID))

# 4. DEFINE ANNOTATIONS AND COLOR PALETTES

class_colors <- c(
  "Adhesion"          = "#E41A1C", 
  "Biofilm"           = "#377EB8", 
  "Iron Acq."         = "#4DAF4A",
  "Immune Evasion"    = "#984EA3", 
  "Toxins"            = "#FF7F00", 
  "Quorum Sensing"    = "#FFFF33", 
  "Exoenzymes"        = "#A65628"
)

# High-contrast palette for your row subtypes
subtype_colors <- c(
  "Invasive Clones"            = "#2C3E50",  # Dark Charcoal Navy
  "NPS Clones (Matching ST)"   = "#27AE60",  # Vivid Green (Marks successful link)
  "NPS Clones (Mismatched ST)" = "#E67E22"   # Ochre Orange (Marks alternative background strains)
)

col_anno <- HeatmapAnnotation(
  Class = gene_categories,
  col = list(Class = class_colors),
  show_annotation_name = FALSE,
  show_legend = FALSE 
)

# LEFT ROW ANNOTATION: Leverages our customized subtype colors vector
row_anno <- rowAnnotation(
  Isolate = row_subtypes, 
  col = list(Isolate = subtype_colors), 
  show_annotation_name = FALSE,
  annotation_legend_param = list(title = "Isolate Lineage Status")
)

functional_legend <- Legend(
  labels = names(class_colors),
  title = "Functional Class",
  legend_gp = gpar(fill = class_colors),
  title_gp = gpar(fontface = "bold", fontsize = 10),
  labels_gp = gpar(fontsize = 9)
)

# 5. BUILD THE HEATMAP OBJECT

final_heatmap <- ComplexHeatmap::Heatmap(
  mat,
  name = "Gene detected",
  col = c("0" = "#F2F2F2", "1" = "steelblue"), 
  
  cluster_columns = FALSE,          
  column_split = gene_categories,   
  column_title = NULL,                   
  column_names_gp = gpar(fontface = "italic", fontsize = 10),
  column_names_rot = 45,                 
  top_annotation = col_anno,             
  
  cluster_rows = FALSE,             
  row_split = patient_split,        
  row_title_rot = 0,                
  row_title_gp = gpar(fontsize = 10, fontface = "bold"),
  show_row_names = FALSE,                
  left_annotation = row_anno,
  
  rect_gp = gpar(col = "white", lwd = 0.5), 
  border = TRUE,
  
  heatmap_legend_param = list(
    at = c(0, 1),                 
    labels = c("Absent", "Present"),
    title_gp = gpar(fontface = "bold", fontsize = 10)
  )
)

# 6. EXPORT TO A PERFECT HIGH-RES PNG

graphics.off() 

# Dynamically adjust height since we have fewer rows
png("Fig 4_matched_virulence.png", 
    width = 4000, 
    height = 2200, 
    res = 300)

ComplexHeatmap::draw(
  final_heatmap, 
  column_title = "Virulence Profiles of matched Patient Pairs",
  column_title_gp = gpar(fontface = "bold", fontsize = 16),
  annotation_legend_list = list(functional_legend) 
)

dev.off()


# AMR Profile for matched patients only

library(dplyr)
library(tidyr)
library(ComplexHeatmap)
library(circlize)

amr_genes <- c("blaZ","dfrG","tet(K)","msr(A)","tet(M)","tet(L)","grlA","mecA",
               "aac(6')-aph(2'')","fusB","qacD","erm(C)","cat(pC233)","aph(3')-III",
               "ant(6)-Ia","ant(9)-Ia","erm(A)","gyrA","qacB","qacJ","rpoB","aadD",
               "bleO","mph(C)","vga(A)","dfrB","fusC","mecA1","sal(A)",
               "fosD","Isa(A)","vga(A)V","fosB","cat(pC221)","fusA")


# STEP 1: IDENTIFY THE 9 MATCHED PATIENTS & RETAIN BOTH NICHE STs

# This matrix tracks exactly what the invasive sequence type is per patient ID
invasive_st_mapping <- DATA_03 %>%
  filter(Specimen_Type == "INVASIVE") %>%
  select(Study_ID, Invasive_ST = ST) %>%
  distinct()

patient_st_matrix <- DATA_03 %>%
  filter(Specimen_Type %in% c("INVASIVE", "NON-INVASIVE")) %>%
  select(Study_ID, Specimen_Type, ST) %>%
  distinct() %>%
  pivot_wider(names_from = Specimen_Type, values_from = ST, values_fn = list) %>%
  rowwise()

# Isolate exclusively the 9 matching human hosts
matched_patients <- patient_st_matrix %>%
  filter(any(`INVASIVE` %in% `NON-INVASIVE`)) %>%
  pull(Study_ID)



# STEP 2: DATA PREPARATION & CONDITIONAL MATCH HIGHLIGHTING

heatmap_df <- DATA_03 %>%
  filter(Study_ID %in% matched_patients) %>%
  left_join(invasive_st_mapping, by = "Study_ID") %>%
  select(Study_ID, Sample_ID, Specimen_Type, ST, Invasive_ST, all_of(amr_genes)) %>%
  pivot_longer(
    cols = all_of(amr_genes),
    names_to = "Gene",
    values_to = "Presence"
  ) %>%
  mutate(
    Presence = ifelse(Presence %in% c("Yes", 1, "Y", TRUE), 1, 0),
    Isolate_Label = paste(Specimen_Type, Sample_ID, sep = "_"),
    Isolate_Order = case_when(
      Specimen_Type == "INVASIVE" ~ 1,
      TRUE ~ 2
    ),
    # DYNAMIC CLASSIFICATION: Breaks rows into 3 explicit categories
    Sub_Type = case_when(
      Specimen_Type == "INVASIVE" ~ "Invasive Clones",
      Specimen_Type == "NON-INVASIVE" & ST == Invasive_ST ~ "NPS Clones (Matching ST)",
      TRUE ~ "NPS Clones (Mismatched ST)"
    )
  ) %>%
  arrange(Study_ID, Isolate_Order, Sample_ID)



# STEP 3: ORDERED ACCESSORY GENE STRUCTURING

ordered_genes <- c(
  "blaZ", "mecA", "mecA1", "dfrG", "dfrB", "tet(K)", "tet(M)", "tet(L)",
  "msr(A)", "erm(C)", "erm(A)", "mph(C)", "vga(A)", "vga(A)V", "Isa(A)",
  "aac(6')-aph(2'')", "aph(3')-III", "ant(6)-Ia", "ant(9)-Ia", "aadD",
  "cat(pC233)", "cat(pC221)", "fusB", "fusC", "fusA", "fosD", "fosB",
  "qacD", "qacB", "qacJ", "bleO", "sal(A)", "grlA", "gyrA", "rpoB"
)

gene_categories <- c(
  rep("Beta-lactam", 3), rep("Trimethoprim", 2), rep("Tetracycline", 3),
  rep("MLS", 7), rep("Aminoglycoside", 5), rep("Chloramphenicol", 2),
  rep("Fusidic acid", 3), rep("Fosfomycin", 2), rep("QAC", 3),
  rep("Bleomycin", 1), rep("Lincosamide", 1), rep("Fluoroquinolone", 2), rep("Rifampicin", 1)
)

gene_categories_factor <- factor(gene_categories, levels = unique(gene_categories))



# STEP 4: GENERATE DATA MATRIX & ROW-METADATA MATRICES

wide_heatmap_data <- heatmap_df %>%
  select(Study_ID, Isolate_Label, Isolate_Order, Sub_Type, Gene, Presence) %>%
  distinct() %>%
  pivot_wider(names_from = Gene, values_from = Presence) %>%
  select(Study_ID, Isolate_Label, Isolate_Order, Sub_Type, all_of(ordered_genes))

mat <- as.matrix(wide_heatmap_data[, ordered_genes])
rownames(mat) <- wide_heatmap_data$Isolate_Label

# Isolate metadata labels exactly aligned with rownames(mat) order
row_subtypes <- wide_heatmap_data$Sub_Type
patient_split <- factor(wide_heatmap_data$Study_ID, levels = unique(wide_heatmap_data$Study_ID))



# STEP 5: COLOR VECTOR ANNOTATION ASSIGNMENTS

class_colors <- c(
  "Beta-lactam"      = "#E41A1C", "Trimethoprim"     = "#377EB8",  
  "Tetracycline"     = "#4DAF4A", "MLS"              = "#984EA3",  
  "Aminoglycoside"   = "#FF7F00", "Chloramphenicol"  = "#A65628",  
  "Fusidic acid"     = "#F781BF", "Fosfomycin"       = "#999999",  
  "QAC"              = "#4A235A", "Bleomycin"        = "#1ABC9C",  
  "Lincosamide"      = "#D4AC0D", "Fluoroquinolone"  = "#2E4053",  
  "Rifampicin"       = "#27AE60"   
)

# High-contrast palette for your row subtypes
subtype_colors <- c(
  "Invasive Clones"            = "#2C3E50",  # Dark Charcoal Navy
  "NPS Clones (Matching ST)"   = "#27AE60",  # Vivid Green (Marks successful link)
  "NPS Clones (Mismatched ST)" = "#E67E22"   # Ochre Orange (Marks alternative background strains)
)

col_anno <- HeatmapAnnotation(
  Class = gene_categories_factor, 
  col = list(Class = class_colors),
  show_annotation_name = FALSE,
  show_legend = FALSE 
)

# LEFT ROW ANNOTATION: Leverages our customized subtype colors vector
row_anno <- rowAnnotation(
  Isolate = row_subtypes, 
  col = list(Isolate = subtype_colors), 
  show_annotation_name = FALSE,
  annotation_legend_param = list(title = "Isolate Lineage Status")
)

functional_legend <- Legend(
  labels = names(class_colors),
  title = "Functional Class",
  legend_gp = gpar(fill = class_colors),
  title_gp = gpar(fontface = "bold", fontsize = 10),
  labels_gp = gpar(fontsize = 9)
)



# STEP 6: BUILD THE MASTER HEATMAP

final_heatmap <- ComplexHeatmap::Heatmap(
  mat,
  name = "Gene detected",
  col = c("0" = "#F2F2F2", "1" = "steelblue"), 
  
  cluster_columns = FALSE,          
  column_split = gene_categories_factor,   
  column_title = NULL,                   
  column_names_gp = gpar(fontface = "italic", fontsize = 10),
  column_names_rot = 45,                 
  top_annotation = col_anno,             
  
  cluster_rows = FALSE,             
  row_split = patient_split,        
  row_title_rot = 0,                
  row_title_gp = gpar(fontsize = 10, fontface = "bold"),
  show_row_names = FALSE,                
  left_annotation = row_anno,        # Embeds our new lineage tracker block
  
  rect_gp = gpar(col = "white", lwd = 0.5), 
  border = TRUE,
  
  heatmap_legend_param = list(
    at = c(0, 1),                 
    labels = c("Absent", "Present"),
    title_gp = gpar(fontface = "bold", fontsize = 10)
  )
)



# STEP 7: EXPORT HIGH-RES INFOGRAPHIC CANVAS

graphics.off() 

# Height calibrated to maximize visual spacing for the 9-patient subset
png("Fig 4_Matched_AMR.png", 
    width = 4000, 
    height = 2200, 
    res = 300)

ComplexHeatmap::draw(
  final_heatmap, 
  column_title = "AMR profiles in Matched Patient Pairs",
  column_title_gp = gpar(fontface = "bold", fontsize = 15),
  annotation_legend_list = list(functional_legend) 
)

dev.off()

# Figure 5:

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# 1. Prepare data with filtering and explicit 0-count completion
cc_data <- DATA_03 %>%
  filter(!is.na(Clonal_Complex), 
         Clonal_Complex != "NA", 
         Clonal_Complex != "") %>%
  
  mutate(Specimen_Type = case_when(
    Specimen_Type == "INVASIVE" ~ "Blood Culture (BC)",
    TRUE ~ "Nasopharyngeal Swab (NPS)"
  )) %>%
  
  count(Specimen_Type, Clonal_Complex) %>%
  
  # CRITICAL STEP: Tells R that every Clonal Complex MUST have both a BC and NPS row
  complete(Specimen_Type, Clonal_Complex, fill = list(n = 0)) %>%
  
  group_by(Specimen_Type) %>%
  mutate(Percentage = n / sum(n)) %>%
  ungroup() %>%
  
  # Hide "n=0" labels so they don't clutter the bottom baseline of the plot
  mutate(label_text = ifelse(n == 0, "", paste0("n=", n)))

# 2. Generate the perfectly aligned bar chart
cc_plot <- ggplot(cc_data, aes(x = Clonal_Complex, y = Percentage, fill = Specimen_Type)) +
  
  # Side-by-side columns ( linetype = 0 removes borders completely for maximum contrast )
  geom_col(position = position_dodge(width = 0.85), width = 0.75, color = NA) +
  
  # Text counts remain perfectly centered over their respective bars
  geom_text(
    aes(label = label_text),
    position = position_dodge(width = 0.85),
    vjust = -0.6,       
    size = 2.8,         
    fontface = "bold",
    color = "#333333"
  ) +
  
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1), 
    expand = expansion(mult = c(0, 0.15)) 
  ) +
  
  scale_fill_manual(values = c("Blood Culture (BC)" = "#2C3E50", 
                               "Nasopharyngeal Swab (NPS)" = "#BDC3C7")) +
  
  theme_classic(base_size = 12) +
  theme(
    # FIXED: Hard rotation to separate CC88 and SINGLETON
    axis_text_x = element_text(angle = 45, hjust = 1, vjust = 1, fontface = "bold", size = 10),
    axis_text_y = element_text(size = 10),
    axis_title = element_text(face = "bold", size = 12),
    
    panel.grid.major.y = element_line(color = "#EAEAEA", linewidth = 0.5),
    
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 11)
  ) +
  
  labs(
    title = "Fig 5: Clonal Complex Distribution Comparison",
    subtitle = "Relative frequencies (%) and sample counts between Blood Culture and NPS isolates",
    x = "Clonal Complex (CC)",
    y = "Frequency (%)"
  )

# 3. Export as a clear PNG file
graphics.off()
png("Fig 5.png", width = 2800, height = 1800, res = 300)
print(cc_plot)
dev.off()


# Figure 5 Alt scatter plot

library(ggplot2)
library(dplyr)
library(ggrepel) # Ensures text labels never overlap or crowd each other


# 1. PREPARE THE LINEAGE FREQUENCY DATAFRAME

# Using your precise percentages from the approved cohort structure
cc_data <- data.frame(
  CC = c("CC152", "CC15", "CC30", "CC361", "CC72", "CC5", 
         "CC121", "CC153", "CC45", "CC8", "CC80", "CC88", "SINGLETON"),
  BC_freq = c(22.9, 17.1, 15.2, 9.1, 3.0, 3.0, 0.0, 3.0, 0.0, 3.0, 0.0, 0.0, 23.7),
  NPS_freq = c(22.9, 34.3, 20.0, 7.0, 2.0, 2.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 11.0)
)


# 2. BUILD THE SCATTER PLOT WITH EQUALITY LINE

max_axis_limit <- max(c(cc_data$BC_freq, cc_data$NPS_freq)) + 5 +

ggplot(cc_data, aes(x = NPS_freq, y = BC_freq)) +
  # Find max upper bound dynamically to make a perfectly square plot area

  # Step A: Add the 1:1 Identity/Equality Line first (placed in background)
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#1E272C", linewidth = 0.8) +
  
  # Step B: Layer high-contrast clinical theme data points
  geom_point(color = "#00A896", size = 4, alpha = 0.8) +
  
  # Step C: Smart labels that self-reposition to prevent overlaps
  geom_text_repel(
    aes(label = CC),
    fontface = "bold",
    color = "#1A252F",
    size = 3.8,
    box_padding = 0.4,
    point_padding = 0.3,
    max.overlaps = Inf
  ) +
  
  # Step D: Apply square dimensions and clean clinical typography
  scale_x_continuous(limits = c(0, max_axis_limit), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, max_axis_limit), expand = c(0, 0)) +
  coord_fixed() + # Forces a 1:1 aspect ratio so the diagonal line is perfectly 45 degrees
  labs(
    x = "Nasopharyngeal Carriage Frequency (NPS %)",
    y = "Invasive Disease Frequency (BC %)"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    panel.grid.minor = element_blank(), # Remove confusing minor grids
    panel.grid.major = element_line(color = "#E5E5E5", linewidth = 0.5), # Soften major grids
    axis.title = element_text(face = "bold", size = 12, color = "#222222"),
    axis.text = element_text(size = 10, color = "#555555"),
    axis.line = element_line(color = "#1E272C", linewidth = 0.5) # Clean axis base lines
  )

  # Step E: Theme polishing for publication layout (clean off-white feel)
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#1A252F", margin = margin(b = 4)),
    plot.subtitle = element_text(size = 11, color = "#566573", margin = margin(b = 15)),
    axis.title.x = element_text(face = "bold", size = 11, color = "#1A252F", margin = margin(t = 12)),
    axis.title.y = element_text(face = "bold", size = 11, color = "#1A252F", margin = margin(r = 12)),
    panel.grid.major = element_line(color = "#EAECEE", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "#D5D8DC", fill = NA, linewidth = 1),
    
  )



ggsave("Fig 5_Symmetry_Scatter.png",width = 7,height = 5,dpi = 300)

# Fig 5 alt forest plot

library(dplyr)
library(tidyr)
library(lme4)
library(purrr)
library(broom.mixed)
library(ggplot2)

# =========================================================================
# 1. IDENTIFY THE UNIQUE CLONAL COMPLEXES IN YOUR DATA
# =========================================================================
# Adjust "CC" below if your clonal complex column is named differently (e.g., "Clonal_Complex")
cc_column_name <- "Clonal_Complex" 

working_data <- DATA_03 %>%
  filter(Specimen_Type %in% c("INVASIVE", "NON-INVASIVE")) %>%
  mutate(
    Is_Invasive = ifelse(Specimen_Type == "INVASIVE", 1, 0),
    Target_CC = as.character(.data[[cc_column_name]])
  )

unique_ccs <- unique(working_data$Target_CC[!is.na(working_data$Target_CC)])

# =========================================================================
# 2. RUN CLUSTER-CORRECTED LOGISTIC MODELS FOR EACH CC
# =========================================================================
calculate_cc_odds_ratio <- function(current_cc, df) {
  # Build a binary presence indicator (1 if this specific CC, 0 if any other CC)
  cc_sub <- df %>%
    mutate(Is_This_CC = ifelse(Target_CC == current_cc, 1, 0))
  
  # Check for complete separation (zero counts in either niche)
  counts <- table(cc_sub$Specimen_Type, cc_sub$Is_This_CC)
  if (any(counts == 0) || nrow(counts) < 2 || ncol(counts) < 2) {
    return(data.frame(CC = current_cc, OR = NA, Lower = NA, Upper = NA, Note = "Zero Count/Separation"))
  }
  
  # Mixed-effects model with Patient ID as a random intercept to guard against pseudoreplication
  model <- tryCatch({
    glmer(Is_This_CC ~ Is_Invasive + (1 | Study_ID), data = cc_sub, family = binomial(link = "logit"))
  }, error = function(e) {
    # Fallback if random effect variance collapses to zero
    suppressWarnings(glm(Is_This_CC ~ Is_Invasive, data = cc_sub, family = binomial(link = "logit")))
  })
  
  # Cleanly pull the Odds Ratio and 95% Confidence Intervals
  tidy_model <- suppressWarnings(tryCatch({
    broom.mixed::tidy(model, conf.int = TRUE, exponentiate = TRUE) %>%
      filter(term == "Is_Invasive")
  }, error = function(e) { NULL }))
  
  if (is.null(tidy_model) || nrow(tidy_model) == 0) {
    return(data.frame(CC = current_cc, OR = NA, Lower = NA, Upper = NA, Note = "Non-convergence"))
  }
  
  data.frame(
    CC = current_cc,
    OR = tidy_model$estimate,
    Lower = tidy_model$conf.low,
    Upper = tidy_model$conf.high,
    Note = "Success"
  )
}

# Loop the model function over all clonal complexes
forest_df <- map_df(unique_ccs, calculate_cc_odds_ratio, df = working_data) %>%
  # Filter out lines that could not be computed due to zero samples
  filter(!is.na(OR)) %>%
  # Arrange vertically from highest to lowest Odds Ratio
  arrange(OR) %>%
  mutate(CC = factor(CC, levels = CC))

# =========================================================================
# 3. GENERATE THE FOREST PLOT
# =========================================================================
forest_plot <- ggplot(forest_df, aes(x = OR, y = CC)) +
  # Reference line at OR = 1.0 (No Niche Association)
  geom_vline(xintercept = 1, linetype = "dashed", color = "#E74C3C", linewidth = 0.8) +
  
  # Confidence Interval Error Bars
  geom_errorbarh(aes(xmin = Lower, xmax = Upper), color = "#34495E", height = 0.2, linewidth = 0.7) +
  
  # Odds Ratio Point Estimates
  geom_point(color = "#2C3E50", fill = "#16A085", shape = 21, size = 3.5, stroke = 1.2) +
  
  # Text labels tracking the precise values on the right-hand margin
  geom_text(
    aes(label = paste0(round(OR, 2), " (", round(Lower, 2), "-", round(Upper, 2), ")")),
    hjust = -0.15, vjust = -0.4, size = 3.2, color = "#566573", check_overlap = TRUE
  ) +
  
  # Logarithmic transformation scales the errors symmetrically on both sides of 1.0
  scale_x_log10(
    breaks = c(0.1, 0.2, 0.5, 1, 2, 5, 10),
    labels = c("0.1", "0.2", "0.5", "1.0", "2.0", "5.0", "10.0")
  ) +
  
  labs(
    title = "Niche Compartment Associations by Clonal Complex",
    subtitle = "Odds Ratios (BC vs NPS) with 95% Cluster-Corrected Confidence Intervals",
    x = "Odds Ratio (Log Scale)\n<-- Enriched in Carriage (NPS) | Enriched in Infection (BC) -->",
    y = "Clonal Complex (CC)"
  ) +
  
  # Clean, submission-quality theme
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#1A252F"),
    plot.subtitle = element_text(size = 11, color = "#7F8C8D", margin = margin(b = 15)),
    axis.title.x = element_text(face = "bold", size = 11, color = "#1A252F", margin = margin(t = 12)),
    axis.title.y = element_text(face = "bold", size = 11, color = "#1A252F"),
    axis.text.y = element_text(face = "bold", color = "#2C3E50"),
    panel.grid.major.x = element_line(color = "#EAECEE", linewidth = 0.5),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "#F2F4F4", linewidth = 0.5),
    plot.margin = margin(20, 40, 20, 20) # Added right-side margin buffer for text strings
  )

# =========================================================================
# 4. SAVE EXPORT AT 300 DPI
# =========================================================================
ggsave(
  filename = "Fig 5_Lineage_ForestPlot.png",
  plot = forest_plot,
  width = 8,
  height = 6,
  dpi = 300,
  bg = "white"
)


# Alt fig 5 : Dual panel

library(dplyr)
library(tidyr)
library(lme4)
library(purrr)
library(broom.mixed)
library(ggplot2)
library(patchwork) 

# =========================================================================
# 1. BASE VARIABLES ALIGNMENT & DATA CLEANING
# =========================================================================
cc_column_name <- "Clonal_Complex"

working_data <- DATA_03 %>%
  filter(Specimen_Type %in% c("INVASIVE", "NON-INVASIVE")) %>%
  mutate(
    Is_Invasive = ifelse(Specimen_Type == "INVASIVE", 1, 0),
    Target_CC = as.character(.data[[cc_column_name]])
  ) %>%
  filter(!is.na(Target_CC))

unique_ccs <- unique(working_data$Target_CC)

# =========================================================================
# 2. ESTABLISH GLOBAL COLOR PALETTE
# =========================================================================
lineage_colors <- c(
  "CC152"     = "#16A085", "CC15"  = "#2C3E50", "CC30"  = "#34495E",
  "CC361"     = "#7F8C8D", "CC72"  = "#95A5A6", "CC5"   = "#BDC3C7",
  "CC121"     = "#D5D8DC", "CC153" = "#E5E7E9", "CC45"  = "#F2F3F4",
  "CC8"       = "#EAEDED", "CC80"  = "#F4F6F7", "CC88"  = "#F8F9F9",
  "SINGLETON" = "#76D7C4"
)

# =========================================================================
# 3. BUILD PANEL A (COMPOSITION PLOT)
# =========================================================================
composition_df <- working_data %>%
  group_by(Specimen_Type, Target_CC) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Specimen_Type) %>%
  mutate(
    Total = sum(Count),
    Percentage = (Count / Total) * 100,
    Compartment = ifelse(Specimen_Type == "INVASIVE", "Blood Culture\n(BC)", "Nasopharyngeal\nSwab (NPS)")
  )

# RESTORED: This whole visualization section was missing from your script
plot_a % mutate(Is_This_CC = ifelse(Target_CC == current_cc, 1, 0))
counts <- table(cc_sub$Specimen_Type, cc_sub$Is_This_CC)

if (any(counts == 0) || nrow(counts) < 2 || ncol(counts) < 2) {
  return(NULL) 
}

model <- tryCatch({
  glmer(Is_This_CC ~ Is_Invasive + (1 | Study_ID), data = cc_sub, family = binomial(link = "logit"))
}, error = function(e) {
  suppressWarnings(glm(Is_This_CC ~ Is_Invasive, data = cc_sub, family = binomial(link = "logit")))
})

tidy_model <- suppressWarnings(tryCatch({
  broom.mixed::tidy(model, conf.int = TRUE, exponentiate = TRUE) %>% filter(term == "Is_Invasive")
}, error = function(e) { NULL }))

if (is.null(tidy_model) || nrow(tidy_model) == 0) return(NULL)

data.frame(
  CC = current_cc, 
  OR = tidy_model$estimate,
  Lower = tidy_model$conf.low, 
  Upper = tidy_model$conf.high,
  P_Val = tidy_model$p.value
)


# =========================================================================
# 5. BUILD PANEL B (FOREST PLOT)
# =========================================================================
forest_df <- map_df(unique_ccs, run_isolated_glmm, input_df = working_data) %>%
  mutate(
    Sig_Label = ifelse(P_Val < 0.05, "*", ""),
    CC = factor(CC, levels = arrange(., OR)$CC)
  )

plot_b <- ggplot(forest_df, aes(x = OR, y = CC)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "#E74C3C", linewidth = 0.8) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper), color = "#7F8C8D", height = 0.15, linewidth = 0.6) +
  geom_point(aes(fill = CC), color = "#1A252F", shape = 21, size = 3.5, stroke = 1.1, show.legend = FALSE) +
  scale_fill_manual(values = lineage_colors) +
  geom_text(aes(label = Sig_Label), hjust = -0.5, vjust = 0.3, size = 6, color = "#E74C3C", fontface = "bold") +
  scale_x_log10(breaks = c(0.1, 0.5, 1, 2, 10), labels = c("0.1", "0.5", "1.0", "2.0", "10.0")) +
  labs(
    title = "B. Cluster-Corrected Odds Ratios",
    subtitle = "(* denotes p < 0.05 significant divergence)",
    x = "Odds Ratio (Log Scale)\n<-- NPS Enriched | BC Enriched -->", y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13, color = "#1A252F"),
    plot.subtitle = element_text(size = 9, color = "#7F8C8D", face = "italic"),
    axis.text.y = element_text(face = "bold", color = "#2C3E50"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#F2F4F4")
  )

# =========================================================================
# 6. STITCH CANVAS AND EXPORT
# =========================================================================
final_dual_plot <- plot_a + plot_b + plot_layout(widths = c(1, 1.2))

ggsave(
  filename = "Fig 5_DualPanel_Lineages.png",
  plot = final_dual_plot,
  width = 11,
  height = 5.5,
  dpi = 300,
  bg = "white"
)



# PCA PLOT

library(readxl)
DATA_03 <- read_excel("~/WORKINGS/DATABASES/DATA_03.xlsx")
View(DATA_03)

library(ade4)
library(dplyr)

# Drop the MLST columns to create the metadata dataframe
metadata <- DATA_03 %>% 
  select(-c(arcC, aroE, glpF, gmk, pta, tpi, yqiL))

# Keep only the MLST columns to create the genomic dataframe
mlst_data <- DATA_03 %>% 
  select(arcC, aroE, glpF, gmk, pta, tpi, yqiL)

# Lock row names using the Sample_ID column
rownames(mlst_data) <- DATA_03$Sample_ID

# 3. FIX: Convert to factors AND force it back into a data frame
mlst_factor_df <- as.data.frame(lapply(mlst_data, as.factor))

# 4. Re-assign row names (lapply can sometimes strip them out)
rownames(mlst_factor_df) <- rownames(mlst_data)

# 5. Expand into the binary disjunctive grid
mlst_binary <- acm.disjonctif(mlst_factor_df)

# 5. Run the dudi.pca on this expanded allelic profile
# center = TRUE centers the alleles; scale = FALSE avoids over-inflating rare alleles

pca_results <- dudi.pca(mlst_binary, center = TRUE, scale = FALSE, scannf = FALSE, nf = 2)


library(ggplot2)

# 1. Extract coordinates from the dudi.pca output
plot_data <- as.data.frame(pca_results$li)
plot_data$Sample_ID <- rownames(mlst_data)

# 2. Merge back the metadata using the common Sample_ID column
final_plot_dataframe <- merge(plot_data, metadata, by = "Sample_ID")

# 3. Create a faceted plot to isolate Blood Culture from NPS
ggplot(final_plot_dataframe, aes(x = Axis1, y = Axis2, color = Clonal_Complex)) +
  geom_point(size = 3, alpha = 0.8) +
  stat_ellipse(aes(fill = Clonal_Complex), geom = "polygon", alpha = 0.1, level = 0.95) +
  facet_wrap(~ Specimen_Type) + 
  labs(
    title = "MLST-Based PCA Distribution of Clonal Complexes",
    subtitle = "Separated by Tissue Source (Blood Culture vs. NPS)",
    x = "Principal Component 1",
    y = "Principal Component 2"
  ) +
  theme_bw() +
  theme(panel.grid = element_blank())

ggsave("PCA.png", width = 10, height = 5, dpi = 300)


library(ade4)

# Plot 1: Colored by Clonal Complex
s.class(pca_results$li, fac = as.factor(metadata$Clonal_Complex), 
        col = rainbow(length(unique(metadata$Clonal_Complex))),
        grid = FALSE, sub = "Isolates Grouped by CC")

# Plot 2: Colored by Source (BC vs NPS)
s.class(pca_results$li, fac = as.factor(metadata$Specimen_Type), 
        col = c("darkred", "darkblue"),
        grid = FALSE, sub = "Isolates Grouped by Source")


library(ggplot2)
library(ggforce)

# 1. Prepare your plotting data frame
plot_data <- as.data.frame(pca_results$li)
plot_data$Clonal_Complex <- metadata$Clonal_Complex

# 2. Calculate centroids (barycenters) for each Clonal Complex
centroids <- aggregate(cbind(Axis1, Axis2) ~ Clonal_Complex, data = plot_data, mean)
colnames(centroids)[2:3] <- c("cX", "cY")

# Merge centroids back into the main plot data frame
plot_data <- merge(plot_data, centroids, by = "Clonal_Complex")

# 3. Generate the Clonal Complex Spider Plot
p_cc <- ggplot(plot_data, aes(x = Axis1, y = Axis2, color = Clonal_Complex)) +
  # Draw spider lines connecting individual isolates to their CC center
  geom_segment(aes(xend = cX, yend = cY), alpha = 0.4, linewidth = 0.5) +
  # Draw the data points
  geom_point(size = 2.5, alpha = 0.8) +
  # Draw clean normal ellipses around each Clonal Complex group
  stat_ellipse(aes(fill = Clonal_Complex), geom = "polygon", alpha = 0.05, level = 0.95) +
  # Place text labels cleanly at the center of each CC cluster
  geom_label(data = centroids, aes(x = cX, y = cY, label = Clonal_Complex), 
             size = 3.5, fontface = "bold", fill = "white", show.legend = FALSE) +
  # Apply professional layout styling
  labs(
    title = "Isolates Grouped by Clonal Complex",
    subtitle = "MLST-based genetic clustering",
    x = "PC1", 
    y = "PC2"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.title = element_blank()
  )

# Print the plot on your screen
print(p_cc)

# 4. Save the plot with publication-grade 300 DPI sharpness
ggsave("PCA_Clonal_Complex_Spider_Plot.png", width = 9, height = 6, dpi = 300)








# PHYLOGENETIC TREE

# Install the remotes package from CRAN
install.packages("remotes")

# Use it to pull ggtree straight from GitHub
remotes::install_github("YuLab-SMU/ggtree")

remotes::install_github("YuLab-SMU/treeio")  # install the missing treeio package from github
remotes::install_github("YuLab-SMU/ggtree")  # re-run installation of ggtree


# Try installing again

install.packages("BiocManager")    # istall manager tool from CRAN
BiocManager::install("ggtree")     # hosted on Bioconductor (A special repository for biology and gene data) instead of CRAN


library(ape)
library(ggtree)
library(tidyverse)


# Read the tree

tree <- read.tree("C:/Users/sulsowe/Documents/WORKINGS/PLOTS/Manuscript_03/staph_tree.treefile")

# Quick Check
tree

plot(tree, cex = 0.5)

# Read meta data from excel
library(readxl)

meta <- read_excel("C:/Users/sulsowe/Documents/WORKINGS/DATABASES/DATA_03.xlsx")

# Check Matching IDs
head(tree$tip.label)
head(meta$Sample_ID)

# Check mismatches
setdiff(tree$tip.label, meta$Sample_ID)

setdiff(meta$Sample_ID, tree$tip.label)


# ISOLATES FILTERING SCRIPT

library(ggtree)
library(treeio)
library(readxl)
library(dplyr) #  filter data easily
library(tibble)
# 1. Load the IQ-TREE file and  Excel file
my_tree <- read.newick(file.choose())
phylogeny_metadata <- read_excel(file.choose())

# 2. Get the list of isolate names that are actually inside the tree
isolates_in_tree <- my_tree$tip.label

phylogeny_metadata$Sample_ID <- trimws(phylogeny_metadata$Sample_ID)

# 3. Filter the Excel sheet to keep only rows that match the tree labels
filtered_metadata <- phylogeny_metadata %>% 
  filter(Sample_ID %in% isolates_in_tree)

#Double-Check
# Count how many tips are in the tree
length(my_tree$tip.label)

# Count how many rows are left in the filtered metadata
nrow(filtered_metadata)

# Diagnosis
# Find isolate names that are in the tree but missing/different in the Excel sheet
mismatched_isolates <- setdiff(my_tree$tip.label, phylogeny_metadata$Sample_ID)
print(mismatched_isolates)



# 4. Bind the filtered metadata to your tree and plot it!
ggtree(my_tree) %<+% filtered_metadata +
  geom_tiplab(size = 3) +
  geom_tippoint(aes(color = Specimen), size = 3) + 
  theme_tree2()

# Plot using the classic unrooted daylight geometry layout
ggtree(my_tree, layout = "daylight") %<+% filtered_metadata +
  geom_tippoint(aes(color = Specimen), size = 3, alpha = 0.8) +
  scale_color_manual(values = c("Blood Culture " = "#d95f02", "NPS" = "#7570b3")) +
  theme_tree() +
  theme(legend.position = "right")


# Quick Check 

plot(my_tree, cex = 0.5)

# Basic ggtree

p <- ggtree(my_tree) %<+% filtered_metadata

p +
  geom_tiplab(size = 2)

library(phangorn)

tree_rooted <- midpoint(my_tree)

ggtree(tree_rooted) +
  geom_tiplab(size = 2)

ggtree(tree_rooted) %<+% filtered_metadata +
  geom_tippoint(aes(color = Specimen ), size = 3) +
  geom_tiplab(size = 2)

# Circular Tree
ggtree(tree_rooted,
       layout = "circular") %<+% filtered_metadata +
  geom_tippoint(aes(color = Clonal_Complex),
                size = 2)

# Bootstrap support
tree_rooted$node.label[1:10]

ggtree(tree_rooted) +
  geom_text2(
    aes(subset = !isTip,
        label = label),
    size = 2
  )

# Suitable figure
library(RColorBrewer)

ggtree(tree_rooted) %<+% filtered_metadata +
  geom_tippoint(
    aes(color = Specimen),
    size = 2.5
  ) +
  geom_tiplab(size = 2) +
  theme_tree2()

# Metadata heatmap beside tree

p <- ggtree(tree_rooted)

meta2 <- filtered_metadata %>%
  column_to_rownames("Sample_ID")

gheatmap(
  p,
  meta2[, c("Specimen",
            "Year",
            "ST")],
  width = 0.3
)

ggtree(my_tree) +
  geom_tiplab(size = 1.5) +
  theme_tree2()

ggtree(tree_rooted) +
  geom_tiplab(size = 1.5) +
  theme_tree2()
# Topology-only version--estimated evolutionary distance cannot be inferred

ggtree(tree_rooted, branch.length = "none") +
  geom_tiplab(size = 1.5) +
  theme_tree2()

############## investigating apparent outgroup
ggtree(my_tree, branch.length = "none")
ggtree(tree_rooted, branch.length = "none")

summary(my_tree$edge.length)

max(my_tree$edge.length)

sort(my_tree$edge.length, decreasing = TRUE)[1:10]

my_tree$tip.label[which.max(my_tree$edge.length)]

# Find the edge with the longest branch
long_edge <- which.max(my_tree$edge.length)

# Get parent and child node numbers
my_tree$edge[long_edge, ]
Ntip(my_tree)

my_tree$edge[long_edge, 2] <= Ntip(my_tree)
library(ape)

long_node <- my_tree$edge[long_edge, 2]

long_clade <- extract.clade(my_tree, node = long_node)

long_clade$tip.label

outgroup <- long_clade$tip.label

outgroup

is.monophyletic(my_tree, outgroup)

long_clade$tip.label
filtered_metadata %>%
  filter(Sample_ID %in% long_clade$tip.label) %>%
  select(Sample_ID, Specimen_Type, Study_ID, ST, Clonal_Complex, Year)

filtered_metadata %>%
  filter(Study_ID %in% c(9756, 6530, 26931, 31576, 30155, 33023)) %>%
  select(Sample_ID, Specimen_Type, Study_ID, ST, Clonal_Complex, Year)