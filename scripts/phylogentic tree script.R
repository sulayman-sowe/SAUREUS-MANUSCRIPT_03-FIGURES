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


long_clade$tip.label
filtered_metadata %>%
  filter(Sample_ID %in% long_clade$tip.label) %>%
  select(Sample_ID, Specimen_Type, Study_ID, ST, Clonal_Complex, Year)

filtered_metadata %>%
  filter(Study_ID %in% c(9756, 6530, 26931, 31576, 30155, 33023)) %>%
  select(Sample_ID, Specimen_Type, Study_ID, ST, Clonal_Complex, Year)

six_isolate_clade <- filtered_metadata %>%
  filter(
    Sample_ID %in% c(
      "06530H1_2",
      "31576H1_1",
      "09756H1_6",
      "30155H1_5",
      "33023H1_1",
      "26931H1_1"
    )
  )

# After confirming six-isolate clade as an outgroup

# establishing correct rooted tree

my_tree

outgroup <- c("06530H1_2","31576H1_1","09756H1_6","30155H1_5","33023H1_1","26931H1_1") # assigning outgroup to the six isolate clade

library(ape)

is.monophyletic(my_tree, outgroup) # confirming monophyly

tree_rooted <- root(my_tree,outgroup = outgroup,resolve.root = TRUE) # rooting the tree
is.rooted(tree_rooted)  # confirmation


# Check that the root is biologically sensible

plot(
  tree_rooted,
  cex = 0.5,
  no.margin = TRUE
)

tiplabels(
  pch = 19,
  col = ifelse(
    tree_rooted$tip.label %in% outgroup,
    "red",
    "black"
  ),
  cex = 0.5
)

# Check the outgroup is actually represented correctly in the metadata

filtered_metadata %>%
  filter(Sample_ID %in% outgroup) %>%
  select(
    Sample_ID,
    Specimen_Type,
    Study_ID,
    ST,
    Clonal_Complex,
    Year
  )
filtered_metadata <- filtered_metadata %>%
  mutate(
    Phylo_Group = if_else(
      Sample_ID %in% outgroup,
      "Outgroup",
      "Study_isolate"
    )
  )

# creating basic ggtree
library(ggtree)

p <- ggtree(tree_rooted)

p + geom_tiplab(size = 2)

# Attach metadata

p <- ggtree(tree_rooted) %<+% filtered_metadata

# verify
p +
  geom_tippoint(
    aes(color = Specimen),
    size = 2
  )

# SUMMARY OF ABOVE
library(ape)
library(ggtree)
library(dplyr)

# Six confirmed non-S. aureus outgroup isolates
outgroup <- c(
  "06530H1_2",
  "31576H1_1",
  "09756H1_6",
  "30155H1_5",
  "33023H1_1",
  "26931H1_1"
)

# Confirm monophyly
is.monophyletic(my_tree, outgroup)

# Root tree
tree_rooted <- root(
  my_tree,
  outgroup = outgroup,
  resolve.root = TRUE
)

# Confirm rooting
is.rooted(tree_rooted)

# Create plotting group
filtered_metadata_plot <- filtered_metadata %>%
  mutate(
    Tree_Group = case_when(
      Sample_ID %in% outgroup ~ "Outgroup",
      Specimen_Type == "INVASIVE" ~ "BC",
      Specimen_Type == "NON-INVASIVE" ~ "NPS",
      TRUE ~ "Other"
    )
  )

# Attach metadata
p <- ggtree(tree_rooted) %<+% filtered_metadata_plot

# Basic biological tree
p +
  geom_tippoint(
    aes(color = Tree_Group),
    size = 2.5
  ) +
  geom_tiplab(
    size = 2
  ) +
  theme_tree2()

# TROUBLESHOOTING
tree_meta <- filtered_metadata_plot %>%
  dplyr::select(
    label = Sample_ID,
    Tree_Group,
    Specimen_Type,
    Study_ID,
    ST,
    Clonal_Complex,
    Year
  )

# Check metadata-tree matching
cat("Tree tips:", length(tree_rooted$tip.label), "\n")
cat("Metadata labels:", length(tree_meta$label), "\n")
cat("Matched:", sum(tree_rooted$tip.label %in% tree_meta$label), "\n")

# Check for duplicated metadata labels
sum(duplicated(tree_meta$label))

# Check whether any tree tips are missing from metadata
setdiff(tree_rooted$tip.label, tree_meta$label)

# Check whether any metadata samples are absent from tree
setdiff(tree_meta$label, tree_rooted$tip.label)

p <- ggtree(tree_rooted) %<+% tree_meta

p +
  geom_tippoint(
    aes(color = Tree_Group),
    size = 2
  ) +
  theme_tree2()   # FIXED ISSUE


# TOPOLOGY FOCUSED VERSION

library(ggtree)
library(dplyr)

p_topology <- ggtree(
  tree_rooted,
  branch.length = "none"
) %<+% tree_meta

p_topology +
  geom_tippoint(
    aes(color = Tree_Group),
    size = 2.2
  ) +
  scale_color_manual(
    values = c(
      "BC" = "#D55E00",
      "NPS" = "#009E73",
      "Outgroup" = "#0072B2"
    )
  ) +
  theme_tree2() +
  theme(
    legend.position = "right"
  )

# BOOTSTRAP SUPPORT VALIDATION

length(tree_rooted$node.label)

head(tree_rooted$node.label, 20)

Ntip(tree_rooted)

Nnode(tree_rooted)

summary(as.numeric(tree_rooted$node.label))

table(tree_rooted$node.label)

support_df <- data.frame(
  node = (Ntip(tree_rooted) + 1):(Ntip(tree_rooted) + Nnode(tree_rooted)),
  support = tree_rooted$node.label,
  stringsAsFactors = FALSE
)

support_df <- support_df %>%
  filter(
    !is.na(support),
    support != "",
    support != "Root"
  ) %>%
  tidyr::separate(
    support,
    into = c("SH_aLRT", "UFBoot"),
    sep = "/",
    convert = TRUE
  )

head(support_df, 20)

summary(support_df$SH_aLRT)
summary(support_df$UFBoot)

sum(
  support_df$SH_aLRT >= 80 &
    support_df$UFBoot >= 95
)

# tree with support values

p_support <- ggtree(
  tree_rooted,
  branch.length = "none"
) %<+% tree_meta

p_support +
  geom_tippoint(
    aes(color = Tree_Group),
    size = 2
  ) +
  geom_text2(
    aes(
      subset = !isTip &
        label != "" &
        label != "Root" &
        as.numeric(sub(".*\\/", "", label)) >= 95 &
        as.numeric(sub("\\/.*", "", label)) >= 90,
      label = label
    ),
    size = 2.2,
    hjust = -0.15
  ) +
  scale_color_manual(
    values = c(
      "BC" = "#D55E00",
      "NPS" = "#009E73",
      "Outgroup" = "#0072B2"
    )
  ) +
  theme_tree() +
  theme(
    legend.position = "right"
  )

# Determining the appropriate support threshold

sum(support_df$SH_aLRT >= 80 & support_df$UFBoot >= 95)

sum(support_df$SH_aLRT >= 90 & support_df$UFBoot >= 95)

sum(support_df$SH_aLRT >= 95 & support_df$UFBoot >= 95)

# Quantifying CC structure

table(tree_meta$Clonal_Complex, useNA = "ifany")

sort(table(tree_meta$Clonal_Complex), decreasing = TRUE)

length(unique(na.omit(tree_meta$Clonal_Complex)))

# CC check
tree_meta %>%
  filter(is.na(Clonal_Complex)) %>%
  select(label, Tree_Group, Specimen_Type, Study_ID, ST, Clonal_Complex)


# what we have in it?
tree_meta %>%
  filter(
    Tree_Group %in% c("BC", "NPS"),
    Clonal_Complex %in% c(
      "CC15",
      "CC152",
      "CC30",
      "CC361",
      "CC121"
    )
  ) %>%
  count(Clonal_Complex, Tree_Group) %>%
  tidyr::pivot_wider(
    names_from = Tree_Group,
    values_from = n,
    values_fill = 0
  )

# Adding the CC_plot column
tree_meta <- tree_meta %>%
  mutate(
    CC_plot = case_when(
      Tree_Group == "Outgroup" ~ "Outgroup",
      is.na(Clonal_Complex) ~ "Missing",
      Clonal_Complex == "Singleton" ~ "Singleton",
      Clonal_Complex %in% c(
        "CC15",
        "CC152",
        "CC30",
        "CC361",
        "CC121"
      ) ~ Clonal_Complex,
      TRUE ~ "Other CC"
    )
  )
 
# CC Annotated Version

library(ggtree)
library(ggplot2)
library(dplyr)
library(tidyr)


# Basic cc attached tree
p_cc <- ggtree(
  tree_rooted,
  branch.length = "none",
  ladderize = TRUE
) %<+% tree_meta

# tree with specimen types and CC

# 1. Install the prerequisite biological tracking package from GitHub
remotes::install_github("Bioconductor/Biostrings")

# 2. Pull ggtreeExtra directly from the author's GitHub repository
remotes::install_github("YuLab-SMU/ggtreeExtra")

# 3. Initialize and load the library
library(ggtreeExtra)


p_cc +
  geom_tippoint(
    aes(color = Tree_Group),
    size = 2.2
  ) +
  ggtreeExtra::geom_fruit(
    geom = geom_tile,
    mapping = aes(
      x = "CC",
      fill = CC_plot
    ),
    offset = 0.02,
    width = 0.08
  ) +
  scale_color_manual(
    name = "Specimen",
    values = c(
      "BC" = "#D55E00",
      "NPS" = "#009E73",
      "Outgroup" = "#0072B2"
    )
  ) +
  scale_fill_manual(
    name = "Clonal Complex",
    values = c(
      "CC15" = "#E69F00",
      "CC152" = "#56B4E9",
      "CC30" = "#009E73",
      "CC361" = "#F0E442",
      "CC121" = "#CC79A7",
      "Singleton" = "#999999",
      "Other CC" = "#666666",
      "Missing" = "#D9D9D9",
      "Outgroup" = "#0072B2"
    ),
    drop = FALSE
  ) +
  theme_tree() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 9)
  )

# Support values added to version above (CCs included version)
p_cc +
  geom_tippoint(
    aes(color = Tree_Group),
    size = 2.2
  ) +
  ggtreeExtra::geom_fruit(
    geom = geom_tile,
    mapping = aes(
      x = "CC",
      fill = CC_plot
    ),
    offset = 0.02,
    width = 0.08
  ) +
  geom_text2(
    aes(
      subset =
        !isTip &
        label != "" &
        label != "Root" &
        as.numeric(sub("\\/.*", "", label)) >= 90 &
        as.numeric(sub(".*\\/", "", label)) >= 95,
      label = label
    ),
    size = 2.1,
    hjust = -0.15
    
  ) +
  scale_color_manual(
    name = "Specimen",
    values = c(
      "BC" = "#D55E00",
      "NPS" = "#009E73",
      "Outgroup" = "#0072B2"
    )
  ) +
  scale_fill_manual(
    name = "Clonal Complex",
    values = c(
      "CC15" = "#E69F00",
      "CC152" = "#56B4E9",
      "CC30" = "#009E73",
      "CC361" = "#F0E442",
      "CC121" = "#CC79A7",
      "Singleton" = "#999999",
      "Other CC" = "#666666",
      "Missing" = "#D9D9D9",
      "Outgroup" = "#0072B2"
    ),
    drop = FALSE
  ) +
  theme_tree() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 9)
  )



# PATIENT PAIRING LAYER  (Version 1)

# Create BC-NPS pairing

library(dplyr)
library(tidyr)
library(ggtree)
library(ggtreeExtra)
library(ggplot2)

pair_links <- tree_meta %>%
  filter(Tree_Group %in% c("BC", "NPS")) %>%
  select(label, Study_ID, Tree_Group) %>%
  group_by(Study_ID) %>%
  filter(
    any(Tree_Group == "BC"),
    any(Tree_Group == "NPS")
  ) %>%
  summarise(
    BC_label = list(label[Tree_Group == "BC"]),
    NPS_label = list(label[Tree_Group == "NPS"]),
    .groups = "drop"
  ) %>%
  unnest(BC_label) %>%
  unnest(NPS_label) %>%
  rename(
    taxa1 = BC_label,
    taxa2 = NPS_label
  )

# patient pairing figure (Version1)
p_pair <- ggtree(
  tree_rooted,
  branch.length = "none",
  ladderize = TRUE
) %<+% tree_meta


p_pair <- p_pair +
  
  # --------------------------------------------------
# 1. Patient-pair links
# --------------------------------------------------
geom_taxalink(
  data = pair_links,
  mapping = aes(
    taxa1 = taxa1,
    taxa2 = taxa2
  ),
  colour = "grey40",
  linewidth = 0.35,
  alpha = 0.30,
  curvature = 0.15
) +
  
  # --------------------------------------------------
# 2. BC / NPS / Outgroup tip colours
# --------------------------------------------------
geom_tippoint(
  aes(color = Tree_Group),
  size = 2.2
) +
  
  # --------------------------------------------------
# 3. CC annotation strip
# --------------------------------------------------
geom_fruit(
  geom = geom_tile,
  mapping = aes(
    x = "CC",
    fill = CC_plot
  ),
  offset = 0.02,
  width = 0.08
) +
  
  # --------------------------------------------------
# 4. Support values
# SH-aLRT >= 90 & UFBoot >= 95
# --------------------------------------------------
geom_text2(
  aes(
    subset =
      !isTip &
      label != "" &
      label != "Root" &
      as.numeric(sub("\\/.*", "", label)) >= 90 &
      as.numeric(sub(".*\\/", "", label)) >= 95,
    label = label
  ),
  size = 2.0,
  hjust = -0.15
) +
  
  # --------------------------------------------------
# 5. Specimen colours
# --------------------------------------------------
scale_color_manual(
  name = "Specimen type",
  values = c(
    "BC" = "#D55E00",
    "NPS" = "#009E73",
    "Outgroup" = "#0072B2"
  )
) +
  
  # --------------------------------------------------
# 6. CC colours
# --------------------------------------------------
scale_fill_manual(
  name = "Clonal complex",
  values = c(
    "CC15" = "#E69F00",
    "CC152" = "#56B4E9",
    "CC30" = "#009E73",
    "CC361" = "#F0E442",
    "CC121" = "#CC79A7",
    "Singleton" = "#999999",
    "Other CC" = "#666666",
    "Missing" = "#D9D9D9",
    "Outgroup" = "#0072B2"
  ),
  drop = FALSE
) +
  
  # --------------------------------------------------
# 7. Theme
# --------------------------------------------------
theme_tree() +
  theme(
    legend.position = "right",
    legend.title = element_text(
      face = "bold",
      size = 10
    ),
    legend.text = element_text(
      size = 9
    )
  )
p_pair



# # patient pairing figure (Version 2)
# Data wrangling 

paired_patients <- tree_meta %>%
  filter(Tree_Group %in% c("BC", "NPS")) %>%
  group_by(Study_ID) %>%
  filter(
    any(Tree_Group == "BC"),
    any(Tree_Group == "NPS")
  ) %>%
  ungroup()

patient_annotation <- paired_patients %>%
  mutate(
    Patient_ID = paste0("P", Study_ID)
  ) %>%
  select(
    label,
    Study_ID,
    Patient_ID,
    Tree_Group
  )

library(dplyr)
library(ggplot2)
library(ggtree)
library(ggtreeExtra)

p_patient <- ggtree(
  tree_rooted,
  branch.length = "none",
  ladderize = TRUE
) %<+% tree_meta


p_patient <- p_patient +
  
  # --------------------------------------------------
# 1. Specimen type
# --------------------------------------------------
geom_tippoint(
  aes(color = Tree_Group),
  size = 2.2
) +
  
  # --------------------------------------------------
# 2. Clonal complex strip
# --------------------------------------------------
geom_fruit(
  geom = geom_tile,
  mapping = aes(
    y = label,
    x = 1,
    fill = CC_plot
  ),
  offset = 0.02,
  pwidth = 0.08,
  width = 0.08
) +
  
  # --------------------------------------------------
# 3. Patient ID annotation
# --------------------------------------------------
geom_fruit(
  data = patient_annotation,
  geom = geom_text,
  mapping = aes(
    y = label,
    x = 1,
    label = Patient_ID
  ),
  offset = 0.04,
  pwidth = 0.12,
  hjust = 0,
  size = 1.8,
  colour = "grey25"
) +
  
  # --------------------------------------------------
# 4. Bootstrap support
# --------------------------------------------------
geom_text2(
  aes(
    subset =
      !isTip &
      label != "" &
      label != "Root" &
      as.numeric(sub("\\/.*", "", label)) >= 90 &
      as.numeric(sub(".*\\/", "", label)) >= 95,
    label = label
  ),
  size = 2.0,
  hjust = -0.15
) +
  
  # --------------------------------------------------
# 5. Specimen colours
# --------------------------------------------------
scale_color_manual(
  name = "Specimen type",
  values = c(
    "BC" = "#D55E00",
    "NPS" = "#009E73",
    "Outgroup" = "#0072B2"
  )
) +
  
  # --------------------------------------------------
# 6. CC colours
# --------------------------------------------------
scale_fill_manual(
  name = "Clonal complex",
  values = c(
    "CC15" = "#E69F00",
    "CC152" = "#56B4E9",
    "CC30" = "#009E73",
    "CC361" = "#F0E442",
    "CC121" = "#CC79A7",
    "Singleton" = "#999999",
    "Other CC" = "#666666",
    "Missing" = "#D9D9D9",
    "Outgroup" = "#0072B2"
  ),
  drop = FALSE
) +
  
  # --------------------------------------------------
# 7. Theme
# --------------------------------------------------
theme_tree() +
  theme(
    legend.position = "right",
    legend.title = element_text(
      face = "bold",
      size = 10
    ),
    legend.text = element_text(
      size = 9
    )
  )

p_patient


## VERSION 3 (TWO PANELS)

# Panel A

p_phylo <- ggtree(
  tree_rooted,
  branch.length = "none",
  ladderize = TRUE
) %<+% tree_meta

p_phylo <- p_phylo +
  geom_tippoint(
    aes(color = Tree_Group),
    size = 2.2
  )

p_phylo <- p_phylo +
  geom_fruit(
    geom = geom_tile,
    mapping = aes(
      y = label,
      x = "CC",
      fill = CC_plot
    ),
    offset = 0.02,
    pwidth = 0.08,
    width = 0.08
  )

p_phylo <- p_phylo +
  geom_text2(
    aes(
      subset =
        !isTip &
        label != "" &
        label != "Root" &
        as.numeric(sub("\\/.*", "", label)) >= 90 &
        as.numeric(sub(".*\\/", "", label)) >= 95,
      label = label
    ),
    size = 2.0,
    hjust = -0.15
  )

p_phylo <- p_phylo +
  scale_color_manual(
    name = "Specimen type",
    values = c(
      "BC" = "#D55E00",
      "NPS" = "#009E73",
      "Outgroup" = "#0072B2"
    )
  ) +
  scale_fill_manual(
    name = "Clonal complex",
    values = c(
      "CC15" = "#E69F00",
      "CC152" = "#56B4E9",
      "CC30" = "#009E73",
      "CC361" = "#F0E442",
      "CC121" = "#CC79A7",
      "Singleton" = "#999999",
      "Other CC" = "#666666",
      "Missing" = "#D9D9D9",
      "Outgroup" = "#0072B2"
    ),
    drop = FALSE
  ) +
  theme_tree() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9)
  )

p_phylo



# PANEL B


# Calculating the tree distance matrix cophenetic function via ape
library(ape)

tree_dist <- cophenetic.phylo(tree_rooted)

# Check 
dim(tree_dist)

tree_dist[1:5, 1:5]

# Generate all BC–NPS pairs within each patient

bc_nps_pairs <- paired_patients %>%
  select(
    Study_ID,
    label,
    Tree_Group
  ) %>%
  group_by(Study_ID) %>%
  summarise(
    BC = list(label[Tree_Group == "BC"]),
    NPS = list(label[Tree_Group == "NPS"]),
    .groups = "drop"
  ) %>%
  tidyr::unnest(BC) %>%
  tidyr::unnest(NPS)

# Rename
bc_nps_pairs <- bc_nps_pairs %>%
  rename(
    BC_label = BC,
    NPS_label = NPS
  )

# Extract the phylogenetic distance

bc_nps_pairs <- bc_nps_pairs %>%
  mutate(
    Patristic_Distance = mapply(
      function(bc, nps) {
        tree_dist[bc, nps]
      },
      BC_label,
      NPS_label
    )
  )

# Adding specimen data

bc_nps_pairs <- bc_nps_pairs %>%
  left_join(
    tree_meta %>%
      select(
        label,
        ST,
        Clonal_Complex
      ) %>%
      rename(
        BC_label = label,
        BC_ST = ST,
        BC_CC = Clonal_Complex
      ),
    by = "BC_label"
  ) %>%
  left_join(
    tree_meta %>%
      select(
        label,
        ST,
        Clonal_Complex
      ) %>%
      rename(
        NPS_label = label,
        NPS_ST = ST,
        NPS_CC = Clonal_Complex
      ),
    by = "NPS_label"
  )

# Plot the within-patient distances using a dotPlot

library(ggplot2)

p_relatedness <- ggplot(
  bc_nps_pairs,
  aes(
    x = Patristic_Distance,
    y = reorder(as.factor(Study_ID), Patristic_Distance)
  )
) +
  geom_point(
    size = 2.5,
    alpha = 0.75
  ) +
  labs(
    x = "BC–NPS patristic distance",
    y = "Patient",
    title = "Within-patient phylogenetic relatedness"
  ) +
  theme_classic(base_size = 11)

p_relatedness


# Combine Panel A and B

library(patchwork)

final_two_panel <- p_phylo +
  p_relatedness +
  plot_layout(
    widths = c(2.4, 1)
  )

# Add Panel Labels

final_two_panel <- (
  p_phylo +
    p_relatedness +
    plot_layout(
      widths = c(2.4, 1)
    )
) +
  plot_annotation(
    tag_levels = "A"
  )

final_two_panel

ggsave("final_two_plot.png", width = 12, height = 6, dpi = 300)

# FINAL version 3 PANEL A

# Panel A — root with the outgroup, then visually omit it

library(dplyr)
library(ggplot2)
library(ggtree)
library(ggtreeExtra)

tree_rooted <- ape::root(
  my_tree,
  outgroup = outgroup,
  resolve.root = TRUE
)

tree_ingroup <- ape::drop.tip(
  tree_rooted,
  tip = outgroup
)

# Check#
ape::is.rooted(tree_ingroup)
ape::Ntip(tree_ingroup)
tree_ingroup$tip.label %in% outgroup


# Building Panel A with actual branch lengths

p_phylo <- ggtree(
  tree_ingroup,
  branch.length = "branch.length",
  ladderize = TRUE
)

# Filtering the metadata to be restricted to the ingroup as well
tree_meta_ingroup <- tree_meta %>%
  filter(label %in% tree_ingroup$tip.label)

# Then:
p_phylo <- ggtree(
  tree_ingroup,
  branch.length = "branch.length",
  ladderize = TRUE
) %<+% tree_meta_ingroup

# CC strip
p_phylo <- p_phylo +
  ggtreeExtra::geom_fruit(
    geom = geom_tile,
    mapping = aes(
      y = label,
      x = 1,
      fill = CC_plot
    ),
    offset = 0.05,  # gap between tree and strip
    pwidth = 0.003,
    width = 0.003,
    inherit.aes = FALSE
  )

  # --------------------------------------------------
# 4. Bootstrap support
# --------------------------------------------------
p_phylo <- p_phylo +
  geom_text2(
  aes(
    subset =
      !isTip &
      label != "" &
      label != "Root" &
      as.numeric(sub("\\/.*", "", label)) >= 90 &
      as.numeric(sub(".*\\/", "", label)) >= 95,
    label = label
  ),
  size = 2.0,
  hjust = -0.15
) 
# Specimen dots

p_phylo <- p_phylo +
  geom_tippoint(
    aes(color = Tree_Group),
    size = 2.0 ) + 
  
scale_color_manual(
  name = "Specimen type",
  values = c(
    "BC" = "#D55E00",
    "NPS" = "#009E73"
  )

  )  +
  scale_fill_manual(
    name = "Clonal complex",
    values = c(
      "CC15" = "#E69F00",
      "CC152" = "#56B4E9",
      "CC30" = "#009E73",
      "CC361" = "#F0E442",
      "CC121" = "#CC79A7",
      "Singleton" = "#999999",
      "Other CC" = "#666666",
      "Missing" = "#D9D9D9",
      "Outgroup" = "#0072B2"
    ),
    drop = FALSE
  ) +
  theme_tree() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9)
  )

p_phylo


# Panel B 

# Rebuilding the BC-NPS comparision Table

library(dplyr)
library(tidyr)
library(ape)

# Original branch-length distances
tree_dist <- cophenetic.phylo(tree_rooted)

bc_nps_pairs <- tree_meta %>%
  filter(Tree_Group %in% c("BC", "NPS")) %>%
  group_by(Study_ID) %>%
  summarise(
    BC = list(label[Tree_Group == "BC"]),
    NPS = list(label[Tree_Group == "NPS"]),
    .groups = "drop"
  ) %>%
  filter(
    lengths(BC) > 0,
    lengths(NPS) > 0
  ) %>%
  unnest(BC) %>%
  unnest(NPS) %>%
  rename(
    BC_label = BC,
    NPS_label = NPS
  ) %>%
  mutate(
    Patristic_Distance = mapply(
      function(bc, nps) {
        tree_dist[bc, nps]
      },
      BC_label,
      NPS_label
    )
  )

# Add number of NPS comparisions per patient -- For example, a patient with four NPS isolates would have four BC–NPS distances.

patient_summary <- bc_nps_pairs %>%
  group_by(Study_ID) %>%
  summarise(
    n_comparisons = n(),
    min_distance = min(Patristic_Distance),
    median_distance = median(Patristic_Distance),
    max_distance = max(Patristic_Distance),
    .groups = "drop"
  )

# Order patinets by median patristic distance
patient_order <- patient_summary %>%
  arrange(median_distance) %>%
  pull(Study_ID)

# Then:

bc_nps_pairs <- bc_nps_pairs %>%
  mutate(
    Study_ID = factor(
      Study_ID,
      levels = patient_order
    )
  )

patient_summary <- patient_summary %>%
  mutate(
    Study_ID = factor(
      Study_ID,
      levels = patient_order
    )
  )

# Actual PLot version 1

library(ggplot2)

p_relatedness <- ggplot(
  bc_nps_pairs,
  aes(
    x = Patristic_Distance,
    y = Study_ID
  )
) +
  
  # Range of distances observed within each patient
  geom_errorbar(
    data = patient_summary,
    aes(
      y = Study_ID,
      xmin = min_distance,
      xmax = max_distance
    ),
    height = 0.15,
    linewidth = 0.8,
    inherit.aes = FALSE
  ) +
  
  # Individual BC–NPS comparisons
  geom_jitter(
    height = 0.10,
    width = 0,
    size = 2.4,
    alpha = 0.75
  ) +
  
  # Patient-level median
  geom_point(
    data = patient_summary,
    aes(
      x = median_distance,
      y = Study_ID
    ),
    shape = 18,
    size = 3.0,
    inherit.aes = FALSE
  ) +
  
  labs(
    x = "BC–NPS patristic distance",
    y = "Patient ID",
    title = "Within-patient phylogenetic relatedness"
  ) +
  
  theme_classic(base_size = 11)

p_relatedness

# Another layer n= numbeer of NPS per patient

p_relatedness <- p_relatedness +
  geom_text(
    data = patient_summary,
    aes(
      x = max_distance,
      y = Study_ID,
      label = paste0("n=", n_comparisons)
    ),
    hjust = -0.3,
    size = 2.8,
    inherit.aes = FALSE
  ) +
  coord_cartesian(
    clip = "off"
  )

p_relatedness +
  theme(
    plot.margin = margin(
      5.5, 35, 5.5, 5.5
    )
  )

p_relatedness

library(patchwork)

final_two_panel <- p_phylo +
  p_relatedness +
  plot_layout(
    widths = c(2.4, 1)
  )

# Add Panel Labels

final_two_panel <- (
  p_phylo +
    p_relatedness +
    plot_layout(
      widths = c(2.4, 1)
    )
) +
  plot_annotation(
    tag_levels = "A"
  )

final_two_panel


# version 2

p_relatedness <- ggplot(
  bc_nps_pairs,
  aes(
    x = Patristic_Distance,
    y = Study_ID
  )
) +
  
  # Range of BC–NPS distances within each patient
  geom_errorbar(
    data = patient_summary,
    aes(
      y = Study_ID,
      xmin = min_distance,
      xmax = max_distance
    ),
    height = 0.15,
    linewidth = 0.8,
    inherit.aes = FALSE
  ) +
  
  # Individual BC–NPS comparisons
  # Green fill = NPS
  # Orange outline = BC
  geom_jitter(
    aes(
      fill = "NPS",
      colour = "BC"
    ),
    height = 0.10,
    width = 0,
    shape = 21,
    size = 3.0,
    stroke = 1.0,
    alpha = 0.9
  ) +
  
  # Patient-level median
  geom_point(
    data = patient_summary,
    aes(
      x = median_distance,
      y = Study_ID
    ),
    shape = 124,
    size = 5,
    colour = "black",
    inherit.aes = FALSE
  ) +
  
  scale_fill_manual(
    values = c(
      "NPS" = "#009E73"
    ),
    guide = "none"
  ) +
  
  scale_colour_manual(
    values = c(
      "BC" = "#D55E00"
    ),
    guide = "none"
  ) +
  
  labs(
    x = "BC–NPS patristic distance",
    y = "Patient ID"
  ) +
  
  theme_classic(base_size = 11)


# Small explanatory annotation

p_relatedness +
  annotate(
    "text",
    x = Inf,
    y = Inf,
    label = "● NPS   ○ BC",
    hjust = 1.05,
    vjust = 1.5,
    size = 3.0
  )


# Another layer n= numbeer of NPS per patient

p_relatedness <- p_relatedness +
  geom_text(
    data = patient_summary,
    aes(
      x = max_distance,
      y = Study_ID,
      label = paste0("n=", n_comparisons)
    ),
    hjust = -0.6,
    size = 2.8,
    inherit.aes = FALSE
  ) +
  coord_cartesian(
    clip = "off"
  )

p_relatedness +
  theme(
    plot.margin = margin(
      5.5, 45, 5.5, 5.5
    )
  )

p_relatedness

library(patchwork)

final_two_panel <- p_phylo +
  p_relatedness +
  plot_layout(
    widths = c(2.8, 1)
  )

# Add Panel Labels

final_two_panel <- (
  p_phylo +
    p_relatedness +
    plot_layout(
      widths = c(2.8, 1)
    )
) +
  plot_annotation(
    tag_levels = "A"
  )

final_two_panel


ggsave(
  filename = "Figure_phylogeny_within_patient_relatedness.png",
  plot = final_two_panel,
  width = 12,
  height = 7,
  units = "in",
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = "Figure_phylogeny_within_patient_relatedness.pdf",
  plot = final_two_panel,
  width = 12,
  height = 7,
  units = "in",
  device = cairo_pdf,
  bg = "white"
)


# Final Version 3 of PANEL B

# Creating patient level CC classification

patient_cc <- tree_meta %>%
  filter(Tree_Group %in% c("BC", "NPS")) %>%
  group_by(Study_ID) %>%
  summarise(
    BC_CC = {
      x <- Clonal_Complex[Tree_Group == "BC"]
      x <- x[!is.na(x)]
      if (length(x) == 0) NA_character_ else x[1]
    },
    
    NPS_CCs = list(
      unique(
        Clonal_Complex[
          Tree_Group == "NPS" &
            !is.na(Clonal_Complex)
        ]
      )
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    CC_relationship = case_when(
      
      # No BC CC available
      is.na(BC_CC) ~ "Unknown",
      
      # BC CC is represented among NPS isolates
      lengths(NPS_CCs) > 0 &
        mapply(
          function(bc, nps) bc %in% nps,
          BC_CC,
          NPS_CCs
        ) ~ "CC match",
      
      # NPS isolates exist but none matches BC CC
      lengths(NPS_CCs) > 0 ~ "CC mismatch",
      
      # No usable NPS CC information
      TRUE ~ "Unknown"
    )
  )

# Inspect
patient_cc %>%
  select(
    Study_ID,
    BC_CC,
    NPS_CCs,
    CC_relationship
  )

table(patient_cc$CC_relationship)

# Attaching this patient-level variable to every BC–NPS comparison

bc_nps_pairs <- bc_nps_pairs %>%
  select(
    Study_ID,
    BC_label,
    NPS_label,
    Patristic_Distance
  ) %>%
  left_join(
    patient_cc %>%
      select(
        Study_ID,
        BC_CC,
        NPS_CCs,
        CC_relationship
      ),
    by = "Study_ID"
  )

# Check
bc_nps_pairs %>%
  select(
    Study_ID,
    BC_label,
    NPS_label,
    BC_CC,
    NPS_CCs,
    CC_relationship,
    Patristic_Distance
  )


# create patient order

patient_order <- patient_summary %>%
  arrange(median_distance) %>%
  pull(Study_ID)

bc_nps_pairs <- bc_nps_pairs %>%
  mutate(
    Study_ID = factor(
      Study_ID,
      levels = patient_order
    )
  )

# Explicitly make STUDY_ID a factor if not visual comes bad
patient_summary <- patient_summary %>%
  mutate(
    Study_ID = factor(
      Study_ID,
      levels = patient_order
    )
  )

# Check
class(bc_nps_pairs$Study_ID)
class(patient_summary$Study_ID)

# Plot

p_relatedness <- ggplot(
  bc_nps_pairs,
  aes(
    x = Patristic_Distance,
    y = Study_ID
  )
) +
  
  # Patient-level minimum–maximum range
  geom_errorbar(
    data = patient_summary,
    aes(
      y = Study_ID,
      xmin = min_distance,
      xmax = max_distance
    ),
    height = 0.15,
    linewidth = 0.8,
    colour = "grey40",
    inherit.aes = FALSE
  ) +
  
  # Individual BC–NPS comparisons
  geom_jitter(
    aes(
      fill = CC_relationship
    ),
    height = 0.10,
    width = 0,
    shape = 21,
    colour = "black",
    size = 3.0,
    stroke = 0.7,
    alpha = 0.9
  ) +
  
  # Patient-level median
  geom_point(
    data = patient_summary,
    aes(
      x = median_distance,
      y = Study_ID
    ),
    shape = 124,
    size = 5,
    colour = "black",
    inherit.aes = FALSE
  ) +
  
  scale_fill_manual(
    name = "Patient-level CC relationship",
    values = c(
      "CC match" = "#0072B2",
      "CC mismatch" = "#D55E00",
      "Unknown" = "#999999"
    )
  ) +
  
  labs(
    x = "BC–NPS patristic distance",
    y = "Patient ID"
  ) +
  
  theme_classic(base_size = 11)

p_relatedness

# Another layer n= numbeer of NPS per patient

p_relatedness <- p_relatedness +
  geom_text(
    data = patient_summary,
    aes(
      x = max_distance,
      y = Study_ID,
      label = paste0("n=", n_comparisons)
    ),
    hjust = -0.6,
    size = 2.8,
    inherit.aes = FALSE
  ) +
  coord_cartesian(
    clip = "off"
  )

p_relatedness +
  theme(
    plot.margin = margin(
      5.5, 45, 5.5, 5.5
    )
  )

p_relatedness

library(patchwork)

final_two_panel <- p_phylo +
  p_relatedness +
  plot_layout(
    widths = c(2.8, 1)
  )

# Add Panel Labels

final_two_panel <- (
  p_phylo +
    p_relatedness +
    plot_layout(
      widths = c(2.8, 1)
    )
) +
  plot_annotation(
    tag_levels = "A"
  )

final_two_panel


ggsave(
  filename = "Figure_phylogeny_within_patient_relatedness02.png",
  plot = final_two_panel,
  width = 12,
  height = 7,
  units = "in",
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = "Figure_phylogeny_within_patient_relatedness02.pdf",
  plot = final_two_panel,
  width = 12,
  height = 7,
  units = "in",
  device = cairo_pdf,
  bg = "white"
)

ggsave(
  filename = "Figure_phylogeny.png",
  plot = p_phylo,
  width = 12,
  height = 7,
  units = "in",
  dpi = 300,
  bg = "white"
)


ggsave(
  filename = "Figure_phylogeny.svg",
  plot = p_phylo,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = "Figure_phylogeny_within_patient_relatedness02.svg",
  plot = final_two_panel,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)



# PATIENT-LEVEL SUMMARY TABLE

# Calculate patient-level ST relationship

library(dplyr)
library(tidyr)

patient_st <- tree_meta %>%
  filter(Tree_Group %in% c("BC", "NPS")) %>%
  group_by(Study_ID) %>%
  summarise(
    
    # ST of the invasive BC isolate
    BC_ST = {
      x <- ST[Tree_Group == "BC"]
      x <- x[!is.na(x) & x != ""]
      
      if (length(x) == 0) {
        NA_character_
      } else {
        as.character(x[1])
      }
    },
    
    # Unique STs among the patient's NPS isolates
    NPS_STs = list(
      unique(
        as.character(
          ST[
            Tree_Group == "NPS" &
              !is.na(ST) &
              ST != ""
          ]
        )
      )
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    
    # Patient-level ST relationship
    ST_relationship = case_when(
      
      is.na(BC_ST) ~ "Unknown",
      
      lengths(NPS_STs) > 0 &
        mapply(
          function(bc, nps) bc %in% nps,
          BC_ST,
          NPS_STs
        ) ~ "ST match",
      
      lengths(NPS_STs) > 0 ~ "ST mismatch",
      
      TRUE ~ "Unknown"
    )
  )

# repalace CC info with ST info in bc_nps_pairs table

bc_nps_pairs_ST <- bc_nps_pairs %>%
  select(
    Study_ID,
    BC_label,
    NPS_label,
    Patristic_Distance
  ) %>%
  left_join(
    patient_st %>%
      select(
        Study_ID,
        BC_ST,
        NPS_STs,
        ST_relationship
      ),
    by = "Study_ID"
  )

# Calculate Patient-level Summary

patient_summary_ST <- bc_nps_pairs_ST %>%
  group_by(Study_ID) %>%
  summarise(
    
    BC_ST = first(BC_ST),
    
    NPS_STs = paste(
      sort(unique(unlist(NPS_STs))),
      collapse = ", "
    ),
    
    ST_relationship = first(ST_relationship),
    
    n_NPS = n_distinct(NPS_label),
    
    n_comparisons = n(),
    
    min_distance = min(
      Patristic_Distance,
      na.rm = TRUE
    ),
    
    median_distance = median(
      Patristic_Distance,
      na.rm = TRUE
    ),
    
    max_distance = max(
      Patristic_Distance,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  arrange(min_distance)

# Add the closest NPS isolate

closest_nps_ST <- bc_nps_pairs_ST %>%
  group_by(Study_ID) %>%
  slice_min(
    order_by = Patristic_Distance,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup() %>%
  select(
    Study_ID,
    BC_label,
    NPS_label,
    Patristic_Distance
  ) %>%
  rename(
    Closest_BC = BC_label,
    Closest_NPS = NPS_label,
    Minimum_Distance = Patristic_Distance
  )

# Join it
patient_summary_ST <- patient_summary_ST %>%
  left_join(
    closest_nps_ST,
    by = "Study_ID"
  )

# Add NPS ST Diversity

nps_st_diversity <- tree_meta %>%
  filter(Tree_Group == "NPS") %>%
  group_by(Study_ID) %>%
  summarise(
    n_unique_NPS_STs = n_distinct(
      ST,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

patient_summary_ST <- patient_summary_ST %>%
  left_join(
    nps_st_diversity,
    by = "Study_ID"
  )

# Then Arrange the Final Columns

patient_summary_ST <- patient_summary_ST %>%
  select(
    Study_ID,
    BC_ST,
    NPS_STs,
    ST_relationship,
    n_NPS,
    n_unique_NPS_STs,
    n_comparisons,
    Closest_BC,
    Closest_NPS,
    Minimum_Distance,
    median_distance,
    max_distance
  ) %>%
  arrange(Minimum_Distance)


# Key Quantitative Comparision

patient_summary_ST %>%
  filter(ST_relationship %in% c("ST match", "ST mismatch")) %>%
  group_by(ST_relationship) %>%
  summarise(
    n = n(),
    median_min_distance = median(
      Minimum_Distance,
      na.rm = TRUE
    ),
    IQR_min_distance = IQR(
      Minimum_Distance,
      na.rm = TRUE
    ),
    min_min_distance = min(
      Minimum_Distance,
      na.rm = TRUE
    ),
    max_min_distance = max(
      Minimum_Distance,
      na.rm = TRUE
    )
  )


# Wilcoxon rank-sum test comparing the patient-level minimum BC–NPS patristic distance between ST-match and ST-mismatch patients.

library(dplyr)

analysis_ST <- patient_summary_ST %>%
  filter(
    ST_relationship %in% c("ST match", "ST mismatch"),
    !is.na(Minimum_Distance)
  ) %>%
  mutate(
    ST_relationship = factor(
      ST_relationship,
      levels = c("ST mismatch", "ST match")
    )
  )

analysis_ST

# Check Sample size

table(analysis_ST$ST_relationship)

# Inspect Discriptive stats

analysis_ST %>%
  group_by(ST_relationship) %>%
  summarise(
    n = n(),
    median = median(Minimum_Distance),
    Q1 = quantile(Minimum_Distance, 0.25),
    Q3 = quantile(Minimum_Distance, 0.75),
    IQR = IQR(Minimum_Distance),
    min = min(Minimum_Distance),
    max = max(Minimum_Distance),
    .groups = "drop"
  )

# Wilcoxon test
wilcox_result <- wilcox.test(
  Minimum_Distance ~ ST_relationship,
  data = analysis_ST,
  alternative = "two.sided",
  exact = FALSE
)

wilcox_result

# Calculate effet size

install.packages("rstatix")
install.packages('coin')
library(rstatix)
library(coin)

cliff_result <- analysis_ST %>%
  wilcox_effsize(
    Minimum_Distance ~ ST_relationship
  )

cliff_result   # This gives estimated direction and magnitude of the difference


#  Compelet statistical sumary in one object

statistical_summary <- analysis_ST %>%
  group_by(ST_relationship) %>%
  summarise(
    n = n(),
    median_min_distance = median(Minimum_Distance),
    Q1_min_distance = quantile(Minimum_Distance, 0.25),
    Q3_min_distance = quantile(Minimum_Distance, 0.75),
    IQR_min_distance = IQR(Minimum_Distance),
    .groups = "drop"
  )

statistical_summary

cat(
  "Wilcoxon rank-sum test\n",
  "W =", wilcox_result$statistic,
  "\nP =", format.pval(
    wilcox_result$p.value,
    digits = 3
  ),
  "\n"
)

#Assess whether minimum distance is associated with the number of NPS isolates:

cor.test(
  analysis_ST$Minimum_Distance,
  analysis_ST$n_NPS,
  method = "spearman",
  exact = FALSE
)


table(analysis_ST$ST_relationship)

statistical_summary

wilcox_result

cliff_result