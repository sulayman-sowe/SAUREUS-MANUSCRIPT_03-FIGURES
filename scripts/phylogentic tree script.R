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

tree_meta <- tree_meta %>%
  mutate(
    CC_plot = case_when(
      Tree_Group == "Outgroup" ~ "Outgroup",
      is.na(Clonal_Complex) ~ "Missing",
      Clonal_Complex == "SINGLETON" ~ "Singleton",
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

table(tree_meta$CC_plot, useNA = "ifany") # Check

# Basic cc attched tree
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

# Support values added version
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