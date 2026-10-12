
# S. aureus ISOLATE FLOWCHART



# 1. INSTALL / LOAD REQUIRED PACKAGES

library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)



# 2. DEFINE STUDY NUMBERS


n_total <- 386
n_metadata <- 361
n_sequenced <- 284
n_qc <- 262
n_final <- 142

excluded_metadata <- 25
excluded_preseq <- 77
excluded_sequencing <- 22
excluded_incomplete <- 120



# 3. CHECK THE SAMPLE FLOW

# These checks ensure that the numbers in the figure remain internally consistent if you modify them later.

stopifnot(
  n_total - excluded_metadata == n_metadata,
  n_metadata - excluded_preseq == n_sequenced,
  n_sequenced - excluded_sequencing == n_qc,
  n_qc - excluded_incomplete == n_final
)



# 4. DEFINE COLOUR PALETTE

col_main <- "#DCEAF7"        # Pale scientific blue
col_main_dark <- "#BFD8EE"   # Slightly stronger blue
col_excluded <- "#F4D9DC"    # Muted pale red
col_final <- "#C8E6C9"       # Soft green
col_border <- "#4A4A4A"      # Dark grey
col_arrow <- "#777777"       # Medium grey
col_text <- "#1F1F1F"        # Near-black


# 5. DEFINE GRAPHVIZ / DOT FLOWCHART

# Helvetica is used because it is broadly available and has good compatibility with Graphviz and scientific figures.

# rankdir = TB means: Top -> Bottom

dot_code <- paste0('
digraph study_flow {

  # GLOBAL GRAPH SETTINGS

  graph [
    rankdir = TB,
    bgcolor = "white",

    # Overall spacing
    nodesep = 0.55,
    ranksep = 0.55,

    # Margins
    margin = 0.05,
    pad = 0.05,

    # Smoother routing
    splines = ortho,

    # Use consistent font
    fontname = "Helvetica"
  ]
  
  # GLOBAL NODE SETTINGS

  node [
    shape = box,
    style = "rounded,filled",

    fontname = "Helvetica",
    fontsize = 11,

    fontcolor = "', col_text, '",

    color = "', col_border, '",
    penwidth = 1.2,

    margin = "0.18,0.12",

    width = 3.55,
    height = 0.85
  ]
  
  # GLOBAL EDGE SETTINGS
  
  edge [
    color = "', col_arrow, '",
    penwidth = 1.25,

    arrowsize = 0.7,

    fontname = "Helvetica",
    fontsize = 9
  ]

  # MAIN STUDY PATHWAY
  
  # NODE 1: INITIAL COLLECTION
 
  n1 [
    fillcolor = "', col_main, '",

    label = <
      <TABLE BORDER="0" CELLBORDER="0" CELLPADDING="1">
        <TR>
          <TD>
            <FONT POINT-SIZE="8" COLOR="#666666">
              COLLECTION
            </FONT>
          </TD>
        </TR>

        <TR>
          <TD>
            <FONT POINT-SIZE="11">
              <B><I>S. aureus</I>  isolates collected</B>
            </FONT>
          </TD>
        </TR>

        <TR>
          <TD>
            <FONT POINT-SIZE="14">
              <B>n = ', n_total, '</B>
            </FONT>
          </TD>
        </TR>
      </TABLE>
    >
  ]
 
  # NODE 2: COMPLETE METADATA
  
  n2 [
    fillcolor = "', col_main, '",

    label = <
      <TABLE BORDER="0" CELLBORDER="0" CELLPADDING="1">
        <TR>
          <TD>
            <FONT POINT-SIZE="8" COLOR="#666666">
              ELIGIBILITY
            </FONT>
          </TD>
        </TR>

        <TR>
          <TD>
            <FONT POINT-SIZE="11">
              <B>Complete metadata</B>
            </FONT>
          </TD>
        </TR>

        <TR>
          <TD>
            <FONT POINT-SIZE="14">
              <B>n = ', n_metadata, '</B>
            </FONT>
          </TD>
        </TR>
      </TABLE>
    >
  ]


  
  # NODE 3: SEQUENCING

  n3 [
    fillcolor = "', col_main, '",

    label = <
      <TABLE BORDER="0" CELLBORDER="0" CELLPADDING="1">
        <TR>
          <TD>
            <FONT POINT-SIZE="8" COLOR="#666666">
              WHOLE-GENOME SEQUENCING
            </FONT>
          </TD>
        </TR>

        <TR>
          <TD>
            <FONT POINT-SIZE="11">
              <B>Whole-genome sequencing performed</B>
            </FONT>
          </TD>
        </TR>

        <TR>
          <TD>
            <FONT POINT-SIZE="14">
              <B>n = ', n_sequenced, '</B>
            </FONT>
          </TD>
        </TR>
      </TABLE>
    >
  ]

  # NODE 4: SEQUENCING QC
  
  n4 [
    fillcolor = "', col_main_dark, '",

    label = <
      <TABLE BORDER="0" CELLBORDER="0" CELLPADDING="1">
        <TR>
          <TD>
            <FONT POINT-SIZE="8" COLOR="#666666">
              QUALITY CONTROL
            </FONT>
          </TD>
        </TR>

        <TR>
          <TD>
            <FONT POINT-SIZE="11">
              <B>Sequencing QC passed</B>
            </FONT>
          </TD>
        </TR>

        <TR>
          <TD>
            <FONT POINT-SIZE="14">
              <B>n = ', n_qc, '</B>
            </FONT>
          </TD>
        </TR>
      </TABLE>
    >
  ]
  
  # NODE 5: FINAL ANALYTICAL COHORT
 
  n5 [
    fillcolor = "', col_final, '",

    color = "#4F7F52",
    penwidth = 1.6,

    width = 3.65,
    height = 1.05,

    label = <
      <TABLE BORDER="0" CELLBORDER="0" CELLPADDING="1">
        <TR>
          <TD>
            <FONT POINT-SIZE="8" COLOR="#4F7F52">
              FINAL ANALYSIS
            </FONT>
          </TD>
        </TR>

        <TR>
          <TD>
            <FONT POINT-SIZE="12">
              <B>Final analytical cohort</B>
            </FONT>
          </TD>
        </TR>

        <TR>
          <TD>
            <FONT POINT-SIZE="15">
              <B>n = ', n_final, '</B>
            </FONT>
          </TD>
        </TR>

        <TR>
          <TD>
            <FONT POINT-SIZE="8" COLOR="#555555">
              Downstream genomic and clinical analyses
            </FONT>
          </TD>
        </TR>
      </TABLE>
    >
  ]

 
  # EXCLUSION NODES

  # EXCLUSION 1

  e1 [
    shape = box,
    style = "rounded,filled",

    fillcolor = "', col_excluded, '",

    color = "#9A6268",
    penwidth = 1.0,

    width = 3.10,
    height = 0.78,

    label = <
      <TABLE BORDER="0" CELLBORDER="0" CELLPADDING="1">

        <TR>
          <TD>
            <FONT POINT-SIZE="8" COLOR="#7A4B51">
              EXCLUDED
            </FONT>
          </TD>
        </TR>

        <TR>
          <TD>
            <FONT POINT-SIZE="10">
              Missing metadata
            </FONT>
          </TD>
        </TR>

        <TR>
          <TD>
            <FONT POINT-SIZE="12">
              <B>n = ', excluded_metadata, '</B>
            </FONT>
          </TD>
        </TR>

      </TABLE>
    >
  ]

  # EXCLUSION 2

  e2 [
    shape = box,
    style = "rounded,filled",

    fillcolor = "', col_excluded, '",

    color = "#9A6268",
    penwidth = 1.0,

    width = 3.10,
    height = 0.90,

    label = <
      <TABLE BORDER="0" CELLBORDER="0" CELLPADDING="1">

        <TR>
          <TD>
            <FONT POINT-SIZE="8" COLOR="#7A4B51">
              EXCLUDED
            </FONT>
          </TD>
        </TR>

        <TR>
          <TD>
            <FONT POINT-SIZE="10">
              Negative culture /<BR/>
              failed DNA extraction
            </FONT>
          </TD>
        </TR>

        <TR>
          <TD>
            <FONT POINT-SIZE="12">
              <B>n = ', excluded_preseq, '</B>
            </FONT>
          </TD>
        </TR>

      </TABLE>
    >
  ]

  # EXCLUSION 3

  e3 [
    shape = box,
    style = "rounded,filled",

    fillcolor = "', col_excluded, '",

    color = "#9A6268",
    penwidth = 1.0,

    width = 3.10,
    height = 0.78,

    label = <
      <TABLE BORDER="0" CELLBORDER="0" CELLPADDING="1">

        <TR>
          <TD>
            <FONT POINT-SIZE="8" COLOR="#7A4B51">
              EXCLUDED
            </FONT>
          </TD>
        </TR>

        <TR>
          <TD>
            <FONT POINT-SIZE="10">
              Sequencing failure
            </FONT>
          </TD>
        </TR>

        <TR>
          <TD>
            <FONT POINT-SIZE="12">
              <B>n = ', excluded_sequencing, '</B>
            </FONT>
          </TD>
        </TR>

      </TABLE>
    >
  ]

  # EXCLUSION 4

  e4 [
    shape = box,
    style = "rounded,filled",

    fillcolor = "', col_excluded, '",

    color = "#9A6268",
    penwidth = 1.0,

    width = 3.10,
    height = 0.90,

    label = <
      <TABLE BORDER="0" CELLBORDER="0" CELLPADDING="1">

        <TR>
          <TD>
            <FONT POINT-SIZE="8" COLOR="#7A4B51">
              EXCLUDED
            </FONT>
          </TD>
        </TR>

        <TR>
          <TD>
            <FONT POINT-SIZE="10">
              Incomplete genomic data 
            </FONT>
          </TD>
        </TR>

        <TR>
          <TD>
            <FONT POINT-SIZE="12">
              <B>n = ', excluded_incomplete, '</B>
            </FONT>
          </TD>
        </TR>

      </TABLE>
    >
  ]

  # MAIN VERTICAL CONNECTIONS

  n1 -> n2 [
    color = "', col_arrow, '",
    penwidth = 1.4
  ]

  n2 -> n3 [
    color = "', col_arrow, '",
    penwidth = 1.4
  ]

  n3 -> n4 [
    color = "', col_arrow, '",
    penwidth = 1.4
  ]

  n4 -> n5 [
    color = "', col_arrow, '",
    penwidth = 1.4
  ]

  # EXCLUSION CONNECTIONS

  n1 -> e1 [
    color = "', col_arrow, '",
    penwidth = 1.1
  ]

  n2 -> e2 [
    color = "', col_arrow, '",
    penwidth = 1.1
  ]

  n3 -> e3 [
    color = "', col_arrow, '",
    penwidth = 1.1
  ]

  n4 -> e4 [
    color = "', col_arrow, '",
    penwidth = 1.1
  ]

  # FORCE EACH EXCLUSION NODE TO THE RIGHT OF ITS MAIN NODE

  { rank = same; n1; e1 }
  { rank = same; n2; e2 }
  { rank = same; n3; e3 }
  { rank = same; n4; e4 }


  # Invisible edges preserve the vertical ordering without introducing additional visual lines.

  e1 -> e2 [
    style = invis,
    weight = 10
  ]

  e2 -> e3 [
    style = invis,
    weight = 10
  ]

  e3 -> e4 [
    style = invis,
    weight = 10
  ]

}
')


# 6. CREATE THE FLOWCHART


flowchart <- grViz(dot_code)


flowchart



# 8. CREATE OUTPUT DIRECTORY

output_dir <- "S_aureus_flowchart_output"

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}


# 9. EXPORT AS SVG


svg_file <- file.path(output_dir,"S_aureus_isolate_flowchart.svg")

svg_code <- export_svg(flowchart)

writeLines(svg_code,con = svg_file)


# 10. EXPORT AS PDF


pdf_file <- file.path(
  output_dir,
  "S_aureus_isolate_flowchart.pdf"
)

rsvg_pdf(
  charToRaw(svg_code),
  file = pdf_file
)




# 11. EXPORT AS HIGH-RESOLUTION PNG

png_file <- file.path(
  output_dir,
  "S_aureus_isolate_flowchart_600dpi.png"
)

rsvg_png(
  charToRaw(svg_code),
  file = png_file,
  width = 4200,
  height = 5400
)


# Alternative structural designed flowchart

dot_code <- paste0('
digraph study_flow {

  # GLOBAL GRAPH SETTINGS

  graph [
    rankdir = TB,
    bgcolor = "white",

    # Overall spacing
    nodesep = 0.55,
    ranksep = 0.55,

    # Margins
    margin = 0.05,
    pad = 0.05,

    # Smoother routing
    splines = polyline,
    
    ordering = out,

    # Use consistent font
    fontname = "Helvetica"
  ]
  
  # GLOBAL NODE SETTINGS

  node [
    shape = box,
    style = "rounded,filled",

    fontname = "Helvetica",
    fontsize = 11,

    fontcolor = "', col_text, '",

    color = "', col_border, '",
    penwidth = 1.2,

    margin = "0.18,0.12",

    width = 3.55,
    height = 0.85
  ]
  
  # GLOBAL EDGE SETTINGS
  
  edge [
    color = "', col_arrow, '",
    penwidth = 1.25,

    arrowsize = 0.7,

    fontname = "Helvetica",
    fontsize = 9
  ]

  # MAIN STUDY PATHWAY
  
  # NODE 1: INITIAL COLLECTION
 
  n1 [
    fillcolor = "', col_main, '",

    label = <
      <TABLE BORDER="0" CELLBORDER="0" CELLPADDING="1">
        <TR>
          <TD>
            <FONT POINT-SIZE="8" COLOR="#666666">
              COLLECTION
            </FONT>
          </TD>
        </TR>

        <TR>
          <TD>
            <FONT POINT-SIZE="11">
              <B><I>S. aureus</I><FONT POINT-SIZE="11">&#160;</FONT>isolates collected</B>
            </FONT>
          </TD>
        </TR>

        <TR>
          <TD>
            <FONT POINT-SIZE="14">
              <B>n = ', n_total, '</B>
            </FONT>
          </TD>
        </TR>
      </TABLE>
    >
  ]
 
  # NODE 2: COMPLETE METADATA
  
  n2 [
    fillcolor = "', col_main, '",

    label = <
      <TABLE BORDER="0" CELLBORDER="0" CELLPADDING="1">
        <TR>
          <TD>
            <FONT POINT-SIZE="8" COLOR="#666666">
              ELIGIBILITY
            </FONT>
          </TD>
        </TR>

        <TR>
          <TD>
            <FONT POINT-SIZE="11">
              <B>Complete metadata</B>
            </FONT>
          </TD>
        </TR>

        <TR>
          <TD>
            <FONT POINT-SIZE="14">
              <B>n = ', n_metadata, '</B>
            </FONT>
          </TD>
        </TR>
      </TABLE>
    >
  ]


  
  # NODE 3: SEQUENCING

  n3 [
    fillcolor = "', col_main, '",

    label = <
      <TABLE BORDER="0" CELLBORDER="0" CELLPADDING="1">
        <TR>
          <TD>
            <FONT POINT-SIZE="8" COLOR="#666666">
              WHOLE-GENOME SEQUENCING
            </FONT>
          </TD>
        </TR>

        <TR>
          <TD>
            <FONT POINT-SIZE="11">
              <B>Whole-genome sequencing performed</B>
            </FONT>
          </TD>
        </TR>

        <TR>
          <TD>
            <FONT POINT-SIZE="14">
              <B>n = ', n_sequenced, '</B>
            </FONT>
          </TD>
        </TR>
      </TABLE>
    >
  ]

  # NODE 4: SEQUENCING QC
  
  n4 [
    fillcolor = "', col_main_dark, '",

    label = <
      <TABLE BORDER="0" CELLBORDER="0" CELLPADDING="1">
        <TR>
          <TD>
            <FONT POINT-SIZE="8" COLOR="#666666">
              QUALITY CONTROL
            </FONT>
          </TD>
        </TR>

        <TR>
          <TD>
            <FONT POINT-SIZE="11">
              <B>Sequencing QC passed</B>
            </FONT>
          </TD>
        </TR>

        <TR>
          <TD>
            <FONT POINT-SIZE="14">
              <B>n = ', n_qc, '</B>
            </FONT>
          </TD>
        </TR>
      </TABLE>
    >
  ]
  
  # NODE 5: FINAL ANALYTICAL COHORT
 
  n5 [
    fillcolor = "', col_final, '",

    color = "#4F7F52",
    penwidth = 1.6,

    width = 3.65,
    height = 1.05,

    label = <
      <TABLE BORDER="0" CELLBORDER="0" CELLPADDING="1">
        <TR>
          <TD>
            <FONT POINT-SIZE="8" COLOR="#4F7F52">
              FINAL ANALYSIS
            </FONT>
          </TD>
        </TR>

        <TR>
          <TD>
            <FONT POINT-SIZE="12">
              <B>Final analytical cohort</B>
            </FONT>
          </TD>
        </TR>

        <TR>
          <TD>
            <FONT POINT-SIZE="15">
              <B>n = ', n_final, '</B>
            </FONT>
          </TD>
        </TR>

        <TR>
          <TD>
            <FONT POINT-SIZE="8" COLOR="#555555">
              Downstream genomic and clinical analyses
            </FONT>
          </TD>
        </TR>
      </TABLE>
    >
  ]

# JUNCTION POINTS FOR EXCLUSION BRANCHES

# ============================================================
# JUNCTION POINTS
# ============================================================

j1 [
  shape = point,
  width = 0.06,
  height = 0.06,
  label = "",
  color = "grey45",
  fillcolor = "grey45"
]

j2 [
  shape = point,
  width = 0.06,
  height = 0.06,
  label = "",
  color = "grey45",
  fillcolor = "grey45"
]

j3 [
  shape = point,
  width = 0.06,
  height = 0.06,
  label = "",
  color = "grey45",
  fillcolor = "grey45"
]

j4 [
  shape = point,
  width = 0.06,
  height = 0.06,
  label = "",
  color = "grey45",
  fillcolor = "grey45"
]

# EXCLUSION NODES


e1 [
  shape = box,
  style = "rounded,filled",

  fillcolor = "', col_excluded, '",
  color = "#9A6268",
  penwidth = 1.0,

  width = 3.15,
  height = 0.78,

  label = <
    <TABLE BORDER="0" CELLBORDER="0" CELLPADDING="1">

      <TR>
        <TD>
          <FONT POINT-SIZE="8" COLOR="#7A4B51">
            EXCLUDED
          </FONT>
        </TD>
      </TR>

      <TR>
        <TD>
          <FONT POINT-SIZE="10">
            Missing metadata
          </FONT>
        </TD>
      </TR>

      <TR>
        <TD>
          <FONT POINT-SIZE="12">
            <B>n = ', excluded_metadata, '</B>
          </FONT>
        </TD>
      </TR>

    </TABLE>
  >
]


e2 [
  shape = box,
  style = "rounded,filled",

  fillcolor = "', col_excluded, '",
  color = "#9A6268",
  penwidth = 1.0,

  width = 3.15,
  height = 0.92,

  label = <
    <TABLE BORDER="0" CELLBORDER="0" CELLPADDING="1">

      <TR>
        <TD>
          <FONT POINT-SIZE="8" COLOR="#7A4B51">
            EXCLUDED
          </FONT>
        </TD>
      </TR>

      <TR>
        <TD>
          <FONT POINT-SIZE="10">
            Negative culture /<BR/>
            failed DNA extraction
          </FONT>
        </TD>
      </TR>

      <TR>
        <TD>
          <FONT POINT-SIZE="12">
            <B>n = ', excluded_preseq, '</B>
          </FONT>
        </TD>
      </TR>

    </TABLE>
  >
]


e3 [
  shape = box,
  style = "rounded,filled",

  fillcolor = "', col_excluded, '",
  color = "#9A6268",
  penwidth = 1.0,

  width = 3.15,
  height = 0.78,

  label = <
    <TABLE BORDER="0" CELLBORDER="0" CELLPADDING="1">

      <TR>
        <TD>
          <FONT POINT-SIZE="8" COLOR="#7A4B51">
            EXCLUDED
          </FONT>
        </TD>
      </TR>

      <TR>
        <TD>
          <FONT POINT-SIZE="10">
            Sequencing failure
          </FONT>
        </TD>
      </TR>

      <TR>
        <TD>
          <FONT POINT-SIZE="12">
            <B>n = ', excluded_sequencing, '</B>
          </FONT>
        </TD>
      </TR>

    </TABLE>
  >
]


e4 [
  shape = box,
  style = "rounded,filled",

  fillcolor = "', col_excluded, '",
  color = "#9A6268",
  penwidth = 1.0,

  width = 3.15,
  height = 0.92,

  label = <
    <TABLE BORDER="0" CELLBORDER="0" CELLPADDING="1">

      <TR>
        <TD>
          <FONT POINT-SIZE="8" COLOR="#7A4B51">
            EXCLUDED
          </FONT>
        </TD>
      </TR>

      <TR>
        <TD>
          <FONT POINT-SIZE="10">
            Incomplete genomic data
          </FONT>
        </TD>
      </TR>

      <TR>
        <TD>
          <FONT POINT-SIZE="12">
            <B>n = ', excluded_incomplete, '</B>
          </FONT>
        </TD>
      </TR>

    </TABLE>
  >
]

# MAIN PATHWAY + EXCLUSION BRANCHES



# MAIN PATHWAY


n1 -> j1 [
  color = "grey45",
  penwidth = 1.3,
  arrowsize = 0.65
]

j1 -> n2 [
  color = "grey45",
  penwidth = 1.3,
  arrowsize = 0.70
]


n2 -> j2 [
  color = "grey45",
  penwidth = 1.3,
  arrowsize = 0.65
]

j2 -> n3 [
  color = "grey45",
  penwidth = 1.3,
  arrowsize = 0.70
]


n3 -> j3 [
  color = "grey45",
  penwidth = 1.3,
  arrowsize = 0.65
]

j3 -> n4 [
  color = "grey45",
  penwidth = 1.3,
  arrowsize = 0.70
]


n4 -> j4 [
  color = "grey45",
  penwidth = 1.3,
  arrowsize = 0.65
]

j4 -> n5 [
  color = "grey45",
  penwidth = 1.3,
  arrowsize = 0.70
]



# EXCLUSION BRANCHES


j1 -> e1 [
  color = "grey45",
  penwidth = 1.15,
  arrowsize = 0.65,
  constraint = false
]

j2 -> e2 [
  color = "grey45",
  penwidth = 1.15,
  arrowsize = 0.65,
  constraint = false
]

j3 -> e3 [
  color = "grey45",
  penwidth = 1.15,
  arrowsize = 0.65,
  constraint = false
]

j4 -> e4 [
  color = "grey45",
  penwidth = 1.15,
  arrowsize = 0.65,
  constraint = false
]



# FORCE EACH EXCLUSION BOX TO THE RIGHT


{ rank = same; j1; e1 }
{ rank = same; j2; e2 }
{ rank = same; j3; e3 }
{ rank = same; j4; e4 }



# MAINTAIN EXCLUSION-BOX ORDER


e1 -> e2 [
  style = invis,
  weight = 10
]

e2 -> e3 [
  style = invis,
  weight = 10
]

e3 -> e4 [
  style = invis,
  weight = 10
]
}
')

# 6. CREATE THE FLOWCHART


flowchart <- grViz(dot_code)


flowchart



# 8. CREATE OUTPUT DIRECTORY

output_dir <- "S_aureus_flowchart_output"

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}


# 9. EXPORT AS SVG


svg_file <- file.path(output_dir,"S_aureus_isolate_flowchart.svg")

svg_code <- export_svg(flowchart)

writeLines(svg_code,con = svg_file)


# 10. EXPORT AS PDF


pdf_file <- file.path(
  output_dir,
  "S_aureus_isolate_flowchart.pdf"
)

rsvg_pdf(
  charToRaw(svg_code),
  file = pdf_file
)


# 11. EXPORT AS HIGH-RESOLUTION PNG

png_file <- file.path(
  output_dir,
  "S_aureus_isolate_flowchart_600dpi.png"
)

rsvg_png(
  charToRaw(svg_code),
  file = png_file,
  width = 4200,
  height = 5400
)



