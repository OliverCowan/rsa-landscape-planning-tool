# =============================================================================
# GENERATE VEGETATION TYPE LOOKUP SPREADSHEET
# =============================================================================
#
# PURPOSE:
#   Creates a searchable Excel reference table of all South African vegetation
#   types with RLE threat status, conservation targets, and key attributes.
#   Useful for identifying the correct T_MAPCODE for offset workflow input.
#
# OUTPUT:
#   Biodiversity_Offset_Workflow/input/veg_type_lookup.xlsx
#
# =============================================================================

library(foreign)
library(dplyr)
library(openxlsx)

# =============================================================================
# PATHS
# =============================================================================

base_dir   <- "C:/Users/OliverCowan/OneDrive - EWT/Documents/02. Data Analyses/03. JRS 30x30/R Workflow_30x30"
offset_dir <- file.path(base_dir, "Biodiversity_Offset_Workflow")
input_dir  <- file.path(offset_dir, "input")

nvm_dbf <- file.path(input_dir, "NVM2024/NVM2024Final_IEM5_12_07012025.dbf")
rle_dbf <- file.path(input_dir, "RLE2021/RLE_Terr_2021_June2021_Remnants_ddw.dbf")
out_path <- file.path(input_dir, "veg_type_lookup.xlsx")

# =============================================================================
# LOAD AND PROCESS
# =============================================================================

cat("Loading NVM 2024...\n")
nvm <- read.dbf(nvm_dbf, as.is = TRUE)

cat("Loading RLE 2021...\n")
rle <- read.dbf(rle_dbf, as.is = TRUE) %>%
  select(MAPCODE18, RLE2021) %>%
  distinct(MAPCODE18, .keep_all = TRUE)

# Summarise NVM to one row per veg type
# Calculate total potential extent from Shape_Area (m²) → km²
cat("Summarising NVM to one row per veg type...\n")
nvm_summary <- nvm %>%
  filter(!is.na(T_MAPCODE), T_MAPCODE != "") %>%
  filter(T_CNSRV_TR > 0) %>%   # exclude non-terrestrial
  group_by(T_MAPCODE) %>%
  summarise(
    Veg_Type_Name      = first(T_Name),
    Biome              = first(T_BIOME),
    Bioregion          = first(T_BIOREGIO),
    Conservation_Target_Pct = first(T_CNSRV_TR),
    Total_Extent_km2   = round(sum(Shape_Area, na.rm = TRUE) / 1e6, 1),
    N_Polygons         = n(),
    .groups = "drop"
  ) %>%
  arrange(Biome, Veg_Type_Name)

# Join RLE status
cat("Joining RLE 2021 threat status...\n")
lookup <- nvm_summary %>%
  left_join(rle, by = c("T_MAPCODE" = "MAPCODE18")) %>%
  mutate(
    RLE_Status = case_when(
      RLE2021 == "CR" ~ "CR — Critically Endangered",
      RLE2021 == "EN" ~ "EN — Endangered",
      RLE2021 == "VU" ~ "VU — Vulnerable",
      RLE2021 == "LC" ~ "LC — Least Concern",
      RLE2021 == "NE" ~ "NE — Not Evaluated",
      is.na(RLE2021)  ~ "No match in RLE 2021",
      TRUE            ~ RLE2021
    ),
    RLE_Code = ifelse(is.na(RLE2021), "NM", RLE2021),
    Offset_Feasibility = case_when(
      RLE2021 == "CR" ~ "Caution — consult authority",
      RLE2021 == "EN" ~ "Feasible with conditions",
      RLE2021 == "VU" ~ "Generally feasible",
      RLE2021 == "LC" ~ "Generally feasible",
      TRUE            ~ "Unknown"
    )
  ) %>%
  select(
    Veg_Type_Name,
    MAPCODE     = T_MAPCODE,
    Biome,
    Bioregion,
    RLE_Status,
    RLE_Code,
    Conservation_Target_Pct,
    Total_Extent_km2,
    N_Polygons,
    Offset_Feasibility
  )

cat(sprintf("Total veg types in lookup: %d\n\n", nrow(lookup)))

# =============================================================================
# BUILD EXCEL WORKBOOK
# =============================================================================

cat("Building Excel workbook...\n")

wb <- createWorkbook()

# Styles
style_header <- createStyle(
  fontName  = "Arial",
  fontSize  = 10,
  fontColour = "#FFFFFF",
  fgFill    = "#1B5E20",
  halign    = "CENTER",
  valign    = "CENTER",
  textDecoration = "BOLD",
  border    = "Bottom",
  borderColour = "#FFFFFF",
  wrapText  = TRUE
)

style_body <- createStyle(
  fontName = "Arial",
  fontSize = 10,
  border   = "TopBottomLeftRight",
  borderColour = "#E0E0E0"
)

style_CR <- createStyle(
  fontName  = "Arial",
  fontSize  = 10,
  fontColour = "#FFFFFF",
  fgFill    = "#B71C1C"
)
style_EN <- createStyle(
  fontName  = "Arial",
  fontSize  = 10,
  fgFill    = "#FF8F00"
)
style_VU <- createStyle(
  fontName  = "Arial",
  fontSize  = 10,
  fgFill    = "#FFF9C4"
)
style_alt <- createStyle(
  fontName = "Arial",
  fontSize = 10,
  fgFill   = "#F5F5F5"
)

# =============================================================================
# SHEET 1: FULL LOOKUP (all veg types, sortable)
# =============================================================================

addWorksheet(wb, "Veg Type Lookup")
ws <- "Veg Type Lookup"

# Write data
writeData(wb, ws, lookup, startRow = 2, startCol = 1,
          headerStyle = style_header, borders = "all",
          borderColour = "#E0E0E0")

# Title row
mergeCells(wb, ws, cols = 1:10, rows = 1)
writeData(wb, ws, "South Africa — Vegetation Type Reference Lookup (NVM 2024 + RLE 2021)",
          startRow = 1, startCol = 1)
addStyle(wb, ws, createStyle(
  fontName = "Arial", fontSize = 12, fontColour = "#FFFFFF",
  fgFill = "#1B5E20", textDecoration = "BOLD",
  halign = "CENTER", valign = "CENTER"
), rows = 1, cols = 1:10, gridExpand = TRUE)
setRowHeights(wb, ws, rows = 1, heights = 28)
setRowHeights(wb, ws, rows = 2, heights = 36)

# Alternating row colours
n_rows <- nrow(lookup)
for (i in seq_len(n_rows)) {
  row_num <- i + 2
  if (i %% 2 == 0) {
    addStyle(wb, ws, style_alt, rows = row_num, cols = 1:10, gridExpand = TRUE)
  }
}

# RLE threat status conditional colours
for (i in seq_len(n_rows)) {
  row_num <- i + 2
  rle_code <- lookup$RLE_Code[i]
  if (!is.na(rle_code)) {
    if (rle_code == "CR") {
      addStyle(wb, ws, style_CR, rows = row_num, cols = 5, stack = TRUE)
    } else if (rle_code == "EN") {
      addStyle(wb, ws, style_EN, rows = row_num, cols = 5, stack = TRUE)
    } else if (rle_code == "VU") {
      addStyle(wb, ws, style_VU, rows = row_num, cols = 5, stack = TRUE)
    }
  }
}

# Column widths
setColWidths(wb, ws, cols = 1,  widths = 40)  # Veg Type Name
setColWidths(wb, ws, cols = 2,  widths = 12)  # MAPCODE
setColWidths(wb, ws, cols = 3,  widths = 20)  # Biome
setColWidths(wb, ws, cols = 4,  widths = 28)  # Bioregion
setColWidths(wb, ws, cols = 5,  widths = 28)  # RLE Status
setColWidths(wb, ws, cols = 6,  widths = 10)  # RLE Code
setColWidths(wb, ws, cols = 7,  widths = 16)  # Target %
setColWidths(wb, ws, cols = 8,  widths = 16)  # Extent km²
setColWidths(wb, ws, cols = 9,  widths = 12)  # N Polygons
setColWidths(wb, ws, cols = 10, widths = 26)  # Offset Feasibility

# Freeze top rows and enable autofilter
freezePane(wb, ws, firstActiveRow = 3, firstActiveCol = 1)
addFilter(wb, ws, rows = 2, cols = 1:10)

# =============================================================================
# SHEET 2: THREATENED ONLY (CR, EN, VU)
# =============================================================================

addWorksheet(wb, "Threatened Types Only")
ws2 <- "Threatened Types Only"

threatened <- lookup %>%
  filter(RLE_Code %in% c("CR", "EN", "VU")) %>%
  arrange(RLE_Code, Veg_Type_Name)

mergeCells(wb, ws2, cols = 1:10, rows = 1)
writeData(wb, ws2, "Threatened Vegetation Types — CR / EN / VU (RLE 2021)",
          startRow = 1, startCol = 1)
addStyle(wb, ws2, createStyle(
  fontName = "Arial", fontSize = 12, fontColour = "#FFFFFF",
  fgFill = "#B71C1C", textDecoration = "BOLD",
  halign = "CENTER", valign = "CENTER"
), rows = 1, cols = 1:10, gridExpand = TRUE)
setRowHeights(wb, ws2, rows = 1, heights = 28)
setRowHeights(wb, ws2, rows = 2, heights = 36)

writeData(wb, ws2, threatened, startRow = 2, startCol = 1,
          headerStyle = style_header, borders = "all",
          borderColour = "#E0E0E0")

# RLE colours on threatened sheet
for (i in seq_len(nrow(threatened))) {
  row_num <- i + 2
  rle_code <- threatened$RLE_Code[i]
  if (rle_code == "CR") {
    addStyle(wb, ws2, style_CR, rows = row_num, cols = 5, stack = TRUE)
  } else if (rle_code == "EN") {
    addStyle(wb, ws2, style_EN, rows = row_num, cols = 5, stack = TRUE)
  } else if (rle_code == "VU") {
    addStyle(wb, ws2, style_VU, rows = row_num, cols = 5, stack = TRUE)
  }
}

setColWidths(wb, ws2, cols = 1,  widths = 40)
setColWidths(wb, ws2, cols = 2,  widths = 12)
setColWidths(wb, ws2, cols = 3,  widths = 20)
setColWidths(wb, ws2, cols = 4,  widths = 28)
setColWidths(wb, ws2, cols = 5,  widths = 28)
setColWidths(wb, ws2, cols = 6,  widths = 10)
setColWidths(wb, ws2, cols = 7,  widths = 16)
setColWidths(wb, ws2, cols = 8,  widths = 16)
setColWidths(wb, ws2, cols = 9,  widths = 12)
setColWidths(wb, ws2, cols = 10, widths = 26)

freezePane(wb, ws2, firstActiveRow = 3, firstActiveCol = 1)
addFilter(wb, ws2, rows = 2, cols = 1:10)

# =============================================================================
# SHEET 3: INSTRUCTIONS / KEY
# =============================================================================

addWorksheet(wb, "How to Use")
ws3 <- "How to Use"

instructions <- data.frame(
  Section = c(
    "PURPOSE",
    "",
    "HOW TO FIND YOUR VEG TYPE",
    "",
    "",
    "",
    "USING WITH OFFSET WORKFLOW",
    "",
    "RLE STATUS KEY",
    "",
    "",
    "",
    "",
    "CONSERVATION TARGET",
    "",
    "DATA SOURCES"
  ),
  Detail = c(
    "This spreadsheet is a searchable reference for all South African vegetation types from NVM 2024, joined with Red List of Ecosystems (RLE) threat status.",
    "",
    "1. Go to 'Veg Type Lookup' sheet",
    "2. Use Ctrl+F to search by vegetation name (e.g. 'Soweto Highveld Grassland')",
    "3. Or use the dropdown filters on any column to filter by Biome, Threat Status etc.",
    "4. Copy the MAPCODE value for use in Script 01 of the Offset Workflow",
    "Set TARGET_MAPCODE in Script 01 to the MAPCODE value from this table.",
    "The conservation target % is automatically extracted from this table by the script.",
    "CR = Critically Endangered — offset may not be permissible; consult conservation authority",
    "EN = Endangered — offset feasible with conditions; higher scrutiny required",
    "VU = Vulnerable — generally feasible offset; standard requirements apply",
    "LC = Least Concern — generally feasible; standard requirements apply",
    "NE = Not Evaluated / No RLE match — use professional judgement",
    "The Conservation Target % is sourced from NVM 2024 (T_CNSRV_TR field), based on NBA 2011.",
    "This represents the minimum % of the veg type's potential extent that should be conserved.",
    "NVM 2024: SANBI National Vegetation Map Project (bgis.sanbi.org/vegmap) | RLE 2021: SANBI Red List of Ecosystems"
  )
)

writeData(wb, ws3, instructions, startRow = 2, startCol = 1)

addStyle(wb, ws3, createStyle(
  fontName = "Arial", fontSize = 10, textDecoration = "BOLD",
  fgFill = "#E8F5E9"
), rows = 3:(nrow(instructions)+2), cols = 1, gridExpand = TRUE)

addStyle(wb, ws3, createStyle(
  fontName = "Arial", fontSize = 10
), rows = 3:(nrow(instructions)+2), cols = 2, gridExpand = TRUE)

mergeCells(wb, ws3, cols = 1:2, rows = 1)
writeData(wb, ws3, "How to Use This Lookup Table", startRow = 1, startCol = 1)
addStyle(wb, ws3, createStyle(
  fontName = "Arial", fontSize = 13, textDecoration = "BOLD",
  fontColour = "#FFFFFF", fgFill = "#1B5E20",
  halign = "CENTER", valign = "CENTER"
), rows = 1, cols = 1:2, gridExpand = TRUE)
setRowHeights(wb, ws3, rows = 1, heights = 28)
setColWidths(wb, ws3, cols = 1, widths = 30)
setColWidths(wb, ws3, cols = 2, widths = 80)

# Set sheet tab order / colours
worksheetOrder(wb) <- c(1, 2, 3)

# =============================================================================
# SAVE
# =============================================================================

saveWorkbook(wb, out_path, overwrite = TRUE)
cat(sprintf("\nLookup table saved to:\n  %s\n\n", out_path))
cat(sprintf("Total veg types: %d\n", nrow(lookup)))
cat(sprintf("Threatened (CR/EN/VU): %d\n", nrow(threatened)))
cat(sprintf("No RLE match: %d\n", sum(lookup$RLE_Code == "NM")))
cat("\nDone.\n")
