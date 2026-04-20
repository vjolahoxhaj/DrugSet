# app.R
# Drug Codelist Manager — Shiny app with 4 tabs:
# 1) Prepare and clean input files (ATC cleaning)
# 2) Create a drug codelist (starts with ATC loader & letter picker)
# 3) Update a drug codelist
# 4) Export the full drug codelist

suppressPackageStartupMessages({
  library(shiny)
  library(DT)
  library(dplyr)
  library(readr)
})

# Serve static files from the project folder so images like Scripts/logo.png work in UI
addResourcePath("appdir", projectFolder)

# Helper: safe write to CSV / XLSX if available
write_out <- function(df, file, ext = c("csv", "xlsx")) {
  ext <- match.arg(ext)
  if (ext == "csv") {
    readr::write_csv(df, file)
  } else {
    if (!requireNamespace("writexl", quietly = TRUE)) {
      stop("Package 'writexl' is required for XLSX export. Please install it or choose CSV.")
    }
    writexl::write_xlsx(df, path = file)
  }
}

# Reusable empty codelist scaffold — REQUIRED columns
# drug_abbreviation, coding_system, code, product_name, tags, label, drug_concept
empty_codelist <- tibble::tibble(
  drug_abbreviation     = character(),
  coding_system    = character(),
  code                  = character(),
  product_name          = character(),
  tags                  = character(),
  label                 = character(),
  drug_concept = character()
)

# ATC top-level descriptions (first letter -> system)
ATC_L1_DESC <- c(
  A = "ALIMENTARY TRACT AND METABOLISM",
  B = "BLOOD AND BLOOD FORMING ORGANS",
  C = "CARDIOVASCULAR SYSTEM",
  D = "DERMATOLOGICALS",
  G = "GENITO URINARY SYSTEM AND SEX HORMONES",
  H = "SYSTEMIC HORMONAL PREPARATIONS, EXCL. SEX HORMONES AND INSULINS",
  J = "ANTIINFECTIVES FOR SYSTEMIC USE",
  L = "ANTINEOPLASTIC AND IMMUNOMODULATING AGENTS",
  M = "MUSCULO-SKELETAL SYSTEM",
  N = "NERVOUS SYSTEM",
  P = "ANTIPARASITIC PRODUCTS, INSECTICIDES AND REPELLENTS",
  R = "RESPIRATORY SYSTEM",
  S = "SENSORY ORGANS",
  V = "VARIOUS"
)

# UI ---------------------------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML('
      .app-header{display:flex;align-items:center;gap:16px;padding:10px 12px;border-bottom:1px solid #e5e7eb;}
      .app-header img{height:100px;}
      .app-phrase{font-size:20px;font-weight:600;color:#1f2937;}
    '))
  ),
  div(
    class = "app-header",
    img(src = "appdir/Scripts/logo.png", alt = "DrugSet logo"),
    div(class = "app-phrase", "Create and CrossMap (ATC to PRODCODEs) Drug Codelists")
  ),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Working codelist controls"),
      actionButton("btn_clear_all", "Clear working codelist", icon = icon("broom"), class = "btn-warning"),
      br(), br(),
      downloadButton("download_templates", "Download CSV templates"),
      hr(),
      h4("Add mandatory columns information"),
      div(
        class = "well",
        style = "padding:8px;",
        tags$ul(
          style = "margin-bottom:6px;",
          tags$li(HTML("<b>coding_system</b> is fixed to <code>ATC</code>")),
          tags$li(HTML("<b>tags</b> is fixed to <code>narrow</code>")),
          tags$li(HTML("<b>label</b> is fixed to <code>DC_Proxy</code>")),
          tags$li(HTML(
            "Provide a single value (no spaces) for <b>drug_abbreviation</b> and <b>drug_concept</b>. ",
            "<b>drug_abbreviation</b> must start with <code>DC_</code> and elements should be separated by underscores (_)."
          ))
        ),
        textInput("atc_abbrev", "drug_abbreviation (must start with DC_, no spaces)", placeholder = "DP_condition_subgroup"),
        textInput("atc_subconcept", "drug_concept (no spaces)", placeholder = "condition_subgroup")
      ),
      hr(),
      h4("Add custom entry"),
      textInput("in_drug_abbrev", "drug_abbreviation"),
      textInput("in_product_identifier", "coding_system", placeholder = "ATC"),
      textInput("in_code", "code"),
      textInput("in_product_name", "product_name"),
      textInput("in_tags", "tags", placeholder = "narrow"),
      textInput("in_label", "label", placeholder = "DC_Proxy"),
      textInput("in_subconcept", "drug_concept"),
      actionButton("btn_add_row", "Add entry", icon = icon("plus-circle"), class = "btn-success"),
      br(), br(),
      checkboxInput("dedupe_on_add", "De-duplicate by (coding_system, code) when adding", TRUE)
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        type = "tabs",
        
        # --- FIRST TAB: Prepare and clean input files --------------------
        tabPanel(
          "Prepare and clean input files",
          br(),
          h4("Prepare and clean ATC input files"),
          helpText(HTML(
            "Scan <code>Scripts/Dictionaries</code> for raw ATC files starting with <code>ATC_</code>, merge them, ",
            "and export a cleaned file with standard columns <code>code</code> and <code>drug_name</code>."
          )),
          fluidRow(
            column(
              4,
              wellPanel(
                h5("ATC source files"),
                radioButtons(
                  "prep_atc_ext",
                  "File type",
                  choices = c(".csv", ".txt", ".xlsx"),
                  selected = ".csv"
                ),
                actionButton("prep_scan_atc", "Scan and load files"),
                uiOutput("prep_atc_files_info"),
                hr(),
                textInput(
                  "prep_atc_code_col",
                  "ATC code column in raw files (→ will become \"code\")"
                ),
                textInput(
                  "prep_atc_name_col",
                  "Drug name column in raw files (→ will become \"drug_name\")"
                ),
                actionButton("prep_apply_clean", "Apply cleaning", class = "btn-primary"),
                br(), br(),
                actionButton("prep_save_clean", "Save cleaned ATC file", class = "btn-success")
              )
            ),
            column(
              8,
              h5("Preview of merged raw ATC file"),
              DTOutput("prep_atc_raw_preview"),
              hr(),
              h5("Preview of cleaned ATC file"),
              DTOutput("prep_atc_clean_preview")
            )
          ),
          
          hr(),
          h4("Prepare and clean CPRD product files"),
          helpText(HTML(
            "Scan <code>Scripts/Dictionaries</code> for raw CPRD product files starting with <code>CPRD_</code>, merge them, ",
            "and export a cleaned file with standard columns <code>code</code>, <code>EMIS_term</code>, ",
            "<code>product_name</code>, and <code>substance_name</code>."
          )),
          fluidRow(
            column(
              4,
              wellPanel(
                h5("CPRD source files"),
                radioButtons(
                  "prep_cprd_ext",
                  "File type",
                  choices = c(".csv", ".txt", ".xlsx"),
                  selected = ".csv"
                ),
                actionButton("prep_scan_cprd", "Scan and load files"),
                uiOutput("prep_cprd_files_info"),
                hr(),
                textInput(
                  "prep_cprd_code_col",
                  "Code column in raw files (→ will become \"code\")"
                ),
                textInput(
                  "prep_cprd_emis_col",
                  "EMIS term column (→ will become \"EMIS_term\")"
                ),
                textInput(
                  "prep_cprd_prodname_col",
                  "Product name column (→ will become \"product_name\")"
                ),
                textInput(
                  "prep_cprd_subst_col",
                  "Substance name column (→ will become \"substance_name\")"
                ),
                textInput(
                  "prep_cprd_form_col",
                  "Formulation column (→ will become \"formulation\")"
                ),
                textInput(
                  "prep_cprd_route_col",
                  "Route of administration column (→ will become \"administration_route\")"
                ),
                actionButton("prep_cprd_apply_clean", "Apply cleaning", class = "btn-primary"),
                br(), br(),
                actionButton("prep_cprd_save_clean", "Save cleaned CPRD file", class = "btn-success")
              )
            ),
            column(
              8,
              h5("Preview of merged raw CPRD file"),
              DTOutput("prep_cprd_raw_preview"),
              hr(),
              h5("Preview of cleaned CPRD file"),
              DTOutput("prep_cprd_clean_preview")
            )
          )
        ),
        
        # --- SECOND TAB: Create a drug codelist -------------------------
        tabPanel(
          "Create a drug codelist",
          br(),
          h4("ATC dictionary (Scripts/Dictionaries/atc.csv)"),
          uiOutput("atc_file_status"),
          div(
            h5("Pick ATC main anatomical groups (A–V) to include"),
            DTOutput("atc_letter_table")
          ),
          br(),
          div(
            h5("Pick ATC therapeutic subgroups (2nd level, 3-character codes, e.g., A01, B03)"),
            p(em("Select rows, then use the buttons to set All Descendants and Include in Codelist.")),
            div(
              style = "margin-bottom:8px",
              actionButton("l2_mark_yes", "All Descendants = Yes", class = "btn btn-success btn-sm"),
              actionButton("l2_mark_no",  "All Descendants = No",  class = "btn btn-secondary btn-sm", style = "margin-left:6px;"),
              span(style = "margin-left:16px;"),
              actionButton("l2_incl_yes", "Include in Codelist = Yes", class = "btn btn-primary btn-sm"),
              actionButton("l2_incl_no",  "Include in Codelist = No",  class = "btn btn-outline-primary btn-sm", style = "margin-left:6px;"),
              span(style = "margin-left:16px;")
            ),
            DTOutput("atc_group_table")
          ),
          br(),
          div(
            h5("Pick ATC pharmacological subgroups (3rd level, 4-character codes, e.g., A01A)"),
            div(
              style = "margin-bottom:8px",
              actionButton("l3_mark_yes", "All Descendants = Yes", class = "btn btn-success btn-sm"),
              actionButton("l3_mark_no",  "All Descendants = No",  class = "btn btn-secondary btn-sm", style = "margin-left:6px;"),
              span(style = "margin-left:16px;"),
              actionButton("l3_incl_yes", "Include in Codelist = Yes", class = "btn btn-primary btn-sm"),
              actionButton("l3_incl_no",  "Include in Codelist = No",  class = "btn btn-outline-primary btn-sm", style = "margin-left:6px;"),
              span(style = "margin-left:16px;"),
              actionButton("add_l3_to_codelist", "Add chosen L3 groups → Working codelist", class = "btn btn-warning btn-sm")
            ),
            DTOutput("atc_l3_table")
          ),
          hr(),
          h4("Working codelist (editable)"),
          helpText("Double-click a cell to edit. Use Remove to delete highlighted rows."),
          DTOutput("working_table"),
          br(),
          actionButton("remove_selected", "Remove selected rows", icon = icon("trash"), class = "btn-danger"),
          hr(),
          h4("PRODCODEID mapping"),
          p("Link working codelist terms to CPRD Aurum/Gold Product codes (PRODCODEID)."),
          radioButtons(
            "include_prodcodeid",
            label   = "Include PRODCODEID?",
            choices = c("No", "Yes"),
            selected = "No",
            inline   = TRUE
          ),
          # mechanism selection, only shown if PRODCODEID mapping is on; systemic vs topical
          conditionalPanel(
            condition = "input.include_prodcodeid == 'Yes'",
            radioButtons(
              "drug_mechanism",
              "Select drug mechanism:",
              choices  = c("All", "Systemic", "Topical"),
              selected = "All",
              inline   = TRUE
            )
          ),
          uiOutput("cprd_file_status"),
          div(
            style = "margin:6px 0;",
            actionButton("build_prodcodeid", "Build PRODCODEID mapping", class = "btn btn-warning btn-sm"),
            actionButton("dedupe_prod", "Deduplicate list", class = "btn btn-secondary btn-sm", style = "margin-left:8px;"),
            actionButton("merge_prod_into_working", "Merge to working codelist", class = "btn btn-primary btn-sm", style = "margin-left:8px;")
          ),
          DTOutput("prodcodeid_table"),
          br(),
          actionButton("remove_selected_prod", "Remove selected PRODCODEID rows", icon = icon("trash"), class = "btn-danger btn-sm"),
          actionButton("clear_prod", "Clear PRODCODEID table", icon = icon("broom"), class = "btn-warning btn-sm", style = "margin-left:8px;")
        ),
        
        # --- THIRD TAB: Update a drug codelist --------------------------
        tabPanel(
          "Update a drug codelist",
          br(),
          h4("Upload an existing codelist to edit / merge"),
          fileInput(
            "existing_file",
            "Existing codelist CSV (must contain all required columns)",
            accept = c(".csv", "text/csv")
          ),
          tags$div(
            em("Required columns: drug_abbreviation, coding_system, code, product_name, tags, label, drug_concept")
          ),
          checkboxInput("merge_dedupe", "De-duplicate when merging (coding_system, code)", TRUE),
          fluidRow(
            column(6, DTOutput("existing_table")),
            column(
              6,
              actionButton("merge_into_working", "Merge into working codelist", icon = icon("arrow-right"), class = "btn-primary"),
              br(), br(),
              h4("Inline edit uploaded codelist"),
              helpText("Edits here affect only the uploaded view until merged."),
              DTOutput("existing_table_editable")
            )
          )
        ),
        
        # --- FOURTH TAB: Export full drug codelist -----------------------
        tabPanel(
          "Export full drug codelist",
          br(),
          h4("Preview & export"),
          fluidRow(
            column(
              4,
              radioButtons(
                "export_format",
                "Format",
                choices = c("CSV" = "csv", "Excel (XLSX)" = "xlsx"),
                inline = FALSE
              ),
              textInput("export_fname", "File name (without extension)", value = "drug_codelist"),
              downloadButton("download_export", "Download full codelist")
            ),
            column(
              8,
              uiOutput("summary_box"),
              DTOutput("export_preview")
            )
          )
        )
      )
    )
  )
)

# Server -----------------------------------------------------------------
server <- function(input, output, session) {
  # Working codelist stored here
  working <- reactiveVal(empty_codelist)
  # Secondary working set for PRODCODEID mapping
  working_prod <- reactiveVal(empty_codelist)
  highlight_codes <- reactiveVal(character())
  
  # Base app paths --------------------------------------------------------
  app_dir  <- file.path(projectFolder)
  dict_dir <- file.path(app_dir, "Scripts", "Dictionaries")
  sys_top_df <- reactive({
    path <- file.path(dict_dir, "systemic_topical_drugs.csv")
    if (!file.exists(path)) return(NULL)
    readr::read_csv(
      path,
      show_col_types = FALSE,
      progress       = FALSE,
      col_types      = readr::cols(.default = readr::col_character())
    )
  })
  
  # ------------ Prepare and clean input files (ATC) ----------------------
  prep_atc_raw   <- reactiveVal(NULL)
  prep_atc_clean <- reactiveVal(NULL)
  
  output$prep_atc_files_info <- renderUI({
    ext_choice  <- input$prep_atc_ext %||% ".csv"
    ext_pattern <- switch(
      ext_choice,
      ".csv" = "csv",
      ".txt" = "txt",
      ".xlsx" = "xlsx",
      "csv"
    )
    
    files <- list.files(
      dict_dir,
      pattern    = paste0("^(ATC_|atc_).+\\.", ext_pattern, "$"),
      full.names = FALSE
    )
    
    if (length(files) == 0) {
      tagList(span(class = "text-muted", "No matching ATC_ / atc_ files found yet."))
    } else {
      tagList(
        strong("Matched files:"),
        tags$ul(lapply(files, tags$li))
      )
    }
  })
  
  observeEvent(input$prep_scan_atc, {
    ext_choice  <- input$prep_atc_ext %||% ".csv"
    ext_pattern <- switch(
      ext_choice,
      ".csv" = "csv",
      ".txt" = "txt",
      ".xlsx" = "xlsx",
      "csv"
    )
    
    files <- list.files(
      dict_dir,
      pattern    = paste0("^(ATC_|atc_).+\\.", ext_pattern, "$"),
      full.names = TRUE
    )
    
    if (length(files) == 0) {
      prep_atc_raw(NULL)
      prep_atc_clean(NULL)
      showNotification("No matching ATC files found in Scripts/Dictionaries.", type = "warning")
      return()
    }
    
    read_one <- function(f) {
      if (ext_pattern == "csv") {
        readr::read_csv(f, show_col_types = FALSE, progress = FALSE)
      } else if (ext_pattern == "txt") {
        readr::read_delim(f, delim = "\t", show_col_types = FALSE, progress = FALSE)
      } else if (ext_pattern == "xlsx") {
        if (!requireNamespace("readxl", quietly = TRUE)) {
          stop("Package 'readxl' is required to read XLSX files. Please install it or choose another format.")
        }
        readxl::read_excel(f)
      } else {
        readr::read_csv(f, show_col_types = FALSE, progress = FALSE)
      }
    }
    
    dfs    <- lapply(files, read_one)
    dfs    <- lapply(dfs, as.data.frame)
    merged <- dplyr::bind_rows(dfs)
    
    prep_atc_raw(merged)
    prep_atc_clean(NULL)
    
    showNotification(
      paste0("Loaded ", length(files), " file(s), merged to ", nrow(merged), " row(s)."),
      type = "message"
    )
  })
  
  observeEvent(input$prep_apply_clean, {
    df <- prep_atc_raw()
    validate(need(!is.null(df), "No ATC file loaded yet. Use 'Scan and load files' first."))
    
    code_col <- input$prep_atc_code_col %||% ""
    name_col <- input$prep_atc_name_col %||% ""
    
    validate(
      need(nzchar(code_col), "Please enter atc_code_column."),
      need(nzchar(name_col), "Please enter drug_name_column."),
      need(code_col %in% names(df), paste0("Column '", code_col, "' not found in merged file.")),
      need(name_col %in% names(df), paste0("Column '", name_col, "' not found in merged file."))
    )
    
    cleaned <- df %>%
      dplyr::transmute(
        code      = .data[[code_col]],
        drug_name = .data[[name_col]]
      ) %>%
      dplyr::filter(
        !is.na(code),      code      != "",
        !is.na(drug_name), drug_name != ""
      ) %>%
      dplyr::distinct()
    
    prep_atc_clean(cleaned)
    showNotification(
      paste0("Cleaned data: ", nrow(cleaned), " row(s) and ", ncol(cleaned), " column(s)."),
      type = "message"
    )
  })
  
  observeEvent(input$prep_save_clean, {
    df <- prep_atc_clean()
    validate(need(!is.null(df), "No cleaned data to save. Click 'Apply cleaning' first."))
    
    clean_dir <- file.path(dict_dir, "Cleaned")
    if (!dir.exists(clean_dir)) dir.create(clean_dir, recursive = TRUE, showWarnings = FALSE)
    out_path <- file.path(clean_dir, "ATC_cleaned.csv")
    
    readr::write_csv(df, out_path)
    showNotification(paste0("Saved cleaned ATC file to ", out_path), type = "message")
  })
  
  output$prep_atc_raw_preview <- renderDT({
    df <- prep_atc_raw()
    req(df)
    datatable(head(df, 10), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$prep_atc_clean_preview <- renderDT({
    df <- prep_atc_clean()
    req(df)
    datatable(head(df, 10), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # ------------ Prepare and clean input files (CPRD) ---------------------
  prep_cprd_raw   <- reactiveVal(NULL)
  prep_cprd_clean <- reactiveVal(NULL)
  
  output$prep_cprd_files_info <- renderUI({
    ext_choice  <- input$prep_cprd_ext %||% ".csv"
    ext_pattern <- switch(
      ext_choice,
      ".csv" = "csv",
      ".txt" = "txt",
      ".xlsx" = "xlsx",
      "csv"
    )
    
    files <- list.files(
      dict_dir,
      pattern    = paste0("^(CPRD_|cprd_).+\\.", ext_pattern, "$"),
      full.names = FALSE
    )
    
    if (length(files) == 0) {
      tagList(span(class = "text-muted", "No matching CPRD_ / cprd_ files found yet."))
    } else {
      tagList(
        strong("Matched files:"),
        tags$ul(lapply(files, tags$li))
      )
    }
  })
  
  observeEvent(input$prep_scan_cprd, {
    ext_choice  <- input$prep_cprd_ext %||% ".csv"
    ext_pattern <- switch(
      ext_choice,
      ".csv" = "csv",
      ".txt" = "txt",
      ".xlsx" = "xlsx",
      "csv"
    )
    
    files <- list.files(
      dict_dir,
      pattern    = paste0("^(CPRD_|cprd_).+\\.", ext_pattern, "$"),
      full.names = TRUE
    )
    
    if (length(files) == 0) {
      prep_cprd_raw(NULL)
      prep_cprd_clean(NULL)
      showNotification("No matching CPRD files found in Scripts/Dictionaries.", type = "warning")
      return()
    }
    
    read_one <- function(f) {
      if (ext_pattern == "csv") {
        readr::read_csv(
          f,
          show_col_types = FALSE,
          progress       = FALSE,
          col_types      = readr::cols(.default = readr::col_character())
        )
      } else if (ext_pattern == "txt") {
        readr::read_delim(
          f,
          delim          = "\t",
          show_col_types = FALSE,
          progress       = FALSE,
          col_types      = readr::cols(.default = readr::col_character())
        )
      } else if (ext_pattern == "xlsx") {
        if (!requireNamespace("readxl", quietly = TRUE)) {
          stop("Package 'readxl' is required to read XLSX files. Please install it or choose another format.")
        }
        readxl::read_excel(f) %>%
          dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
      } else {
        readr::read_csv(
          f,
          show_col_types = FALSE,
          progress       = FALSE,
          col_types      = readr::cols(.default = readr::col_character())
        )
      }
    }
    
    dfs <- lapply(files, read_one)
    dfs <- lapply(dfs, as.data.frame)  # keep as data.frame, but all character
    merged <- dplyr::bind_rows(dfs)
    
    prep_cprd_raw(merged)
    prep_cprd_clean(NULL)
    
    showNotification(
      paste0("Loaded ", length(files), " CPRD file(s), merged to ", nrow(merged), " row(s)."),
      type = "message"
    )
  })
  
  
  observeEvent(input$prep_cprd_apply_clean, {
    df <- prep_cprd_raw()
    validate(need(!is.null(df), "No CPRD file loaded yet. Use 'Scan and load files' first."))
    
    code_col   <- input$prep_cprd_code_col      %||% ""
    emis_col   <- input$prep_cprd_emis_col      %||% ""
    prod_col   <- input$prep_cprd_prodname_col  %||% ""
    subst_col  <- input$prep_cprd_subst_col     %||% ""
    form_col  <- input$prep_cprd_form_col     %||% ""  
    route_col  <- input$prep_cprd_route_col     %||% ""
    
    validate(
      need(nzchar(code_col),  "Please enter code column."),
      need(nzchar(emis_col),  "Please enter EMIS term column."),
      need(nzchar(prod_col),  "Please enter product name column."),
      need(nzchar(subst_col), "Please enter substance name column."),
      need(nzchar(form_col), "Please enter formulation name column."),
      need(nzchar(route_col), "Please enter route of administration name column."),
      need(code_col  %in% names(df),  paste0("Column '", code_col,  "' not found in merged file.")),
      need(emis_col  %in% names(df),  paste0("Column '", emis_col,  "' not found in merged file.")),
      need(prod_col  %in% names(df),  paste0("Column '", prod_col,  "' not found in merged file.")),
      need(subst_col %in% names(df),  paste0("Column '", subst_col, "' not found in merged file.")),
      need(form_col %in% names(df),  paste0("Column '", form_col, "' not found in merged file.")),
      need(route_col %in% names(df),  paste0("Column '", route_col, "' not found in merged file."))
      
      
    )
    
    cleaned <- df %>%
      dplyr::transmute(
        code           = .data[[code_col]],
        EMIS_term      = .data[[emis_col]],
        product_name   = .data[[prod_col]],
        substance_name = .data[[subst_col]],
        formulation = .data[[form_col]],
        administration_route = .data[[route_col]]
      ) %>%
      dplyr::filter(
        !is.na(code),
        code != ""
      ) %>%
      dplyr::distinct() 
    
    prep_cprd_clean(cleaned)
    showNotification(
      paste0("Cleaned CPRD data: ", nrow(cleaned), " row(s) and ", ncol(cleaned), " column(s)."),
      type = "message"
    )
  })
  
  observeEvent(input$prep_cprd_save_clean, {
    df <- prep_cprd_clean()
    validate(need(!is.null(df), "No cleaned CPRD data to save. Click 'Apply cleaning' first."))
    
    clean_dir <- file.path(dict_dir, "Cleaned")
    if (!dir.exists(clean_dir)) dir.create(clean_dir, recursive = TRUE, showWarnings = FALSE)
    out_path <- file.path(clean_dir, "CPRD_cleaned.csv")
    library(dplyr)
    df %>%
      mutate(across(everything(), as.character)) %>%
      readr::write_csv(out_path)
    
    showNotification(paste0("Saved cleaned CPRD file to ", out_path), type = "message")
  })
  
  output$prep_cprd_raw_preview <- renderDT({
    df <- prep_cprd_raw()
    req(df)
    datatable(head(df, 10),options = list(pageLength = 10, scrollX = TRUE))
  })
  
  
  output$prep_cprd_clean_preview <- renderDT({
    df <- prep_cprd_clean()
    req(df)
    datatable(head(df, 10), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # ------------ Helper: assign systemic / topical mechanism based on lookup -----------
  assign_mechanism <- function(df_hits, sys_df) {
    if (is.null(sys_df) || nrow(df_hits) == 0) {
      df_hits$mechanism <- ""
      return(df_hits)
    }
    
    norm_str <- function(x) {
      x <- ifelse(is.na(x), "", x)
      tolower(gsub("\\s+", "", x))
    }
    
    # Pre-normalise CPRD fields we search in
    df_hits <- df_hits %>%
      dplyr::mutate(
        norm_prod  = norm_str(product_name),
        norm_form  = norm_str(formulation),
        norm_route = norm_str(administration_route)
      )
    
    # Split lookup into route-based and formulation-based terms
    sys_route <- sys_df %>%
      dplyr::filter(!is.na(administration_route), administration_route != "") %>%
      dplyr::mutate(
        term_norm     = norm_str(administration_route),
        priority_num  = suppressWarnings(as.integer(priority))
      )
    
    sys_form <- sys_df %>%
      dplyr::filter(!is.na(formulation), formulation != "") %>%
      dplyr::mutate(
        term_norm     = norm_str(formulation),
        priority_num  = suppressWarnings(as.integer(priority))
      )
    
    # Inner helper: get best classification (classification, priority) for one row
    match_one_set <- function(p_norm, f_norm, r_norm, term_df) {
      if (nrow(term_df) == 0) {
        return(list(class = NA_character_, prio = NA_integer_))
      }
      hits <- term_df %>%
        dplyr::filter(
          term_norm != "",
          purrr::map_lgl(term_norm, ~ {
            t <- .
            (nzchar(p_norm) && grepl(t, p_norm, fixed = TRUE)) ||
              (nzchar(f_norm) && grepl(t, f_norm, fixed = TRUE)) ||
              (nzchar(r_norm) && grepl(t, r_norm, fixed = TRUE))
          })
        )
      if (nrow(hits) == 0) {
        return(list(class = NA_character_, prio = NA_integer_))
      }
      # Choose lowest priority (1 beats 2); on ties take first
      hits <- hits %>%
        dplyr::arrange(priority_num)
      list(
        class = hits$classification[1],
        prio  = hits$priority_num[1]
      )
    }
    
    # Route-based classification
    res_route <- purrr::pmap(
      list(df_hits$norm_prod, df_hits$norm_form, df_hits$norm_route),
      ~ match_one_set(..1, ..2, ..3, sys_route)
    )
    class_route <- vapply(res_route, `[[`, character(1), "class")
    prio_route  <- vapply(res_route, function(x) as.integer(x$prio), integer(1))
    
    # Formulation-based classification
    res_form <- purrr::pmap(
      list(df_hits$norm_prod, df_hits$norm_form, df_hits$norm_route),
      ~ match_one_set(..1, ..2, ..3, sys_form)
    )
    class_form <- vapply(res_form, `[[`, character(1), "class")
    prio_form  <- vapply(res_form, function(x) as.integer(x$prio), integer(1))
    
    df_hits <- df_hits %>%
      dplyr::mutate(
        class_route = class_route,
        prio_route  = prio_route,
        class_form  = class_form,
        prio_form   = prio_form
      ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        mechanism = {
          # Reconciliation rules:
          # - If both route + form labels present → choose lower priority (1 beats 2)
          # - If only one label present → use that
          # - If no label → empty string
          cls <- c(class_route, class_form)
          pri <- c(prio_route,  prio_form)
          if (all(is.na(cls) | cls == "")) {
            ""
          } else if (!is.na(cls[1]) && cls[1] != "" && (is.na(cls[2]) || cls[2] == "")) {
            cls[1]
          } else if ((is.na(cls[1]) || cls[1] == "") && !is.na(cls[2]) && cls[2] != "") {
            cls[2]
          } else {
            # both have labels → pick lower priority (1 vs 2)
            pri_clean <- ifelse(is.na(pri), Inf, pri)
            best_idx  <- which.min(pri_clean)
            cls[best_idx]
          }
        }
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(mechanism = ifelse(is.na(mechanism), "", mechanism))
    
    # Drop helper cols before returning
    df_hits %>%
      dplyr::select(-norm_prod, -norm_form, -norm_route,
                    -class_route, -prio_route, -class_form, -prio_form)
  }
  
  # ------------ Templates download --------------------------------------
  output$download_templates <- downloadHandler(
    filename = function() sprintf("codelist_templates_%s.zip", format(Sys.Date(), "%Y%m%d")),
    content = function(file) {
      tmpdir <- tempdir()
      dict_tmpl <- tibble::tibble(
        drug_abbreviation     = c("DP_Diabetes", "DP_Cholesterol"),
        coding_system    = c("ATC", "ATC"),
        code                  = c("A10BA02", "C10AA05"),
        product_name          = c("Metformin", "Atorvastatin"),
        tags                  = c("narrow", "narrow"),
        label                 = c("DC_Proxy", "DC_Proxy"),
        drug_concept = c("glucose_lowering", "lipid_lowering")
      )
      codelist_tmpl <- dict_tmpl[0, ]  # same schema, no rows
      dict_path <- file.path(tmpdir, "dictionary_template.csv")
      cl_path   <- file.path(tmpdir, "codelist_template.csv")
      readr::write_csv(dict_tmpl, dict_path)
      readr::write_csv(codelist_tmpl, cl_path)
      old <- setwd(tmpdir)
      on.exit(setwd(old), add = TRUE)
      utils::zip(zipfile = file, files = c(basename(dict_path), basename(cl_path)))
    }
  )
  
  # ------------ ATC loader & first-LEVEL list (Create tab) --------------
  atc_default_path  <- file.path(dict_dir, "Cleaned/ATC_cleaned.csv")
  cprd_default_path <- file.path(dict_dir, "Cleaned/CPRD_cleaned.csv")
  
  # Use either the default path or a user-selected file if the default isn't found
  atc_df <- reactive({
    path <- atc_default_path
    if (!file.exists(path) && !is.null(input$atc_file_browse)) path <- input$atc_file_browse$datapath
    if (!file.exists(path)) return(NULL)
    readr::read_csv(path, show_col_types = FALSE, progress = FALSE) %>%
      janitor::clean_names() %>%
      mutate(
        code       = as.character(.data$code),
        atc_letter = substr(.data$code, 1, 1)
      )
  })
  
  output$atc_file_status <- renderUI({
    if (is.null(atc_df())) {
      tagList(
        div(class = "text-danger", icon("exclamation-triangle"), strong("atc.csv not found at:"), span(atc_default_path)),
        br(),
        fileInput("atc_file_browse", "Or browse to atc.csv", accept = c(".csv", "text/csv"), buttonLabel = "Choose atc.csv")
      )
    } else {
      df <- atc_df()
      div(
        strong("Loaded:"),
        span(if (!is.null(input$atc_file_browse)) input$atc_file_browse$name else atc_default_path),
        HTML("&nbsp;|&nbsp;"), span(nrow(df)), " rows"
      )
    }
  })
  
  # ------------ CPRD Aurum Product loader (for PRODCODEID mapping) ------
  cprd_df <- reactive({
    req(input$include_prodcodeid == "Yes")
    path <- cprd_default_path
    if (!file.exists(path)) return(NULL)
    
    suppressWarnings({
      df <- readr::read_csv(
        path,
        show_col_types = FALSE,
        progress = FALSE,
        col_types = readr::cols(.default = readr::col_character())
      )
    })
    
    needed <- c("code", "EMIS_term", "product_name", "substance_name", "formulation", "administration_route")
    if (!all(needed %in% names(df))) return(NULL)
    df %>% dplyr::select(all_of(needed))
  })
  
  output$cprd_file_status <- renderUI({
    req(input$include_prodcodeid)
    if (input$include_prodcodeid == "No") return(NULL)
    if (is.null(cprd_df())) {
      div(
        class = "text-danger",
        icon("exclamation-triangle"),
        strong("CPRD_cleaned.csv not found or missing required columns at: "),
        span(cprd_default_path)
      )
    } else {
      df <- cprd_df()
      div(
        strong("Loaded CPRD file:"), span(cprd_default_path),
        HTML("&nbsp;|&nbsp;"), span(nrow(df)), " rows",
        HTML("&nbsp;|&nbsp;"), span(ncol(df)), " cols"
      )
    }
  })
  
  letters_rx <- reactive({
    df <- atc_df()
    if (is.null(df)) return(NULL)
    letters <- sort(unique(substr(df$code, 1, 1)))
    tibble::tibble(
      first_level_ATC        = letters,
      anatomical_main_group  = unname(ATC_L1_DESC[letters])
    )
  })
  
  output$atc_letter_table <- renderDT({
    lt <- letters_rx(); req(lt)
    datatable(
      lt,
      rownames  = FALSE,
      selection = "multiple",
      options   = list(dom = "t", paging = FALSE)
    )
  })
  
  # Build L2 (3-character) group table from selection
  l2_groups_rx <- reactive({
    df <- atc_df(); req(df)
    lt <- letters_rx(); req(lt)
    sel <- input$atc_letter_table_rows_selected
    validate(need(length(sel) > 0, "Select one or more ATC main groups above."))
    picked_letters <- lt$first_level_ATC[sel]
    df %>%
      dplyr::filter(substr(code, 1, 1) %in% picked_letters, nchar(code) == 3) %>%
      dplyr::mutate(second_level_ATC = substr(code, 1, 3)) %>%
      dplyr::arrange(second_level_ATC) %>%
      dplyr::distinct(second_level_ATC, .keep_all = TRUE) %>%
      dplyr::select(second_level_ATC, therapeutic_subgroup = drug_name)
  })
  
  # State for L2 with flags
  groups_state <- reactiveVal(NULL)
  
  sanitize_id <- function(x) gsub("[^A-Za-z0-9_]", "_", x)
  
  observeEvent(l2_groups_rx(), {
    base <- l2_groups_rx()
    if (is.null(base)) return()
    prev <- groups_state()
    if (is.null(prev)) {
      prev <- tibble::tibble(
        second_level_ATC        = character(),
        include_all_descendants = character(),
        include_in_codelist     = character()
      )
    }
    merged <- base %>%
      dplyr::left_join(
        prev %>% dplyr::select(second_level_ATC, include_all_descendants, include_in_codelist),
        by = "second_level_ATC"
      ) %>%
      dplyr::mutate(row_id = sanitize_id(second_level_ATC))
    groups_state(merged)
  }, ignoreInit = FALSE)
  
  output$atc_group_table <- renderDT({
    gt <- groups_state(); req(gt)
    datatable(
      gt %>% dplyr::select(second_level_ATC, therapeutic_subgroup, include_all_descendants, include_in_codelist),
      rownames  = FALSE,
      selection = "multiple",
      options   = list(dom = "t", paging = FALSE)
    )
  })
  
  observeEvent(input$l2_mark_yes, {
    gt <- groups_state(); req(gt)
    rows <- input$atc_group_table_rows_selected
    validate(need(length(rows) > 0, "Select one or more L2 rows."))
    gt$include_all_descendants[rows] <- "Yes"
    groups_state(gt)
  })
  observeEvent(input$l2_mark_no, {
    gt <- groups_state(); req(gt)
    rows <- input$atc_group_table_rows_selected
    validate(need(length(rows) > 0, "Select one or more L2 rows."))
    gt$include_all_descendants[rows] <- "No"
    groups_state(gt)
  })
  
  observeEvent(input$l2_incl_yes, {
    gt <- groups_state(); req(gt)
    rows <- input$atc_group_table_rows_selected
    validate(need(length(rows) > 0, "Select one or more L2 rows."))
    gt$include_in_codelist[rows] <- "Yes"
    groups_state(gt)
  })
  observeEvent(input$l2_incl_no, {
    gt <- groups_state(); req(gt)
    rows <- input$atc_group_table_rows_selected
    validate(need(length(rows) > 0, "Select one or more L2 rows."))
    gt$include_in_codelist[rows] <- "No"
    groups_state(gt)
  })
  
  # -------- L3 (4-character) table built from L2 rows --------------------
  l3_table <- reactive({
    df <- atc_df(); req(df)
    gt <- groups_state(); req(gt)
    expand_groups <- gt %>%
      dplyr::filter(include_in_codelist == "Yes") %>%
      dplyr::pull(second_level_ATC)
    if (length(expand_groups) == 0) {
      return(tibble::tibble(
        second_level_ATC        = character(),
        third_level_atc         = character(),
        pharmacological_subgroup = character()
      ))
    }
    df %>%
      dplyr::filter(nchar(code) == 4, substr(code, 1, 3) %in% expand_groups) %>%
      dplyr::mutate(third_level_atc = substr(code, 1, 4)) %>%
      dplyr::arrange(third_level_atc) %>%
      dplyr::distinct(third_level_atc, .keep_all = TRUE) %>%
      dplyr::mutate(second_level_ATC = substr(code, 1, 3)) %>%
      dplyr::select(second_level_ATC, third_level_atc, pharmacological_subgroup = drug_name)
  })
  
  l3_state <- reactiveVal(NULL)
  
  observeEvent(l3_table(), {
    base <- l3_table()
    prev <- l3_state()
    if (is.null(prev)) {
      prev <- tibble::tibble(
        third_level_atc        = character(),
        include_all_descendants = character(),
        include_in_codelist     = character()
      )
    }
    gt <- groups_state(); req(gt)
    
    merged <- base %>%
      dplyr::left_join(
        prev %>% dplyr::select(third_level_atc, include_all_descendants, include_in_codelist),
        by = "third_level_atc"
      ) %>%
      dplyr::left_join(
        gt %>%
          dplyr::select(second_level_ATC, include_in_codelist, include_all_descendants) %>%
          dplyr::rename(l2_incl = include_in_codelist, l2_all = include_all_descendants),
        by = "second_level_ATC"
      ) %>%
      dplyr::mutate(
        include_in_codelist     = dplyr::if_else(
          is.na(include_in_codelist) & l2_incl == "Yes" & l2_all == "Yes",
          "Yes",
          include_in_codelist
        ),
        include_all_descendants = dplyr::if_else(
          is.na(include_all_descendants) & l2_incl == "Yes" & l2_all == "Yes",
          "Yes",
          include_all_descendants
        )
      ) %>%
      dplyr::select(-l2_incl, -l2_all)
    
    l3_state(merged)
  }, ignoreInit = FALSE)
  
  output$atc_l3_table <- renderDT({
    lt <- l3_state(); req(lt)
    datatable(
      lt %>% dplyr::select(third_level_atc, pharmacological_subgroup, include_all_descendants, include_in_codelist),
      rownames  = FALSE,
      selection = "multiple",
      options   = list(dom = "t", paging = FALSE)
    )
  })
  
  observeEvent(input$l3_mark_yes, {
    lt <- l3_state(); req(lt)
    rows <- input$atc_l3_table_rows_selected
    validate(need(length(rows) > 0, "Select one or more L3 rows."))
    lt$include_all_descendants[rows] <- "Yes"
    l3_state(lt)
  })
  observeEvent(input$l3_mark_no, {
    lt <- l3_state(); req(lt)
    rows <- input$atc_l3_table_rows_selected
    validate(need(length(rows) > 0, "Select one or more L3 rows."))
    lt$include_all_descendants[rows] <- "No"
    l3_state(lt)
  })
  
  observeEvent(input$l3_incl_yes, {
    lt <- l3_state(); req(lt)
    rows <- input$atc_l3_table_rows_selected
    validate(need(length(rows) > 0, "Select one or more L3 rows."))
    lt$include_in_codelist[rows] <- "Yes"
    l3_state(lt)
  })
  observeEvent(input$l3_incl_no, {
    lt <- l3_state(); req(lt)
    rows <- input$atc_l3_table_rows_selected
    validate(need(length(rows) > 0, "Select one or more L3 rows."))
    lt$include_in_codelist[rows] <- "No"
    l3_state(lt)
  })
  
  observeEvent(input$add_l3_to_codelist, {
    lt <- l3_state(); req(lt)
    df_atc <- atc_df(); req(df_atc)
    
    sel <- lt %>% dplyr::filter(include_in_codelist == "Yes")
    validate(need(nrow(sel) > 0, "No L3 rows have Include in Codelist = Yes."))
    
    abbr <- input$atc_abbrev %||% ""
    if (nzchar(abbr)) {
      abbr <- gsub("\\s+", "_", abbr)
      if (!startsWith(abbr, "DC_")) abbr <- paste0("DC_", gsub("^DC_+", "", abbr))
    }
    subcon <- input$atc_subconcept %||% ""
    if (nzchar(subcon)) subcon <- gsub("\\s+", "_", subcon)
    
    expand_one <- function(prefix) {
      df_atc %>%
        dplyr::filter(startsWith(code, prefix)) %>%
        dplyr::transmute(code = as.character(code), product_name = drug_name)
    }
    
    sel_yes_all <- sel %>% dplyr::filter(include_all_descendants == "Yes")
    sel_no_all  <- sel %>% dplyr::filter(include_all_descendants != "Yes")
    
    expanded_yes <- if (nrow(sel_yes_all) > 0) purrr::map_dfr(sel_yes_all$third_level_atc, expand_one) else tibble::tibble(code = character(), product_name = character())
    expanded_no  <- if (nrow(sel_no_all)  > 0) purrr::map_dfr(sel_no_all$third_level_atc,  expand_one) else tibble::tibble(code = character(), product_name = character())
    
    make_rows <- function(df_codes) {
      if (nrow(df_codes) == 0) return(empty_codelist[0, ])
      tibble::tibble(
        drug_abbreviation     = abbr,
        coding_system    = "ATC",
        code                  = df_codes$code,
        product_name          = df_codes$product_name,
        tags                  = "narrow",
        label                 = "DC_Proxy",
        drug_concept = subcon
      )
    }
    
    rows_yes <- make_rows(expanded_yes)
    rows_no  <- make_rows(expanded_no)
    
    df <- dplyr::bind_rows(working(), rows_yes, rows_no) %>%
      dplyr::distinct(coding_system, code, .keep_all = TRUE)
    working(df)
    
    hi <- unique(c(highlight_codes(), rows_no$code))
    highlight_codes(hi)
    
    added_n <- nrow(rows_yes) + nrow(rows_no)
    showNotification(
      paste0("Added ", added_n, " code(s) to working codelist."),
      type = "message"
    )
  })
  
  output$atc_group_preview <- renderDT({
    df <- atc_df(); req(df)
    gt <- l2_groups_rx(); req(gt)
    sel <- input$atc_group_table_rows_selected
    validate(need(length(sel) > 0, "Select one or more 3-character groups above."))
    picked_groups <- gt$second_level_ATC[sel]
    
    abbr <- input$atc_abbrev %||% ""
    if (nzchar(abbr)) {
      abbr <- gsub("\\s+", "_", abbr)
      if (!startsWith(abbr, "DC_")) abbr <- paste0("DC_", gsub("^DC_+", "", abbr))
    }
    subcon <- input$atc_subconcept %||% ""
    if (nzchar(subcon)) subcon <- gsub("\\s+", "_", subcon)
    
    preview_df <- df %>%
      dplyr::filter(substr(code, 1, 3) %in% picked_groups) %>%
      dplyr::transmute(
        drug_abbreviation     = abbr,
        coding_system    = "ATC",
        code                  = as.character(code),
        product_name          = drug_name,
        tags                  = "narrow",
        label                 = "DC_Proxy",
        drug_concept = subcon
      )
    datatable(preview_df, options = list(pageLength = 8, scrollX = TRUE))
  })
  
  output$atc_preview <- renderDT({
    df <- atc_df(); req(df)
    lt <- letters_rx(); req(lt)
    sel <- input$atc_letter_table_rows_selected
    validate(need(length(sel) > 0, "Select one or more ATC main groups above."))
    picked <- lt$first_level_ATC[sel]
    datatable(
      df %>% dplyr::filter(atc_letter %in% picked) %>% dplyr::select(cut, code, drug_name),
      options = list(pageLength = 8, scrollX = TRUE)
    )
  })
  
  # Enforce formatting live for the ATC preview inputs
  observeEvent(input$atc_abbrev, ignoreInit = TRUE, {
    val <- input$atc_abbrev %||% ""
    if (nzchar(val)) {
      val <- gsub("\\s+", "_", val)
      if (!startsWith(val, "DC_")) val <- paste0("DC_", gsub("^DC_+", "", val))
    }
    updateTextInput(session, "atc_abbrev", value = val)
  })
  observeEvent(input$atc_subconcept, ignoreInit = TRUE, {
    val <- input$atc_subconcept %||% ""
    if (nzchar(val)) val <- gsub("\\s+", "_", val)
    updateTextInput(session, "atc_subconcept", value = val)
  })
  
  # ------------ Add custom entry ----------------------------------------
  observeEvent(input$btn_add_row, {
    validate(
      need(nzchar(input$in_product_name), "product_name is required"),
      need(nzchar(input$in_code), "code is required")
    )
    newrow <- tibble::tibble(
      drug_abbreviation     = input$in_drug_abbrev %||% "",
      coding_system    = input$in_product_identifier %||% "",
      code                  = input$in_code %||% "",
      product_name          = input$in_product_name %||% "",
      tags                  = input$in_tags %||% "",
      label                 = input$in_label %||% "",
      drug_concept = input$in_subconcept %||% ""
    )
    df <- bind_rows(working(), newrow)
    if (isTRUE(input$dedupe_on_add)) df <- distinct(df, coding_system, code, .keep_all = TRUE)
    working(df)
    showNotification("Entry added", type = "message")
    updateTextInput(session, "in_drug_abbrev", value = "")
    updateTextInput(session, "in_product_identifier", value = "")
    updateTextInput(session, "in_code", value = "")
    updateTextInput(session, "in_product_name", value = "")
    updateTextInput(session, "in_tags", value = "")
    updateTextInput(session, "in_label", value = "")
    updateTextInput(session, "in_subconcept", value = "")
  })
  
  # ------------ Working table (editable) --------------------------------
  output$working_table <- renderDT({
    df <- working()
    dt <- datatable(df, selection = "multiple", editable = TRUE, options = list(pageLength = 8, scrollX = TRUE))
    hi <- highlight_codes()
    if (length(hi) > 0) {
      dt <- DT::formatStyle(
        dt, "code",
        target         = "row",
        backgroundColor = DT::styleEqual(hi, rep("#FFF9C4", length(hi)))
      )
    }
    dt
  })
  
  observeEvent(input$working_table_cell_edit, {
    info <- input$working_table_cell_edit
    df   <- working()
    row  <- info$row; col <- info$col; val <- info$value
    colname <- colnames(df)[col]
    df[row, colname] <- val
    working(df)
  })
  
  observeEvent(input$remove_selected, {
    rows <- input$working_table_rows_selected
    validate(need(length(rows) > 0, "Select one or more rows to remove."))
    df <- working()
    df <- df[-rows, , drop = FALSE]
    working(df)
  })
  
  observeEvent(input$btn_clear_all, {
    showModal(modalDialog(
      title = "Clear working codelist?",
      "This will remove all rows from the working codelist.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_clear", "Clear", class = "btn-danger")
      )
    ))
  })
  observeEvent(input$confirm_clear, {
    removeModal()
    working(empty_codelist)
  })
  
  # ------------ Update / merge existing codelist ------------------------
  existing_df <- reactiveVal(NULL)
  
  observeEvent(input$existing_file, {
    req(input$existing_file)
    df <- readr::read_csv(
      input$existing_file$datapath,
      col_types = readr::cols(.default = readr::col_character()),
      show_col_types = FALSE
    ) %>%
      janitor::clean_names()
    
    required <- names(empty_codelist)
    validate(need(
      all(required %in% names(df)),
      sprintf("Codelist must contain required columns: %s", paste(required, collapse = ", "))
    ))
    
    df <- df %>%
      select(all_of(required)) %>%
      mutate(across(everything(), as.character))
    existing_df(df)
  })
  
  output$existing_table <- renderDT({
    req(existing_df())
    datatable(existing_df(), options = list(pageLength = 5, scrollX = TRUE))
  })
  
  output$existing_table_editable <- renderDT({
    req(existing_df())
    datatable(existing_df(), editable = TRUE, options = list(pageLength = 5, scrollX = TRUE))
  })
  
  observeEvent(input$existing_table_editable_cell_edit, {
    info <- input$existing_table_editable_cell_edit
    df   <- existing_df()
    row  <- info$row; col <- info$col; val <- info$value
    colname <- colnames(df)[col]
    df[row, colname] <- val
    existing_df(df)
  })
  
  observeEvent(input$merge_into_working, {
    req(existing_df())
    merged <- bind_rows(working(), existing_df())
    if (isTRUE(input$merge_dedupe)) {
      merged <- distinct(merged, coding_system, code, .keep_all = TRUE)
    }
    working(merged)
    showNotification("Merged uploaded codelist into working set", type = "message")
  })
  
  # ------------ PRODCODEID mapping logic --------------------------------
  observeEvent(input$build_prodcodeid, {
    req(input$include_prodcodeid == "Yes")
    df_cprd <- cprd_df();   req(df_cprd)
    df_work <- working();   req(df_work)
    
    terms <- unique(df_work$product_name)
    terms <- terms[!is.na(terms) & nzchar(terms)]
    
    match_term <- function(term) {
      hits <- df_cprd %>%
        dplyr::filter(
          grepl(term, EMIS_term,      ignore.case = TRUE) |
            grepl(term, product_name,    ignore.case = TRUE) |
            grepl(term, substance_name,  ignore.case = TRUE)
        )
      if (nrow(hits) == 0) return(NULL)
      hits
    }
    
    matched <- purrr::map(terms, match_term)
    matched <- matched[!vapply(matched, is.null, logical(1))]
    if (length(matched) == 0) {
      working_prod(empty_codelist)
      showNotification("No PRODCODEIDs matches found from working codelist terms.", type = "warning")
      return()
    }
    
    df_hits <- dplyr::bind_rows(matched)
    
    # --- NEW: apply systemic/topical mechanism classification -------------
    sys_df <- sys_top_df()
    df_hits <- assign_mechanism(df_hits, sys_df)
    
    # Filter by requested mechanism, if any
    mech_choice <- input$drug_mechanism %||% "All"
    if (!is.null(mech_choice) && mech_choice != "All") {
      target <- tolower(mech_choice)
      df_hits <- df_hits %>%
        dplyr::mutate(mech_lower = tolower(trimws(mechanism))) %>%
        dplyr::filter(mech_lower == target) %>%
        dplyr::select(-mech_lower)
      
      if (nrow(df_hits) == 0) {
        working_prod(empty_codelist)
        showNotification(
          paste0("No PRODCODEID matches after filtering for ", mech_choice, " drugs."),
          type = "warning"
        )
        return()
      }
    }
    
    # --- Existing logic to build codelist, now including mechanism --------
    abbr <- input$atc_abbrev %||% ""
    if (nzchar(abbr)) {
      abbr <- gsub("\\s+", "_", abbr)
      if (!startsWith(abbr, "DC_")) abbr <- paste0("DC_", gsub("^DC_+", "", abbr))
    }
    subcon <- input$atc_subconcept %||% ""
    if (nzchar(subcon)) subcon <- gsub("\\s+", "_", subcon)
    
    best_name <- dplyr::coalesce(
      dplyr::na_if(df_hits$product_name,  ""),
      dplyr::na_if(df_hits$substance_name, ""),
      dplyr::na_if(df_hits$EMIS_term,      "")
    )
    
    out <- tibble::tibble(
      drug_abbreviation = abbr,
      coding_system     = "PRODCODEID",
      code              = df_hits$code,
      product_name      = best_name,
      tags              = "narrow",
      label             = "DC_Proxy",
      drug_concept      = subcon,
      mechanism         = df_hits$mechanism   # NEW COLUMN
    ) %>%
      dplyr::distinct(coding_system, code, .keep_all = TRUE)
    
    working_prod(out)
    showNotification(
      paste0("Built PRODCODEID mapping with ", nrow(out), " unique code(s)"),
      type = "message"
    )
  })
  
  output$prodcodeid_table <- renderDT({
    dat <- working_prod()
    datatable(dat, selection = "multiple", options = list(pageLength = 8, scrollX = TRUE))
  })
  
  observeEvent(input$remove_selected_prod, {
    rows <- input$prodcodeid_table_rows_selected
    validate(need(length(rows) > 0, "Select one or more PRODCODEID rows to remove."))
    df <- working_prod()
    df <- df[-rows, , drop = FALSE]
    working_prod(df)
  })
  
  observeEvent(input$clear_prod, {
    showModal(modalDialog(
      title = "Clear PRODCODEID table?",
      "This will remove all rows from the PRODCODEID table.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_clear_prod", "Clear", class = "btn-danger")
      )
    ))
  })
  observeEvent(input$confirm_clear_prod, {
    removeModal()
    working_prod(empty_codelist)
  })
  
  observeEvent(input$dedupe_prod, {
    df <- working_prod()
    validate(need(nrow(df) > 0, "PRODCODEID table is empty."))
    df2 <- dplyr::distinct(df, coding_system, code, .keep_all = TRUE)
    working_prod(df2)
    showNotification(
      paste0("Deduplicated to ", nrow(df2), " unique code(s)."),
      type = "message"
    )
  })
  
  observeEvent(input$merge_prod_into_working, {
    df_main <- working()
    df_prod <- working_prod()
    validate(need(nrow(df_prod) > 0, "No PRODCODEID rows to merge."))
    merged <- dplyr::bind_rows(df_main, df_prod) %>%
      dplyr::distinct(coding_system, code, .keep_all = TRUE)
    working(merged)
    showNotification(
      paste0("Merged ", nrow(df_prod), " PRODCODEID row(s) into working codelist."),
      type = "message"
    )
  })
  
  # ------------ Export tab -----------------------------------------------
  output$summary_box <- renderUI({
    df <- working()
    tagList(
      strong("Rows:"), span(nrow(df)), HTML("&nbsp;&nbsp;"),
      strong("Unique product_identifiers:"), span(dplyr::n_distinct(df$coding_system)), HTML("&nbsp;&nbsp;"),
      strong("Unique codes:"), span(dplyr::n_distinct(df$code))
    )
  })
  
  output$export_preview <- renderDT({
    datatable(working(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$download_export <- downloadHandler(
    filename = function() {
      ext <- input$export_format
      sprintf(
        "%s.%s",
        gsub("\\s+", "_", input$export_fname %||% "drug_codelist"),
        ext
      )
    },
    content = function(file) {
      df  <- working()
      validate(need(nrow(df) > 0, "Working codelist is empty; nothing to export."))
      
      ext <- tolower(input$export_format)
      
      write_out <- function(df, file, ext) {
        if (ext == "csv") {
          # optional: keep everything as text in the output values
          df <- dplyr::mutate(df, dplyr::across(everything(), as.character))
          readr::write_csv(df, file, na = "")
        } else if (ext %in% c("xlsx", "xls")) {
          wb <- openxlsx::createWorkbook()
          openxlsx::addWorksheet(wb, "data")
          openxlsx::writeData(
            wb, "data", df,
            colTypes = rep("text", ncol(df))
          )
          openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
        } else {
          stop("Unsupported format: ", ext)
        }
      }
      
      # IMPORTANT: actually write the file
      write_out(df, file, ext)
    }
  )
  
}


# Utility: null-coalescing for base R
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (is.character(x) && !nzchar(x))) y else x

shinyApp(ui, server)
