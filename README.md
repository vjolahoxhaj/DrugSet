![Status](https://img.shields.io/badge/Study_status-For_publication-blue)

# DrugSet

DrugSet is a Shiny application designed to support the creation, cleaning, updating, and export of medicinal products codelists for real-world data studies.

The app was developed to make medicinal products codelist construction more transparent, consistent, and reproducible. It allows users to build ATC-based medicinal products codelists, add custom entries, update existing codelists, and, when CPRD product dictionaries are available, cross-map ATC-based medicinal products terms to CPRD Aurum/Gold product codes (`PRODCODEID`).

DrugSet is intended for researchers working with routinely collected healthcare data, common data models, and distributed pharmacoepidemiology studies where consistent medicinal products definitions are needed across databases.

------------------------------------------------------------------------

## Main features

DrugSet currently supports four main workflows:

1.  Prepare and clean input files
2.  Create a drug codelist
3.  Update an existing drug codelist
4.  Export the final drug codelist

The app also supports optional mapping from ATC drug terms to CPRD product codes.

------------------------------------------------------------------------

## Folder structure

The app expects the following folder structure:

``` text
DrugSet/
├── app.R
├── torun.R
├── packages.R
├── README.md
├── Scripts/
│   ├── logo.png
│   └── Dictionaries/
│       ├── ATC_*.csv / ATC_*.txt / ATC_*.xlsx
│       ├── CPRD_*.csv / CPRD_*.txt / CPRD_*.xlsx
│       ├── systemic_topical_drugs.csv
│       └── Cleaned/
│           ├── ATC_cleaned.csv
│           └── CPRD_cleaned.csv
```

The `Scripts/Dictionaries` folder is where raw dictionary files should be placed before running the cleaning step.

The `Scripts/Dictionaries/Cleaned` folder is created by the app when cleaned ATC or CPRD files are saved.

------------------------------------------------------------------------

## Required files

The following files are required to run the app:

``` text
app.R
torun.R
renv.lock
```

The following files are optional but recommended:

``` text
Scripts/Dictionaries/systemic_topical_drugs.csv
```

The `systemic_topical_drugs.csv` file is used when classifying CPRD product-code matches as systemic or topical.

------------------------------------------------------------------------

## Required R packages

DrugSet uses the following R packages:

``` r
shiny
DT
dplyr
readr
readxl
stringr
data.table
shinyjs
rstudioapi
htmltools
utils
tinytex
shinyscreenshot
tibble
purrr
janitor
openxlsx
writexl
rmarkdown
remotes
```

The `renv` script installs and loads the required packages automatically.

If installation fails, install the missing package manually in RStudio using:

``` r
install.packages("package_name")
```

For example:

``` r
install.packages("dplyr")
install.packages("readr")
install.packages("janitor")
install.packages("openxlsx")
install.packages("writexl")
```

------------------------------------------------------------------------

## How to run the app

1.  Open `DrugSet_Public.Rproj` in RStudio.

2.  Run in the console the following commands:

``` r
  install.packages("renv")
  library(renv)
  renv::activate()
  renv::restore()
```

3.  Open `torun.R` in RStudio and press Run App button.

The script will:

1.  identify the project folder;
2.  set the working directory;
3.  load the required packages from `packages.R`;
4.  remove the temporary `Codelist` folder if it exists;
5.  launch the Shiny app.

The app should open in the RStudio Viewer or in your default browser.

------------------------------------------------------------------------

## Input dictionaries

DrugSet uses two main types of dictionary files:

1.  ATC dictionary files
2.  CPRD product dictionary files

Raw dictionary files should be placed in:

``` text
Scripts/Dictionaries/
```

------------------------------------------------------------------------

## ATC input files

ATC files should start with:

``` text
ATC_
```

or:

``` text
atc_
```

Accepted file formats are:

``` text
.csv
.txt
.xlsx
```

The raw ATC file must contain at least:

| Required information | Description                                  |
|----------------------|----------------------------------------------|
| ATC code column      | Column containing the ATC code               |
| Drug name column     | Column containing the ATC drug or group name |

During cleaning, the app converts these into:

| Cleaned column | Description            |
|----------------|------------------------|
| code           | ATC code               |
| drug_name      | ATC drug or group name |

The cleaned ATC file is saved as:

``` text
Scripts/Dictionaries/Cleaned/ATC_cleaned.csv
```

------------------------------------------------------------------------

## CPRD input files

CPRD product files should start with:

``` text
CPRD_
```

or:

``` text
cprd_
```

Accepted file formats are:

``` text
.csv
.txt
.xlsx
```

The raw CPRD file must contain columns corresponding to:

| Required information | Description                    |
|----------------------|--------------------------------|
| code                 | CPRD product code / PRODCODEID |
| EMIS term            | CPRD EMIS product term         |
| product name         | Product name                   |
| substance name       | Substance or active ingredient |
| formulation          | Product formulation            |
| administration route | Route of administration        |

During cleaning, the app converts these into:

| Cleaned column       | Description                    |
|----------------------|--------------------------------|
| code                 | CPRD product code / PRODCODEID |
| EMIS_term            | EMIS product term              |
| product_name         | Product name                   |
| substance_name       | Substance or active ingredient |
| formulation          | Product formulation            |
| administration_route | Route of administration        |

The cleaned CPRD file is saved as:

``` text
Scripts/Dictionaries/Cleaned/CPRD_cleaned.csv
```

------------------------------------------------------------------------

## Output codelist structure

The exported codelist uses the following columns:

| Column            | Description                                             |
|-------------------|-----------------------------------------------------|
| drug_abbreviation | Short drug or drug-class identifier                     |
| coding_system     | Coding system, for example `ATC` or `PRODCODEID`        |
| code              | Drug code                                               |
| product_name      | Drug, product, or concept name                          |
| tags              | Tag used to describe the code type; default is `narrow` |
| label             | Label used in the codelist; default is `DC_Proxy`       |
| drug_concept      | Drug concept or subgroup name                           |

When CPRD mapping is used, an additional `mechanism` column may also be included. This column describes whether a product was classified as systemic, topical, or left unclassified.

------------------------------------------------------------------------

## Naming rules

Before adding ATC codes to the working codelist, the app asks for two fields:

### `drug_abbreviation`

This should be a short, study-friendly abbreviation.

Rules:

- it should not contain spaces;
- it should start with `DC_`;
- elements should be separated using underscores.

Example:

``` text
DC_isotretinoin_oral
```

### `drug_concept`

This should describe the drug concept or subgroup.

Rules:

- it should not contain spaces;
- use underscores instead of spaces.

Example:

``` text
isotretinoin_oral
```

------------------------------------------------------------------------

## Typical workflow

### Step 1: Prepare dictionaries

Place raw ATC and CPRD dictionary files in:

``` text
Scripts/Dictionaries/
```

Open the app and go to:

``` text
Prepare and clean input files
```

For ATC files:

1.  Select the file type.
2.  Click `Scan and load files`.
3.  Enter the raw column name containing ATC codes.
4.  Enter the raw column name containing drug names.
5.  Click `Apply cleaning`.
6.  Click `Save cleaned ATC file`.

For CPRD files:

1.  Select the file type.
2.  Click `Scan and load files`.
3.  Enter the raw column names requested by the app.
4.  Click `Apply cleaning`.
5.  Click `Save cleaned CPRD file`.

------------------------------------------------------------------------

### Step 2: Create an ATC codelist

Go to:

``` text
Create a drug codelist
```

Then:

1.  Enter `drug_abbreviation`.
2.  Enter `drug_concept`.
3.  Select the ATC main anatomical group.
4.  Select the relevant second-level ATC therapeutic subgroup.
5.  Mark whether all descendants should be included.
6.  Mark whether the selected group should be included in the codelist.
7.  Review the third-level ATC pharmacological subgroups.
8.  Select the relevant third-level groups.
9.  Click `Add chosen L3 groups → Working codelist`.

The selected codes will be added to the editable working codelist.

------------------------------------------------------------------------

### Step 3: Review and edit the working codelist

The working codelist can be edited directly in the app.

You can:

- double-click cells to edit values;
- remove selected rows;
- add a custom entry manually;
- clear the full working codelist;
- deduplicate entries.

Manual entries are useful when a code or product needs to be added outside the ATC selection workflow.

------------------------------------------------------------------------

### Step 4: Add CPRD product-code mappings

To map ATC-based terms to CPRD product codes:

1.  In the `Create a drug codelist` tab, set `Include PRODCODEID?` to `Yes`.
2.  Select the drug mechanism:
    - `All`
    - `Systemic`
    - `Topical`
3.  Click `Build PRODCODEID mapping`.
4.  Review the candidate product-code matches.
5.  Remove irrelevant rows if needed.
6.  Click `Deduplicate list`.
7.  Click `Merge to working codelist`.

The CPRD product-code rows will be added to the working codelist.

------------------------------------------------------------------------

### Step 5: Update an existing codelist

Go to:

``` text
Update a drug codelist
```

Upload an existing CSV codelist with the required columns:

``` text
drug_abbreviation
coding_system
code
product_name
tags
label
drug_concept
```

You can edit the uploaded codelist in the app before merging it with the working codelist.

------------------------------------------------------------------------

### Step 6: Export the final codelist

Go to:

``` text
Export full drug codelist
```

Choose the export format:

``` text
CSV
Excel (XLSX)
```

Enter the output file name and click:

``` text
Download full codelist
```

The exported file will contain the final reviewed codelist.

------------------------------------------------------------------------

## Example output

A simple exported codelist may look like this:

| drug_abbreviation | coding_system | code | product_name | tags | label | drug_concept |
|-----------|-----------|-----------|-----------|-----------|-----------|-----------|
| DC_isotretinoin_oral | ATC | D10BA01 | Isotretinoin | narrow | DC_Proxy | isotretinoin_oral |
| DC_isotretinoin_oral | PRODCODEID | 123456 | Isotretinoin 20mg capsules | narrow | DC_Proxy | isotretinoin_oral |

------------------------------------------------------------------------

## Important notes

DrugSet is intended to support codelist construction, but it does not replace expert review.

All exported codelists should be reviewed by someone with clinical, pharmacological, or database-specific expertise before use in a study.

In particular, users should carefully check:

- whether the ATC level selected is appropriate for the study question;
- whether all descendants should truly be included;
- whether combination products should be included or excluded;
- whether topical, oral, injectable, or other formulations are relevant;
- whether CPRD product-code matches are clinically appropriate;
- whether historical or discontinued products should be retained;
- whether the final codelist matches the intended exposure definition.

------------------------------------------------------------------------

## Known limitations

The CPRD product-code mapping is based on text matching against product names, EMIS terms, and substance names. This means that candidate matches may include irrelevant products and should always be manually reviewed.

Systemic and topical classification depends on the availability and quality of the `systemic_topical_drugs.csv` lookup file. If this file is missing, incomplete, or not aligned with the CPRD dictionary, some products may remain unclassified or may require manual checking.

The app currently focuses on drug codelist construction and ATC-to-CPRD product-code mapping. Other coding systems or other database-specific dictionaries may require further adaptation.

------------------------------------------------------------------------

## Troubleshooting

### The app does not open

Check that you opened `torun.R` in RStudio and ran:

``` r
source("torun.R")
```

Also check that the required packages are installed.

------------------------------------------------------------------------

### The app cannot find the ATC file

Make sure the cleaned ATC file exists here:

``` text
Scripts/Dictionaries/Cleaned/ATC_cleaned.csv
```

If it does not exist, go to the first tab and run the ATC cleaning workflow.

------------------------------------------------------------------------

### The app cannot find the CPRD file

Make sure the cleaned CPRD file exists here:

``` text
Scripts/Dictionaries/Cleaned/CPRD_cleaned.csv
```

If it does not exist, go to the first tab and run the CPRD cleaning workflow.

------------------------------------------------------------------------

### No CPRD product-code matches are found

This may happen if:

- the CPRD dictionary is missing;
- the cleaned CPRD file does not contain the required columns;
- the working codelist product names are too broad or too specific;
- the product names do not appear in the CPRD product dictionary;
- the systemic/topical filter is too restrictive.

Try using `All` as the mechanism filter and review the matches manually.

------------------------------------------------------------------------

### The exported file is empty

The working codelist must contain at least one row before export.

Go back to the codelist creation tab and add ATC codes, custom entries, or CPRD product-code mappings before exporting.

------------------------------------------------------------------------

## Suggested acknowledgement

DrugSet was developed to support transparent and reproducible drug codelist construction for real-world data studies, including studies using ATC codes and CPRD product-code mappings.

If used in a study, please describe:

- the version or date of the app used;
- the source and date of the ATC dictionary;
- the source and date of the CPRD product dictionary;
- the rules used to include or exclude drug groups, formulations, and product-code mappings;
- any manual review steps applied before finalising the codelist.

------------------------------------------------------------------------

## Version information update

Last updated: July 2026.

------------------------------------------------------------------------

## How to cite this work

<a href="https://doi.org/10.5281/zenodo.21257372"><img src="https://zenodo.org/badge/1216081309.svg" alt="DOI"/></a>

------------------------------------------------------------------------

## Author information

For any suggestions relating the DrugSet app feel free to contact Vjola Hoxhaj ([v.hoxhaj\@umcutrecht.nl](mailto:v.hoxhaj@umcutrecht.nl){.email}).
