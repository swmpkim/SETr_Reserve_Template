Reserve-level workflow repository
================

  - [Directory structure](#directory-structure)
  - [Proposed workflow](#proposed-workflow)

This page and repository are for the scripts as they’re modified to work
at the Reserve-level. The file structure necessarily needs to be
different from what I’ve been doing to wrangle data from multiple
reserves. It needs to work on other people’s computers, on their
individual datasets.

Here, my efforts are to construct a standardized file directory that
Reserves can use to manage and perform basic analyses on SET data into
the future. The only folder being tracked here on GitHub is the
`R_scripts` folder.

## Directory structure

The top level is the `Reserve_Template` folder; everything performed by
the R scripts will be relative to this folder. It can be anywhere on the
user’s computer.

The only folder being tracked here on GitHub is the `R_scripts` folder.

<img src="2020-01-14_SETr_directory.png" style="display: block; margin: auto;" />

## Proposed workflow

Each of the folders referenced below is a subfolder of the
`Reserve_Template` directory. I’ll use Grand Bay as an example in this
workflow; anywhere you see *gnd*, substitute in your own Reserve code.

More detail will be available in the “Guide to the SETr Workflow”
document produced at the end of the project (Feb. 28, 2020).

1.  **Enter new data** into raw data spreadsheet, *gndset.xlsx*, in the
    `data/raw` folder. This spreadsheet is a single Excel file, with a
    separate worksheet for each individual SET.  
2.  **Run a processing script**, `R_scripts/01_process_raw_data.R`, to
    read in the raw Excel file and generate a single csv file that
    contains data for all SETs on all dates. This file, ,
    *gndset\_processed.csv*, will be saved into the `data/processed`
    folder.  
3.  **Update your metadata**, *gndset\_metadata.xlsx*, if necessary.
    Found in the `metadata` folder.  
4.  **Run interactive QC app**. This is a Shiny application run by the
    script `02_interact_qaqc_app.R` in the `R_scripts` folder. It pulls
    in data from the `data/processed` folder.  
5.  **Fix/flag any problems** *in the raw data spreadsheet* (
    *gndset.xlsx*, in `data/raw` folder).  
6.  **Update metadata** if necessary to address any data quality
    issues.  
7.  **Re-run processing script** from step 2, to generate an updated
    *gndset\_processed.csv* file in the `data/processed` folder. This
    will overwrite the previous file.  
8.  **Run analysis script**, `03_analyze_word.R`, in the `R_scripts`
    folder. Two report files will be generated: an outreach document in
    the `R_outputs/outreach_doc` folder, and a technical analysis
    document in the `R_outputs/analysis` folder.  
9.  **Run interactive mapping script**, `04_interact_maps.R`, in the
    `R_scripts` folder to look for spatial patterns in the results.  
10. Recommended, pending SET working group and DMC approval: **Submit
    processed data** (*gndset\_processed.csv*) and metadata to the CDMO
    annually for flat file hosting.
