# Materials for Distancing and Moral Dumbfounding Studies

## This directory contains the following files

- distancing_supplementary_materials.pdf (contains the materials to be used for the proposed pre-registered study)
- distancing_supplementary_materials.qmd (qmd file to generate distancing_supplementary_materials.pdf)
- load_all_pilot_data.R (R script for processing the raw spss files and generating csv files and .RData files)
- Pilot_studies_full.pdf (contains a full write up of all eight pilot studies)
- Pilot_studies_full.qmd (will generate Pilot_studies_full.pdf when rendered, includes full analysis code for all studies)
- plots.RData (automatically generated when rendering Pilot_studies_full.qmd, and contains the plots key plots generated for each study as objects)
- README.md (this file)
- sample_and_simulated_data.html (contains the proposed analysis for the pre-registered study based on a simulated dataset)
- sample_and_simulated_data.qmd (.qmd file to generate sample_and_simulated_data.html)
- set_up_sample_data.R (R script for preparing the sample dataset)
- simulate_data.R (R script for creating a simulated dataset)



## This directory contains the following folders

- _extensions (contains the relevant code for the apaquarto extension to produce APA formatted documents from the quarto files)
- pilot_data (contains all the data files for the pilot studies)
    subdirectories:
    - raw_SPSS_files (contains the unprocessed spss files downloaded from qualtrics/questback)
    - csv_files (contains the processed csv files for all studied)
    - loaded_data (contains .RData files for each study that are used in the main analyses)
- Pilot_studies_full_files (folder automatically generated when Pilot_studies_full.qmd is rendered, this is left empty here as it will be populated on rendering)
- Resources (contains additional files needed to render the qmd files)
    subdirectories:
    - bib (contains bibliographic files)
    - img (contains images)
- sample_and_simulated_data_files (folder automatically generated when sample_and_simulated_data.qmd is rendered, this is left empty here as it will be populated on rendering)
- sample_data (contains a sample dataset to use as a template for generating a simulated dataset for testing the code for the proposed analyses)

