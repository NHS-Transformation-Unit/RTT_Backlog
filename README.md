<img src="images/TU_logo_large.png" alt="TU logo" width="200" align="right"/>

# Referral to Treatment (RTT) Backlog
This repository contains the scripts for the analysis of how the RTT backlog has changed since the start of the pandemic. Data on RTT Pathways is published monthly by NHS England and can be found [here](https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/).

<br/>

## Using the Repository
The repository can be cloned to re-run the analysis that has been carried out. This will also rely on updating the extracts of monthly published RTT data to include more recent months.

<br/>

## Repository Structure

The structure of this repository is detailed below:

``` plaintext

├───data
├───images
└───src
    ├───config
    ├───outputs
    ├───processing
        └───sql_extraction
    ├───requirements
    └───visualisation
    
```

<br/>

### `data`
Where the extracts of the monthly published RTT data are saved for loading.

### `images`
Images such as TU logos and branding to add to outputs.

### `src`

All code is stored in src. This is subdivided into five modules:

1. `config`: Files for configuring the output such as the `theme.css`.
2. `data`: Extracts of the monthly published RTT data.
3. `processing`: Files for loading, cleaning and processing the RTT data.
4. `requirements`: Requirements for undertaking the analysis such as the `packages.R` script.
5. `visualisation`: Files for producing the visualisations used within the outputs.

<br/>

## Contributors
This repository has been created and developed by:

-   [Andy Wilson](https://github.com/ASW-Analyst)