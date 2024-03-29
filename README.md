---
editor_options: 
  markdown: 
    wrap: 72
---

# 22-07995-000_ClarkCountyTables

Regina Lionheart 2023-06-23

![knitr](https://img.shields.io/badge/knitr-table-%231D455C)
![RMarkdown](https://img.shields.io/badge/knitr-RMarkdown-%231D455C)

------------------------------------------------------------------------

![](thumbnail.jpg)

## Table of Contents

-   [Project Description](#Project-Description)
-   [Location in Herrera Database](#Location-in-Herrera-Database)\
-   [Requirements and Dependencies](#Requirements-and-Dependencies)
-   [Installation and Usage](#Installation-and-Usage)
    -   [Layout of directory and data](#Layout-of-directory-and-data)
    -   [Detailed description of data and
        analysis](#Detailed-description-of-data-and-analysis)
-   [Visualizations](#Visualization)
-   [Pull Requests](#Pull-Requests)
-   [Contributors and Contact
    Information](#Contributors-and-Contact-Information)

------------------------------------------------------------------------

# Project 22-07995 - Task 000

Clark County Shore Project

**SharePoint Site:** [SharePoint
Site](https://herrerainc.sharepoint.com/teams/22-07995-000)

**Vantagepoint Site:** [VantagePoint
Site](https://herrerainc.deltekfirst.com/HerreraInc/app/#!ProjectView/view/0/0/22-07995-000/presentation)

------------------------------------------------------------------------

## Project Description {#project-description}

This script renders 36 summary tables for the Clark County Shore
Project. There are 18 tables of Canopy Cover information, and 18 tables
of Impervious Cover information.

## :droplet: Location in Herrera Database

The original, unmodified data used in this project is located in the
"data_raw" folder within this repository. Please use the "Revised" excel
sheet, which should be the correct sheet used in the script.

That folder is backed up to: `K:\Projects\Y2022\22-07995-000\Table\`

If you do not have access to the data, please contact the emails listed
at the bottom of the repository.

## 📦 Requirements and Dependencies {#requirements-and-dependencies}

Below is a list of packages and external softwares that this project
utilizes. Please ensure you have the package(s) installed and have
access to the tools listed below.

| Name                              | Description                                                                                                                           |
|:----------------|:------------------------------------------------------|
| [`R`](https://www.r-project.org/) | Programming language used for this project.                                                                                           |
| Original Exported Excel Data      | The raw data for analysis, located in data_raw. If you do not have access to this data, please contact the owners of this repository. |

## :computer: Installation and Usage

In order to run this script and recreate the analysis, you will need to
have R and Rstudio installed on your computer. All the data produced by
this analysis can be found in the data_secondary/ folder, while all
figures can be found in the figures/ directory.

### :arrows_counterclockwise: Layout of directory and data

This repository is organized into a main.Rmd markdown script, which
produces the results from scratch when run in its entirety. The main
script references folders of raw data (data_raw/), and produces results
data that has been modified or created by the analysis (data_secondary/,
figures/). All analysis scripts are contained in the R/ directory. The
extras/ folder contains an image of the original template for the data,
created in Excel.

The data_raw/ folder is **READ ONLY** and should never be modified or
deleted.

### :heavy_check_mark: Detailed description of data and analysis

The raw data consists of an Excel sheet of canopy and impervious
coverage data for 18 watersheds. The analysis loads and tidies the data,
then splits by watershed and creates new summary columns for each
category. The category columns are then pasted together to form "Acres,
(%)" columns.

Once each table has been created, a loop knits, renders and saves the
tables in LaTeX to the figures/ folder as pdfs.

------------------------------------------------------------------------

## Visualization {#visualization}

<figure>

<embed src="figures/Allen%20Canyon%20CreekCanopy.pdf" />

<figcaption aria-hidden="true">

Example of one output table:

</figcaption>

</figure>

------------------------------------------------------------------------

## 🔧 Pull Requests {#pull-requests}

Pull requests are welcome. For major changes, please open an issue
first.

All functioning code is located on the main branch. Dev branches are to
be named <specific_issue_description>\_dev.

## 💬 Contributors + Contact Information

-   [Regina Lionheart](https://github.com/R-Lionheart)
-   [Lauren
    Ode-Giles](https://www.herrerainc.com/team-member/lauren-ode-giles/)
-   [Jenn Schmidt](https://www.herrerainc.com/team-member/jenn-schmidt/)
