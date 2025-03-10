# CartesianBehaviour - Points to behaviour <img src="man/figures/logo.png" align="right" width="120" />
[![R-CMD-check.yaml](https://github.com/Faccco/CartesianBehaviour/actions/workflows/R-CMD-check.yaml/badge.svg?branch=main)](https://github.com/Faccco/CartesianBehaviour/actions/workflows/R-CMD-check.yaml)
![CRAN](https://www.r-pkg.org/badges/version/CartesianBehaviour)
![CRANLOGS](https://cranlogs.r-pkg.org/badges/CartesianBehaviour)
[![DOI:<ssrn.5123233>](http://img.shields.io/badge/DOI-<ssrn.5123233>-<#800080>.svg)](<http://dx.doi.org/10.2139/ssrn.5123233>)

## Description
This project seeks to simplify the conversion of Cartesian tabular data provided from any source into behavioral endpoints. It get inspired by the packages <a href="https://swarm-lab.github.io/trackR/">TrackR</a> and <a href="https://github.com/JimMcL/trajr">trajr</a>, check them for options of tracking already implemented.

## Instalation
```
library(devtools)
install_github("Faccco/CartesianBehaviour")

# OR

library(githubinstall)
githubinstall("CartesianBehaviour")
```

## Basic use

For those not used to R here is a basic workflow to get your data.
Note that to run CartesianBehaviour you do not need to install the packages bellow, but they make dealing with .csv, .xls and .txt files easier.
Also, be sure to install CartesianBehaviour prior to the analysis.

```
# Install and load required packages for analysis

## List of required packages
package.list <- c("devtools",  # Development tools for GitHub installations
                  "data.table", # Efficient data manipulation
                  "readxl",    # Excel file handling
                  "tools")     # File path operations


## Identify packages needing installation
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]

## Install missing packages
if(length(new.packages)) {
  message("Installing required packages: ", paste(new.packages, collapse = ", "))
  install.packages(new.packages)
}

## Load all packages (silently)
invisible(lapply(package.list, require, character.only = TRUE))

## Cleanup installation variables
rm(package.list, new.packages)

```

When working with tracking software they usually export the tracks in csv, txt or xml format. Place all files in one folder and change the folder path with the folder you crated.

### .csv

```
# Set path to data directory
folder_path <- "D:/Path/Of/The/Files"  # Replace with actual data directory path

# Set the column names for the
X_axis <- "x_cm"
Y_axis <- "y_cm"
Time <- "frame"

# Get list of CSV files in directory
csv_list <- list.files(folder_path, pattern = "\\.csv$", full.names = FALSE)

# Load files with automatic delimiter detection
for (file in csv_list) {
  file_path <- file.path(folder_path, file)

  ## First attempt with comma separator
  data <- read.csv(file_path,
                   sep = ",",
                   header = TRUE,
                   stringsAsFactors = FALSE)

  ## Fallback to semicolon if single column detected (common European CSV format)
  if (ncol(data) == 1) {
    data <- read.csv(file_path,
                     sep = ";",
                     header = TRUE,
                     stringsAsFactors = FALSE)
  }

  ## Create dataframe using filename (without .csv extension)
  assign(gsub(".csv$", "", file), data)
}

# Cleanup temporary objects
rm(data, file, file_path, folder_path, csv_list)

# Combine all loaded datasets into a single list
Dados <- mget(setdiff(ls(), c("Time", "X_axis", "Y_axis")))

# Standardize column names across all datasets (time, x, y coordinates)
for(i in seq_along(Dados)){
  Dados[[i]] <- Dados[[i]][, colnames(Dados[[i]]) %in% c(Time, X_axis, Y_axis)]
  colnames(Dados[[i]]) <- c("time", "x", "y")  # Ensure consistent naming convention
}

# Environment Cleanup
rm(list = setdiff(ls(), "Dados"))
```

### .txt

```
# Set path to data directory (update with actual path)
folder_path <- "D:/Path/Of/The/Files" # Replace with actual data directory path

# Identify all .txt files in directory
txt_files <- list.files(folder_path, pattern = "\\.txt$", full.names = FALSE)

# Load and process tab-delimited text files
for (file in txt_files) {
  file_path <- file.path(folder_path, file)

  ## Efficiently read tab-separated files using data.table's fread
  data <- data.table::fread(file_path, sep = "\t", encoding = "UTF-8")

  ## Create dataframe using filename (without .txt extension)
  assign(gsub(".txt$", "", file), data)
}

# Cleanup temporary objects
rm(data, txt_files, file, file_path, folder_path)

# Combine all loaded datasets into a single list
Dados <- mget(ls())

# Enforce consistent column names across all datasets (time, x, y coordinates)
for(i in seq_along(Dados)){
  colnames(Dados[[i]]) <- c("time", "x", "y")  # Required column format for downstream analysis
}

# Remove all objects except the compiled dataset list
rm(list = setdiff(ls(), "Dados"))
```

### .xml

```

```
