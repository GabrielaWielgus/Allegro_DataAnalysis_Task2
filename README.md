# Data Analysis README

This repository contains a data analysis script written in R for analyzing survey responses. The script performs data cleaning, analysis, and visualization on the provided datasets.

## Getting Started

To use this script, you need to have R installed on your system. You also need to have the following CSV files in the same directory as the script:
- `questions.csv`: Contains the questions data.
- `Data-1.csv`: Contains the survey responses data.

## Usage

1. Clone or download this repository to your local machine.
2. Place the required CSV files (`questions.csv` and `Data-1.csv`) in the same directory as the script.
3. Open the R script in your preferred R environment.
4. Run the script.

## Data Cleaning

The script performs the following data cleaning tasks:
- Converts certain columns to the appropriate data types.
- Removes invalid or incomplete data entries.
- Splits and processes specific columns for analysis.

## Data Analysis

The script analyzes the survey data in various ways, including:
- Calculating mean ratings and Net Promoter Score (NPS).
- Grouping data by age and buying intentions.
- Conducting correlation analysis between age and survey responses.

## Libraries Used

The following R libraries are used in this script:
- `tm`: For text mining and preprocessing.
- `SnowballC`: For text stemming.
- `wordcloud`: For creating word clouds.
- `sentimentr`: For sentiment analysis.

## Outputs

The script outputs various results and visualizations, including:
- Mean ratings and NPS.
- Proportions of responses by age and buying intentions.
- Correlation between age and survey responses.

## Issues

An error in text analysis is noted and marked for future correction.

## Repository Structure

- `questions.csv`: Contains the questions data.
- `Data-1.csv`: Contains the survey responses data.
- `analysis_script.R`: The R script for data cleaning, analysis, and visualization.

## Contributors

- Gabriela Wielgus
