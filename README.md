# Time to MA Project

## Overview
The "Time to MA" project aims to analyze the time it takes for blood clot to reach maximum amplitude (MA)

## Objective
The primary objective of the "Time to MA" project is to understand and analyze the distribution of Time to MA and its probability of being more than a specified threshold, to explore subgroups within the dataset to understand how various factors may influence the Time to MA.

## Data Sources
The project utilizes data from clinical trials (laboratory) where blood clotting parameters, including time to maximum amplitude (MA), are measured.

## Methodology
The project follows a data-driven approach, involving the following steps:

1. **Data Collection**: Gather experimental data on blood clotting parameters, participant information, and experimental conditions from clinical trials.
2. **Data Preprocessing**: Cleanse and preprocess the data, including handling missing values, merging missing data, removing unnecessary features
3. **Exploratory Data Analysis (EDA)**: Explore the data to identify patterns and distribution.  Visualizations and statistical analyses are used to gain insights into the data.
4. **Model Development**: Develop predictive model to fit to the "Time to MA" based on available data.

## Components
The "Time to MA" project consists of the following components:  


1. **data_processing** script: Function for , preprocessing, and transforming raw experimental data into a format suitable for analysis. 
2. **tma_distribution** script: Function to calculate the likelihood of Time to MA being greater then threshold + epsilon
3. **tma_liklihood** script: Scripts for developing and training predictive models using machine learning algorithms or statistical techniques.
4. **tma_cumulative** script: Function to calculate CDF of Time to MA.
5. **tma_model** script: Function to fit the model to data and return the resalts.
6. **Main.R** script: Combines all the functions and created visualizations
7. **plots**: Directory to save plots
7. **README**: This document providing an overview of the project, its objectives, methodology, and key components.

## Getting Started
To get started with the "Time to MA" project, follow these steps:

1. Clone the project repository to your local machine.
2. Install the required dependencies and libraries specified in the project documentation.

...to be continued

## Contributors
- Razmik Manukyan

## License
This project is licensed under the *ClinSoft License*