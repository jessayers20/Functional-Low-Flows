# Functional-Low-Flows

# The scripts in this folder: 1. Calculate low flow metrics and 2. Model dry season and low flow metrics

# 1. Calculate low flow metrics

Script 1_Calculate_low_flow_metrics_dry_season

Calculates the minimum 7-day moving average, minimum 7-day occurence date, number of zero flow days, start of first no flow day, flow duration curve metrics.
These metrics are calculated for all reference CA stream gages included in the California Environmental Flows Framework (CEFF). Script takes daily discharge data downloaded from USGS and calculates metrics over the CA dry season (June 1-December 31). 


# 2. Model dry season and low flow metrics

2_Create_new_variable_file_dry_season_models_all_COMIDS

This script updates the metric file used as input to model CEFF metrics. The updated file includes key variables to increase performance of the dry season metrics (climate variables for the overlapping dry season) and variables from 2015 to 2021. The file binds together each flow metric (including new low flow metrics) and the variables that are input into the model. Output is one csv file. 

3_Model_low_flow_metrics

Script that models all dry season metrics for CEFF (baseflow magnitude 50 and 90, dry season start date, dry season duration, minimum 7-day moving average magnitude, and minimum 7-day occurence date). We train models with leave one out cross validation to get model performance. This script runs through a loop of each dry season metric and site to get an output of predictions.
