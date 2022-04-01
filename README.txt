Code created by Sophia Bryson (sab159@duke.edu) for Masters Project at the Nicholas School of the Environment at Duke University. 
MP completed with Blair Johnson (bsj9@duke.edu) for client Internet of Water, 2021 - 2022.

These scripts create the data powering the dashboard found at https://www.arcgis.com/apps/dashboards/0dbe111a2c1542a4a1ff01387b037d13 examining the spatial distribution of Water Quality Portal data. 

Scripts should be run in this order: 
0_SetEnvirons.R - before running any other scripts, to configure workspace
1_InitialDataPull.R - to create the initial dataset and to update the dataset whenever new ACS datasets are available from the Census Bureau
2_UpdateData.R - to analyze and create final outputs after script 1 is run and to update the monitoring coverage data between ACS data releases
3_DataUpload.py - after each update to update the hosted feature layers on AGOL powering the dashboard with the local files created in the prior script
3_DataAnalysis.Rmd - after each update or as desried to generate a summary document analysing the distribution of monitoring data

** DOCUMENTATION FOLDER: ***
** ADD METHODS DOCUMENT **
** ADD PPT WITH FULL NOTES ** 

