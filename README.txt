README
There are a few interactive published items:
	R Shiny Application:
		http://ajatkins02.shinyapps.io/MgmtRequest

	Interactive Map of Insured Locations:
		https://rpubs.com/ajatkins02/1245703



Management Request Workbook.xlsx
	Main Excel Deliverable. The historical hurricane tasks are completed here, as well as a major chunk of the management requests and underwriting report
	There should be notes within the document that will explain some of the data tables to the user.

CAS_Project_Script.R
	Main R code. This helps deal with a number of the management requests. Outputs from this file have already been moved to the excel 
	document or, for the maps, has been published online.

app.R
	Source code for the R Shiny application. The published version should work independently, but this (with the corresponding data) should be 
	sufficient to run the application locally.


Hurricane-Dataset.xlsx
	this is the name of the data that both the R files pull from. The code will pull from this file if its in RStudio's working directory.


exposures_mod.xlsx
hurricaneRiskWind.xlsx
	These files are the raw outputs from CAS_Project_Script.R; The data has already been moved to the main excel workbook.
	