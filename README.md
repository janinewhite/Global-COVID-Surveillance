# Global COVID Surveillance
 
This is the procedure to complete the daily and weekly updates of GASSP charts.

## Prerequisite Software

* Github Desktop
    * Install: https://desktop.github.com/
    * Repository: https://github.com/janinewhite/Global-COVID-Surveillance
* Python
    * Install: https://www.python.org/downloads/
* Anaconda with Jupyter Notebook
    * Install: https://www.anaconda.com/products/individual
* R
    * Install: https://cran.r-project.org/
* R Studio
    * Install: https://rstudio.com/products/rstudio/download/#download
* Tableau Desktop (TCMV-643B-AA60-9E68-0BD0)
    * Install: https://www.tableau.com/products/desktop/download

## Procedure

Sundays perform the weekly and daily updates.  All other days perform only the daily update.

### Weekly Update (Sundays)
1) Open Jupyter Notebook.
2) Navigate to Documents/GitHub/Global-COVID-Surveillance/scripts.
3) Open Cleaned Input 2.ipynb.
4) Update last_saturday.
5) Update last_sunday.
6) Select Kernel > Restart & Run All.
7) Open Windows folder to C:\Users\jwg4880\Documents\GitHub\Global-COVID-Surveillance\data\cleaned.
8) Right click on GeneralGmmCode12-02-20Cases and select Open with > R Studio.
9) Select Session > Restart Session.
10) Click in the Code Windows.
11) Select all of the Code.
12) Select Code > Run Selected Lines.
13) When the lines have completed running, verify that output files have been created in cleaned folder.
14) Open Box folder, https://app.box.com/folder/122135816502?s=xx0klt7b0uosjn6i16s7yw0w7evzhw7j
15) Create a folder with the name in the format <yyyymmdd>.
16) Upload the files updated that day and the R script to the Box folder.
17) Open Daily Update 2.
18) Update file_date in Daily Update 2 to equal the date of the files.
19) Save Daily Update 2.
### Daily Update (Every Day)
20) Open Jupyter Notebook.
21) Navigate to Documents/GitHub/Global-COVID-Surveillance/scripts
22) Open Daily Update 2.
23) Select Kernel > Restart & Run All, then wait for all of the cells to complete.
24) Logout of all Jupyter notebooks and close Chrome.
25) Open Tableau.
26) Open Global SARS-CoV-2 Surveillance Polivy, Persistence, and Transmission.twb.
27) Select Data > Refresh all extracts ...
28) Click Refresh and wait for the data to finish loading.
29) Click Close.
30) Select Server > Tableau Public > Save to Tableau Public ..., then wait for the refreshed web page to pop up.
    * Tableau Public Workbook: https://public.tableau.com/profile/lori.post#!/vizhome/GlobalSARS-CoV-2SurveillancePolicyPersistenceandTransmission/Global
31) Select File > Save.
32) Select File > Close.
33) Select File > Exit.
34) Open Github Desktop.
35) Enter Summary, 'Daily update' or 'Weekly update'.
36) Click Commmit to master.
37) Click Push Commit.
38) Close Github Desktop.
