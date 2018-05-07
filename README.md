# jr_analytics
Web application to make analysis of jobs vacancies and reviews companies.

# Load Data
The data are save in .RDATA object, is necessary execute loadData.R to load data in CSV format.

Execute: ```Rscript loadData.R```

# Extract, Transform and Load (ETL)
To make ETL just open the ETL directory, there are differents files:
* **dataMining.R**: script to apply data mining;
* **jobs.R**: script to collect (web scraping), transform and save data;
* **reviews.R**: script to collect (web scraping), transform and save data;
* **utils.R**: differents functions

# ml_classification
Directory with script to make the classification machine learning in review.

