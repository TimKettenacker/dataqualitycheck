# dataqualitycheck
Data Visualization for Data Quality Checks

Uniserv hosts a variety of services to assess and improve data quality. Unlike the Data Quality Audit, which usually happens on-site at the customer, the data quality check takes places in a data center. Data Quality computations done within a data quality check follow a standardized process that includes
- data consolidation and enrichment
- identification of duplicates
- address qualification and formatting of addresses
- postal validation and correction (including relocation services)
The customer receives a free report containing an aggregated view on these KPIs. This script visualizes all of these KPIs. Eventually, it creates a PDF to be send to the customer, so the graphs are optimized for static output. 

This script is called in the last step of the data quality check from the processing tool. The script is a .rmd file. I have enclosed the raw .R as well as the finished PDF. Basis is an attached .csv containing sample data.


