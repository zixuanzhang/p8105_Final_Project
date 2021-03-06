Final Project Proposal
================
2018-11-07

Group Member
------------

-   Jiayi Shen (js5354)
-   Shan Jiang (sj2921)
-   Jie Yu (jy2944)
-   Eleanor Zhang (zz2602)
-   Guojing Wu (gw2383)

Project Title
-------------

Where you are going: Analysis of Data Science Job Outlook of United States Labor Market in 2018.

Motivation
----------

According to [the 2018 August Linkedin Workforce Report](https://economicgraph.linkedin.com/resources/linkedin-workforce-report-august-2018), there is a rising demand of work forces with data science skills across industries in United States. As more institutions and industry sectors rely on big data to make operations and decisions, the national shortage of data scientists has been reported to be 151,717. Some regions are in acute shortage of workforce in data science such as New York city, San Francisco and Los Angeles, etc.

Given that graduate students entering into the job market with bachelor, master, or PhD degree are actively seeking jobs locally or nationwide, we want to utilize the 7000 job posting information scraped from [Indeed](https://www.indeed.com/jobs?q=data+science&start=10) in order to investigate the data science job market in U.S. Particularly, we are interested in areas with job opportunities, what skills employers are looking for, and different qualifications and requirements for different types of job titles . We are hoping these exploratory analysis will help students better taylor at their skill building and job hunting in the future.

Intended Final Product
----------------------

#### Part I Background and Introduction

1.  Data science vs. other occupations

2.  geographic distribution of mean total employment, mean salary by state, mean job per thousands

Background check of the job market dynamics for the past decade:

-   Graph : Computer and mathematical occupations trend from 2007 to 2017

-   Graph : Comparison of average annual wage estimates for computer and mathematical occupations and that across all industries between 2007 and 2017.

#### Part II Core Analysis (focus on data science field)

##### Geographical Visualization

-   Graph (a bubble map) : Which regions are in high demand of labor with data science skills?

-   Potential suggestions for new graduates: local shortages for data science skills, will this cause a migration?

##### Descriptive Part (Table or Graph)

-   Degree preference? (bar)

-   master
-   phd
-   high school
-   bachelor
-   other

-   Related background fields (tree map)

-   What programming / statistical tools are most required? (bar)

-   What kinds of data science skills are most in need? (bubble plot)

-   Comparison of requirements between "top 100" largest industrial corporations and non "top 100" companies in U.S. (dashboard)

Anticipated Data Source
-----------------------

-   [National Occupational Employment Status and Wage Estimates from US Bureau of Labour Statistics (using data covering 2007 until 2017)](https://www.bls.gov/oes/tables.htm)

-   [Data Scientist Job Posts in the U.S on Indeed Website in 2018 August](https://www.kaggle.com/sl6149/data-scientist-job-market-in-the-us#alldata.csv)

-   [The 500 Largest Industrial Corporations in the U.S., Published by Fortune magazine (updated by October 5, 2018)](https://catalog.data.gov/dataset/fortune-500-corporate-headquarters)

Challenges
----------

-   Data Tidying:
    -   detect key strings in the job descriptions.
    -   Reduce redundancy
    -   Deal with missing value
    -   Classification of job titles
-   Final product
    -   Matching statistical analysis result to the map

Planned Timeline
----------------

| Scheduled Tasks                   | Due dates                 |
|-----------------------------------|---------------------------|
| Scrape data and tidy              | Nov. 16 (Friday)          |
| Analyse and produce visualization | Nov. 23 (Friday)          |
| Draft report                      | Nov. 30 (Friday)          |
| Make webpage and screencast       | Dec. 4 (Tuesday)          |
| Final editing                     | Dec. 6 (Friday by 4 p.m.) |
