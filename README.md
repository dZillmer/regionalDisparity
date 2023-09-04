# Analysis of BMI Disparity Between U.S. Census Regions

Conducted with a civilian PhD in the Department of Mathematical Sciences (D/Math) at the United States Military Academy, West Point, NY, in conjunction with several other researchers, to investigate lifestyle factors that could explain the disparity in  regional BMI prevalence. Our research question: are there explanatory lifestyle factors that contribute or partially explain the markedly higher obesity rates in the U.S. South and Midwest than the Northeast and West? 

# Data
Data come from the 2004-2018 National Health Interview Survey (NHIS) via the Integrated Public Use Microdata Series (IPUMS) website. The IPUMS NHIS dataset consists of self-reported responses to questions regarding health health-related behavior harmonized across NHIS waves.  A total of N=444,743 sample adults  were used for this analysis.

This dataset is 251MB, and so is not uploaded here for space's sake.... which is obviously too big for here. The file is available from me upon request!

# Cleaning and Analysis
To read the data into R, I wrote a script to read the fixed-width formatting provided by NHIS based on the provided codebooks. Because the data were weighted census data, the R package `survey` was used extensively. 

# My Work
While I was only one member of the teach which worked on this research grant, my part was to support the team with work in R: Exploratory Data Analysis, verification of primary findings from the other researchers (who were primarily working in SAS), and investigation of additional model types to answer the research question. I used three primary types of models:
- Linear Regression (to predict BMI, and then classify obesity),
- Logistic Regression (to predict obesity),
- Decision Trees (to predict obesity).

# Results
The work was aimed at academic publication in response to a grant obtained by one of the civilian faculty in D/Math. As of September 2023, I understand we still do not have a formal publication acceptance. 
