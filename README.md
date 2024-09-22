---
pdf_document: default
author: "By **Stanislas Mahussi Gandaho**"
date: "2024-22-09"
output:
  pdf_document: default
title: '**_Nest tree selection by White-backed Vultures (Gyps africanus) in South Africa_**'
---


This readme file provides necessary description of data representation and data analysis file structure. We explain some R script file for general understanding. The aim is not to explain every single line code.

The analysis project is organized into different directories (folders), including **`data`**, **`documents`**, **`fonts`**, **`images`**, **`plots`**, **`scripts`**, and **`tables`**.

## 1. Data
This folder has main data collected and generated for data analysis. About data collected in the field, we have *Kempiana_ERAIFT_Tree_Data.xlsx* file and *Manyeleti_ERAIFT_Tree_Data.xlsx* on kempiana and Manyeleti respectively. The first one has different sheet that indicate the date of record. The second sheet in the second file represent the rest of the first file. The structure of both files differ because of data gathering by different group.  
The datasets include detailed information on various tree attributes. Each tree is uniquely identified by a "Tree no" and assessed for vulture presence. Key characteristics recorded include tree species, height (measured with two stacked poles of 8m), circumference at breast height (CBH1 and CBH2), canopy width (taken with type measuring), and number of stems. Health indicators such as debarking, insect damage, fire damage, and fungus presence are noted, alongside GPS coordinates. The damage were recorded on for level; 0 indicating no damage, 1 for light, 2 for medium and 3 for high damage. Additional descriptive notes provide context on the tree's environment or any unique observations.  

The `data` directory has some subdirectories such as:  

* **`aoi`** (area of interest) that has all geographic information data (roads, waterway, area of study boundary, etc. in ESRI shape file format) we needed to present the area of study. To design a complete map, we used a complementary data on South Africa (for example to add ZA miniature). Due to the size of this file, we prefer to don't include in the project resource. Therefore, it can be downloaded [here](http://download.geofabrik.de/africa/south-africa-latest-free.shp.zip). Additionally, the directory contains *grid* shape file of 200x200m, that was generated for the second specific objective analysis (distance sampling application).  

* **`density_data`** contains *bloc* shape file created and used to split the unhealthy tree in cluster. This allowed to compute the distance of unhealthy tree to waterway inside corresponding bloc. Doing that, we avoid to compute the distance of every unhealthy tree to all waterway. The *distance_and_envar.csv* contains the identifier (id) of each grid of 200x200m, the distance of tree inside the grid and near waterway, the number of tree per grid (size), and the total walking distance along waterway (effort) and environment covariates (Normalized Difference Vegetation Index, Distance to Near Water, Distance to Near Road, Land Use Land Cover Class, Digital Terrain Model, and Digital Surface Model). The and and *distance.csv* represent the standard dataset required by distance sampling approach. It has the identifier (id) of each grid of 200x200m, the distance of tree inside the grid and near waterway (distance needed), and the number of tree per grid (size). The *seg_data* table has the sample identifiers which define the segments, the corresponding effort (waterway length) expended and the environmental covariates that will be used to model abundance/density. *obs_data* provides a link table between the observations used in the detection function and the samples (segments). The shape file of *unhealthy_tree* and *grid_with_unh_tree* hold the geographical representation of unhealthy across their corresponding grid respectively.  

* **`tracks`** includes the tracking for several date during the surveys. The date a beginning of each file name represent the date at which the data were recorded.  

## 2. Documents
The `documents` directory contains some document used in parallel during the data analysis. The article or book inside consist of the complement for literature review. These material cover Spatial Data Analysis, Spatial models for distance sampling data: recent developments and future directions, Distance Sampling:
Methods and Applications, and A Literature Review Of Computer Vision Techniques In Wildlife Monitoring.

## 3. Fonts
Inside this directory is Montserrat font used to customize font of graphic designed in R.  

## 4. Images
The images folder contains all images created or taken in the field and used in the thesis during the writing or in the presentation for defending.  

## 5. Plots
The `plots` directory holds the data visualization output.  

## 6. Scripts
It is in the `scripts` folder we have all R code wrote for data processing, analysis and visualization. The prefix digit represents the order of the script in analysis workflow (e.g '3' means the script for third analysis).


>`process_data.R` is the first script to run to get the cleaned data, before to get started with analysis.

* `process_data.R`: This script processes and merges tree survey data from multiple Excel sheets, standardizes the columns, and creates new health and scientific name columns. Initially, it reads all sheets from `Manyeleti_ERAIFT_Tree_Data.xlsx` and `Kempiana_ERAIFT_Tree_Data.xlsx`, converting specific columns to the correct data types and removing unnecessary columns. The script then ensures uniform column names and combines the datasets. A function `char2num` converts categorical health indicators into numeric values, which are used to calculate a `health_level` column categorizing trees as "Healthy," "Unhealthy," or "Very unhealthy." Additionally, it assigns scientific names to tree species and interprets vulture presence codes into descriptive text. The final combined and processed dataset is saved as a CSV file named "survey_data.csv."  

* `trees_scientific_name.R`: Defines a function named tree_sn that takes a tree name as input (tree_name) and returns its corresponding scientific name. It uses a series of case_when statements to match specific tree names to their scientific classifications.  
* `fonts_and_colors.R`: This R script utilizes the showtext package to enable the use of custom fonts in R plotting. It defines variables for effects and color palettes and then loads various styles of the Montserrat font family (Regular, MediumItalic, SemiBold, SemiBoldItalic, Bold, ExtraBold) from specified font files. The showtext_auto() function is employed to automatically detect and substitute these custom fonts in R graphics, ensuring consistent and enhanced typography for generated visualizations or plots.  


* `1_tree_species_proportion_plot.R`: Analyzes tree survey data to generate two visualizations. The first visualization is a circular bar plot depicting the proportion of different tree species, calculated and labeled with percentages. The second visualization is a stacked bar plot showing the distribution of vulture presence (categorized as "No nest", "Large nest" and "Vulture nest") across various tree species. The script reads the survey data from a CSV file, computes the necessary proportions, and uses ggplot2 to create and customize the plots, saving them as JPEG files. Custom fonts and colors are applied to enhance the visual appeal of the charts.  

* `2_tree_health_status_proportion.R`: Generates visualizations of the health status of trees in the survey data. It first calculates the proportion of trees in each health level category ("Healthy," "Unhealthy," "Very unhealthy") and creates a bar plot with these proportions, displaying the health level labels and percentages. The plot is saved as a JPEG file. The script then focuses on "Very unhealthy" trees, calculating the proportion of these trees by species, and creating a horizontal bar plot showing these proportions with species names and percentages. This plot is also saved as a JPEG file. Custom fonts and colors are used throughout for visual enhancement.  

* `3_association_vulture_presence_and_tree_health.R`: Examines the relationship between tree health status and tree characteristics (Height, CBH1, CBH2, and Canopy width) as well as the relationship between vulture presence and tree health. It starts by renaming and scaling relevant columns, then checks the normality of the tree characteristic variables using the Shapiro-Wilk test, finding that none follow a normal distribution. Consequently, the script performs Kruskal-Wallis tests to assess associations between health status and each characteristic, storing the results in a CSV file. The analysis finds no significant associations. Finally, the script uses a Fisher's exact test to check for an association between vulture presence and tree health, finding no significant relationship.  

* `4_Kruskall_Wallis_tree_parameter_vulture_presence.R`: Investigates the association between tree characteristics (Height, CBH1, CBH2, and Canopy width) and vulture presence. The script checks the normality of the tree characteristics and finds that none follow a normal distribution. Consequently, it performs Kruskal-Wallis tests for each characteristic, assessing their association with vulture presence, and stores the results in a CSV file. For variables where the Kruskal-Wallis test is significant, post hoc Dunn tests are conducted. The results are visualized using box plots with significant differences annotated, and the plots are saved as JPEG files. Finally, the script saves the significant Dunn test results in another CSV file.  


## 7. Tables
The `tables` folder contains some analysis output in table format such as density surface model, density surface model prediction, Dunn post hoc test, and Kruskall-Wallis test.  
