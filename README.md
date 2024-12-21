# Statistical Data Visualization with R and ggplot2

Welcome to my repository summarizing key concepts, techniques, and applications learned during the course on statistical data visualization using R and ggplot2. This repository showcases my understanding of statistical methods and their practical application to real-world data.

---

## **Topics Covered**

### **1. Data Wrangling**  
- Importing, inspecting, and understanding data structures using R (`read_csv`, `glimpse`, `head`).
- Transforming and filtering datasets using the tidyverse functions like `dplyr::filter`, `mutate`, and `rename`.
- Combining datasets through joins (`inner_join`, `left_join`) and summarizing data with `group_by` and `summarize`.

**Example Output:**
![Data Wrangling Example](images/data-wrangling.png)

---

### **2. Data Visualization**
#### Barplots  
- Creating basic barplots to compare categories using `geom_bar` and `geom_col`.
- Improving readability by reordering axes and using appropriate labels.

**Example Output:**
![Barplot Example](images/barplot.png)

#### Distributions  
- Visualizing data distributions using histograms and density plots.
- Overlapping distributions with fitted models using `stat_function`.

**Example Output:**
![Distribution Example](images/distributions.png)

#### Scatter Plots and Regression  
- Exploring relationships between two continuous variables with scatter plots (`geom_point`).
- Adding linear regression lines using `stat_smooth(method = "lm")`.

**Example Output:**
![Scatter Plot with Regression](images/scatter-regression.png)

#### Spatial Data  
- Visualizing geographic data using `geom_polygon` and `coord_map`.
- Merging spatial and statistical data to create informative maps.

**Example Output:**
![Spatial Data Example](images/spatial-data.png)

---

### **3. Statistical Methods**
#### Correlation Analysis  
- Calculating and visualizing Pearson’s and Spearman’s correlations to explore relationships among variables.

**Example Output:**
![Correlation Matrix](images/correlation-matrix.png)

#### Linear Regression  
- Performing simple and multiple linear regression using `lm()` for predictive modeling.

**Example Output:**
![Linear Regression Example](images/linear-regression.png)

#### Comparative Analysis  
- Comparing groups with boxplots and violin plots to understand distributions and variability.

**Example Output:**
![Boxplot Example](images/boxplot.png)

---

## **Final Project**
As part of the course, I applied all learned concepts to analyze the relationship between Taiwan’s bio-industry index and external factors (e.g., COVID-19 statistics).

**Key Analysis Outputs:**
1. Time series plot of the bio-stock index before and after COVID.  
   ![Time Series Plot](images/time-series.png)

2. Correlation matrix showing relationships between bio-index and other factors.  
   ![Final Correlation Matrix](images/final-correlation.png)

---

## **How to Use This Repository**
1. Clone the repository:
   ```bash
   git clone https://github.com/Dewey-Wang/R-statistic.git
