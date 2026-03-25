# United Kingdom Health Trends Dashboard (1960–2024)

[![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-000000?style=for-the-badge&logo=R&logoColor=white)](https://shiny.rstudio.com/)
[![Bootstrap](https://img.shields.io/badge/Bootstrap-563D7C?style=for-the-badge&logo=bootstrap&logoColor=white)](https://getbootstrap.com/)

## 📊 Overview
This is an interactive dashboard developed in **R** using the **Shiny** framework. It analyzes and visualizes historical health, demographic, and epidemiological trends in the United Kingdom over a 64-year period. 

The dashboard provides a dynamic look at how population structures, survival rates, and healthcare resource allocations have evolved over time.

👉 **[View the Live Dashboard Here](https://savindya322.shinyapps.io/Medical_Stat_Dashboard/)**

---

## 🔍 Key Indicators Analyzed

### Demographic & Fertility Indicators
* **Total Fertility Rate:** Historical trends in births per woman.
* **Total Population:** Overall population growth trends and total count.
* **Population Pyramid:** Interactive, year-by-year breakdown of the UK's age and gender structure.
* **Sex Ratio at Birth:** Proportion of male to female births.
* **Gender Distribution:** Historical and current overall population breakdown by gender.
* **Urban vs. Rural Population:** The proportional shift of people living in urban versus rural environments.
* **Age Dependency Ratio (Old):** The ratio of older dependents (aged 65+) to the working-age population.

### Mortality & Life Indicators
* **Life Expectancy at Birth:** Segmented by male and female cohorts.
* **Child Mortality Trends:** Tracking the decline in Under-5, Infant, and Neonatal deaths per 1,000 live births.
* **Non-Communicable Disease (NCD) Mortality:** Tracking mortality rates from cardiovascular disease, cancer, diabetes, or chronic respiratory disease between exact ages 30 and 70.

### Epidemiological Trends (Incidence & Prevalence)
* **Incidence of Tuberculosis:** Tracking infectious disease markers.
* **Prevalence of Overweight (Children under 5):** Monitoring key health risk factors.
* **Anemia Prevalence:** Tracking the prevalence of anemia among women of reproductive age (ages 15-49).

### Healthcare System
* **Hospital Beds:** Capacity tracking (beds per 1,000 people).
* **Current Health Expenditure:** Total health spending as a percentage of national GDP.
* **Immunization Rate (DPT):** The percentage of children ages 12-23 months immunized for diphtheria, pertussis, and tetanus.

---

## 🛠️ Built With
* **[R](https://www.r-project.org/)** - Core programming language
* **[Shiny](https://shiny.rstudio.com/)** - Web application framework for R
* **[Tidyverse](https://www.tidyverse.org/)** - Data manipulation and cleaning (`dplyr`, `tidyr`)
* **[Plotly](https://plotly.com/r/)** - Interactive graphing library
* **[bslib](https://rstudio.github.io/bslib/)** - Custom Bootstrap theming (Darkly theme)

---

## 📂 Data Sources & References
The data driving this dashboard was sourced from the following reputable institutions:
1. **World Bank Open Data:** Health, Nutrition and Population Statistics – [United Kingdom Data](https://data.worldbank.org/country/united-kingdom)
2. **UK Office for National Statistics (ONS):** Official UK demographic and health data – [ons.gov.uk](https://www.ons.gov.uk)
3. **PopulationPyramid.net:** Historical population structure references – [United Kingdom Pyramids](https://www.populationpyramid.net/united-kingdom)

---
