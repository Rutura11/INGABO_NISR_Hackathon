

# Interactive Dashboard for Economic Surveys (Year 2022)

## Overview

This interactive dashboard provides visualizations and insights into key economic surveys conducted in the year 2022. The dashboard covers the following surveys:

- **Gross Domestic Product (GDP)**
- **Consumer Price Index (CPI)**
- **Labour Force Survey (LFS)**
- **Seasonal Agriculture Survey (SAS)**

## Features

- **Home Tab:**
  - Overview of the purpose and content of the dashboard.

- **GDP Tab:**
  - Visualizations of macro-economic aggregates.
  - Selection of specific GDP indicators.

- **LFS Tab:**
  - Different tabs providing insights into the Labour Force Survey, including participation maps, trends, and key indicators.
  - Charts and statistics related to labour force education, distribution, and more.

- **OLFS Tab:**
  - Insights into the Outside Labour Force, including education attainment, population status, and age distribution.

- **LFU Tab:**
  - Visualizations related to Labour Force Underutilization, including underemployment status, education breakdown, and age distribution.

- **CPI Tab:**
  - Consumer Price Index trends over time.
  - Selection of product types and time periods.

- **SAS Tab:**
  - Visualizations and statistics related to the Seasonal Agriculture Survey.

- **SAS1 Tab:**
  - Insights into the cultivated area by the mixed cropping system.

## How to Run

1. Install the required R packages mentioned in the code (`shiny`, `shinydashboard`, `highcharter`, `readxl`, etc.).

```R
install.packages(c("shiny", "shinydashboard", "highcharter", "readxl", ...))
```

2. Load the required data files using the specified file paths.

3. Run the Shiny app:

```R
shiny::runApp("path/to/your/app")
```

4. Access the dashboard through your web browser.

## External Links

- [GDP Report](https://www.statistics.gov.rw/publication/1914)
- [CPI Report](https://www.statistics.gov.rw/publication/1873)
- [LFS Report](https://www.statistics.gov.rw/publication/1919)
- [SAS Report](https://www.statistics.gov.rw/publication/1914)

## Credits

Developed by Ingabo Group

![Ingabo Group Logo](ingabo.png)
