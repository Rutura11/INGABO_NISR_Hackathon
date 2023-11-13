# INGABO_NISR_Hackathon


# 2022 Surveys Interactive Dashboard

## Overview

The 2022 Surveys Interactive Dashboard is a Shiny web application developed for the NISR (National Institute of Statistics of Rwanda) hackathon competition. This dashboard provides an interactive and visual representation of key indicators from the 2022 Labor Force Survey (LFS), Consumer Price Index (CPI), and Gross Domestic Product (GDP) for Rwanda.

## Features

- **Home Tab**: Introduces users to the purpose and significance of the dashboard, offering insights into the economic landscape of 2022 through the LFS, CPI, and GDP surveys.

- **LFS Trends Tab**: Explores trends in participation rate, employment rate, unemployment rate, and overall labor force dynamics for the years 2019-2022.

- **2022 LFS Indicators Tab**: Displays population, labor force, employed, unemployed, and outside labor force statistics for 2022.

- **2022 CPI Indicators Tab**: Visualizes the Consumer Price Index (CPI) from 2009 to 2022, allowing users to explore the behavior of CPI over time.

- **2022 GDP Indicators Tab**: Presents macro-economic aggregates from 1999 to 2022, enabling users to select and explore specific GDP indicators.

- **Map Visualization Tab**: Utilizes leaflet to display a map of Rwanda with district-level participation rates for 2022. Also includes a table comparing participation rates for multiple years.

## Data Sources

- **Labor Force Survey (LFS) Data**: Loaded from the "RLFS Tables_ Annual_2022" Excel file.
- **Consumer Price Index (CPI) Data**: Transposed and processed from the "CPI_time_series_November_2022" Excel file.
- **Gross Domestic Product (GDP) Data**: Loaded from the "GDP.xlsx" Excel file.
- **Geospatial Data**: District-level map data loaded from the "geoBoundaries-RWA-ADM2_simplified.geojson" GeoJSON file.

## External Links

- [LFS 2022 Report](https://www.statistics.gov.rw/publication/1919)
- [GDP 2022 Report](https://www.statistics.gov.rw/publication/1914)
- [CPI 2022 Report](https://www.statistics.gov.rw/publication/1873)

## Installation and Execution

1. Install the required R packages by running the necessary installation commands.
2. Load the required data files, including LFS, CPI, and GDP data.
3. Run the Shiny app by executing the `shinyApp(ui, server)` command.

## Acknowledgments

This dashboard was developed by the "Ingabo" group for the NISR hackathon competition (2023).


