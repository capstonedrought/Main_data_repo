# Main_data_repo

Precipitation data was gathered from the National Centers for Environmental Information National Oceanic and Atmospheric Administration (NCEI NOAA or NOAA), formerly the National Climatic Data Center (NCDC), using their Global Historical Climate Network Daily (GHCN-Daily).  GHCN includes daily land surface observations from around the world. The GHCN-Daily was developed to meet the needs of climate analysis and monitoring studies that require data at a sub-monthly time resolution (e.g., assessments of the frequency of heavy rainfall, heat wave duration, etc.). The dataset includes observations from World Meteorological Organization, Cooperative, and Community Collaborative Rain, Hail & Snow Network (CoCoRaHS). If observed, the station dataset includes max and minimum temperatures, total precipitation, snowfall, and depth of snow on ground. Some U.S. station data are typically delayed only 24 hours. 

For a more in-depth explanation see GHCND_documentation.pdf

Temperature data was gathered from NOAA using their nClimDiv Divisional Temperature-Precipitation-Drought data sets. The major parameters for this data set are sequential climatic division monthly maximum, minimum and average temperature (deg. F. to 10ths, national temperature to 100ths), precipitation (inches to 100ths), Standardized Precipitation Index (SPI), and Palmer Drought Indices (PDSI, PHDI, PMDI, and ZNDX). Period of record is 1895 through latest month available, updated monthly.

For a more in-depth explanation see nClimDiv_DIVISIONAL_TEMPERATURE-PRECIPITATION-DROUGHT.pdf

Soil Moisture data was gathered from NOAA using the files master.f, format2.f, and palmcode.f, from ftp://ftp.cpc.ncep.noaa.gov/htdocs/temp2/ .These routines compute the palmer drought, crop moisture index, and precipitation needed to end a drought for given weeks, year, and climate divisions. this program was obtained from Irma Lewis at NCDC and modified by T Heddingaus.¬¬¬

For a more in-depth explanation see:
master.f
format2.f
palmcode.f
