# LSMPA Fishing Effort Dashboard


This dashboard explores changes in industrial fishing effort at various timescales in regions with established Large-Scale Marine Protected Areas (LSMPAs), and relies on data downloaded from Global Fishing Watch, accessed under Creative Commons.

# File Structure
## Dashboard Specific Files
The Shiny-R script titled "LSMPA_Fishing_Effort_Dashboard.R" houses the code to run the LSMPA Fishing Effort Dashboard using Shiny-R, relying on data dependencies. The code relies on filtered Global Fishing Watch Fishing Effort and vessel information data and geospatial data dependencies (shapefiles). The folder titled "Geospatial Boundaries" includes examples of the geospatial data dependencies (does not include the large folder "Global Exclusive Economic Zones", which can be downloaded from Flanders Marine Institute — see "Data Sources" below). The folder titled "LSMPA Specific Datasets" includes examples of filtered Global Fishing Watch Fishing Effort data. The folder titled "Fishing Vessel Reference Information" includes Global Fishing Watch vessel information data.


## Data Processesing Files
A series of scripts were used to process Global Fishing Watch data prior to dashboard analysis. These scripts are included in the folder titled "Processing Files."

## Result Image Files
Images of example resulting dashboard and analysis images are included in the folder titled "Result Images."

# Data Sources
## Fishing Effort and Vessel Characteristics Data: Global Fishing Watch
Copyright 2021, Global Fishing Watch, Inc. Accessed on 30 June 2021. https://globalfishingwatch.org/data-download/datasets/public-fishing-effort.

Global Fishing Watch data is licensed under a Creative Commons Attribution-ShareAlike 4.0 International license (https://creativecommons.org/licenses/by-sa/4.0/) and code under an Apache 2.0 license (http://www.apache.org/licenses/LICENSE-2.0)
                        
D.A. Kroodsma, J. Mayorga, T. Hochberg, N.A. Miller, K. Boerder, F. Ferretti, A. Wilson, B. Bergman, T.D. White, B.A. Block, P. Woods, B. Sullivan, C. Costello, and B. Worm. "Tracking the global footprint of fisheries." Science 361.6378 (2018). (http://science.sciencemag.org/content/359/6378/904                        
                        
## Exclusive Economic Zone Boundaries: Flanders Marine Institute
Flanders Marine Institute (2019). Maritime Boundaries Geodatabase: Maritime Boundaries and Exclusive Economic Zones (200NM), version 11. Available online at https://www.marineregions.org/. https://doi.org/10.14284/386
                        
Flanders Marine Institute (2019). Maritime Boundaries Geodatabase: Contiguous Zones (24NM), version 3. Available online at https://www.marineregions.org/https://doi.org/10.14284/384
                       
Flanders Marine Institute (2019). Maritime Boundaries Geodatabase: Territorial Seas (12NM), version 3. Available online at https://www.marineregions.org/.https://doi.org/10.14284/387
                        
                        
## Large-scale Marine Protected Area Boundaries: Protected Planet
UNEP-WCMC and IUCN (2021), Protected Planet: The World Database on Protected Areas (WDPA) and World Database on Other Effective Area-based Conservation Measures (WD-OECM) [Online], August 2021, Cambridge, UK: UNEP-WCMC and IUCN. Available at: www.protectedplanet.net.
                        
## Stanford Center for Ocean Solutions Report on the Palau National Marine Sanctuary
Palau International Coral Reef Center and the Stanford Center for Ocean Solutions, “Palau’s National Marine Sanctuary: Managing Ocean Change and Supporting Food Security,” PICRC, December 2019. Available at: http://picrc.org/picrcpage/palau-national-marine-sanctuary and https://oceansolutions.stanford.edu/pnms-report.
                        
