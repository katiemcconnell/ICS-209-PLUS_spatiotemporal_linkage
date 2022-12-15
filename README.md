# ICS-209-PLUS linkage to spatiotemporal units

[![DOI](https://zenodo.org/badge/547538267.svg)](https://zenodo.org/badge/latestdoi/547538267)

## Description of the Data
This repository stores source codes that link wildfire records from the ICS-209-PLUS dataset to commonly-used spatial (counties, census tracts, and census block groups) and temporal (quarters, years) units in the United States from 1999 to 2020. Data are integrated from administrative records retained in the U.S. National Incident Management System/Incident Command System ICS-209 Incident Status Summary Forms, and fire burn footprints taken from the [Monitoring Trends in Burn Severity (MTBS)](https://www.mtbs.gov/) database and the [FIRED CONUS + AK](https://scholar.colorado.edu/concern/datasets/nv935382p) database. The final data produced by these codes are available [here](https://figshare.com/articles/dataset/ICS209-PLUS_Cleaned_databases/8048252/10).

The ICS-209-PLUS spatiotemporal linkage allows researchers to observe wildfire characteristics and impact metrics at the Incident-GEOID-Quarter-Year level. In this way, they are distinct from the primary ICS-209-PLUS incident tables, which report wildfire characteristics and impact metrics at the Incident-level and daily report-level. The ICS-209-PLUS spatiotemporal linkage is designed primarily for social and health science researchers who aim to examine wildfire metrics in tandem with relevant social data (e.g. U.S. Census, American Community Survey, etc.). For example, see [McConnell _et al_ 2021.](https://www.clevelandfed.org/en/newsroom-and-events/publications/working-papers/2021-working-papers/wp-2129-effects-of-wildfire-destruction-on-migration.aspx) We strongly recommend that users read the full data documentation in the corresponding publication (St. Denis *et al.* Under Review). Understanding details on the association of wildfire metrics with spatial and temporal units within the same incident is critical for appropriate data use. 

  <br />

<figure>

<p float="center">
  <img src="https://github.com/katiemcconnell/ICS-209-PLUS_spatiotemporal_linkage/blob/main/Figure_Github/Texas_Complex_2011.png" width="900" />
</p>

<figcaption align = "center"><b>Three scales of the ICS-209-PLUS spatiotemporal linkage shown for the Southeast Texas Fire Complex (2011). Grey polygons show relevant spatial units and red polyogons show MTBS wildfire footprints. </b></figcaption>
  
  <figure>
    



## Overview of the Code
This code ascribes each wildfire incident to relevant spatial and temporal units, drawing on spatial details from MTBS and FIRED databases of wildfire footprints and from ICS details on each incident's point of origin (POO). There is substantial overlap between the MTBS and FIRED footprints for wildfire incidents linked to both (see figures below). By including footprints from the FIRED database, we link more than 7,000 additional wildfire incidents to spatial burn footprints that are not included in the MTBS database. Many of these additions are smaller incidents that are below the MTBS acreage thresholds for inclusion. For incidents in which no corresponding MTBS or FIRED footprint is available, its POO is used to designate the spatial unit in which the fire originated. Through this process, we associate 97.9% of relevant ICS wildfire incidents with corresponding spatial units. Originating codes for the ICS-209-PLUS database were created by Lise St. Denis and are available [here](https://github.com/lisestdenis/ics209plus). Originating codes linking the FIRED database to the ICS-209-PLUS database were created by Maxwell Cook and are available [here](https://github.com/maxwellCcook/ics209-plus-fired).

  
  <br />
  <br />
  
  <figure>

<p float="center">
  <img src="https://github.com/katiemcconnell/ICS-209-PLUS_spatiotemporal_linkage/blob/main/Figure_Github/Cedar_2003.png" width="380" />
  <img src="https://github.com/katiemcconnell/ICS-209-PLUS_spatiotemporal_linkage/blob/main/Figure_Github/Thomas_2017.png" width="400" /> 
</p>

<figcaption align = "center"><b>Spatial association procedure for the ICS-209-PLUS spatiotemporal linkage. MTBS fire footprints shown in red, FIRED fire footprints shown in blue, overlap between MTBS and FIRED footprints shown in purple, and corresponding census tracts shown in grey. </b></figcaption>
  
  <figure>
  
<br />
<br />    

The following codes are stored in this repository:
* **ICS_Spatial_Counties_Github.R**: processing codes for county-level spatiotemporal data
* **ICS_Spatial_Tracts_Github.R**: processing codes for tract-level spatiotemporal data
* **ICS_Spatial_Block_Groups_Github.R**: processing codes for census block group-level spatiotemporal data
* **ICS_spatiotemporal_link_to_ics209plus_Github.R**: connects ICS-209-PLUS spatiotemporal observations to full suite of variables available in the primary ICS-209-PLUS incident tables
* **Figure2_Github.R**: Code used to create manuscript figure 2
    
    
 The primary processing codes stored in this repository conduct the following core steps:

1. Filter ICS-209-PLUS dataset to remove controlled burns and Initial Attack fires.
2. Manually clean ICS-209-PLUS data based on external research on specific incidents.
3. Join MTBS wildfire footprints to ICS-209-PLUS data. Spatially join MTBS wildfire footprints to overlapping spatial units (counties, tracts, and census block groups).
4. Join FIRED wildfire footprints to ICS-209-PLUS data. Spatially join FIRED wildfire footprints to overlapping spatial units (counties, tracts, and census block groups).
5. For ICS-209-PLUS wildfire incidents that did not have either an MTBS or FIRED footprint, use the POO to spatially join remaining incidents to overlapping spatial units (counties, tracts, and census block groups).
6. Combine spatial units designated through MTBS, FIRED, and POO spatial joins.
7. Based primarily on ICS-209-PLUS's Discovery Date and Cessation Date, allocate each wildfire incident to its corresponding quarter(s) and year(s). The resulting dataset is observed at the Incident-Spatial Unit-Quarter-Year level.
8. To associate all ICS-209-PLUS variables with the spatiotemporal linkage, join ics_spatial files with ICS-209-PLUS incident tables using INCIDENT_ID as the join key.

## References
Eidenshink, Jeff, Brian Schwind, Ken Brewer, Zhi-Liang Zhu, Brad Quayle, and Stephen Howard. “A Project for Monitoring Trends in Burn Severity.” *Fire Ecology* 3, no. 1 (June 2007): 3–21. https://doi.org/10.4996/fireecology.0301003.

Mahood, Adam L., Estelle J. Lindrooth, Maxwell C. Cook, and Jennifer K. Balch. “Country-Level Fire Perimeter Datasets (2001–2021).” *Scientific Data* 9, no. 1 (July 30, 2022): 458. https://doi.org/10.1038/s41597-022-01572-3.

Steven Manson, Jonathan Schroeder, David Van Riper, Tracy Kugler, and Steven Ruggles. IPUMS National Historical Geographic Information System: Version 17.0 Minneapolis, MN: IPUMS. 2022. http://doi.org/10.18128/D050.V17.0

St. Denis, Lise A., Karen C. Short, Kathryn McConnell, Maxwell C. Cook, Nathan P. Mietkiewicz,  Mollie Buckland, and Jennifer K. Balch. “All-Hazards Dataset Mined from the US National Incident Management System 1999–2020.” Under review at *Scientific Data*.

St. Denis, Lise A., Nathan P. Mietkiewicz, Karen C. Short, Mollie Buckland, and Jennifer K. Balch. “All-Hazards Dataset Mined from the US National Incident Management System 1999–2014.” *Scientific Data* 7, no. 1 (February 21, 2020): 1–18. https://doi.org/10.1038/s41597-020-0403-0.

Williams, T., Mahood, A. L., McGlinchy, J., Cook, M. C., St. Denis, L. A., Mietkiewicz, N. P., & Balch, J. K. (2020). FIRED CONUS: Events. University of Colorado Boulder. https://doi.org/10.25810/3HWY-4G07

## Funding Acknowledgement
This research was funded by the National Science Foundation Sociology Award #2001261, “The Effects of Wildfire Damage on Housing Access and Migration” and the National Science Foundation Sociology and Human-Environment and Geographical Sciences (HEGS) Award \#2117405, “Analysis of Impacts of Environmental and Natural Hazards on Human Migration.” Any opinions, findings, and conclusions or recommendations expressed in this material are those of the author and do not necessarily reflect the views of the NSF.

## Recommended Citation
St. Denis, Lise A., Karen C. Short, Kathryn McConnell, Maxwell C. Cook, Nathan P. Mietkiewicz,  Mollie Buckland, and Jennifer K. Balch. “All-Hazards Dataset Mined from the US National Incident Management System 1999–2020.” Under review at *Scientific Data*.
