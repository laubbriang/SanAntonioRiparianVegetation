# SanAntonioRiparianVegetation
Data and code for the study of how woody riparian vegetation communities vary along a gradient of urbanization in headwater ephemeral streams in San Antonio, Texas

The repository contains the following files:

AnalysisAndPlottingCode_Revised.R which contains the R code for running the analysis and visualizations

DiversityRichnessEnvVars_GeomorphicUnitScale.csv which contains values for Shannon diversity and richness at each geomorphic surface at each site measured by canopy cover, basal area, and stem count. Also includeds watershed area (km2), land use variables (including watershed percent impervious cover), and hydrologic variables (including average time between flood events, flood frequency, and flood duration) for each geomorphic surface type at each site. This file was used to perform 1) regression analysis between land use and hydrologic variables and diversity and richness, 2) regression analysis between land use and hydrologic variables and community-weighted trait scores, and 3) the environmental fitting to NMDS models for both species composition and trait composition

GeoSurfaceBasalAreaRaw.csv which contains total basal area (cm2) for each species identified at each geomorphic surface type at each site

GeoSurfaceBasalAreaSqRtRelativeAbundanceProportion.csv which contains the square-root transformed values of relative abundance (as a proportion) of each species at each geomorphic surface type at each site as measured by basal area. This file was used to conduct the NMDS and PERMANOVA analysis for species composition for basal area

GeoSurfaceCanopyCoverRaw.csv which contains total number of canopy points measured for each species identified at each geomorphic surface type at each site

GeoSurfaceCanopyCoverSqRtRelativeAbundanceProportion.csv which contains the square-root transformed values of relative abundance (as a proportion) of each species at each geomorphic surface type at each site as measured by canopy cover. This file was used to conduct the NMDS and PERMANOVA analysis for species composition for canopy cover

GeoSurfaceStemCountRaw.csv which contains total stem count for each species identified at each geomorphic surface type at each site

GeoSurfaceStemCountSqRtRelativeAbundanceProportion.csv which contains the square-root transformed values of relative abundance (as a proportion) of each species at each geomorphic surface type at each site as measured by stem count. This file was used to conduct the NMDS and PERMANOVA analysis for species composition for stem count

TraitRelativeAbundancesGeomorphicSurfaceScale.csv which contains the matrix of community-weighted trait scores for each geomorphic surface type at each site based on canopy cover, basal area, and stem count measurements. This file provided the community-weighted trait scores for regression analysis between land use and hydrological variables and community-weighted trait scores and was also used to conduct the NMDS and PERMANOVA analyses for trait composition


