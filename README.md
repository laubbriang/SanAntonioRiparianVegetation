# SanAntonioRiparianVegetation
Data and code for the study of how woody riparian vegetation communities vary along a gradient of urbanization in headwater ephemeral streams in San Antonio, Texas

The repository contains the following files:

AnalysisAndPlottingCode.R which contains the R code for running the analysis and visualizations

BasalAreaRaw.csv which contains total basal area (cm2) for each species identified at each site across all sample quadrats

BasalAreaSitebyTrait.csv which contains the matrix of community trait scores for each sampling site based on basal area measurements. This file was used to perform the environmental fitting of traits to the NMDS analysis of basal area

CanopyCoverRaw.csv which contains total number of canopy points measured for each species identified at each site across all transects

CanopyCoverSitebyTrait.csv which contains the matrix of community trait scores for each sampling site based on canopy cover measurements. This file was used to perform the environmental fitting of traits to the NMDS analysis of canopy cover

DiversityAndRichness.csv which contains values for Shannon diversity and richness at each site measured by canopy cover, basal area, and stem count. Also includeds watershed impervious cover (%), average duration dry (number of days between flow events), average number of flow events per year, and watershed area (km2) for each study site. This file was used to perform regression analysis between impervious cover and diversity and richness and between flow metrics and diversity and richness

GeoSurfaceBasalAreaSqRtRelativeAbundanceProportion.csv which contains the square-root transformed values of relative abundance (as a proportion) of each species at each geomorphic surface at each site as measured by basal area. This file was used to conduct the NMDS and PERMANOVA analysis at the geomorphic unit scale for basal area

GeoSurfaceCanopyCoverSqRtRelativeAbundanceProportion.csv which contains the square-root transformed values of relative abundance (as a proportion) of each species at each geomorphic surface at each site as measured by canopy cover. This file was used to conduct the NMDS and PERMANOVA analysis at the geomorphic unit scale for canopy cover

GeoSurfaceStemCountSqRtRelativeAbundanceProportion.csv which contains the square-root transformed values of relative abundance (as a proportion) of each species at each geomorphic surface at each site as measured by canopy cover. This file was used to conduct the NMDS and PERMANOVA analysis at the geomorphic unit scale for stem count

SpeciesByTraitMatrix.csv which contains the numerical scores for each species for drought tolerance, heat tolerance, and water use and nonnative status (1 = nonnative)

SqrtBasalAreaRelativeAbundanceProportion.csv which contains the square-root transformed values of relative abundance (as a proportion) of each species at each study site as measured by basal area. This file was used to conduct the NMDS analysis at the site level for basal area

SqrtCanopyCoverRelativeAbundanceProportion.csv which contains the square-root transformed values of relative abundance (as a proportion) of each species at each study site as measured by canopy cover. This file was used to conduct the NMDS analysis at the site level for canopy cover

SqrtStemCountRelativeAbundanceProportion.csv which contains the square-root transformed values of relative abundance (as a proportion) of each species at each study site as measured by stem count. This file was used to conduct the NMDS analysis at the site level for stem count

StemCountRaw.csv which contains total stem count for each species identified at each site across all sample quadrats

StemCountSitebyTrait.csv which contains the matrix of community trait scores for each sampling site based on stem count measurements. This file was used to perform the environmental fitting of traits to the NMDS analysis of stem count

TraitDataRelativeAbundances.csv which contains the relative abundances of all species categorized at a given level of each trait for each site and includes data for canopy cover, basal area, and stem count. This file was used to conduct the regression analysis of whether community abundance of different trait categories were related to watershed impervious cover

land use and water part2.csv which contains land use data for each study site, with land use data from the 2019 NLCS. Also includes average duration dry (number of days between flow events) and average number of flow events per year for each site
