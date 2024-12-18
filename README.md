![Logo of the project](https://cienciadedatosysalud.org/wp-content/uploads/logo-Data-Science-VPM.png)

# AtlasVPM-socioeconomic

## Objetive

The main objective was to provide a method for the reuse of INE socioeconomic data, reconstructing the SNS primary care areas (PCAs) from INE census tracts.

## Method:

Allocating socioeconomic census data into PCAs requires creating new PCAs out of the geographical vectors of the census tracts. This process, well described in geo-statistics, comprises seven steps: 
 1) aligning census and health maps; 

 2) assigning census areas to the corresponding PCAs; 

 3) evaluating nesting of census tracts into PCA areas; 

 4) re-assignment of areas with poor nesting; 

 5) reconstruction of the PCA map; 

 6) allocating socio-economic values to the new PCAs; and, 

 7) internal validation of the results.

All analyses were performed using R Statistical Software (v4.3.2; R Core Team 2021). 


## Results 

- [The vectorial map reconstructed PCAs (shapefile)](https://github.com/cienciadedatosysalud/AtlasVPM-socioeconomic/blob/main/outputs/1_outputs/new_PCA_ESP_4258_2022_all.shp) can be found in folder 'outputs/1_outputs/';

- [The CSV file](https://github.com/cienciadedatosysalud/AtlasVPM-socioeconomic/blob/main/outputs/2_outputs/census_tracts_intersection_pca_point_on_surface_with_percent_2022_2020_after_corrected_with_indicators.csv), including the census tract information and corresponding PCAs can be found in folder 'outputs/2_outputs/';

- Different figures as a matter of illustration can be found in folder 'outputs/2_outputs/'.

## License

<a href="https://creativecommons.org/licenses/by/4.0/" target="_blank" ><img src="https://img.shields.io/badge/license-CC--BY%204.0-lightgrey" alt="License: CC-BY 4.0"></a>
