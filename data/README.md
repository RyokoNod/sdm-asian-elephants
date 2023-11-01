# The datasets and output files

This folder contains the datasets I used for modeling, the input files I created from them, and the outputs of the models in the ```codes``` folder. Most files here are either tab-separated txt files or CSV, and if not, they are most likely CSVT files, which are metafiles that are used in QGIS. The files that contain data or predictions have a common key , HID, which are unique identifiers for each geospatial grid used in mapping features, labels, and predictions.

Below are descriptions of what you can find inside each folder.

## Climate

The raw files for the features used in modeling. The files that have "ETCCDI" in their names are the climate extreme indices from CCSM4, and the files that have "WC30AS"  in their names are from WorldClim. Present-day and future features have separate files. The files that have either "2061_2080" or "2070" in their names are predicted future data.

The full definitions of features can be found [here](http://etccdi.pacificclimate.org/list_27_indices.shtml) and [here](https://www.worldclim.org/data/bioclim.html).

## Elephas_Maximus

The elephant range and psuedo-absence data, or in other words, the labels. 

* ```Elephas_Maximus_Folds_S10_Natural_O20.txt```: The folds used in spatial CV in the previous research by M. Mechenich. The files ending in ```_short``` are truncated and CSV-converted version of the original and its metafile.
* ```Elephas_Maximus_PA_Natural_O20.txt```: The range and psuedo-absence data for Asian elephants. This also has a CSV-converted version.

## Modeling_Data

The data files I created from the files in the other folders for modeling or invesigating outputs. 

The subfolder ```areas_to_invesigate``` has feature data for areas on Earth where the models seemed to give unrealistic predictions: Greenland, Novaya Zemlya (Islands off the north coast of Russia), and the Sahara desert. The features are separated into different files by present-day features, future features, and feature set (GLM: random CV features, SGLM: spatial CV features).

Other files that can be found here:
* ```areas_to_investigate.ipynb```: The Jupyter notebook I used to create the files in the ```areas_to_investigate``` subfolder.
* ```feature_selection_origreseach.ipynb```: The Jupyter notebook I used to create the files I used for modeling.
* ```greenland.csv```: The CSV file that contains the HID for grid cells in Greenland. These were manually selected on QGIS.
* ```novaya_zemlya.csv```: The CSV file that contains the HID for grid cells in Novaya Zemlya. These were manually selected on QGIS.
* ```sahara.csv```: The CSV file that contains the HID for the Sahara desert and the area that surrounds it (The north half of Africa). Manually selected on QGIS.
* ```testdata_GLM.csv```: Future data for random CV features.
* ```testdata_SGLM.csv```: Future data for spatial CV features.
* ```testdata_pres_GLM.csv```: Present-day data for random CV features. Includes grid cells used for training but not labels.
* ```testdata_pres_SGLM.csv```: Present-day data for spatial CV features. Includes grid cells used for training but not labels.
* ```traindata_GLM.csv```: The random CV features and labels used for training.
* ```traindata_SGLM.csv```: The spatial CV features and labels used for training.

## Results

The outputs (prediction results) from the models I created within the project. These are separated into four folders:

* ```Bayesian_linearGLM```: Bayesian logistic regression
* ```Bayesian_splineGLM```: Bayesian GAM
* ```Standard_RF```: Random forest
* ```Standard_linearGLM```: MLE Logistic regression

```Standard_RF``` and ```Standard_linearGLM``` have file names that are named like *\<prediction type\>\_\<model\>\_\<feature set\>\_\<random seed>*. For example, ```valpreds_RF_normGLM_seed12244.csv``` contains the validation fold predictions (all folds appended) for the random forest model fit on scaled random CV features using random seed 12244. Predictions for future data begin with ```results_``` and predictions for present-day data begin with ```results_present_```. 

The prediction files for ```Bayesian_linearGLM``` and ```Bayesian_splineGLM``` have the same name structure, but has a different naming convention (I got confused in the frenzy of research and forgot the old convention at this point). In these files predictions for future data begin with ```preds_``` and predictions for validation folds begin with ```valpreds_```. For example, ```pres_preds_bayeslinGLM_SGLMunnorm_seed12244.csv``` contains the present-day predictions for the Bayesian logistic regression model fit on raw spatial CV features using random seed 12244. 

The Bayesian model folders are further separated into subfolders by the prior and basis dimension settings I used in modeling:

* ```baseline_priors```: The outputs from models created with initial prior settings.
* ```adjusted_priors```: The outputs from models created with adjusted prior settings.
* ```bnorm_sdsnorm```: Normal priors for coefficients, intercept, and non-linearity.
* ```bnorm_sdst```: Normal priors for coefficients and intercept and Student's t-distribution for non-linearity.
* ```bunif_sdsnorm```: Uniform (flat) priors for coefficients and intercept and normal priors for non-linearity.
* ```k_default```: Default basis dimension.
* ```k1```: Basis dimension 1.
* ```k4```: Basis dimension 4.
* ```k5```: Basis dimension 5.

In some folders you may see a subfolder named ```elevation_filtered```. These are results with geographic areas above 3000 meters in elevation omitted. These results were used in the second submission to Ecological Informatics.

## Spatial_Datasets

The files to use in QGIS if you need to create visualizations on maps. For exactly how to do this, you can look at the [QGIS documentation](https://docs.qgis.org/3.22/en/docs/).

* ```Centroids_ISEA3H09_Geodetic_V_WGS84.shp```: The centroids of the grid cells the features, labels, and predictions are mapped on. Files with the same name that end in .dbf, .prj, and .shx are its metafiles.
* ```ISEA3H09 - Geodetic Coordinates - Centroids.txt```: The World Geodetic System coordinates for the centroids of the grid cells. Not used in anything, but I am leaving it in the folder as additional information.
* ```test_area.shp```: The centroids of the grid cells that are on the land areas. In other words, the areas I needed to predict habitat suitability. This has metafiles that end in .cpg, .dbf, .prj, and .shx.
