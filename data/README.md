# The datasets and output files

This folder contains the datasets I used for modeling, the input files I created from them, and the outputs of the models in the ```codes``` folder. Most files here are either tab-separated txt files or CSV, and if not, they are most likely CSVT files, metafiles that are used in QGIS. The files that contain data or predictions have a common key "HID", which are unique identifiers for each grid used in mapping features, labels, and predictions.

Below are descriptions of what you can find inside each folder.

## Climate

The raw files for the features used in modeling. The files that have "ETCCDI" in their names are the climate extreme indices from CCSM4, and the files that have "WC30AS" indices in them are from WorldClim. Present-day and future features have separate files. The files that have either "2061_2080" or "2070" in their names are predicted future data.

The full definitions of features can be found [here](https://www.worldclim.org/data/bioclim.html) and [here](http://etccdi.pacificclimate.org/list_27_indices.shtml).

## Elephas_Maximus

The elephant range and psuedo-absence data, or in other words, the labels. 

* ```Elephas_Maximus_Folds_S10_Natural_O20.txt```: The folds used in spatial CV in the previous research by my thesis advisor. The files ending in ```_short``` are truncated and CSV-converted version of it and its metafile.
