# Notes on the Sberbank Competition

## General Improvements

1. Put everything on GitHub.
2. Create a Makefile for R.
3. Use sparse matrices.
4. Use HDF5, also in R.


## Observations on the dataset

The geographical analysis available in [this link](https://www.kaggle.com/jtremoureux/map-visualizations-with-external-shapefile) shows that raions are too big units for deriving any valuable information. There are clear differences at the level of individual `sub_areas`.

### Dummy Variables With Missing Values in R

By default `model.matrix` will remove all the rows containing one or more missing values. This behaviour can be modified by temporarily changing the option `na.action`.

```r
op <- options(na.action = "na.pass")
```

This will allow the creation of model matrices with NAs. Remember to change the option as soon as you're done.

