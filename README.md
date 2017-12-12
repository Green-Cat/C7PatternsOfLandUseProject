# C7 Patterns Of Land Use Project

A project created for the MSc Global Change Ecology course C7 Patterns of Land Use and Ecosystem Dynamics.
The project assesses the impact of a large solar instalation on the island of Kauai on the deforestation.

## Required R dependencies
- raster
- rgdal
- maps
- mapdata

## Required inputs:
- **ctr_boundaries/USA_adm2.rds** (The level 2 boundaries of the US territories, downloaded from http://www.gadm.org/)
- **raster/before.tif** (A Landsat cover image from before the solar installation (2015) downloaded from https://earthexplorer.usgs.gov/)
- **raster/after.tif** (A Landsat cover image from after the solar installation (2017) downloaded from https://earthexplorer.usgs.gov/)
- **vector/beforeclass.shp & vector/afterclass.shp** (Training data for the classification of land uses, created using QGIS)
