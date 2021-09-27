# ***NeoNet*** app <br> <img src="doc/img/neonet.png" width='150px' align="right"/>
> Created by [Thomas Huet](mailto:thomashuet7@gmail.com), [Niccolo Mazzucco](mailto:niccolo.mazzucco@unipi.it), [Miriam Cubas Morera](mailto:mcubas.morera@gmail.com), [Juan Gibaja](jfgibaja@gmail.com), and [F. Xavier Oms](oms@ub.edu)

A RShiny app for mapping radiocarbon dates (C14) from Late Mesolithic/Early Neolithic transition in the North Central-Western Mediterranean watershed. The [stable version of the ***NeoNet app***](http://shinyserver.cfs.unipi.it:3838/C14/) is hosted on the [University of Pisa](https://www.unipi.it/index.php/english) Shiny Server


The ***NeoNet app*** offers a geographical window provided by the Leaflet package used for selection of radiocarbon dates by location, by chronology, and by quality of dates. 
  
<center>
  
![](doc/img/panel_map.png)
  
</center>
  
Once selected, the radiocarbon dates can be calibrated on-the-fly, separately or grouped by layers, sites, periods, and the seriated summed probability densities (SPD) plots of these dates can be downloaded. See the [web tutorial](https://zoometh.github.io/neonet/).
  
<center>
  
![](doc/img/neonet_calib_example.png)
  
</center>
  
To go further on the development version of the app, you can check the [contribution rules](https://github.com/zoometh/neonet/blob/master/github/CONTRIBUTING.md) and the [relevant license](https://github.com/zoometh/neonet/blob/master/LICENSE)

## The app

The development version of the app is in the [R/](https://github.com/zoometh/neonet/tree/main/R) folder

## The dataset

A sample dataset (n = 100 dates) is provided in the [inst/extdata/](https://github.com/zoometh/neonet/tree/main/inst/extdata) folder with BibTex references
