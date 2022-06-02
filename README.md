# ***NeoNet*** Rshiny app <br> development version <img src="doc/img/neonet.png" width='150px' align="right"/>
> Thomas Huet, Niccolo Mazzucco, Miriam Cubas Morera, Juan Gibaja and F. Xavier Oms

***NeoNet app*** is an R Shiny app for mapping radiocarbon dates (C14) of the Late Mesolithic/Early Neolithic transition in the North Central-Western Mediterranean watershed. The app offers a mobile geographical window and different other tools for the selection of dates by location, by chronology, and by the quality of dates. 

## Stable version

The <a href="http://shinyserver.cfs.unipi.it:3838/C14/" target="_blank">stable version of the <b>NeoNet app</b></a> is hosted online by the [University of Pisa](https://www.unipi.it/index.php/english). 

### Overview
  
  
![](doc/img/panel_map.png)
  
  
![](doc/img/neonet_calib_spd.png)
  
Once selected, the radiocarbon dates can be calibrated on-the-fly, separately or grouped by layers, sites, periods, and the seriated summed probability densities (SPD) plots of these dates can be downloaded. See the [web tutorial](https://zoometh.github.io/neonet/).
  
![](doc/img/neonet_calib_example.png)
  
## Citation

The NeoNet dataset has been published in the [Journal of Open Archaeology Data](https://openarchaeologydata.metajnl.com/) under this BibTex reference:

```
@article{Huet22,
  author = {Huet, Thomas and Cubas, Miriam and Gibaja, Juan .F. and Oms, F. Xavier and Mazzucco, Niccolo},
  title = {NeoNet Dataset. Radiocarbon Dates for the Late Mesolithic/Early Neolithic Transition in the North Central-Western Mediterranean Basin},
  journal = {Journal of Open Archaeology Data},
  year = {2022},
  volume = {10},
  number = {3},
  pages = {1-8},
  doi={10.5334/joad.87},
}
```
## Development version

 This GitHub repository host the development version of the app, with:
 
* R computer scripts: [R/](https://github.com/zoometh/neonet/tree/main/R) folder

* a sample dataset (n = 100 dates): [inst/extdata/](https://github.com/zoometh/neonet/tree/main/inst/extdata) folder

* a correspondance table: [inst/extdata/](https://github.com/zoometh/neonet/tree/main/inst/extdata) folder

* BibTex references: [inst/extdata/](https://github.com/zoometh/neonet/tree/main/inst/extdata) folder


To contribute to the development version of the app, or the dataset, check the [contribution rules](https://github.com/zoometh/neonet/blob/master/github/CONTRIBUTING.md) and the [relevant license](https://github.com/zoometh/neonet/blob/master/LICENSE)



## Futur work

We plan to publish the southwestern Atlantic counterpart of the NeoNet dataset, improving some functionalities of the app:

* download button for the selected dataset

* set *tpq* and *taq* limits for the SPDs
  
<p align="center">
  <img width="500" src="doc/img/watersheds.png"><br>
  <a href="https://zoometh.github.io/neonet/doc/img/neonet_atl.html" target="_blank">map NeoNet atlantique</a>
</p>
<center>

## Contacts

* [Thomas Huet](mailto:thomashuet7@gmail.com)

* [Niccolo Mazzucco](mailto:niccolo.mazzucco@unipi.it)

* [Miriam Cubas Morera](mailto:mcubas.morera@gmail.com)

* [Juan Gibaja](jfgibaja@gmail.com)

* [F. Xavier Oms](oms@ub.edu)

* [António Faustino Carvalho](a.faustino.carvalho@gmail.com)
  
