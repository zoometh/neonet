# Text summarisation with ChatGPT

On the `xxx.pdf` article:

```chat-gpt
🖇️
Using this article, can you create a GeoJSON feature collection that brings together for each climate period: spatial extent enlarged to the region, temporal extension with start date and end date iin calendar years before present, this climate aligned with the Koppen climate classification?
```

Gives:

```json
{
	"type": "FeatureCollection",
	"features": [
	  {
		"type": "Feature",
		"properties": {
		  "name": "Çatalhöyük",
		  "climate_period": "8200 cal yr BP event",
		  "temporal_start": "ca. 8400 cal yr BP",
		  "temporal_end": "ca. 8000 cal yr BP",
		  "Koppen_classification": "Presumed BSk (Semi-arid Steppe)",
		  "observations": "Decreased rainfall and increased aridity linked to rapid climate changes."
		},
		"geometry": {
		  "type": "Point",
		  "coordinates": [32.8264, 37.6671]
		}
	  }
	]
  }
```