#%% Collect the coordinates of a site of sites using a geolocator

import pandas as pd
import geopandas as gpd
from shapely.geometry import Point
from geopy.geocoders import Nominatim


col_site_name = 'Site'
df = pd.read_excel("./db_data/12520_2014_193_MOESM1_ESM.xlsx") # Brami 2017 supp. data
sites = df[col_site_name].unique()

# Initialize a geolocator
geolocator = Nominatim(user_agent="my_geocoder")
locations = []

for site in sites:
	print(site)
	location = geolocator.geocode(site)
	if location:
		locations.append([site, location.longitude, location.latitude])
	else:
		locations.append([site, None, None])
		print("     Not Found")

df = pd.DataFrame(locations, columns=['SiteName', 'lon', 'lat'])
df.dropna()
gdf = gpd.GeoDataFrame(
	df, 
	geometry=[Point(xy) for xy in zip(df['lon'], df['lat'])]
)
gdf.set_crs(epsg=4326, inplace=True)
# export as a GeoJSON
gdf.to_file("./archaeological_sites.geojson", driver='GeoJSON')

## Layout NeoNet
# sourcedb     SiteName  LabCode C14Age C14SD db_period db_culture Period       lon      lat colors    median   tpq   taq



# %%
