#%% 

import pandas as pd

sites = pd.read_csv('sites.tsv', sep='\t')
sites = [x for x in sites['nom']]
sites

col_site_name = 'Site'
df = pd.read_excel("./db_data/12520_2014_193_MOESM1_ESM.xlsx")
# sites = [x for x in df[col_site_name]]
sites = df[col_site_name].unique()

#%% import pandas as pd
from geopy.geocoders import Nominatim

# Create a list of sites
# sites = ['Achilleion', 'Agios Petros', ...]

# Initialize a geolocator
geolocator = Nominatim(user_agent="my_geocoder")

# Create empty lists to store data
locations = []

# Iterate over the sites and get coordinates
for site in sites:
	print(site)
	location = geolocator.geocode(site)
	if location:
		locations.append([site, location.longitude, location.latitude])
	else:
		locations.append([site, None, None])
		print("     Not Found")

# Create a DataFrame
df = pd.DataFrame(locations, columns=['SiteName', 'lon', 'lat'])

# sourcedb     SiteName  LabCode C14Age C14SD db_period db_culture Period       lon      lat colors    median   tpq   taq

# Save as a TSV file
# %%
import geopandas as gpd
from shapely.geometry import Point

df.dropna()
gdf = gpd.GeoDataFrame(
	df, 
	geometry=[Point(xy) for xy in zip(df['lon'], df['lat'])]
)
gdf.set_crs(epsg=4326, inplace=True)
gdf.to_file("./archaeological_sites.geojson", driver='GeoJSON')

# df.to_csv('archaeological_sites.tsv', sep='\t', index=False)


# %%
