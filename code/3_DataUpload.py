################################################################################
#
#         Code to upload files created in previous scripts to AGOL 
#                        as hosted feature layers
#               Developed by Sophia Bryson (sab159@duke.edu)
#               Masters Project for client Internet of Water
#                                 2021 - 2022
#
################################################################################

# See https://developers.arcgis.com/python/sample-notebooks/using-and-updating-gis-content/
# See https://community.esri.com/t5/arcgis-api-for-python-questions/using-python-api-to-update-a-hosted-feature-layer/td-p/807034

# Note that this script updates previously (manually) created feature layers associated with the dashboard.
# It does not handle the initial creation of these hosted feature layers. 

### Import packages ############################################################

from arcgis import GIS

### Setup ######################################################################

# Connect to AGOL -
#  This requires that the code is being run within an ArcGIS Pro environment.
#  The credentials associated with the ArcGIS session will be accessed. 
#  Verify that the user whose credentials are being accessed has write permission for the hosted feature layers to be updated. 
# 
#  Alternative authentication methods here: 
#  https://github.com/sab159/ArcGIS-PythonAPI/blob/master/1-Connecting-to-AGOL-via-the-GIS-object.ipynb

gis = GIS('pro')

### Set paths ##################################################################

# Local data to upload 
pointUpdate = "..\\data\\WQPSiteData.csv"

# Polygons (.shp) need to be a zipfile?
from zipfile import ZipFile
shpZip = ZipFile(".\\data\\HUC12AllData.zip", 'w')
#Add .shp files
shpZip.write('.\\data\\HUC12AllData.shp')
shpZip.write('.\\data\\HUC12AllData.shx')
shpZip.write('.\\data\\HUC12AllData.dbf')
shpZip.write('.\\data\\HUC12AllData.prj')
# close the zip
shpZip.close()
polygonUpdate = ".\\data\\HUC12AllData.zip"

# Hosted Feature Layers
pointHFL = gis.content.get('9a7c73c6dc1b494286d99fe9990db090') #object id for previously created hosted feature layer
polygonHFL = gis.content.get('52a327a928574b188e0b2cc6b6deaaed') #object id for previously created hosted feature layer

### Overwrite feature layer with updated local files ######################################
from arcgis.features import FeatureLayerCollection

#Access the layers
pointFLC = FeatureLayerCollection.fromitem(pointHFL)
polyFLC = FeatureLayerCollection.fromitem(polygonHFL)

#Overwrite - troubleshooting here :https://doc.arcgis.com/en/arcgis-online/manage-data/manage-hosted-feature-layers.htm
pointFLC.manager.overwrite(pointUpdate)
polyFLC.manager.overwrite(polygonUpdate) # can be finnicky - alternative option to manually update from service layer overview page when logged into AGOL as an editor

