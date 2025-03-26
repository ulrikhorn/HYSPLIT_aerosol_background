# HYSPLIT_aerosol_background
Scripts to use the Hybrid Single-Particle Lagrangian Integrated Trajectory model (HYSPLIT) to retrieve information about the aerosol package in a location at a certain time.

The aerosol package is the air that arrives at the specified location at a specified time (daily_hours). This aerosol package can be traced backwards in time (duration) to reveal where the air came from, its altitude at different times during its journey and other metadata. 
By using land cover data, we can extrapulate what kind of terrain (forrest, mountains, sea) the air has moved over during its journey. This can then be used in conjunction with particle concentration, size and fluorescence data to predict and make models about what kind of background particles (the size of the particle, the concentration and fraction of bioaerosols) will be present in the aerosol at a spesific location, at a spesific time. 

Screenshot of interactive map showing the path of an aerosol package before reaching Oslo.  
![image](https://github.com/user-attachments/assets/bfe3bcbd-2f79-4e43-87a9-439fb4fc68ee)

Plot of the air package altitude along the route, overlaid with the type of terrain it passed over:
![image](https://github.com/user-attachments/assets/06b0d4cc-7273-4c30-a09a-b16c767c0180)



Usage:
Download copernicus Land Cover raster (.tif) files for the area of interest and add them to the working directory. 

get a google API key and add to the script
Specify a location (lat, lon), day of interest, the time of that daym and the duration backwards in time.
