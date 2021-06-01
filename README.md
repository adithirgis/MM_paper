# Malleshwaram Mobile Monitoring paper

Hi there! 👋

All codes for the analysis for Phase II Analysis of Mobile Monitoring of Air Quality in Bangalore.

### Data collection

-   Duration : {May 2019 (Diesel)} 10 July 2019 to 12 March 2020.

### Study Area

-   Residential : Malleshwaram / MAL (1 + 2)

    -   No. of rides - 22 + 21
    -   No. of hours - 88 + 84 (If an assumption of 4 hours for each ride is considered)
    -   No. of kms covered - 820.6 + 506.1 km
    -   Unique kms - 37.3 + 24.1 km

### Parameter

-   BC - Black Carbon (ug/m3)
-   UFPs - Ultrafine Particles (#/m3)
-   PM2.5 - Particulate Matter (ug/m3)
-   CO2 - Carbon dioxide (ppm)
-   RH - Relative Humidity (%)

### Inportant files

-   Data availability file \<- "D:\\Dropbox\\APMfull\\Phase_II\\data_availability.csv"
-   Data stats using the Median of means (road segments) layer for each area \<- "D:\\Dropbox\\APMfull\\Phase_II\\layer30m_stats.csv"
-   Data stats for point layer (1 Hz) collected \<- "D:\\Dropbox\\APMfull\\Phase_II\\Points_stats.csv"

#### Data Organization

-   All raw data inside \<- "D:\\Dropbox\\APMfull\\Phase_II"

#### Raw Data of pollutants

-   GPS raw data \<- "D:\\Dropbox\\APMfull\\Phase_II\\GPS"
-   CO2 raw data \<- "D:\\Dropbox\\APMfull\\Phase_II\\LICOR"
-   BC raw data \<- "D:\\Dropbox\\APMfull\\Phase_II\\AE51"
-   PM2.5 raw data \<- "D:\\Dropbox\\APMfull\\Phase_II\\DustTrak"
-   UFP's raw data \<- "D:\\Dropbox\\APMfull\\Phase_II\\CPC"
-   RH raw data \<- "D:\\Dropbox\\APMfull\\Phase_II\\RH_USB"

#### Corrected pollutant concentrations and joined together files

-   MAL1 \<- "D:\\Dropbox\\APMfull\\Phase_II\\Corrected\\MAL1"
-   MAL2 \<- "D:\\Dropbox\\APMfull\\Phase_II\\Corrected\\MAL2"

#### Final Layers

-   30 m road segements, Median of means files for each area \<- "D:\\Dropbox\\APMfull\\Phase_II\\Analysis_layers" (Contains all information)
-   More information regarding the final layers \<-"D:\\Dropbox\\APMfull\\Phase_II\\Phase_II_Layers" (Different layers for different parameters)

#### Maps

-   All maps exported from QGIS are stored here \<- "D:\\Dropbox\\APMfull\\Phase_II\\Images"

#### Folder structure

1.  "D:\\Dropbox\\APMfull\\Phase_II\\MAL2_CNG_Diesel", "D:\\Dropbox\\APMfull\\Phase_II\\MAL1_CNG_Diesel" (folder names with "CNG_Diesel" indicates usage of both data collected from CNG and Diesel cars)

-   "BC_Residual_Layers" \<- Residual Layers generated after the Monte Carlo Sub-sampling for BC.
-   "BC_Subsampled_Layers" \<- Monte-Carlo Subsampled Layers for BC.
-   "CPC_Residual_Layers" \<- Residual Layers generated after the Monte Carlo Sub-sampling for UFP's.
-   "CPC_Residual_Layers" \<- Residual Layers generated after the Monte Carlo Sub-sampling for UFP's.
-   "Analysis_BC_CBD_Corrected.csv" \<- Statistics of sub-sampled layers of BC.
-   "Analysis_CPC_CBD_Corrected.csv" \<- Statistics of sub-sampled layers of UFP's.

1.  "D:\\Dropbox\\APMfull\\Phase_II\\MAL1", "D:\\Dropbox\\APMfull\\Phase_II\\MAL2" (Data from CNG car)

-   "BC_Residual_Layers" \<- Residual Layers generated after the Monte Carlo Sub-sampling for BC.
-   "BC_Subsampled_Layers" \<- Monte-Carlo Subsampled Layers for BC.
-   "CPC_Residual_Layers" \<- Residual Layers generated after the Monte Carlo Sub-sampling for UFP's.
-   "CPC_Residual_Layers" \<- Residual Layers generated after the Monte Carlo Sub-sampling for UFP's.
-   "Final_data_speed" \<- 30-m road segments (median of means) data.
-   "Joined_shp" \<- point shapefiles with corrected concentration data.
-   "Joined_snp" \<- snapped point shapefiles with corrected concentration data.
-   "All_rides_MAL1_30m" \<- All days 30-m road segment data along with corrected concentration in csv format.
-   "Analysis_BC_MAL1_Corrected.csv" \<- Statistics of sub-sampled layers of BC.
-   "Analysis_CPC_MAL1_Corrected.csv" \<- Statistics of sub-sampled layers of UFP's.
-   "Final_layer.csv" \<- Final Median of Means road segment data in csv format.

xx - BC, CPC, CO2; zzz -  MAL1, MAL2

-   "zzz_Bootstrap_xxx_mean.csv" \<- Bootstrap resampling using point file to estimate mean.
-   "zzz_Bootstrap_xxx_mean_drive_pass_means.csv" \<- Bootstrap resampling using road file to estimate mean.
-   "zzz_Bootstrap_xxx_median.csv" \<- Bootstrap resampling using point file to estimate median.
-   "zzz_Bootstrap_xxx_median_drive_pass_means.csv" \<- Bootstrap resampling using road file to estimate median.
-   "zzz_min.csv" \<- Minute average of data in the area.
-   "zzz_points_stats.csv" \<- Data stats for 1 Hz corrected data.
-   "zzz_sec.csv" \<- 1 Hz corrected data.
-   "zzz_snap_change.csv" \<- % removal of point data due to snapping.
-   "zzz_data_removed_join.csv" \<- % data removed due to joing.


PR for suggestions and feedback.

🔭 It contains code for cleaning data.

💬 Also has generating the road segment data, performing monte carlo analysis, ICC, and other analysis.

⚡ Fun fact: Its a tidy code!

🔭 Looking for better ggplot2 themes!
