This file contains description of the data in this folder

1: "./data/TxtInOut": A TxtInOut folder and all associated files created by ArcSWAT

2. "swatParam.txt": An input file describing all SWAT parameters that you will need 
                    for 1. General Setting => 4. Files with list of all SWAT Parameters

3. "obs_var_1.txt": Observed data file (streamflow) at the catchment outlet that you 
                    might need to input to evaluate the model performance 
                    4.1. Objective function => 2. Get observed data files
    
4. "./examples": you will see examples how to include new parameters in new files 
                (e.g., this could be the case if you modify SWAT code)

5. "./Watershed/Shapes": the HRU (hru1.shp) and subbasin (subs1.shp) shape files for
                          visualization

NOTE: The precipitation "pcp1.pcp", temperature "Tmp1.Tmp" and streamflow at the 
      catchment outlet (obs_var_1.txt) were minipulated because we cannot provide 
      the original data. To get the original data, please contact me 
      (tam.nguyen(at)ufz.de)
