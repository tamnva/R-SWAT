## <img src="data/figures/R-SWAT_logo.svg" width=20% >

In this branch, we demonstrate that R-SWAT correctly updates SWAT input files with a given parameter sets. The parameter that are needed to be updated and their values are listed as follows:

| Parameter   | Value    | Change   |
| ----------- | -------- | -------- |
| GW_DELAY.gw | 0.774645 | replace  |
| GW_REVAP.gw | 0.07085  | replace  |
| GWQMN.gw    | 885      | replace  |
| RCHRG_DP.gw | 0.0867   | replace  |
| REVAPMN.gw  | 447.25   | replace  |
| SURLAG.bsn  | 7.49035  | replace  |
| EPCO.hru    | 0.106525 | replace  |
| OV_N.hru    | 0.288565 | replace  |
| CN2.mgt     | 0.09225  | relative |
| CH_N2.rte   | 0.047913 | replace  |
| CH_K2.rte   | 5.145    | replace  |
| SOL_AWC.sol | 0.15625  | relative |
| SOL_K.sol   | -0.00925 | relative |
| CH_N1.sub   | 0.042137 | replace  |

To compare the updated SWAT input files with original files, please [click here](https://github.com/tamnva/R-SWAT/commit/34acfaec3cf980124d500253ba1d570a93d6dce8). Please also check the lower and upper ranges of SWAT parameters defined in the the [swatParam.txt file](https://github.com/tamnva/R-SWAT/blob/check_R_SWAT_TxtInOut_Edit_Function/data/TxtInOut/swatParam.txt).
