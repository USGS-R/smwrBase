smwrBase
==========

Base R functions to support statistical methods in water resources.

### Reporting bugs

Please consider reporting bugs and asking questions on the Issues page:
[https://code.usgs.gov/water/analysis-tools/smwrBase/-/issues](https://code.usgs.gov/water/analysis-tools/smwrBase/-/issues)

## Code of Conduct

We want to encourage a warm, welcoming, and safe environment for contributing to this project. See the [code of conduct](https://code.usgs.gov/water/analysis-tools/smwrBase/-/blob/main/CONDUCT.md) for more information.

## Package Installation

Those within the USGS network can ask to have the prod-legacy R Package Manager added to their R enviornment. Installation and updates are done using the standard `install.packages` command.

External to the USGS network, first install the "remotes" package. Then, using the remotes package, install "smwrData", then "smwrBase":


```r
remotes::install_gitlab("water/analysis-tools/smwrData",
                        host = "code.usgs.gov")
remotes::install_gitlab("water/analysis-tools/smwrBase",
                        host = "code.usgs.gov")
```


## Disclaimer

This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the official USGS copyright policy at [https://www.usgs.gov/visual-id/credit_usgs.html#copyright](https://www.usgs.gov/visual-id/credit_usgs.html#copyright)

Although this software program has been used by the U.S. Geological Survey (USGS), no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."
