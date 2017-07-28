smwrBase
==========

Base R functions to support statistical methods in water resources.


## Package Status

### Current build tests:

|Linux|Test Coverage| USGS Status |
|----------|------------|------------|
| [![travis](https://travis-ci.org/USGS-R/smwrBase.svg?branch=master)](https://travis-ci.org/USGS-R/smwrBase)|[![Coverage Status](https://coveralls.io/repos/github/USGS-R/smwrBase/badge.svg?branch=master)](https://coveralls.io/github/USGS-R/smwrBase?branch=master)|[![status](https://img.shields.io/badge/USGS-Research-blue.svg)](https://owi.usgs.gov/R/packages.html#research)|

### Reporting bugs

Please consider reporting bugs and asking questions on the Issues page:
[https://github.com/USGS-R/smwrBase/issues](https://github.com/USGS-R/smwrBase/issues)

Follow `@USGS_R` on Twitter for updates on USGS R packages:

[![Twitter Follow](https://img.shields.io/twitter/follow/USGS_R.svg?style=social&label=Follow%20USGS_R)](https://twitter.com/USGS_R)

## Code of Conduct

We want to encourage a warm, welcoming, and safe environment for contributing to this project. See the [code of conduct](https://github.com/USGS-R/smwrBase/blob/master/CONDUCT.md) for more information.

## Package Installation
To install the `smwrBase` package:

USGS R Installation Instructions: [https://owi.usgs.gov/R/training-curriculum/installr/](https://owi.usgs.gov/R/training-curriculum/installr/)


1. Install R (version 3.0 or greater) from: [https://cran.r-project.org/](https://cran.r-project.org/)

2. Install RStudio from: [https://www.rstudio.com/products/rstudio/download/](https://www.rstudio.com/products/rstudio/download/)

3. Add the USGS R repository to your "Rprofile" to get automatic updates. Run the following code:
  
  ```r
  rprofile_path = file.path(Sys.getenv("HOME"), ".Rprofile")
  write('\noptions(repos=c(getOption(\'repos\'),
    CRAN=\'https://cloud.r-project.org\',
    USGS=\'https://owi.usgs.gov/R\'))\n',
      rprofile_path, 
      append =  TRUE)

  cat('Your Rprofile has been updated to include GRAN.
    Please restart R for changes to take effect.')
  ```

4. Restart R!

5. In the RStudio "Console" window (usually left or lower-left), run the following command:

  ```r
  install.packages("smwrBase")
  ```
  

6. Update often. Next time you update your packages, `smwrBase` will automatically update:

   ![update](images/update.png)

7. Make sure to keep your version of R up-to-date. CRAN and the USGS repository will only update packages for the most recent version of R, and one version behind that.

## Disclaimer

This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the official USGS copyright policy at [https://www.usgs.gov/visual-id/credit_usgs.html#copyright](https://www.usgs.gov/visual-id/credit_usgs.html#copyright)

Although this software program has been used by the U.S. Geological Survey (USGS), no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."


Linux: [![travis](https://travis-ci.org/USGS-R/smwrBase.svg?branch=master)](https://travis-ci.org/USGS-R/smwrBase)

