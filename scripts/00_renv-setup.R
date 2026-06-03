  ##
  ## Setting up the R environment
  ##
  ## --------------------------------------------------------##
  
  ## If you don't have {renv} installed, comment the line below
  ## into the console and install the packages
  
  ## install.packages ("renv", "1.1.5")
  
  ## Then, run the following command, which will restore the R environment
  ## using the same package versions as we used
  
  renv::restore()
  
  ## --------------------------------------------------------##
  ##
  ## NOTE:
  ##
  ## Package versions & dependencies were captured using {renv} and
  ## locked in the 'lock.file' file
  ##
  ## If you install the packages by yourself, we cannot guarantee their versions
  ## will be same as we used in this Rproject.
  ##
  ## --------------------------------------------------------##
  ##
  ## Credit to Nico Daudt for the note