

# Note: 'path.sharepoint' is set in setup.R
path.sharepoint.data <- file.path(path.sharepoint, 'data', 'linelist', 'world')


get_updated_msf_data <- ifelse(isTRUE(update_msf_data) | 
                                 !file.exists(file.path(path.local.msf.data, 'dta_MSF.RData')), 
                               TRUE, 
                               FALSE)


# Get the MSF linelist dataset
if (get_updated_msf_data) {
  
  dta_path <- max(fs::dir_ls(path.sharepoint.data, regexp = "[.]rds$"))
  
  dta <- readRDS(dta_path)
  dta <- prepare_msf_dta(dta) # Prepare the linelist dataset
  
  } else {
    
    load(file.path(path.local.msf.data, 'dta_MSF.RData'))
  }



