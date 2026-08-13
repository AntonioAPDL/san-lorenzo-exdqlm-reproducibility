###############################################################################
# Plot utilities (non-semantic helpers)
# Inputs:
#   - Filenames and plotting parameters
# Outputs:
#   - Opens graphics devices with consistent settings
# Dependencies:
#   - grDevices
###############################################################################

open_png <- function(filename, width = 6000, height = 4000, res = 600) {
  png(filename, width = width, height = height, res = res)
}
