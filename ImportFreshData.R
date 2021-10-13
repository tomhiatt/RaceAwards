# Bring in and process Race TB dataset

library(tidyverse)

# =========================================
# Step One: Download data from RaceTB =====
# =========================================

# Download the latest data from racetb.doh.gov.ph (extract the zip and replace the file at this path)
p <- read.csv("C:/Users/hiattt/Downloads/Actuals_Data.csv", sep=";") 
as.of.date <- file.info("C:/Users/hiattt/Downloads/Actuals_Data.csv")
as.of.date <- as.of.date$ctime

# =========================================
# Step Two: Massage the data ==============
# =========================================

# Add organizing variables
## categorize locations
p[p$facilityname!="", "location_type"] <- "facility"
p[p$facilityname=="", "location_type"] <- "municipality"
p[p$districtname=="", "location_type"] <- "province"
p[p$provincename=="", "location_type"] <- "region"
p[p$countryname=="Philippines", "location_type"] <- "country"

# Create bump chart of regional and provincial rankings for Race awards. https://dominikkoch.github.io/Bump-Chart/
# - showing all regions
# - showing all provinces

qrts <- c("JAN-MAR", "APR-JUN",  "JUL-SEP", "OCT-DEC")
yrs <- c(2019:2021)
qys <- c(paste(qrts, yrs[1], sep="-"), paste(qrts, yrs[2], sep="-"), paste(qrts, yrs[3], sep="-"))
qysdf <- data.frame(periodvalue=qys, Quarter=c(paste(yrs[1], paste0("Q", 1:4), sep="-"), paste(yrs[2], paste0("Q", 1:4), sep="-"), paste(yrs[3], paste0("Q", 1:4), sep="-")))

# ==============================================
# Step Three: Define the look of the chart =====
# ==============================================


my_theme <- function() {
  
  # Colors
  color.background = "white"
  color.text = "#22211d"
  
  # Begin construction of chart
  theme_bw(base_size=15) +
    
    # Format background colors
    theme(panel.background = element_rect(fill=color.background, color=color.background)) +
    theme(plot.background  = element_rect(fill=color.background, color=color.background)) +
    theme(panel.border     = element_rect(color=color.background)) +
    theme(strip.background = element_rect(fill=color.background, color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    theme(axis.ticks       = element_blank()) +
    
    # Format the legend
    theme(legend.position = "none") +
    
    # Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=20, face = "bold")) +
    theme(axis.title.x     = element_text(size=14, color="black", face = "bold")) +
    theme(axis.title.y     = element_text(size=14, color="black", face = "bold", vjust=1.25)) +
    theme(axis.text.x      = element_text(size=10, vjust=0.5, hjust=0.5, color = color.text)) +
    theme(axis.text.y      = element_text(size=10, color = color.text)) +
    theme(strip.text       = element_text(face = "bold")) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

# =============================================
# Step Four: Simplify and combine dataset =====
# =============================================

# All in one dataset
ba <- p %>% filter(periodvalue %in% qys, location_type %in% c("region","province"), indicatorname %in% c("Population testing rate", "Case notification rate, bacteriologically positive new+relapse", "Treatment Success, DRTB (inc. RR-,MDR-, and XDR-TB)", "Eligible under-five contacts started LTBI treatment")) %>% inner_join(qysdf) %>% transmute(loc.name=if_else(location_type=="region", countryname, provincename), location_type, percent, Quarter, indicatorname)

# ===================================================
# Step Five: Save the pieces to upload to Shiny =====
# ===================================================


# save as Rdata file

save(ba, my_theme, as.of.date, file="ingredients.Rdata")
