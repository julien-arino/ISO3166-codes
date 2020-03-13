# Create a master list of countries/places with all the information we require here:
# - centroids
# - population
# - GDP
# - ..

library(wbstats)

# Set top directory for code
TOP_DIR_CODE = here::here()
if (length(TOP_DIR_CODE) == 0) {
  TOP_DIR_CODE = rprojroot::find_root(is_git_root,".")
}
# Set other directories
source(sprintf("%s/set_directories.R", TOP_DIR_CODE))

# WORLD BANK DATA
# The following are "area" codes (Arab World, World, etc.) that we remove in the list of ISOs
ISO_groups = c("ARB", "CSS", "CEB", "EAR", "EAS", "EAP", "TEA", "EMU", "ECS",
               "ECA", "TEC", "EUU", "FCS", "HPC", "HIC", "IBD", "IBT", "IDB",
               "IDX", "IDA", "LTE", "LCN", "LAC", "TLA", "LDC", "LMY", "LIC",
               "LMC", "MEA", "MNA", "TMN", "MIC", "NAC", "OED", "OSS", "PSS",
               "PST", "PRE", "SST", "SAS", "TSA", "SSF", "SSA", "TSS", "UMC", "WLD")

## Get population info from World Bank
pop_data_wb <- wb(indicator = "SP.POP.TOTL",
                  startdate = 2000,
                  enddate = 2019)
pop_data_wb <- latest_values_general(pop_data_wb,
                                     "iso3c",
                                     "date")
# Prepare to remove data from above ISO_groups
rownames(pop_data_wb) <- pop_data_wb$iso3c
pop_data_wb <- pop_data_wb[setdiff(pop_data_wb$iso3c,ISO_groups),]
# Keep only relevant columns and rename the useful ones
pop_data_wb <- pop_data_wb[,c("iso3c","iso2c","country","value")]
colnames(pop_data_wb)[4] = "population"


## Get GDP info from World Bank "NY.GDP.PCAP.CD" or "NY.GDP.PCAP.PP.CD"
GDP_data_wb <- wb(indicator = "NY.GDP.PCAP.CD",
                  startdate = 2000,
                  enddate = 2019)
GDP_data_wb <- latest_values_general(GDP_data_wb,
                                     "iso3c",
                                     "date")
# Prepare to remove data from above ISO_groups
rownames(GDP_data_wb) <- GDP_data_wb$iso3c
GDP_data_wb <- GDP_data_wb[setdiff(GDP_data_wb$iso3c,ISO_groups),]
# Keep only relevant columns and rename the useful ones
GDP_data_wb <- GDP_data_wb[,c("iso3c","value")]
colnames(GDP_data_wb)[2] = "GDP"

# Start with a merge of these and see what's missing
WB_data = merge(x = pop_data_wb, y = GDP_data_wb,
                by.x = "iso3c", by.y = "iso3c",
                all = TRUE)
rownames(WB_data) = WB_data$iso3c
# What is missing? (Seems to be only on the GDP side)
missing_idx = which(is.na(WB_data$GDP)==TRUE)
missing_iso3c = WB_data$iso3c[missing_idx]

# Fill in the blanks (on 2020-02-18, only 5 missing)
WB_data["GIB",]$GDP = 92843 # https://en.wikipedia.org/wiki/Economy_of_Gibraltar
WB_data["MAF",]$GDP = 15400 # https://en.wikipedia.org/wiki/Economy_of_Saint_Martin (2008)
WB_data["PRK",]$GDP = 1300 # https://en.wikipedia.org/wiki/Economy_of_North_Korea
WB_data["SXM",]$GDP = 15400 # https://en.wikipedia.org/wiki/Economy_of_Saint_Martin (2008)
WB_data["VGB",]$GDP = 34246 # https://en.wikipedia.org/wiki/Economy_of_the_British_Virgin_Islands

# Fill in info for one obviously missing case
WB_data = rbind(WB_data,
                c("TWN","TW","Taiwan","23780452","24828"))
# Re-run row names after latest update
rownames(WB_data) = WB_data$iso3c



## CENTROIDS
centroids_tmp = read.csv(sprintf("%s/Country_centroids.csv", DIR_DATA_RAW), stringsAsFactors = FALSE)
# Get index of Namibia, because it will go wrong (NA..)
idx_Namibia = which(centroids_tmp$name == "Namibia")
# Convert most stuff to iso3c without problems
centroids_tmp$iso3c = countrycode(centroids_tmp$country, origin="iso2c", destination="iso3c")
# Fix Namibia
centroids_tmp$iso3c[idx_Namibia] = "NAM"
# Can we get a bit better by using country names instead
idx_issues = which(is.na(centroids_tmp$iso3c)==TRUE)
centroids_tmp$iso3c[idx_issues] = countrycode(centroids_tmp$name[idx_issues], 
                                              origin="country.name", 
                                              destination="iso3c")
# World Bank uses XKX for Kosovo (XK), so just use this.
centroids_tmp$iso3c[which(centroids_tmp$name=="Kosovo")] = "XKX"
# PSE results from both PS (Palestinian territory) and GZ (Gaza strip). Simplify (keep only 1)
idx_PSE = which(centroids_tmp$iso3c == "PSE")[1] # The one we remove
centroids_tmp = centroids_tmp[setdiff(1:dim(centroids_tmp)[1],idx_PSE),]




# MERGE
iso3c_data_all = merge(x=WB_data,y=centroids_tmp,
                       by.x = "iso3c", by.y="iso3c",
                       all = TRUE)
iso3c_data_all = iso3c_data_all[,c(1,2,3,4,5,7,8)]
colnames(iso3c_data_all)[3] = "name"

# Set row names
rownames(iso3c_data_all) = iso3c_data_all$iso3c
# Take stock of what we are missing
missing_idx = which(!complete.cases(iso3c_data_all))
missing_iso3c = iso3c_data_all$iso3c[missing_idx]
# List of iso3c codes we are happy to remove
to_remove = c("ATA", # Antartica
              "ATF", # https://en.wikipedia.org/wiki/French_Southern_and_Antarctic_Lands
              "BVT", # Bouvet Island
              "HMD", # Heard and Mc Donald Islands
              "IOT", # https://en.wikipedia.org/wiki/British_Indian_Ocean_Territory
              "PCN", # https://en.wikipedia.org/wiki/Pitcairn_Islands
              "SGS", # https://en.wikipedia.org/wiki/South_Georgia_and_the_South_Sandwich_Islands
              "SJM", # https://en.wikipedia.org/wiki/Svalbard_and_Jan_Mayen
              "TKL" # https://en.wikipedia.org/wiki/Tokelau
              )
iso3c_data_all = iso3c_data_all[setdiff(iso3c_data_all$iso3c,to_remove),]
# Take stock of what we are missing after first round of removals
missing_idx = which(!complete.cases(iso3c_data_all))
missing_iso3c = iso3c_data_all$iso3c[missing_idx]

# Load a file with a lot of information and try to find remaining info in there
countries_long = read.csv(file = sprintf("%s/country_centroids_az8.csv", DIR_DATA_RAW),
                          stringsAsFactors = FALSE)
# Get rid of the first few columns which are useless here
countries_long = countries_long[,7:dim(countries_long)[2]]
# Get list of names of columns with iso3c codes
countries_long_iso3c_cols = colnames(countries_long)[grep("a3",colnames(countries_long))]
# Populate the information for missing codes. As we progress, reduce the size of the list of missing codes.
for (c in countries_long_iso3c_cols) {
  for (iso in missing_iso3c) {
    idx = which(countries_long[,c] == iso)
    if (length(idx) > 0) {
      if (is.na(iso3c_data_all[iso,]$iso2c))
        iso3c_data_all[iso,]$iso2c = countries_long[idx[1],"iso_a2"]
      if (is.na(iso3c_data_all[iso,]$name))
        iso3c_data_all[iso,]$name = countries_long[idx[1],"name"]
      if (is.na(iso3c_data_all[iso,]$population))
        iso3c_data_all[iso,]$population = countries_long[idx[1],"pop_est"]
      if (is.na(iso3c_data_all[iso,]$GDP))
        iso3c_data_all[iso,]$GDP = countries_long[idx[1],"gdp_md_est"]
      if (is.na(iso3c_data_all[iso,]$latitude)) {
        iso3c_data_all[iso,]$latitude = countries_long[idx[1],"latitude"]
        iso3c_data_all[iso,]$longitude = countries_long[idx[1],"longitude"]
      }
    }
  }
}
# Take stock of what we are missing now
missing_idx = which(!complete.cases(iso3c_data_all))
missing_iso3c = iso3c_data_all$iso3c[missing_idx]
# Missing mostly overseas territories


missing_codes = list()
missing_codes[["CCK"]] = c(iso3c = "CCK",
                           name = "Cocos (Keeling) Islands",
                           population = 544,
                           GDP = 5000,
                           latitude = -12.116667,
                           longitude = 96.9)
missing_codes[["CHI"]] = c(iso3c = "CHI",
                           name = "Channel Islands",
                           population = 170499,
                           GDP = 74463,
                           latitude = 49.433333,
                           longitude = -2.316667)
missing_codes[["CXR"]] = c(iso3c = "CXR",
                           name = "Christmas Island",
                           population = 1843,
                           GDP = 5000,
                           latitude = -10.483333,
                           longitude = 105.633333)
missing_codes[["GLP"]] = c(iso3c = "GLP",
                           name = "Guadeloupe",
                           population = 395700,
                           GDP = 7900,
                           latitude = 15.94,
                           longitude = -61.578)
missing_codes[["GUF"]] = c(iso3c = "GUF",
                           name = "French Guiana",
                           population = 296711,
                           GDP = 18313,
                           latitude = 4,
                           longitude = -53)
missing_codes[["MTQ"]] = c(iso3c = "MTQ",
                           name = "Martinique",
                           population = 376480,
                           GDP = 24513,
                           latitude = 14.666667,
                           longitude = -61)
missing_codes[["MYT"]] = c(iso3c = "MYT",
                           name = "Mayotte",
                           population = 270372,
                           GDP = 13163,
                           latitude = -12.806,
                           longitude = 45.015)
missing_codes[["REU"]] = c(iso3c = "REU",
                           name = "Réunion",
                           population = 866506,
                           GDP = 27793,
                           latitude = -21.1349,
                           longitude = 55.3872)
missing_codes[["UMI"]] = c(iso3c = "UMI",
                           name = "United States Minor Outlying Islands",
                           population = 300,
                           GDP = 46381,
                           latitude = 13.8404034,
                           longitude = 174.2680647)


for (iso in missing_iso3c) {
    if (is.na(iso3c_data_all[iso,]$name))
      iso3c_data_all[iso,]$name = missing_codes[[iso]]["name"]
    if (is.na(iso3c_data_all[iso,]$population))
      iso3c_data_all[iso,]$population = missing_codes[[iso]]["population"]
    if (is.na(iso3c_data_all[iso,]$GDP))
      iso3c_data_all[iso,]$GDP = missing_codes[[iso]]["GDP"]
    if (is.na(iso3c_data_all[iso,]$latitude)) {
      iso3c_data_all[iso,]$latitude = missing_codes[[iso]]["latitude"]
      iso3c_data_all[iso,]$longitude = missing_codes[[iso]]["longitude"]
    }
}

# The following 3 are in the flights database and need to be entered
missing_codes[["BLM"]] = c(iso3c = "BLM",
                           name = "Saint Barthélemy",
                           population = 9793,
                           GDP = 28000,
                           latitude = 17.9,
                           longitude = -62.833333)
missing_codes[["COK"]] = c(iso3c = "COK",
                           name = "Cook Islands",
                           population = 17518,
                           GDP = 15002,
                           latitude = -21.233333,
                           longitude = -159.766667)
missing_codes[["NFK"]] = c(iso3c = "NFK",
                           name = "Norfolk Island",
                           population = 1748,
                           GDP = 5000,
                           latitude = -29.033333,
                           longitude = 167.95)
for (c in c("BLM","COK","NFK")) {
  tmp = c(missing_codes[[c]]["iso3c"],
          NA,
          missing_codes[[c]]["name"],
          missing_codes[[c]]["population"],
          missing_codes[[c]]["GDP"],
          missing_codes[[c]]["latitude"],
          missing_codes[[c]]["longitude"])
  iso3c_data_all = rbind(iso3c_data_all,
                         tmp)
}

# On visual inspection, there remains a few problems
iso3c_data_all["CUW",]$latitude = 12.183333
iso3c_data_all["CUW",]$longitude = -69
iso3c_data_all["MAF",]$latitude = 18.075278
iso3c_data_all["MAF",]$longitude = -63.06
iso3c_data_all["SSD",]$latitude = 8
iso3c_data_all["SSD",]$longitude = 30
iso3c_data_all["ESH",]$population = 567402
iso3c_data_all["ESH",]$GDP = 2500

# Add the following: Caribbean Netherlands
iso3c_data_all = rbind(iso3c_data_all,
                       c("BES",NA,"Caribbean Netherlands",
                         25157, 21700,
                         12.183333, -68.233333))

# Check for duplicated iso3c's
idx_duplicated_IATA = which(duplicated(iso3c_data_all$iso3c) == TRUE)
# Remove any duplicated iso
iso3c_data_all = iso3c_data_all[setdiff(1:dim(iso3c_data_all)[1],
                                        idx_duplicated_IATA),]

# Finally, put numeric fields as numeric fiels..
iso3c_data_all$population = as.numeric(iso3c_data_all$population)
iso3c_data_all$GDP = as.numeric(iso3c_data_all$GDP)
iso3c_data_all$latitude = as.numeric(iso3c_data_all$latitude)
iso3c_data_all$longitude = as.numeric(iso3c_data_all$longitude)

# and sort by iso3c code
iso3c_data_all = iso3c_data_all[order(iso3c_data_all$iso3c),]
rownames(iso3c_data_all) = iso3c_data_all$iso3c

saveRDS(iso3c_data_all,
        sprintf("%s/iso3c_pop_GDP_centroids.Rds", DIR_DATA_PROCESSED_GLOBAL))





