#####
# Create region of interest
#####

## Region of interest from Galicia

# obtain geographic data from the 'GADM' database corresponding to Spain (country = 'spain')
# at the autonomous region level (level=1)
spain <- gadm('GADM', country = 'spain', level=1)

# select data corresponding to Galicia
gali <- spain[spain$NAME_1=='Galicia',]

## Region of interest from north and center Portugal

# obtain geographic data from the 'GADM' database corresponding to Portugal (country = 'portugal')
# at the district level (level=1)
pt <- gadm('GADM', country = 'portugal', level = 1)

districts <- c('Santar\u00e9m','Braga',"Vila Real","Coimbra","Guarda","Aveiro","Viseu"
               ,"Castelo Branco","Portalegre","Bragan\u00e7a",'Porto')
# create initial SpatVector to which vectorial data from districts will be added
# using rbind
north <- pt[pt$NAME_1 == "Viana do Castelo",]

for (i in districts){
  district <- pt[pt$NAME_1 == i,]
  north <- rbind(north,district)
}

# bind both regions to obtain full Region of Interest (RoI)
ROI <- rbind(north,gali)

# transform SPolyDF to simple features data frame (sf) 
ROI <- st_as_sf(ROI)

# mapview(ROI)

#####
# Create rtoi
#####

# path for the database
dir.create("D:/galicia",showWarnings = F)
datb.path <- file.path("D:/galicia", "DATABASE")
dir.create(datb.path,showWarnings = F)

# rtoi path
rtoi.path <- file.path("D:/galicia/rtoi")
dir.create(rtoi.path,showWarnings = F)

# initialize rtoi
ptgal <- new_rtoi(name = "ptgal",
                  region = ROI,
                  db_path = datb.path,
                  rtoi_path = rtoi.path)

# read rtoi if already initialized
# ptgal <- read_rtoi(file.path(rtoi.path,"ptgal"))


#####
# Search terra and aqua data for Region of Interest
#####

# Credentials
set_credentials("rsat.package","UpnaSSG.2021")

# Time of Interest (month of study and previous and posterior months)
time <- as.Date("2017-09-01") + 0:90

# start new records object
records(ptgal) <- new('records')

# search records corresponding to 2017
rsat_search(region = ptgal,
           product = c("MOD09GA","MYD09GA"),
           dates = time)
# search for 2016 LandCover
rsat_search(region = ptgal,
            product = c("MCD12Q1"),
            dates = as.Date("2016-10-01"))

#####
# Download found images
#####
rsat_download(ptgal)

#####
# Extract only images from the RoI
#####
rsat_mosaic(ptgal)


#####
# Obtain indexes for terra and aqua
#####

## NBR1
rsat_derive(ptgal,
             product="MOD09GA",
             variable="NBR1",
             fun = function(nir,swir2){
               nbr<-(nir-swir2)/(nir+swir2)
               return(nbr)
             })

rsat_derive(ptgal,
             product="MYD09GA",
             variable="NBR1",
             fun = function(nir,swir2){
               nbr<-(nir-swir2)/(nir+swir2)
               return(nbr)
             })

# NIR reescaled approx. -1 to 1
rsat_derive(ptgal,
            product="MOD09GA",
            variable="NIR",
            fun = function(nir){
              NIR <- nir/1e8
              return(NIR)
            }
            )

rsat_derive(ptgal,
            product="MYD09GA",
            variable="NIR",
            fun = function(nir){
              NIR <- nir/1e8
              return(NIR)
            })

# NBR2
rsat_derive(ptgal,
             product="MOD09GA",
             variable="NBR2")

rsat_derive(ptgal,
             product="MYD09GA",
             variable="NBR2")

# MIRBI
rsat_derive(ptgal,
             product="MOD09GA",
             variable="MIRBI",
             fun = function(swir1,swir2){
               MIRBI <- 10*swir2/1e8 - 9.8*swir1/1e8 + 2
               return(MIRBI)
             })

rsat_derive(ptgal,
             product="MYD09GA",
             variable="MIRBI",
             fun = function(swir1,swir2){
               MIRBI <- 10*swir2/1e8 - 9.8*swir1/1e8 + 2
               return(MIRBI)
             })

# MVI
rsat_derive(ptgal,
            product="MOD09GA",
            variable="MVI",
            fun = function(tirs1,swir2){
              MVI <- (tirs1 - swir2) / (tirs1 + swir2)
              return(MVI)
            })

rsat_derive(ptgal,
            product="MYD09GA",
            variable="MVI",
            fun = function(tirs1,swir2){
              MVI <- (tirs1 - swir2) / (tirs1 + swir2)
              return(MVI)
            })

# NDVI
rsat_derive(ptgal,
            product="MOD09GA",
            variable="NDVI")

rsat_derive(ptgal,
            product="MYD09GA",
            variable="NDVI")

#####
# Obtain Cloud Mask
#####
rsat_cloudMask(ptgal)
