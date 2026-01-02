# packages ----
libs <- c(
        "giscoR","terra","elevatr",
        "png","rayshader","magick","rayrender",
        "rgl","sf", "rayvista" 
        )
installed_libs <- libs %in% rownames(
        installed.packages()
)

devtools::install_github(
        "h-a-graham/rayvista",
        dependencies = T
)

if(any(installed_libs==F)){
        install.packages(
                libs[!installed_libs]
        )
}

invisible(lapply(libs,library,
                 character.only=T))
#
providers <- c("OpenStreetMap", "OpenStreetMap.DE", 
               "OpenStreetMap.France", "OpenStreetMap.HOT", 
               "OpenTopoMap","Esri.WorldStreetMap",  
               "Esri.WorldTopoMap", "Esri.WorldImagery",
               "Esri.WorldGrayCanvas", "CartoDB.Positron", "CartoDB.PositronNoLabels",
"CartoDB.PositronOnlyLabels", "CartoDB.DarkMatter", "CartoDB.DarkMatterNoLabels", "CartoDB.DarkMatterOnlyLabels", "CartoDB.Voyager", "CartoDB.VoyagerNoLabels", "CartoDB.VoyagerOnlyLabels",
"Thunderforest.OpenCycleMap", "Thunderforest.Transport", "Thunderforest.TransportDark", "Thunderforest.SpinalMap", "Thunderforest.Landscape", "Thunderforest.Outdoors", "Thunderforest.Pioneer", "Thunderforest.MobileAtlas", "Thunderforest.Neighbourhood")
# country boundaries ----


crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"
country_sf <- giscoR::gisco_get_nuts(
        #country = "ES",
        nuts_id="ES41",
        resolution = "1"
) %>%
        sf::st_transform(crs = crsLONGLAT)


# elevation ---
elev <- elevatr::get_elev_raster(
        locations = country_sf,
        z=1,
#        clip="location"
)

names(elev) <- "elevation"

rgl::close3d()
dem <- rayvista::plot_3d_vista(
        dem = elev$elevation,
        overlay_detail = 8,
        #img_provider = providers[30],
        # img_provider = "Thunderforest.Outdoors",
#        api_key = "76d31526cab940eda6f3e332723f3e69",
        outlier_filter = 0.001, 
        zscale = 1,
        zoom = 0.5,
        theta = -10,
        phi = 30,
        solid = T,
        windowsize=c(800,800)
)
#
## coordinates ----
#tapita
 tapia_lat <- 42.74798
 tapia_long <- -5.783270
# Valdeon
 tapia_lat <- 43.151633
tapia_long <- -4.917941
# North MAcedonia
tapia_lat <- 40.998724
tapia_long <- 20.825064
# Maastricht
tapia_lat <- 50.827318
tapia_long <- 5.716192

# Sestri levante
tapia_lat <- 44.266443
tapia_long <- 9.41

# Cinqueterre
tapia_lat <- 44.142424
tapia_long <- 9.667007

# Fuente Dé
tapia_lat <- 43.141509
tapia_long <- -4.811261

# Tepoztlan
tapia_lat <- 18.98528
tapia_long <- -99.09972
 
# wintersports
tapia_lat <- 47.178034 
tapia_long <- 11.799512

tapia <- rayvista::plot_3d_vista(
        lat=tapia_lat,
        long=tapia_long,
        radius = 8e3,
        zscale=3,
        zoom=.6,
        solid=T,
        elevation_detail = 13,
        overlay_detail = 15,
        theta=0,
        windowsize=500,
        background="grey",
        img_provider = providers[8],
        # img_provider = "Thunderforest.Outdoors",
         api_key = "76d31526cab940eda6f3e332723f3e69",
        outlier_filter = 0.002, 
        # water = TRUE,
        # waterdepth = 0,
        # wateralpha = 0.5,
        # watercolor = "#233aa1",
        # waterlinecolor = "white",
        # waterlinealpha = 0.5
)
# Tepoztlan 
library(gpx)
df <- read_gpx( "C:/Users/dmend/Downloads/activity_9887561050.gpx")

path <- df$tracks$`Tepoztlán Hiking`[df$tracks$`Tepoztlán Hiking`$Time <df$tracks$`Tepoztlán Hiking`[df$tracks$`Tepoztlán Hiking`$Elevation ==max(df$tracks$`Tepoztlán Hiking`$Elevation),]$Time,]

render_path(extent = attr(tapia,"extent"),  
            lat = path$Latitude, 
            long = path$Longitude, 
            #altitude = df$tracks$`Tepoztlán Hiking`$Elevation+10, 
            color="grey",
            heightmap = tapia,
            zscale=3,
            resample_evenly = T,
            antialias=TRUE,
            clear_previous = T)

rayshader::render_label(
        heightmap= tapia, 
        text='Posada Gemma', 
        lat = 18.985142,
        long = -99.106927, 
        extent = attr(tapia, 'extent'),
        altitude=80,
        clear_previous = T, 
        zscale = 3,
        textcolor = "white",
        linecolor = "white",
        textsize = 2
)
rayshader::render_label(
        heightmap= tapia, 
        text='La boda', 
        lat = 18.977291,
        long=-99.116845, 
        extent = attr(tapia, 'extent'),
        altitude=80,
        clear_previous = F, 
        zscale = 3,
        textcolor = "white",
        linecolor = "white",
        textsize = 2
)
rayshader::render_label(
        heightmap= tapia, 
        text='La pirámide', 
        lat = 19.000661,
        long=-99.101230, 
        extent = attr(tapia, 'extent'),
        altitude=100,
        clear_previous = F, 
        zscale = 3,
        textcolor = "white",
        linecolor = "white",
        textsize = 2
)


# Tapia
rayshader::render_label(
        heightmap= tapia, 
        text='Tapia de la Ribera', 
        lat = 42.747464,
        long = -5.780610, 
        extent = attr(tapia, 'extent'),
        altitude=80,
        clear_previous = T, 
        zscale = 2
        )
rayshader::render_label(
        heightmap= tapia, 
        text='La Torre', 
        lat = 42.748888,
        long=-5.787430, 
        extent = attr(tapia, 'extent'),
        altitude=80,
        clear_previous = F, 
        zscale = 2
)
rayshader::render_label(
        heightmap= tapia, 
        text='El accidente', 
        lat = 42.752243,
        long=-5.792305, 
        extent = attr(tapia, 'extent'),
        altitude=100,
        clear_previous = F, 
        zscale = 2
)

# NMAK
rayshader::render_label(
        heightmap= tapia, 
        text='Resen', 
        lat = 41.0804,
        long = 21.011962, 
        extent = attr(tapia, 'extent'),
        altitude=500,
        clear_previous = T, 
        zscale = 10,
        textsize = 1.5,
        textcolor = "white"
)

rayshader::render_label(
        heightmap= tapia, 
        text='Ohrid', 
        lat = 41.113158,
        long = 20.810015, 
        extent = attr(tapia, 'extent'),
        altitude=500,
        clear_previous = F, 
        zscale = 10,
        textsize = 1.5,
        textcolor = "white"
)


## play with dots ----
library(rayrender)

montereybay %>%
        sphere_shade(zscale = 10, texture = "imhof1") %>%
        plot_3d(montereybay, zscale = 50, fov = 70, theta = 270, phi = 30, 
                windowsize = c(1000, 800), zoom = 0.6,  
                water = TRUE, waterdepth = 0, wateralpha = 0.5, watercolor = "#233aa1",
                waterlinecolor = "white", waterlinealpha = 0.5)
Sys.sleep(0.2)
render_highquality(lightdirection = c(-45,45), lightaltitude  = 30, clamp_value = 10, 
                   samples = 256, camera_lookat= c(0,-50,0),
                   ground_material = diffuse(color="grey50",checkercolor = "grey20", checkerperiod = 100),
                   clear = TRUE)
moss_landing_coord = c(36.806807, -121.793332) 
x_vel_out = -0.001 + rnorm(1000)[1:500]/1000
y_vel_out = rnorm(1000)[1:500]/200
z_out = c(seq(0,2000,length.out = 180), seq(2000,0,length.out=10), 
          seq(0,2000,length.out = 100), seq(2000,0,length.out=10))

bird_track_lat = list()
bird_track_long = list()
bird_track_lat[[1]] = moss_landing_coord[1]
bird_track_long[[1]] = moss_landing_coord[2]

for(i in 2:500) {
        bird_track_lat[[i]] = bird_track_lat[[i-1]] + y_vel_out[i]
        bird_track_long[[i]] = bird_track_long[[i-1]] + x_vel_out[i]
}

render_path(extent = attr(montereybay,"extent"),  
            lat = unlist(bird_track_lat), long = unlist(bird_track_long), 
            altitude = z_out, zscale=50,  antialias=TRUE)


## Rivers ----

url <- "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_eu_shp.zip"
download.file(
        url = url,
        destfile = basename(url),
        mode="wb"
)
unzip(basename(url))
list.files()


## Shortest path ----


