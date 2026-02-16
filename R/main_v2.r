# 1. Packages
#------------
install.packages("pacman")
pacman::p_load(
    geodata, sf, elevatr, terra,
    tidyterra, colorspace, tidyverse,
    metR, ggspatial, rayshader
)

# 2. Load Kerala shapefile with districts
#---------------------------------------
# Read the shapefile from the data folder
districts_sf <- sf::st_read("data/kerala_district.shp")

# Check the structure and unique districts
print(unique(districts_sf$DISTRICT))

# 3. Create output directory for district maps
#--------------------------------------------
output_dir <- paste0("kerala_district_maps_", format(Sys.time(), "%Y%m%d_%H%M%S"))
dir.create(output_dir, showWarnings = FALSE)

# 4. Loop through each district
#-----------------------------
for(district in unique(districts_sf$DISTRICT)) {
    
    # Select the current district
    region_sf <- districts_sf[districts_sf$DISTRICT == district, ]
    
    # Print progress
    cat("Processing district:", district, "\n")
    
    # 5. Download & prepare DEM for this district
    #--------------------------------------------
    dem_rast <- elevatr::get_elev_raster(
        region_sf,
        z = 9, clip = "locations"
    )
    
    # Pseudo-Mercator projection
    proj <- "EPSG:3857"
    
    # Convert raster to data.frame for ggplot
    dem_rast_proj <- dem_rast |>
        terra::rast() |>
        terra::project(proj)
    
    dem_rast_proj[dem_rast_proj < 0] <- 0
    
    dem_df <- as.data.frame(dem_rast_proj, xy = TRUE)
    
    colnames(dem_df) <- c("x", "y", "elevation")
    
    # Skip if no valid elevation data
    if(all(is.na(dem_df$elevation)) || nrow(dem_df) == 0) {
        cat("No elevation data for district:", district, "\n")
        next
    }
    
    # 6. Compute breaks & limits for this district
    # --------------------------------------------
    limits <- range(dem_df$elevation, na.rm = TRUE)
    
    # Adjust breaks based on elevation range
    if(diff(limits) < 200) {
        # For areas with low elevation variation, use smaller intervals
        breaks <- seq(
            floor(limits[1] / 50) * 50,
            ceiling(limits[2] / 50) * 50,
            by = max(50, round(diff(limits)/10, -1))
        )
    } else {
        breaks <- seq(
            floor(limits[1] / 50) * 50,
            ceiling(limits[2] / 50) * 50,
            by = 200
        )
    }
    
    # Ensure breaks vector is valid
    if(length(breaks) < 2) {
        breaks <- seq(limits[1], limits[2], length.out = 5)
    }
    
    # 7. Build your hypsometric palette
    # ---------------------------------
    pal_vec <- tidyterra::hypso.colors2(
        n = min(12, length(breaks)-1),
        palette = "dem_poster",
        alpha = 1,
        rev = FALSE
    )
    
    # Adjust palette if needed
    if(length(pal_vec) >= 6) {
        light_col <- colorspace::lighten(pal_vec[2], amount = 0.15)
        dark_col <- colorspace::darken(pal_vec[5], amount = 0.25)
    } else {
        # Fallback colors if palette is too small
        light_col <- "white"
        dark_col <- "black"
        pal_vec <- colorRampPalette(c("blue", "green", "yellow", "brown"))(length(breaks)-1)
    }
    
    # 8. Define a custom theme
    # ------------------------
    theme_for_the_win <- function() {
        theme_minimal(base_family = "Helvetica") +
            theme(
                axis.line = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                plot.background = element_rect(
                    fill = "white", color = NA
                ),
                plot.title = element_text(
                    size = 16, color = "grey10",
                    hjust = .5, margin = margin(b = 5),
                    vjust = -5
                ),
                plot.caption = element_text(
                    size = 8, face = "italic", hjust = 1,
                    margin = margin(t = 5), vjust = 15
                ),
                plot.margin = unit(
                    c(
                        t = .1, r = .1,
                        l = .1, b = .1
                    ), "lines"
                ),
                legend.position = "right"
            )
    }
    
    # 9. Build the 2D Tanaka‐style map
    # --------------------------------
    gg_tanaka_hypso <- ggplot(
        data = dem_df, aes(x = x, y = y, z = elevation)
    ) +
        geom_contour_fill(
            breaks = breaks
        ) +
        scale_fill_gradientn(
            name = "Elevation",
            colors = pal_vec,
            breaks = breaks,
            labels = round(breaks, 0),
            limits = limits,
            guide = guide_colourbar(
                title.position = "top",
                title.hjust = .5,
                ticks = FALSE,
                barheight = unit(5, "cm"),
                frame.colour = NA
            )
        ) +
        metR::geom_contour_tanaka(
            breaks = breaks,
            sun.angle = 45,
            light = light_col,
            dark = dark_col,
            range = c(0.01, 0.3),
            smooth = 0.8
        ) +
        ggspatial::annotation_scale(
            location = "bl",
            width_hint = 0.25,
            text_cex = 0.7
        ) +
        ggspatial::annotation_north_arrow(
            location = "bl",
            which_north = "true",
            pad_x = unit(0.1, "in"),
            pad_y = unit(0.6, "in"),
            style = north_arrow_fancy_orienteering()
        ) +
        coord_sf(crs = proj) +
        labs(
            title = paste(district, ": Digital Elevation Model"),
            caption = "Data: Amazon Web Services Tiles"
        ) +
        theme_for_the_win()
    
    # Save 2D map
    png_filename_2d <- file.path(output_dir, paste0("kerala-", tolower(gsub(" ", "_", district)), "-tanaka-2d.png"))
    ggsave(
        png_filename_2d, gg_tanaka_hypso,
        width = 7, height = 7, bg = "white"
    )
    
    # 10. 3D extrusion & high‐quality render
    # --------------------------------------
    tryCatch({
        rayshader::plot_gg(
            ggobj = gg_tanaka_hypso,
            width = 7,
            height = 7,
            scale = 150,
            shadow = TRUE,
            shadow_intensity = 1,
            windowsize = c(700, 700),
            zoom = 0.55,
            phi = 60,
            theta = 0,
            background = "white",
            multicore = TRUE
        )
        
        # 11. Prepare HDR environment map (only download once)
        # ------------------------------
        u <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/venice_sunrise_4k.hdr"
        hdri_file <- "venice_sunrise_4k.hdr"
        
        if(!file.exists(hdri_file)) {
            download.file(
                url = u,
                destfile = hdri_file,
                mode = "wb"
            )
        }
        
        # 12. Final high‐quality PNG for 3D
        # --------------------------
        png_filename_3d <- file.path(output_dir, paste0("kerala-", tolower(gsub(" ", "_", district)), "-tanaka-3d.png"))
        rayshader::render_highquality(
            filename = png_filename_3d,
            preview = TRUE,
            light = FALSE,
            environment_light = hdri_file,
            intensity = 3,
            rotate_env = 90,
            parallel = TRUE,
            width = 1800,
            height = 1800,
            interactive = FALSE
        )
    }, error = function(e) {
        cat("Error generating 3D map for", district, ":", e$message, "\n")
        # Still save a basic version if 3D rendering fails
        png_filename_3d <- file.path(output_dir, paste0("kerala-", tolower(gsub(" ", "_", district)), "-tanaka-3d-failed.png"))
        ggsave(png_filename_3d, gg_tanaka_hypso, width=7, height=7, bg="white")
    })
}

cat("\nAll district maps have been generated and saved in:", output_dir, "\n")