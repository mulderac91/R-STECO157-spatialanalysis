# Make a hexagonal grid over the bounding box of a sf object
st_make_hexgrid <- function(x, cellwidth, cellarea, clip = TRUE) {
  # x	        = object of class sf or sfc
  # cellwidth = hexagon width, or
  # cellarea  = hexagon area
  # clip      = should hexagons be clipped beyond borders of x?

  # Error messages
  if (missing(x))
    stop("Provide object of class sf")

  # Cellwidth and cellheight
  if (missing(cellwidth)) {
    if (missing(cellarea)) {
      stop("Provide value for cellwidth or cellarea")
    }
    cellwidth <- sqrt(2*cellarea/sqrt(3))
  }

  # Create buffer
  x.buff <- x %>% st_buffer(dist = cellwidth, nQuadSegs = 3)

  # Create the hex-grid
  require(sp)
  hex <- spsample(as_Spatial(x.buff), type = "hexagonal", cellsize = cellwidth, offset = c(0.5, 0.5)) %>%
    HexPoints2SpatialPolygons(dx = cellwidth) %>%
    st_as_sf %>%
    st_set_crs(st_crs(x))

  # Clip hexagons?
  if (clip) {
    # Parallel clipping by intersection
    require(parallel)
    n.cores <- detectCores()
    hex <- do.call(
      args = mclapply(
        X = splitIndices(nx = nrow(hex), ncl = n.cores),
        FUN = function(i) st_intersection(hex[i, ], x),
        mc.cores = n.cores),
      what = rbind)
  }
  # Return output
  hex
}
