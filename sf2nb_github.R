# Construct neighbours list (graph) from sf polygons,
# but without disjoint connected subgraphs in the graph
sf2nb <- function(x) {

  # Require packages
  require(spdep)
  require(RANN)

  # Convert sf object to SpatialPolygons object
  # There is no implementation of spdep functions for sf objects (yet)
  x.sp <- as_Spatial(st_geometry(x))

  # Create neighbours list
  x.nb <- poly2nb(x.sp, queen = FALSE)

  # Get Coordinates
  x.coords <- coordinates(x.sp)

  # Identify disjoint connected subgraphs
  x.comp <- n.comp.nb(nb = x.nb)

  # Count number of subgraphs
  n.subgraph <- x.comp$nc

  # While the number of subgraphs is > 1, connect subgraphs by connecting closest neighbours
  # Result: spatial neighbours list without 'islands'
  while(n.subgraph > 1) {

    # Find index number of nodes within each subgraph
    x.comp$node.index <- NA
    for (i in 1:n.subgraph) {
      x.comp <- within(x.comp, node.index[comp.id == i] <- seq_len(sum(comp.id == i)))
    }

    # Split coordinates by subgraph
    x.coords.split <- split(
      x = data.frame(x.coords),
      f = x.comp$comp.id)

    # Distance matrix between all subgraphs
    dist.subgraph <- matrix(NA, nrow = n.subgraph, ncol = n.subgraph)
    for (i in 1:n.subgraph) {
      for (j in 1:n.subgraph) {
        # Get distances between all points in x.coords.split[[j]] and nearest point in x.coords.split[[i]]
        # Use nn2 function from RANN package for fast nearest neighbour search
        nn.list <- nn2(
          data  = x.coords.split[[i]],
          query = x.coords.split[[j]],
          k = 1)
        # Return nearest distance between x.coords.split[[i]] and x.coords.split[[j]]
        dist.subgraph[i, j] <- with(nn.list, nn.dists[which.min(nn.dists)])
      }
    }
    # Set diagonal (distance = 0) to infinity
    diag(dist.subgraph) <- Inf

    # Which two subgraphs are the closest to eachother and should be connected?
    index1 <- unlist(apply(
      X = dist.subgraph == dist.subgraph[which.min(dist.subgraph)],
      MARGIN = 1,
      FUN = which))

    # Which nodes between the two subgraphs should be connected?
    nn.list1 <- nn2(
      data  = x.coords.split[[index1[1]]],
      query = x.coords.split[[index1[2]]],
      k = 1)
    nn.list2 <- nn2(
      data  = x.coords.split[[index1[2]]],
      query = x.coords.split[[index1[1]]],
      k = 1)
    index2 <- c(
      with(nn.list1, nn.idx[which.min(nn.dists)]),
      with(nn.list2, nn.idx[which.min(nn.dists)]))

    # These two nodes are to be connected
    add <- with(x.comp, c(
      which(comp.id == index1[1] & node.index == index2[1]),
      which(comp.id == index1[2] & node.index == index2[2])))

    # Make the connection (made easy via adjacency matrix)
    x.mat <- nb2mat(
      neighbours = x.nb,
      style = "B",
      zero.policy = TRUE)
    # It should be symmetric of course
    x.mat[add[1], add[2]] <- x.mat[add[2], add[1]] <- 1

    # Create new neighbours list
    x.nb <- mat2listw(x.mat)$neighbours

    # Re-identify disjoint connected subgraphs
    x.comp <- n.comp.nb(nb = x.nb)

    # Re-count number of subgraphs
    n.subgraph <- x.comp$nc
  }

  # Return output
  x.nb
}
