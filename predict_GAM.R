# GAM predictions: municipality maps for specific NI years
kommune.poly <- readOGR(dsn = "Data/Shapefiles", layer = "Norway municipalities", encoding = "")
norway <- raster("Data/Raster/Norway.tif")  # Background raster of Norway (all values = 1)
year <- c(1900, 1950, 1990, 2000, 2010, 2014, 2019)
xy <- coordinates(norway)
x <- y <- norway
values(x) <- xy[,1]
values(y) <- xy[,2]
NIGAM_All.list <- list()
pdf("Results/GAMplotsMunicAll.pdf")
# load("Results/gam.results.all.gamma3.RData")  # Uncomment to load results from previous steps, if entire script is not run continuously
for(j in 1:length(species)){
  print(species[j])
  NIGAM_All.list[[j]] <- list()
  for(i in 1:length(year)){
    print(year[i])
    pred.ras <- stack(year[i]*norway, x, y, log(norway))
    names(pred.ras)[1:4] <- c("year", "x", "y", "logS")
    pred.dat <- as.data.frame(values(pred.ras))
    p <- norway
    pred <- predict(gam.results[[j]], pred.dat, se.fit = TRUE, type = "response") # The ordinary raster prediction does not always work: p <- predict(pred.ras, gam.results[[j]], type = "response")
    values(p) <- as.vector(pred$fit)
    p.poly <- extract(p, kommune.poly, sp = TRUE, fun = mean, na.rm = T, weights = TRUE, normalizeWeights = TRUE)
    b <- seq(0, 0.1, by = 0.001)
    n <- length(b)
    plot(p.poly, col = rev(terrain.colors(n-1))[raster::cut(p.poly$Norway, breaks = b)], main = paste(species[j], year[i]))
    values(p) <- as.vector(pred$se.fit)
    p.poly.se <- extract(p, kommune.poly, sp = TRUE, fun = mean, na.rm = T, weights = TRUE, normalizeWeights = TRUE)
    NIGAM_All.list[[j]][[i]] <- list(p = p.poly, p.se = p.poly.se)
  }
}
dev.off()
names(NIGAM_All.list) <- species
