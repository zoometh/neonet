# functions used by the NeoNet Shiny app
intCal <- 'intcal20'

findLocations <- function(shape, location_coordinates, location_id_colname){
  # finding the dates locations inside the shapes we draw
  # derive polygon coordinates and feature_type from shape input
  polygon_coordinates <- shape$geometry$coordinates
  feature_type <- shape$properties$feature_type
  # transform into a spatial polygon
  drawn_polygon <- Polygon(do.call(rbind,lapply(polygon_coordinates[[1]],
                                                function(x){c(x[[1]][1], x[[2]][1])})))
  drawn.polyg <- sp::SpatialPolygons(list(sp::Polygons(list(drawn_polygon), "drawn_polygon")))
  if (class(location_coordinates) != "standardGeneric"){
    # if shape exist
    selected_locs <- sp::over(location_coordinates, drawn.polyg) # intersection
    x = (location_coordinates[which(!is.na(selected_locs)), location_id_colname])
    selected_loc_id = as.character(x[[location_id_colname]])
    return(selected_loc_id)
  } else {
    # if shape doesn't exist
    x = (location_coordinates[which(is.na(selected_locs)), location_id_colname])
    selected_loc_id = as.character(x[[location_id_colname]])
    return(selected_loc_id)
  } 
}

f.gcalib <- function(some_14C, clicked_site, C14.grouped){
  # plot various calibrated dates in order to cumulated dates for:
  # i) same period, ii) same us, iii) etc.
  ## graphical param
  gcalib.w <- 1500 # abs, px
  gcalib.h <- 200 # rel, px
  gcalib.lbl.sz <- 1.5 # text
  gcalib.strip.text.sz <- 2.5 # facet label
  gcalib.axis.title.sz <- 4
  gcalib.xaxis.sz <- 3
  gcalib.yaxis.sz <- 2
  gcalib.gline.sz <- .3 # geom_line sz
  gcalib.bin <- 100 # the chrono granul
  dat.cumul <- data.frame(datations = numeric(0),
                          densites = numeric(0),
                          lbl = character(0),
                          color = character(0))
  if (C14.grouped == "C14all"){
    # only one curve
    nb.c14 <- nrow(some_14C)
    cumul.caldates <- rcarbon::calibrate(x = some_14C$C14Age,
                                         errors = some_14C$C14SD,
                                         calCurves = intCal,
                                         verbose = FALSE)
    # large range, if not possible error
    cumul.spd <- rcarbon::spd(cumul.caldates,
                              timeRange = c(15000, 1000))
    datees <- cumul.spd[[2]][[1]]
    densitt <- cumul.spd[[2]][[2]]
    dens0 <- which(densitt %in% c(densitt == 0)) # rm densities = 0
    dat1.ageGrid <- datees[-dens0]
    dat1_densities <- densitt[-dens0]
    dat1.ageGrid <- -1*as.numeric(lapply(dat1.ageGrid, function(x){return(x-1950)})) # to BC
    a.dat <- data.frame(datations = dat1.ageGrid,
                        densites = dat1_densities)
    nb.sites <- length(unique(some_14C$SiteName))
    a.dat$site.per <- "all C14" 
    a.dat$dat.code <- paste0("** ",as.character(nb.sites)," SITES **","\n",
                             "** ",as.character(nb.c14)," dates **") # lbl is nb of dates
    a.dat$means <- weighted.mean(dat1.ageGrid,dat1_densities)
    a.dat$color <- "black"
    a.dat$clicked <- 0
    a.dat$alpha <- .5
    dat.cumul <- rbind(dat.cumul,a.dat)
  }
  if (C14.grouped == "C14groupp"){
    # - - - - - - - - - - - - - -
    # group on period
    for(a.per in unique(some_14C$Period)){
      some_14C.a.per <- some_14C[some_14C$Period == a.per, ]
      nb.c14 <- nrow(some_14C.a.per)
      if(nb.c14 > 1){
        # various dates
        cumul.caldates <- rcarbon::calibrate(x = some_14C.a.per$C14Age,
                                             errors = some_14C.a.per$C14SD,
                                             calCurves = intCal,
                                             verbose = FALSE)
        # large range, if not possible error
        cumul.spd <- rcarbon::spd(cumul.caldates, timeRange=c(15000, 1000))
        datees <- cumul.spd[[2]][[1]]
        densitt <- cumul.spd[[2]][[2]]
        dens0 <- which(densitt %in% c(densitt == 0)) # rm densities = 0
        dat1.ageGrid <- datees[-dens0]
        dat1_densities <- densitt[-dens0]
        dat1.ageGrid <- -1*as.numeric(lapply(dat1.ageGrid, function(x){return(x-1950)})) # to BC
        a.dat <- data.frame(datations = dat1.ageGrid,
                            densites = dat1_densities)
        nb.sites <- length(unique(some_14C.a.per$SiteName))
        a.dat$site.per <- a.per 
        a.dat$dat.code <- paste0("** ",as.character(nb.sites)," SITES **","\n",
                                 "** ",as.character(nb.c14)," dates **") # lbl is nb of dates
        a.dat$means <- weighted.mean(dat1.ageGrid,dat1_densities)
        a.dat$color <- some_14C.a.per[1,"colors"]
        dat.cumul <- rbind(dat.cumul,a.dat)
      }
      if(nb.c14 == 1){
        # 1 date
        ages3 <- BchronCalibrate(ages=some_14C.a.per[, "C14Age"], 
                                 ageSds=some_14C.a.per[, "C14SD"], 
                                 positions=c(1:nrow(some_14C.a.per)), 
                                 calCurves=c(rep(intCal, nrow(some_14C.a.per)))
        )
        # single 14C by LabCode
        dat1.ageGrid <- ages3$Date1$ageGrid # load the 14C age
        dat1_densities <- ages3$Date1$densities # load the 14C age
        dat1.ageGrid <- -1*as.numeric(lapply(dat1.ageGrid, function(x){return(x-1950)})) # to BC
        a.dat <- data.frame(datations = dat1.ageGrid,
                            densites = dat1_densities)
        a.dat$site.per <- paste0(some_14C.a.per[, "SiteName"],'\n',
                                 some_14C.a.per[, "Period"])
        a.dat$dat.code <- paste0(some_14C.a.per[, "SiteName"],' - ',
                                 some_14C.a.per[, "Period"],"\n",
                                 some_14C.a.per[, "C14Age"],'+/-',
                                 some_14C.a.per[, "C14SD"],' BP [',
                                 some_14C.a.per[, "LabCode"],']')
        a.dat$means <- weighted.mean(dat1.ageGrid, dat1_densities)
        a.dat$color <- some_14C.a.per[,"colors"]
        dat.cumul <- rbind(dat.cumul,a.dat)
      }
    }
    dat.cumul$clicked <- 0
    dat.cumul$alpha <- .5
  }
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # group on sites and us (= layer = PhaseCode) - - - - - - - - - - - - - - - -
  if (C14.grouped == "C14groupsl"){
    for (a.site in unique(some_14C$SiteName)){
      # loop on sites
      some_14C.a.site <- some_14C[some_14C$SiteName == a.site, ]
      uniq.PhaseCode <- unique(some_14C.a.site$PhaseCode)
      # remove 'unknown'
      uniq.PhaseCode <- uniq.PhaseCode[!uniq.PhaseCode %in% c("unknown")]
      d.colPhaseCode <- data.frame(colors.phase = colorRampPalette(c("#800080", "#ffb6c1"))(length(uniq.PhaseCode)),
                                   PhaseCode = uniq.PhaseCode,
                                   stringsAsFactors = F)
      # add unknown/light grey
      d.colPhaseCode[nrow(d.colPhaseCode)+1,] <- c("#d3d3d3", "unknown") # grey
      # merge
      some_14C.a.site <- merge(some_14C.a.site,d.colPhaseCode,by="PhaseCode",all.x=T)
      for(a.us in unique(some_14C.a.site$PhaseCode)){
        # loop on us
        some_14C.a.site.us <- some_14C.a.site[some_14C.a.site$PhaseCode == a.us, ]
        nb.c14 <- nrow(some_14C.a.site.us)
        if(nb.c14 > 1){
          # cumulated 14C
          cumul.caldates <- rcarbon::calibrate(x = some_14C.a.site.us$C14Age,
                                               errors = some_14C.a.site.us$C14SD,
                                               calCurves = intCal,
                                               verbose = FALSE)
          # large range, if not possible error
          cumul.spd <- rcarbon::spd(cumul.caldates,timeRange=c(15000, 1000))
          datees <- cumul.spd[[2]][[1]]
          densitt <- cumul.spd[[2]][[2]]
          dens0 <- which(densitt %in% c(densitt == 0)) # rm densities = 0
          dat1.ageGrid <- datees[-dens0]
          dat1_densities <- densitt[-dens0]
          dat1.ageGrid <- -1*as.numeric(lapply(dat1.ageGrid, function(x){return(x-1950)})) # to BC
          a.dat <- data.frame(datations = dat1.ageGrid,
                              densites = dat1_densities)
          site.per <- paste0(some_14C.a.site.us[1, "SiteName"],'\n',
                             some_14C.a.site.us[1, "PhaseCode"])
          a.dat$site.per <- site.per 
          wm <- weighted.mean(dat1.ageGrid,dat1_densities)
          a.dat$dat.code <- paste0(some_14C.a.site.us[1, "SiteName"],' - ', some_14C.a.site.us[1,"PhaseCode"],"\n",
                                   "** ",as.character(nb.c14)," dates **") # lbl is nb of dates
          a.dat$means <- wm
          a.dat$color <- some_14C.a.site.us[1,"colors.phase"]
          dat.cumul <- rbind(dat.cumul,a.dat)
        }
        if(nb.c14 == 1){
          # 1 date
          ages3 <- BchronCalibrate(ages = some_14C.a.site.us[,"C14Age"], 
                                   ageSds = some_14C.a.site.us[,"C14SD"], 
                                   positions = c(1:nrow(some_14C.a.site.us)), 
                                   calCurves = c(rep(intCal, nrow(some_14C.a.site.us)))
          )
          dat1.ageGrid <- ages3$Date1$ageGrid # load the 14C age
          dat1_densities <- ages3$Date1$densities # load the 14C age
          dat1.ageGrid <- -1*as.numeric(lapply(dat1.ageGrid, function(x){return(x-1950)})) # to BC
          a.dat <- data.frame(datations = dat1.ageGrid,
                              densites = dat1_densities)
          a.dat$site.per <- paste0(some_14C.a.site.us[, "SiteName"],'\n',
                                   some_14C.a.site.us[, "Period"],'\n',
                                   some_14C.a.site.us[, "PhaseCode"])
          a.dat$dat.code <- paste0(some_14C.a.site.us[, "SiteName"],' - ',
                                   some_14C.a.site.us[, "Period"],"\n",
                                   some_14C.a.site.us[, "PhaseCode"],"\n",
                                   some_14C.a.site.us[, "C14Age"],'+/-',
                                   some_14C.a.site.us[, "C14SD"],' BP\n [',
                                   some_14C.a.site.us[, "LabCode"],']')
          a.dat$means <- weighted.mean(dat1.ageGrid,dat1_densities)
          a.dat$color <- some_14C.a.site.us[,"colors.phase"]
          dat.cumul <- rbind(dat.cumul,a.dat)
        }
      }
    }
    dat.cumul$clicked <- 0
    dat.cumul$alpha <- .5
  }
  # group on sites and periods - - - - - - - - - - - - - - - -
  if (C14.grouped == "C14groupsp"){
    for(a.per in unique(some_14C$Period)){
      # loop on periods
      some_14C.a.per <- some_14C[some_14C$Period == a.per, ]
      for (a.site in unique(some_14C.a.per$SiteName)){
        # loop on sites
        some_14C.a.per.site <- some_14C.a.per[some_14C.a.per$SiteName == a.site, ]
        nb.c14 <- nrow(some_14C.a.per.site)
        if(nb.c14 > 1){
          # cumulated 14C
          cumul.caldates <- rcarbon::calibrate(x = some_14C.a.per.site$C14Age,
                                               errors = some_14C.a.per.site$C14SD,
                                               calCurves = intCal,
                                               verbose = FALSE)
          # large range, if not possible error
          cumul.spd <- rcarbon::spd(cumul.caldates,timeRange=c(15000, 1000))
          datees <- cumul.spd[[2]][[1]]
          densitt <- cumul.spd[[2]][[2]]
          dens0 <- which(densitt %in% c(densitt == 0)) # rm densities = 0
          dat1.ageGrid <- datees[-dens0]
          dat1_densities <- densitt[-dens0]
          dat1.ageGrid <- -1*as.numeric(lapply(dat1.ageGrid, function(x){return(x-1950)})) # to BC
          a.dat <- data.frame(datations = dat1.ageGrid,
                              densites = dat1_densities)
          site.per <- paste0(some_14C.a.per.site[1, "SiteName"],'\n',
                             some_14C.a.per.site[1, "Period"])
          a.dat$site.per <- site.per 
          wm <- weighted.mean(dat1.ageGrid,dat1_densities)
          a.dat$dat.code <- paste0(some_14C.a.per.site[1, "SiteName"],' - ', some_14C.a.per.site[1,"Period"],"\n",
                                   "** ",as.character(nb.c14), " dates **") # lbl is nb of dates
          a.dat$means <- wm
          a.dat$color <- some_14C.a.per.site[1,"colors"]
          dat.cumul <- rbind(dat.cumul,a.dat)
        }
        if(nb.c14 == 1){
          # 1 date
          ages3 <- BchronCalibrate(ages=some_14C.a.per.site[, "C14Age"], 
                                   ageSds=some_14C.a.per.site[, "C14SD"], 
                                   positions=c(1:nrow(some_14C.a.per.site)), 
                                   calCurves=c(rep(intCal, nrow(some_14C.a.per.site)))
          )
          dat1.ageGrid <- ages3$Date1$ageGrid # load the 14C age
          dat1_densities <- ages3$Date1$densities # load the 14C age
          dat1.ageGrid <- -1*as.numeric(lapply(dat1.ageGrid, function(x){return(x-1950)})) # to BC
          a.dat <- data.frame(datations = dat1.ageGrid,
                              densites = dat1_densities)
          a.dat$site.per <- paste0(some_14C.a.per.site[, "SiteName"],'\n',
                                   some_14C.a.per.site[, "Period"])
          a.dat$dat.code <- paste0(some_14C.a.per.site[, "SiteName"],' - ',
                                   some_14C.a.per.site[, "Period"],"\n",
                                   some_14C.a.per.site[, "C14Age"],'+/-',
                                   some_14C.a.per.site[, "C14SD"],' BP\n [',
                                   some_14C.a.per.site[, "LabCode"],']')
          a.dat$means <- weighted.mean(dat1.ageGrid,dat1_densities)
          a.dat$color <- some_14C.a.per.site[,"colors"]
          dat.cumul <- rbind(dat.cumul,a.dat)
        }
      }
    }
    dat.cumul$clicked <- 0
    dat.cumul$alpha <- .5
  }
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # not grouped
  if (C14.grouped == "C14ungroup"){
    ages3 <- BchronCalibrate(ages = some_14C[, "C14Age"], 
                             ageSds = some_14C[, "C14SD"], 
                             positions = c(1:nrow(some_14C)), 
                             calCurves = c(rep(intCal, nrow(some_14C)))
    )
    for(i in 1:length(ages3)){
      # single 14C by LabCode
      # i <- 1
      ageGid <- paste0("ages3$Date",i,"$ageGrid") # load the 14C age
      ageDen <- paste0("ages3$Date",i,"$densities") # load the 14C age
      dat1.ageGrid <- eval(parse(text=ageGid))
      dat1.ageGrid <- -1*as.numeric(lapply(dat1.ageGrid, function(x){return(x-1950)})) # to BC
      dat1_densities <- eval(parse(text=ageDen))
      a.dat <- data.frame(datations = dat1.ageGrid,
                          densites = dat1_densities)
      a.dat$site.per <- paste0(some_14C[i, "SiteName"],'\n',
                               some_14C[i, "Period"])
      a.dat$dat.code <- paste0(some_14C[i, "SiteName"],' - ',
                               some_14C[i, "Period"],"\n",
                               some_14C[i, "C14Age"],'+/-',
                               some_14C[i, "C14SD"],' BP \n[',
                               some_14C[i, "LabCode"],']')
      a.dat$means <- weighted.mean(dat1.ageGrid,dat1_densities)
      a.dat$color <- some_14C[i,"colors"]
      # test if clicked site on SiteName & date & code
      is.clicked.site <- some_14C[i, "SiteName"] == clicked_site[1]
      is.clicked.age <- some_14C[i, "C14Age"] == as.numeric(clicked_site[2])
      is.clicked.code <- some_14C[i, "LabCode"] == clicked_site[3]
      is.clicked <- is.clicked.site & is.clicked.age & is.clicked.code
      a.dat$clicked <- ifelse(is.clicked, 1, 0)
      a.dat$alpha <- ifelse(is.clicked, .5, .1)
      dat.cumul <- rbind(dat.cumul,a.dat)
      # }
    }
  }
  ## labels
  # facet ribbons
  supp.labs <- unique(dat.cumul$dat.code)
  names(supp.labs) <- as.character(unique(dat.cumul$means))
  # calibrated curves
  a.label <- data.frame(means = dat.cumul$means,
                        site.per = dat.cumul$site.per,
                        color = dat.cumul$color,
                        clicked = dat.cumul$clicked,
                        alpha = dat.cumul$alpha)
  a.label <- a.label[!duplicated(a.label), ]
  a.label <- a.label[with(a.label, order(means)), ]
  ## general info
  # x extent
  MM <- max(dat.cumul$datations)
  if(!is.na(MM %% gcalib.bin)){MM <- MM - (MM %% gcalib.bin)} # round to next xx'
  mm <- min(dat.cumul$datations)
  if(!is.na(mm %% gcalib.bin)){mm <- mm - (mm %% gcalib.bin)} # round to next xx'
  Md <- max(dat.cumul$densites)
  gcalib <- ggplot(dat.cumul)+
    # order on weighted means
    facet_grid(means ~ .,
               scales = "free_y", 
               switch = "both",
               labeller = labeller(means = supp.labs)) +
    geom_text(data = a.label,
              size = gcalib.lbl.sz,
              aes(x = means,
                  y = -Inf,
                  label = site.per,
                  colour = color,
                  vjust = -0.1,
                  fontface = ifelse(clicked, "bold", "plain"))) +
    geom_line(data = dat.cumul,
              aes(datations, densites, colour = color),
              linewidth = gcalib.gline.sz) +
    scale_color_identity() +
    xlab("cal BC") + 
    ylab("densities") +
    scale_x_continuous(breaks = seq(mm,MM,gcalib.bin),
                       sec.axis = dup_axis()) +
    scale_y_continuous(sec.axis = dup_axis()) +
    theme(legend.position = c(1, 1),
          strip.text = element_text(size = gcalib.strip.text.sz),
          strip.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          panel.background = element_rect(fill = 'transparent'),
          panel.grid.major = element_blank(),
          panel.grid.major.x = element_line(colour = 'grey', size = .1),
          panel.grid.minor = element_blank(),
          axis.title = element_text(size = gcalib.axis.title.sz),
          axis.text.y = element_text(size = gcalib.yaxis.sz),
          axis.text.x = element_text(size = gcalib.xaxis.sz, angle = 90, vjust = 0.5, hjust=1),
          axis.ticks = element_line(size = .01))
  return(gcalib)
}