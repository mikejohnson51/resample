col_lu <- data.frame(
  nlcd.code = c(-.1, 0, 11, 12, 21, 22, 23, 24, 31, 41, 42, 43, 51, 52, 71, 72, 73, 74, 81, 82, 90, 95),
  nwm.code  = c(-.1, 0, 16, 23, NA, 1,  NA, NA, 19, 11, 14, 15, 22, 8,  7,  20, NA, NA, 2,  3,  18, 17),

  color = c("#000000",
            "#476BA0", "#D1DDF9",
            "#DDC9C9", "#D89382", "#ED0000", "#AA0000",
            "#B2ADA3",
            "#68AA63", "#1C6330", "#B5C98E",
            "#A58C30", "#CCBA7C",
            "#E2E2C1", "#C9C977", "#99C147", "#77AD93",
            "#DBD83D", "#AA7028",
            "#BAD8EA", "#70A3BA", NA) ,

  name = c(NA, "EMPTY", "Open Water", "Ice/Snow", "Developed (Open)", "Developed (Low)", 'Developed (Medium)', 'Developed (High)', "Barren",
           "Deciduous Forest", "Evergreen Forest", "Mixed Forest", "Dwarf Scrub", "Shurb", "Grassland", "Sedge", 'Lichens', "Moss",
           "Pasture", "Culitivated Crops", "Woody Wetlands", "Herbaceous Wetlands"),

  stringsAsFactors = FALSE)
