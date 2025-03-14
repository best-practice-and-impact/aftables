# Recreate the hex sticker for {aftables}

# Having installed the Free Sans font:
#   https://www.fontspace.com/freesans-font-f13276

install.packages(c("remotes", "ggplotify"))  # if not yet installed
remotes::install_github("matt-dray/gex")
packageVersion("gex")  # v0.2.2

temp_path <- tempfile(fileext = ".png")
gex::open_device(temp_path)
gex::add_hex(col = "#042D45")
gex::add_text(
  "aftables",
  x = 0.5,
  y = 0.7,
  family = "FreeSans",
  size = 23,
  col = "white"
)
grid::pushViewport(grid::viewport(x = 0.5, y = 0.2, width = 1, height = 0.75))
p <- ggplotify::base2grob(
  \() {
    set.seed(2025)
    cols <- c(
      "#12436D",
      "#28A197",
      "#801650",
      "#F46A25",
      "#3D3D3D",
      "#A285D1",
      "#2073BC",
      "#6BACE6",
      "#BFBFBF"
    )
    c_num <- 10
    r_num <- 11
    nums <- expand.grid(1:r_num, 1:c_num)
    nums[["col"]] <- sample(cols, r_num * c_num, replace = TRUE)
    par(mar = rep(0, 4))
    plot(
      nums[[1]],
      nums[[2]],
      pch = "",
      xaxt = "n",
      yaxt = "n",
      bty = "n",
      axes = FALSE,
      frame.plot = FALSE,
      ann = FALSE
    )
    points(
      nums[[1]],
      nums[[2]],
      col = nums[["col"]],
      pch = 15,
      cex = 1.8
    )
  }
)
grid::grid.draw(p)
grid::popViewport()
gex::add_border(width = 0.04, col = "#2E2E2E")
gex::close_device()
system(paste("open", temp_path))
