GeomArrowBar <- ggproto("GeomArrowBar", Geom,
                        required_aes = c("x", "y", "xmin"),
                        default_aes = aes(colour = NA, fill = "grey20", size = 0.5, linetype = 1, alpha = 1),
                        extra_params = c("na.rm", "head_width", "column_width", "head_length"),
                        draw_key = draw_key_polygon,
                        draw_panel = function(data, panel_params, coord, head_width = 1,
                                              column_width = 1, head_length = 1) {
                          hwidth <- head_width / 5
                          wid <- column_width / 10
                          len <- head_length / 10
                          # data <- transform(data, xend = x, x = xmin, yend = y)
                          data2 <- data
                          # data2$x[1] <- xmin
                          data2$y[1] <- 0
                          zero <- coord$transform(data2, panel_params)$xmin
                          coords <- coord$transform(data, panel_params)
                          make_arrow_y <- function(y, wid, hwidth) {
                            c(y - wid/2, y - wid/2, y - hwidth/2, y, y + hwidth/2, y + wid/2, y + wid/2)
                          }
                          make_arrow_x <- function(x, zero, len){
                            if(x < zero){
                              len <- -len
                              basea <- min(zero, x - len)
                              return(c(zero, basea, basea, x, basea, basea, zero))
                            } else {
                              basea <- max(zero, x - len)
                              return(c(zero, basea, basea, x, basea, basea, zero))
                            }
                          }
                          my_tree <- grid::gTree()
                          for(i in seq(nrow(coords))){
                            my_tree <- grid::addGrob(my_tree, grid::polygonGrob(
                              make_arrow_x(coords$x[i], zero[i], len),
                              make_arrow_y(coords$y[i], wid, hwidth),
                              default.units = "native",
                              gp = grid::gpar(
                                col = coords$colour[i],
                                fill = scales::alpha(coords$fill[i], coords$alpha[i]),
                                lwd = coords$size[i] * .pt,
                                lty = coords$linetype[i]))) }
                          my_tree}
)

geom_arrowbar <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE, head_width = 1, column_width = 1,
                          head_length = 1, ...)
{
  layer(geom = GeomArrowBar, mapping = mapping, data = data, stat = stat,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, head_width = head_width,
                      column_width = column_width, head_length = head_length, ...))
}
