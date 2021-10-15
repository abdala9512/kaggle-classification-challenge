library(ggplot2)



custom_style <- function(){

  font <- "Roboto"

  ggplot2::theme(


    plot.title = ggplot2::element_text(
      family = font,
      size = 18,
      face = 'bold'
    ),

    plot.subtitle = ggplot2::element_text(family=font,
                                          size=12,
                                          margin=ggplot2::margin(5,0,5,0)),
    plot.caption = ggplot2::element_text(
      family = font,
      size = 10
    ),



    legend.position = "right",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_text(
      family = font,
      size = 12
    ),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family=font,
                                        size=12,
                                        color="#222222"),


    axis.title = ggplot2::element_text(
      family = font,
      size = 12,
      face = 'bold'
    ),
    axis.text = ggplot2::element_text(family=font,
                                      size=10,
                                      color="#222222"),
    axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10)),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    #axis.line.x  = element_line(color =  "#000000"),



    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"),
    panel.grid.major.x = ggplot2::element_blank(),



    panel.background = ggplot2::element_blank(),
    strip.background = ggplot2::element_rect(fill="white"),
    strip.text = ggplot2::element_text(size  = 22,  hjust = 0)
  )
}
