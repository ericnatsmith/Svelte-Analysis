# ggplot themes -----------------------------------------------------------
## Many thanks to Dave Paunesku for offering the initial ggplot themes that inspired these.

## report bar theme --------------------------------------------------------
report_bar_theme <- theme(
  panel.background    = element_blank() ,              #   control the major gridlines
  panel.grid.major.y  = element_line() ,               #   suppress the vertical grid lines
  panel.grid.major.x  = element_blank() ,              #   suppress the minor grid lines
  panel.grid.minor    = element_blank() ,              #   add axes
  axis.text           = element_text( size=8, family="Helvetica", colour='black'),
  axis.line           = element_line( size=.8),
  axis.ticks          = element_line( size=.8, colour='black'),
  axis.title.y        = element_text( angle=90, vjust= 1.5, hjust=.47,
                                      size=8, family="Helvetica"),
  axis.title.x        = element_text( vjust=-.5, hjust = .52,
                                      size=8, family="Helvetica"),
  plot.title          = element_text(family = "Helvetica", color="black",
                                     # face="bold",
                                     size=8
                                     # hjust=0
                                     ),
  plot.margin = unit( c(.125, 0, .125, .125),  # sets the margin around the plot c(T,R,B,L)
                      "in"),  # unit of measure for margins "in", "cm", or "lines"
  legend.text = element_text(size=8, family="Helvetica"),
  legend.title = element_text(size=8, family="Helvetica", face = 'bold'), # default: face = 'bold'; alt: 'plain'
  legend.position=("none")
  ) # format titles http://www.sharpsightlabs.com/format-titles-and-axes-in-ggplot2/

## slide_bar_theme  --------------------------------------------------------
slide_bar_theme <- theme(
  panel.background    = element_blank() ,                             #   control the major gridlines
  panel.grid.major.y  = element_line() ,                              #   suppress the vertical grid lines
  panel.grid.major.x  = element_blank() ,                             #   suppress the minor grid lines
  panel.grid.minor    = element_blank() ,                             #   add axes
  axis.text           = element_text( size=30, family= "Helvetica", colour='black'),   #   tick labels' size, font & color.
  axis.line           = element_line( size=.8),   #   adjust the axis ticks
  axis.ticks          = element_line( size=.8, colour='black'),   #   axis colors and thickness
  axis.title.y        = element_text( angle=90, vjust= 1.5, hjust=.47,
                                      size=30, family= "Helvetica"),
  axis.title.x        = element_text( vjust=-.5, hjust = .52,
                                      size=25, family= "Helvetica"),
  plot.title          = element_text(family = "Helvetica", color="black", face="plain", size=30, hjust=.5),
  plot.margin = unit( c(.125, 2, .250, .125) , "in"),  # sets the margin around the facet panels, "in", "cm", or "lines"; c(T,R,B,L)
  legend.text = element_text(size=25, family= "Helvetica"),
  # change face to 'plain' to override default of 'bold'
  legend.title = element_text(size=25, family= "Helvetica", face = 'bold'),
  # APS prefers legend in top right unless impractical, as in the case of Density where I located the legend all the way to the right and bottom.
  legend.position=c(1.1, # 0 = far left, 1 = far right
                    0.25) # legend.position=("none") # to suppress legend
)       # format titles http://www.sharpsightlabs.com/format-titles-and-axes-in-ggplot2/

## report line theme --------------------------------------------------------
report_line_theme <- theme(
  panel.background    = element_blank() ,                             #   control the major gridlines
  panel.grid.major.y  = element_line() ,                              #   suppress the vertical grid lines
  panel.grid.major.x  = element_blank() ,                             #   suppress the minor grid lines
  panel.grid.minor    = element_blank() ,                             #   add axes
  axis.text           = element_text( size=8, family="Helvetica", colour='black'),
  axis.line           = element_line( size=.8),
  axis.ticks          = element_line( size=.8, colour='black'),
  axis.title.y        = element_text( angle=90, vjust= 1.5, hjust=.47,
                                      size=8, family="Helvetica"),
  axis.title.x        = element_text( vjust=-.5, hjust = .52,
                                      size=8, family="Helvetica"),
  plot.title          = element_text(family = "Helvetica", color="black",
                                     # face="bold",
                                     size=8
                                     #hjust=0
  ),
  plot.margin = unit( c(.125, 1, .125, .125) , "in"),  # sets the margin around the facet panels, "in", "cm", or "lines"; c(T,R,B,L)
  legend.text = element_text(size=8, family="Helvetica"),
  # change face to 'plain' to override default of 'bold'
  legend.title = element_text(size=8, family="Helvetica", face = 'bold'),
  # APS prefers legend in top right unless impractical, as in the case of Density where I located the legend all the way to the right and bottom.
  legend.position=c(1.17,0.5)
)

# format titles http://www.sharpsightlabs.com/format-titles-and-axes-in-ggplot2/

## PsychSci_theme ----------------------------------------------------------

PsychSci_theme <- theme(
  panel.background    = element_blank() ,                             #   control the major gridlines
  panel.grid.major.y  = element_line() ,                              #   suppress the vertical grid lines
  panel.grid.major.x  = element_blank() ,                             #   suppress the minor grid lines
  panel.grid.minor    = element_blank() ,                             #   add axes
  axis.text           = element_text( size=8, family="Helvetica", colour='black'),   #   tick labels' size, font & color.
  axis.line           = element_line( size=.8),   #   adjust the axis ticks
  axis.ticks          = element_line( size=.8, colour='black'),   #   axis colors and thickness
  axis.title.y        = element_text( angle=90, vjust= 1.5, hjust=.47,
                                      size=8, family="Helvetica"),
  axis.title.x        = element_text( vjust=-.5, hjust = .52,
                                      size=8, family="Helvetica"),
  plot.title          = element_text(family="Helvetica", face="bold", size=8),
  plot.margin = unit( c(.125,1,.125,.125) , "in"),  # sets the margin around the facet panels, "in", "cm", or "lines"; c(T,R,B,L)
  legend.text = element_text(size=8, family="Helvetica"),
  # change face to 'plain' to override default of 'bold'
  legend.title = element_text(size=8, family="Helvetica", face = 'plain'),
  # APS prefers legend in top right unless impractical, as in the case of Density where I located the legend all the way to the right and bottom.
  legend.position=c(1.17,0.5)
  )
  # Be sure that later additions maintain font sizes of
      # size 9 for axis-ticks, legend, and annotations
      # size 10 for axis titles

########################################################################
########################################################################