tBE <- function (base_size = 12, base_family = "Helvetica", axiscolor='grey50',...){
  
  theme_classic (base_size = base_size, base_family = base_family)+
    theme (
      line =               element_line(colour = axiscolor),
      text =               element_text(family = base_family, face = "plain",
                                        colour = "black", size = base_size,
                                        hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9),
      
      
      
      axis.ticks =         element_line(colour = axiscolor, size = 0.2),
      axis.line =          element_line(colour = axiscolor, size = 0.2),
      axis.text =          element_text(colour = axiscolor),
      
      plot.title =         element_text(size =base_size*1.2)
      
      
    )}
