library(DiagrammeR)
library(magick)

image_path <- "https://raw.githubusercontent.com/zoometh/thomashuet/main/img/r-pkg-c14bazaar.png"
image_path <- "C:/Rprojects/thomashuet/img/r-pkg-c14bazaar.png"

formatted_image_path <- gsub("\\\\", "/", image_path)

# grViz graph string with an image
graph_string <- sprintf("
digraph flowchart {
  node [shape=box]
  A [label='Start']
  
  B [label=< 
    <TABLE BORDER='0' CELLBORDER='0' CELLSPACING='0'>
      <TR><TD><IMG SRC='file:///%s' /></TD></TR>
      <TR><TD>Step with Image</TD></TR>
    </TABLE>
    >, shape=plaintext]
    
  C [label='End']
  
  A -> B
  B -> C
}", formatted_image_path)

# Render the graph
DiagrammeR::grViz(graph_string)


