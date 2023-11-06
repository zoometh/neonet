# c14 <- data.frame(LabID = c("date1", "date2"),
#                   period = c("Late Mesolithic", "Early Neolithic"),
#                   CRA = c(4000, 3800),
#                   Error = c(100, 80)
# )
# x <- rcarbon::calibrate(c14$CRA, c14$Error, ids = c14$LabID)
# rcarbon::multiplot(x)

library(ggplot2)
library(oxcAAR)
quickSetupOxcal()


knitr::opts_chunk$set(cache=TRUE)
my_oxcal_code <- ' Plot()
 {
  Sequence("Sequence1")
  {
   Boundary("Beginn");
   Phase("Phase1")
   {
    R_Date("Lab-1",5000,25);
    R_Date("Lab-2",4900,37);
   };
   Boundary("Between");
   Phase("Phase2")
   {
    R_Date("Lab-3",4800,43);
   };
   Boundary("End");
  };
 };'
my_result_file <- executeOxcalScript(my_oxcal_code)
my_result_text <- readOxcalOutput(my_result_file)