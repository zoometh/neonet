c14 <- data.frame(LabID = c("date1", "date2"),
                  period = c("Late Mesolithic", "Early Neolithic"),
                  CRA = c(4000, 3800),
                  Error = c(100, 80)
)
x <- rcarbon::calibrate(c14$CRA, c14$Error, ids = c14$LabID)
rcarbon::multiplot(x)