library(readxl)
library(magrittr)
Year <- 1997:2021
NDI <- 
  read_excel("./data/GNDI_kr.xlsx",
             range = "B1:Z6",
             col_names = FALSE) %>% 
  t %>%
  `colnames<-`(c("Year", "coorporate1", "coorporate2", "general_govt", "household", "GNDI")) %>%
  data.frame(row.names = 1)
NDI$coorporate <- NDI$coorporate1 + NDI$coorporate2
plot(GNDI / 1000 ~ Year, data = NDI,
     yaxt = "n", ann = FALSE, las = 1,
     type = "b", pch = 17, ylim = c(0, 2200))
lines(household / 1000 ~ Year, data = NDI, type = "b", pch = 17, col = "blue")
lines(coorporate / 1000 ~ Year, data = NDI, type = "b", pch = 17, col = "red")
# lines(coorporate2 / 1000 ~ Year, data = NDI, type = "b", pch = 17, col = "pink")
lines(general_govt / 1000 ~ Year, data = NDI, type = "b", pch = 17, col = "green")
axis(side = 1, at = seq(2000, 2020, by = 5), labels = seq(2000, 2020, by = 5))
axis(side = 2, 
     at = seq(0, 2000, by = 500), 
     labels = format(seq(0, 2000, by = 500), big.mark = ","),
     las = 1)
legend("topleft", inset = 0.05, 
       legend = c("국민총처분소득", "가계 및 비영리단체", "일반정부", "법인"),
       pch = 17, col = c("black", "blue", "green", "red"))
mtext("(조 원)", side = 2, at = 2250, las = 1)
text(x = 2021, 
     y = NDI[25, c("GNDI", "household", "coorporate", "general_govt")] / 1000, 
     labels = format(NDI[25, c("GNDI", "household", "coorporate", "general_govt")] / 1000, 
                     digits = 4, big.mark = ","),
     pos = c(3, 3, 1, 3))
title(main = "국민총처분가능소득의 분배", cex.main = 1.8)