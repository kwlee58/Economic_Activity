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
L <- length(Year)
NDI$coorporate <- NDI$coorporate1 + NDI$coorporate2
plot(GNDI / 1000 ~ Year, data = NDI,
     yaxt = "n", ann = FALSE, las = 1,
     type = "b", pch = 17, ylim = c(0, 2200))
lines(household / 1000 ~ Year, data = NDI, type = "b", pch = 17, col = "blue")
lines(coorporate / 1000 ~ Year, data = NDI, type = "b", pch = 17, col = "red")
# lines(coorporate2 / 1000 ~ Year, data = NDI, type = "b", pch = 17, col = "pink")
lines(general_govt / 1000 ~ Year, data = NDI, type = "b", pch = 17, col = "green")
axis(side = 1, at = c(1997, seq(2000, 2020, by = 5)), labels = c(1997, seq(2000, 2020, by = 5)))
axis(side = 2, 
     at = seq(0, 2000, by = 500), 
     labels = format(seq(0, 2000, by = 500), big.mark = ","),
     las = 1)
legend("topleft", inset = 0.05, 
       legend = c("국민총처분소득", "가계 및 비영리단체", "일반정부", "법인(금융법인 포함)"),
       pch = 17, col = c("black", "blue", "green", "red"))
mtext("(조 원)", side = 2, at = 2250, las = 1)
text(x = Year[length(Year)], 
     y = NDI[length(Year), c("GNDI", "household", "coorporate", "general_govt")] / 1000, 
     labels = format(NDI[length(Year), c("GNDI", "household", "coorporate", "general_govt")] / 1000, 
                     digits = 2, big.mark = ",", nsmall = 0),
     pos = c(3, 3, 1, 3))
text(x = Year[1], 
    y = NDI[1, c("GNDI", "household", "coorporate", "general_govt")] / 1000, 
    labels = format(NDI[1, c("GNDI", "household", "coorporate", "general_govt")] / 1000, 
                    digits = 2, big.mark = ",", nsmall = 0),
    pos = c(3, 3, 1, 3))
title(main = "국민총처분가능소득의 분배", cex.main = 1.8)
dev.copy(png, file = "./pics/GNDI_kr.png", width = 720, height = 480)
dev.off()
plot(general_govt / GNDI * 100 ~ Year, data = NDI,
     xlab = "연도", ylab = "제도부문 비중(누적, %)",
     las = 1,
     type = "n", pch = 17, ylim = c(0, 100))
# lines((general_govt + household) / GNDI * 100 ~ Year, data = NDI, type = "b", pch = 17, col = "blue")
axis(side = 1, at = c(1997, seq(2000, 2020, by = 5)), labels = c(1997, seq(2000, 2020, by = 5)))
polygon(x = Year[c(1:L, L, 1)], 
        y = c(NDI$general_govt / NDI$GNDI * 100, 0, 0),
        col = NULL,
        lwd = 3,
        border = "green")
polygon(x = Year[c(1:L, L:1)], 
        y = c((NDI$general_govt + NDI$household) / NDI$GNDI * 100, 
              rev(NDI$general_govt / NDI$GNDI * 100)),
        col = NULL,
        lwd = 3,
        border = "blue")
polygon(x = Year[c(1, L, L:1)], 
        y = c(100, 100, rev((NDI$general_govt + NDI$household) / NDI$GNDI * 100)), 
        col = NULL,
        lwd = 3,
        border = "red")
pos <- function(x)
{cumsum(x) - x / 2}
y_text <- apply(NDI[c(1, L), c("general_govt", "household", "coorporate")] / NDI$GNDI[c(1, L)] * 100, 
                MARGIN = 1, FUN = pos)
text(x = rep(c(1997, 2021), each = 3), y = y_text,
     labels = apply(NDI[c(1, L), c("general_govt", "household", "coorporate")] / NDI$GNDI[c(1, L)] * 100, 
                    MARGIN = 1, FUN = function(x) paste0(format(x, digits = 3, nsmall = 1), "%")), pos = rep(c(4, 2), each = 3))
y2_text <- apply(NDI[Year == "2010", c("general_govt", "household", "coorporate")] / NDI$GNDI[Year == "2010"] * 100,
                 MARGIN = 1, FUN = pos)
text(x = 2010, y = y2_text, labels = c("일반정부", "가계 및 비영리단체", "법인(금융법인 포함)"))
title(main = "국민총처분가능소득의 분배", cex.main = 1.8)
dev.copy(png, file = "./pics/GNDI_proportions_kr.png", width = 640, height = 480)
dev.off()