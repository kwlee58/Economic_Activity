library(readxl)
library(magrittr)
library(extrafont)
Year <- c(2002, 2004, 2006, 2007, 2008, 2009, 2010, 2011, 2013, 2015, 2017, 2019, 2021)
Reading <- 
  read_excel("./data/Reading_2021.xlsx",
             range = "C3:O25",
             na = "-",
             col_names = FALSE) %>% 
  t %>%
  `colnames<-`(c("Total", "Male", "Female", 
                 "Under_20", "Age19_29", "Age30_39", "Age40_49", "Age50_59", "Over50", "Over60",
                 "Middle_School", "High_School", "Above_College", 
                 "W100", "W101_200", "W200", "W200_300", "W200_500", "W300_400", "W301", "W400_500", "W401", "W500")) %>%
  data.frame
# class(Reading[, -1]) <- "numeric"
str(Reading)
L <- length(Year)
par(family = "KoPubWorldDotum Medium")
## 전체 독서율
plot(x = Year, y = Reading$Total,
     xaxt = "n", yaxt = "n", ann = FALSE, 
     xlab = "연도", ylab = "종이책독서율", 
     las = 1,
     type = "b", pch = 17, xlim = c(2001, 2022), ylim = c(30, 90))
# lines(x = Year, y = Reading$Male, type = "b", pch = 17, col = "blue")
# lines(x = Year, y = Reading$Female, type = "b", pch = 17, col = "red")
axis(side = 1, at = Year, labels = paste0("`", substr(Year, start = 3, stop = 4)))
axis(side = 2, 
     at = seq(30, 90, by = 10), 
     labels = format(seq(30, 90, by = 10)),
     las = 1)
text(x = Year[length(Year)], 
     y = Reading[length(Year), 1], 
     labels = format(Reading[length(Year), 1], 
                     digits = 2, big.mark = ",", nsmall = 1),
     pos = 4)
text(x = Year[1], 
     y = Reading[1, 1], 
     labels = format(Reading[1, 1], 
                     digits = 2, big.mark = ",", nsmall = 1),
     pos = 2)
title(main = "전체 종이책 독서율", 
      xlab = "연도", ylab = "종이책독서율(%)",
      cex.main = 1.8, 
      family = "KoPubWorldDotum Bold")
dev.copy(png, file = "./pics/Reading_total.png", width = 720, height = 480)
dev.off()
## 성별 독서율
plot(x = Year, y = Reading$Total,
     xaxt = "n", yaxt = "n", ann = FALSE, 
     xlab = "연도", ylab = "종이책독서율", 
     las = 1,
     type = "n", pch = 17, xlim = c(2001, 2022), ylim = c(30, 90))
lines(x = Year, y = Reading$Male, type = "b", pch = 17, col = "blue")
lines(x = Year, y = Reading$Female, type = "b", pch = 17, col = "red")
axis(side = 1, at = Year, labels = paste0("`", substr(Year, start = 3, stop = 4)))
axis(side = 2, 
     at = seq(30, 90, by = 10), 
     labels = format(seq(30, 90, by = 10)),
     las = 1)
legend("topright", inset = 0.05, 
       legend = c("남성", "여성"),
       pch = 17, col = c("blue", "red"))
# mtext("%", side = 2, at = 95, las = 1)
text(x = Year[length(Year)], 
     y = Reading[length(Year), c("Male", "Female")], 
     labels = format(Reading[length(Year), c("Male", "Female")], 
                     digits = 2, big.mark = ",", nsmall = 1),
     pos = c(1, 3))
text(x = Year[1], 
    y = Reading[1, c("Male", "Female")], 
    labels = format(Reading[1, c("Male", "Female")], 
                    digits = 2, big.mark = ",", nsmall = 1),
    pos = c(1, 3))
title(main = "성별 종이책 독서율", 
      xlab = "연도", ylab = "종이책독서율(%)",
      cex.main = 1.8, 
      family = "KoPubWorldDotum Bold")
dev.copy(png, file = "./pics/Reading_gender.png", width = 720, height = 480)
dev.off()
## 연령대별 독서율
plot(x = Year, y = Reading$Under_20,
     xaxt = "n", yaxt = "n", ann = FALSE,
     las = 1,
     type = "b", pch = 17, xlim = c(2001, 2022), ylim = c(0, 100))
lines(x = Year, y = Reading$Age19_29, type = "b", pch = 17, col = "blue")
lines(x = Year, y = Reading$Age30_39, type = "b", pch = 17, col = "red")
lines(x = Year, y = Reading$Age40_49, type = "b", pch = 17, col = "orange")
lines(x = Year, y = Reading$Over50, type = "b", pch = 17, col = "pink")
lines(x = Year, y = Reading$Age50_59, type = "b", pch = 17, col = "grey")
lines(x = Year, y = Reading$Over60, type = "b", pch = 17, col = "darkgrey")
axis(side = 1, at = Year, labels = paste0("`", substr(Year, start = 3, stop = 4)))
axis(side = 2, 
     at = seq(0, 100, by = 10), 
     labels = format(seq(0, 100, by = 10)),
     las = 1)
legend(x = 2005, y = 45, inset = 0.05, 
       legend = c("20대 이하", "19~29세", "30~39세", "40~49세", "50세 이상", "50~59세", "60세 이상"),
       pch = 17, col = c("black", "blue", "red", "orange", "pink", "grey", "darkgrey"))
# mtext("%", side = 2, at = 105, las = 1)
text(x = Year[length(Year)], 
     y = Reading[length(Year), c(5:8, 10)], 
     labels = format(Reading[length(Year), c(5:8, 10)], 
                     digits = 2, big.mark = ",", nsmall = 1),
     pos = 4)
text(x = Year[1], 
     y = Reading[1, c(4, 6, 7, 9)], 
     labels = format(Reading[1, c(4, 6, 7, 9)], 
                     digits = 2, big.mark = ",", nsmall = 1),
     pos = 2)
text(x = Year[6], 
     y = Reading[6, c(8, 10)], 
     labels = format(Reading[6, c(8, 10)], 
                     digits = 2, big.mark = ",", nsmall = 1),
     pos = 3)
title(main = "연령대별 종이책 독서율", 
      xlab = "연도", ylab = "종이책독서율(%)",
      cex.main = 1.8, 
      family = "KoPubWorldDotum Bold")
dev.copy(png, file = "./pics/Reading_age.png", width = 720, height = 480)
dev.off()
# 학력별 독서율
plot(x = Year, y = Reading$Above_College,
     xaxt = "n", yaxt = "n", ann = FALSE, 
     xlab = "연도", ylab = "종이책독서율", 
     las = 1,
     type = "b", pch = 17, xlim = c(2001, 2022), ylim = c(0, 100))
lines(x = Year, y = Reading$Middle_School, type = "b", pch = 17, col = "red")
lines(x = Year, y = Reading$High_School, type = "b", pch = 17, col = "blue")
axis(side = 1, at = Year, labels = paste0("`", substr(Year, start = 3, stop = 4)))
axis(side = 2, 
     at = seq(0, 100, by = 10), 
     labels = format(seq(0, 100, by = 10)),
     las = 1)
legend("topright", inset = 0.05, 
       legend = c("대재 이상", "고졸, 고퇴", "중졸 이하"),
       pch = 17, col = c("black", "blue", "red"))
# mtext("%", side = 2, at = 95, las = 1)
text(x = Year[length(Year)], 
     y = Reading[length(Year), 11:13], 
     labels = format(Reading[length(Year), 11:13], 
                     digits = 2, big.mark = ",", nsmall = 1),
     pos = 4)
text(x = Year[1], 
     y = Reading[1, 11:13], 
     labels = format(Reading[1, 11:13], 
                     digits = 2, big.mark = ",", nsmall = 1),
     pos = 2)
title(main = "학력별 종이책 독서율", 
      xlab = "연도", ylab = "종이책독서율(%)",
      cex.main = 1.8, 
      family = "KoPubWorldDotum Bold")
dev.copy(png, file = "./pics/Reading_education.png", width = 720, height = 480)
dev.off()
