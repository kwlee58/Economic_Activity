library(extrafont)
par(family = "KoPubWorldDotum Medium")
load("./R/econ_activity.rda")
Year <- 1963:2021
Total <- c(employment.rate$Total, 60.5, 60.6, 60.8, 60.7, 60.9, 60.1, 60.5)
Men <- c(employment.rate$Men, 71.4, 71.2, 71.2, 70.8, 70.7, 69.8, 70.0)
Women <- c(employment.rate$Women, 50.1, 50.3, 50.8, 50.9, 51.6, 50.7, 51.7)
plot(x = Year, y = Total, pch = 17, type = "b", ylim = c(20, 80), ann = F)
lines(x = Year, y = Women, pch = 17, type = "b", col = "blue")
lines(x = Year, y = Men, pch = 17, type = "b", col = "red")
text(x = 1998, y = 75, labels = "1998", pos = 3)
text(x = 1963, y = c(52.0, 71.6, 34.3), 
     labels = paste0(format(c(52.0, 71.6, 34.3), digits = 3, nsmall = 1), "%"), pos = 3)
text(x = 2021, y = c(60.5, 70.0, 51.7), 
     labels = paste0(format(c(60.5, 70.0, 51.7), digits = 3, nsmall = 1), "%"), pos = 3)
title(main = "남녀 고용률의 변화", xlab = "연도", ylab = "고용률(%)", cex.main = 1.5)
legend("bottomright", inset = 0.05, pch = 17, col = c("black", "blue", "red"), legend = c("전체", "남자", "여자"))
dev.copy(device = png, file = "./pics/Employment_kor.png", width = 960, height = 540)
dev.off()