#We choose three assets for further analysis 
portfolio4 <- DaneOG[c(2, 3, 4, 5)]
#Variables for all calculations
gold <- portfolio4$GOLD
pzu <- portfolio4$PZU
eur <- portfolio4$EUR
etf <- portfolio4$ETF
#importing weights from file
w1 <- weights4inv$W1
w1 <- as.numeric(w1)
w2 <- weights4inv$W2
w2 <- as.numeric(w2)
w3 <- weights4inv$W3
w3 <- as.numeric(w3)
w4 <- weights4inv$W4
w4 <- as.numeric(w4)
#calculating SD
s1 <- sd(gold)
s2 <- sd(pzu)
s3 <- sd(eur)
s4 <- sd(etf)
#Calculating corellation
corr12 <- cor(gold,pzu)
corr13 <- cor(gold,eur)
corr14 <- cor(gold, etf)
corr23 <- cor(pzu, eur)
corr24 <- cor(pzu, etf)
corr34 <- cor(eur, etf)
#calculating ip
iportfolio <- mean(gold)*w1+mean(pzu)*w2+mean(eur)*w3+mean(etf)*w4
#portfolio risk
sdp <- ((w1^2*s1^2 + w2^2*s2^2 + w3^2*s3^2 + w4^2*s4^2 + 2*w1*w2*s1*s2*corr12 + 2*w1*w3*s1*s3*corr13 + 2*w1*w4*s1*s4*corr14 + 
           2*w2*w3*s2*s3*corr23 + 2*w2*w4*s2*s4*corr24 + 2*w3*w4*s3*s4*corr34)^0.5)
#calculating effectivness
rf <- 0.01*mean(pzu) # zmienić na 0.01 razy średnie PZU
sharp <- (iportfolio-rf)/sdp
#preparing df with results
data <- cbind(w1, w2, w3, w4, iportfolio, sdp, sharp)
data <- as.data.frame(data)
#finding interesting portfolios
min.risk <- subset(data, data$sdp==min(data$sdp))
max.effectivness <- subset(data, data$sharp==max(data$sharp))
max.ip <- subset(data, data$iportfolio==max(data$iportfolio))
max.w1 <- subset(data, data$w1==1)
max.w2 <- subset(data, data$w2==1)
max.w3 <- subset(data, data$w3==1)
max.w4 <- subset(data, data$w4==1)
des <- c("Minimal risk portfolio", "Maximum efficiency portfolio", "Maximum rate of return portfolio", "Max weight one portfolio", "Max weight two portfolio", "Max weight three portfolio", "Max weight four portfolio")
#Creating table with results 3 portfolios and showing results in console
results <- cbind(rbind(min.risk, max.effectivness, max.ip, max.w1, max.w2, max.w3, max.w4), des)
results
write.csv(x=results, file = "results.csv", row.names=FALSE)
#creating and saving OS
# Zakresy osi X i Y z dodatkowym marginesem
x_range <- c(0, max(sdp) * 1.2)  # Zakres dla osi X z dodatkową przestrzenią
y_range <- c(0, max(iportfolio) * 1.2)  # Zakres dla osi Y z dodatkową przestrzenią

# Tworzenie Opportunity Set i ustawienie zakresu osi
plot(sdp, iportfolio, type = "p", col = "red", pch = 16, cex = 1.5,
     main = "Opportunity set for four risky assets without SS",
     xlab = "Ryzyko portfela (sdp)", ylab = "Stopa zwrotu portfela (iportfolio)", 
     cex.lab = 1.2, cex.main = 1.4, xlim = x_range, ylim = y_range)

# Dodanie kratki (grid) dla lepszej widoczności
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")

# Dodanie różnych punktów oraz zwiększenie ich rozmiarów
points(min.risk$sdp, min.risk$iportfolio, pch = 19, col = "green", cex = 2)  # Minimalne ryzyko
points(max.effectivness$sdp, max.effectivness$iportfolio, pch = 19, col = "blue", cex = 2)  # Maksymalna efektywność
points(max.ip$sdp, max.ip$iportfolio, pch = 19, col = "yellow", cex = 2)  # Maksymalny zwrot
points(max.w1$sdp, max.w1$iportfolio, pch = 19, col = "black", cex = 1.5)  # Waga 1
points(max.w2$sdp, max.w2$iportfolio, pch = 19, col = "black", cex = 1.5)  # Waga 2
points(max.w3$sdp, max.w3$iportfolio, pch = 19, col = "black", cex = 1.5)  # Waga 3
points(max.w4$sdp, max.w4$iportfolio, pch = 19, col = "black", cex = 1.5)  # Waga 4

# Dodanie punktu oznaczającego minimalne ryzyko (0.01) na wykresie
points(0.01, min.risk$iportfolio, pch = 19, col = "purple", cex = 2)
text(0.01, min.risk$iportfolio, labels = "Minimalne ryzyko (0.01)", pos = 3, col = "purple")

# Dodanie legendy z większą czcionką i rozmiarami punktów
legend("right", legend = c("Opportunity set without SS", "Minimum risk portfolio", 
                           "Maximum efficiency portfolio", "Maximum RoR portfolio", 
                           "One-element portfolio", "Minimalne ryzyko (0.01)"), 
       pch = c(19, 19, 19, 19, 19, 19), 
       col = c("red", "green", "blue", "yellow", "black", "purple"), 
       pt.cex = 1.5,  # Rozmiar punktów w legendzie
       cex = 1.2)     # Wielkość tekstu w legendzie

# Zapisanie wykresu jako pliku PNG
dev.copy(png, filename = "plot_with_min_risk_point.png")
dev.off()

