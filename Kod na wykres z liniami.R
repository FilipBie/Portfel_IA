# Współrzędne punktu minimalnego ryzyka
x1 <- 0.00  # Punkt minimalnego ryzyka na osi X
y1 <- min.risk$iportfolio  # Wartość portfela dla minimalnego ryzyka

# Ustawienie zakresu osi X i Y
x_range <- c(0, max(sdp) * 1.5)  # Zakres dla osi X z dodatkową przestrzenią
y_range <- c(0, max(iportfolio) * 1.5 )  # Zakres dla osi Y z dodatkową przestrzenią

# Tworzenie wykresu Opportunity Set
plot(sdp, iportfolio, type = "p", col = "red", pch = 16, cex = 1,
     main = "Opportunity set with horizontal and inclined lines",
     xlab = "Ryzyko portfela (sdp)", ylab = "Stopa zwrotu portfela (iportfolio)", 
     cex.lab = 1.2, cex.main = 1.4, xlim = x_range, ylim = y_range)

# Dodanie kratki (grid) dla lepszej widoczności
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")

# Dodanie punktów do wykresu
points(min.risk$sdp, min.risk$iportfolio, pch = 19, col = "green", cex = 1.5)  # Minimalne ryzyko

# Kąt nachylenia dla drugiej linii (np. pod kątem 0.2594259 rad)
angle <- 0.2594259
m <- tan(angle)

# Rysowanie pierwszej linii poziomej (kąt 0, pozioma linia)
lines(c(x_range[1], x_range[2]), c(y1, y1), col = "black", lty = 2)  # Pozioma linia

# Obliczanie współrzędnych y dla drugiej linii (nachylonej)
y_vals <- y1 + m * (x_range[1] - x1)  # Wartość y w punkcie x_range[1]
y_vals2 <- y1 + m * (x_range[2] - x1)  # Wartość y w punkcie x_range[2]

# Rysowanie drugiej linii nachylonej
lines(c(x_range[1], x_range[2]), c(y_vals, y_vals2), col = "black", lty = 2)  # Nachylona linia

# Dodanie innych punktów (np. maximalna efektywność, maximal return, etc.)
points(max.effectivness$sdp, max.effectivness$iportfolio, pch = 19, col = "blue", cex = 1.5)  # Maksymalna efektywność
points(max.ip$sdp, max.ip$iportfolio, pch = 19, col = "yellow", cex = 2)  # Maksymalny zwrot
points(max.w1$sdp, max.w1$iportfolio, pch = 19, col = "black", cex = 1)  # Waga 1
points(max.w2$sdp, max.w2$iportfolio, pch = 19, col = "black", cex = 1)  # Waga 2
points(max.w3$sdp, max.w3$iportfolio, pch = 19, col = "black", cex = 1)  # Waga 3
points(max.w4$sdp, max.w4$iportfolio, pch = 19, col = "black", cex = 1)  # Waga 4

# Dodanie punktu oznaczającego minimalne ryzyko (0.01) na wykresie
points(0.00, min.risk$iportfolio, pch = 19, col = "purple", cex = 1)

legend ("topright", legend = c("Opportunity set without SS", "Minimum risk portfolio", 
                               "Maximum efficiency portfolio", "Maximum RoR portfolio", 
                               "One-element portfolio", "Minimalne ryzyko (0.01)"), 
        pch = c(19, 19, 19, 19, 19, 19), 
        col = c("red", "green", "blue", "yellow", "black", "purple"), 
        pt.cex = 1,  # Rozmiar punktów w legendzie
        cex = 1)     # Wielkość tekstu w legendzie


