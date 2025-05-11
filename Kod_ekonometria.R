# ==========================================================
# Model regresji sprzedaży lokali mieszkalnych (Podkarpacie)
# Autor: Mateusz Rzeźnikiewicz
# Dane: dane.txt | Język: R
# ==========================================================

# Część I - wczytanie danych
dane <- read.table("sciezka/do/pliku/dane.txt", 
                   header = TRUE,
                   sep = "\t",
                   dec = ",",
                   stringsAsFactors = FALSE,
                   fileEncoding = "Windows-1250",
                   fill = TRUE)

dane <- subset(dane, select = -Lp)

kolumny_do_konwersji <- c("Y", "X2", "X3", "X5", "X8")
for (kol in kolumny_do_konwersji) {
  dane[[kol]] <- as.numeric(gsub(",", ".", gsub(" ", "", dane[[kol]])))
}

# Część II - wykresy
pozycje_lat <- seq(1, 40, by = 4)
etykiety_lat <- dane$Rok[pozycje_lat]

# X1
plot(dane$X1, type = "l", col = "steelblue", lwd = 2,
     xaxt = "n", xlab = "Rok", ylab = "Stopa bezrobocia rejestrowanego",
     main = "Wykres zmian wartości X1 w czasie",
     cex.lab = 1.1, cex.main = 1.3, font.lab = 2)
axis(1, at = pozycje_lat, labels = etykiety_lat, las = 2, cex.axis = 0.9)
grid(nx = NA, ny = NULL, col = "gray80", lty = "dotted")
abline(v = pozycje_lat, col = "gray80", lty = "dotted")

# X2
plot(dane$X2, type = "l", col = "coral3", lwd = 2,
     xaxt = "n", xlab = "Rok", ylab = "Mieszkania oddane do użytkowania",
     main = "Wykres zmian wartości X2 w czasie",
     cex.lab = 1.1, cex.main = 1.3, font.lab = 2)
axis(1, at = pozycje_lat, labels = etykiety_lat, las = 2, cex.axis = 0.9)
grid(nx = NA, ny = NULL, col = "gray80", lty = "dotted")
abline(v = pozycje_lat, col = "gray80", lty = "dotted")

# X3
plot(dane$X3, type = "l", col = "bisque3", lwd = 2,
     xaxt = "n", xlab = "Rok", ylab = "Średnia cena za metr kwadratowy",
     main = "Wykres zmian wartości X3 w czasie",
     cex.lab = 1.1, cex.main = 1.3, font.lab = 2)
axis(1, at = pozycje_lat, labels = etykiety_lat, las = 2, cex.axis = 0.9)
grid(nx = NA, ny = NULL, col = "gray80", lty = "dotted")
abline(v = pozycje_lat, col = "gray80", lty = "dotted")

# X4
plot(dane$X4, type = "l", col = "chocolate3", lwd = 2,
     xaxt = "n", xlab = "Rok", ylab = "Inflacja",
     main = "Wykres zmian wartości X4 w czasie",
     cex.lab = 1.1, cex.main = 1.3, font.lab = 2)
axis(1, at = pozycje_lat, labels = etykiety_lat, las = 2, cex.axis = 0.9)
grid(nx = NA, ny = NULL, col = "gray80", lty = "dotted")
abline(v = pozycje_lat, col = "gray80", lty = "dotted")

# X5
plot(dane$X5, type = "l", col = "cadetblue", lwd = 2,
     xaxt = "n", xlab = "Rok", ylab = "Przeciętne miesięczne wynagrodzenie brutto",
     main = "Wykres zmian wartości X5 w czasie",
     cex.lab = 1.1, cex.main = 1.3, font.lab = 2)
axis(1, at = pozycje_lat, labels = etykiety_lat, las = 2, cex.axis = 0.9)
grid(nx = NA, ny = NULL, col = "gray80", lty = "dotted")
abline(v = pozycje_lat, col = "gray80", lty = "dotted")

# X6
plot(dane$X6, type = "l", col = "brown2", lwd = 2,
     xaxt = "n", xlab = "Rok", ylab = "Aktywność ekonomiczna",
     main = "Wykres zmian wartości X6 w czasie",
     cex.lab = 1.1, cex.main = 1.3, font.lab = 2)
axis(1, at = pozycje_lat, labels = etykiety_lat, las = 2, cex.axis = 0.9)
grid(nx = NA, ny = NULL, col = "gray80", lty = "dotted")
abline(v = pozycje_lat, col = "gray80", lty = "dotted")

# X7
plot(dane$X7, type = "l", col = "orange", lwd = 2,
     xaxt = "n", xlab = "Rok", ylab = "Liczba rozwodów",
     main = "Wykres zmian wartości X7 w czasie",
     cex.lab = 1.1, cex.main = 1.3, font.lab = 2)
axis(1, at = pozycje_lat, labels = etykiety_lat, las = 2, cex.axis = 0.9)
grid(nx = NA, ny = NULL, col = "gray80", lty = "dotted")
abline(v = pozycje_lat, col = "gray80", lty = "dotted")

# X8
plot(dane$X8, type = "l", col = "chartreuse3", lwd = 2,
     xaxt = "n", xlab = "Rok", ylab = "Liczba urodzeń",
     main = "Wykres zmian wartości X8 w czasie",
     cex.lab = 1.1, cex.main = 1.3, font.lab = 2)
axis(1, at = pozycje_lat, labels = etykiety_lat, las = 2, cex.axis = 0.9)
grid(nx = NA, ny = NULL, col = "gray80", lty = "dotted")
abline(v = pozycje_lat, col = "gray80", lty = "dotted")

# Y
plot(dane$Y, type = "l", col = "coral", lwd = 2,
     xaxt = "n", xlab = "Rok", ylab = "Liczba sprzedanych lokali mieszkalnych",
     main = "Wykres zmian wartości Y w czasie",
     cex.lab = 1.1, cex.main = 1.3, font.lab = 2)

axis(1, at = pozycje_lat, labels = etykiety_lat, las = 2, cex.axis = 0.9)
grid(nx = NA, ny = NULL, col = "gray80", lty = "dotted")
abline(v = pozycje_lat, col = "gray80", lty = "dotted")


# Część III - metoda Hellwiga
r <- rbind(
  c(1, -0.500508711125166, -0.64135225731249, -0.656143319355219, 0.0735007347795687),
  c(-0.500508711125166, 1, 0.484575912194174, 0.502395169959821, 0.061296600035604),
  c(-0.641352257312492, 0.484575912194174, 1, 0.966298617781545, -0.0840700162214095),
  c(-0.656143319355219, 0.502395169959821, 0.966298617781545, 1, -0.0592133427121493),
  c(0.0735007347795687, 0.061296600035604, -0.0840700162214095, -0.0592133427121493, 1)
)

R <- c(-0.742611736976987, 0.609278732981079, 0.811966123374464, 0.749589313626393, 0.0512552456461393)
N <- length(R)
M <- 2^N - 1
r <- as.matrix(abs(r))
tab <- as.matrix(expand.grid(rep(list(0:1), N)))[-1,]
wyniki <- matrix(0, M, N)

for (i in 1:M) {
  for (j in 1:N) {
    if (tab[i, j] != 0) {
      wyniki[i, j] <- (R[j]^2) / (tab[i, ] %*% as.vector(r[, j]))
    }
  }
}

maks <- which.max(rowSums(wyniki))
najlepsza_kombinacja <- tab[maks, ]
cat("Najlepsza kombinacja zmiennych (1 - wybrana, 0 - pominięta):\n")
print(najlepsza_kombinacja)

wynikiS <- cbind(wyniki, 0)
wynikiS[, N + 1] <- rowSums(wyniki)
colnames(wynikiS) <- c("X1", "X2", "X3", "X5", "X7", "H")

ind <- order(wynikiS[, N + 1], decreasing = TRUE)[1:31]
wszystkie <- wynikiS[ind, ]
cat("\nWszystkie kombinacje:\n")
print(wszystkie)

# Część IV - obliczanie wartości statystyk testowych
t_krytyczne1 <- qt(1 - 0.05/2, df = 37)
print(t_krytyczne1)
F_krytyczne <- qf(1 - 0.05, df1 = 2, df2 = 37)
print(F_krytyczne)
t_krytyczne2 <- qt(1- 0.05/2, df = 39)
print(t_krytyczne2)

# Część V - test Walda-Wolfowitza
wald_wolfowitz_przedzial <- function(liczba_dodatnich, liczba_ujemnych, alfa) {
  n_dodatnie <- liczba_dodatnich
  n_ujemne <- liczba_ujemnych
  oczekiwana_R <- ((2 * n_dodatnie * n_ujemne) / (n_dodatnie + n_ujemne)) + 1
  wariancja_R <- (2 * n_dodatnie * n_ujemne * (2 * n_dodatnie * n_ujemne - n_dodatnie - n_ujemne)) /
    ((n_dodatnie + n_ujemne)^2 * (n_dodatnie + n_ujemne - 1))
  z_alfa_polowa <- qnorm(1 - alfa / 2)
  dolny_prog <- floor(oczekiwana_R - z_alfa_polowa * sqrt(wariancja_R))
  gorny_prog <- ceiling(oczekiwana_R + z_alfa_polowa * sqrt(wariancja_R))
  list(
    oczekiwana_liczba_serii = oczekiwana_R,
    wariancja_liczby_serii = wariancja_R,
    dolny_przedzial_krytyczny = dolny_prog,
    gorny_przedzial_krytyczny = gorny_prog,
    z_wartosc = z_alfa_polowa
  )
}
wald_wolfowitz_przedzial(21, 19, 0.05)

# Część VI - budowa modelu regresji i testy
model <- lm(Y ~ X1 + X3, data = dane)
reszty <- residuals(model)
shapiro.test(reszty)

install.packages("lmtest")  # tylko przy pierwszym użyciu
library(lmtest)
bp_test <- bptest(model)
print(bp_test)
dw_test <- dwtest(model)
print(dw_test)

# Część VII - wykres dla prognozowania
Y_true <- c(667, 624, 739, 654, 818, 1026, 864, 939, 826, 813, 589, 618,
            1024, 689, 795, 871, 935, 1135, 1572, 1433, 1594, 1437, 1649, 2236,
            1537, 1066, 1935, 1694, 1931, 1737, 1788, 2139, 1787, 1464, 1873,
            1768, 1648, 1321, 1471, 1895)

Y_hat <- c(515, 639, 715, 697, 663, 884, 865, 934, 792, 881, 824, 881,
           979, 1020, 1049, 1107, 1123, 1261, 1320, 1330, 1379, 1444, 1489,
           1564, 1487, 1452, 1546, 1549, 1610, 1706, 1690, 1647, 1598,
           1643, 1809, 1811, 1824, 1811, 2007, 2057)

lata <- rep(2014:2023, each = 4)
pozycje_lat <- seq(1, length(Y_true), by = 4)
etykiety_lat <- lata[pozycje_lat]

plot(Y_true, type = "l", col = "steelblue", lwd = 2,
     xaxt = "n", xlab = "Rok", ylab = "Liczba lokali mieszkalnych",
     main = "Porównanie wartości rzeczywistych i przewidywanych w czasie",
     cex.lab = 1.1, cex.main = 1.3, font.lab = 2)
lines(Y_hat, type = "l", col = "tomato", lwd = 2)
axis(1, at = pozycje_lat, labels = etykiety_lat, las = 2, cex.axis = 0.9)
grid(nx = NA, ny = NULL, col = "gray80", lty = "dotted")
abline(v = pozycje_lat, col = "gray80", lty = "dotted")
legend("topleft", legend = c("Rzeczywiste Y", "Przewidywane Y"),
       col = c("steelblue", "tomato"), lwd = 2, cex = 0.9)

