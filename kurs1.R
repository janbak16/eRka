# Import/eksport danych


library(openxlsx)
# tylko xlsx
ludnosc <- read.xlsx("../resources/LUDN_2137_20160225144358.xlsx", sheet = 2)
head(ludnosc)

library(XML)
library(RCurl)
link <- "https://pl.wikipedia.org/wiki/Lista_mecz%C3%B3w_reprezentacji_Polski_w_pi%C5%
82ce_no%C5%BCnej"
xData <- getURL(link)
tabele <- readHTMLTable(xData, stringsAsFactors = FALSE)
length(tabele)

statystyki <- tabele[[1]]
head(statystyki)


x <- rnorm(1000, 180, 5)
plot(ecdf(x), main="Dystrybuanta empiryczna wieku")


# Wykres funkcji matematycznych
f <- .5         # czestotliwosc
phi <- 2        # przesuniecie fazowe
curve(sin(2*pi*f*x + phi), from = -10, to = 10, n = 1000, type = "l")


y <- 
plot(ecdf(mezczyzni$wiek), main="Wiek / plec", pch=21)
# Dodanie kolejnej krzywej
plot(ecdf(kobiety$wiek), add=TRUE, col = "grey")


library(tidyr)
szeroka <- spread(tsdtr210, time, value)
szeroka %>% filter(geo == "PL")

# Wybor zmiennych
dplyr:::select(autaZWiekiem, Wiek.auta, Rok.produkcji)

# Wybor obserwacji (w oparciu o warunki logiczne) - funkcja filter (pakiet dplyr)

# Sortowanie obserwacji po jednej lub wiêkszej liczbie zmiennych Funkcj¹ arrange mozemy wykonaæ 

posortowanePorsche <- arrange(tylkoPorscheZDuzymSilnikiem, Cena.w.PLN)

# Funkcja mutate() pozwala na stworzenie nowej zmiennej (jednej b¹dŸ wielu)
mutate()
Grupowanie

# Funkcja group_by pozwala na operacje na agregatach w grupach opisanych przez zmienn¹ jakoœciow¹.

auta2012 %>%
  filter(Marka == "Volkswagen", Rok.produkcji == 2007) %>%
  group_by(Rodzaj.paliwa) %>%
  summarise(medianaCeny = median(Cena.w.PLN, na.rm=TRUE),
            medianaPrzebieg = median(Przebieg.w.km, na.rm=TRUE),
            liczba = n())

dplyr::summarise()



boxplot(daneSoc$cisnienie.rozk, daneSoc$cisnienie.skur,
        horizontal = TRUE, names = c("Skurczowe","Rozkurczowe"))

###############################
# KMNRL - estymacja KMNK oraz weryfikacja modelu
stats::lm 
(model <- lm(100/mpg ~ disp + hp + wt + am, data = mtcars))
(summod <- summary(fit))

names(model)
names(summod)
methods(class = 'lm')
methods(generic.function = 'summary')

par(mfrow = c(2, 2))
plot(model)

model.frame(model)

qqnorm(y, ylim, main = "Normal Q-Q Plot",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE, datax = FALSE, ...)

qqline(y, datax = FALSE, distribution = qnorm,
       probs = c(0.25, 0.75), qtype = 7, ...)

qqplot(x, y, plot.it = TRUE, xlab = deparse(substitute(x)),
       ylab = deparse(substitute(y)), ...)


# Przedzial ufnosci dla oszacowan parametrow KMNRL
confint(model, level = 0.95)
confint(model, param = "wt", level = 0.95)

# Analiza wariancji modelu / modelow
stats::anova
anova(model)

stats::influence.measures

# Wykrywanie wartosci wplywowych
influence.measures(model)

# rstandard, rstudent, dffits, dfbeta, dfbetas, covratio, cooks.distance, hatvalues, hat

# Ocena wspolliniowosci - VIF (Variance Inflation Factor)
car::vif
vif(model)

perturb::colldiag
library(perturb)
colldiag(model, scale = TRUE, center = FALSE, add.intercept = TRUE)

# Test liniowosci modelu
lmtest::harvtest      # Harvey-Collier Test
library(lmtest)
harvtest(model, order.by = NULL, data = list())

# Testowanie heteroskedastycznosci reszt
lmtest::gqtest        # Test Goldfelda-Quandta
lmtest::bptest        # Test Breuscha-Pagana
lmtest::hmctest       # Test Harrisona-McCabe'a

gqtest(model, point = 0.5, fraction = 0,
       alternative = "two.sided",
       order.by = NULL, data = list())

# Heteroscedasticity robust covariance estimator vcovHC or vcovHAC 

# Testy autokorelacji reszt
lmtest::dwtest
lmtest:bgtest

car::durbinwatson

# Predykcja w modelu liniowym
predict.lm
