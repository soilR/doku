# Erstelle Plot für Korrelationen
spezialplot <- function(x = rnorm(100),
                        y = rnorm(100),
                        add.mean = FALSE,
                        add.regression = FALSE,
                        p.threshold = .05,
                        add.modeltext = FALSE,
                        ...  # Weitere Argumente die ergänzt werden sollen
) {
  # Erstelle den Plot
  # und setze ggf. weitere Argumente wie `main` für Titel
  plot(x, y, ...)
  
  # Erstelle Referenzlinie vom Mittelwert wenn add.mean = TRUE
  if(add.mean == TRUE) {
    
    abline(h = mean(y), lty = 2)
    abline(v = mean(x), lty = 2)
  }
  
  # Erstelle Regressionlinie wenn add.regression = TRUE
  if(add.regression == TRUE) {
    
    model <- lm(y ~ x) # Erstelle Regression mit `lm` linear model
    
    p.value <- anova(model)$"Pr(>F)"[1] # Ziehe den p Wert aus dem Objekt p.value
    
    # Definiere die Farbe in Abhängigkeit von p.value und p.threshold
    if(p.value < p.threshold) {line.col <- "red"}
    if(p.value >= p.threshold) {line.col <- "black"}
    
    abline(lm(y ~ x), col = line.col, lwd = 2) # Füge die Regressionlinie hinzu
    
  }
  
  # Add regression equation text if add.modeltext is TRUE
  if(add.modeltext == TRUE) {
    
    # Erstelle Regressionsdaten
    model <- lm(y ~ x)
    
    # Extrahiere die Koeffizienten vom Objekt `model`
    coefficients <- model$coefficients
    a <- round(coefficients[1], 2)
    b <- round(coefficients[2], 2)
    
    # Create text
    model.text <- paste("Regression Equation: ", a, " + ", b, " * x", sep = "")
    
    # Add text to top of plot
    mtext(model.text, side = 3, line = .5, cex = .8)
    
  }
}




