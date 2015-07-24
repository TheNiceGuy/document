#!/usr/bin/env Rscript

library(extrafont)

#############
# Variables #
#############
TITRE  <- "Potentiel V en fonction de la distance équipotentielle x\nsur l'axe central"
FONT   <- "CM Roman"
XAXIS  <- 0:7
YAXIS  <- 0:7
LAB <- c("x (cm)", "V (V)")

COL  <- c("blue"  , "red"  )
SYM  <- c( 17     ,  18    )
SYMS <- c( 1      ,  1     )
LTY  <- c( 1      ,  2     )

################
# Incertitudes #
################
dx=0.1
dy=0.1

DATA = read.table("data.csv", header=T,sep=",")

pdf("graph.pdf", family=FONT, fonts=c(FONT), width=10, height=10, pointsize=16)
#png(filename="graph.png", width=2000, height=2000, pointsize=50)
#svg(filename="graph.svg", width=40, height=40, pointsize=73)
plot.default(DATA$x1, DATA$V1, type="n", xlab=LAB[1], ylab=LAB[2],
             xaxt="n",  yaxt="n", family=FONT)

#######
# Axe #
#######
axis(1, family=FONT, at=XAXIS, labels=sprintf("%.2f", XAXIS))
axis(2, family=FONT, at=YAXIS, labels=sprintf("%.2f", YAXIS))
grid(NULL, NULL, lwd = 1, lty=2)

#####################
# Séries de données #
#####################
points(DATA$x1, DATA$V1, pch=SYM[1], cex=SYMS[1], col=COL[1])
points(DATA$x2, DATA$V2, pch=SYM[2], cex=SYMS[2], col=COL[2])
title(main=TITRE, family=FONT)

#####################
# Bar d'incertitude #
#####################
for(i in 1:length(DATA$x1)) {
    # Données 1
    a = DATA$V1[i]+dy
    b = DATA$V1[i]-dy

    segments(DATA$x1[i],    a,
             DATA$x1[i],    b, col=COL[1])
    segments(DATA$x1[i]-dx, a,
             DATA$x1[i]+dx, a, col=COL[1])
    segments(DATA$x1[i]-dx, b,
             DATA$x1[i]+dx, b, col=COL[1])

    # Données 2
    a = DATA$V2[i]+dy
    b = DATA$V2[i]-dy

    segments(DATA$x2[i],    a,
             DATA$x2[i],    b, col=COL[2])
    segments(DATA$x2[i]-dx, a,
             DATA$x2[i]+dx, a, col=COL[2])
    segments(DATA$x2[i]-dx, b,
             DATA$x2[i]+dx, b, col=COL[2])
}

###################
# Courbe tendance #
###################
# Données 1
fit1 <- glm(DATA$V1~DATA$x1)
abline(fit1, col=COL[1], lty=LTY[1])

# Données 2
x <- DATA$x2
y <- DATA$V2
func <- function(x,a,b,c,d) {(a*x^3) + (b*x^2) + (c*x) + d}
fit2 <- nls(y~func(x,a,b,c,d), start = c(a=0, b=0, c=0, d=0))
coef <- coef(fit2)
curve(func(x, a=coef[1], b=coef[2], c=coef[3], d=coef[4]), from=-1, to=8,
      add = TRUE, col=COL[2], lwd=1, lty=LTY[2])

###########
# Légende #
###########
par(family=FONT)
legend("topleft", c("Dipôle", "Plaques Parallèles"), col=c(COL[1], COL[2]),
       pch=SYM, lwd=c(1, 1), lty=LTY, bty="o", bg="white")

#############
# Équations #
#############
# Données 1
M <- sprintf("%.2f",coef(fit1)[[2]])
B <- sprintf("%.2f",coef(fit1)[[1]])
text(3, 1, substitute(paste("V"[dipôle]*"(x)=",M,"x-",B), list(M=M, B=B)))

#Données 2
A <- sprintf("%.2f", coef[1])
B <- sprintf("%.2f", coef[2])
C <- sprintf("%.2f", coef[3])
D <- sprintf("%.2f", coef[4])
text(5, 2, substitute(paste("V"[plaques]*"(x)=",A,"x"^3,B,"x"^2,"+",C,"x+",D),
    list(A=A, B=B, C=C, D=D)))

dev.off()
embed_fonts("graph.pdf", outfile="graph_embed.pdf")
