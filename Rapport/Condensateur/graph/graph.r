#!/usr/bin/env Rscript

library(extrafont)

TITRE <- expression(paste("Charge Q Accumulée en Fonction de la Différence de Potentiel ", Delta, "V"))
FONT  <- "CM Roman"
LAB   <- c(expression(paste(Delta, "V (V)")), "Q (mC)")
COL   <- c( "red","blue","green","purple")
SYM   <- c(     0,     0,      0,       0)
SYMS  <- c(   0.3,   0.5,    0.3,     0.3)
LTY   <- c(     2,     2,      2,       2)
LWD   <- 0.6

DATA   <- read.table("graph.csv", header=T,sep=",")
CONST  <- read.table("const.csv", header=T,sep=",")
CHARGE <- matrix(nrow=18, ncol=8)

############
# Fonction #
############
formatEng <- function(x) {
    s<-as.numeric(strsplit(format(x, scientific=T),"e")[[1]])
    return(paste(s[1]*10^(s[2]%%3),as.integer(s[2]-(s[2]%%3)),sep="e"))
}

formatExp <- function(x,y, show=TRUE) {
    s <- as.numeric(strsplit(format(x, scientific=T),"e")[[1]])

    if(isTRUE(show)) {
        output <- paste(s[1]*10^(s[2]-y),y,sep="e")
    } else {
        output <- s[1]*10^(s[2]-y)
    }

    return(output)
}

###########
# Calculs #
###########
for(i in 1:4) {
    for(j in 1:nrow(DATA[i])) {
        ################################
        #  t: DATA[j,i+1]              #
        # dt: CONST[4,i]               #
        #  I: CONST[1,i]*10^CONST[3,i] #
        # dI: CONST[2,i]*10^CONST[3,i] #
        #                              #
        #  Q = I*t                     #
        # dQ = t*dI+I*dt               #
        ################################
        CHARGE[j,2*i-1] <- (CONST[1,i]*10^CONST[3,i])*(DATA[j,i+1])
        CHARGE[j,2*i  ] <- (DATA[j,i+1])*(CONST[2,i]*10^CONST[3,i])+
                           (CONST[1,i]*10^CONST[3,i])*(CONST[4,i])
    }
}

#############
# Graphique #
#############
XSCALE <- 1*10^0
YSCALE <- 1*10^3
XAXIS  <- XSCALE*seq(from=0.0, to=17.00, by=1.000)
YAXIS  <- YSCALE*seq(from=0.0, to=00.07, by=0.005)

pdf("graph.pdf", family=FONT, fonts=c(FONT), width=10, height=10, pointsize=16)
#png(filename="graph1.png", width=2000, height=2000, pointsize=50)
plot.default(XSCALE*DATA[,1], YSCALE*CHARGE[,7], type="n", xlab=LAB[1], ylab=LAB[2],
             xaxt="n", yaxt="n", family=FONT)

#####################
# Séries de données #
#####################
for(i in 1:4) {
    points(DATA[,1]*XSCALE, CHARGE[,2*i-1]*YSCALE, pch=SYM[i], cex=0, col=COL[i])
}
title(main=TITRE, family=FONT)

#######
# Axe #
#######
axis(1, family=FONT, at=XAXIS, labels=sprintf("%.1f", XAXIS))
axis(2, family=FONT, at=YAXIS, labels=sprintf("%.0f", YAXIS))

##########
# Grille #
##########
abline(v=(seq(0, 20,1)), lty="dotted", col="grey")
abline(h=(seq(0,100,5)), lty="dotted", col="grey")

#####################
# Bar d'incertitude #
#####################
dV <- 0.1
for(i in 1:4) {
    for(j in 1:nrow(DATA[i])) {
        a = YSCALE*(CHARGE[j,2*i-1]+CHARGE[j,2*i])
        b = YSCALE*(CHARGE[j,2*i-1]-CHARGE[j,2*i])

        segments(XSCALE*(DATA[j,1]-dV), a,
                 XSCALE*(DATA[j,1]+dV), a, col=COL[i], lwd=LWD)
        segments(XSCALE*(DATA[j,1]-dV), b,
                 XSCALE*(DATA[j,1]+dV), b, col=COL[i], lwd=LWD)
        segments(XSCALE*(DATA[j,1]-dV), a,
                 XSCALE*(DATA[j,1]-dV), b, col=COL[i], lwd=LWD)
        segments(XSCALE*(DATA[j,1]+dV), a,
                 XSCALE*(DATA[j,1]+dV), b, col=COL[i], lwd=LWD)
    }
}

###################
# Courbe tendance #
###################
equation <- list()
for(i in 1:4) {
    X <- DATA[,1]
    Y <- CHARGE[,2*i-1]

    equation[[i]] <- lm(Y~X)

    line <- equation[[i]]
    abline(YSCALE*coef(line)[[1]], YSCALE*coef(line)[[2]], col=COL[i], lty=LTY[i], lwd=LWD)
}

#########
# Texte #
#########
M <- sprintf("%.2f",formatExp(coef(equation[[1]])[[2]],-3,FALSE))
B <- sprintf("%.2f",formatExp(coef(equation[[1]])[[1]],-3,FALSE))
text(3,45, substitute(paste("Q"[1]*"("*Delta*"V)=",M,Delta*"V",B), list(M=M, B=B)), family=FONT)

M <- sprintf("%.2f",formatExp(coef(equation[[2]])[[2]],-3,FALSE))
B <- sprintf("%.2f",formatExp(coef(equation[[2]])[[1]],-3,FALSE))
text(3,41, substitute(paste("Q"[2]*"("*Delta*"V)=",M,Delta*"V",B), list(M=M, B=B)), family=FONT)

M <- sprintf("%.2f",formatExp(coef(equation[[3]])[[2]],-3,FALSE))
B <- sprintf("%.2f",formatExp(coef(equation[[3]])[[1]],-3,FALSE))
text(3,37, substitute(paste("Q"[s]*"("*Delta*"V)=",M,Delta*"V",B), list(M=M, B=B)), family=FONT)

M <- sprintf("%.2f",formatExp(coef(equation[[4]])[[2]],-3,FALSE))
B <- sprintf("%.2f",formatExp(coef(equation[[4]])[[1]],-3,FALSE))
text(3,33, substitute(paste("Q"[p]*"("*Delta*"V)=",M,Delta*"V",B), list(M=M, B=B)), family=FONT)
#cat(summary(equation[[i]])$r.squared, "\n")
###########
# Légende #
###########
NAME <- c("Condensateur 1", "Condensateur 2",
          "Condensateurs en série", "Condensateurs en parallèle")

par(family=FONT)
legend("topleft", NAME, col=COL, lty=LTY, pch=SYM, bty="o",bg="white", lwd=LWD)

###########
# Tableau #
###########
for(i in 1:18) {
    for(j in 1:4) {
         C <- formatC(sprintf("%.2f", formatExp(CHARGE[i,2*j-1],-3, FALSE)), width = 5, format = "d", flag = "0")
        dC <- formatC(sprintf("%.2f", formatExp(CHARGE[i,2*j  ],-3, FALSE)), width = 4, format = "d", flag = "0")
        cat("(", C, "±", dC, ")\t")
    }
    cat("\n")
}

dev.off()
embed_fonts("graph.pdf", outfile="graph_embed.pdf")
