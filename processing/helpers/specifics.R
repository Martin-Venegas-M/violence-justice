bwcomp <- "
RI_x =~ 1*c18_11_w01 + 1*c18_11_w02 + 1*c18_11_w03 + 1*c18_11_w04 + 1*c18_11_w05 + 1*c18_11_w06 + 1*c18_11_w07
RI_y =~ 1*f05_04_w01 + 1*f05_04_w02 + 1*f05_04_w03 + 1*f05_04_w04 + 1*f05_04_w05 + 1*f05_04_w06 + 1*f05_04_w07
cx1 =~ 1*c18_11_w01
cx2 =~ 1*c18_11_w02
cx3 =~ 1*c18_11_w03
cx4 =~ 1*c18_11_w04
cx5 =~ 1*c18_11_w05
cx6 =~ 1*c18_11_w06
cx7 =~ 1*c18_11_w07
cy1 =~ 1*f05_04_w01
cy2 =~ 1*f05_04_w02
cy3 =~ 1*f05_04_w03
cy4 =~ 1*f05_04_w04
cy5 =~ 1*f05_04_w05
cy6 =~ 1*f05_04_w06
cy7 =~ 1*f05_04_w07
c18_11_w01 ~~ 0*c18_11_w01
c18_11_w02 ~~ 0*c18_11_w02
c18_11_w03 ~~ 0*c18_11_w03
c18_11_w04 ~~ 0*c18_11_w04
c18_11_w05 ~~ 0*c18_11_w05
c18_11_w06 ~~ 0*c18_11_w06
c18_11_w07 ~~ 0*c18_11_w07
f05_04_w01 ~~ 0*f05_04_w01
f05_04_w02 ~~ 0*f05_04_w02
f05_04_w03 ~~ 0*f05_04_w03
f05_04_w04 ~~ 0*f05_04_w04
f05_04_w05 ~~ 0*f05_04_w05
f05_04_w06 ~~ 0*f05_04_w06
f05_04_w07 ~~ 0*f05_04_w07
"

varcovs <- "

cx1 ~~ cy1
cx2 ~~ cy2
cx3 ~~ cy3
cx4 ~~ cy4
cx5 ~~ cy5
cx6 ~~ cy6
cx7 ~~ cy7
RI_x ~~ RI_x
RI_y ~~ RI_y
RI_x ~~ RI_y
RI_x ~~ 0*cx1
RI_x ~~ 0*cy1
RI_y ~~ 0*cx1
RI_y ~~ 0*cy1

"

# a1

a1 <- "
cx2 ~ cx1 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cx3 ~ cx2 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cx4 ~ cx3 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cx5 ~ cx4 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cx6 ~ cx5 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cx7 ~ cx6 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cy2 ~ cy1 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cy3 ~ cy2 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cy4 ~ cy3 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cy5 ~ cy4 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cy6 ~ cy5 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cy7 ~ cy6 + m0_edad_w01 + m0_sexo_w01 + m01_w01
"

# a2

a2 <- "
cx2 ~ a*cx1 + c*m0_edad_w01 + d*m0_sexo_w01 + e*m01_w01
cx3 ~ a*cx2 + c*m0_edad_w01 + d*m0_sexo_w01 + e*m01_w01
cx4 ~ a*cx3 + c*m0_edad_w01 + d*m0_sexo_w01 + e*m01_w01
cx5 ~ a*cx4 + c*m0_edad_w01 + d*m0_sexo_w01 + e*m01_w01
cx6 ~ a*cx5 + c*m0_edad_w01 + d*m0_sexo_w01 + e*m01_w01
cx7 ~ a*cx6 + c*m0_edad_w01 + d*m0_sexo_w01 + e*m01_w01
cy2 ~ g*cy1 + h*m0_edad_w01 + i*m0_sexo_w01 + j*m01_w01
cy3 ~ g*cy2 + h*m0_edad_w01 + i*m0_sexo_w01 + j*m01_w01
cy4 ~ g*cy3 + h*m0_edad_w01 + i*m0_sexo_w01 + j*m01_w01
cy5 ~ g*cy4 + h*m0_edad_w01 + i*m0_sexo_w01 + j*m01_w01
cy6 ~ g*cy5 + h*m0_edad_w01 + i*m0_sexo_w01 + j*m01_w01
cy7 ~ g*cy6 + h*m0_edad_w01 + i*m0_sexo_w01 + j*m01_w01

"

# b1

b1 <- "
cx2 ~ cx1 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cx3 ~ cx2 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cx4 ~ cx3 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cx5 ~ cx4 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cx6 ~ cx5 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cx7 ~ cx6 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cy2 ~ cx1 + cy1 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cy3 ~ cx2 + cy2 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cy4 ~ cx3 + cy3 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cy5 ~ cx4 + cy4 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cy6 ~ cx5 + cy5 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cy7 ~ cx6 + cy6 + m0_edad_w01 + m0_sexo_w01 + m01_w01
"

# b2

b2 <- "

cx2 ~ a*cx1 + c*m0_edad_w01 + d*m0_sexo_w01 + e*m01_w01
cx3 ~ a*cx2 + c*m0_edad_w01 + d*m0_sexo_w01 + e*m01_w01
cx4 ~ a*cx3 + c*m0_edad_w01 + d*m0_sexo_w01 + e*m01_w01
cx5 ~ a*cx4 + c*m0_edad_w01 + d*m0_sexo_w01 + e*m01_w01
cx6 ~ a*cx5 + c*m0_edad_w01 + d*m0_sexo_w01 + e*m01_w01
cx7 ~ a*cx6 + c*m0_edad_w01 + d*m0_sexo_w01 + e*m01_w01
cy2 ~ f*cx1 + g*cy1 + h*m0_edad_w01 + i*m0_sexo_w01 + j*m01_w01
cy3 ~ f*cx2 + g*cy2 + h*m0_edad_w01 + i*m0_sexo_w01 + j*m01_w01
cy4 ~ f*cx3 + g*cy3 + h*m0_edad_w01 + i*m0_sexo_w01 + j*m01_w01
cy5 ~ f*cx4 + g*cy4 + h*m0_edad_w01 + i*m0_sexo_w01 + j*m01_w01
cy6 ~ f*cx5 + g*cy5 + h*m0_edad_w01 + i*m0_sexo_w01 + j*m01_w01
cy7 ~ f*cx6 + g*cy6 + h*m0_edad_w01 + i*m0_sexo_w01 + j*m01_w01

"

# c1

c1 <- "
cx2 ~ cx1 + cy1 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cx3 ~ cx2 + cy2 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cx4 ~ cx3 + cy3 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cx5 ~ cx4 + cy4 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cx6 ~ cx5 + cy5 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cx7 ~ cx6 + cy6 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cy2 ~ cy1 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cy3 ~ cy2 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cy4 ~ cy3 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cy5 ~ cy4 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cy6 ~ cy5 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cy7 ~ cy6 + m0_edad_w01 + m0_sexo_w01 + m01_w01
"

# c2

c2 <- "

cx2 ~ a*cx1 + b*cy1 + c*m0_edad_w01 + d*m0_sexo_w01 + e*m01_w01
cx3 ~ a*cx2 + b*cy2 + c*m0_edad_w01 + d*m0_sexo_w01 + e*m01_w01
cx4 ~ a*cx3 + b*cy3 + c*m0_edad_w01 + d*m0_sexo_w01 + e*m01_w01
cx5 ~ a*cx4 + b*cy4 + c*m0_edad_w01 + d*m0_sexo_w01 + e*m01_w01
cx6 ~ a*cx5 + b*cy5 + c*m0_edad_w01 + d*m0_sexo_w01 + e*m01_w01
cx7 ~ a*cx6 + b*cy6 + c*m0_edad_w01 + d*m0_sexo_w01 + e*m01_w01
cy2 ~ g*cy1 + h*m0_edad_w01 + i*m0_sexo_w01 + j*m01_w01
cy3 ~ g*cy2 + h*m0_edad_w01 + i*m0_sexo_w01 + j*m01_w01
cy4 ~ g*cy3 + h*m0_edad_w01 + i*m0_sexo_w01 + j*m01_w01
cy5 ~ g*cy4 + h*m0_edad_w01 + i*m0_sexo_w01 + j*m01_w01
cy6 ~ g*cy5 + h*m0_edad_w01 + i*m0_sexo_w01 + j*m01_w01
cy7 ~ g*cy6 + h*m0_edad_w01 + i*m0_sexo_w01 + j*m01_w01

"

# d1

d1 <- "
cx2 ~ cx1 + cy1 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cx3 ~ cx2 + cy2 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cx4 ~ cx3 + cy3 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cx5 ~ cx4 + cy4 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cx6 ~ cx5 + cy5 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cx7 ~ cx6 + cy6 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cy2 ~ cx1 + cy1 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cy3 ~ cx2 + cy2 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cy4 ~ cx3 + cy3 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cy5 ~ cx4 + cy4 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cy6 ~ cx5 + cy5 + m0_edad_w01 + m0_sexo_w01 + m01_w01
cy7 ~ cx6 + cy6 + m0_edad_w01 + m0_sexo_w01 + m01_w01
"

d2 <- "

cx2 ~ a*cx1 + b*cy1 + c*m0_edad_w01 + d*m0_sexo_w01 + e*m01_w01
cx3 ~ a*cx2 + b*cy2 + c*m0_edad_w01 + d*m0_sexo_w01 + e*m01_w01
cx4 ~ a*cx3 + b*cy3 + c*m0_edad_w01 + d*m0_sexo_w01 + e*m01_w01
cx5 ~ a*cx4 + b*cy4 + c*m0_edad_w01 + d*m0_sexo_w01 + e*m01_w01
cx6 ~ a*cx5 + b*cy5 + c*m0_edad_w01 + d*m0_sexo_w01 + e*m01_w01
cx7 ~ a*cx6 + b*cy6 + c*m0_edad_w01 + d*m0_sexo_w01 + e*m01_w01
cy2 ~ f*cx1 + g*cy1 + h*m0_edad_w01 + i*m0_sexo_w01 + j*m01_w01
cy3 ~ f*cx2 + g*cy2 + h*m0_edad_w01 + i*m0_sexo_w01 + j*m01_w01
cy4 ~ f*cx3 + g*cy3 + h*m0_edad_w01 + i*m0_sexo_w01 + j*m01_w01
cy5 ~ f*cx4 + g*cy4 + h*m0_edad_w01 + i*m0_sexo_w01 + j*m01_w01
cy6 ~ f*cx5 + g*cy5 + h*m0_edad_w01 + i*m0_sexo_w01 + j*m01_w01
cy7 ~ f*cx6 + g*cy6 + h*m0_edad_w01 + i*m0_sexo_w01 + j*m01_w01

"

d2_mod4 <- "

cx2 ~ c(a1, a2, a3, a4)*cx1 + c(b1, b2, b3, b4)*cy1 + c(c1, c2, c3, c4)*m0_edad_w01 + c(d1, d2, d3, d4)*m0_sexo_w01 + c(e1, e2, e3, e4)*m01_w01
cx3 ~ c(a1, a2, a3, a4)*cx2 + c(b1, b2, b3, b4)*cy2 + c(c1, c2, c3, c4)*m0_edad_w01 + c(d1, d2, d3, d4)*m0_sexo_w01 + c(e1, e2, e3, e4)*m01_w01
cx4 ~ c(a1, a2, a3, a4)*cx3 + c(b1, b2, b3, b4)*cy3 + c(c1, c2, c3, c4)*m0_edad_w01 + c(d1, d2, d3, d4)*m0_sexo_w01 + c(e1, e2, e3, e4)*m01_w01
cx5 ~ c(a1, a2, a3, a4)*cx4 + c(b1, b2, b3, b4)*cy4 + c(c1, c2, c3, c4)*m0_edad_w01 + c(d1, d2, d3, d4)*m0_sexo_w01 + c(e1, e2, e3, e4)*m01_w01
cx6 ~ c(a1, a2, a3, a4)*cx5 + c(b1, b2, b3, b4)*cy5 + c(c1, c2, c3, c4)*m0_edad_w01 + c(d1, d2, d3, d4)*m0_sexo_w01 + c(e1, e2, e3, e4)*m01_w01
cx7 ~ c(a1, a2, a3, a4)*cx6 + c(b1, b2, b3, b4)*cy6 + c(c1, c2, c3, c4)*m0_edad_w01 + c(d1, d2, d3, d4)*m0_sexo_w01 + c(e1, e2, e3, e4)*m01_w01
cy2 ~ c(f1, f2, f3, f4)*cx1 + c(g1, g2, g3, g4)*cy1 + c(h1, h2, h3, h4)*m0_edad_w01 + c(i1, i2, i3, i4)*m0_sexo_w01 + c(j1, j2, j3, j4)*m01_w01
cy3 ~ c(f1, f2, f3, f4)*cx2 + c(g1, g2, g3, g4)*cy2 + c(h1, h2, h3, h4)*m0_edad_w01 + c(i1, i2, i3, i4)*m0_sexo_w01 + c(j1, j2, j3, j4)*m01_w01
cy4 ~ c(f1, f2, f3, f4)*cx3 + c(g1, g2, g3, g4)*cy3 + c(h1, h2, h3, h4)*m0_edad_w01 + c(i1, i2, i3, i4)*m0_sexo_w01 + c(j1, j2, j3, j4)*m01_w01
cy5 ~ c(f1, f2, f3, f4)*cx4 + c(g1, g2, g3, g4)*cy4 + c(h1, h2, h3, h4)*m0_edad_w01 + c(i1, i2, i3, i4)*m0_sexo_w01 + c(j1, j2, j3, j4)*m01_w01
cy6 ~ c(f1, f2, f3, f4)*cx5 + c(g1, g2, g3, g4)*cy5 + c(h1, h2, h3, h4)*m0_edad_w01 + c(i1, i2, i3, i4)*m0_sexo_w01 + c(j1, j2, j3, j4)*m01_w01
cy7 ~ c(f1, f2, f3, f4)*cx6 + c(g1, g2, g3, g4)*cy6 + c(h1, h2, h3, h4)*m0_edad_w01 + c(i1, i2, i3, i4)*m0_sexo_w01 + c(j1, j2, j3, j4)*m01_w01

"

d2_mod2 <- "

cx2 ~ c(a1, a2)*cx1 + c(b1, b2)*cy1 + c(c1, c2)*m0_edad_w01 + c(d1, d2)*m0_sexo_w01 + c(e1, e2)*m01_w01
cx3 ~ c(a1, a2)*cx2 + c(b1, b2)*cy2 + c(c1, c2)*m0_edad_w01 + c(d1, d2)*m0_sexo_w01 + c(e1, e2)*m01_w01
cx4 ~ c(a1, a2)*cx3 + c(b1, b2)*cy3 + c(c1, c2)*m0_edad_w01 + c(d1, d2)*m0_sexo_w01 + c(e1, e2)*m01_w01
cx5 ~ c(a1, a2)*cx4 + c(b1, b2)*cy4 + c(c1, c2)*m0_edad_w01 + c(d1, d2)*m0_sexo_w01 + c(e1, e2)*m01_w01
cx6 ~ c(a1, a2)*cx5 + c(b1, b2)*cy5 + c(c1, c2)*m0_edad_w01 + c(d1, d2)*m0_sexo_w01 + c(e1, e2)*m01_w01
cx7 ~ c(a1, a2)*cx6 + c(b1, b2)*cy6 + c(c1, c2)*m0_edad_w01 + c(d1, d2)*m0_sexo_w01 + c(e1, e2)*m01_w01
cy2 ~ c(f1, f2)*cx1 + c(g1, g2)*cy1 + c(h1, h2)*m0_edad_w01 + c(i1, i2)*m0_sexo_w01 + c(j1, j2)*m01_w01
cy3 ~ c(f1, f2)*cx2 + c(g1, g2)*cy2 + c(h1, h2)*m0_edad_w01 + c(i1, i2)*m0_sexo_w01 + c(j1, j2)*m01_w01
cy4 ~ c(f1, f2)*cx3 + c(g1, g2)*cy3 + c(h1, h2)*m0_edad_w01 + c(i1, i2)*m0_sexo_w01 + c(j1, j2)*m01_w01
cy5 ~ c(f1, f2)*cx4 + c(g1, g2)*cy4 + c(h1, h2)*m0_edad_w01 + c(i1, i2)*m0_sexo_w01 + c(j1, j2)*m01_w01
cy6 ~ c(f1, f2)*cx5 + c(g1, g2)*cy5 + c(h1, h2)*m0_edad_w01 + c(i1, i2)*m0_sexo_w01 + c(j1, j2)*m01_w01
cy7 ~ c(f1, f2)*cx6 + c(g1, g2)*cy6 + c(h1, h2)*m0_edad_w01 + c(i1, i2)*m0_sexo_w01 + c(j1, j2)*m01_w01

"