
* Readme


* Figure was created with the following command in Stata




. twoway (scatter ml_mean_compress2 agency_ideol_E, ytitle("Sentiment") msize(vsmall) msymbol(circle)) (scatter sent_agency agency_ideol_E, mcolor(black) msize(vlarge) msymbol(circle_hollow)), legend(on) scheme(s2mono) xsize(4.5) ysize(3)
