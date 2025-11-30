cls
clear all
glo path 	"C:\Users\et396\OneDrive\Dropbox"
glo chapter "${path}\Docencia\UNAC\Evaluation\S4"
glo Data 	"${chapter}\Aplication"
*gl Data 	"D:\Dropbox\EducatePeru\Stata\S1\Data"
glo imagen 	"${chapter}\Imagen"
glo tablas 	"${chapter}\Tablas"

/// En el slide
glo figures "${chapter}\Slide\Figures"
glo tables "${chapter}\Slide\Tables"


us "${Data}\BD4.dta",clear

******************************************************************************
*****Question 1 - Summarize the Raw Data & Test for any Gender Differences********
******************************************************************************

gl Xs "educ exper expersq tenure married divorced single male"
*Grafico

	cumul lnwage if female==1 , g(y1) 
	cumul lnwage if female==0 , g(y2) 
	
	tw conn y1 y2 lnwage if lnwage<4 , ///
	sort xline(1000) connect(J J ) ms(none none ) ///
	graphr(color(white)) ///
	legend(label(1 "Mujer") label(2 "Hombre") rows(1) size(1.9)) ///
	subtitle("") ///
	xtitle("Salarios - mensuales por trabajador (S/.)") ///
	ytitle("Porcentaje de trabajadores" ,size(2.9)) ///
	note("Fuente : Data" "Elaboracion: Autor"  ) 
	foreach w in "imagen" "figures"{		
		graph export "${`w'}\\G_1.eps", replace
		graph export "${`w'}\\G_1.png", replace	
	}

	tw (kdensity lnwage if female==1) (kdensity lnwage if female==0), ///
	legend(label(1 "Mujer") label(2 "Hombre") ///
	rows(1) size(1.9)) ///
	graphr(color(white)) ///
	ytitle("Densidad") xtitle("")
	foreach w in "imagen" "figures"{		
		graph export "${`w'}\\G_2.eps", replace
		graph export "${`w'}\\G_2.png", replace	
	}

*Estadisticas
*------------------------------------
sum 

	eststo clear
	estpost tabstat lnwage $Xs, s(n mean p50 min max sd) col(stat) 

	foreach w in "tables" "tablas"{	
		esttab ///
		using "${`w'}\T_1.tex", keep(lnwage $Xs) ///
		c("count(label(Trabajadores)) mean(label(Promedio) fmt(%10.2fc)) p50(label(Mediana) fmt(%10.2fc) ) min(label(Min.) fmt(%10.2fc)) max(label(Max.) fmt(%10.2fc)) sd(label(Std) fmt(%10.0fc))") ///
		varlabels(`e(labels)') /// 
		label nomtitles nodepvars noobs nonumbers booktabs ///
		prehead("\begin{table}[H] \scriptsize \centering \begin{threeparttable} \protect \caption{\label{tab:T1} Statistics }  \begin{tabular}{lrrrrrrrr}" \hline \hline) ///
			posthead(\hline) prefoot() postfoot(\hline \end{tabular} ///
			\begin{tablenotes} ///
			\begin{footnotesize} ///
			\item[] Fuente: BD4. ///
			\item[] Elaboracion: Author ///
			\end{footnotesize} ///
			"\end{tablenotes} \end{threeparttable} \end{table}" ) replace
	}
	
*Test de Media
*------------------------------------
gl Zs "redad rnivel2 rnivel3 rnivel4 rnivel5 rinformal"

	eststo clear
eststo mujer: quietly estpost summarize ///
    lnwage $Xs if female==1
eststo hombre: quietly estpost summarize ///
    lnwage $Xs if female==0
eststo diff: quietly estpost ttest ///
    lnwage $Xs, by(female) unequal 

	foreach w in "tables" "tablas"{
		esttab ///
		mujer hombre  diff ///
		using "${`w'}\T_2a.tex", ///
	cells("mean(pattern(1 1 0) fmt(2)) b (star pattern(0 0 1) fmt(2))" "sd(pattern(1 1 0) par)") ///
	collabels(none) nonumber nogaps label booktabs ///
	varlabels(`e(labels)') ///
	mtitles("Informal" "Formal" "Differencial") ///
	prehead("\begin{table}[H] \scriptsize \centering \begin{threeparttable} \protect \caption{\label{tab:T2} Tipo de trabajo : t-test estadistico }  \begin{tabular}{lrrrrrrrr}" \hline \hline) ///
			posthead(\hline) prefoot() postfoot(\hline \end{tabular} ///
			\begin{tablenotes} ///
			\begin{footnotesize} ///
			\item[] Standard deviations in parentheses. ///
			\item[] Fuente: BD4. ///
			\item[] Note: The standard deviations for the outcomes variables are reported in parentheses in the first two columns; the standard error of the average differential is reported in parenthesis in the final column . ///
			\end{footnotesize} ///
			"\end{tablenotes} \end{threeparttable} \end{table}" ) replace
	}

	eststo clear
	eststo mujer1: quietly estpost summarize ///
		lnwage $Xs if female==1
	eststo hombre1: quietly estpost summarize ///
		lnwage $Xs if female==0
	eststo diff1: quietly estpost ttest ///
		lnwage $Xs , by(female)	unequal	
		
	foreach w in "tables" "tablas"{
		
		esttab /// 
		mujer1 hombre1 diff1 /// 
		using "${`w'}\T_2b.tex", ///
		cells("mean(pattern(1 1 0 1 1 0 1 1 0) fmt(2)) b(star pattern(0 0 1 0 0 1 0 0 1 ) fmt(2))" "sd(pattern(1 1 0 1 1 0 1 1 0 ) par)") ///
		collabels(none) nonumber  nogaps label booktabs varlabels(`e(labels)') ///
		keep(lnwage $Xs) ///
		mgroups("Diferencias por genero", ///
		pattern(1 0 0 1 0 0 1 0 0 ) /// 
		prefix(\multicolumn{@span}{c}{) suffix(}) ///
		span erepeat(\cmidrule(lr){@span})) ///
		mtitles("Mujer" "Hombre" "$\bigtriangleup$") ///
		prehead("\begin{table}[H] \tiny \centering \begin{threeparttable} \protect \caption{\label{tab:T2} test de media segun genero}  \begin{tabular}{lccccccccccccccccc}" \hline \hline) ///
			posthead(\hline) prefoot() postfoot(\hline \end{tabular} ///
			\begin{tablenotes} ///
			\begin{footnotesize} ///
			\item[] \tiny{\textbf{Note:} Grupos de control y tratamiento en relacion al genero} ///
			\item[] \tiny{Standard deviations in parentheses}. ///
			\item[] \tiny{***, **, * denote statistical significance at the 1\%, 5\% and 10\% levels respectively for zero. }  ///
			\item[] Fuente: BD4. ///
			\item[] Elaboration: Author  ///
			\end{footnotesize} ///
			"\end{tablenotes} \end{threeparttable} \end{table}") replace
	}
	
*****************************************************************************
*Question 2 - Mean Regressions by Gender Group & Oaxaca-Blinder (OB)decompositions
*****************************************************************************	
	gl Xs "educ exper expersq tenure married divorced single male"
	gl Zs "educ exper expersq tenure married divorced single"

	*Regresion por tipo de empleo
	*Oaxaca - Blinder
	*------------------------------------
	reg lnwage $Zs if female==1 , r
	estimates store mujer

	reg lnwage $Zs if female==0, r
	estimates store hombre

**********************************************************************************************
*** Question 2: Mean Oaxaca-Blinder decomposition (aggregate and detailed sub-scomponent) ****
**********************************************************************************************
	gl Zs "educ exper expersq tenure married divorced"
	*Esta ecuacion asume la estructura de informal en ausencia de  tratamiento desigual
	oaxaca8 mujer hombre, weight(1)
	*Esta ecuacion asume la estructura de formal en ausencia de  tratamiento desigual
	oaxaca8 mujer hombre, weight(0)

	*Detalle de Oaxaca Blinder
	oaxaca lnwage $Zs, by(female) weight(1) vce(robust) nodetail

	oaxaca lnwage $Zs, by(female) weight(0) vce(robust) nodetail

	eststo clear	
	eststo: reg lnwage $Zs if female==1 , robust
	estadd local Fixed1 "$\surd$",replace
*	estadd local Fixed1 "$No$",replace

	eststo: reg lnwage $Zs if female==0, robust
	estadd local Fixed1 "$\surd$",replace
*	estadd local Fixed1 "$No$",replace
	
	foreach w in "tables" "tablas"{
		
		esttab using "${`w'}\T_3.tex", ///
		label booktabs b(2) se(2) nonumber ///
		mtitles("Mujer" "Hombre") ///
		drop(_cons ) star(* 0.10 ** 0.05 *** 0.01)  ///
		varlabels(`e(labels)')  ///
		stats(N r2_a , layout(@) fmt(a3 a3  ) ///
		labels("Observaciones" "Adj. R$^2$"  )  ) ///
		addnote("Recurso: ENAHO - 2019" "Elaboracion: Autor") ///
		prehead("\begin{table}[H] \scriptsize \centering \begin{threeparttable} \protect \caption{\label{tab:T3} Log (Salarios) OLS por tipo }  \begin{tabular}{lcccccccccccccccccc}" \hline \hline) ///
			posthead(\hline) prefoot() postfoot(\hline \end{tabular} ///
			\begin{tablenotes} ///
			\begin{footnotesize} ///
			\item[] Fuente: BD4. ///
			\item[] Elaboracion: Autor  ///
			\item[] Nota: Estimaciones robustas (MVC)  ///	
			\end{footnotesize} ///
			"\end{tablenotes} \end{threeparttable} \end{table}" ) replace
	}	
	
	gl Zs "educ exper expersq tenure married divorced"
	*version 15.1
	eststo clear
	eststo: oaxaca lnwage $Zs , by(female) nodetail  weight(1) vce(robust) 
	estadd local Fixedy "$\surd$",replace
	*nlcom ///
		(Explained: _b[explained] / _b[difference] * 100) ///
		(Unexplained: _b[unexplained] / _b[difference] * 100), post
	
	foreach w in "tables" "tablas"{
		
		esttab ///
		using "${`w'}\T_4.tex", ///
		keep(difference explained unexplained) star b(3) se(3) ///
		 noobs nonumber mtitle("Descomposicion") /// 
		 varlabels(difference "Brecha" explained "Explicada" unexplained "No-explicada" ) ///
		 stats(N , layout(@) fmt(a2 a2  ) ///
		 labels("Observaciones" )  ) eqlab(none) prehead("\begin{table}[H] \small \centering \begin{threeparttable} \protect \caption{\label{tab:T4} Descomposicion Oaxaca-Blinder   }  \begin{tabular}{lccccccccccccccccccccc}" \hline \hline) ///
				posthead(\hline) prefoot() postfoot(\hline \end{tabular} ///
				\begin{tablenotes} ///
				\begin{footnotesize} ///
				\item[] Fuente: BD4. ///
				\item[] ***, **, * denote statistical significance at the 1\%, 5\% and 10\% levels respectively for zero.   ///		
				\end{footnotesize} ///
				"\end{tablenotes} \end{threeparttable} \end{table}" ) replace

	}
	
	
************************************************************************
***** Question 3 *** Summarise the Log Wage Data by Quantiles*********** 
************************************************************************
	eststo clear
	eststo: reg lnwage female $Zs   , r
	eststo: qreg2 lnwage female $Zs , quantile (0.1)
	eststo: qreg2 lnwage female $Zs , quantile (0.25)
	eststo: qreg2 lnwage female $Zs 
	eststo: qreg2 lnwage female $Zs  , quantile (0.75)
	eststo: qreg2 lnwage female $Zs  , quantile (0.9)
	
*	estadd local Fixed1 "$\surd$",replace
*	estadd local Fixed1 "$No$",replace
		foreach w in "tables" "tablas"{
			esttab ///
			using "${`w'}/T_5.tex", ///
			label booktabs b(4) se(4) nonumber ///
			mtitles("OLS" "$10^{th}$ Percentile" "$25^{th}$ Percentile" "$50^{th}$ Percentile" "$75^{th}$ Percentile" "$90^{th}$ Percentile")  ///
			keep(female _cons )  ///
			star(* 0.10 ** 0.05 *** 0.01) /*
			*/  varlabels(`e(labels)') /*
			*/ stats(N r2  , layout(@) fmt(a3 a3 ) labels("Observaciones" "Adj. R$^2$" )  ) addnote("Recurso: Exercise 1" "Elaboracion: Autor") prehead("\begin{table}[H] \scriptsize \centering \begin{threeparttable} \protect \caption{\label{tab:T5} Estimacion del Log (Salarios) por regresion cuantilica condicional y OLS}  \begin{tabular}{lccccccccccc}" \hline \hline) ///
				posthead(\hline) prefoot() postfoot(\hline \end{tabular} ///
				\begin{tablenotes} ///
				\begin{footnotesize} ///
				\item[] Error estandar en parentesis ///
				\item[] Fuente: BD4. ///
				\item[] Nota: ***,**,* denota las significancia estadisticas al 0.01, 0.05 y 0.10  ///
				\item[] Los errores estandar  (parentensis) son corregidos a traves del comando robust , usando el comando qreg2 (Machado y Santos-Silva(2013)) ///
				\item[] Elaboracion: Autor  ///
				\end{footnotesize} ///
				"\end{tablenotes} \end{threeparttable} \end{table}" ) replace
		}
		
		
///  Pregunta 4
/// =======================================		
	gl Zs "educ exper expersq tenure married divorced"
	
	eststo clear
	
	foreach q in 10 25 50 75 90{	
		qui oaxaca_rif lnwage  $Zs , rif(q(`q')) by(female) swap wgt(1) 
		eststo decomp_q`q'
	}
	
	esttab decomp_q10 decomp_q25 decomp_q50 decomp_q75 decomp_q90, ///
	mtitle(q10 q25 q50 q75 q90) ///
	b(3) nose not noobs

	/// Export to latex
	foreach w in "tables" "tablas"{	
		esttab ///
		decomp_q10 decomp_q25  decomp_q50 ///
		decomp_q75 decomp_q90 ///
		using "${`w'}\T_6.tex", ///
		keep(difference explained unexplained) star b(3) se(3) ///
		varlabels(difference "Gap" explained "Explicada" unexplained "No Explicada" ) ///
		noobs nonumber mtitle("$10^{th}$" "$25^{th}$" "$50^{th}$" "$75^{th}$" "$90^{th}$") /*
		*/ stats(N , layout(@) fmt(a2 a2  ) ///
		labels("Obs" )  ) ///
		eqlab(none) prehead("\begin{table}[H] \small \centering \begin{threeparttable} \protect \caption{\label{tab:T3a} RIF-Oaxaca-Blinder desomposition   }  \begin{tabular}{lrrrrrrrr}" \hline \hline) ///
				posthead(\hline) prefoot() postfoot(\hline \end{tabular} ///
				\begin{tablenotes} ///
				\begin{footnotesize} ///
				\item[] \tiny{Source: DB4.} ///
				\item[] \tiny{Elaboration: Author}  ///
				\item[] \tiny{***, **, * denote statistical significance at the 1\%, 5\% and 10\% levels respectively for zero.}  ///	
				\end{footnotesize} ///
				"\end{tablenotes} \end{threeparttable} \end{table}" ) replace
	}	
	