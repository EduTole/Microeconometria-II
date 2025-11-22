
cls
clear all
glo main		"C:\Users\et396\OneDrive\Dropbox"
glo lecture		"${main}\Docencia\UNAC\Evaluation\S1"
glo Data		"${lecture}\Code"
glo Tablas		"${lecture}\Tablas"
glo figura		"${lecture}\Slides\Figures"

*Carga de Data
import delimited "${Data}\BD1.csv"
d

* Filtros
g logr6 =log(r6)
keep if logr6>0
keep if redad<70
g redadsq = redad*redad

*Modelo Multivariado
*-----------------------------------
*Analisis exploratorio
glo Xs "r6 rneduca reduca rmujer redad rinformal"
glo Zs "rneduca i.reduca rmujer redad redadsq rinformal"
sum $Xs

	eststo clear
	estpost tabstat $Xs , s(n mean p50 min max sd) col(stat) 

	esttab using "${Tablas}\T_1_stata.tex", ///
	c("count(label(Personas)) mean(label(Promedio) fmt(%10.2fc)) p50(label(Mediana) fmt(%10.2fc) ) min(label(Min.) fmt(%10.2fc)) max(label(Max.) fmt(%10.2fc)) sd(label(Std) fmt(%10.0fc))") ///
	label nomtitles nodepvars noobs nonumbers booktabs prehead("\begin{table}[H] \scriptsize \centering \begin{threeparttable} \protect \caption{\label{tab:T1} Estadisticas descriptivas }  \begin{tabular}{lrrrrrrrr}" \hline \hline) ///
		posthead(\hline) prefoot() postfoot(\hline \end{tabular} ///
		\begin{tablenotes} ///
		\begin{footnotesize} ///
		\item[] Fuente: ENAHO - 2021. ///
		\item[] Elaboracion: Autor  ///
		\end{footnotesize} ///
		"\end{tablenotes} \end{threeparttable} \end{table}" ) replace	

/// Grafico de ingresos y educacion
		preserve 
		collapse (mean) r6 if ryear==2021, by(redad reduca)
		tw ///
		(line r6 redad if reduca==2) ///
		(line r6 redad if reduca==3) ///		
		(line r6  redad if reduca==5), ///
		legend(label(1 "Primaria") label(2 "Secundaria") label(3 "Superior")) ///
		xtitle("Edad") ytitle("Salarios")
		graph export "${figura}\g1.eps",replace
		
		restore 
		
/// ============================================
/// Pregunta 1
/// ============================================
reg logr6 rneduca

glo Zs "rneduca i.reduca rmujer redad redadsq rinformal"
reg logr6 $Zs
encode rdpto, g(rDpto)	

	eststo clear
	eststo: reg logr6 rneduca 
	eststo: reg logr6 $Zs 
	eststo: reg logr6 $Zs i.ryear i.rDpto

	esttab using "${Tablas}\T_3_stata.tex", keep(rneduca)  label booktabs b(3) se(3) nonumber star(* 0.10 ** 0.05 *** 0.01) ///
		stats(N r2_a, layout(@) fmt(a3 a3  a2 ) ///
		labels("Observaciones" "R$^2$")  ) ///
		varlabels(`e(labels)') mtitles("(1)" "(2)" "(3)")  prehead("\begin{table}[H] \scriptsize \centering \begin{threeparttable} \protect \caption{\label{tab:T3} Modelo Lineal }  \begin{tabular}{lrrrrrrrr}" \hline \hline) ///
		posthead(\hline) prefoot() postfoot(\hline \end{tabular} ///
		\begin{tablenotes} ///
		\begin{footnotesize} ///
		\item[] Errores estandar en parentesis. ///
		\item[] Fuente: ENAHO. ///
		\item[] Elaboracion: Autor  ///
		\item[] ***, **, * denote statistical significance at the 1\%, 5\% and 10\% levels respectively for zero.	///	
		\end{footnotesize} ///
		"\end{tablenotes} \end{threeparttable} \end{table}" ) replace

		*Pregunta 1b) Estimacion de errores del modelo		
		*--------------------------------------------------------------
		reg logr6 $Zs
		*Prediciones de los errores del modelo
		predict uhat,resid

		*Bosquejo de la densidad kernel de las estimaciones
		*del error y tets para nomalidad de la distribucion de errors
		kdensity uhat
		sktest uhat, noadj 
		graph export "${figura}/g2.eps", replace

		*Grafico de caja
		graph box uhat
		graph export "${figura}/g3.eps", replace

		*Pregunta 1c) prueba de errores de heterocedasticidad	
		*--------------------------------------------------------------
		*Test de Koenker para heterocedasticidad
		hettest $Zs, iid

		*Test manual de heterocedasticidad
		gen uhatsq=uhat^2
		label var uhatsq "$\mu^{2}$"

		reg uhatsq $Zs
		scalar r2 = e(r2)
		scalar sample = e(N)
		scalar lm_het = r2*sample
		display lm_het

		eststo clear
		eststo: reg uhatsq $Zs

		esttab using "${Tablas}\T_4_stata.tex",  label booktabs b(3) se(2) wide nonumber star(* 0.10 ** 0.05 *** 0.01) ///
		stats(N r2_a, layout(@) fmt(a3 a3  a2 ) ///
		labels("Observaciones" "R$^2$")  ) ///
		varlabels(`e(labels)') mtitles("$\varepsilon^{2}$" ) prehead("\begin{table}[H] \tiny \centering \begin{threeparttable} \protect \caption{\label{tab:T3} Modelo Lineal }  \begin{tabular}{lrrrrrrrr}" \hline \hline) ///
			posthead(\hline) prefoot() postfoot(\hline \end{tabular} ///
			\begin{tablenotes} ///
			\begin{footnotesize} ///
			\item[] Errores estandar en parentesis. ///
			\item[] Fuente: ENAHO. ///
			\item[] Elaboracion: Autor  ///
			\item[] ***, **, * denote statistical significance at the 1\%, 5\% and 10\% levels respectively for zero.	///	
			\end{footnotesize} ///
			"\end{tablenotes} \end{threeparttable} \end{table}" ) replace

/// ============================================
/// Pregunta 2
/// ============================================
	*Pregunta 2a) prediccion de los errores

	eststo clear
	eststo: reg logr6 rneduca        			,r
	eststo: reg logr6 $Zs			      		,r
	eststo: reg logr6 $Zs   i.ryear i.rDpto    	,r

	esttab using "${Tablas}\T_4a_stata.tex", keep(rneduca rmujer redad redadsq ) ///
	label booktabs b(3) se(3) nonumber wide star(* 0.10 ** 0.05 *** 0.01) ///
		stats(N r2_a, layout(@) fmt(a3 a3  a2 ) ///
		labels("Observaciones" "R$^2$")  ) ///
		varlabels(`e(labels)') mtitles("$\ln wage$" "$\ln wage$" "$\ln wage$" ) prehead("\begin{table}[H] \scriptsize \centering \begin{threeparttable} \protect \caption{\label{tab:T4} Modelo Lineal robusto }  \begin{tabular}{lrrrrrrrr}" \hline \hline) ///
		posthead(\hline) prefoot() postfoot(\hline \end{tabular} ///
		\begin{tablenotes} ///
		\begin{footnotesize} ///
		\item[] Errores estandar en parentesis. ///
		\item[] Fuente: ENAHO. ///
		\item[] Elaboracion: Autor  ///
		\item[] ***, **, * denote statistical significance at the 1\%, 5\% and 10\% levels respectively for zero.	///	
		\end{footnotesize} ///
		"\end{tablenotes} \end{threeparttable} \end{table}" ) replace
		
	*Pregunta 2b) prediccion de los errores
	*Test de Wald: edad y mujer

	eststo clear
	eststo: reg logr6 rneduca        		,r
	eststo: reg logr6 rneduca rmujer       	,r
	eststo: reg logr6 $Zs       			,r
	
	estadd local Fixed1 "$\surd$",replace
 		  
	esttab using "${Tablas}\T_5_stata.tex", keep(rneduca rmujer redad redadsq) ///
	label booktabs  b(2) se(2) nonumber mtitles("(1)" "(2)" "(3)")  star(* 0.10 ** 0.05 *** 0.01) /*
	*/  varlabels(`e(labels)') /*
	*/ stats(N r2_a Fixed1, layout(@) fmt(a3 a3  a2 a2 ) labels("Observaciones" "R$^2$" "Controls")  ) addnote("Recurso: Exercise 4" "Elaboracion: Autor") prehead("\begin{table}[H] \tiny \centering \begin{threeparttable} \protect \caption{\label{tab:T5} Modelos Lineales Robustos  }  \begin{tabular}{lrrrrrrrr}" \hline \hline) ///
		posthead(\hline) prefoot() postfoot(\hline \end{tabular} ///
		\begin{tablenotes} ///
		\begin{footnotesize} ///
		\item[] Fuente: ENAHO. ///
		\item[] Elaboracion: Autor  ///
		\item[] ***, **, * denote statistical significance at the 1\%, 5\% and 10\% levels respectively for zero.	///	
		\end{footnotesize} ///
		"\end{tablenotes} \end{threeparttable} \end{table}" ) replace

		/// Estimacion de test de wald paso a paso 
		reg logr6 $Zs , r
		*Extraer matrices
		matrix b=e(b) 
		matrix list b
		matrix vb=e(V) 
		matrix list vb

		matrix bi=b[1,7..8] 
		matrix vi=vb[7..8,7..8]
		matrix w_test=bi*inv(vi)*bi'
		matrix list w_test
		
		* Test de wald de stata es la mitad
		test rmujer redad
		
/// ===============================================
/// Prgunta 3
/// ===============================================
	glo Xs "rneduca i.reduca rmujer redad redadsq rinformal"	
	reg logr6 $Xs , r

	/// Edad optima
	scalar edad_optima = _b[redad]/(-2*_b[redadsq])
	display edad_optima

	/// Test no lineal
	nlcom - _b[redad]/(2*_b[redadsq]) - 50		
	gen logr6_predicted=_b[redad]*redad +_b[redadsq]*redadsq
	scatter logr6_predicted redad		
	graph export "${figura}/g4.eps", replace	



	eststo clear
	eststo: reg logr6 rneduca        ,r
	eststo: reg logr6 rneduca rmujer       ,r
	eststo: reg logr6 $Xs       ,r
	estadd local Fixed1 "$\surd$",replace
 		  
	esttab using "${Tablas}\T_6_stata.tex",  label booktabs  b(3) se(3) nonumber keep(rneduca ) mtitles("\ln wage" "\ln wage" "\ln wage") ///
		star(* 0.10 ** 0.05 *** 0.01) ///
	   varlabels(`e(labels)') ///
	 stats(N r2_a Fixed1, layout(@) fmt(a3 a3  a2 a2 ) labels("Observaciones" "R$^2$" "Controls")  ) addnote("Recurso: Exercise 4" "Elaboracion: Autor") prehead("\begin{table}[H] \scriptsize \centering \begin{threeparttable} \protect \caption{\label{tab:T5} Modelos Lineales Robustos  }  \begin{tabular}{lrrrrrrrr}" \hline \hline) ///
		posthead(\hline) prefoot() postfoot(\hline \end{tabular} ///
		\begin{tablenotes} ///
		\begin{footnotesize} ///
		\item[] Fuente: ENAHO - 2021. ///
		\item[] Elaboracion: Autor  ///
		\item[] ***, **, * denote statistical significance at the 1\%, 5\% and 10\% levels respectively for zero.	///	
		\end{footnotesize} ///
		"\end{tablenotes} \end{threeparttable} \end{table}" ) replace

/// ==================================
/// Pregunta 4
/// ==================================
	glo Xs "rneduca i.reduca rmujer redad redadsq rinformal"	
	*OLS INTERACCIONES	
	g i1=rmujer*rneduca
	g i2=rmujer*redad
	g i3=rmujer*redadsq
	g i4=rmujer*rinformal

	label var i1 "educa x mujer"
	reg logr6 $Xs i1 i2 i3 i4

	test i1 
	test i1 i2
	test i1 i2 i3 i4

	reg logr6 $Xs i1 i2 i3 i4
	reg logr6 $Xs
	reg logr6 $Xs if rmujer==1
	reg logr6 $Xs if rmujer==0

* Output latex
	eststo clear
	eststo: reg logr6 $Xs i1 i2 i3 i4, r
	estadd local Fixed1 "$\surd$",replace
	eststo: reg logr6 $Xs 
	estadd local Fixed1 "$\surd$",replace
	eststo: reg logr6 $Xs if rmujer==1
	estadd local Fixed1 "$\surd$",replace
	eststo: reg logr6 $Xs if rmujer==0
	estadd local Fixed1 "$\surd$",replace
 		  
	esttab using "${Tablas}\T_7_stata.tex", label booktabs  b(3) se(3) nonumber keep(rneduca i1) mtitles("Interaccion" "(1)" "Mujer" "Hombre")  star(* 0.10 ** 0.05 *** 0.01) /*
	*/  varlabels(`e(labels)') /*
	*/ stats(N r2_a Fixed1, layout(@) fmt(a3 a3 a2 ) labels("Observaciones" "R$^2$" "Controls")  ) addnote("Recurso: Exercise 4" "Elaboracion: Autor") prehead("\begin{table}[H] \scriptsize \centering \begin{threeparttable} \protect \caption{\label{tab:T7} Modelos Lineales Robustos  }  \begin{tabular}{lrrrrrrrr}" \hline \hline) ///
		posthead(\hline) prefoot() postfoot(\hline \end{tabular} ///
		\begin{tablenotes} ///
		\begin{footnotesize} ///
		\item[] Fuente: ENAHO. ///
		\item[] Elaboracion: Autor  ///
		\item[] ***, **, * denote statistical significance at the 1\%, 5\% and 10\% levels respectively for zero.	///	
		\end{footnotesize} ///
		"\end{tablenotes} \end{threeparttable} \end{table}" ) replace
	