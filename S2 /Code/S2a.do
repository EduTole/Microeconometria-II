/* **************************************************************************
Institucion:					
Autor:							Edinson Tolentino
Proyecto:						Union de bases:
								100
								500
								Sumaria 
Fecha Ultima Modificacion:		2022
*****************************************************************************/
	cls
	clear all
	set more off

	//Set pathways
	*glo path "D:\Dropbox\BASES\ENAHO" // ET
	glo main 	"C:\Users\et396\OneDrive\Dropbox\Docencia\UNAC\Evaluation\S2"
	glo clean 	"${main}/Aplicacion"
	**********************************************************************
	cls
	cd "C:\Users\et396\OneDrive\Dropbox\Docencia\UNAC\Evaluation\S2"

	u "${clean}/BD2a.dta",clear
	d

	***************************
	*	Análisis estadístico  *
	***************************
	sum rgsd, d
	summ rgsd redad rsexo rneduca renfermedad rly 
	summ rgsd redad rsexo rneduca renfermedad rly if rgsd>0
	
	*tabstat rneduca if rgsd>0, c(mean rgsd) row
	tabstat rgsd , by(rneduca) stat(mean)
	tabstat rgsd if rgsd>0, by(rneduca) stat(mean)
	*table rsexo if rgsd>0, 	c(mean rgsd) row
	tabstat rgsd , by(rsexo) stat(mean)
	tabstat rgsd if rgsd>0, by(rsexo) stat(mean)

	***************************
	*	Análisis económetrico  *
	***************************

	global x "c.redad i.rneduca i.rsexo i.renfermedad c.rly" 
	*global x "c.redad i.rneduca i.rsexo i.renfermedad c.rly"
	reg rgsd $x
	estimates store m_mco
	tobit rgsd $x , ll(0)
	estimates store m_censura
	estimates table m_mco m_censura, star varlabel b(%5.4f) equations(1)
	
	* ========================================
	* Prediccion 
	* ========================================
	tobit rgsd $x , ll(0)
	predict zb
	su zb

	display sqrt(6550.569)
	display -85.66653/(sqrt(6550.569))
	display normalden(-1.058)
	* 0.22795
	* La probabilidad : indica el porcentaje de gasto nulo (gasto-0)
	* es de 23 %
	
	*** Efectos marginales
	quietly tobit rgsd $x , ll(0)
	* Estos efectos marginales son condicionales a tener una censura
	* esto debido a que se esta considerando la escala de la distribucion probit
	margins, dydx(*) predict(e(0,.))
	margins, dydx(*) predict(e(0,.)) atmeans

	///Para calcular los efectos marginal no condicionales a la censura , usamos
	* la version 8 de STATA dtobit , la cual considera XB / sigma
	tab rneduca, g(rnivel)
	glo Xs "redad rnivel2 rnivel3 rsexo renfermedad rly"
	vers 8.0
	tobit rgsd $Xs , ll(0)
	dtobit
	mat M=(52.70, 0.35, 0.22, 0.27,0.55,6.25,-162)
	dtobit, at(M)



