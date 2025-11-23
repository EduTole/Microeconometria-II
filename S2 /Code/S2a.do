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
9
	*** Efectos marginales
	quietly tobit rgsd $x , ll(0)
	margins, dydx(*) predict(e(0,.))
	margins, dydx(*) predict(e(0,.)) atmeans

	



