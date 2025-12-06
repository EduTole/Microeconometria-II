cls
clear all
glo main "C:\Users\et396\OneDrive\Dropbox"
glo chapter "${main}\\Docencia\UNAC\Evaluation\S5\Code"

power onemean 0 1, knownsd onesided 
power onemean 0 1,  n(5) knownsd onesided 

///  =======================================================
///  Tamano de muestra
///  =======================================================
power onemean 0 1, knownsd onesided

///  =======================================================
///  Calculo poder
///  =======================================================
power onemean 0 1, n(5) knownsd onesided

///  =======================================================
///  Efecto de tamano 
///  =======================================================
power onemean 0, n(5)  power(0.8) knownsd onesided

///  =======================================================
/// FPC
///  =======================================================
power onemean 0 1, n(5) knownsd fpc(0.1) onesided
/// rango : 0 a 20%
power onemean 0 1, n(5) knownsd fpc(0.0(0.02)0.2) onesided
/// rango : 0 a 20% , grafico
power onemean 0 1, n(5) knownsd fpc(0.0(0.02)0.2) onesided  ///
graph(ydimension(power) xdimension(fpc))

/// ===========================================================================
///  Simulacion 
/// =============================================

// BASIC T-TEST SIMULATION
capture program drop simttest
program simttest, rclass
    version 15.1 
	
    // DEFINE THE INPUT PARAMETERS AND THEIR DEFAULT VALUES
    syntax, n(integer)          ///  Sample size
          [ alpha(real 0.05)    ///  Alpha level
            m0(real 0)          ///  Mean under the null
            ma(real 1)          ///  Mean under the alternative
            sd(real 1) ]        //   Standard deviation

    // GENERATE THE RANDOM DATA AND TEST THE NULL HYPOTHESIS		  
    drawnorm y, n(`n') means(`ma') sds(`sd') clear  
    ttest y = `m0' 
	
    // RETURN RESULTS
    return scalar reject = (r(p)<`alpha') 
end

simttest, n(100) m0(70) ma(75) sd(15) alpha(0.05)

return list

// USE -simulate- TO RUN -simttest- 100 TIMES
simulate reject=r(reject), reps(100) seed(12345): 	///
	simttest, n(100) m0(70) ma(75) sd(15) alpha(0.05)

list in 1/5
summarize reject
local power = r(mean)
display "Power = `power'"


// COMPUTE POWER USING -power onemean-
power onemean 70 75, n(100) sd(15) alpha(0.05)


/// ===========================================================================
/// Simulacion II
/// ===========================================================================

global GraphWidth16x9 = 1200
global GraphHeight16x9 = 720


// COMPUTE POWER USING -power onemean- FOR A 
// RANGE OF SAMPLE SIZES AND GRAPH THE RESULTS
power onemean 70 75, n(50(10)100) sd(15) alpha(0.05)

power onemean 70 75, n(50(10)100) sd(15) alpha(0.05) graph
graph export "${chapter}\power_onemean.png", ///
 as(png) width($GraphWidth16x9) ///
 height($GraphHeight16x9) replace


// BASIC T-TEST SIMULATION (DO NOT INCLUDE IN BLOG 2)
capture program drop simttest
program simttest, rclass
	version 15.1
	syntax,   n(integer)			/// 
			[ alpha(real 0.05) 		///
			  m0(real 0) 			///
			  ma(real 1) 			///
			  sd(real 1) ]
	drawnorm y, n(`n') means(`ma') sds(`sd') clear double 
	quietly ttest y = `m0' 
	return scalar reject = (r(p)<`alpha') 
end

simulate reject=r(reject), reps(100) seed(12345): 	///
	simttest, n(100) m0(70) ma(75) sd(15) alpha(0.05)

summarize reject
local power = r(mean)
display "power = `power'"




capture program drop power_cmd_simttest
program power_cmd_simttest, rclass
	version 15.1
	
	// DEFINE THE INPUT PARAMETERS AND THEIR DEFAULT VALUES
    syntax, n(integer)          ///  Sample size
          [ alpha(real 0.05)    ///  Alpha level
            m0(real 0)          ///  Mean under the null
            ma(real 1)          ///  Mean under the alternative
            sd(real 1)          ///  Standard deviation
            reps(integer 100)]  //   Number of repetitions
			  
	// GENERATE THE RANDOM DATA AND TEST THE NULL HYPOTHESIS
	quietly simulate reject=r(reject), reps(`reps'): 	///
			simttest, n(`n') m0(`m0') ma(`ma') sd(`sd') alpha(`alpha')
	quietly summarize reject
	
	// RETURN RESULTS
	return scalar power = r(mean)
	return scalar N = `n'
	return scalar alpha = `alpha'
	return scalar m0 = `m0'
	return scalar ma = `ma'
	return scalar sd = `sd'
end

power simttest, n(100) m0(70) ma(75) sd(15)
power simttest, n(50(10)100) m0(70) ma(75) sd(15) table graph

graph export "${chapter}\simttest1.png", ///
as(png) width($GraphWidth16x9) ///
height($GraphHeight16x9) replace


capture program drop power_cmd_simttest_init
program power_cmd_simttest_init, sclass
	// ADD COLUMNS TO THE OUTPUT TABLE
	sreturn local pss_colnames "m0 ma sd"
	// ALLOW NUMLISTS FOR m0, ma, and sd
	sreturn local pss_numopts  "m0 ma sd"
end

power simttest, n(75 100) m0(70) ma(72(1)75) sd(15) reps(1000)
power simttest, n(75 100) m0(70) ma(72(1)75) sd(15) reps(1000) graph(xdimension(ma))
graph export "${chapter}\simttest2.png", ///
as(png) width($GraphWidth16x9) ///
height($GraphHeight16x9) ///
replace


/// ===========================================================================
/// Regresion lineal 
/// ===========================================================================

global GraphWidth16x9 = 1200
global GraphHeight16x9 = 720

// IDENTIFY REASONABLE PARAMETER VALUES
webuse nhanes2, clear
//regress bpsystol age ib1.sex height weight
regress bpsystol c.age##ib1.sex 
logistic highbp c.age##ib1.sex 


set seed 15
clear
set obs 100
generate age = runiformint(18,65)
generate female = rbinomial(1,0.5)
generate interact = age*female
generate e = rnormal(0,20)

generate sbp = 110 + 0.5*age + (-20)*female + 0.35*interact  + e

regress sbp age i.female c.age#i.female
estimates store full
quietly regress sbp age i.female
estimates store reduced
lrtest full reduced 
return list
local reject = (r(p)<0.05)

display "reject = `reject'"



/// ===========================================
/// BASIC -simregress- COMMAND
/// ===========================================
capture program drop simregress
program simregress, rclass
	version 15
	// PARSE INPUT
	syntax, n(integer)			/// 
		  [ alpha(real 0.05) 	///
			intercept(real 110)	///
			age(real 0.5) 		///
			female(real -20) 	///
			interact(real 0.35)	///
			esd(real 20) ]
	// COMPUTE POWER
	quietly {
		drop _all
		set obs `n'
		generate age = runiformint(18,65)
		generate female = rbinomial(1,0.5)
		generate interact = age*female
		generate e = rnormal(0,`esd')

		generate sbp = `intercept' + `age'*age + `female'*female + `interact'*interact  + e

		regress sbp age i.female c.age#i.female
		estimates store full
		regress sbp age i.female
		estimates store reduced
		lrtest full reduced
	}
	/// RETURN RESULTS
	return scalar reject = (r(p)<`alpha')
end

simregress, n(100) age(0.5) female(20) interact(0.35) esd(20) alpha(0.05)



simulate reject=r(reject), reps(100): 	///
	simregress, n(100) age(0.5) female(20) interact(0.35) esd(20) alpha(0.05)

summarize reject




///  ========================================================
/// Regresion Multivariada 
/// ====================================================
capture program drop power_cmd_simregress
program power_cmd_simregress, rclass
	version 15
	// PARSE INPUT
	syntax, n(integer)			/// 
		  [	alpha(real 0.05) 	///
			intercept(real 110)	///
			age(real 0.5) 		///
			female(real -20) 	///
			interact(real 0.35)	///
			esd(real 20) 		///
			reps(integer 100) ]
	// COMPUTE POWER
	quietly {
		simulate reject=r(reject), reps(`reps'): 	    ///
		simregress, n(`n') age(`age') female(`female')  ///
		            interact(`interact') esd(`esd') alpha(`alpha')
		summarize reject
	}

	// RETURN RESULTS
	return scalar power 	= r(mean)
	return scalar N 		= `n'
	return scalar alpha 	= `alpha'
	return scalar intercept = `intercept'
	return scalar age 		= `age'
	return scalar female 	= `female'
	return scalar interact 	= `interact'
	return scalar esd 		= `esd'
end


// ADD PARAMETER FLEXIBILITY TO -power simregress-
// ==========================================================================
* pss_colnames: define los nombres de las columnas que aparecerán en los resultados.
* pss_numopts: define qué opciones se tratan como valores numéricos a simular.
capture program drop power_cmd_simregress_init
program power_cmd_simregress_init, sclass
	sreturn local pss_colnames "intercept age female interact esd"
	sreturn local pss_numopts  "intercept age female interact esd"
end


power simregress, n(400(100)700) intercept(110) age(0.5) female(-20) interact(0.2(0.05)0.4) ///
                  reps(1000) table  ///
				  graph(xdimension(interact) legend(rows(1))) 

graph export "${chapter}\simregress2.png", ///
 as(png) width($GraphWidth16x9) ///
 height($GraphHeight16x9) replace