///Dead Ends, Lost Time: Rural Mobility Constraints///
///version: Stata 18.0

*control variables
global X age gender education_year formally_employed temporary_worker self_employed peasant others health household_income household_size household_older household_school car_ownership bike_ownership motor_ownership tricycle_ownership Pop_density Road_density Dis_city Dis_county Dis_highways Dis_localroad Healthcare_service Educational_service Shopping_service Bus_station Precipitation Slope Temperature 

*dependent variables
local Y_list time_of_subsistence time_of_stores time_of_fairs time_of_visits time_of_entertainment


*****************************************************sensitivity analysis1 (SA1)
foreach y in `Y_list' {
  use data.dta, clear  
  preserve
  drop if `y' ==. 

  set seed 123
  ddml init partial, fcluster(code) kfolds(10) reps(5)
  ddml E[Y|X]: pystacked `y' $X || ///
   m(ols)     || ///
   m(lassocv) || ///
   m(svm)     || ///
   m(nnet)    || ///
   m(rf)      || ///
   m(gradboost), ///
   njobs(-1)
  ddml E[D|X]: pystacked dead_density $X || ///
   m(ols)     || ///
   m(lassocv) || ///
   m(svm)     || ///
   m(nnet)    || ///
   m(rf)      || ///
   m(gradboost), ///
   njobs(-1)
  ddml crossfit shortstack
  ddml extract, show(stweights)
  ddml estimate, robust vce(cluster code)
  est store SA1_`y'
      
  parmest, saving("SA1_`y'.dta", replace) star(0.1 0.05 0.01) ///
  format(estimate %8.3f std %8.3f min95 %8.3f max95 %8.3f p %8.3f)
  restore
}
esttab SA1* using "SA1.rtf", replace nogap se n b(3) compress star(* 0.1 ** 0.05 *** 0.01)

*****************************************************sensitivity analysis2 (SA2)
foreach y in `Y_list' {
  use data.dta, clear
  drop if town == "平山街道"
  preserve
  drop if `y' ==. 

  set seed 123
  ddml init partial, fcluster(code) kfolds(5) reps(5)
  ddml E[Y|X]: pystacked `y' $X || ///
   m(ols)     || ///
   m(lassocv) || ///
   m(svm)     || ///
   m(nnet)    || ///
   m(rf)      || ///
   m(gradboost), ///
   njobs(-1)
  ddml E[D|X]: pystacked dead_density $X || ///
   m(ols)     || ///
   m(lassocv) || ///
   m(svm)     || ///
   m(nnet)    || ///
   m(rf)      || ///
   m(gradboost), ///
   njobs(-1)
  ddml crossfit shortstack
  ddml extract, show(stweights)
  ddml estimate, robust vce(cluster code)
  est store SA2_`y'
    
  parmest, saving("SA2_`y'.dta", replace) star(0.1 0.05 0.01) ///
  format(estimate %8.3f std %8.3f min95 %8.3f max95 %8.3f p %8.3f)
  restore
}
esttab SA2* using "SA2.rtf", replace nogap se n b(3) compress star(* 0.1 ** 0.05 *** 0.01)

*****************************************************sensitivity analysis3 (SA3)
foreach y in `Y_list' {
  use data.dta, clear
  winsor2 age education_year Pop_density Road_density Dis_city Dis_county Dis_highways Dis_localroad Healthcare_service Educational_service Shopping_service Bus_station Precipitation Slope Temperature travel_subsistence travel_shopping travel_fairs  travel_visiting travel_leisure_min Dead_density1 D6, cuts(1 99) replace
  preserve
  drop if `y' ==. 

  set seed 123
  ddml init partial, fcluster(code) kfolds(5) reps(5)
  ddml E[Y|X]: pystacked `y' $X || ///
   m(ols)     || ///
   m(lassocv) || ///
   m(svm)     || ///
   m(nnet)    || ///
   m(rf)      || ///
   m(gradboost), ///
   njobs(-1)
  ddml E[D|X]: pystacked dead_density $X || ///
   m(ols)     || ///
   m(lassocv) || ///
   m(svm)     || ///
   m(nnet)    || ///
   m(rf)      || ///
   m(gradboost), ///
   njobs(-1)
  ddml crossfit shortstack
  ddml extract, show(stweights)
  ddml estimate, robust vce(cluster code)
  est store SA3_`y'
  
  parmest, saving("SA3_`y'.dta", replace) star(0.1 0.05 0.01) ///
  format(estimate %8.3f std %8.3f min95 %8.3f max95 %8.3f p %8.3f)  
  restore
}
esttab SA3* using "SA3.rtf", replace nogap se n b(3) compress star(* 0.1 ** 0.05 *** 0.01)

*****************************************************sensitivity analysis4 (SA4)
foreach y in `Y_list' {
  use data.dta, clear
  encode town, gen(town1)
  preserve
  drop if `y' ==. 

  set seed 134
  ddml init partial, fcluster(code) kfolds(5) reps(5)
  ddml E[Y|X]: pystacked `y' $X i.town1 || ///
   m(ols)     || ///
   m(lassocv) || ///
   m(svm)     || ///
   m(nnet)    || ///
   m(rf)      || ///
   m(gradboost), ///
   njobs(-1)
  ddml E[D|X]: pystacked dead_density $X i.town1 || ///
   m(ols)     || ///
   m(lassocv) || ///
   m(svm)     || ///
   m(nnet)    || ///
   m(rf)      || ///
   m(gradboost), ///
   njobs(-1)
  ddml crossfit shortstack
  ddml extract, show(stweights)
  ddml estimate, robust vce(cluster code)
  est store SA4_`y'
  
  parmest, saving("SA4_`y'.dta", replace) star(0.1 0.05 0.01) ///
  format(estimate %8.3f std %8.3f min95 %8.3f max95 %8.3f p %8.3f)
  restore
}
esttab SA4* using "SA4.rtf", replace nogap se n b(3) compress star(* 0.1 ** 0.05 *** 0.01)