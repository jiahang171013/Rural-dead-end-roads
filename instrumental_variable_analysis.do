///Dead Ends, Lost Time: Rural Mobility Constraints///
///version: Stata 18.0

*control variables
global X age gender education_year formally_employed temporary_worker self_employed peasant others health household_income household_size household_older household_school car_ownership bike_ownership motor_ownership tricycle_ownership Pop_density Road_density Dis_city Dis_county Dis_highways Dis_localroad Healthcare_service Educational_service Shopping_service Bus_station Precipitation Slope Temperature 

*dependent variables
local Y_list time_of_stores time_of_fairs time_of_entertainment

*********************************************instrumental variable analysis (IV)
foreach y in `Y_list' {
  use data.dta, clear  
  preserve
  drop if `y' ==. 

  set seed 124
  ddml init iv, fcluster(code) kfolds(5) reps(5)
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
  ddml E[Z|X]: pystacked dead_density_2021 $X || ///
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
  est store IV_`y'
  
  parmest, saving("IV_`y'.dta", replace) star(0.1 0.05 0.01) ///
  format(estimate %8.3f std %8.3f min95 %8.3f max95 %8.3f p %8.3f)
  restore
}
esttab IV* using "IV.rtf", replace nogap se n b(3) compress star(* 0.1 ** 0.05 *** 0.01)