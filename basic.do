/// 20250101
cd "D:\research\2024\Rural road and travel behavior\result\ddml\basic"

global X age gender education_year formally_employed temporary_worker self_employed peasant others health household_income household_size household_older household_school car_ownership bike_ownership motor_ownership tricycle_ownership Pop_density Road_density Dis_city Dis_county Dis_highways Dis_localroad Healthcare_service Educational_service Shopping_service Bus_station Precipitation Slope Temperature 

local Y_list travel_subsistence travel_shopping travel_fairs travel_visiting travel_leisure_min


*******************************************************************Dead_density1
foreach y in `Y_list' {
  use "D:\research\2024\Rural road and travel behavior\data\descriptive_statistics\data.dta", clear
  rename Dead_density1 D5
  
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
  ddml E[D|X]: pystacked D5 $X || ///
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
  est store D5_`y'

  parmest, saving("D5_`y'.dta", replace) star(0.1 0.05 0.01) ///
  format(estimate %8.3f std %8.3f min95 %8.3f max95 %8.3f p %8.3f)
  restore
}
esttab D5* using "D5.rtf", replace nogap se n b(3) compress star(* 0.1 ** 0.05 *** 0.01)

*******************************************************************Dead_density2
foreach y in `Y_list' {
  use "D:\research\2024\Rural road and travel behavior\data\descriptive_statistics\data.dta", clear
  rename Dead_density2 D6  
  
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
  ddml E[D|X]: pystacked D6 $X || ///
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
  est store D6_`y'
  
  parmest, saving("D6_`y'.dta", replace) star(0.1 0.05 0.01) ///
  format(estimate %8.3f std %8.3f min95 %8.3f max95 %8.3f p %8.3f)
  restore
}
esttab D6* using "D6.rtf", replace nogap se n b(3) compress star(* 0.1 ** 0.05 *** 0.01)


