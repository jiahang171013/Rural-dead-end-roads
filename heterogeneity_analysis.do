///Dead Ends, Lost Time: Rural Mobility Constraints///
///version: Stata 18.0

*dependent variables
local Y_list time_of_subsistence time_of_stores time_of_fairs time_of_visits time_of_entertainment

**************************************************Heterogeneity analysis1 (age1)
foreach y in `Y_list' {
  use data.dta, clear
  global X gender education_year formally_employed temporary_worker self_employed peasant others health household_income household_size household_older household_school car_ownership bike_ownership motor_ownership tricycle_ownership Pop_density Road_density Dis_city Dis_county Dis_highways Dis_localroad Healthcare_service Educational_service Shopping_service Bus_station Precipitation Slope Temperature  
  preserve
  drop if `y' ==.
  keep if age <= 44

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
  est store age1_`y'
  
  parmest, saving("age1_`y'.dta", replace) star(0.1 0.05 0.01) ///
  format(estimate %8.3f std %8.3f min95 %8.3f max95 %8.3f p %8.3f)
  restore
}
esttab age1* using "age1.rtf", replace nogap se n b(3) compress star(* 0.1 ** 0.05 *** 0.01)

**************************************************Heterogeneity analysis1 (age2)
foreach y in `Y_list' {
  use data.dta, clear
  global X gender education_year formally_employed temporary_worker self_employed peasant others health household_income household_size household_older household_school car_ownership bike_ownership motor_ownership tricycle_ownership Pop_density Road_density Dis_city Dis_county Dis_highways Dis_localroad Healthcare_service Educational_service Shopping_service Bus_station Precipitation Slope Temperature  
  preserve
  drop if `y' ==.
  keep if age >= 44 & age <= 59

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
  est store age2_`y'
  
  parmest, saving("age2_`y'.dta", replace) star(0.1 0.05 0.01) ///
  format(estimate %8.3f std %8.3f min95 %8.3f max95 %8.3f p %8.3f)
  restore
}
esttab age2* using "age2.rtf", replace nogap se n b(3) compress star(* 0.1 ** 0.05 *** 0.01)

**************************************************Heterogeneity analysis1 (age3)
foreach y in `Y_list' {
  use data.dta, clear
  global X gender education_year formally_employed temporary_worker self_employed peasant others health household_income household_size household_older household_school car_ownership bike_ownership motor_ownership tricycle_ownership Pop_density Road_density Dis_city Dis_county Dis_highways Dis_localroad Healthcare_service Educational_service Shopping_service Bus_station Precipitation Slope Temperature  
  preserve
  drop if `y' ==.
  keep if age >= 60

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
  est store age3_`y'
  
  parmest, saving("age3_`y'.dta", replace) star(0.1 0.05 0.01) ///
  format(estimate %8.3f std %8.3f min95 %8.3f max95 %8.3f p %8.3f)
  restore
}
esttab age3* using "age3.rtf", replace nogap se n b(3) compress star(* 0.1 ** 0.05 *** 0.01)

**************************************************Heterogeneity analysis1 (male)
foreach y in `Y_list' {
  use data.dta, clear
  global X age education_year formally_employed temporary_worker self_employed peasant others health household_income household_size household_older household_school car_ownership bike_ownership motor_ownership tricycle_ownership Pop_density Road_density Dis_city Dis_county Dis_highways Dis_localroad Healthcare_service Educational_service Shopping_service Bus_station Precipitation Slope Temperature   
  preserve
  drop if `y' ==.
  keep if gender == 1

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
  est store male_`y'
  
  parmest, saving("male_`y'.dta", replace) star(0.1 0.05 0.01) ///
  format(estimate %8.3f std %8.3f min95 %8.3f max95 %8.3f p %8.3f)
  restore
}
esttab male* using "male.rtf", replace nogap se n b(3) compress star(* 0.1 ** 0.05 *** 0.01)

************************************************Heterogeneity analysis1 (female)
foreach y in `Y_list' {
  use data.dta, clear
  global X age education_year formally_employed temporary_worker self_employed peasant others health household_income household_size household_older household_school car_ownership bike_ownership motor_ownership tricycle_ownership Pop_density Road_density Dis_city Dis_county Dis_highways Dis_localroad Healthcare_service Educational_service Shopping_service Bus_station Precipitation Slope Temperature  
  preserve
  drop if `y' ==.
  keep if gender == 0

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
  est store female_`y'
  
  parmest, saving("female_`y'.dta", replace) star(0.1 0.05 0.01) ///
  format(estimate %8.3f std %8.3f min95 %8.3f max95 %8.3f p %8.3f)
  restore
}
esttab female* using "female.rtf", replace nogap se n b(3) compress star(* 0.1 ** 0.05 *** 0.01)

***********************************************Heterogeneity analysis3 (income1)
foreach y in `Y_list' {
  use data.dta, clear
  global X age gender education_year formally_employed temporary_worker self_employed peasant others health household_size household_older household_school car_ownership bike_ownership motor_ownership tricycle_ownership Pop_density Road_density Dis_city Dis_county Dis_highways Dis_localroad Healthcare_service Educational_service Shopping_service Bus_station Precipitation Slope Temperature   
  preserve
  drop if `y' ==.
  keep if household_income == 1 | household_income == 2

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
  est store income1_`y'
  
  parmest, saving("income1_`y'.dta", replace) star(0.1 0.05 0.01) ///
  format(estimate %8.3f std %8.3f min95 %8.3f max95 %8.3f p %8.3f)
  restore
}
esttab income1* using "income1.rtf", replace nogap se n b(3) compress star(* 0.1 ** 0.05 *** 0.01)

***********************************************Heterogeneity analysis3 (income2)
foreach y in `Y_list' {
  use data.dta, clear
  global X age gender education_year formally_employed temporary_worker self_employed peasant others health household_size household_older household_school car_ownership bike_ownership motor_ownership tricycle_ownership Pop_density Road_density Dis_city Dis_county Dis_highways Dis_localroad Healthcare_service Educational_service Shopping_service Bus_station Precipitation Slope Temperature    
  preserve
  drop if `y' ==.
  keep if household_income == 3

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
  est store income2_`y'
  
  parmest, saving("income2_`y'.dta", replace) star(0.1 0.05 0.01) ///
  format(estimate %8.3f std %8.3f min95 %8.3f max95 %8.3f p %8.3f)
  restore
}
esttab income2* using "income2.rtf", replace nogap se n b(3) compress star(* 0.1 ** 0.05 *** 0.01)

***********************************************Heterogeneity analysis3 (income3)
foreach y in `Y_list' {
  use data.dta, clear
  global X age gender education_year formally_employed temporary_worker self_employed peasant others health household_size household_older household_school car_ownership bike_ownership motor_ownership tricycle_ownership Pop_density Road_density Dis_city Dis_county Dis_highways Dis_localroad Healthcare_service Educational_service Shopping_service Bus_station Precipitation Slope Temperature   
  preserve
  drop if `y' ==.
  keep if household_income == 4

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
  est store income3_`y'
  
  parmest, saving("income3_`y'.dta", replace) star(0.1 0.05 0.01) ///
  format(estimate %8.3f std %8.3f min95 %8.3f max95 %8.3f p %8.3f)
  restore
}
esttab income3* using "income3.rtf", replace nogap se n b(3) compress star(* 0.1 ** 0.05 *** 0.01)

***********************************************Heterogeneity analysis3 (income4)
foreach y in `Y_list' {
  use data.dta, clear
  global X age gender education_year formally_employed temporary_worker self_employed peasant others health household_size household_older household_school car_ownership bike_ownership motor_ownership tricycle_ownership Pop_density Road_density Dis_city Dis_county Dis_highways Dis_localroad Healthcare_service Educational_service Shopping_service Bus_station Precipitation Slope Temperature   
  preserve
  drop if `y' ==.
  keep if household_income == 5

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
  est store income4_`y'
  
  parmest, saving("income4_`y'.dta", replace) star(0.1 0.05 0.01) ///
  format(estimate %8.3f std %8.3f min95 %8.3f max95 %8.3f p %8.3f)
  restore
}
esttab income4* using "income4.rtf", replace nogap se n b(3) compress star(* 0.1 ** 0.05 *** 0.01)

*******************************************Heterogeneity analysis4 (employment1)
foreach y in `Y_list' {
  use data.dta, clear
  global X age gender education_year health household_income household_size household_older household_school car_ownership bike_ownership motor_ownership tricycle_ownership Pop_density Road_density Dis_city Dis_county Dis_highways Dis_localroad Healthcare_service Educational_service Shopping_service Bus_station Precipitation Slope Temperature   
  preserve
  drop if `y' ==.
  keep if employment == 1

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
  est store em1_`y'
  
  parmest, saving("em1_`y'.dta", replace) star(0.1 0.05 0.01) ///
  format(estimate %8.3f std %8.3f min95 %8.3f max95 %8.3f p %8.3f)
  restore
}
esttab em1* using "em1.rtf", replace nogap se n b(3) compress star(* 0.1 ** 0.05 *** 0.01)

*******************************************Heterogeneity analysis4 (employment2)
foreach y in `Y_list' {
  use data.dta, clear
  global X age gender education_year health household_income household_size household_older household_school car_ownership bike_ownership motor_ownership tricycle_ownership Pop_density Road_density Dis_city Dis_county Dis_highways Dis_localroad Healthcare_service Educational_service Shopping_service Bus_station Precipitation Slope Temperature   
  preserve
  drop if `y' ==.
  keep if employment == 2

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
  est store em2_`y'
  
  parmest, saving("em2_`y'.dta", replace) star(0.1 0.05 0.01) ///
  format(estimate %8.3f std %8.3f min95 %8.3f max95 %8.3f p %8.3f)
  restore
}
esttab em2* using "em2.rtf", replace nogap se n b(3) compress star(* 0.1 ** 0.05 *** 0.01)

*******************************************Heterogeneity analysis4 (employment3)
foreach y in `Y_list' {
  use data.dta, clear
  global X age gender education_year health household_income household_size household_older household_school car_ownership bike_ownership motor_ownership tricycle_ownership Pop_density Road_density Dis_city Dis_county Dis_highways Dis_localroad Healthcare_service Educational_service Shopping_service Bus_station Precipitation Slope Temperature   
  preserve
  drop if `y' ==.
  keep if employment == 3

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
  est store em3_`y'
  
  parmest, saving("em3_`y'.dta", replace) star(0.1 0.05 0.01) ///
  format(estimate %8.3f std %8.3f min95 %8.3f max95 %8.3f p %8.3f)
  restore
}
esttab em3* using "em3.rtf", replace nogap se n b(3) compress star(* 0.1 ** 0.05 *** 0.01)

*******************************************Heterogeneity analysis4 (employment4)
foreach y in `Y_list' {
  use data.dta, clear
  global X age gender education_year health household_income household_size household_older household_school car_ownership bike_ownership motor_ownership tricycle_ownership Pop_density Road_density Dis_city Dis_county Dis_highways Dis_localroad Healthcare_service Educational_service Shopping_service Bus_station Precipitation Slope Temperature  
  preserve
  drop if `y' ==.
  keep if employment == 4

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
  est store em4_`y'
  
  parmest, saving("em4_`y'.dta", replace) star(0.1 0.05 0.01) ///
  format(estimate %8.3f std %8.3f min95 %8.3f max95 %8.3f p %8.3f)
  restore
}
esttab em4* using "em4.rtf", replace nogap se n b(3) compress star(* 0.1 ** 0.05 *** 0.01)

*******************************************Heterogeneity analysis4 (employment5)
foreach y in `Y_list' {
  use data.dta, clear
  global X age gender education_year health household_income household_size household_older household_school car_ownership bike_ownership motor_ownership tricycle_ownership Pop_density Road_density Dis_city Dis_county Dis_highways Dis_localroad Healthcare_service Educational_service Shopping_service Bus_station Precipitation Slope Temperature   
  preserve
  drop if `y' ==.
  keep if employment == 5

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
  est store em5_`y'
  
  parmest, saving("em5_`y'.dta", replace) star(0.1 0.05 0.01) ///
  format(estimate %8.3f std %8.3f min95 %8.3f max95 %8.3f p %8.3f)
  restore
}
esttab em5* using "em5.rtf", replace nogap se n b(3) compress star(* 0.1 ** 0.05 *** 0.01)