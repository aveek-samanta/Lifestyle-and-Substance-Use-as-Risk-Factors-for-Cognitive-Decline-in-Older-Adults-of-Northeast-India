*________________________________________________________________________________________________________________________________________*

*----------------------------------------------------------------------------------------------------------------------------------------*
******************************************************************************************************************************************
* Topic : Examining the risk of substance use and lifestyle factors on cognitive decline among older people in Northeast states of India *
******************************************************************************************************************************************
*----------------------------------------------------------------------------------------------------------------------------------------*

* NAME : Aveek Samanta
* CLASS : MSD
* Enrollment no. : IIPS092025008
* Roll no. : 250906 

*________________________________________________________________________________________________________________________________________*




***************************************************
* Recoding and droping observations of age 18-44  *
***************************************************

use "D:\Nandita_Maam_project\nandita_maam_project_main.dta"
cd "D:\Nandita_Maam_project"

ta ht302
ta dm005

recode dm005 (18/44= 0 "18-44") (45/59 = 1 "Adult") (60/79 = 2 "Older Adult") (80/max = 3 "Oldest adult") , gen(age)
ta age
drop if age==0       //// as we are working with adults and older adults population
ta age

***********************************************************************************
* Encoding and define lebel of a variable if lebel of variable is not defined     *
***********************************************************************************

br ht302
encode ht302, gen(ht302_num)
ta ht302_num
ta ht302_num, nol
recode ht302_num (4/38 = 1 "impairement exists") (else = 0 "no impairement"), gen (imparement)
ta imparement

***************************************************
*                     Caste                       *
***************************************************

ta dm013 , nol
drop if dm013 == . | dm013 == .d |dm013 == .r
ta dm013 , nol
recode dm013 (1/2 = 1 "SC-ST") (3/4 = 2 "non SC-ST"), gen (caste_n)
ta caste_n
codebook hb101
drop if hb101 ==. | hb101== .d | hb101 == .r
codebook hb001
drop if hb001 ==. | hb001== .d | hb001 == .r

***************************************************
*               Physical Activity                 *
***************************************************

drop if hb215 == .|hb215== .d | hb215 == .r

drop if fs507  == .|fs507 == .d | fs507  == .r

recode hb215 (1/2 = 1 "Almost everyday") (3/5 = 2 "Several times in month") (5 = 3 "Almost never"), gen (Physical_activity)
recode fs507 (1/2 = 1 "Almost everyday") (3/5 = 2 "Several times in month") (6/7 = 3 "Almost never"), gen (outdoor_sports)
ta Physical_activity


***************************************************
*          recoding of reading habit              *
***************************************************

ta fs512
codebook fs512
recode fs512 (1/2 = 1 "Almost Daily") (3/5 = 2 "Sometimes") (6/7 = 3 "Almost Never"), gen (reading_habit)


***************************************************
*   recoding of Attend cultural performances      *
***************************************************

recode fs509 (1/4 = 1 "Several times in a month") (5/6 = 2 "Rarely") (7=3 "Never") , gen (attend_programme)

***************************************************
*        Recoding of living arrangement           *
***************************************************

codebook living_arrangements
recode living_arrangements (1= 1 "living alone") (2/5 = 2 "living with someone") , gen (living_arrangement)

***************************************************
*    Recoding and cleaning of marital status      *
***************************************************

codebook dm021
drop if dm021== .r
recode dm021 (1=1 "currently in union") (6=2 "live in relationship") (else=3 "currently not in union"), gen (marital_status)
ta marital_status

***************************************************
*           statewise prevalence for              *
***************************************************
ta state imparement , r nof

***************************************************
*      Only selecting north-east states           *
***************************************************

ta state, nol
codebook state
encode state , gen (state_n)
ta state_n , nol
ta state_n imparement , r nof


*-----------------------------------------------------------------------*
* Checking association b/w Independent variable & cognitive impairement
*-----------------------------------------------------------------------*


tabulate impairement hb101, row chi2
tabulate impairement hb001, row chi2
tabulate impairement ht219, row chi2
tabulate impairement residence, row chi2
tabulate impairement dm003, row chi2
tabulate impairement mpce_quintile, row chi2
tabulate impairement fo231, row chi2
tabulate impairement marital_status, row chi2
tabulate impairement living_arrangement, row chi2
tabulate impairement attend_programme , row chi2
tabulate impairement reading_habit , row chi2
tabulate impairement Physical_activity , row chi2
tabulate impairement caste_n  , row chi2
tabulate impairement age  , row chi2
tabulate impairement dm006 , row chi2



***************************************************
*             Regression Analysis                 *
***************************************************
              //// Adjusted ////
			  
logistic impairement i.age i.caste_n i.Physical_activity i.dm006 i.reading_habit i.attend_programme i.living_arrangement i.marital_status i.fo231 i.mpce_quintile i.dm003 i.ht219 i.hb001 i.hb101 i.residence ,or   

**************************************************
*             Checking AIC & BIC                 *
**************************************************

estat ic

ssc install asdoc
asdoc do "C:\Users\HP\AppData\Local\Temp\STD3478_000000.tmp"

***************************************************
*             Regression Analysis                 *
***************************************************
*-------------Undjusted Odds Ratio----------------*		
			  
logistic impairement  i.hb001 , or
logistic impairement  i.hb101 , or	
logistic impairement  i.ht219 , or	
logistic impairement  i.residence , or
logistic impairement  i.dm003 , or
logistic impairement  i.mpce_quintile , or	
logistic impairement  i.fo231 , or	
logistic impairement  i.marital_status , or
logistic impairement  i.dm006 , or
logistic impairement  i.living_arrangement , or	
logistic impairement  i.attend_programme  , or	
logistic impairement  i.reading_habit , or
logistic impairement  i.Physical_activity , or	
logistic impairement  i.caste_n , or	
logistic impairement  i.age, or
	 
	 
*---------------------------------------------------------*
*     Checking Prevalence for each dependent variable     *
*---------------------------------------------------------*
	  
	  
	  
ta hb001 impairement , r nof  // prevalence of impairement among adults those who smoked or used smokeless tobaco
ta hb101 impairement , r nof // prevalence of impairement among adults those who consumed alcohol
ta ht219 impairement , r nof // prevalence of impairement among adults those who have trouble falling asleep
ta residence impairement , r nof  // prevalence of impairement among adults based on their resident
ta dm003 impairement , r nof // prevalence of impairement among adults based on sex 
ta mpce_quintile impairement , r nof // prevalence of impairement among adults based on their wealth index
ta fo231 impairement , r nof  // prevalence of impairement among adults based on their food habit
ta marital_status impairement , r nof // prevalence of impairement among adults according to their marital_status
ta living_arrangement impairement , r nof // prevalence of impairement among adults according to their living_arrangement
ta attend_programme impairement , r nof  // prevalence of impairement among adults based on frequency of attending cultural programme
ta reading_habit impairement , r nof // prevalence of impairement among adults based on frequency of their reading
ta outdoor_sports impairement , r nof // prevalence of impairement among adults based on frequency of participating in outdoor sports
ta Physical_activity impairement , r nof  // prevalence of impairement among adults based on frequency of participating in physical activity
ta caste_n impairement , r nof // prevalence of impairement among adults according to their caste
ta age impairement , r nof // prevalence of impairement among different age group of adults 



*-----------------------------------------------------*
*                    Margin Graph                     *
*-----------------------------------------------------*

logistic impairement i.age##i.caste_n i.Physical_activity i.dm006 i.reading_habit i.attend_programme i.living_arrangement i.marital_status i.fo231 i.mpce_quintile i.dm003 i.ht219 i.hb001 i.hb101 i.residence


* plotting margin graph of residence according to age *

margins age#residence 

marginsplot, xdimension(age) recast(line) recastci(rarea) title("Cognitive Impairment by Age and residence") ytitle("Predicted probability of Cognitive Impairment") xtitle("Age") legend(label(1 "Rural") label(2 "Urban")) graphregion(color(white)) name(graph1, replace)

* plotting margin graph of residence according to age *

margins age#hb001

marginsplot, xdimension(age) recast(line) recastci(rarea) title("Cognitive Impairment by Age and consumption of tobaco") ytitle("Predicted probability of Cognitive Impairment") xtitle("Age") legend(label(1 "Yes") label(2 "No")) graphregion(color(white)) name(graph2, replace)

* plotting margin graph of residence according to age *

margins age#hb101

marginsplot, xdimension(age) recast(line) recastci(rarea) title("Cognitive Impairment by Age and consumption of Alcohol") ytitle("Predicted probability of Cognitive Impairment") xtitle("Age") legend(label(1 "Yes") label(2 "No")) graphregion(color(white)) name(graph3, replace)

*------- combining 3 graphs together into one --------*

graph combine graph2 graph3, rows(3) cols(1) graphregion(color(white)) ysize(12) xsize(6)

graph export "combined_margins_plot.png", replace width(2400)
graph save "combined_margins_plot.gph", replace






















