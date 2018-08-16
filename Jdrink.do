clear all
set more off
set graphics off
set scheme s1mono

cd "C:\Users\Daiji\Documents\GitHub\alcohol\analysis" /*Daiji laptop*/
*cd "E:\Dropbox\Alcohol Patch Project\Data_Analysis"
*cd "C:\Users\Jungmin Lee\Dropbox\Alcohol Patch Project\Data_Analysis"
*cd "C:\Users\jmlee\Dropbox\Alcohol Patch Project\Data_Analysis"
*cd "C:\Users\user\Dropbox\Alcohol Patch Project\Data_Analysis"			/* Jungmin laptop */
*cd "C:\Users\izumi\Dropbox\Alcohol Patch Project\Data_Analysis"         /* Izumi's laptop */


global outpath="C:\Users\Daiji\Documents\GitHub\alcohol\draft\figures&tables" /*Daiji laptop*/
*global outpath="C:\Users\kawag\Dropbox\Alcohol Patch Project\LaTex3\figures&tables"
*global outpath="C:\Users\user\Dropbox\Alcohol Patch Project\LaTex3\figures&tables"		/* Jungmin laptop */
*global outpath="C:\Users\izumi\Dropbox\Alcohol Patch Project\LaTex3\figures&tables_20Jul_IY"		/* Izumi's laptop */

local control="age age2 college married single fdrink mdrink fcoll mcoll"

cap log close
log using Janalysis.log, replace

* Survey period : February 2017 

use Jdata, clear

* Demographic variables

label var age "Age"
label var age2 "Age squared"
rename educ edu
label var edu "Education completed 1-7 (junior high to graduate)"
g college = (edu >= 4)
g married = (marriage == 1)
g single = (marriage == 4)
replace health = 6 - health
label var health "Health 1-5 (good)"
rename area_current region
g stress = (effect_on_stress <= 2) if effect_on_stress ~= .
label var stress "Relieve stress"
g coworker = Num_coworkers
label var coworker "# coworkers"
g lncoworker = ln(coworker)
g interperson = (interpersonal_skill <= 2) if interpersonal_skill ~= .
label var interperson "Interpersonal skills"
g norelig = (religion == 1) if religion ~= .
label var norelig "Atheism"
g fdrink = fath_drink
replace fdrink = . if fdrink == 3
replace fdrink = 2 - fdrink
label var fdrink "Father drink"
g mdrink = moth_drink
replace mdrink = . if mdrink == 3
replace mdrink = 2 - mdrink
label var mdrink "Mother drink"
rename sleep_hours sleep
rename borrow_money borrow 
replace smoke = (smoke <= 2) if smoke ~= .

g fcoll = (fath_educ >= 5) 
g mcoll = (moth_educ >= 5)
replace fcoll = . if fath_educ >= 8
replace mcoll = . if moth_educ >= 8
label var fcoll "Father college"
label var mcoll "Mother college"

* Patch test result

su patch1 patch2 patch3
tab patch1
tab patch2
tab patch3

tab patch3 patch2 if patch1 == 4
tab patch3 patch1 if patch2 == 5

g strong = (patch1 == 3 & patch2 == 4 & patch3 == 0)

g miss_patch1 = (patch1 == 4)
*replace patch1 = . if patch1 == 4	/* 47 obs */

label var patch2 "Skin color change 1-4 (no change)"
g miss_patch2 = (patch2 == 5)
*replace patch2 = . if patch2 == 5	/* 4 obs */

replace patch3 = 10 - patch3
label var patch3 "Skin color change 0-10 (no change)"

g dpatch3 = 1 if patch3 <= 3
replace dpatch3 = 2 if patch3 >= 4 & patch3 <= 6
replace dpatch3 = 3 if patch3 >= 7 & patch3 <= 10

* Drinking

rename drink_days_lastweek drink_lastweek
label var drink_lastweek "Days drink last week 0-7"
rename drink_days_usually drink_usual
label var drink_usual "Days drink usually 0-7"
rename binge_drinker binge 
label var binge "5 or more drinks in 2 hrs usually"

foreach ml of varlist beer_ml happo_ml sours_ml sake_ml shochu_ml whiskey_ml wine_ml ///
awamori_ml umeshu_ml coctails_ml highballs_ml korean_drinks_ml chinese_drinks_ml other_drinks_ml {
replace `ml'=0 if `ml'==. 
}
g alcohol = beer*beer_ml*0.05 + happo*happo_ml*0.05 + sours*sours_ml*0.05 ///
+ sake*sake_ml*0.15 + shochu*shochu_ml*0.25 + whiskey*whiskey_ml*0.4 ///
+ wine*wine_ml*0.12 + awamori*awamori_ml*0.3 + umeshu*umeshu_ml*0.1 ///
+ coctails*coctails_ml*0.135 + highballs*highballs_ml*0.07 ///
+ korean_drinks*korean_drinks_ml*0.06 +	chinese_drinks*chinese_drinks_ml*0.16
replace alcohol = . if other_drinks == 1
replace alcohol = 0 if none ~= 0
label var alcohol "Amount of alcohol on average day (ml)"

g beer_alcohol = beer*beer_ml*0.05

foreach ml of varlist beer_ml2 happo_ml2 sours_ml2 sake_ml2 shochu_ml2 whiskey_ml2 wine_ml2 ///
awamori_ml2 umeshu_ml2 coctails_ml2 highballs_ml2 korean_drinks_ml2 chinese_drinks_ml2 other_drinks_ml2 {
replace `ml' = 0 if `ml' == . 
}
g alcohol_heavy = beer2*beer_ml2*0.05 + happo2*happo_ml2*0.05 + sours2*sours_ml2*0.05 ///
+ sake2*sake_ml2*0.15 + shochu2*shochu_ml2*0.25 + whiskey2*whiskey_ml2*0.4 ///
+ wine2*wine_ml2*0.12 + awamori2*awamori_ml2*0.3 + umeshu2*umeshu_ml2*0.1 ///
+ coctails2*coctails_ml2*0.135 + highballs2*highballs_ml2*0.07 ///
+ korean_drinks2*korean_drinks_ml2*0.06 + chinese_drinks2*chinese_drinks_ml2*0.16
replace alcohol_heavy = alcohol_heavy/0.8
replace alcohol_heavy = . if other_drinks2 == 1
replace alcohol_heavy = 0 if none2 ~= 0
label var alcohol_heavy "Amount of alcohol on heavy drinking day (ml)"
g sober = (drink_usual == 0)
label var sober "No drinking usually"

g alcohol_litre = (beer_ml+happo_ml+sours_ml+sake_ml+shochu_ml+whiskey_ml+wine_ml+awamori_ml+umeshu_ml+coctails_ml+highballs_ml+korean_drinks_ml+chinese_drinks_ml+other_drinks_ml)/1000

su drink_usual drink_lastweek alcohol alcohol_heavy beer_alcohol

g ln_alcohol=ln(alcohol+1)

* Parents' ALDH2 types 

rename fath_alcohol_type ftype
rename moth_alcohol_type mtype
label var ftype "Father's type"
label var mtype "Mother's type"
g miss_ftype = (ftype == 4)
g miss_mtype = (mtype == 4)
replace ftype = . if miss_ftype == 1
replace mtype = . if miss_mtype == 1
replace ftype = . if ftype == 4
replace ftype = 0 if ftype == 1 | ftype == 2
replace ftype = 1 if ftype == 3
replace mtype = . if mtype == 4
replace mtype = 0 if mtype == 1 | mtype == 2
replace mtype = 1 if mtype == 3

* Economic outcomes

*hist lnincome		/* outliers: drop if lnincome < 2 */
drop if lnIncome < 2 

replace income = income*10
replace income = income/12
ge lnincome = ln(income)
label var lnincome "log monthly income (1,000 Yen)"
g hwage = income/(hours*4.3)
g lnhwage = ln(hwage)
label var hours "Weekly working hours"
g work_lastweek = (work_status == 1)
label var work_lastweek "Worked last week" 
g rest_lastweek = (work_status == 2)
label var rest_lastweek "Rest last week"
g unemp_lastweek = (work_status == 3)
label var unemp_lastweek "Unemployed"
g lnhours = ln(hours)
g wagesalary = (type_emp == 5)

* Attitudes toward drinking

g network = effect_of_drinking1
g communicate = (effect_of_drinking2 == 1 | effect_of_drinking3 == 1)
g business = effect_of_drinking4
g physical = effect_of_drinking5
g mental = effect_of_drinking7

g physical_bad = effect_of_drinking6
g mental_bad = effect_of_drinking8
g timewaste = effect_of_drinking9
g badresult = (result_of_drinking2 + result_of_drinking3 + result_of_drinking4 + result_of_drinking5 + result_of_drinking6 >= 2)

g good = stress + network + communicate + business + physical + mental
g bad = physical_bad + mental_bad + timewaste + badresult

*negative impact of drinking

g late = result_of_drinking2
g absent =  result_of_drinking3
g unproductive =  result_of_drinking3
g attitude = result_of_drinking4
g chronic = result_of_drinking5

label var late "late for work because of hangover"
label var absent "absent from work because of hangover"
label var unproductive "unproductive because of hangover"
label var attitude "negative attitude because of hangover"
label var chronic "chronically negative impact"

******************************************************************

keep if work_lastweek == 1
keep if lnhwage ~= .
qui reg drink_lastweek patch1 `control'
keep if e(sample)==1

g Patch1=patch1
tab Patch1
replace Patch1=3 if Patch1==4
tab Patch1
* Dichotomize
replace patch1 = (patch1 == 3 | patch1 ==4) 
label var patch1 "Patch test"
lab def d1 0 "Weak type" 1 "Strong type"
lab val patch1 d1

replace patch2 = (patch2 == 3 | patch2 == 4 | patch2 == 5) 
lab val patch2 d1

corr patch1 patch2


*salary wage workers
tab wagesalary

keep if wagesalary == 1

tab patch1
tab Patch1

*association with parental type
tab patch1 ftype
tab patch1 mtype

gen fmtype=ftype*mtype
reg patch1 ftype mtype fmtype, robust
test ftype mtype fmtype


tabstat patch1 if ftype==0 & mtype==0, s(mean sd) format(%9.3f)
tabstat patch1 if ftype==1 & mtype==0, s(mean sd) format(%9.3f)
tabstat patch1 if ftype==0 & mtype==1, s(mean sd) format(%9.3f)
tabstat patch1 if ftype==1 & mtype==1, s(mean sd) format(%9.3f)

sum patch1 if ftype==0 & mtype==0
sum patch1 if ftype==1 & mtype==0
sum patch1 if ftype==0 & mtype==1
sum patch1 if ftype==1 & mtype==1

count if ftype==0 & mtype==0
count if ftype==1 & mtype==0
count if ftype==0 & mtype==1
count if ftype==1 & mtype==1

tab patch1 ftype, chi2
tab patch1 mtype, chi2
tab ftype mtype, sum(patch1)



save Jregsample, replace

* Figures

g n = 1
tab Patch1
collapse (sum) n (mean) alcohol, by(Patch1)
g frac = n/1392
lab def tt 1 "Intolerent (Weak)" 2 "Sensitive (Weak)" 3 "Tolerent (Strong)"
lab val Patch1 tt 
gr bar frac, over(Patch1) ytit("Fraction of respondents (N = 1,392)") blabel(total, format(%9.3f)) saving(J_type.gph, replace) xsize(4) ysize(4) ylabel(0.2 0.4 0.6) 
graph export "$outpath\J_type.pdf", replace

gr bar (mean) alcohol, over(Patch1) ytit("Fraction of respondents (N = 1,392)") blabel(total, format(%9.3f)) saving(J_alcohol.gph, replace) 
graph export "$outpath\J_alcohol.pdf", replace

use Jregsample, clear

lab def tt 1 "Intolerent (Weak)" 2 "Sensitive (Weak)" 3 "Tolerent (Strong)"
lab val Patch1 tt 
graph pie Patch1, over(Patch1) pie(1, color(white)) pie(2, color(gs14)) pie(3, color(gs10)) line(lcolor(black) lwidth(thin)) plabel(1 percent, format(%9.1f)) plabel(2 percent, format(%9.1f)) plabel(3 percent, format(%9.1f)) note("N = 1,392") title("Japan") saving(J_Type.gph, replace) xsize(4) ysize(4)

graph export "$outpath\J_Type.pdf", replace


graph bar (count), saving(J_frequency.gph, replace) over(patch1) blabel(bar) xsize(4) ysize(4)
graph export "$outpath\J_frequency.pdf", replace

graph combine J_Type.gph K_Type.gph, row(1) saving(Types.gph, replace) xsize(8) ysize(4)
graph export "$outpath\Types.pdf", replace


gr bar drink_usual, over(patch1) saving(J_drink_usual.gph, replace) ytit("") tit("Drinking days of a typical week") blabel(bar, format(%9.2f))
gr bar alcohol, over(patch1) saving(J_alcohol.gph, replace) ytit("") tit("Amount of alcohol per day (ml)") blabel(bar, format(%9.2f))
gr bar binge, over(patch1) saving(J_binge.gph, replace) ytit("") tit("Binge-drink 5 glasses or more in 2 hours") blabel(bar, format(%9.2f))
gr bar sober, over(patch1) saving(J_sober.gph, replace) ytit("") tit("Abstainer-do not drink in a typical week") blabel(bar, format(%9.2f))
gr combine J_drink_usual.gph J_binge.gph J_sober.gph J_alcohol.gph, col(2) saving(J_drink.gph, replace)
graph export "$outpath\J_drink.pdf", replace

histogram alcohol if alcohol<=300, by(patch1) saving(J_amount.gph, replace)
graph export "$outpath\J_amount.pdf", replace

gr bar hours, over(patch1) saving(J_hours.gph, replace) ytit("") tit("Hours per week") blabel(bar, format(%9.2f))
gr bar income, over(patch1) saving(J_income.gph, replace) ytit("") tit("Monthly earnings") blabel(bar, format(%9.2f))
gr bar hwage, over(patch1) saving(J_hwage.gph, replace) ytit("") tit("Hourly wage") blabel(bar, format(%9.2f))
gr combine J_hours.gph J_income.gph J_hwage.gph, col(2) saving(J_labor.gph, replace)
gr export "$outpath\J_labor.pdf", replace


use Kregsample, clear

foreach var of varlist fcoll mcoll age college married single health smoke risk sleep hwage hours income{
sum `var' if patch1==1
local k_`var'1=string(r(mean),"%9.2f")
local k_`var'1s=string(r(sd),"%9.2f")

sum `var' if patch1==0
local k_`var'2=string(r(mean),"%9.2f")
local k_`var'2s=string(r(sd),"%9.2f")

reg `var' patch1, robust
local k_`var'3b=string(_b[patch1],"%9.2f")
local k_`var'3s=string(_se[patch1],"%9.2f")

reg `var' patch1 `control', robust
local k_`var'4b=string(_b[patch1],"%9.2f")
local k_`var'4s=string(_se[patch1],"%9.2f")
}

use Jregsample, clear

foreach var of varlist fcoll mcoll age college married single health smoke risk sleep hwage hours income{
sum `var' if patch1==1
local j_`var'1=string(r(mean),"%9.2f")
local j_`var'1s=string(r(sd),"%9.2f")

sum `var' if patch1==0
local j_`var'2=string(r(mean),"%9.2f")
local j_`var'2s=string(r(sd),"%9.2f")

reg `var' patch1, robust
local j_`var'3b=string(_b[patch1],"%9.2f")
local j_`var'3s=string(_se[patch1],"%9.2f")

reg `var' patch1 `control', robust
local j_`var'4b=string(_b[patch1],"%9.2f")
local j_`var'4s=string(_se[patch1],"%9.2f")
}



mata:  

Title=("&","(1)&","(2)&","(3)&","(4) &&","(5)&","(6)&","(7)&","(8)\\ \hline")
Sub=("&","Strong&","Weak&","Strong-Weak&","Adj.&&","Strong&","Weak&","Strong-Weak&","Adj.\\ \hline")
Fb=("Father's college&","`j_fcoll1'&","`j_fcoll2'&","`j_fcoll3b'&","`j_fcoll4b' &&","`k_fcoll1'&","`k_fcoll2'&","`k_fcoll3b'&","`k_fcoll4b' \\")
Fs=("&",                "&"         ,"&"          ,"[`j_fcoll3s']&","[`j_fcoll4s']&&","&"          ,"&"          ,"[`k_fcoll3s']&","[`k_fcoll4s'] \\")
Mb=("Mother's college&","`j_mcoll1'&","`j_mcoll2'&","`j_mcoll3b'&","`j_mcoll4b' &&","`k_mcoll1'&","`k_mcoll2'&","`k_mcoll3b'&","`k_mcoll4b' \\")
Ms=("&",                "&"         ,"&"          ,"[`j_mcoll3s']&","[`j_mcoll4s']&&","&"          ,"&"          ,"[`k_mcoll3s']&","[`k_mcoll4s'] \\")
Ab=("Age&"             ,"`j_age1'&","`j_age2'&","`j_age3b'&","`j_age4b' &&","`k_age1'&","`k_age2'&","`k_age3b'&","`k_age4b' \\")
As=("&"                , "(`j_age1s')&","(`j_age2s')&" ,"[`j_age3s']&","[`j_age4s']&&","(`k_age1s')&","(`k_age2s')&"          ,"[`k_age3s']&","[`k_age4s'] \\")
Cb=("College educated&"             ,"`j_college1'&","`j_college2'&","`j_college3b'&","`j_college4b' &&","`k_college1'&","`k_college2'&","`k_college3b'&","`k_college4b' \\")
Cs=("&",        "&"         ,"&"          ,"[`j_college3s']&","[`j_college4s']&&","&"          ,"&"          ,"[`k_college3s']&","[`k_college4s'] \\")
Mab=("Married&"             ,"`j_married1'&","`j_married2'&","`j_married3b'&","`j_married4b' &&","`k_married1'&","`k_married2'&","`k_married3b'&","`k_married4b' \\")
Mas=("&",        "&"         ,"&"          ,"[`j_married3s']&","[`j_married4s']&&","&"          ,"&"          ,"[`k_married3s']&","[`k_married4s'] \\")
Nb=("Never married&"             ,"`j_single1'&","`j_single2'&","`j_single3b'&","`j_single4b' &&","`k_single1'&","`k_single2'&","`k_single3b'&","`k_single4b' \\")
Ns=("&",        "&"         ,"&"          ,"[`j_single3s']&","[`j_single4s']&&","&"          ,"&"          ,"[`k_single3s']&","[`k_single4s'] \\")
Hb=("Health (1-5)&"             ,"`j_health1'&","`j_health2'&","`j_health3b'&","`j_health4b' &&","`k_health1'&","`k_health2'&","`k_health3b'&","`k_health4b' \\")
Hs=("(1=Good, 5=Bad)&" , "(`j_health1s')&","(`j_health2s')&" ,"[`j_health3s']&","[`j_health4s']&&","(`k_health1s')&","(`k_health2s')&"          ,"[`k_health3s']&","[`k_health4s'] \\")
Sb=("Smoking&","`j_smoke1'&","`j_smoke2'&","`j_smoke3b'&","`j_smoke4b' &&","`k_smoke1'&","`k_smoke2'&","`k_smoke3b'&","`k_smoke4b' \\")
Ss=("&",                "&"         ,"&"          ,"[`j_smoke3s']&","[`j_smoke4s']&&","&"          ,"&"          ,"[`k_smoke3s']&","[`k_smoke4s'] \\")
Rb=("Risk(0-10)&"             ,"`j_risk1'&","`j_risk2'&","`j_risk3b'&","`j_risk4b' &&","`k_risk1'&","`k_risk2'&","`k_risk3b'&","`k_risk4b' \\")
Rs=("(0=Intorelant, 10=Torelant)&"                , "(`j_risk1s')&","(`j_risk2s')&" ,"[`j_risk3s']&","[`j_risk4s']&&","(`k_risk1s')&","(`k_risk2s')&"          ,"[`k_risk3s']&","[`k_risk4s'] \\")
SLb=("Hours of sleep&"             ,"`j_sleep1'&","`j_sleep2'&","`j_sleep3b'&","`j_sleep4b' &&","`k_sleep1'&","`k_sleep2'&","`k_sleep3b'&","`k_sleep4b' \\")
SLs=("&"                , "(`j_sleep1s')&","(`j_sleep2s')&" ,"[`j_sleep3s']&","[`j_sleep4s']&&","(`k_sleep1s')&","(`k_sleep2s')&"          ,"[`k_sleep3s']&","[`k_sleep4s'] \\")
HWb=("Hourly wage&"             ,"`j_hwage1'&","`j_hwage2'&","`j_hwage3b'&","`j_hwage4b' &&","`k_hwage1'&","`k_hwage2'&","`k_hwage3b'&","`k_hwage4b' \\")
HWs=("&"                , "(`j_hwage1s')&","(`j_hwage2s')&" ,"[`j_hwage3s']&","[`j_hwage4s']&&","(`k_hwage1s')&","(`k_hwage2s')&"          ,"[`k_hwage3s']&","[`k_hwage4s'] \\")
Wb=("Work hours&"             ,"`j_hours1'&","`j_hours2'&","`j_hours3b'&","`j_hours4b' &&","`k_hours1'&","`k_hours2'&","`k_hours3b'&","`k_hours4b' \\")
Ws=("&"                , "(`j_hours1s')&","(`j_hours2s')&" ,"[`j_hours3s']&","[`j_hours4s']&&","(`k_hours1s')&","(`k_hours2s')&"          ,"[`k_hours3s']&","[`k_hours4s'] \\")
Eb=("Monthly earnings&"             ,"`j_income1'&","`j_income2'&","`j_income3b'&","`j_income4b' &&","`k_income1'&","`k_income2'&","`k_income3b'&","`k_income4b' \\")
Es=("&"                , "(`j_income1s')&","(`j_income2s')&" ,"[`j_income3s']&","[`j_income4s']&&","(`k_income1s')&","(`k_income2s')&"          ,"[`k_income3s']&","[`k_income4s'] \\")

end

mata:
Table2=(Title\Sub\Fb\Fs\Mb\Ms\Ab\As\Cb\Cs\Mab\Mas\Nb\Ns\Hb\Hs\Sb\Ss\Rb\Rs\SLb\SLs\HWb\HWs\Wb\Ws\Eb\Es)
Table2
end


* Patch result and drinking

reg drink_lastweek patch1 `control', r
outreg2 using Jpatch_drink1, replace excel dec(3) nonot
outreg2 using "$outpath\Jpatch_drink1.tex", replace tex(frag) dec(3) nonot

foreach var of varlist drink_usual binge ln_alcohol sober{
reg `var' patch1 `control', r
outreg2 using Jpatch_drink1, append excel dec(3) nonot
outreg2 using "$outpath\Jpatch_drink1.tex", append tex(frag) dec(3) nonot
}

reg drink_lastweek patch3 `control', r
outreg2 using Jpatch_drink3, replace excel dec(3) nonot
outreg2 using "$outpath\Jpatch_drink3.tex", replace tex(frag) dec(3) nonot

foreach var of varlist drink_usual binge ln_alcohol sober{
reg `var' patch3 `control', r
outreg2 using Jpatch_drink3, append excel dec(3) nonot
outreg2 using "$outpath\Jpatch_drink3.tex", append tex(frag) dec(3) nonot
}



* Patch result and economic outcomes

reg lnhwage patch1 `control', r
outreg2 using Jpatch_labor1, replace excel dec(3) nonot
outreg2 using "$outpath\Jpatch_labor1.tex", replace tex(frag) dec(3) nonot

reg lnhours patch1 `control', r
outreg2 using Jpatch_labor1, append excel dec(3) nonot
outreg2 using "$outpath\Jpatch_labor1.tex", append tex(frag) dec(3) nonot

reg lnincome patch1 `control', r
outreg2 using Jpatch_labor1, append excel dec(3) nonot
outreg2 using "$outpath\Jpatch_labor1.tex", append tex(frag) dec(3) nonot


reg lnhwage patch3 `control', r
outreg2 using Jpatch_labor3, replace excel dec(3) nonot
outreg2 using "$outpath\Jpatch_labor3.tex", replace tex(frag) dec(3) nonot

reg lnhours patch3 `control', r
outreg2 using Jpatch_labor3, append excel dec(3) nonot
outreg2 using "$outpath\Jpatch_labor3.tex", append tex(frag) dec(3) nonot

reg lnincome patch3 `control', r
outreg2 using Jpatch_labor3, append excel dec(3) nonot
outreg2 using "$outpath\Jpatch_labor3.tex", append tex(frag) dec(3) nonot




* Attitudes toward drinking

use Kregsample, clear
foreach var of varlist stress network communicate business physical mental physical_bad mental_bad timewaste{
sum `var' if patch1==1
local k_`var'1=string(r(mean),"%9.3f")
local k_`var'1s=string(r(sd),"%9.3f")

sum `var' if patch1==0
local k_`var'2=string(r(mean),"%9.3f")
local k_`var'2s=string(r(sd),"%9.3f")

reg `var' patch1, robust
local k_`var'3b=string(_b[patch1],"%9.3f")
local k_`var'3s=string(_se[patch1],"%9.3f")

reg `var' patch1 `control', robust
local k_`var'4b=string(_b[patch1],"%9.3f")
local k_`var'4s=string(_se[patch1],"%9.3f")
}

use Jregsample, clear

foreach var of varlist stress network communicate business physical mental physical_bad mental_bad timewaste{
sum `var' if patch1==1
local j_`var'1=string(r(mean),"%9.3f")
local j_`var'1s=string(r(sd),"%9.3f")

sum `var' if patch1==0
local j_`var'2=string(r(mean),"%9.3f")
local j_`var'2s=string(r(sd),"%9.3f")

reg `var' patch1, robust
local j_`var'3b=string(_b[patch1],"%9.3f")
local j_`var'3s=string(_se[patch1],"%9.3f")

reg `var' patch1 `control', robust
local j_`var'4b=string(_b[patch1],"%9.3f")
local j_`var'4s=string(_se[patch1],"%9.3f")
}





mata:  

Title=("&","(1)&","(2)&","(3)&","(4) &&","(5)&","(6)&","(7)&","(8)\\ \hline")
Sub=("&","Strong&","Weak&","Strong-Weak&","Adj.&&","Strong&","Weak&","Strong-Weak&","Adj.\\ \hline")
Fb=("Relieve stress&","`j_stress1'&","`j_stress2'&","`j_stress3b'&","`j_stress4b' &&","`k_stress1'&","`k_stress2'&","`k_stress3b'&","`k_stress4b' \\")
Fs=("&",                "&"         ,"&"          ,"[`j_stress3s']&","[`j_stress4s']&&","&"          ,"&"          ,"[`k_stress3s']&","[`k_stress4s'] \\")
Mb=("Social network&","`j_network1'&","`j_network2'&","`j_network3b'&","`j_network4b' &&","`k_network1'&","`k_network2'&","`k_network3b'&","`k_network4b' \\")
Ms=("&",                "&"         ,"&"          ,"[`j_network3s']&","[`j_network4s']&&","&"          ,"&"          ,"[`k_network3s']&","[`k_network4s'] \\")
Ab=("Communication with colleagues or supervisors&"             ,"`j_communicate1'&","`j_communicate2'&","`j_communicate3b'&","`j_communicate4b' &&","`k_communicate1'&","`k_communicate2'&","`k_communicate3b'&","`k_communicate4b' \\")
As=("&"                , "&","&" ,"[`j_communicate3s']&","[`j_communicate4s']&&","&","&"          ,"[`k_communicate3s']&","[`k_communicate4s'] \\")
Cb=("Relationship with business partners&"             ,"`j_business1'&","`j_business2'&","`j_business3b'&","`j_business4b' &&","`k_business1'&","`k_business2'&","`k_business3b'&","`k_business4b' \\")
Cs=("&",        "&"         ,"&"          ,"[`j_business3s']&","[`j_business4s']&&","&"          ,"&"          ,"[`k_business3s']&","[`k_business4s'] \\")
Mab=("Good effect on physical health &"             ,"`j_physical1'&","`j_physical2'&","`j_physical3b'&","`j_physical4b' &&","`k_physical1'&","`k_physical2'&","`k_physical3b'&","`k_physical4b' \\")
Mas=("&",        "&"         ,"&"          ,"[`j_physical3s']&","[`j_physical4s']&&","&"          ,"&"          ,"[`k_physical3s']&","[`k_physical4s'] \\")
Nb=("Good effect on mental health &"             ,"`j_mental1'&","`j_mental2'&","`j_mental3b'&","`j_mental4b' &&","`k_mental1'&","`k_mental2'&","`k_mental3b'&","`k_mental4b' \\")
Ns=("&",        "&"         ,"&"          ,"[`j_mental3s']&","[`j_mental4s']&&","&"          ,"&"          ,"[`k_mental3s']&","[`k_mental4s'] \\")
Hb=("Bad effect on physical health &"             ,"`j_physical_bad1'&","`j_physical_bad2'&","`j_physical_bad3b'&","`j_physical_bad4b' &&","`k_physical_bad1'&","`k_physical_bad2'&","`k_physical_bad3b'&","`k_physical_bad4b' \\")
Hs=("&" , "&","&" ,"[`j_physical_bad3s']&","[`j_physical_bad4s']&&","&","&"          ,"[`k_physical_bad3s']&","[`k_physical_bad4s'] \\")
Sb=("Bad effect on mental health &","`j_mental_bad1'&","`j_mental_bad2'&","`j_mental_bad3b'&","`j_mental_bad4b' &&","`k_mental_bad1'&","`k_mental_bad2'&","`k_mental_bad3b'&","`k_mental_bad4b' \\")
Ss=("&",                "&"         ,"&"          ,"[`j_mental_bad3s']&","[`j_mental_bad4s']&&","&"          ,"&"          ,"[`k_mental_bad3s']&","[`k_mental_bad4s'] \\")
Rb=("Drinking wastes time&"             ,"`j_timewaste1'&","`j_timewaste2'&","`j_timewaste3b'&","`j_timewaste4b' &&","`k_timewaste1'&","`k_timewaste2'&","`k_timewaste3b'&","`k_timewaste4b' \\")
Rs=("&"                , "&","&" ,"[`j_timewaste3s']&","[`j_timewaste4s']&&","&","&"          ,"[`k_timewaste3s']&","[`k_timewaste4s'] \\")
end

mata:
Table3=(Title\Sub\Fb\Fs\Mb\Ms\Ab\As\Cb\Cs\Mab\Mas\Nb\Ns\Hb\Hs\Sb\Ss\Rb\Rs)
Table3
end
mmat2tex Table3 using Table3.tex , replace

* Mechanism

use Kregsample, clear

foreach var of varlist health sleep coworker interperson borrow{
sum `var' if patch1==1
local k_`var'1=string(r(mean),"%9.2f")
local k_`var'1s=string(r(sd),"%9.2f")

sum `var' if patch1==0
local k_`var'2=string(r(mean),"%9.2f")
local k_`var'2s=string(r(sd),"%9.2f")

reg `var' patch1, robust
local k_`var'3b=string(_b[patch1],"%9.2f")
local k_`var'3s=string(_se[patch1],"%9.2f")

reg `var' patch1 `control', robust
local k_`var'4b=string(_b[patch1],"%9.2f")
local k_`var'4s=string(_se[patch1],"%9.2f")
}

use Jregsample, clear

foreach var of varlist health sleep coworker interperson borrow{
sum `var' if patch1==1
local j_`var'1=string(r(mean),"%9.2f")
local j_`var'1s=string(r(sd),"%9.2f")

sum `var' if patch1==0
local j_`var'2=string(r(mean),"%9.2f")
local j_`var'2s=string(r(sd),"%9.2f")

reg `var' patch1, robust
local j_`var'3b=string(_b[patch1],"%9.2f")
local j_`var'3s=string(_se[patch1],"%9.2f")

reg `var' patch1 `control', robust
local j_`var'4b=string(_b[patch1],"%9.2f")
local j_`var'4s=string(_se[patch1],"%9.2f")
}



mata:  

Title=("&","(1)&","(2)&","(3)&","(4) &&","(5)&","(6)&","(7)&","(8)\\ \hline")
Sub=("&","Strong&","Weak&","Strong-Weak&","Adj.&&","Strong&","Weak&","Strong-Weak&","Adj.\\ \hline")
Hb=("Health (1-5)&"             ,"`j_health1'&","`j_health2'&","`j_health3b'&","`j_health4b' &&","`k_health1'&","`k_health2'&","`k_health3b'&","`k_health4b' \\")
Hs=("(1=Good, 5=Bad)&" , "(`j_health1s')&","(`j_health2s')&" ,"[`j_health3s']&","[`j_health4s']&&","(`k_health1s')&","(`k_health2s')&"          ,"[`k_health3s']&","[`k_health4s'] \\")
SLb=("Hours of sleep&"             ,"`j_sleep1'&","`j_sleep2'&","`j_sleep3b'&","`j_sleep4b' &&","`k_sleep1'&","`k_sleep2'&","`k_sleep3b'&","`k_sleep4b' \\")
SLs=("&"                , "(`j_sleep1s')&","(`j_sleep2s')&" ,"[`j_sleep3s']&","[`j_sleep4s']&&","(`k_sleep1s')&","(`k_sleep2s')&"          ,"[`k_sleep3s']&","[`k_sleep4s'] \\")
Cb=("No. of people at work&"             ,"`j_coworker1'&","`j_coworker2'&","`j_coworker3b'&","`j_coworker4b' &&","`k_coworker1'&","`k_coworker2'&","`k_coworker3b'&","`k_coworker4b' \\")
Cs=("&"                , "(`j_coworker1s')&","(`j_coworker2s')&" ,"[`j_coworker3s']&","[`j_coworker4s']&&","(`k_coworker1s')&","(`k_coworker2s')&"          ,"[`k_coworker3s']&","[`k_coworker4s'] \\")
Ib=("Interpersonal skills required&"             ,"`j_interperson1'&","`j_interperson2'&","`j_interperson3b'&","`j_interperson4b' &&","`k_interperson1'&","`k_interperson2'&","`k_interperson3b'&","`k_interperson4b' \\")
Is=("&"                , "(`j_interperson1s')&","(`j_interperson2s')&" ,"[`j_interperson3s']&","[`j_interperson4s']&&","(`k_interperson1s')&","(`k_interperson2s')&"          ,"[`k_interperson3s']&","[`k_interperson4s'] \\")
Nb=("No. of people you can borrow 0.5 million yen&"             ,"`j_borrow1'&","`j_borrow2'&","`j_borrow3b'&","`j_borrow4b' &&","`k_borrow1'&","`k_borrow2'&","`k_borrow3b'&","`k_borrow4b' \\")
Ns=("&"                , "(`j_borrow1s')&","(`j_borrow2s')&" ,"[`j_borrow3s']&","[`j_borrow4s']&&","(`k_borrow1s')&","(`k_borrow2s')&"          ,"[`k_borrow3s']&","[`k_borrow4s'] \\")
end

mata:
Table2=(Title\Sub\Hb\Hs\SLb\SLs\Cb\Cs\Ib\Is\Nb\Ns)
Table2
end

*IV validity by wooldridge

gen patch1alcohol=patch1*alcohol

foreach var of varlist stress network communicate business ///
physical mental physical_bad mental_bad timewaste {
qui reg `var' patch1 alcohol patch1alcohol
est sto model_`var'
}
suest model_stress model_network model_communicate model_business ///
model_physical model_mental model_physical_bad model_mental_bad model_timewaste, robust
test patch1alcohol

foreach var of varlist stress network communicate business ///
physical mental physical_bad mental_bad timewaste {
qui reg `var' patch1 alcohol patch1alcohol `control'
est sto model_`var'
}
suest model_stress model_network model_communicate model_business ///
model_physical model_mental model_physical_bad model_mental_bad model_timewaste, robust
test patch1alcohol

* IV regression

reg lnhwage alcohol `control', r
outreg2 using J_IV, replace excel dec(3) nonot
ivreg2 lnhwage (alcohol = patch1) `control', r
outreg2 using J_IV, append excel dec(3) nonot

*robustness
reg alcohol patch3 `control', r
outreg2 using Jrobust, replace excel dec(3) nonot
reg lnhwage patch3 `control', r
outreg2 using Jrobust, append excel dec(3) nonot
ivreg2 lnhwage (alcohol=patch3) `control', r
outreg2 using Jrobust, append excel dec(3) nonot

*local average treatment effect
gen drink=(alcohol>0&alcohol<.)

reg drink patch1
local pia=_b[_cons]
local pic=_b[patch1]
local pin=1-`pia'-`pic'
di `pia'
di `pic'
di `pin'

reg lnhwage if drink==0&patch1==1
local y0n=_b[_cons]
reg lnhwage if drink==1&patch1==0
local y1a=_b[_cons]

reg lnhwage if drink==0&patch1==0
local y0cn=_b[_cons]
reg lnhwage if drink==1&patch1==1
local y1ca=_b[_cons]

local y0c=`y0cn'*(`pic'+`pin')/`pic' - `y0n'*`pin'/`pic'
local y1c=`y1ca'*(`pic'+`pia')/`pic' - `y1a'*`pia'/`pic'

di `y0n'
di `y0c'
di `y1a'
di `y1c'

twoway kdensity lnhwage if drink==0&patch1==0 || kdensity lnhwage ///
if drink==0&patch1==1, xtitle("lnwage, non-drinkers") ///
legend(label(1 "weak") label(2 "strong")) saving(J_validity0.gph, replace)

twoway kdensity lnhwage if drink==1&patch1==0 || kdensity lnhwage ///
if drink==1&patch1==1, xtitle("lnwage, drinkers") ///
legend(label(1 "weak") label(2 "strong")) saving(J_validity1.gph, replace)

graph combine J_validity0.gph J_validity1.gph, col(2) saving(J_valid.gph, replace)

ivreg lnhwage (drink=patch1), robust
ivreg lnhwage (drink=patch1) `control', robust

log close
exit
 
