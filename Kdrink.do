clear all
set more off
set graphics off
set scheme s1mono

cd "C:\Users\Daiji\Documents\GitHub\alcohol\analysis"
*cd "E:\Dropbox\Alcohol Patch Project\Data_Analysis"
*cd "C:\Users\Jungmin Lee\Dropbox\Alcohol Patch Project\Data_Analysis"
*cd "C:\Users\jmlee\Dropbox\Alcohol Patch Project\Data_Analysis"
*cd "C:\Users\user\Dropbox\Alcohol Patch Project\Data_Analysis"			/* laptop */
*cd "C:\Users\izumi\Dropbox\Alcohol Patch Project\Data_Analysis"         /* Izumi's laptop */


global outpath="C:\Users\Daiji\Documents\GitHub\alcohol\draft\figures&tables"
*global outpath="C:\Users\kawag\Dropbox\Alcohol Patch Project\LaTex3\figures&tables"
*global outpath="C:\Users\user\Dropbox\Alcohol Patch Project\LaTex3\figures&tables"
*global outpath="C:\Users\izumi\Dropbox\Alcohol Patch Project\LaTex3\figures&tables_20Jul_IY"		/* Izumi's laptop */

local control="age age2 college married single fdrink mdrink fcoll mcoll"

cap log close
log using Kanalysis_20Jul_IY.log, replace

* Survey period : February 2017 

use Kdata, clear

sort q10_2a
g atype = q10_2a
merge m:m atype using size_ml
drop _merge
rename size_ml usual_size_ml
drop atype 

sort q11_2a
g atype = q11_2a
merge m:m atype using size_ml
drop _merge
rename size_ml max_size_ml
drop atype
drop if cnr_id == .

* Demographic variables

rename 연령 age
g age2 = age^2
label var age "Age"
label var age2 "Age squared"
g single = (q32 == 1)
g married = (q32 == 2)
rename q34 height
rename q35 weight
rename q36 edu
g college = (q36 >= 6)
rename q36_1 hschrank
replace hschrank = hschrank/10
encode 지역, gen(region)
g health = 6 - q20
label var health "Health 1-5 (good)"
g stress = (q21 <= 2) if q21 ~= .
label var stress "Relieve stress"
g coworker = q28
label var coworker "# coworkers"
g lncoworker = ln(coworker)
g interperson = (q29 <= 2) if q29 ~= .
label var interperson "Interpersonal skills"
g fcoll = (q37 >= 5) if q37 ~= .
g mcoll = (q38 >= 5) if q37 ~= .
label var fcoll "Father college"
label var mcoll "Mother college"
g norelig = (q45 == 1) if q45 ~= .
label var norelig "Atheism"
g fdrink = q14
replace fdrink = . if fdrink == 3
replace fdrink = 2 - fdrink
label var fdrink "Father drink"
g mdrink = q15
replace mdrink = . if mdrink == 3
replace mdrink = 2 - mdrink
label var mdrink "Mother drink"
g sleep = q40
g risk = q42
g borrow = q41
g smoke = (q16 <= 2) if q16 ~= .
rename q8_1 agefdrink

/*
rename educ edu
label var edu "Education completed 1-7 (junior high to graduate)"
g married = (marriage == 1)
g single = (marriage == 4)
*/

* Patch test result

rename q2 patch1
rename q3 patch2
rename q4 patch3

su patch1 patch2 patch3
tab patch1
tab patch2
tab patch3

tab patch3 patch2 if patch1 == 4
tab patch3 patch1 if patch2 == 5

g strong = (patch1 == 3 & patch2 == 4 & patch3 == 0)

label var patch1 "ALDH type 1-3 (tolerant)"
g miss_patch1 = (patch1 == 4)
*replace patch1 = . if patch1 == 4	/* 3 obs */

label var patch2 "Skin color change 1-4 (no change)"
g miss_patch2 = (patch2 == 5)
*replace patch2 = . if patch2 == 5	/* 0 obs */

replace patch3 = 10 - patch3
label var patch3 "Skin color change 0-10 (no change)"

g dpatch3 = 1 if patch3 <= 3
replace dpatch3 = 2 if patch3 >= 4 & patch3 <= 6
replace dpatch3 = 3 if patch3 >= 7 & patch3 <= 10

* Drinking

rename q7 drink_lastweek
label var drink_lastweek "Days drink last week 0-7"
rename q8 drink_usual
label var drink_usual "Days drink usually 0-7"
rename q9 binge
replace binge = 2 - binge 
label var binge "5 or more drinks in 2 hrs usually"

rename q10_1a beer_usual
rename q10_1b soju_usual
label var beer_usual "Usual amt beer (ml)"
label var soju_usual "Usual amt soju (bottle)"

* Data correction
replace usual_size_ml = 360 if usual_size_ml == .
*replace drink_usual = 0.5 if drink_usual == 0 & (beer_usual ~= 0 | soju_usual ~= 0)	/* check whether this is valid */

g beersoju1 = beer_usual*0.05*drink_usual/7
g beersoju2 = soju_usual*usual_size_ml*0.2*drink_usual/7
g beersoju = beersoju1 + beersoju2
drop beersoju1 beersoju2
 
g alcohol = (q10_2b/100)*q10_2c*drink_usual/7
replace alcohol = (q10_2b/100)*(q10_2c*usual_size_ml)*drink_usual/7 if q10_2d == "병"
replace alcohol = 0 if q10_2a == .

g beer_alcohol = (q10_2b/100)*q10_2c*drink_usual/7
replace beer_alcohol = . if q10_2d == "병"
replace beer_alcohol = 0 if q10_2a == .

rename q11_1a beer_capa
rename q11_1b soju_capa
label var beer_capa "Max amt beer (ml)"
label var soju_capa "Max amt soju (bottle)"

g alcohol_heavy = (q11_2b/100)*q11_2c*drink_usual/7
replace alcohol_heavy = (q11_2b/100)*(q11_2c*max_size_ml)*drink_usual/7 if q11_2d == "병"
replace alcohol_heavy = 0 if q11_2a == .

g sober = (drink_usual == 0)
label var sober "No drinking usually"

g alcohol_litre = q10_2c
replace alcohol_litre = (q10_2c*usual_size_ml) if q10_2d == "병"
replace alcohol_litre = 0 if q10_2a == .
replace alcohol_litre = alcohol_litre/1000

su drink_usual drink_lastweek alcohol alcohol_heavy beer_alcohol beersoju


* Which measure?
replace alcohol = beersoju

g ln_alcohol=ln(alcohol+1)

* Parents' ALDH2 types 

rename q12 ftype
rename q13 mtype
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

tab patch1 ftype
tab patch1 mtype
tab ftype mtype




******************************************************************

* Economic outcomes

g income = 25 if q24 == 1
replace income = 75 if q24 == 2
replace income = 150 if q24 == 3
replace income = 250 if q24 == 4
replace income = 350 if q24 == 5
replace income = 450 if q24 == 6
replace income = 750 if q24 == 7
replace income = 1500 if q24 == 8
label var income "Monthly earnings (before tax) 10K KRW"
g lnincome = ln(income)
label var lnincome "log monthly earnings"
g hwage = income/(q23_1*4.3)
label var hwage "Hourly wage rate"
g lnhwage = ln(income/(q23_1*4.3))
rename q23_1 hours
label var hours "Weekly working hours"
g work_lastweek = (q22 == 1)
label var work_lastweek "Worked last week" 
g rest_lastweek = (q22 == 2)
label var rest_lastweek "Rest last week"
g unemp_lastweek = (q22 == 3)
label var unemp_lastweek "Unemployed"
g lnhours = ln(hours)
g wagesalary = (q27 == 2)

* Attitudes toward drinking

g network = (q25_1 == 1)
g communicate = (q25_2 == 1)
g business = (q25_3 == 1)
g physical = (q25_4 == 1)
g mental = (q25_5 == 1)

g physical_bad = (q25_6 == 1)
g mental_bad = (q25_7 == 1)
g timewaste = (q25_8 == 1)
g badresult = ((q26_1 == 1) + (q26_2 == 2) + (q26_3 == 3) + (q26_4 == 4) + (q26_5 == 5) >= 2)

g good = stress + network + communicate + business + physical + mental
g bad = physical_bad + mental_bad + timewaste + badresult

******************************************************************

keep if work_lastweek == 1
keep if lnhwage ~= .
*hist lnincome		/* no outliers for Korean data b/c multiple choices */

tab patch1
tab patch2

preserve

* Dichotomize

g Patch1=patch1
tab Patch1
replace Patch1=3 if Patch1==4
tab Patch1

replace patch1 = (patch1 == 3 | patch1 ==4) 
lab def d1 0 "Weak type" 1 "Strong type"
lab val patch1 d1

replace patch2 = (patch2 == 3 | patch2 == 4) 
lab val patch2 d1

corr patch1 patch2


* Salary wage workers
tab wagesalary
keep if wagesalary == 1

reg drink_lastweek patch1 `control', r
keep if e(sample)
tab patch1
tab Patch1


* Figures
gr bar drink_usual, over(patch1) saving(K1_drink_usual.gph, replace) ytit("") tit("Drinking days of a typical week") blabel(bar, format(%9.3f))
gr bar alcohol, over(patch1) saving(K1_alcohol.gph, replace) ytit("") tit("Amount of alcohol per day (ml)") blabel(bar, format(%9.3f))
gr bar binge, over(patch1) saving(K1_binge.gph, replace) ytit("") tit("Binge") blabel(bar, format(%9.3f))
gr bar sober, over(patch1) saving(K1_sober.gph, replace) ytit("") tit("Abstainer") blabel(bar, format(%9.3f))
foreach num of numlist 1 {
gr combine K`num'_drink_usual.gph K`num'_alcohol.gph K`num'_binge.gph K`num'_sober.gph, col(2) saving(K`num'_drink.gph, replace)
} 



* Parents

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

g fmtype = ftype*mtype
reg patch1 ftype mtype fmtype

foreach var of varlist fcoll mcoll age college married single health smoke risk sleep wagesalary {
sum `var' if patch1==0
reg `var' patch1, robust
}
foreach var of varlist hwage hours income {
sum `var' if patch1==0
reg `var' patch1, robust
}



save Kregsample, replace

*************************
* Figures

g n = 1
tab Patch1
collapse (sum) n (mean) alcohol, by(Patch1)
g frac = n/364
lab def tt 1 "Intolerent (Weak)" 2 "Sensitive (Weak)" 3 "Tolerent (Strong)"
lab val Patch1 tt 
gr bar frac, over(Patch1) ytit("Fraction of respondents (N = 364)") blabel(total, format(%9.3f)) saving(K_type.gph, replace) xsize(4) ysize(4) ylabel(0.2 0.4 0.6) 
graph export "$outpath\J_type.pdf", replace

gr bar (mean) alcohol, over(Patch1) ytit("Fraction of respondents (N = 364)") blabel(total, format(%9.3f)) saving(K_alcohol.gph, replace) 
graph export "$outpath\J_alcohol.pdf", replace

use Kregsample, clear

lab def tt 1 "Intolerent (Weak)" 2 "Sensitive (Weak)" 3 "Tolerent (Strong)"
lab val Patch1 tt 
graph pie Patch1, over(Patch1) pie(1, color(white)) pie(2, color(gs14)) pie(3, color(gs10)) line(lcolor(black) lwidth(thin)) plabel(1 percent, format(%9.1f)) plabel(2 percent, format(%9.1f)) plabel(3 percent, format(%9.1f)) note("N = 364") title("Korea") saving(K_Type.gph, replace) xsize(4) ysize(4)
graph export "$outpath\K_Type.pdf", replace


graph bar (count), saving(K_frequency.gph, replace) over(patch1) blabel(bar)
graph export "$outpath\K_frequency.pdf", replace

graph combine J_Type.gph K_Type.gph, row(1) saving(Types.gph, replace) xsize(8) ysize(4)
graph export "$outpath\Types.pdf", replace



gr bar drink_usual, over(patch1) saving(K_drink_usual.gph, replace) ytit("") tit("Drinking days of a typical week") blabel(bar, format(%9.2f))
gr bar alcohol, over(patch1) saving(K_alcohol.gph, replace) ytit("") tit("Amount of alcohol per day (ml)") blabel(bar, format(%9.2f))
gr bar binge, over(patch1) saving(K_binge.gph, replace) ytit("") tit("Binge-drink 5 glasses or more in 2 hours") blabel(bar, format(%9.2f))
gr bar sober, over(patch1) saving(K_sober.gph, replace) ytit("") tit("Abstainer-do not drink in a typical week") blabel(bar, format(%9.2f))
gr combine K_drink_usual.gph K_binge.gph K_sober.gph K_alcohol.gph, col(2) saving(K_drink.gph, replace)
graph export "$outpath\K_drink.pdf", replace

histogram alcohol if alcohol<=300, by(patch1) saving(K_amount.gph, replace)
graph export "$outpath\K_amount.pdf", replace

gr bar hours, over(patch1) saving(K_hours.gph, replace) ytit("") tit("Hours per week") blabel(bar, format(%9.2f))
gr bar income, over(patch1) saving(K_income.gph, replace) ytit("") tit("Monthly earnings") blabel(bar, format(%9.2f))
gr bar hwage, over(patch1) saving(K_hwage.gph, replace) ytit("") tit("Hourly wage") blabel(bar, format(%9.2f))
gr combine K_hours.gph K_income.gph K_hwage.gph, col(2) saving(K_labor.gph, replace)
gr export "$outpath\K_labor.pdf", replace

foreach var of varlist fcoll mcoll age college married single health smoke risk sleep {
sum `var' if patch1==0
reg `var' patch1, robust
}
foreach var of varlist hwage hours income {
sum `var' if patch1==0
reg `var' patch1, robust
}

* Patch result and drinking


reg drink_lastweek patch1 `control', r
outreg2 using Kpatch_drink1, replace excel dec(3) nonot
outreg2 using "$outpath\Kpatch_drink1.tex", replace tex(frag) dec(3) nonot

foreach var of varlist drink_usual binge ln_alcohol sober{
reg `var' patch1 `control', r
outreg2 using Kpatch_drink1, append excel dec(3) nonot
outreg2 using "$outpath\Kpatch_drink1.tex", append tex(frag) dec(3) nonot
}

reg drink_lastweek patch3 `control', r
outreg2 using Kpatch_drink3, replace excel dec(3) nonot
outreg2 using "$outpath\Kpatch_drink3.tex", replace tex(frag) dec(3) nonot

foreach var of varlist drink_usual binge ln_alcohol sober{
reg `var' patch3 `control', r
outreg2 using Kpatch_drink3, append excel dec(3) nonot
outreg2 using "$outpath\Kpatch_drink3.tex", append tex(frag) dec(3) nonot
}



* Patch result and economic outcomes

reg lnhwage patch1 `control', r
outreg2 using Kpatch_labor1, replace excel dec(3) nonot
outreg2 using "$outpath\Kpatch_labor1.tex", replace tex(frag) dec(3) nonot

reg lnhours patch1 `control', r
outreg2 using Kpatch_labor1, append excel dec(3) nonot
outreg2 using "$outpath\Kpatch_labor1.tex", append tex(frag) dec(3) nonot

reg lnincome patch1 `control', r
outreg2 using Kpatch_labor1, append excel dec(3) nonot
outreg2 using "$outpath\Kpatch_labor1.tex", append tex(frag) dec(3) nonot


reg lnhwage patch3 `control', r
outreg2 using Kpatch_labor3, replace excel dec(3) nonot
outreg2 using "$outpath\Kpatch_labor3.tex", replace tex(frag) dec(3) nonot

reg lnhours patch3 `control', r
outreg2 using Kpatch_labor3, append excel dec(3) nonot
outreg2 using "$outpath\Kpatch_labor3.tex", append tex(frag) dec(3) nonot

reg lnincome patch3 `control', r
outreg2 using Kpatch_labor3, append excel dec(3) nonot
outreg2 using "$outpath\Kpatch_labor3.tex", append tex(frag) dec(3) nonot
tab patch1
tab Patch1


* Attitudes toward drinking

foreach var of varlist stress network communicate business ///
physical mental physical_bad mental_bad timewaste {
qui reg `var' patch1
est sto model_`var'
}
suest model_stress model_network model_communicate model_business ///
model_physical model_mental model_physical_bad model_mental_bad model_timewaste, robust
test patch1

foreach var of varlist stress network communicate business ///
physical mental physical_bad mental_bad timewaste {
qui reg `var' patch1 `control'
est sto model_`var'
}
suest model_stress model_network model_communicate model_business ///
model_physical model_mental model_physical_bad model_mental_bad model_timewaste, robust
test patch1

* Mechanism

foreach var of varlist health sleep coworker interperson borrow{
qui reg `var' patch1
est sto model_`var'
}
suest model_health model_sleep model_coworker model_interperson model_borrow, robust
  test patch1

foreach var of varlist health sleep coworker interperson borrow{
qui reg `var' patch1 `control'
est sto model_`var'
}
suest model_health model_sleep model_coworker model_interperson model_borrow, robust
test patch1

tab patch1
tab Patch1


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

tab patch1
tab Patch1


* IV regression

reg lnhwage alcohol `control', r
outreg2 using K_IV, replace excel dec(3) nonot
ivreg2 lnhwage (alcohol = patch1) `control', r
outreg2 using K_IV, append excel dec(3) nonot

*robustness
reg alcohol patch3 `control', r
outreg2 using Krobust, replace excel dec(3) nonot
reg lnhwage patch3 `control', r
outreg2 using Krobust, append excel dec(3) nonot
ivreg2 lnhwage (alcohol=patch3) `control', r
outreg2 using Krobust, append excel dec(3) nonot

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
legend(label(1 "weak") label(2 "strong")) saving(K_validity0.gph, replace)

twoway kdensity lnhwage if drink==1&patch1==0 || kdensity lnhwage ///
if drink==1&patch1==1, xtitle("lnwage, drinkers") ///
legend(label(1 "weak") label(2 "strong")) saving(K_validity1.gph, replace)

graph combine K_validity0.gph K_validity1.gph, col(2) saving(K_valid.gph, replace)

ivreg lnhwage (drink=patch1), robust
ivreg lnhwage (drink=patch1) `control', robust

log close
exit
