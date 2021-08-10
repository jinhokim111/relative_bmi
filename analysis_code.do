
clear
set more off
set memory 700m

********************************************************************************
***** Load data
********************************************************************************
use "[file_location]merged_data.dta", clear
rename _all, lower

********************************************************************************
***** Creating control and other variables
********************************************************************************
***** Creating School ID
destring scid, gen(schoolidw1)

***** Demographics (gender, age, race/ethnicity)
gen female=1 if bio_sex==2
rename iyear interviewyear
rename h1gi1y birthyear
gen agew1=interviewyear-birthyear
rename h1gi4  hispanic
replace hispanic=0 if hispanic==6|hispanic==8|hispanic==9
rename h1gi20 gradew1
recode gradew1 96=. 97=. 98=. 99=.
ta gradew1, gen(gradew1)
rename h1gi6a white
replace white=0 if white==6|white==8|white==9
rename h1gi6b black
replace black=0 if black==6|black==8|black==9
rename h1gi6c indian
recode indian 6=. 7=. 8=. 9=.
rename h1gi6d asian
replace asian=0 if asian==6|asian==8|asian==9
replace white=0 if black==1
replace white=0 if hispanic==1
replace hispanic=0 if black==1
gen racetemp=black+hispanic+white
gen otherrace=1-racetemp

***** Stadndardized PVT score (ability score)
g pvtscore=ah_pvt
egen stdpvtscore=std(pvtscore)

***** First-born status
rename h1hr15 sibrankw1
replace sibrank=. if sibrank==99
gen birthorder=sibrankw1
recode birthorder 96=. 97=1 98=.
g firstborn=birthorder
recode firstborn (2/max=0)

***** Maternal education (completed years of schooling)
g momeducationw1=h1rm1 
recode momeducationw1 1=8 2=11 3=11 4=12 5=12 6=13 7=14 8=16 9=17 10=0 11=. 12=. 17=. 97=. 98=. 99=. 96=.

***** Family income
g familyincw1=pa55 
replace familyincw1=. if familyinc>9000

***** Rural residence
g locationw1=h1ir12 
replace locationw1=. if locationw1>6
gen rural=1 if locationw1==1
replace rural=0 if locationw1~=1 & locationw1~=.

********************************************************************************
***** Creating key variables
********************************************************************************
***** Body mass index (BMI)
recode h1gh59a (90/max=.)
recode h1gh60  (800/max=.)
g w1inches=(h1gh59a*12)+h1gh59b
g w1bmi=((h1gh60)/(w1inches*w1inches))*703
egen xt10bmi1=xtile(bmi1), nq(10)

***** Ordinal BMI rank (deciles)
by schoolidw1 gradew1 female, sort: egen n2b = count(bmi1)
by schoolidw1 gradew1 female: egen i2b = rank(bmi1), track 
gen bmi1rankg = (i2b - 1) / (n2b - 1)  
drop n2b i2b
egen xt10bmi1rankg = xtile(bmi1rankg), by(female schoolidw1 gradew1) nq(10)

***** Weight perception
g weightimage=h1gh28
recode weightimage (1/2=0) (3=1) (4/5=2) (6/max=.)

********************************************************************************
***** Dependent variables
********************************************************************************
***** Self-esteem
egen selfestw1=rowmean(h1pf30r h1pf32r h1pf33r h1pf34r h1pf35r h1pf36r)
egen selfestw2=rowmean(h2pf21r h2pf23r h2pf24r h2pf25r h2pf26r h2pf27r)

***** Depressive symptoms
recode h1fs1 h1fs2 h1fs3 h1fs4 h1fs5 h1fs6 h1fs7 h1fs8 h1fs9 h1fs10 ///
h1fs11 h1fs12 h1fs13 h1fs14 h1fs15 h1fs16 h1fs17 h1fs18 h1fs19 (6/max=.)
foreach var of varlist h1fs4 h1fs8 h1fs11 h1fs15 {
g `var'r=3-`var'
}
alpha h1fs1 h1fs2 h1fs3 h1fs4r h1fs5 h1fs6 h1fs7 h1fs8r h1fs9 h1fs10 ///
h1fs11r h1fs12 h1fs13 h1fs14 h1fs15r h1fs16 h1fs17 h1fs18 h1fs19, item g(depressw1)
alpha h2fs1 h2fs2 h2fs3 h2fs4r h2fs5 h2fs6 h2fs7 h2fs8r h2fs9 h2fs10 ///
h2fs11r h2fs12 h2fs13 h2fs14 h2fs15r h2fs16 h2fs17 h2fs18 h2fs19, item g(depressw2)
foreach var of varlist selfestw1 selfestw2 depressw1 depressw2 {
egen S`var'=std(`var')
}

***** Educational attainment
g dropoutw34=dropoutw4
replace dropoutw34 =dropoutw3 if dropoutw34 ==.
g collegew34=collegew4
replace collegew34 =collegew3 if collegew34 ==.
g educationbw3=educationw3
recode educationbw3  6/8=8 9/11=10 13/15=13 16/17=15 18/22=17
g educationbw34=educationbw4
replace educationbw34 =educationbw3 if educationbw34 ==.

********************************************************************************
***** Mediating variables
********************************************************************************
* School attachment
g closesch1=h1ed19r
g closesch1b = closesch1
recode closesch1b (1/4=0) (5=1) (6/max=.)
g partsch1=h1ed20r
g partsch1b = partsch1
recode partsch1b (1/4=0) (5=1) (6/max=.)
g happysch1=h1ed22r
g happysch1b = happysch1
recode happysch1b (1/4=0) (5=1) (6/max=.)

* Student-teacher relationships
g diffteacher1=h1ed15
g diffteacher1b=diffteacher1
recode diffteacher1b (1/2=0) (3/4=1) (6/max=.)
g teachercare1=h1pr2
g teachercare1b=teachercare1
recode teachercare1b (1/4=0) (5=1) (6/max=.)
g treatfair1=h1ed23r
g treatfair1b = treatfair1
recode treatfair1b (1/4=0) (5=1) (6/max=.)

* Student-student relationships
g diffstudent1=h1ed18
g diffstudent1b=diffstudent1
recode diffstudent1b (1/2=0) (3/4=1) (6/max=.)
g friendcare1=h1pr4
g friendcare1b=friendcare1
recode friendcare1b  (1/4=0) (5=1) (6/max=.)
g prejudice1=h1ed21r
g prejudice1b = prejudice1
recode prejudice1b (1/4=0) (5=1) (6/max=.)

* Intermediate educational variables
g eng=h1ed11
g math=h1ed12
g hist=h1ed13
g sci=h1ed14
recode eng math hist sci (1=4) (2=3) (3=2) (4=0.5) (5/max=.)
egen gpa_new1=rowmean(eng math hist sci)
g wantcollege1=h1ee1
recode wantcollege1 (6/max=.)
g bwantcollege1=wantcollege1
recode bwantcollege1 (1/4=0) (5=1)

log using "C:\Users\jinhokim\Downloads\analysis_output.log", replace

********************************************************************************
***** Before data analysis
********************************************************************************
***** Computing female cohort size
by schoolidw1 gradew1 female, sort: egen gcohortsize=count(id)

***** Creating missing indicator
misschk weightimage Sselfestw1 Sselfestw2 Sdepressw1 Sdepressw2 , generate(miss)

***** Defining a set of covariates
global control black hispanic otherrace agew1 stdpvtscore firstborn momeducationw1 familyincw1 rural gradew12 gradew13 gradew14 gradew15 gradew16

********************************************************************************
***** Descriptives (Table 1)
********************************************************************************
misschk weightimage Sselfestw1 Sselfestw2 Sdepressw1 Sdepressw2 xt10bmi1 xt10bmi1rankg schoolidw1 gradew1, generate(des_miss)

global descr Sselfestw1 Sselfestw2 Sdepressw1 Sdepressw2 /// 
dropoutw34 collegew34 educationbw34  /// 
bmi1rankg bmi1 ///
closesch1b partsch1b happysch1b ///
diffteacher1b teachercare1b treatfair1b ///
diffstudent1b friendcare1b prejudice1b ///
lnidgx2 odgx2 bcent10x  ///
gpa_new1 bwantcollege1 ///
white black hispanic otherrace agew1 stdpvtscore ///
gradew11 gradew12 gradew13 gradew14 gradew15 gradew16 firstborn ///
momeducationw1 familyincw1 rural 

estpost summarize $descr if des_missnumber==0 & gcohortsize>=5 & female==1

********************************************************************************
***** Multinomial logistic regression of weight perception on relative BMI (Table 2)
********************************************************************************
estimates clear
foreach samp in female  {
foreach num of numlist 5  {
foreach var in ib4.xt10bmi1rankg  {
foreach outcome in weightimage {
eststo:  mi estimate, post:  mlogit `outcome' `var' ib1.xt10bmi1 $control i.schoolidw1 if gcohortsize>=`num' & `samp'==1 & missnumber==0  , robust cluster (schoolidw1) b(1)
}
}
}
}
esttab, p compress eform d(*schoolid*)

********************************************************************************
****** OLS regression of psychological well-being on relative BMI (Table 3)
********************************************************************************
estimates clear
foreach samp in female   {
foreach num of numlist 5 {
foreach var in ib4.xt10bmi1rankg  {
foreach outcome in Sselfestw1   {
eststo:  mi estimate, post:   xtreg `outcome' `var' ib1.xt10bmi1 $control  if gcohortsize>=`num' & `samp'==1 & missnumber==0 , robust cluster (schoolidw1) i(schoolidw1) fe
}
}
}
}
foreach samp in female   {
foreach num of numlist 5 {
foreach var in ib4.xt10bmi1rankg  {
foreach outcome in Sselfestw2  {
eststo:  mi estimate, post:   xtreg `outcome' `var' ib1.xt10bmi1 $control  if gcohortsize>=`num' & `samp'==1 & weightimage!=. & missnumber==0 , robust cluster (schoolidw1) i(schoolidw1) fe
eststo:  mi estimate, post:   xtreg `outcome' `var' ib1.xt10bmi1 $control weightimage1 weightimage3 if gcohortsize>=`num' & `samp'==1 & missnumber==0 , robust cluster (schoolidw1) i(schoolidw1) fe
}
}
}
}
foreach samp in female   {
foreach num of numlist 5 {
foreach var in ib4.xt10bmi1rankg  {
foreach outcome in Sdepressw1   {
eststo:  mi estimate, post:   xtreg `outcome' `var' ib1.xt10bmi1 $control  if gcohortsize>=`num' & `samp'==1 & missnumber==0 , robust cluster (schoolidw1) i(schoolidw1) fe
}
}
}
}
foreach samp in female   {
foreach num of numlist 5 {
foreach var in ib4.xt10bmi1rankg  {
foreach outcome in Sdepressw2  {
eststo:  mi estimate, post:   xtreg `outcome' `var' ib1.xt10bmi1 $control  if gcohortsize>=`num' & `samp'==1 & weightimage!=. & missnumber==0 , robust cluster (schoolidw1) i(schoolidw1) fe
eststo:  mi estimate, post:   xtreg `outcome' `var' ib1.xt10bmi1 $control weightimage1 weightimage3  if gcohortsize>=`num' & `samp'==1 & missnumber==0  , robust cluster (schoolidw1) i(schoolidw1) fe
}
}
}
}
esttab, p compress  

********************************************************************************
****** Mediation analyses connecting relative BMI to psychological well-being (Table 4)
********************************************************************************
global psy1 weightimage1 weightimage3
global socb1 closesch1b partsch1b happysch1b 
global socb2 diffstudent1b friendcare1b prejudice1b
global socb3 diffteacher1b teachercare1b treatfair1b
global net1 lnidgx2M odgx2M  bcent10xM 

estimates clear
foreach samp in female  {
foreach num of numlist 5  {
foreach var in ib4.xt10bmi1rankg  {
foreach outcome in Sselfestw2 {
eststo:  mi estimate, post: xtreg `outcome' `var' i.xt10bmi1 $control $psy1 $socb1 if gcohortsize>=`num' & `samp'==1 & missnumber==0 , robust cluster (schoolidw1) i(schoolidw1) fe
eststo:  mi estimate, post: xtreg `outcome' `var' i.xt10bmi1 $control $psy1 $socb2 if gcohortsize>=`num' & `samp'==1 & missnumber==0 , robust cluster (schoolidw1) i(schoolidw1) fe
eststo:  mi estimate, post: xtreg `outcome' `var' i.xt10bmi1 $control $psy1 $socb3 if gcohortsize>=`num' & `samp'==1 & missnumber==0 , robust cluster (schoolidw1) i(schoolidw1) fe
eststo:  mi estimate, post: xtreg `outcome' `var' i.xt10bmi1 $control $psy1 $net1 lnidgx2K if gcohortsize>=`num' & `samp'==1 & missnumber==0 , robust cluster (schoolidw1) i(schoolidw1) fe
}
}
}
}
foreach samp in female  {
foreach num of numlist 5  {
foreach var in ib4.xt10bmi1rankg {
foreach outcome in Sdepressw2 {
eststo:  mi estimate, post: xtreg `outcome' `var' i.xt10bmi1 $control $psy1 $socb1 if gcohortsize>=`num' & `samp'==1 & missnumber==0 , robust cluster (schoolidw1) i(schoolidw1) fe
eststo:  mi estimate, post: xtreg `outcome' `var' i.xt10bmi1 $control $psy1 $socb2 if gcohortsize>=`num' & `samp'==1 & missnumber==0 , robust cluster (schoolidw1) i(schoolidw1) fe
eststo:  mi estimate, post: xtreg `outcome' `var' i.xt10bmi1 $control $psy1 $socb3 if gcohortsize>=`num' & `samp'==1 & missnumber==0 , robust cluster (schoolidw1) i(schoolidw1) fe
eststo:  mi estimate, post: xtreg `outcome' `var' i.xt10bmi1 $control $psy1 $net1 idgx2K if gcohortsize>=`num' & `samp'==1 & missnumber==0 , robust cluster (schoolidw1) i(schoolidw1) fe
}
}
}
}
esttab, p compress  

********************************************************************************
***** OLS regression of educational attainment on relative BMI (Table 5)
********************************************************************************
estimates clear
foreach samp in female   {
foreach num of numlist 5  {
foreach var in ib4.xt10bmi1rankg  {
foreach outcome in dropoutw34 collegew34 educationbw34  {
eststo:  mi estimate, post:   xtreg `outcome' `var' ib1.xt10bmi1 $control  if gcohortsize>=`num' & `samp'==1 &missnumber==0 , robust cluster (schoolidw1) i(schoolidw1) fe

}
}
}
}
esttab, p compress  

********************************************************************************
***** Mediation analyses connecting relative BMI to educational attainment (Table 6)
********************************************************************************
estimates clear
foreach samp in female   {
foreach num of numlist 5  {
foreach var in ib4.xt10bmi1rankg  {
foreach outcome in dropoutw34  {
eststo:  mi estimate, post:   xtreg `outcome' `var' ib1.xt10bmi1 $control  if gcohortsize>=`num' & `samp'==1 & missnumber==0 , robust cluster (schoolidw1) i(schoolidw1) fe
eststo:  mi estimate, post:   xtreg `outcome' `var' ib1.xt10bmi1 $control Sselfestw1 if gcohortsize>=`num' & `samp'==1 & missnumber==0 , robust cluster (schoolidw1) i(schoolidw1) fe
eststo:  mi estimate, post:   xtreg `outcome' `var' ib1.xt10bmi1 $control Sdepressw1 if gcohortsize>=`num' & `samp'==1 & missnumber==0 , robust cluster (schoolidw1) i(schoolidw1) fe
eststo:  mi estimate, post:   xtreg `outcome' `var' ib1.xt10bmi1 $control bwantcollege1 if gcohortsize>=`num' & `samp'==1 & missnumber==0 , robust cluster (schoolidw1) i(schoolidw1) fe
eststo:  mi estimate, post:   xtreg `outcome' `var' ib1.xt10bmi1 $control gpa_new1 if gcohortsize>=`num' & `samp'==1 & missnumber==0 , robust cluster (schoolidw1) i(schoolidw1) fe
eststo:  mi estimate, post:   xtreg `outcome' `var' ib1.xt10bmi1 $control Sselfestw1 Sdepressw1 bwantcollege1 gpa_new1 if gcohortsize>=`num' & `samp'==1 & missnumber==0 , robust cluster (schoolidw1) i(schoolidw1) fe


}
}
}
}
esttab, p compress  

********************************************************************************
***** Absolute vs. Relative BMI (Figure 1)
********************************************************************************
label var bmi1rankg "Percentile rank in school cohort"
label define bmi 1 "1st" 2 "2nd" 3 "3rd" 4 "4th" 5 "5th" 6 "6th" 7 "7th" 8 "8th" 9 "9th" 10 "10th"
label values xt10bmi1 bmi 
label var xt10bmi1 "Decile in individual-level BMI"

graph box bmi1rankg if des_missnumber==0 & gcohortsize>=5 & female==1,  scheme(s1mono) nooutsides over(xt10bmi1) b2title("Decile in individual-level BMI", size(medsmall))

********************************************************************************
***** Balancing tests (Figure 2)
********************************************************************************
foreach var of varlist bmi1  {
egen total = total(`var'), by(schoolidw1 gradew1)
egen n = count(`var'), by(schoolidw1 gradew1)
gen totalMINUSi = total - cond(missing(`var'), 0, `var')
gen mx_`var' = totalMINUSi / (n - !missing(`var'))
drop total totalMINUSi total n
}

drop m_bmi1
bysort schoolidg: egen m_bmi1=mean(bmi1) if schoolidg!=.
bysort schoolidg: egen sd_bmi1=sd(bmi1) if schoolidg!=.

***** Creating additional variables for balancing test
g divorce=pa10
recode divorce (1/3=0) (4/5=1) (6=.) /*4: Divorced, 5: Separated*/

g momwhite=pa6_1
recode momwhite (6/max=.)

g momblack=pa6_2
recode momblack (6/max=.)

g momhispanic=pa4
recode momhispanic (6/max=.)

global variables white black hispanic momwhite momblack momhispanic agew1 stdpvtscore ///
momeducationw1 familyincw1 divorce married rural

foreach this in $variables {
g m0`this'=mx_bmi1
}

label var m0white "White"
label var m0black "Black"
label var m0hispanic "Hispanic"
label var m0momwhite "Mother = White"
label var m0momblack "Mother = Black"
label var m0momhispanic "Mother = Hispanic"
label var m0agew1 "Age"
label var m0stdpvtscore "Std PVT score"
label var m0momeducationw1 "Maternal education"
label var m0familyincw1 "Family income"
label var m0divorce "Divorced or separated"
label var m0married "Married parents"
label var m0rural "Rural"

global var white black hispanic agew1 stdpvtscore momeducationw1 familyincw1 divorce married rural

estimates clear
foreach out of varlist $var {
eststo: reg `out' m0`out' gradew12 gradew13 gradew14 gradew15 gradew16 if des_missnumber==0 & gcohortsize>=5 & female==1, robust cluster(schoolidw1)
}
foreach out of varlist $var {
eststo: areg `out' m0`out' gradew12 gradew13 gradew14 gradew15 gradew16 if des_missnumber==0 & gcohortsize>=5 & female==1, robust cluster(schoolidw1) absorb(schoolidw1)
}

coefplot est1 est2 est3 est4 est5 est6 est7 est8 est9 est10, bylabel(No School FE)  ///
|| est11 est12 est13 est14 est15 est16 est17 est18 est19 est20, bylabel(School FE)  ///
|| , ms(oh) mcolor(gray) ciopt(lcolor(gray)) keep(m0*) xline(0)  ///
scheme(s1mono) nooffsets level(95) ///
byopts(legend(off))

********************************************************************************
***** OLS regression of intermediate psychological variables on relative BMI (Figure 3)
********************************************************************************
label define xt10bmi1rankgL 1 "D1" 2 "D2" 3 "D3" 4 "D4 (Ref.)" 5 "D5" 6 "D6" 7 "D7" 8 "D8" 9 "D9" 10 "D10"
label values xt10bmi1rankg xt10bmi1rankgL

global socb1 closesch1b partsch1b happysch1b 
global socb2 diffstudent1b friendcare1b prejudice1b
global socb3 diffteacher1b teachercare1b treatfair1b
global net1 lnidgx2M odgx2M  bcent10xM 

*** School attachment
estimates clear
foreach samp in female   {
foreach num of numlist 5  {
foreach var in ib4.xt10bmi1rankg  {
foreach outcome in closesch1b {
eststo:  mi estimate, post:  xtreg `outcome' `var' i.xt10bmi1 $control  if gcohortsize>=`num' & `samp'==1 & missnumber==0 , robust cluster (schoolidw1) i(schoolidw1) fe
coefplot , ms(oh) drop(_cons) xline(0) keep(*rank* )  base scheme(s1mono) ///
title("Feel close to people" "at school" " ", size(m)) level(95) name(`outcome', replace)
}
}
}
}
foreach samp in female   {
foreach num of numlist 5  {
foreach var in ib4.xt10bmi1rankg  {
foreach outcome in partsch1b {
eststo:  mi estimate, post:  xtreg `outcome' `var' i.xt10bmi1 $control  if gcohortsize>=`num' & `samp'==1 & missnumber==0 , robust cluster (schoolidw1) i(schoolidw1) fe
coefplot , ms(oh) drop(_cons) xline(0) keep(*rank* )  base scheme(s1mono)  ///
title("Feel part of school" " " " ", size(m)) level(95) name(`outcome', replace)
}
}
}
}
foreach samp in female   {
foreach num of numlist 5  {
foreach var in ib4.xt10bmi1rankg  {
foreach outcome in happysch1b {
eststo:  mi estimate, post:  xtreg `outcome' `var' i.xt10bmi1 $control if gcohortsize>=`num' & `samp'==1 & missnumber==0 , robust cluster (schoolidw1) i(schoolidw1) fe
coefplot , ms(oh) drop(_cons) xline(0) keep(*rank* )  base scheme(s1mono)  ///
title("Happy to be at school" " " " ", size(m)) level(95) name(`outcome', replace)
}
}
}
}
graph combine $socb1 , c(3) saving(socb1, replace)

*** Student-student relationships
estimates clear
foreach samp in female   {
foreach num of numlist 5  {
foreach var in ib4.xt10bmi1rankg  {
foreach outcome in diffstudent1b  {
eststo:  mi estimate, post:  xtreg `outcome' `var' i.xt10bmi1 $control  if gcohortsize>=`num' & `samp'==1 & missnumber==0 , robust cluster (schoolidw1) i(schoolidw1) fe
coefplot , ms(oh) drop(_cons) xline(0) keep(*rank* )  base  scheme(s1mono) ///
title("Difficulty getting along" "with other students" " ", size(m)) level(95) name(`outcome', replace)
}
}
}
}
foreach samp in female   {
foreach num of numlist 5  {
foreach var in ib4.xt10bmi1rankg  {
foreach outcome in friendcare1b  {
eststo:  mi estimate, post:  xtreg `outcome' `var' i.xt10bmi1 $control  if gcohortsize>=`num' & `samp'==1 & missnumber==0 , robust cluster (schoolidw1) i(schoolidw1) fe
coefplot , ms(oh) drop(_cons) xline(0) keep(*rank* )  base  scheme(s1mono) ///
title("Friends care about me" " " " ", size(m)) level(95) name(`outcome', replace)
}
}
}
}
foreach samp in female   {
foreach num of numlist 5  {
foreach var in ib4.xt10bmi1rankg  {
foreach outcome in prejudice1b {
eststo:  mi estimate, post:  xtreg `outcome' `var' i.xt10bmi1 $control  if gcohortsize>=`num' & `samp'==1 & missnumber==0 , robust cluster (schoolidw1) i(schoolidw1) fe
coefplot , ms(oh) drop(_cons) xline(0) keep(*rank* )  base  scheme(s1mono) ///
title("Students are prejudiced" " " " ", size(m)) level(95) name(`outcome', replace)
}
}
}
}
graph combine $socb2 , c(3) saving(socb2, replace)

*** Student-teacher relationships
estimates clear
foreach samp in female   {
foreach num of numlist 5  {
foreach var in ib4.xt10bmi1rankg  {
foreach outcome in diffteacher1b {
eststo:  mi estimate, post:  xtreg `outcome' `var' i.xt10bmi1 $control  if gcohortsize>=`num' & `samp'==1 & missnumber==0 , robust cluster (schoolidw1) i(schoolidw1) fe
coefplot , ms(oh) drop(_cons) xline(0) keep(*rank* )  base  scheme(s1mono) ///
title("Difficulty getting along" "with teachers" " ", size(m)) level(95) name(`outcome', replace)
}
}
}
}
foreach samp in female   {
foreach num of numlist 5  {
foreach var in ib4.xt10bmi1rankg  {
foreach outcome in teachercare1b   {
eststo:  mi estimate, post:  xtreg `outcome' `var' i.xt10bmi1 $control  if gcohortsize>=`num' & `samp'==1 & missnumber==0 , robust cluster (schoolidw1) i(schoolidw1) fe
coefplot , ms(oh) drop(_cons) xline(0) keep(*rank* )  base  scheme(s1mono) ///
title("Teachers care about me" " " " ", size(m)) level(95) name(`outcome', replace)
}
}
}
}
foreach samp in female   {
foreach num of numlist 5  {
foreach var in ib4.xt10bmi1rankg  {
foreach outcome in treatfair1b {
eststo:  mi estimate, post:  xtreg `outcome' `var' i.xt10bmi1 $control  if gcohortsize>=`num' & `samp'==1 & missnumber==0 , robust cluster (schoolidw1) i(schoolidw1) fe
coefplot , ms(oh) drop(_cons) xline(0) keep(*rank* )  base  scheme(s1mono) ///
title("Teachers treat students fairly" " " " ", size(m)) level(95) name(`outcome', replace)
}
}
}
}
graph combine $socb3 , c(3) saving(socb3, replace)

*** Friendship network
estimates clear
foreach samp in female   {
foreach num of numlist 5  {
foreach var in ib4.xt10bmi1rankg  {
foreach outcome in lnidgx2M {
eststo:  mi estimate, post:  xtreg `outcome' `var' i.xt10bmi1 $control idgx2K if gcohortsize>=`num' & `samp'==1 & missnumber==0 , robust cluster (schoolidw1) i(schoolidw1) fe
coefplot , ms(oh) drop(_cons) xline(0) keep(*rank* )  base  scheme(s1mono) ///
title("Friend nominations" "received" " ", size(m) ) level(95) name(`outcome', replace)
}
}
}
}
foreach samp in female   {
foreach num of numlist 5  {
foreach var in ib4.xt10bmi1rankg  {
foreach outcome in odgx2M {
eststo:  mi estimate, post:  xtreg `outcome' `var' i.xt10bmi1 $control idgx2K if gcohortsize>=`num' & `samp'==1 & missnumber==0 , robust cluster (schoolidw1) i(schoolidw1) fe
coefplot , ms(oh) drop(_cons) xline(0) keep(*rank* )  base  scheme(s1mono)  ///
title("Friend nominations" "sent" " ", size(m)) level(95) name(`outcome', replace)
}
}
}
}
foreach samp in female   {
foreach num of numlist 5  {
foreach var in ib4.xt10bmi1rankg  {
foreach outcome in bcent10xM {
eststo:  mi estimate, post:  xtreg `outcome' `var' i.xt10bmi1 $control idgx2K if gcohortsize>=`num' & `samp'==1 & missnumber==0 , robust cluster (schoolidw1) i(schoolidw1) fe
coefplot , ms(oh) drop(_cons) xline(0) keep(*rank* )  base  scheme(s1mono) ///
title("Centrality" " " " ", size(m)) level(95) name(`outcome', replace)
}
}
}
}
graph combine $net1, c(3) saving(net1, replace)

********************************************************************************
***** OLS regression of intermediate educational variables on relative BMI (Figure 4)
********************************************************************************
estimates clear
foreach samp in female   {
foreach num of numlist 5  {
foreach var in ib4.xt10bmi1rankg  {
foreach outcome in gpa_new1 {
eststo:  mi estimate, post:  xtreg `outcome' `var' i.xt10bmi1 $control  if gcohortsize>=`num' & `samp'==1 & missnumber==0 , robust cluster (schoolidw1) i(schoolidw1) fe
coefplot , ms(oh) drop(_cons) xline(0) keep(*rank* )  base scheme(s1mono) ///
title("Self-reported GPA" " ", size(m)) level(95) name(`outcome', replace)
}
}
}
}
foreach samp in female   {
foreach num of numlist 5  {
foreach var in ib4.xt10bmi1rankg  {
foreach outcome in bwantcollege1 {
eststo:  mi estimate, post:  xtreg `outcome' `var' i.xt10bmi1 $control  if gcohortsize>=`num' & `samp'==1 & missnumber==0 , robust cluster (schoolidw1) i(schoolidw1) fe
coefplot , ms(oh) drop(_cons) xline(0) keep(*rank* )  base scheme(s1mono)  ///
title("College aspirations" " ", size(m)) level(95) name(`outcome', replace)
}
}
}
}
graph combine gpa_new1 bwantcollege1 , c(2) saving(edu, replace)

log close