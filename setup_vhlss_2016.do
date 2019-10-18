/*
Author: Nguyen ngoc Binh
Organization:ILSSA
Last update: 30/03/2016
**************************************************************************/
clear all
set mem 500m
set more off
global data10  "F:\GoogleDrive\Data\VHLSS2010\Data_9300ho\"
global temp  "F:\data\temp\vhlss2010"               // change directory here
global id "tinh huyen xa diaban hoso matv"
global idh "tinh huyen xa diaban hoso"

use "$data10\ho11.dta",clear
local i=2
while `i'< 6 {
	merge 1:1 $idh using "$data10\ho1`i'.dta"
	keep if _merge == 3
	drop _merge
	sort $idh
local i=`i'+1
}
sort $idh
merge 1:1 $idh using "$data10\muc7.dta"
	keep if _merge == 3
	drop _merge
gen hhsize = tsnguoi
*============ Expenditure ==========
* Chi tieu giao duc 
gen hhexp_edu = m2act
*Chi tieu cho y te 
gen hhexp_healthcare = m3ct
* Chi tieu luong thuc thuc pham cua ho trong 12 thang qua
egen hhexp_food= rsum(m5a1ct m5a2ct)
recode hhexp_food (0=.)
* Chi phi luong thuc thuc pham cua ho trong 12 thang qua
egen hhexp_nonfood = rsum(m5b1ct m5b2ct)
recode hhexp_nonfood (0=.)
* Chi khac cua ho
gen hhexp_other=m5b3ct
* Chi ?? d?ng l?u b?n
gen hhexp_durables=m6c7
* Chi thuong xuyen
gen hhexp_daily=m7c23
*Tong chi tieu cua ho gia dinh
egen hhexpenditure = rsum(hhexp_edu hhexp_healthcare hhexp_food hhexp_nonfood hhexp_other hhexp_durables hhexp_daily)
recode hhexpenditure (0=.)
*Chi tieu binh quan cua ho gia dinh1 th?ng
gen  chibq=hhexpenditure/tsnguoi/12
*Chi tieu cho y te binh quan dau nguoi 1 n?m
gen av_exp_healthcare=hhexp_healthcare/hhsize

quiet compress
save "$temp\ho.dta",replace

* Gh?p m?c 4a v?i nhau
use "$data10\muc4a1.dta",clear
local i=2
while `i'< 6 {
	merge 1:1 $id using "$data10\muc4a`i'.dta"
	keep if _merge == 3
	drop _merge
local i=`i'+1
}
save "$temp\muc4a.dta",replace

use "$data10\muc1a.dta" , clear
// ??c ?i?m ngh? nghi?p
merge 1:1 $id using  "$temp\muc4a.dta"
keep if _merge ==3
drop _merge
// Tr?nh ?? h?c v?n
merge 1:1 $id using  "$data10\muc2a1.dta" 
keep if _merge ==3
drop _merge
// ??c ?i?m d?n t?c, th?nh th? n?ng th?n
merge m:1 $idh using  "$temp\ho.dta" 
keep if _merge == 3
drop _merge
// L?y th?m quy?n s?
merge m:1 tinh huyen xa diaban using  "$data10\weight10.dta" 
keep if _merge == 3
drop _merge
merge 1:1 $id using "$data10\muc2_QX_cau9_11.dta" 
keep if _merge == 3
drop _merge
egen educex_2 = sum( m2ac11k), by ($idh)
// Gi?i t?nh
gen gender = m1ac2
label define gender 1"Nam" 2 "N?" 99 "Missing"
// Tu?i
gen age = m1ac5
do "F:\Dropbox\dofile\labels\agegroup5.do"

// T?nh tr?ng h?n nh?n
gen marital_status = m1ac6
recode marital_status (5=4)
#delimit;
label define marital_status
1 " Ch?a c? v?/ ch?ng"
2 " ?? c? v?/ ch?ng"
3 " G?a"
4 " Ly h?n/ ly th?n"
99 "Mising";
#delimit cr
// Quan h? v?i ch? h?
gen quanhe =  m1ac3
#delimit;
label define quanhe
1 "Ch? h?"
2 "V? ch?ng"
3 "Con"
4 "B? m?"
5 "?ng b? n?i/ngo?i"
6 "Ch?u	n?i/ngo?i"
7 "Kh?c";
#delimit cr

// Quy m? h?
/* by tinh huyen xa diaban hoso, sort : egen float hhsize = count(matv)
*/

gen hhincome = thunhap
gen monthly_wageA = m4ac10
egen yearly_wageA = rsum(m4ac11 m4ac12a m4ac12b)
egen wage = rsum(m4ac11 m4ac12a m4ac12b m4ac23 m4ac24a m4ac24b m4ac26 m4ac28a m4ac28b m4ac28c m4ac28d m4ac28e)
gen hhwage = m4atn

gen highest_degree =  m2ac2a
gen train =  m2ac2b
replace train =.  if age <=7
gen dihoc = 100 if m2ac4 ==1 | 	m2ac4 ==2	// T? l? ?i h?c
recode dihoc (.=0)
gen degree_now = m2ac6
gen edu_now = m2ac7
*
gen train1 = m2ac2a
replace train1 = m2ac2b if m2ac2b > m2ac2a & m2ac2b !=.
recode train1 0/3=1 4=2 5/6=3 7/8=4 9/11=5 12=1 .=99
#delimit;
label define train1 
1	"Kh?ng c? CMKT"
2	"S? c?p ngh?"
3	"Trung c?p"
4	"Cao ??ng"
5	"?h/tr?n ?h"
99	"Missing" ;
#delimit cr
// S? n?m ?i h?c
destring m2ac1, replace 
gen edu = m2ac1 		//  h?c h?t l?p
gen yearschool = edu		// S? n?m ?i h?c
replace yearschool = yearschool + 0.5 if train ==4
replace yearschool = yearschool + 1.5 if train ==5
replace yearschool = yearschool + 2 if train ==6
replace yearschool = yearschool + 3 if train ==7
replace yearschool = yearschool + 3 if highest_degree ==8
replace yearschool = yearschool + 4 if highest_degree ==9
replace yearschool = yearschool + 6 if highest_degree ==10
replace yearschool = yearschool + 9 if highest_degree ==11

#delimit;
label define highest_degree
0 "Kh?ng b?ng c?p"
1 "Ti?u h?c"
2 "THCS"
3 "THPT"
8 "Cao ??ng"
9 "??i h?c"
10 "Th?c s?"
11 "Ti?n s?"
12 "Kh?c"
99 "Missing";  
#delimit cr

#delimit;
label define train
0 "Kh?ng"
4 "S? c?p ngh?"
5 "Trung c?p ngh?"
6 "THCN"
7 "Cao ??ng ngh?"
99 "Missing";

#delimit;
label define degree_now
0 "Nh? tr?, m?u gi?o"
1 "Ti?u h?c"
2 "THCS"
3 "THPT"
4 "S? c?p ngh?"
5 "Trung c?p ngh?"
6 "THCN"
7 "Cao ??ng ngh?"
8 "Cao ??ng"
9 "??i h?c"
10 "Th?c s?"
11 "Ti?n s?"
12 "Kh?c"
99 "Missing";  
#delimit cr

// T?o bi?n v?ng kinh t?
do "F:\Dropbox\dofile\labels\region6.do"

// Ng?nh c?p 2
gen indus2 = m4ac4
// Ng?nh c?p 1

// C? 3 ng?nh m?i??u n?m trong nh?m ng?nh n?ng nghi?p
gen indus1=.
replace indus1 = 1 if indus2 ==110
replace indus1 = 1 if indus2 ==140
replace indus1 = 1 if indus2 ==160
do "F:\Dropbox\dofile\labels\indus1.do"
gen occup2 =  m4ac3
do "F:\Dropbox\dofile\labels\occup1.do"

// Lo?i h?nh kinh t?
gen economic_sector = m4ac8a
#delimit;
label define economic_sector
1	"H? c? nh?n"
2	"H? kinh doanh c? th?"
3	"T?p th?"
4	"T? nh?n"
5	"Nh? n??c"
6	"V?n ??u t? n??c ngo?i"
99	"Missing" ;
#delimit cr

gen weight = wt9

gen lamviec = .
recode lamviec . = 1 if m4ac1a ==1
recode lamviec .= 2 if m4ac1b ==1
recode lamviec .= 3 if m4ac1c ==1

label define lamviec 1 "C? ti?n c?ng, ti?n l??ng" 2 "T? l?m n?ng, l?m th?y s?n" 3 "T? l?m kinh doanh, d?ch v?" 99 "Missing"

* Chu?n ngh?o 2006 - 2010 cho th?nh th? n?ng th?n l?n l??t: 3120 v? 2400 theo n?m
* Chu?n ngh?o 2011 - 2015 cho th?nh th? n?ng th?n l?n l??t: 6000 v? 4800 theo n?m
gen pov_lines = 450 if ttnt ==1		// Chu?n ngh?o ??n v? ngh?n ??ng theo th?ng
recode pov_lines (. = 360) if ttnt ==2 

/* T? l? ngh?o chu?n -> kh?ng d?ng
gen tyle_ngheo = 0
replace tyle_ngheo = 100 if thubq<=450 & ttnt==1
replace tyle_ngheo = 100 if thubq<=360 & ttnt==2
*/
// T? l? ngh?o ?? ?i?u ch?nh t?nh theo chu?n ngh?o 2010
gen tyle_ngheo = 0
replace tyle_ngheo = 100 if thubq<=561 & ttnt==1
replace tyle_ngheo = 100 if thubq<=405 & ttnt==2

gen tyle_ngheo_new = 0
replace tyle_ngheo_new = 100 if thubq<=660 & ttnt==1
replace tyle_ngheo_new = 100 if thubq<=504 & ttnt==2
// T? l? can ngh?o
gen can_ngheo = 0
replace can_ngheo = 100 if thubq<=450 * 1.3 & ttnt==1
replace can_ngheo = 100 if thubq<=360 * 1.3& ttnt==2

// T? l? d?n t?c thi?u s? ngh?o/ t?ng s? ng??i ngh?o
gen dt_ngheo = tyle_ngheo
recode dt_ngheo(0=.)
replace dt_ngheo = 0 if dantoc ==1

**Phan to theo 5 nhom thu nhap
xtile quint= thubq,nq(5)
#delimit;
label define quint
1	"Nh?m thu nh?p th?p nh?t"
2	"Nh?m thu nh?p th?p th? 2"
3	"Nh?m thu nh?p th?p th? 3"
4	"Nh?m thu nh?p th?p th? 4"
5	"Nh?m thu nh?p cao nh?t";
#delimit cr

// T? l? ?i h?c ??ng tu?i
* T? l? h?c ti?u  h?c ??ng tu?i
gen dung_tieuhoc = 0 if age >=6 & age <=10
recode dung_tieuhoc (0=1) if edu_now >=1 & edu_now <=5  // ?ang h?c t? l?p 1 ??n l?p 5
replace dung_tieuhoc = dung_tieuhoc *100 
* T? l? h?c trung h?c c? s???ng tu?i
gen dung_thcs = 0 if age >=11 & age <=14
recode dung_thcs (0=1) if edu_now >=6 & edu_now <=9
replace dung_thcs = dung_thcs *100 
* T? l? h?c trung h?c ph? th?ng ??ng tu?i
gen dung_thpt = 0 if age >=15 & age <=17
recode dung_thpt (0=1) if edu_now >=10 & edu_now <=12
replace dung_thpt = dung_thpt *100 


**Phan to theo 5 nhom chi ti?u
xtile quint_ct= chibq,nq(5)
  #delimit;
label define quint_ct
1	"Nh?m chi ti?u th?p nh?t"
2	"Nh?m chi ti?u th?p th? 2"
3	"Nh?m chi ti?u th?p th? 3"
4	"Nh?m chi ti?u th?p th? 4"
5	"Nh?m chi ti?u cao nh?t";
#delimit cr

merge 1:1 $id using "$data10\muc4a5.dta" 
keep if _merge ==3
drop _merge

gen trocapTn=0
replace trocapTn=1 if m4ac28a~=.&m4ac28a>0
lab var trocapTn "Tro cap that nghiep"

gen trocapTv=0
replace trocapTv=1 if m4ac28b~=.&m4ac28b>0
lab var trocapTv "Tro cap thoi viec mot lan"

gen nghihuu=0
replace nghihuu=1 if m4ac28c ~= . & m4ac28c>0
lab var nghihuu "Nghi huu binh thuong o do tuoi quy dinh"

gen nghihuusom=0
replace nghihuusom=1 if m4ac28d~=.&m4ac28d>0
lab var nghihuusom "Nghi huu binh thuong o do tuoi quy dinh"

gen matsuc=0
replace matsuc=1 if  m4ac28e~=.&m4ac28e>0
lab var matsuc "Tro cap mat suc lao dong"

gen huutri=0
replace huutri=1 if nghihuu==1|nghihuusom==1
lab var huutri "Huutri"

// N??c s?ch
gen  nuocsach = 100
replace nuocsach = 0 if m7c14 == 5 | m7c14 ==7 | m7c14 ==10
// Nh? ?
gen cot=m7c3
recode cot 1/3=1 4/5=0
gen mai=m7c4
recode mai 1/2=1 3/5=0
gen tuong=m7c5
recode tuong 1/3=1 4/6=0
egen loainha=rsum(cot mai tuong) 

  #delimit;
label define loainha
3 "Nh? ki?n c?"
2 "Nh? b?n ki?n c?"
1 "Nh? thi?u ki?n c?"
0 "Nh? ??n s?" ;
#delimit cr

gen bhxh = m4ac13c
label define bhxh 1" C?" 2 "Kh?ng"

// Gi?i t?nh c?a ch? h?
gen temp = 1 if quanhe ==1 & gender ==1
replace temp = 2 if quanhe ==1 & gender ==2
egen headsex = min(temp), by( tinh huyen xa diaban hoso)
drop temp

  #delimit;
label define headsex
1"Ch? h? nam "
2 "Ch? h? n?";
#delimit cr

gen dttsheadsex = headsex
replace dttsheadsex = . if dantoc ==1 | dantoc ==4
  #delimit;
label define dttsheadsex
1"Ch? h? nam "
2 "Ch? h? n?";
#delimit cr

// bi?n gi? l?i
global list_all region6 age agegroup5_new marital_status gender  quanhe train  edu yearschool edu_now highest_degree degree_now economic_sector indus1 nganh_N_C_D /// 
occup1 ttnt  dantoc hhincome wage hhwage hhsize lamviec pov_lines quint quint_ct ///
hhexp_edu hhexp_healthcare hhexp_food hhexp_nonfood hhexp_other hhexp_durables hhexpenditure av_exp_healthcare chibq  thubq ///
tyle_ngheo  can_ngheo tyle_ngheo_new dung_tieuhoc dung_thcs dung_thpt dihoc nuocsach dt_ngheo educex_2 loainha bhxh headsex dttsheadsex reg8 monthly_wageA yearly_wageA
// bi?n g?n nh?n
global list_label marital_status  gender  quanhe highest_degree degree_now train  region6 indus1 nganh_N_C_D occup1 ///
economic_sector  edu_now agegroup5_new quint quint_ct loainha reg8 train1
foreach name in $list_label {
recode `name' ( .=99)
label values `name' `name'
}
label var marital_status "T?nh tr?ng h?n nh?n"
label var highest_degree "Tr?nh ?? h?c v?n cao nh?t"
label var train "Tr?nh ?? chuy?n m?n k? thu?t"
label var degree_now "C?p h?c n?m 2012"
label var agegroup5 "Nh?m tu?i"
label var agegroup5_new "Nh?m tu?i"
label var quanhe "Quan h? v?i ch? h?"
label var economic_sector "H?nh th?c s? h?u"
label var indus1 "Nh?m ng?nh kinh t? c?p 1"
label var occup1 "Nh?m ngh? c?p 1"
label var  nganh_N_C_D "3 ng?nh l?n"
label var region6 "V?ng kinh t?"
label var gender "Gi?i t?nh"
label var dantoc "D?n t?c"
label var ttnt "Khu v?c"
label var av_exp_healthcare "Chi ti?u y t? b?nh qu?n ??u ng??i c?a h?"
label var hhexpenditure"T?ng chi ti?u h?"
label var  chibq "Chi ti?u b?nh qu?n ??u ng??i c?a h?"
label var  hhexp_food "Chi ti?u l??ng th?c th?c ph?m trong 12 th?ng n?m 2012 c?a h?"
label var hhexp_durables "Chi ?? d?ng l?u b?n c?a h?"
label var thubq "Thu nh?p bq/ ng??i/ th?ng"
label var hhincome "Thu nh?p c?a h? trong 1 n?m"
label var hhsize "Quy m? h?"
label var tyle_ngheo "T? l? ngh?o"
label var dt_ngheo  "T? l? h? d?n t?c thi?u s? trong h? ngh?o"
label var educex_2 "Chi ti?u gi?o d?c"

order $list_all $id weight
do "F:\Dropbox\dofile\labels\label_dantoc.do"
quiet compress
save "$temp\vhlss2010.dta" , replace
