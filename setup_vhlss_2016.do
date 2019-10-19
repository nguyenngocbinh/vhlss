/*
Author: Nguyen ngoc Binh
Organization:ILSSA
Last update: 06/05/2018
**************************************************************************/
clear all
set mem 500m
set more off
global data08  "F:\GoogleDrive\Data\VHLSS2008"
global temp  "F:\Data\temp\vhlss2008"               // change directory here
global id tinh huyen xa diaban hoso matv
global idh tinh huyen xa diaban hoso

use $data08\ho11.dta,clear
local i=2
while `i'< 7 {
	merge 1:1 $idh using $data08\ho1`i'.dta
	keep if _merge == 3
	drop _merge
	sort $idh
local i=`i'+1
}
sort $idh
merge 1:1 $idh using $data08\muc7.dta
	keep if _merge == 3
	drop _merge
merge 1:1 $idh using  $data08\ho.dta 
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
egen hhexp_durables=rsum(m6bc7)
* Chi tai san co dinh + khoang khac khong tinh vao chi tieu
egen hhexp_assets=rsum(m5b4c m6ac8 m6ac9)
* Chi nha o
egen hhexp_housing = rsum(m7c21 m7c23a m7c24)
* Chi thuong xuyen
gen hhexp_daily=m7c40
*Tong chi tieu cua ho gia dinh
egen hhexpenditure = rsum(hhexp_edu hhexp_healthcare hhexp_food hhexp_nonfood hhexp_other hhexp_durables hhexp_daily)
recode hhexpenditure (0=.)
*Chi tieu binh quan cua ho gia dinh1 th?ng
* gen  chibq=hhexpenditure/tsnguoi/12
*Chi tieu cho y te binh quan dau nguoi 1 n?m
gen av_exp_healthcare=hhexp_healthcare/hhsize
quiet compress
save $temp\ho.dta,replace

use $data08\muc123a.dta , clear
// ??c ?i?m ngh? nghi?p
merge 1:1 $id using  $data08\muc4a.dta
// Giu lai tat ca cac quan sat
drop _merge
// ??c ?i?m d?n t?c, th?nh th? n?ng th?n
merge m:1 $idh using  $temp\ho.dta 
keep if _merge == 3
drop _merge
// L?y th?m s? li?u chi ti?u
merge m:1 tinh huyen xa diaban hoso using  $data08\hhexpe08.dta
keep if _merge == 3
drop _merge
// L?y th?m quy?n s?
merge m:1 tinh huyen xa diaban using  $data08\weight08new4.dta
keep  if _merge == 3
drop _merge
// Gi?i t?nh
gen gender = m1ac2
recode gender (. = 99)
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
recode quanhe (9=7)

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
* by tinh huyen xa diaban hoso, sort : egen float hhsize = count(matv)

gen hhincome = thunhap
egen yearly_wageA = rsum( m4ac11  m4ac12f)	// tien luong tu cong viec chinh
egen wage = rsum( m4ac11  m4ac12f m4ac21 m4ac22f m4ac25 )
gen hhwage = m4atn

// B?ng c?p cao nh?t gi?o d?c ph? th?ng
gen highest_degree =  m2ac3a
recode highest_degree (-1 =0)
// B?ng c?p ngh?
gen train =  m2ac3b
replace train =.  if age <=7
gen dihoc = 100 if m2ac5 ==1 | 	m2ac5 ==2	// T? l? ?i h?c
recode dihoc (.=0)
gen degree_now = m2ac8

// S? n?m ?i h?c
destring m2ac1, replace 
gen edu = m2ac1 		//  h?c h?t l?p
gen highest_edu = edu		// S? n?m ?i h?c
gen yearschool = edu
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
#delimit cr

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

// m? t?nh m?i ko c? H? T?y
gen tinh_old = tinh
recode tinh_old (105 = 101)
merge m:1 tinh_old using "F:\Dropbox\temp\id\id_tinh.dta"
keep if _merge ==3
drop _merge

// T?o bi?n v?ng kinh t?
do "F:\Dropbox\dofile\labels\region6.do"

// Ng?nh c?p 2
gen indus2 = m4ac5
// Ng?nh c?p 1
gen indus1 =.
do "F:\Dropbox\dofile\labels\indus1.do"

gen occup2 =  m4ac4
// Ngh? c?p 1
do "F:\Dropbox\dofile\labels\occup1.do"

// H?nh th?c s? h?u
gen economic_sector = .
replace economic_sector = 1 if m4ac10a == 2
replace economic_sector = 2 if m4ac10a == 1 | m4ac10a == 3
replace economic_sector = 3 if m4ac10a == 5
replace economic_sector = 4 if m4ac10a == 6
replace economic_sector = 5 if m4ac10a == 4
replace economic_sector = 6 if m4ac10a == 7

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

gen lamviec = .
recode lamviec . = 1 if m4ac1a ==1
recode lamviec .= 2 if m4ac1b ==1
recode lamviec .= 3 if m4ac1c ==1
label define lamviec 1 "C? ti?n c?ng, ti?n l??ng" 2 "T? l?m n?ng, l?m th?y s?n" 3 "T? l?m kinh doanh, d?ch v?" 99 "Missing"


gen weight = wt9

* Chu?n ngh?o 2006 - 2010 cho th?nh th? n?ng th?n l?n l??t: 3120 v? 2400 theo n?m
* Theo chu?n ngh?o 2011 - 2015
gen pov_lines = 370 if ttnt ==1		// Chu?n ngh?o ??n v? ngh?n ??ng theo th?ng
recode pov_lines (. = 290) if ttnt ==2 

// T? l? ngh?o c? c?p nh?t tr??t gi?
gen tyle_ngheo = 0
replace tyle_ngheo = 100 if thubq<465 & ttnt==1
replace tyle_ngheo = 100 if thubq<345 & ttnt==2

// T? l? can ngh?o
gen can_ngheo = 0
replace can_ngheo = 100 if thubq<=370 * 1.3 & ttnt==1
replace can_ngheo = 100 if thubq<=290 * 1.3& ttnt==2

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
gen dung_tieuhoc = 0 if age >=7 & age <=11
recode dung_tieuhoc (0=1) if edu >=1 & edu <=5
replace dung_tieuhoc = dung_tieuhoc *100 
* T? l? h?c trung h?c c? s???ng tu?i
gen dung_thcs = 0 if age >=12 & age <=15
recode dung_thcs (0=1) if edu >=6 & edu <=9
replace dung_thcs = dung_thcs *100 
* T? l? h?c trung h?c ph? th?ng ??ng tu?i
gen dung_thpt = 0 if age >=16 & age <=18
recode dung_thpt (0=1) if edu >=10 & edu <=12
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

// N??c s?ch
gen  nuocsach = 100
replace nuocsach = 0 if m7c26 == 6 | m7c26 ==8 | m7c26 ==13 | m7c26 ==14
// Nh? ?
gen loainha =  m7c3
  #delimit;
label define loainha
1" Nh? bi?t th?"
2 "Nh? ki?n c? kh?p k?n"
3 "Nh? ki?n c? kh?ng kh?p k?n"
4 "Nh? b?n ki?n c?"
5 "Nh? t?m v? kh?c";
#delimit cr

global list_all region6 age agegroup5_new marital_status gender  quanhe train  edu yearschool highest_degree degree_now economic_sector indus1 nganh_N_C_D /// 
occup1 ttnt  dantoc hhincome wage hhwage hhsize lamviec pov_lines quint quint_ct ///
hhexp_edu hhexp_healthcare hhexp_food hhexp_nonfood hhexp_other hhexp_durables hhexpenditure av_exp_healthcare hhexp_assets hhexp_housing chibq  thubq ///
tyle_ngheo can_ngheo dung_tieuhoc dung_thcs dung_thpt dihoc nuocsach dt_ngheo  educex_2 loainha reg8 yearly_wageA
// bi?n g?n nh?n
global list_label marital_status  gender  quanhe highest_degree degree_now train  region6 indus1 nganh_N_C_D occup1 ///
economic_sector  agegroup5_new quint quint_ct loainha reg8
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

order $list_all $id weight tinh_old
do "F:\Dropbox\dofile\labels\label_dantoc.do"
quiet compress
save $temp\vhlss2008.dta , replace
