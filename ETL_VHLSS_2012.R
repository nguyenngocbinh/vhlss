# Author: Nguyen ngoc Binh
# First: 18/10/2019

library(rio)
library(tidyverse)

# Support functions
# Quantile cut function
qcut <- function(x, nbr_bin = 5){
  b <- x %>% 
    quantile(probs = seq(0, 1, length.out = nbr_bin + 1), na.rm = TRUE) %>% 
    .[2:nbr_bin + 1] %>% 
    c(-Inf, ., Inf) %>% 
    unique(.)
  
  quint <- cut(x, breaks = b, include.lowest = TRUE, right = FALSE, dig.lab = 10)
  
}

source('D:/R/VHLSS/labels.R')

# Import all file 
file_vhlss_2012 <- Sys.glob("vhlss2012/*.dta")
dt <- rio::import_list(file_vhlss_2012)

# Rename 
names(dt) <- tolower(names(dt))

# Merge các file hộ với nhau
ho <- dt$ho11 %>% 
  inner_join(dt$ho12) %>% 
  inner_join(dt$ho13) %>% 
  inner_join(dt$ho14) %>% 
  inner_join(dt$ho15) %>% 
  inner_join(dt$muc7)

# Tạo biến các mục chi tiêu hộ gia đình
ho %<>% 
  mutate_if(is.numeric, replace_na, 0) %>% 
  mutate(hhsize = tsnguoi,
         # Chi tieu giao duc 
         hhexp_edu = m2act,   
         #Chi tieu cho y te 
         hhexp_healthcare = m3ct, 
         # Chi tieu luong thuc thuc pham cua ho trong 12 thang qua
         hhexp_food = m5a1ct + m5a2ct,
         # Chi phi luong thuc thuc pham cua ho trong 12 thang qua
         hhexp_nonfood = m5b1ct + m5b2ct,
         # Chi khac cua ho
         hhexp_other = m5b3ct,
         # Chi ?? d?ng l?u b?n
         hhexp_durables=m6c7,
         # Chi thuong xuyen
         hhexp_daily = m7c27,
         #Tong chi tieu cua ho gia dinh
         hhexpenditure = hhexp_edu + hhexp_healthcare + hhexp_food + 
           hhexp_nonfood + hhexp_other + hhexp_durables + hhexp_daily,
         #Chi tieu binh quan cua ho gia dinh1 th?ng
         chibq = hhexpenditure/hhsize/12,
         #Chi tieu cho y te binh quan dau nguoi 1 n?m
         av_exp_healthcare=hhexp_healthcare/hhsize) 

# Ghep các mục thành viên hộ
dt_join <- dt$muc1a %>% 
  inner_join(dt$muc2a1) %>% 
  #inner_join(dt$muc4a) %>% 
  inner_join(dt$muc2a2) %>% 
  inner_join(ho) %>% 
  inner_join(dt$weight2012) 


thanh_vien <- dt_join %>% 
  group_by(tinh, huyen, xa, diaban, hoso) %>% 
  mutate(educex_2 = sum(m2ac11k)) %>% 
  ungroup() %>% 
  mutate(gender = case_when(m1ac2 == 1 ~ 'Nam', TRUE ~ 'Nữ'),
         age = m1ac5,
         agegroup5 = case_when(age >=0 & age <=14 ~ '0 - 14',
                               age <= 19 ~ '15 - 19',
                               age <= 24 ~ '20 - 24',
                               age <= 29 ~ '25 - 19',
                               age <= 34 ~ '30 - 34',
                               age <= 39 ~ '35 - 39',
                               age <= 44 ~ '40 - 44',
                               age <= 49 ~ '45 - 49',
                               age <= 54 ~ '50 - 54',
                               age <= 59 ~ '55 - 59',
                               age <= 64 ~ '60 - 64',
                               age >= 65 ~ '65+'),
         marital_status = case_when(m1ac6 == 1 ~ 'Chưa có vợ/chồng',
                                    m1ac6 == 2 ~ 'Đã có vợ/chồng',
                                    m1ac6 == 3 ~ 'Góa',
                                    m1ac6 == 4 & m1ac6 == 5~ 'Ly hôn/Ly thân',
                                    TRUE ~ 'missing'),
         quanhe = case_when(m1ac3 == 1 ~ 'Chủ hộ',
                            m1ac3 == 2 ~ 'Vợ/chồng',
                            m1ac3 == 3 ~ 'Con',
                            m1ac3 == 4 ~ 'Bố/mẹ',
                            m1ac3 == 5 ~ 'Ông bà nội/ngoại',
                            m1ac3 == 6 ~ 'Cháu nội/ngoại',
                            TRUE ~ 'Khác'),
         hhincome = thunhap,
         monthly_wageA = m4ac10,
         yearly_wageA = m4ac11 + m4ac12a + m4ac12b,
         allowance = m4ac28a + m4ac28b + m4ac28c + m4ac28d + m4ac28e,
         yearly_wage = m4ac11 + m4ac12a + m4ac12b + 
           m4ac23 + m4ac24a + m4ac24b + 
           m4ac26 + allowance,
         hhwage = m4atn,
         # Trình độ học vấn cao nhất
         highest_degree = case_when(m2ac2a == 0 ~ 'Không bằng cấp',
                                    m2ac2a == 1 ~ 'Tiểu học',
                                    m2ac2a == 2 ~ 'THCS',
                                    m2ac2a == 3 ~ 'THPT',
                                    m2ac2a == 8 ~ 'Cao đẳng',
                                    m2ac2a == 9 ~ 'Đại học',
                                    m2ac2a == 10 ~ 'Thạc sỹ',
                                    m2ac2a == 11 ~ 'Tiến sỹ',
                                    TRUE ~ 'Khác'),
         train =  case_when(m2ac2b == 0 ~ 'Không',
                            m2ac2b == 4 ~ 'Sơ cấp nghề',
                            m2ac2b == 5 ~ 'Trung cấp nghề',
                            m2ac2b == 6 ~ 'THCN',
                            m2ac2b == 7 ~ 'Cao đẳng nghề',
                            TRUE ~ 'Khác'),
         dihoc = case_when(m2ac4 ==1 | m2ac4 ==2 ~ 100, TRUE ~ 0),
         degree_now = case_when(m2ac6 == 0 ~ 'Nhà trẻ, mẫu giáo',
                                m2ac6 == 1 ~ 'Tiểu học',
                                m2ac6 == 2 ~ 'THCS',
                                m2ac6 == 3 ~ 'THPT',
                                m2ac6 == 4 ~ 'Sơ cấp nghề',
                                m2ac6 == 5 ~ 'Trung cấp nghề',
                                m2ac6 == 6 ~ 'THCN',
                                m2ac6 == 7 ~ 'Cao đẳng nghề',
                                m2ac6 == 8 ~ 'Cao đẳng',
                                m2ac6 == 9 ~ 'Đại học',
                                m2ac6 == 10 ~ 'Thạc sỹ',
                                m2ac6 == 11 ~ 'Tiến sỹ',
                                TRUE ~ 'Khác'),
         # Đang học lớp mấy
         edu_now = m2ac7,
         # Học hết lớp mấy
         edu = as.numeric(m2ac1),
         yearschool = case_when(train ==4 ~ edu + 0.5,
                                train ==5 ~ edu + 1.5,
                                train ==6 ~ edu + 2,
                                train ==7 ~ edu + 3,
                                highest_degree == 8 ~ edu + 3,
                                highest_degree == 9 ~ edu + 4,
                                highest_degree == 10 ~ edu + 6,
                                highest_degree == 11 ~ edu + 9,
                                TRUE ~ edu),
         reg6 = f_reg6(tinh),
         reg8 = f_reg8(tinh),
         reg_hn_hcm = f_reg_hn_hcm(tinh),
         indus2 = case_when(m4ac4 %in% c('110', '140', '160', '170') ~ 1, TRUE ~ m4ac4),
         indus1 = f_indus1(indus2),
         occup2 =  m4ac3,
         occup1 = f_occup1(occup2),
         economic_sector = case_when(m4ac8a == 1 ~ 'Hộ NLT/ Cá nhân',
                                     m4ac8a == 2 ~ 'Hộ SXKD cá thể',
                                     m4ac8a == 3 ~ 'Tập thể',
                                     m4ac8a == 4 ~ 'Tư nhân',
                                     m4ac8a == 5 ~ 'Nhà nước',
                                     m4ac8a == 6 ~ 'Vốn đầu tư nước ngoài',
                                     TRUE ~ 'KXĐ'),
         weight = wt9,
         lamviec = case_when(m4ac1a == 1 ~ 'Có tiền công/lương',
                             m4ac1b == 1 ~ 'Tự làm NLTS',
                             m4ac1c == 1 ~ 'Tự làm KD',
                             TRUE ~ 'Khác'),
         quint = qcut(thubq),
         # quint_ct = cut(chibq),
         dung_tieuhoc = case_when((edu_now >=1 & edu_now <=5) & (age >=6 & age <=10) ~ 100,
                                  TRUE ~ 0),
         dung_thcs = case_when((edu_now >=6 & edu_now <=9) & (age >=11 & age <=14) ~ 100,
                               TRUE ~ 0),
         dung_thpt = case_when((edu_now >=10 & edu_now <=12) & (age >=15 & age <=17) ~ 100,
                               TRUE ~ 0),
         nuocsach = case_when(m7c18 == 5 | m7c18 ==7 | m7c18 ==10 ~ 0, TRUE ~ 100), 
         loainha = case_when(m7c4d == 1 ~ 'Nhà biệt thự',
                             m7c4d == 2 ~ 'Nhà kiên cố khép kín',
                             m7c4d == 3 ~ 'Nhà kiên cố không khép kín',
                             m7c4d == 4 ~ 'Nhà bán kiên cố',
                             m7c4d == 5 ~ 'Nhà tạm và khác',
                             TRUE ~ 'Missing'),
         bhxh = case_when(m4ac13c == 1 ~ 'Có', m4ac13c == 2 ~ 'Không', TRUE ~ 'Khác')) %>% 
  mutate(headsex = case_when(quanhe =='Chủ hộ' & gender == 'Nam' ~ 'Chủ hộ Nam',
                             quanhe =='Chủ hộ' & gender == 'Nữ' ~ 'Chủ hộ nữ'))
