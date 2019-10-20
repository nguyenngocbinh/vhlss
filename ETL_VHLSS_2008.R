# Author: Nguyen ngoc Binh
# First: 20/10/2019
# R 3.6.1
# magrittr_1.5 tidyr_1.0.0  dplyr_0.8.3  rio_0.5.16

library(rio)
library(dplyr)
library(tidyr)
library(magrittr)

# Support functions
# Quantile cut function
qcut <- function(x, nbr_bin = 5) {
  b <- x %>%
    quantile(probs = seq(0, 1, length.out = nbr_bin + 1),
             na.rm = TRUE) %>%
    .[2:nbr_bin + 1] %>%
    c(-Inf, ., Inf) %>%
    unique(.)
  
  quint <-
    cut(
      x,
      breaks = b,
      include.lowest = TRUE,
      right = FALSE,
      dig.lab = 10
    )
  
}

source('D:/R/VHLSS/labels.R')

# Import all file
file_vhlss_2008 <- Sys.glob("vhlss2008/*.dta")
dt <- rio::import_list(file_vhlss_2008)

# Rename
names(dt) <- tolower(names(dt))

# Merge các file hộ với nhau
ho <- dt$ho %>%
  inner_join(dt$ho11) %>%
  inner_join(dt$ho12) %>%
  inner_join(dt$ho13) %>%
  inner_join(dt$ho14) %>%
  inner_join(dt$ho15) %>%
  inner_join(dt$ho16) %>%
  inner_join(dt$muc7)

# Tạo biến các mục chi tiêu hộ gia đình
ho %<>%
  mutate_if(is.numeric, replace_na, 0) %>%
  mutate(
    hhsize = tsnguoi,
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
    # Chi đồ dùng lâu bền
    hhexp_durables = m6bc7,
    # Chi tai san co dinh + khoang khac khong tinh vao chi tieu
    hhexp_assets = m5b4c + m6ac8 + m6ac9,
    # Chi nha o
    hhexp_housing = m7c21 + m7c23a + m7c24,
    # Chi thuong xuyen
    hhexp_daily = m7c40,
    #Tong chi tieu cua ho gia dinh
    hhexpenditure = hhexp_edu + hhexp_healthcare + hhexp_food +
      hhexp_nonfood + hhexp_other + hhexp_durables + hhexp_daily,
    #Chi tieu binh quan cua ho gia dinh1 th?ng
    chibq = hhexpenditure / hhsize / 12,
    #Chi tieu cho y te binh quan dau nguoi 1 n?m
    av_exp_healthcare = hhexp_healthcare / hhsize
  )

# Ghep các mục thành viên hộ
dt_join <- dt$muc123a %>%
  inner_join(dt$muc4a) %>%
  inner_join(dt$hhexpe08) %>%
  inner_join(ho)
# %>%  inner_join(dt$weight08new4)


thanh_vien <- dt_join %>%
  #group_by(tinh, huyen, xa, diaban, hoso) %>%
  #mutate(educex_2 = sum(m2ac11k)) %>%
  #ungroup() %>%
  mutate(
    gender = case_when(m1ac2 == 1 ~ 'Nam', TRUE ~ 'Nữ'),
    age = m1ac5,
    agegroup5 = case_when(
      age >= 0 & age <= 14 ~ '0 - 14',
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
      age >= 65 ~ '65+'
    ),
    marital_status = case_when(
      m1ac6 == 1 ~ 'Chưa có vợ/chồng',
      m1ac6 == 2 ~ 'Đã có vợ/chồng',
      m1ac6 == 3 ~ 'Góa',
      m1ac6 == 4 |
        m1ac6 == 5 ~ 'Ly hôn/Ly thân',
      TRUE ~ 'missing'
    ),
    quanhe = case_when(
      m1ac3 == 1 ~ 'Chủ hộ',
      m1ac3 == 2 ~ 'Vợ/chồng',
      m1ac3 == 3 ~ 'Con',
      m1ac3 == 4 ~ 'Bố/mẹ',
      m1ac3 == 5 ~ 'Ông bà nội/ngoại',
      m1ac3 == 6 ~ 'Cháu nội/ngoại',
      TRUE ~ 'Khác'
    ),
    hhincome = thunhap,
    yearly_wageA = m4ac11 + m4ac12f,
    # Tiền lương từ công việc chính
    yearly_wage = m4ac11 + m4ac12f + m4ac21 + m4ac22f + m4ac25,
    # Tiền lương tất cả các CV
    hhwage = m4atn,
    # Trình độ học vấn cao nhất
    highest_degree = case_when(
      m2ac3a == 0 ~ 'Không bằng cấp',
      m2ac3a == 1 ~ 'Tiểu học',
      m2ac3a == 2 ~ 'THCS',
      m2ac3a == 3 ~ 'THPT',
      m2ac3a == 8 ~ 'Cao đẳng',
      m2ac3a == 9 ~ 'Đại học',
      m2ac3a == 10 ~ 'Thạc sỹ',
      m2ac3a == 11 ~ 'Tiến sỹ',
      TRUE ~ 'Khác'
    ),
    train =  case_when(
      m2ac3b == 0 ~ 'Không',
      m2ac3b == 4 ~ 'Sơ cấp nghề',
      m2ac3b == 5 ~ 'Trung cấp nghề',
      m2ac3b == 6 ~ 'THCN',
      m2ac3b == 7 ~ 'Cao đẳng nghề',
      TRUE ~ 'Khác'
    ),
    dihoc = case_when(m2ac5 == 1 | m2ac5 == 2 ~ 100, TRUE ~ 0),
    degree_now = case_when(
      m2ac8 == 0 ~ 'Nhà trẻ, mẫu giáo',
      m2ac8 == 1 ~ 'Tiểu học',
      m2ac8 == 2 ~ 'THCS',
      m2ac8 == 3 ~ 'THPT',
      m2ac8 == 4 ~ 'Sơ cấp nghề',
      m2ac8 == 5 ~ 'Trung cấp nghề',
      m2ac8 == 6 ~ 'THCN',
      m2ac8 == 7 ~ 'Cao đẳng nghề',
      m2ac8 == 8 ~ 'Cao đẳng',
      m2ac8 == 9 ~ 'Đại học',
      m2ac8 == 10 ~ 'Thạc sỹ',
      m2ac8 == 11 ~ 'Tiến sỹ',
      TRUE ~ 'Khác'
    ),
    # Học hết lớp mấy
    edu = as.numeric(m2ac1),
    yearschool = case_when(
      train == 4 ~ edu + 0.5,
      train == 5 ~ edu + 1.5,
      train == 6 ~ edu + 2,
      train == 7 ~ edu + 3,
      highest_degree == 8 ~ edu + 3,
      highest_degree == 9 ~ edu + 4,
      highest_degree == 10 ~ edu + 6,
      highest_degree == 11 ~ edu + 9,
      TRUE ~ edu
    ),
    tinh = f_recode_tinh(tinh),
    reg6 = f_reg6(tinh),
    reg8 = f_reg8(tinh),
    reg_hn_hcm = f_reg_hn_hcm(tinh),
    indus2 = case_when(m4ac5 %in% c('110', '140', '160', '170') ~ 1, TRUE ~ m4ac5),
    indus1 = f_indus1(indus2),
    occup2 =  m4ac4,
    occup1 = f_occup1(occup2),
    economic_sector = case_when(
      m4ac10a == 2 ~ 'Hộ NLT/ Cá nhân',
      m4ac10a == 1 |
        m4ac10a == 3 ~ 'Hộ SXKD cá thể',
      m4ac10a == 5 ~ 'Tập thể',
      m4ac10a == 6 ~ 'Tư nhân',
      m4ac10a == 4 ~ 'Nhà nước',
      m4ac10a == 7 ~ 'Vốn đầu tư nước ngoài',
      TRUE ~ 'KXĐ'
    ),
    weight = wt9,
    lamviec = case_when(
      m4ac1a == 1 ~ 'Có tiền công/lương',
      m4ac1b == 1 ~ 'Tự làm NLTS',
      m4ac1c == 1 ~ 'Tự làm KD',
      TRUE ~ 'Khác'
    ),
    quint = qcut(thubq),
    # quint_ct = cut(chibq),
    dung_tieuhoc = case_when((edu >= 1 &
                                edu <= 5) & (age >= 7 & age <= 11) ~ 100,
                             TRUE ~ 0),
    dung_thcs = case_when((edu >= 6 &
                             edu <= 9) & (age >= 12 & age <= 15) ~ 100,
                          TRUE ~ 0),
    dung_thpt = case_when((edu >= 10 &
                             edu <= 12) & (age >= 16 & age <= 18) ~ 100,
                          TRUE ~ 0),
    nuocsach = case_when(m7c26 %in% c(6, 8, 13, 14) ~ 0, TRUE ~ 100),
    loainha = case_when(
      m7c3 == 1 ~ 'Nhà biệt thự',
      m7c3 == 2 ~ 'Nhà kiên cố khép kín',
      m7c3 == 3 ~ 'Nhà kiên cố không khép kín',
      m7c3 == 4 ~ 'Nhà bán kiên cố',
      m7c3 == 5 ~ 'Nhà tạm và khác',
      TRUE ~ 'Missing'
    )
  ) %>%
  mutate(
    headsex = case_when(
      quanhe == 'Chủ hộ' & gender == 'Nam' ~ 'Chủ hộ nam',
      quanhe == 'Chủ hộ' &
        gender == 'Nữ' ~ 'Chủ hộ nữ'
    )
  )
