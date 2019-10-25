# Author: Nguyễn Ngọc Bình 
# First: 22/10/2019      
# Update: 25/20/2019
# --------------------------

f_age_grp5 <- function(age){
  grp <- case_when(
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
    age >= 65 ~ '65+',
    TRUE ~ "Missing")
}

f_reg6 <- function(tinh){
  reg <- case_when(tinh == 1 | tinh == 22 | tinh >= 26 & tinh <=37 ~ 'Đồng bằng sông Hồng',
                   tinh == 24 | tinh == 25 | tinh >= 2 & tinh <= 20 ~ 'Trung du miền núi phía bắc',
                   tinh >=38 & tinh <= 60 ~ 'Bắc Trung bộ và Duyên hải miền trung',
                   tinh >=62 & tinh<=68 ~ 'Tây Nguyên',
                   tinh >=70 & tinh<=79 ~ 'Đông Nam bộ',
                   tinh >=80 & tinh <=96 ~ 'Đồng bằng sông Cửu Long',
                   TRUE ~ 'Missing')
}

f_reg_hn_hcm <- function(tinh){
  reg <- case_when(tinh == 1 ~ 'TP.Hà Nội',
                   tinh == 79 ~ 'TP.HCM',
                   tinh == 22 | tinh >= 26 & tinh <=37 ~ 'Đồng bằng sông Hồng',
                   tinh == 24 | tinh == 25 | tinh >= 2 & tinh <= 20 ~ 'Trung du miền núi phía bắc',
                   tinh >=38 & tinh <= 60 ~ 'Bắc Trung bộ và Duyên hải miền trung',
                   tinh >=62 & tinh <=68 ~ 'Tây Nguyên',
                   tinh >=70 & tinh <=78 ~ 'Đông Nam bộ',
                   tinh >=80 & tinh <=96 ~ 'Đồng bằng sông Cửu Long',
                   TRUE ~ 'Missing')
}

f_reg8<- function(tinh){
  reg <- case_when(tinh == 1 | tinh >= 26 & tinh <= 37 ~ 'Đồng bằng sông Hồng',
                   tinh >= 2 & tinh <=10 | tinh == 15 | tinh >=19 & tinh <= 25 ~ 'Đông Bắc bộ',
                   tinh >= 11 & tinh <=14 | tinh == 17 ~ 'Tây Bắc bộ',
                   tinh >=38 & tinh <=46 ~ 'Bắc Trung bộ',
                   tinh >=48 & tinh <=56 ~ 'Nam Trung bộ',
                   tinh >=62 & tinh<=68 ~ 'Tây Nguyên',
                   tinh >=58 & tinh <=60 | tinh >=70 & tinh<=79 ~ 'Đông Nam bộ',
                   tinh >=80 & tinh <=96 ~ 'Đồng bằng sông Cửu Long',
                   TRUE ~ 'Missing')
}

#=============================================================================

f_indus1 <- function(indus2){
  indus <- case_when(indus2 >=1 & indus2 <=3 ~ "1. Nông nghiệp, lâm nghiệp và thủy sản",
                     indus2 >=5 & indus2 <=9 ~ "2. Khai khoáng",
                     indus2 >=10 & indus2 <=33 ~ "3. Công nghiệp chế biến, chế tạo",
                     indus2 ==35 ~ "4. SX và phân phối điện, khí đốt, nước nóng, hơi nước và điều hòa không khí",
                     indus2 >=36 & indus2 <=39 ~ "5. Cung cấp nước; HĐ quản lý và xử lý rác thải, nước thải",
                     indus2 >=41 & indus2 <=43 ~ "6. Xây dựng",
                     indus2 >=45 & indus2 <=47 ~ "7. Bán buôn và bán lẻ; sửa chữa ô tô, mô tô, xe máy và các xe có động cơ khác",
                     indus2 >=49 & indus2 <=53 ~ "8. Vận tải kho bãi",
                     indus2 >=55 & indus2 <=56 ~ "9. Dịch vụ lưu trú và ăn uống",
                     indus2 >=58 & indus2 <=63 ~ "10. Thông tin và truyền thông",
                     indus2 >=64 & indus2 <=66 ~ "11. HĐ tài chính, ngân hàng và bảo hiểm",
                     indus2 == 68 ~ "12. HĐ KD bất động sản",
                     indus2 >=69 & indus2 <=75 ~ "13. HĐ chuyên môn, khoa học và công nghệ",
                     indus2 >=77 & indus2 <=82 ~ "14. HĐ hành chính và dịch vụ hỗ trợ",
                     indus2 ==84 ~ "15. HĐ của ĐCS, tổ chức chính trị-xã hội, QLNN, ANQP, BĐXH bắt buộc",
                     indus2 ==85 ~ "16. Giáo dục và đào tạo",
                     indus2 >=86 & indus2 <=88 ~ "17. Y tế và HĐ trợ giúp xã hội",
                     indus2 >=90 & indus2 <=93 ~ "18. Nghệ thuật vui chơi và giải trí",
                     indus2 >=94 & indus2 <=96 ~ "19. HĐ dịch vụ khác",
                     indus2 >=97 & indus2 <=98 ~ "20. HĐ làm thuê các công việc trong các hộ gia đình",
                     indus2 ==99 ~ "21. HĐ của các tổ chức và cơ quan quốc tế",
                     TRUE ~ "Missing")
}

f_nganh_n_c_d <- function(indus2){
  grp <- case_when(indus2 >=1 & indus2 <=3 ~ 'Nông nghiệp',
                   indus2 >=5 & indus2 <=35 | indus2 >=41 & indus2 <=43 ~ 'Công nghiệp',
                   indus2 >=36 & indus2 <=39 | indus2 >=45 & indus2 <=99 ~ 'Dịch vụ',
                   TRUE ~ 'Missing'
  )
}
#=============================================================================

f_occup1 <- function(occup2){
  op <- case_when(occup2 >=11 & occup2 <=19 ~ "1. Lãnh đạo trong các ngành, cấp và đơn vị",
                  occup2 >=21 & occup2 <=26 ~ "2. Chuyên môn bậc cao"	,
                  occup2 >=31 & occup2 <=36 ~ "3. Chuyên môn bậc trung",
                  occup2 >=41 & occup2 <=44 ~ "4. Nhân viên trợ lý văn phòng",
                  occup2 >=51 & occup2 <=54 ~ "5. Nhân viên dịch vụ và bán hàng",
                  occup2 >=61 & occup2 <=63 ~ "6. LĐ có kỹ năng trong N-L-TS",
                  occup2 >=71 & occup2 <=75 ~ "7. LĐ thủ công và các nghề nghiệp có liên quan khác",
                  occup2 >=81 & occup2 <=83 ~ "8. Thợ lắp ráp và vận hành máy móc, thiết bị",
                  occup2 >=91 & occup2 <=96 ~ "9. Lao động giản đơn",
                  occup2 >=0 & occup2 <=3 ~ "10. Lực lượng quân đội",
                  TRUE ~ "Missing")
}

f_recode_tinh <- function(x){
  case_when(x == 101 ~ 1,
            x == 201 ~ 2,
            x == 203 ~ 4,
            x == 207 ~ 6,
            x == 211 ~ 8,
            x == 205 ~ 10,
            x == 302 ~ 11,
            x == 301 ~ 12,
            x == 303 ~ 14,
            x == 213 ~ 15,
            x == 305 ~ 17,
            x == 215 ~ 19,
            x == 209 ~ 20,
            x == 225 ~ 22,
            x == 221 ~ 24,
            x == 217 ~ 25,
            x == 104 ~ 26,
            x == 106 ~ 27,
            x == 107 ~ 30,
            x == 103 ~ 31,
            x == 109 ~ 33,
            x == 115 ~ 34,
            x == 111 ~ 35,
            x == 113 ~ 36,
            x == 117 ~ 37,
            x == 401 ~ 38,
            x == 403 ~ 40,
            x == 405 ~ 42,
            x == 407 ~ 44,
            x == 409 ~ 45,
            x == 411 ~ 46,
            x == 501 ~ 48,
            x == 503 ~ 49,
            x == 505 ~ 51,
            x == 507 ~ 52,
            x == 509 ~ 54,
            x == 511 ~ 56,
            x == 705 ~ 58,
            x == 715 ~ 60,
            x == 601 ~ 62,
            x == 603 ~ 64,
            x == 605 ~ 66,
            x == 606 ~ 67,
            x == 607 ~ 68,
            x == 707 ~ 70,
            x == 709 ~ 72,
            x == 711 ~ 74,
            x == 713 ~ 75,
            x == 717 ~ 77,
            x == 701 ~ 79,
            x == 801 ~ 80,
            x == 807 ~ 82,
            x == 811 ~ 83,
            x == 817 ~ 84,
            x == 809 ~ 86,
            x == 803 ~ 87,
            x == 805 ~ 89,
            x == 813 ~ 91,
            x == 815 ~ 92,
            x == 816 ~ 93,
            x == 819 ~ 94,
            x == 821 ~ 95,
            x == 823 ~ 96,
            TRUE ~ 9999999)  
}
