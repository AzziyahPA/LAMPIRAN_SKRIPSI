# Data input
#Input data dari Yahoo Finance
library(quantmod)
symbol = "BRIS.JK" # Ganti dengan simbol saham yang diinginkan
start_date = "2022-12-01" # Tanggal mulai
end_date = "2024-12-31" # Tanggal akhir
data_saham = getSymbols(symbol, src = "yahoo", from = start_date, 
                        to = end_date, auto.assign = FALSE, periodicity = "daily")
data = Cl(data_saham) # Menggunakan harga penutupan
data

#Analisis Deskriptif
# Menghitung nilai rata-rata, terendah, dan tertinggi
rata_rata <- mean(data, na.rm = TRUE)
rata_rata
nilai_terendah <- min(data, na.rm = TRUE)
nilai_terendah
tanggal_terendah <- index(data)[which.min(data)]
tanggal_terendah
nilai_tertinggi <- max(data, na.rm = TRUE)
nilai_tertinggi
tanggal_tertinggi <- index(data)[which.max(data)]
tanggal_tertinggi

#Plot data aktual
library(ggplot2)
plot(data, xlab = "Periode", ylab = "Harga", type = "l")

#Membentuk himpunan semesta
U1 = nilai_terendah - 50
U1
U2 = nilai_tertinggi + 50
U2
U = data.frame(U1, U2)
U

# Mencari banyak kelas
# 1. Menghitung rata-rata selisih absolut antar data
n <- length(data)
n
delta_X <- abs(diff(data))  # Menghitung perubahan absolut antar nilai
delta_X
sum_delta <- sum(delta_X, na.rm = TRUE)
sum_delta
mean_delta <- mean(delta_X, na.rm = TRUE)
mean_delta

#Menentukan panjang interval (l = Mean/2)
l <- mean_delta / 2  
l

#Menentukan basis panjang interval berdasarkan tabel
if (l >= 0.1 & l <= 1.0) {
  basis <- 0.1
} else if (l > 1 & l <= 10) {
  basis <- 1
} else if (l > 10 & l <= 100) {
  basis <- 10
} else {
  basis <- 100
}
basis

# Membulatkan panjang interval berdasarkan basis
L <- round(l/basis)*basis
L

#Menentukan jumlah interval (p)
p <- ((nilai_tertinggi+50 - nilai_terendah-50)/ L)  
p

# Membuat interval fuzzy
interval = seq(U1,U2, len = p + 1)
interval

#Membentuk himpunan fuzzy
n_intervals <- length(interval) - 1
box1 <- data.frame(bawah = interval[-length(interval)], atas = interval[-1], kel = 1:n_intervals)
print("Interval Fuzzy:")
print(box1)

#Mencari nilai tengah setiap interval
nilai_tengah <- data.frame(tengah = (box1$bawah + box1$atas) / 2, kel = box1$kel)
print("Nilai Tengah Interval:")
print(nilai_tengah)

#Fuzzyfikasi data aktual
fuzzyfikasi <- numeric(length(data))
for (i in 1:length(data)) {
  for (j in 1:nrow(box1)) {
    if (data[i] >= box1$bawah[j] & data[i] < box1$atas[j]) {
      fuzzyfikasi[i] = box1$kel[j]
      break
    }
    if (data[i] == box1$atas[j] & j == nrow(box1)) {
      fuzzyfikasi[i] = box1$kel[j]
    }
  }
}
print("Fuzzyfikasi Data:")
print(fuzzyfikasi)

#Menggabungkan hasil dalam DataFrame
Fuzzyfi <- data.frame(Data = data, Fuzzyfikasi = fuzzyfikasi)
print("Hasil Fuzzyfikasi:")
print(Fuzzyfi)


#Fuzzyfikasi ke data asal
Fuzzyfi <- data.frame(Data = data, Fuzzyfikasi = fuzzyfikasi)
Fuzzyfi

#Pembetukan FLR (Fuzzy Logical Relationship)
FLR <- data.frame(Current = fuzzyfikasi[-length(fuzzyfikasi)], Next = fuzzyfikasi[-1])
FLR

# Membuat FLRG (Fuzzy Logical Relationship Group)
FLRG <- split(FLR$Next, FLR$Current)
FLRG

# Defuzzifikasi Chen 
defuzzifikasi_chen_flrg <- function(FLRG, nilai_tengah) {
  hasil_defuzzifikasi <- numeric(length(FLRG))
  names(hasil_defuzzifikasi) <- names(FLRG)
  
  for (i in seq_along(FLRG)) {
    current_state_num <- as.numeric(names(FLRG)[i])
    next_states_num <- as.numeric(as.character(FLRG[[i]]))
    
    if (length(next_states_num) > 0) {
      # Chen: Mean of midpoints of next states
      midpoints_of_next_states <- nilai_tengah$tengah[nilai_tengah$kel %in% next_states_num]
      if (length(midpoints_of_next_states) > 0) {
        hasil_defuzzifikasi[i] <- mean(midpoints_of_next_states, na.rm = TRUE)
      } else {
        hasil_defuzzifikasi[i] <- NA # Handle cases where midpoints are not found
      }
    } else {
      # If no next states, default to midpoint of current state
      hasil_defuzzifikasi[i] <- nilai_tengah$tengah[nilai_tengah$kel == current_state_num]
    }
  }
  return(hasil_defuzzifikasi)
}
defuzzified_values_chen_flrg <- defuzzifikasi_chen_flrg(FLRG, nilai_tengah)
print("Defuzzified Values (Chen, per FLRG group):")
print(defuzzified_values_chen_flrg)
write.xlsx(as.data.frame(defuzzified_values_chen_flrg), "defuzz chen.xlsx")

#Peramalan dgn FTS Chen
chen_forecast <- numeric(length(data))
chen_forecast[1] <- NA # No forecast for the first data point
for (i in 1:(length(data) - 1)) {
  current_fuzzy_state <- as.character(fuzzyfikasi[i])
  if (current_fuzzy_state %in% names(defuzzified_values_chen_flrg)) {
    chen_forecast[i + 1] <- defuzzified_values_chen_flrg[current_fuzzy_state]
  } else {
    # If the current fuzzy state is not in the FLRG (e.g., it's a new state or last one)
    # Default to its own midpoint
    chen_forecast[i + 1] <- nilai_tengah$tengah[nilai_tengah$kel == as.numeric(current_fuzzy_state)]
  }
}
print("Peramalan Chen (Time Series):")
print(chen_forecast)

#FTS CHENG
# Matriks pembobotan untuk Cheng
# Hitung matriks pembobot berdasarkan FLR
num_kelas_fuzzy <- length(interval) - 1 # Jumlah kelas fuzzy adalah panjang interval - 1
matriks_pembobot <- matrix(0, nrow = num_kelas_fuzzy, ncol = num_kelas_fuzzy)
rownames(matriks_pembobot) <- colnames(matriks_pembobot) <- 1:num_kelas_fuzzy
for (i in 1:nrow(FLR)) {
  current <- FLR$Current[i]
  next_state <- FLR$Next[i]
  if (current >= 1 && current <= num_kelas_fuzzy && next_state >= 1 && next_state <= num_kelas_fuzzy) {
    matriks_pembobot[current, next_state] <- matriks_pembobot[current, next_state] + 1
  }
}
print("Matriks Pembobot (Frekuensi):")
print(matriks_pembobot)


#Heatmap Matriks Pembobot
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)

# Buat matriks frekuensi FLR
FLRG_freq <- FLR %>%
  count(Current, Next) %>%
  pivot_wider(names_from = Next, values_from = n, values_fill = 0) %>%
  tibble::column_to_rownames("Current")

# Ubah matrix jadi long format
df_long <- melt(as.matrix(FLRG_freq))

# Buat heatmap dengan gradasi berbeda untuk range tertentu
ggplot(df_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(
    colors = c("white", "blue",  # 0–3 (kuning muda → hijau muda)
               "green", "#008000",  # 4–8 (hijau → hijau tua)
               "yellow", "red",  # 9–12 (hijau gelap → biru)
               "brown", "black"), # 13–16 (biru tua → ungu)
    values = scales::rescale(c(0, 3, 4, 8, 9, 12, 13, 16)),
    limits = c(0, 16),
    name = "Frekuensi"
  ) +
  labs(title = "Heatmap Matriks Pembobot",
       x = "Current State", y = "Next State") +
  theme_minimal()

# Matriks standarisasi untuk defuzzifikasi
bobot = round(prop.table(table(FLR), 1), 5)
bobot
# Matriks standarisasi (bobot peluang)
bobot_prob <- prop.table(matriks_pembobot, 1)
print("Matriks Bobot Probabilistik:")
print(bobot_prob)

# Heatmap untuk bobot probabilistik
# Plot heatmap
ggplot(df_prob, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(
    colors = c("white", "#008000",     # 0–0.25 (kuning muda ke hijau muda)
               "orange", "red",     # 0.26–0.5 (hijau ke hijau tua)
               "blue", "navy",     # 0.6–0.75 (hijau gelap ke biru)
               "purple", "black"),    # 0.76–1 (biru tua ke ungu)
    values = scales::rescale(c(0, 0.25, 0.26, 0.5, 0.6, 0.75, 0.76, 1)),
    limits = c(0, 1),
    name = "Probabilitas"
  ) +
  labs(title = "Heatmap Matriks Bobot Probabilistik (Cheng)",
       x = "Current State", y = "Next State") +
  theme_minimal()#FTS CHENG

#Defuzzifikasi Cheng
defuzzifikasi_cheng_flrg <- function(FLRG, nilai_tengah) {
  nilai_tengah$kel <- as.numeric(as.character(nilai_tengah$kel))
  hasil_defuzzifikasi <- numeric(length(FLRG))
  names(hasil_defuzzifikasi) <- names(FLRG) # Name the results by the current state
  
  for (i in seq_along(FLRG)) {
    current_state <- names(FLRG)[i]
    next_states <- as.numeric(as.character(FLRG[[i]]))
    
    if (length(next_states) == 0 || all(is.na(next_states))) {
      hasil_defuzzifikasi[i] <- NA # No next states, cannot defuzzify
      next
    }
    
    table_states <- table(next_states)
    bobot <- table_states / sum(table_states) # Probabilistic weights
    
    nilai_mid <- numeric(length(bobot))
    valid_bobot_indices <- character(0) # To store names of valid states
    
    for (state_name in names(bobot)) {
      state_num <- as.numeric(state_name)
      match_val <- nilai_tengah$tengah[nilai_tengah$kel == state_num]
      
      if (length(match_val) > 0 && !is.na(match_val)) {
        nilai_mid[length(valid_bobot_indices) + 1] <- match_val
        valid_bobot_indices <- c(valid_bobot_indices, state_name)
      } else {
        cat("⚠️ Tidak ditemukan nilai tengah untuk state:", state_num, "\n")
      }
    }
    
    # Filter bobot to only include valid_bobot_indices
    bobot_filtered <- bobot[valid_bobot_indices]
    
    if (length(bobot_filtered) > 0 && sum(bobot_filtered) > 0 && !any(is.na(nilai_mid))) {
      hasil_defuzzifikasi[i] <- sum(as.numeric(bobot_filtered) * nilai_mid[1:length(bobot_filtered)]) # Ensure lengths match
    } else {
      # Fallback: if no valid weighted average, use midpoint of current state
      hasil_defuzzifikasi[i] <- nilai_tengah$tengah[nilai_tengah$kel == as.numeric(current_state)]
      cat("Peringatan: Defuzzifikasi Cheng gagal untuk state", current_state, ". Menggunakan nilai tengah current state.\n")
    }
  }
  return(hasil_defuzzifikasi)
}
defuzzified_values_cheng_flrg <- defuzzifikasi_cheng_flrg(FLRG, nilai_tengah)
print("Defuzzified Values (Cheng, per FLRG group):")
print(defuzzified_values_cheng_flrg)
write.xlsx(as.data.frame(defuzzified_values_cheng_flrg), "defuzz cheng.xlsx")

# Peramalan dgn FTS Cheng
cheng_forecast <- numeric(length(data))
cheng_forecast[1] <- NA # No forecast for the first data point

for (i in 1:(length(data) - 1)) {
  current_fuzzy_state <- as.character(fuzzyfikasi[i])
  if (current_fuzzy_state %in% names(defuzzified_values_cheng_flrg)) {
    cheng_forecast[i + 1] <- defuzzified_values_cheng_flrg[current_fuzzy_state]
  } else {
    # If the current fuzzy state is not in the FLRG (e.g., it's a new state or last one)
    # Default to its own midpoint
    cheng_forecast[i + 1] <- nilai_tengah$tengah[nilai_tengah$kel == as.numeric(current_fuzzy_state)]
  }
}
print("Peramalan Cheng (Time Series):")
print(cheng_forecast)

# Gabungkan hasil ke dalam data frame
dates <- index(data)
hasil <- data.frame(
  Tanggal = dates,
  Aktual = data,
  Fuzzyfikasi = fuzzyfikasi,
  Chen = chen_forecast,
  Cheng = cheng_forecast
)
hasil

#Menyimpan Data HAsil Peramalan ke Excel
library(openxlsx)
write.xlsx(as.data.frame(hasil), "hasil revisi.xlsx")

#PLOT TERPISAH
library(tidyr)
library(ggplot2)
hasil_panjang <- hasil %>%
  pivot_longer(cols = c(Chen, Cheng), names_to = "Metode", values_to = "Peramalan")
# Plot untuk Metode Chen
plot_chen <- ggplot(hasil_panjang %>% filter(Metode == "Chen"), aes(x = Tanggal)) +
  geom_line(aes(y = data, color = "Aktual")) +
  geom_line(aes(y = Peramalan, color = "Chen")) +
  labs(title = "Peramalan dengan Metode Chen", x = "Tanggal", y = "Harga") +
  scale_color_manual(values = c("Aktual" = "#16E959", "Chen" = "red")) +
  theme_minimal()

# Plot untuk Metode Cheng
plot_cheng <- ggplot(hasil_panjang %>% filter(Metode == "Cheng"), aes(x = Tanggal)) +
  geom_line(aes(y = data, color = "Aktual")) +
  geom_line(aes(y = Peramalan, color = "Cheng")) +
  labs(title = "Peramalan dengan Metode Cheng", x = "Tanggal", y = "Harga") +
  scale_color_manual(values = c("Aktual" = "#16E959", "Cheng" = "red")) +
  theme_minimal()

# Untuk menampilkan plot
print(plot_chen)
print(plot_cheng)

library(tidyr)
library(dplyr)
library(ggplot2)

# Pastikan kolom Tanggal dalam format Date
hasil_panjang$Tanggal <- as.Date(hasil_panjang$Tanggal)

# Hitung tanggal awal untuk 3 bulan terakhir (dengan base R)
tanggal_akhir <- max(hasil_panjang$Tanggal, na.rm = TRUE)
tanggal_awal <- as.Date(format(tanggal_akhir, "%Y-%m-%d")) - 90  # ~3 bulan (90 hari)

# Filter data 3 bulan terakhir
hasil_3bulan <- hasil_panjang %>%
  filter(Tanggal >= tanggal_awal)

# Plot gabungan
plot_gabungan <- ggplot(hasil_3bulan, aes(x = Tanggal)) +
  geom_line(aes(y = `BRIS.JK.Close`, color = "Aktual"), linewidth = 1) +
  geom_line(aes(y = Peramalan, color = Metode), linewidth = 1) +
  labs(title = "Peramalan 3 Bulan Terakhir: Chen vs Cheng",
       x = "Tanggal", y = "Harga") +
  scale_color_manual(values = c("Aktual" = "black", "Chen" = "skyblue", "Cheng" = "orange")) +
  theme_minimal()
# Tampilkan plot
print(plot_gabungan)

# Evaluasi peramalan
galat_chen <- abs(data$BRIS.JK.Close - chen_forecast)
galat_cheng <- abs(data$BRIS.JK.Close - cheng_forecast)
MAPE_chen <- mean(abs(galat_chen / data$BRIS.JK.Close * 100), na.rm = TRUE)
MAPE_cheng <- mean(abs(galat_cheng / data$BRIS.JK.Close * 100), na.rm = TRUE)

cat("MAPE Chen:", MAPE_chen, "%\n")
cat("MAPE Cheng:", MAPE_cheng, "%\n")

#Menyimpan Data galat ke Excel
library(openxlsx)
write.xlsx(as.data.frame(galat_chen), "galat chen1.xlsx")
write.xlsx(as.data.frame(galat_cheng), "galat cheng1.xlsx")

# Ambil fuzzyfikasi terakhir
last_fuzzy <- fuzzyfikasi[length(fuzzyfikasi)]

# Prediksi Cheng: Hitung rata-rata tertimbang berdasarkan matriks pembobotan
if (!is.null(bobot[last_fuzzy, ]) && sum(bobot[last_fuzzy, ]) > 0) {
  forecast_cheng_next <- sum(bobot_prob[last_fuzzy, ] * nilai_tengah$tengah)
} else {
  # Jika tidak ada probabilitas, fallback ke nilai tengah dari kelompok fuzzy terakhir
  forecast_cheng_next <- nilai_tengah$tengah[last_fuzzy]
}
forecast_cheng_next

# Inisialisasi
forecast_cheng <- c()  # Menyimpan hasil prediksi
fuzzy_seq <- fuzzyfikasi  # Copy dari fuzzyfikasi awal
current_fuzzy <- fuzzy_seq[length(fuzzy_seq)]  # Fuzzy terakhir

# Peramalan selama 7 hari
for (i in 1:7) {
  if (!is.null(bobot[current_fuzzy, ]) && sum(bobot[current_fuzzy, ]) > 0) {
    prediksi <- sum(bobot_prob[current_fuzzy, ] * nilai_tengah$tengah)
  } else {
    # Fallback: jika tidak ada relasi, gunakan nilai tengah dari current fuzzy
    prediksi <- nilai_tengah$tengah[current_fuzzy]
  }
  
  # Simpan hasil prediksi
  forecast_cheng[i] <- prediksi
  
  # Lakukan fuzzyfikasi terhadap prediksi untuk iterasi berikutnya
  selisih <- abs(nilai_tengah$tengah - prediksi)
  current_fuzzy <- which.min(selisih)  # Cari interval fuzzy terdekat
}

# Output hasil prediksi
forecast_cheng


library(dplyr)
library(tidyr)
library(ggplot2)

# Misal data adalah xts/zoo dengan kolom BRIS.JK.Close

# Ambil tanggal dari index data
Tanggal <- index(data)

# Buat dataframe hasil dengan kolom Tanggal dan prediksi Chen, Cheng
hasil <- data.frame(
  Tanggal = Tanggal,
  Chen = chen_forecast,   # pastikan ini numeric vector sesuai tanggal
  Cheng = cheng_forecast  # sama
)

# Tambahkan kolom Aktual jadi numeric vector dari xts
hasil$Aktual <- as.numeric(coredata(data$BRIS.JK.Close))

# Sekarang pivot_longer aman karena semua numeric
hasil_panjang <- hasil %>%
  pivot_longer(cols = c("Aktual", "Chen", "Cheng"),
               names_to = "Metode",
               values_to = "Peramalan")

# Filter 3 bulan terakhir
tanggal_akhir <- max(hasil_panjang$Tanggal, na.rm = TRUE)
tanggal_awal_3bulan <- tanggal_akhir - 90
hasil_3bulan <- hasil_panjang %>%
  filter(Tanggal >= tanggal_awal_3bulan)

# Gabungkan forecast_point_df yang sudah punya kolom Tanggal, Metode, Peramalan
library(dplyr)
hasil_3bulan_with_forecast <- bind_rows(hasil_3bulan, forecast_point_df)

# Plot
ggplot(hasil_3bulan_with_forecast, aes(x = Tanggal)) +
  geom_line(aes(y = Peramalan, color = Metode), linewidth = 1) +
  geom_point(data = forecast_point_df, aes(y = Peramalan, color = Metode),
             shape = 8, size = 3, stroke = 1.5) +
  geom_segment(data = connecting_segment_df, aes(x = x1, y = y1, xend = x2, yend = y2, color = Metode),
               linetype = "solid", size = 1, inherit.aes = FALSE) +
  scale_color_manual(values = c(
    "Aktual" = "#16E959",
    "Chen" = "red",
    "Cheng" = "blue",
    "Cheng Forecast" = "#eb1491"
  )) +
  labs(title = "Peramalan 3 Bulan Terakhir: Chen vs Cheng dengan Prediksi Selanjutnya",
       x = "Tanggal", y = "Harga") +
  theme_minimal() +
  theme(legend.position = "bottom")

# PLOT CHEN DAN CHENG DAN & 7 hari peramalan
# Asumsikan forecast_cheng sudah berisi hasil peramalan 7 hari
# Buat tanggal peramalan dimulai dari 2 Januari 2025
tanggal_forecast <- seq(as.Date("2025-01-02"), by = "day", length.out = 7)


# 1. Pisahkan data Cheng dari hasil_3bulan untuk diproses
cheng_historis_df <- hasil_3bulan %>% filter(Metode == "Cheng")

# 2. Buat dataframe forecast Cheng, pastikan 'Metode' sama dengan historisnya ('Cheng')
forecast_point_df <- data.frame(
  Tanggal = tanggal_forecast,
  Metode = "Cheng", # PENTING: Harus "Cheng" agar geom_line menyambungkan
  Peramalan = forecast_cheng
)

# 3. Gabungkan data Cheng historis dan Cheng forecast menjadi satu dataframe 'Cheng'
cheng_all_data <- bind_rows(cheng_historis_df, forecast_point_df) %>%
  arrange(Tanggal) # Pastikan diurutkan berdasarkan tanggal

# 4. Tambahkan kolom 'PlotColor' atau 'SegmentType' ke data Cheng
cheng_all_data <- cheng_all_data %>%
  mutate(PlotColor = ifelse(
    Tanggal > max(cheng_historis_df$Tanggal),
    "Cheng Forecast",
    "Cheng Historis"
  ))

# 5. Gabungkan kembali semua data untuk plotting
#    Pertama, saring data Cheng historis dari hasil_3bulan asli.
hasil_3bulan_tanpa_cheng <- hasil_3bulan %>% filter(Metode != "Cheng")

#    Kemudian, gabungkan data historis tanpa Cheng, dengan cheng_all_data.
#    Di sini, kita juga perlu menambahkan 'PlotColor' untuk "Aktual" dan "Chen"
#    agar mereka juga terdaftar di scale_color_manual.
final_plot_data <- bind_rows(hasil_3bulan_tanpa_cheng, cheng_all_data) %>%
  mutate(PlotColor = case_when(
    Metode == "Aktual" ~ "Aktual",
    Metode == "Chen" ~ "Chen",
    TRUE ~ PlotColor # Ini akan menggunakan PlotColor yang sudah dibuat untuk Cheng
  ))


# 6. Atur urutan level factor untuk 'PlotColor' untuk kontrol legenda
final_plot_data$PlotColor <- factor(final_plot_data$PlotColor,
                                    levels = c("Aktual", "Chen", "Cheng Historis", "Cheng Forecast"))

# 7. Plotting
ggplot(final_plot_data, aes(x = Tanggal)) +
  # Gunakan satu geom_line saja untuk semua data, dengan 'group = Metode'
  # untuk memastikan garis yang berbeda tidak terhubung,
  # dan 'color = PlotColor' untuk mengatur warna.
  geom_line(aes(y = Peramalan, color = PlotColor, group = Metode), linewidth = 1) +
  
  # Scale warna manual untuk semua kategori PlotColor
  scale_color_manual(values = c(
    "Aktual" = "#16E959",
    "Chen" = "red",
    "Cheng Historis" = "blue",      # Warna untuk bagian historis Cheng
    "Cheng Forecast" = "#eb1491"    # Warna khusus Cheng 7 hari ke depan (magenta)
  )) +
  labs(title = "Peramalan 3 Bulan Terakhir + 7 Hari ke Depan (Cheng)",
       x = "Tanggal", y = "Harga") +
  theme_minimal() +
  theme(legend.position = "bottom")

library(quantmod)
library(dplyr)
library(ggplot2)
library(lubridate)

# 2. Ambil data aktual dari Yahoo Finance
# Misal saham BRIS.JK, ambil dari 3 bulan sebelum 2025-01-02 hingga 7 hari sesudah
start_date <- as.Date("2024-10-02")  # kira-kira 3 bulan sebelum 2 Jan 2025
end_date <- as.Date("2025-01-09")    # 7 hari setelah 2 Jan 2025

getSymbols("BRIS.JK", src = "yahoo", from = start_date, to = end_date, auto.assign = TRUE)
aktual_df <- `BRIS.JK` %>%
  as.data.frame() %>%
  mutate(Tanggal = as.Date(rownames(.))) %>%
  select(Tanggal, Close = `BRIS.JK.Close`) %>%
  rename(Peramalan = Close) %>%
  mutate(Metode = "Aktual")

# 3. Gabungkan dengan hasil_3bulan (Chen dan Cheng historis)
# Misal `hasil_3bulan` sudah memuat data dari metode Chen dan Cheng dalam format:
# Tanggal | Metode | Peramalan

# 4. Forecast Cheng (asumsikan sudah tersedia dalam variabel `forecast_cheng`)
tanggal_forecast <- seq(as.Date("2025-01-02"), by = "day", length.out = 7)

forecast_point_df <- data.frame(
  Tanggal = tanggal_forecast,
  Metode = "Cheng",
  Peramalan = forecast_cheng
)

# 5. Data historis Cheng
cheng_historis_df <- hasil_3bulan %>% filter(Metode == "Cheng")

# 6. Gabungkan historis + forecast Cheng
cheng_all_data <- bind_rows(cheng_historis_df, forecast_point_df) %>%
  arrange(Tanggal) %>%
  mutate(PlotColor = ifelse(Tanggal > max(cheng_historis_df$Tanggal), "Cheng Forecast", "Cheng Historis"))

# 7. Gabungkan seluruh data untuk plot
hasil_3bulan_tanpa_cheng <- hasil_3bulan %>% filter(Metode != "Cheng")

final_plot_data <- bind_rows(hasil_3bulan_tanpa_cheng, cheng_all_data, aktual_df) %>%
  mutate(PlotColor = case_when(
    Metode == "Aktual" ~ "Aktual",
    Metode == "Chen" ~ "Chen",
    TRUE ~ PlotColor
  ))

# 8. Set faktor warna
final_plot_data$PlotColor <- factor(final_plot_data$PlotColor,
                                    levels = c("Aktual", "Chen", "Cheng Historis", "Cheng Forecast"))

# 9. Plot
ggplot(final_plot_data, aes(x = Tanggal)) +
  geom_line(aes(y = Peramalan, color = PlotColor, group = Metode), linewidth = 1) +
  scale_color_manual(values = c(
    "Aktual" = "#16E959",
    "Chen" = "red",
    "Cheng Historis" = "blue",
    "Cheng Forecast" = "#eb1491"
  )) +
  labs(title = "Peramalan 3 Bulan Terakhir + 7 Hari ke Depan (Cheng)",
       x = "Tanggal", y = "Harga") +
  theme_minimal() +
  theme(legend.position = "bottom")
