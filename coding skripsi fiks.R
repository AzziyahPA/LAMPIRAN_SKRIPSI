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
n <- length(X)
n
delta_X <- abs(diff(X))  # Menghitung perubahan absolut antar nilai
delta_X
write.xlsx(as.data.frame(delta_X), "selisih data.xlsx")
sum(delta_X)
mean_delta <- sum(delta_X)/n  # Menghitung rata-rata perubahan
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
fuzzyfikasi <- numeric(length(X))
for (i in 1:length(X)) {
  for (j in 1:nrow(box1)) {
    if (X[i] >= box1$bawah[j] & X[i] < box1$atas[j]) {
      fuzzyfikasi[i] = box1$kel[j]
      break
    }
    if (X[i] == box1$atas[j] & j == nrow(box1)) {
      fuzzyfikasi[i] = box1$kel[j]
    }
  }
}
print("Fuzzyfikasi Data:")
print(fuzzyfikasi)

#Menggabungkan hasil dalam DataFrame
Fuzzyfi <- data.frame(Data = X, Fuzzyfikasi = fuzzyfikasi)
print("Hasil Fuzzyfikasi:")
print(Fuzzyfi)


#Fuzzyfikasi ke data asal
Fuzzyfi <- data.frame(Data = X, Fuzzyfikasi = fuzzyfikasi)
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
chen_forecast_final <- numeric(length(data))
chen_forecast_final[1] <- NA # No forecast for the first data point
for (i in 1:(length(data) - 1)) {
  current_fuzzy_state <- as.character(fuzzyfikasi[i])
  if (current_fuzzy_state %in% names(defuzzified_values_chen_flrg)) {
    chen_forecast_final[i + 1] <- defuzzified_values_chen_flrg[current_fuzzy_state]
  } else {
    # If the current fuzzy state is not in the FLRG (e.g., it's a new state or last one)
    # Default to its own midpoint
    chen_forecast_final[i + 1] <- nilai_tengah$tengah[nilai_tengah$kel == as.numeric(current_fuzzy_state)]
  }
}
print("Peramalan Chen (Time Series):")
print(chen_forecast_final)

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
FLRG_freq <- FLR %>%
  count(Current, Next) %>%
  tidyr::pivot_wider(names_from = Next, values_from = n, values_fill = 0) %>%
  tibble::column_to_rownames("Current")
library(ggplot2)
# Ubah matrix jadi long format
library(reshape2)
df_long <- melt(as.matrix(FLRG_freq))
ggplot(df_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "yellow", high = "black") +
  labs(title = "Heatmap Matriks Pembobot",
       x = "Current State", y = "Next State") +
  theme_minimal()

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
    colors = c("white", "blue",  # 0-3 (kuning muda ??? hijau muda)
               "green", "#008000",  # 4-8 (hijau ??? hijau tua)
               "yellow", "red",  # 9-12 (hijau gelap ??? biru)
               "brown", "black"), # 13-16 (biru tua ??? ungu)
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
df_prob <- melt(bobot_prob)
ggplot(df_prob, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "navy") +
  labs(title = "Heatmap Matriks Bobot Probabilistik (Cheng)",
       x = "Currrent State", y = "Next State") +
  theme_minimal()

# Plot heatmap
ggplot(df_prob, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(
    colors = c("white", "#008000",     # 0-0.25 (kuning muda ke hijau muda)
               "orange", "red",     # 0.26-0.5 (hijau ke hijau tua)
               "blue", "navy",     # 0.6-0.75 (hijau gelap ke biru)
               "purple", "black"),    # 0.76-1 (biru tua ke ungu)
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
        cat("?????? Tidak ditemukan nilai tengah untuk state:", state_num, "\n")
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
cheng_forecast_final <- numeric(length(data))
cheng_forecast_final[1] <- NA # No forecast for the first data point

for (i in 1:(length(data) - 1)) {
  current_fuzzy_state <- as.character(fuzzyfikasi[i])
  if (current_fuzzy_state %in% names(defuzzified_values_cheng_flrg)) {
    cheng_forecast_final[i + 1] <- defuzzified_values_cheng_flrg[current_fuzzy_state]
  } else {
    # If the current fuzzy state is not in the FLRG (e.g., it's a new state or last one)
    # Default to its own midpoint
    cheng_forecast_final[i + 1] <- nilai_tengah$tengah[nilai_tengah$kel == as.numeric(current_fuzzy_state)]
  }
}
print("Peramalan Cheng (Time Series):")
print(cheng_forecast_final)

# Gabungkan hasil ke dalam data frame
dates <- index(data)
hasil <- data.frame(
  Tanggal = dates,
  Aktual = X,
  Fuzzyfikasi = fuzzyfikasi,
  Chen = chen_forecast_final,
  Cheng = cheng_forecast_final
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
  geom_line(aes(y = Aktual, color = "Aktual")) +
  geom_line(aes(y = Peramalan, color = "Chen")) +
  labs(title = "Peramalan dengan Metode Chen", x = "Tanggal", y = "Harga") +
  scale_color_manual(values = c("Aktual" = "black", "Chen" = "red")) +
  theme_minimal()

# Plot untuk Metode Cheng
plot_cheng <- ggplot(hasil_panjang %>% filter(Metode == "Cheng"), aes(x = Tanggal)) +
  geom_line(aes(y = Aktual, color = "Aktual")) +
  geom_line(aes(y = Peramalan, color = "Cheng")) +
  labs(title = "Peramalan dengan Metode Cheng", x = "Tanggal", y = "Harga") +
  scale_color_manual(values = c("Aktual" = "black", "Cheng" = "blue")) +
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
  geom_line(aes(y = Aktual, color = "Aktual"), size = 1) +
  geom_line(aes(y = Peramalan, color = Metode), size = 1) +
  labs(title = "Peramalan 3 Bulan Terakhir: Chen vs Cheng",
       x = "Tanggal", y = "Harga") +
  scale_color_manual(values = c("Aktual" = "black", "Chen" = "red", "Cheng" = "blue")) +
  theme_minimal()
# Tampilkan plot
print(plot_gabungan)

# Evaluasi peramalan
galat_chen <- abs(X - chen_forecast_original)
galat_cheng <- abs(X - cheng_forecast_original)
MAPE_chen <- mean(abs(galat_chen / X * 100), na.rm = TRUE)
MAPE_cheng <- mean(abs(galat_cheng / X * 100), na.rm = TRUE)

cat("MAPE Chen:", MAPE_chen, "%\n")
cat("MAPE Cheng:", MAPE_cheng, "%\n")

#Menyimpan Data galat ke Excel
library(openxlsx)
write.xlsx(as.data.frame(galat_chen), "galat chen1.xlsx")
write.xlsx(as.data.frame(galat_cheng), "galat cheng1.xlsx")

# Ambil fuzzyfikasi terakhir
last_fuzzy <- fuzzyfikasi[length(fuzzyfikasi)]

# Prediksi Chen: Gunakan nilai tengah dari fuzzy terakhir
forecast_chen_next <- nilai_tengah$tengah[last_fuzzy]
forecast_chen_next


# Calculate the single next-step forecast for Cheng
last_fuzzy_state_num <- fuzzyfikasi[length(fuzzyfikasi)]
last_fuzzy_state_char <- as.character(last_fuzzy_state_num)

if (last_fuzzy_state_char %in% rownames(bobot_prob)) {
  weights_for_last_state <- bobot_prob[last_fuzzy_state_char, ]
  forecast_cheng_next_step <- 0
  for (next_state_char in colnames(bobot_prob)) {
    prob <- weights_for_last_state[next_state_char]
    next_state_num <- as.numeric(next_state_char)
    if (prob > 0 && next_state_num %in% names(nilai_tengah_map)) {
      midpoint_val <- nilai_tengah_map[as.character(next_state_num)]
      forecast_cheng_next_step <- forecast_cheng_next_step + (prob * midpoint_val)
    }
  }
} else {
  forecast_cheng_next_step <- nilai_tengah_map[last_fuzzy_state_char]
}
cat("Prediksi Cheng (One-Step Ahead):", forecast_cheng_next_step, "\n")


# --- 6. Combine and Plot Data ---
# Gabungkan hasil ke dalam data frame
dates <- index(data)
hasil <- data.frame(
  Tanggal = dates,
  Aktual = X,
  Fuzzyfikasi = fuzzyfikasi,
  Chen = chen_forecast_original,
  Cheng = cheng_forecast_original
)

# Pivot to long format for plotting
hasil_panjang <- hasil %>%
  pivot_longer(cols = c(Chen, Cheng), names_to = "Metode", values_to = "Peramalan")

# Ensure Tanggal is in Date format
hasil_panjang$Tanggal <- as.Date(hasil_panjang$Tanggal)

# Filter data for the last 3 months
tanggal_akhir <- max(hasil_panjang$Tanggal, na.rm = TRUE)
tanggal_awal_3bulan <- tanggal_akhir - days(90) # ~3 months (90 days)

hasil_3bulan <- hasil_panjang %>%
  filter(Tanggal >= tanggal_awal_3bulan)

# Get the last date from your historical data to plot the forecast point
last_actual_date <- max(hasil_panjang$Tanggal, na.rm = TRUE)

# Create a data frame for the forecast point (this uses 'forecast_cheng_next_step')
forecast_point_df <- data.frame(
  Tanggal = last_actual_date + days(1), # Forecast is for the next day
  Aktual = NA, # No actual data for the forecast point
  Fuzzyfikasi = NA, # This column is crucial for rbind compatibility
  Metode = "Cheng Forecast", # A new category for the single forecast point
  Peramalan = forecast_cheng_next_step
)

# Combine the 3-month historical data with the new forecast point
hasil_3bulan_with_forecast <- rbind(hasil_3bulan, forecast_point_df)

# Plot gabungan with the added forecast point
plot_gabungan_with_forecast <- ggplot(hasil_3bulan_with_forecast, aes(x = Tanggal)) +
  geom_line(aes(y = Aktual, color = "Aktual"), size = 1) +
  geom_line(aes(y = Peramalan, color = Metode), size = 1) +
  # Add the specific forecast point with a distinct shape and color
  geom_point(data = forecast_point_df, aes(y = Peramalan, color = Metode),
             shape = 8, size = 3, stroke = 1.5) + # Shape 8 is a star, stroke makes it bolder
  labs(title = "Peramalan 3 Bulan Terakhir: Chen vs Cheng dengan Prediksi Selanjutnya",
       x = "Tanggal", y = "Harga") +
  scale_color_manual(values = c("Aktual" = "black",
                                "Chen" = "red",
                                "Cheng" = "blue",
                                "Cheng Forecast" = "darkgreen")) + # Distinct color for the forecast point
  theme_minimal() +
  theme(legend.position = "bottom")

# Tampilkan plot
print(plot_gabungan_with_forecast)

# --- 6. Combine and Plot Data ---
# Gabungkan hasil ke dalam data frame
dates <- index(data)
hasil <- data.frame(
  Tanggal = dates,
  Aktual = X,
  Fuzzyfikasi = fuzzyfikasi,
  Chen = chen_forecast_original,
  Cheng = cheng_forecast_original
)

# Pivot to long format for plotting
hasil_panjang <- hasil %>%
  pivot_longer(cols = c(Chen, Cheng), names_to = "Metode", values_to = "Peramalan")

# Ensure Tanggal is in Date format
hasil_panjang$Tanggal <- as.Date(hasil_panjang$Tanggal)

# Filter data for the last 3 months
tanggal_akhir <- max(hasil_panjang$Tanggal, na.rm = TRUE)
tanggal_awal_3bulan <- tanggal_akhir - days(90) # ~3 months (90 days)

hasil_3bulan <- hasil_panjang %>%
  filter(Tanggal >= tanggal_awal_3bulan)


# Get the last date from your historical data (this will be the last trading day)
last_actual_trading_date <- max(hasil_panjang$Tanggal, na.rm = TRUE)

# --- Determine the next trading day for the forecast point ---
# This function finds the next weekday, skipping Saturday and Sunday.
# It does NOT account for public holidays (e.g., New Year's Day).
# For specific year-end behavior (Dec 30 -> Jan 2), this simple function might not be enough.
get_next_weekday <- function(current_date) {
  next_day <- current_date + days(1)
  # Loop until it's not a Saturday (6) or Sunday (7) - assuming week_start = 1 (Monday)
  while (wday(next_day, week_start = 1) %in% c(6, 7)) {
    next_day <- next_day + days(1)
  }
  return(next_day)
}

# Apply custom logic for the specific "Dec 30 -> Jan 2" scenario for 2024
# If you run this in a different year, you might need to adjust this conditional.
if (format(last_actual_trading_date, "%Y-%m-%d") == "2024-12-30") {
  forecast_date_for_plot <- as.Date("2025-01-02") # Hardcode if 31st and 1st are non-trading
} else {
  forecast_date_for_plot <- get_next_weekday(last_actual_trading_date)
}


# Create a data frame for the forecast point
forecast_point_df <- data.frame(
  Tanggal = forecast_date_for_plot, # Use the determined 'next trading day'
  Aktual = NA,
  Fuzzyfikasi = NA, # Crucial for rbind compatibility
  Metode = "Cheng Forecast",
  Peramalan = forecast_cheng_next_step
)

# Find the last point of the 'Cheng' forecast line within the 3-month window
last_cheng_forecast_data <- hasil_3bulan %>%
  filter(Metode == "Cheng") %>%
  arrange(Tanggal) %>%
  slice_tail(n = 1)

# Create a data frame for the connecting segment
connecting_segment_df <- data.frame(
  x1 = last_cheng_forecast_data$Tanggal,
  y1 = last_cheng_forecast_data$Peramalan,
  x2 = forecast_point_df$Tanggal, # Connect to the new forecast date
  y2 = forecast_point_df$Peramalan,
  Metode = "Cheng Forecast"
)

# Combine the 3-month historical data with the new forecast point
hasil_3bulan_with_forecast <- rbind(hasil_3bulan, forecast_point_df)

# Plot gabungan with the added forecast point and connecting segment
plot_gabungan_with_forecast <- ggplot(hasil_3bulan_with_forecast, aes(x = Tanggal)) +
  geom_line(aes(y = Aktual, color = "Aktual"), size = 1) +
  geom_line(aes(y = Peramalan, color = Metode), size = 1) +
  
  # Add the specific forecast point with a distinct shape and color
  geom_point(data = forecast_point_df, aes(y = Peramalan, color = Metode),
             shape = 8, size = 3, stroke = 1.5) +
  
  # Add the connecting segment from the last Cheng forecast to the new point
  geom_segment(data = connecting_segment_df, aes(x = x1, y = y1, xend = x2, yend = y2, color = Metode),
               linetype = "solid", size = 1, inherit.aes = FALSE) +
  
  labs(title = "Peramalan 3 Bulan Terakhir: Chen vs Cheng dengan Prediksi Selanjutnya",
       x = "Tanggal", y = "Harga") +
  scale_color_manual(values = c("Aktual" = "black",
                                "Chen" = "red",
                                "Cheng" = "blue",
                                "Cheng Forecast" = "darkgreen")) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Tampilkan plot
print(plot_gabungan_with_forecast)
