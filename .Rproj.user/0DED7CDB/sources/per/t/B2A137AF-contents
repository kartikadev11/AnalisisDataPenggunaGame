library(readxl) #mengaktifkan packege xlsx
datamhs = read_xlsx("dataGame.xlsx") #Membuat function datamhs untuk
                                      #membaca file xlsx yaitu dataGame.xlsx

str(datamhs) #memanggil function datamhs untuk mengetahui 
            #class dari masing-masing variabel

#GAME YANG PALING DIMINATI

datamhs$nama_game #melihat isi kolom nama_game pada tabel datamhs

tabel = table(datamhs$nama_game) 
tabel
cbind(tabel)

max(table(datamhs$nama_game)) # menampilkan nilai frekuensi tertinggi


barplot(tabel)

#Distribusi Frekuensi Relatif
options(digits = 1)#jml angka setelah koma
prop.table(table(datamhs$nama_game))

#Kemudian dibuat dalam bentuk prosentase
options(digits = 2)# jml angka setelah koma
prop.table(table(datamhs$nama_game))*100

tabel = table(datamhs$nama_game) #membuat function tabel yang berisi distibusi frekuensi variabel nama_game
cbind(tabel) #menampilkan format kolom distribusi frekuesi
barplot(tabel) #membuat Bar Graph dari data kualitatif yang menampilkan distribusi frekuensi variabel nama_game

#PLATFORM YANG PALING BANYAK DIGUNAKAN UNTUK BERMAIN GAME

library(dplyr)
datamhs = mutate(datamhs,Jenis.Kelamin = 
                   if_else(datamhs$jk==0,"Laki-laki","Perempuan"),
                 Platform = if_else(datamhs$platform_game==0,"PC","Mobile"))
table(datamhs$Jenis.Kelamin)
prop.table(table(datamhs$Jenis.Kelamin))*100

table(datamhs$Platform)
prop.table(table(datamhs$Platform))*100

addmargins(table(datamhs$Platform,datamhs$Jenis.Kelamin))
prop.table(table(datamhs$Platform,datamhs$Jenis.Kelamin), margin = 2)*100
barplot(prop.table(table(datamhs$Platform,datamhs$Jenis.Kelamin), 
                   margin = 2)*100,beside = TRUE,
        legend.text = TRUE, ylab = "Persen",ylim = c(0,100))


#WAKTU MAIN TERLAMA

table(datamhs$waktu_main)
summary(datamhs$waktu_main)
range(datamhs$waktu_main)
sub.interval = seq(0, 12, by=2)
sub.interval
cut.durasi = cut(datamhs$waktu_main, sub.interval, right = TRUE)
cut.durasi
durasi.frek = table(cut.durasi) 
durasi.frek
cbind(durasi.frek)
barplot(durasi.frek, main = "Visualisasi data durasi waktu bermain game mahasiswa D4 TI"
        , xlab = "Durasi Waktu Bermain Game", ylab = "Jumlah Mahasiswa")



durasi = datamhs$waktu_main
addmargins(table(cut(datamhs$waktu_main, sub.interval, right = TRUE),
                 datamhs$Jenis.Kelamin))

prop.table(table(cut(datamhs$waktu_main, sub.interval, right = TRUE),datamhs$Jenis.Kelamin),margin = 2)*100




warna_kolom = c("red","orange","yellow","green","blue","violet")
barplot(table(cut(datamhs$waktu_main, sub.interval, right = TRUE),datamhs$Jenis.Kelamin),beside = TRUE,
        legend.text = TRUE, main = "Durasi Waktu Bermain Game",xlab = "Durasi Waktu (jam)", ylab = "Jumlah Mahasiswa",ylim = c(0,50), col = warna_kolom )



prop.table(table(datamhs$Jenis.Kelamin,cut(datamhs$waktu_main, sub.interval, right = TRUE)),margin = 2)*100
barplot(prop.table(table(datamhs$Jenis.Kelamin,cut(datamhs$waktu_main, sub.interval, right = TRUE)),margin = 2)*100,beside = TRUE,
        legend.text = TRUE, main = "Durasi Waktu Bermain Game",xlab = "Durasi Waktu (jam)", ylab = "Persen")


barplot(prop.table(table(datamhs$Jenis.Kelamin,cut(datamhs$waktu_main, sub.interval, right = TRUE)),margin = 2)*100,beside = TRUE,
        legend.text = TRUE, main = "Durasi Waktu Bermain Game",xlab = "Durasi Waktu (jam)", ylab = "Persen")


#PENGELUARAN TERBANYAK
table(datamhs$pengeluaran)
summary(datamhs$pengeluaran)
range(datamhs$pengeluaran)
sub.interval = seq(0, 1100000, by=100000)
sub.interval
cut.durasi = cut(datamhs$pengeluaran, sub.interval, right = FALSE)
cut.durasi
durasi.frek = table(cut.durasi) 
durasi.frek
cbind(durasi.frek)
barplot(durasi.frek,main = "Pengeluaran uang untuk pembelian dalam game ", xlab = "Pengeluaran uang (Rupiah)",ylab = "Jumlah mahasiswa",ylim = c(0,80))

durasi = datamhs$pengeluaran
hist(durasi,right = FALSE, main = "Pengeluaran uang untuk pembelian dalam game ", xlab = "Pengeluaran uang (Rupiah)",ylim = c(0,80))

addmargins(table(cut(datamhs$pengeluaran, sub.interval, right = FALSE),datamhs$Jenis.Kelamin))


warna = c("red","orange","yellow","green","blue","violet","pink","brown","grey","cyan","black")
barplot(table(cut(datamhs$pengeluaran, sub.interval, right = FALSE),datamhs$Jenis.Kelamin),beside = TRUE,
        legend.text = TRUE, main =  "Pengeluaran uang untuk pembelian dalam game ", xlab = "Pengeluaran uang (Rupiah)", 
        ylab = "Jumlah mahasiswa",ylim = c(0,50),col = warna)



prop.table(table(cut(datamhs$pengeluaran, sub.interval, right = FALSE),datamhs$Jenis.Kelamin),margin = 2)*100
barplot(prop.table(table(datamhs$Jenis.Kelamin,cut(datamhs$pengeluaran, sub.interval, right = FALSE)),margin = 2)*100,beside = TRUE,
        legend.text = TRUE, main =  "Pengeluaran uang untuk pembelian dalam game ", xlab = "Pengeluaran uang (Rupiah)", ylab = "Persen")

barplot(addmargins(table(datamhs$Jenis.Kelamin,cut(datamhs$waktu_main, sub.interval, right = FALSE))),beside = TRUE,legend.text = TRUE, main = "Durasi Waktu Bermain Game",xlab = "Durasi Waktu (jam)", ylab = "Jumlah Mahasiswa")

#PENGELUARA (PAY/FREE)

library(dplyr)
datamhs = mutate(datamhs,Jenis.Kelamin = 
                   if_else(datamhs$jk==0,"Laki-laki","Perempuan"),
                 biaya = if_else(datamhs$pengeluaran==0,"FREE","PAY"))
table(datamhs$Jenis.Kelamin)
prop.table(table(datamhs$Jenis.Kelamin))*100

table(datamhs$biaya)
prop.table(table(datamhs$biaya))*100

addmargins(table(datamhs$biaya,datamhs$Jenis.Kelamin))
prop.table(table(datamhs$biaya,datamhs$Jenis.Kelamin), margin = 2)*100
barplot(prop.table(table(datamhs$biaya,datamhs$Jenis.Kelamin), 
                   margin = 2)*100,beside = TRUE,
        legend.text = TRUE, ylab = "Persen",ylim = c(0,80))

#Mencari hubungan/korelasi waktu_main dengan pengeluaran

lama_bermain = datamhs$waktu_main
biaya_dikeluarkan = datamhs$pengeluaran
cor.test(lama_bermain,biaya_dikeluarkan)
cor(datamhs$waktu_main,datamhs$pengeluaran)
cov(datamhs$waktu_main,datamhs$pengeluaran)

summary(datamhs$waktu_main)
summary(datamhs$pengeluaran)

boxplot(datamhs$waktu_main, horizontal = TRUE)
boxplot(datamhs$pengeluaran, horizontal = TRUE)

# Analisis Korelasi terhadap variable waktu_main dan pengeluaran
lama_bermain = datamhs$waktu_main
biaya_dikeluarkan = datamhs$pengeluaran
cor.test(lama_bermain,biaya_dikeluarkan)

plot(datamhs$waktu_main, datamhs$pengeluaran, main="Scatterplot waktu_main vs pengeluaran",xlab="waktu_main",ylab="pengeluaran", pch=20,las=1)
abline(lm(datamhs$pengeluaran~datamhs$waktu_main),col="red")

