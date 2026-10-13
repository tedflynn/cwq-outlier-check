# Builds examples/example_station.csv: 90 days of synthetic 15-minute sonde data with known problems planted in it,
# so the checks (and the tests) have known answers. The layout follows the DWR/NCRO export:
# Station_Code, Date_Time (month/day/year), Water_Temperature, Specific_Conductance, DO, Turbidity, pH, Chlorophyll.
#
# Planted problems (rows are 0-based steps from 2024-07-01 00:00; one day is 96 steps):
#   turbidity   five single-reading spikes; a 6 h rise on day 30 that conductivity and DO also show (a real event);
#               a 5 h rise on day 45 that nothing else shows (fouling); an erratic stretch on days 70-72
#   dissolved oxygen   a flat line of 8 h on day 60
#   pH          three readings of 277; water temperature two readings of 9999
#   chlorophyll no readings for two days; other parameters a few scattered gaps and four missing rows
set.seed(42)
n <- 90 * 96
t <- seq(as.POSIXct("2024-07-01 00:00:00", tz = "Etc/GMT+8"), by = "15 min", length.out = n)
hour <- as.numeric(format(t, "%H")) + as.numeric(format(t, "%M")) / 60
tide <- sin(2 * pi * seq_len(n) / (12.42 * 4))
diurnal <- sin(2 * pi * (hour - 9) / 24)
smooth_noise <- function(sd) as.numeric(stats::filter(rnorm(n, 0, sd), 0.9, method = "recursive"))

temp <- 22 + 2 * diurnal + 0.01 * rnorm(n)
ph   <- 7.9 + 0.15 * diurnal + 0.01 * rnorm(n)
cond <- 450 + 40 * tide + 3 * rnorm(n)
do   <- 8 + 1.2 * diurnal + 0.05 * rnorm(n)
chl  <- pmax(1, 6 + 2 * diurnal + smooth_noise(0.15) + 0.2 * rnorm(n))
turb <- pmax(0.5, 8 + 2 * tide + smooth_noise(0.5) + 0.3 * rnorm(n))

day <- function(d, from_h = 0, hours = 1) { i0 <- d * 96 + from_h * 4; i0 + seq_len(hours * 4) }

turb[c(day(10, 5)[1], day(18, 14)[1], day(25, 3)[1], day(38, 20)[1], day(52, 11)[1])] <- turb[c(day(10, 5)[1], day(18, 14)[1], day(25, 3)[1], day(38, 20)[1], day(52, 11)[1])] + 60
storm <- day(30, 8, 2)
turb[storm] <- turb[storm] + 45 * sin(seq(0.2, 3, length.out = length(storm)))
cond[storm] <- cond[storm] - 120 * sin(seq(0.2, 3, length.out = length(storm)))
do[storm]   <- do[storm] - 1.5 * sin(seq(0.2, 3, length.out = length(storm)))
foul <- day(45, 6, 2)
turb[foul] <- turb[foul] + 40
erratic <- day(70, 0, 48)
turb[erratic] <- turb[erratic] + rnorm(length(erratic), 0, 30)
turb <- pmax(turb, 0.5)
do[day(60, 12, 8)] <- do[day(60, 12, 8)[1]]
ph[c(day(15, 4)[1], day(15, 4)[1] + 1, day(33, 9)[1])] <- 277
temp[c(day(22, 7)[1], day(22, 7)[1] + 1)] <- 9999
chl[day(40, 0, 48)] <- NA
for (v in c("temp", "ph", "cond", "do", "turb")) { x <- get(v); x[sample(n, 25)] <- NA; assign(v, x) }

out <- data.frame(Station_Code = "EXA", Date_Time = format(t, "%m/%d/%Y %H:%M:%S"),
                  Water_Temperature = round(temp, 2), Specific_Conductance = round(cond), DO = round(do, 2),
                  Turbidity = round(turb, 1), pH = round(ph, 2), Chlorophyll = round(chl, 2))
out <- out[-c(300, 301, 4000, 6500), ]          # four rows missing altogether
write.csv(out, file.path("examples", "example_station.csv"), row.names = FALSE, na = "")
cat("wrote examples/example_station.csv:", nrow(out), "rows\n")
