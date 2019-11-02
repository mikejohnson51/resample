
{cols = c(adjustcolor( "darkgreen", alpha.f = 0.2), adjustcolor( "red", alpha.f = 0.2), adjustcolor( "blue", alpha.f = 0.2), adjustcolor( "purple", alpha.f = 0.2))


p.1 = vector()
p.2 = vector()
p.3 = vector()


u.1 = vector()
u.2 = vector()
u.3 = vector()


overall.1 = vector()
overall.2 = vector()
overall.3 = vector()

low.1 = vector()
low.2 = vector()
low.3 = vector()


high.1 = vector()
high.2 = vector()
high.3 = vector()

m.1 = vector()
m.2 = vector()
m.3 = vector()


for(j in 1:length(mats)){
  tmp = mats[[j]]

  p.1[j] = tmp[1,1]
  p.2[j] = tmp[2,1]
  p.3[j] = tmp[3,1]


  u.1[j] = tmp[1,2]
  u.2[j] = tmp[2,2]
  u.3[j] = tmp[3,2]


  overall.1[j] = tmp[1,3] * 100
  overall.2[j] = tmp[2,3] * 100
  overall.3[j] = tmp[3,3] * 100


  low.1[j] = abs(tmp[1,4])
  low.2[j] = abs(tmp[2,4])
  low.3[j] = abs(tmp[3,4])


  high.1[j] = tmp[1,5]
  high.2[j] = tmp[2,5]
  high.3[j] = tmp[3,5]


  m.1[j] = tmp[1,6]
  m.2[j] = tmp[2,6]
  m.3[j] = tmp[3,6]

  }
}

par(mfrow = c(2,3), mar = c(3,6,3,3))
boxplot(p.1, p.2, p.3, col = cols, main =  'Classes with highest Producers Accuracy', horizontal = T, at = c(1,3,5), xlim = c(0,6))
axis(side = 2, at = c(1,3,5), label = c("Nearest\n Neighbor", "Majority\n Rule", "PAA"), las = 1)
text(mean(p.1), 1.75, labels = round(mean(p.1),2), col = 'black')
text(mean(p.2), 3.75, labels = round(mean(p.2),2), col = 'black')
text(mean(p.3), 5.75, labels = round(mean(p.3),2), col = 'black')

boxplot(u.1, u.2, u.3, col = cols, main =  'Classes with highest Users Accuracy', horizontal = T, at = c(1,3,5), xlim = c(0,6))
axis(side = 2, at = c(1,3,5), label = c("Nearest\n Neighbor", "Majority\n Rule", "PAA"), las = 1)
text(mean(u.1), 1.75, labels = round(mean(u.1),2), col = 'black')
text(mean(u.2), 3.75, labels = round(mean(u.2),2), col = 'black')
text(mean(u.3), 5.75, labels = round(mean(u.3),2), col = 'black')

boxplot(overall.1, overall.2, overall.3,col = cols, main =  'Overall Map Accuracy (%)', horizontal = T, at = c(1,3,5), xlim = c(0,6))
axis(side = 2, at = c(1,3,5), label = c("Nearest\n Neighbor", "Majority\n Rule", "PAA"), las = 1)
text(mean(overall.1), 1.75, labels = round(mean(overall.1),2), col = 'black')
text(mean(overall.2), 3.75, labels = round(mean(overall.2),2), col = 'black')
text(mean(overall.3), 5.75, labels = round(mean(overall.3),2), col = 'black')

boxplot(low.1, low.2, low.3, col = cols, main =  'Largest Areal Under Prediction (%)', horizontal = T, at = c(1,3,5), xlim = c(0,6))
axis(side = 2, at = c(1,3,5), label = c("Nearest\n Neighbor", "Majority\n Rule", "PAA"), las = 1)
text(mean(low.1), 1.75, labels = round(mean(low.1),2), col = 'black')
text(mean(low.2), 3.75, labels = round(mean(low.2),2), col = 'black')
text(mean(low.3), 5.75, labels = round(mean(low.3),2), col = 'black')

boxplot(high.1, high.2, high.3, col = cols, main =  'Largest Areal Over Prediction (%)', horizontal = T, at = c(1,3,5), xlim = c(0,6))
axis(side = 2, at = c(1,3,5), label = c("Nearest\n Neighbor", "Majority\n Rule", "PAA"), las = 1)
text(mean(high.1), 1.75, labels = round(mean(high.1),2), col = 'black')
text(mean(high.2), 3.75, labels = round(mean(high.2),2), col = 'black')
text(mean(high.3), 5.75, labels = round(mean(high.3),2), col = 'black')


boxplot(m.1, m.2, m.3, col = cols, main =  'Number of Missing Classes', horizontal = T, at = c(1,3,5), xlim = c(0,6))
axis(side = 2, at = c(1,3,5), label = c("Nearest\n Neighbor", "Majority\n Rule", "PAA"), las = 1)
text(mean(m.1), 1.75, labels = round(mean(m.1),2), col = 'black')
text(mean(m.2), 3.75, labels = round(mean(m.2),2), col = 'black')
text(mean(m.3), 5.75, labels = round(mean(m.3),2), col = 'black')



