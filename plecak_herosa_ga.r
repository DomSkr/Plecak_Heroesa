#Rozwiązanie problemu plecakowego przy użyciu algorytmu genetycznego.

#zadanie: 
#Gracz musi zebrac jednostki do bitwy.
#Gracz moze miec 8 jednostek jednoczesnie.
#Każda z jednostek ma swoją wartość bojową.
#Gracz ma do wykorzystania 300 złotych monet.
#Określ optymalny skład armii z zachowaniem limitu miejsca i kosztu.

#------------------------------------------------#
#instalacja i import paczki "GA"
#install.packages("GA")
library(GA)

#Definiujemy zbiór danych i limit kosztu armii
armia = data.frame(
  jednostka = c("Pikinier", "Łucznik", "Gryf", "Zbrojny", "Mnich", "Kawalerzysta", "Anioł",
                "Troglodyta", "Harpia", "Złe oko", "Meduza", "Minotaur", "Mantikora", "Czerwony smok",
                "Szkielet", "Ożywieniec", "Zjawa", "Wampir", "Lisz", "Czarny Rycerz", "Kościany smok",
                "Goblin", "Wilczy jeździec", "Ork", "Ogr", "Rok", "Cyklop", "Behemot"),
  atak = c(10, 25, 28, 35, 61, 82, 102, 4, 14, 21, 30, 54, 70, 172, 8, 19, 23, 38, 49, 66, 82, 11, 24, 28, 55, 59, 93, 92),
  obrona = c(11, 2, 31, 14, 41, 72, 62, 14, 4, 17, 28, 52, 60, 12, 9, 17, 21, 30, 45, 46, 35, 15, 22, 8, 47, 50, 33, 22),
  przyrost = c(9, 8, 6, 5, 3, 2, 1, 10, 9, 12, 5, 4, 2, 3, 7, 11, 5, 4, 13, 1, 3, 12, 8, 3, 6, 2, 1, 3),
  koszt_jedn = c(10, 45, 30, 21, 33, 82, 91, 11, 27, 33, 14, 65, 80, 101, 6, 25, 64, 44, 78, 81, 120, 1, 55, 37, 44, 20, 78, 16)
)

armiaKosztMax = 300
#armia

# definiujemy kolumnę w macierzy pzechowującą wyliczenie wartości jednostki
armia$wartosc_bojowa <- rowSums(armia[,2:3], na.rm=FALSE) * armia[,4]
armia$koszt_sumaryczny <- armia[,4]*armia[,5]

#Definiujemy funkcję przystosowania
fitnessFunc = function(chr) {
  calkowitaWartoscChr = chr %*% armia$wartosc_bojowa
  calkowitykosztChr = chr %*% armia$koszt_sumaryczny
  print(calkowitaWartoscChr)
  print(calkowitykosztChr)
  if (calkowitykosztChr > armiaKosztMax) return(-calkowitaWartoscChr) 
  else return(calkowitaWartoscChr)
}

#Uruchamiamy algorytm genetyczny dla zadanych parametrów
wyniki=ga( type="binary",
           lower = 8,
           upper = 8,
           nBits=28,
           fitness=fitnessFunc,
           popSize=100,
           pcrossover=0.85,
           pmutation=0.05,
           elitism=5,
           maxiter=30,
           seed=10 )

#Podsumawanie działania algorytmu genetycznego		   
summary(wyniki)
plot(wyniki)

#Dekodowanie (prezentacja) pojedynczego rozwiązania
decode=function(chr){
  print("Rozwiązanie: ")
  print( armia[chr == 1, ] ) #wskaż jednostki, które zostały wybrane (== 1)
  print( paste("Koszt armii =",chr %*% armia$koszt_sumaryczny) )
  print( paste("Wartość bojowa jednostek =",chr %*% armia$wartosc_bojowa) )
}
decode(wyniki@solution[1,])

