#Rozwiązanie problemu plecakowego przy użyciu algorytmu genetycznego.

#zadanie: 
#Gracz musi zebrac jednostki do bitwy.
#Gracz moze miec 8 jednostek jednoczesnie.
#Każda z jednostek ma swoją wartość bojową.
#Gracz ma do wykorzystania złotych monet.
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
  wartosc_bojowa = c(10, 25, 28, 35, 61, 82, 102, 
              4, 14, 21, 30, 54, 70, 102, 
              8, 19, 23, 38, 49, 66, 102, 
              11, 24, 28, 55, 59, 93, 102),
  koszt = c(10, 20, 30, 40, 60, 80, 100,
            10, 20, 30, 40, 60, 80, 100, 
            10, 20, 30, 40, 60, 80, 100, 
            10, 20, 30, 40, 60, 80, 100)
)

armiaKosztMax = 300
#armia

#Definiujemy funkcję przystosowania
fitnessFunc = function(chr) {
  calkowitaWartoscChr = chr %*% armia$wartosc_bojowa
  calkowitykosztChr = chr %*% armia$koszt
  print(calkowitaWartoscChr)
  print(calkowitykosztChr)
  if (calkowitykosztChr > armiaKosztMax) return(-calkowitaWartoscChr) 
  else return(calkowitaWartoscChr)
}

#Uruchamiamy algorytm genetyczny dla zadanych parametrów
wyniki=ga( type="binary",
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
  print( armia[chr == 1, ] )
  print( paste("Koszt armii =",chr %*% armia$koszt) )
  print( paste("Wartość bojowa jednostek =",chr %*% armia$wartosc_bojowa) )
}
decode(wyniki@solution[1,])

