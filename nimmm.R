# NIM game by Tibi Raducanu

newGame = function(){
  t = 0          # dificultatea
  s = 1          # numarul total de pietre
  n = 0          # numarul de linii
  v = c(1:100)   # vector ce contine numarul de pietre de pe fiecare linie
  Xor = 0        # variabila ce retine xor-ul tuturor liniilor
  Max = 0        # cel mai mare numar de pietre de pe o linie
  cat("Welcome to the NIM game!\n\n\n")
  
  while(t<1 || t>5){
    cat("Select the dificulty of the game, choosing between 1, 2, 3, 4 or 5:\n\n
        1. Easy\n
        2. Medium\n
        3. Hard\n
        4. Insane\n
        5. Random\n\n\n")
    t = readline(prompt="Insert the number here: " )
    t = as.integer(t)
    cat(t)
    if(t<1 || t>5) cat("Please insert a value between 1 and 5.\n\n")
  }
  
  if(t==1){       # setam numarul de bete din fiecare gramada
    n = 3          # in functie de dificultate
    v[1] = 1 
    v[2] = 3
    v[3] = 5
  }
  if(t==2){
    n = 4
    v[1] = 1
    v[2] = 3
    v[3] = 5
    v[4] = 8
  }
  if(t==3){
    n = 5
    v[1] = 1
    v[2] = 3
    v[3] = 5
    v[4] = 7
    v[5] = 9
  }
  if(t==4){
    n = 6
    v[1] = 1
    v[2] = 3
    v[3] = 5
    v[4] = 7
    v[5] = 9
    v[6] = 11
  }
  if(t==5){
    n = sample(3:6, 1)  # pietrele sunt alese random
    for(i in 1:n) v[i] = sample(1:8, 1)
  }
  
  cat("\nThe Game Has Started!\n\n")
  cat("Rules:\n")
  cat("1. You can pick at least one stone at every step\n")
  cat("2. You can pick stones from only one line\n")
  cat("3. The player that picks the last stone loses\n\n")
  
  Afisare(n,v); # afiseaza jocul in starea curenta
  
  while(s){
    line = readline(prompt = "Insert line: ")  # linia de pe care vei lua pietre
    line = as.integer(line)
    
    while(line<1 || line>n || v[line]==0){
      line = readline(prompt = "Insert a valid line: ")
      line = as.integer(line)
    }
    
    stones = readline(prompt = "Insert the number of stones that you want to pick: ") # numarul de pietre de pe linia respectiva
    stones = as.integer(stones)
    
    while(stones<=0 || stones>v[line]){
      stones = readline(prompt = "Insert a valid number of stones: ")
      stones = as.integer(stones)
    }
    
    v[line] = v[line] - stones
    s = 0
    Xor = 0
    Max = 0
    pos = 0
    PickedLine = 0
    StonesNumber = 0
    
    for(i in 1:n){ # calculam suma, xor-ul si maximul pietrelor
      s = s + v[i]
      Xor = bitwXor(Xor, v[i])
      if(v[i]>Max){
        Max = v[i]
        pos = i
      }
    }
    
    Afisare(n,v) # afisam starea
    
    if(s==0){  # daca jocul a ramas fara pietre dupa ce ai mutat, computerul a castigat
      cat("Computer has won!\n")
      break;
    }
    if(s==1){ # daca a ramas o singura piatra si computerul e la mutare, jucatorul a castigat
      cat("You have won, congratulations!\n")
      break;
    }
    
    # Computer is thinking, calculam mutarea computerului
    Sys.sleep(1)
    cat("Thinking..\n")
    Sys.sleep(1.5)
    # Computer is thinking
    
    if(Xor==0){  # daca jocul este echilibrat (Xor == 0) vom lua o singura piatra din gramada cea mai mare
      PickedLine = pos
      StonesNumber = 1
      s = s-1
      v[pos] = v[pos] - 1
    }
    else{ # daca jocul nu e echilibrat, il vom aduce intr-o stare de echilibru
      OneLines = 0 # numarul de linii cu o singura piatra
      for(i in 1:n){
        if(v[i] == 1){
          OneLines = OneLines + 1
        }
      }
      
      XorS = 0 # xor auxiliar, vom retine in el xor dintre xor-ul total si numarul de pietre de pe linia curenta
      for(i in 1:n){
          if(v[i]==0){ # daca o linie nu are pietre nu are rost sa verificam daca putem lua pietre de acolo
            next
          }
          XorS = bitwXor(Xor,v[i]) # xor-ul total fara linia curenta
          #cat("Line ",i,"  Xors: ",XorS, "  v[i]=  ", v[i], "\n", s, "\n")
          if(s-v[i] == OneLines){ # daca pe langa linia curenta avem numai linii de lungime 1
            if(OneLines %% 2 == 0){ # tratam cazul cu numar impar de linii de lungime 1
              PickedLine = i
              StonesNumber = v[i] - 1 # vrem sa ramana un numar impar de linii cu o piatra
            }
            else { # tratam cazul cu numar par de linii de lungime 1
              PickedLine = i
              StonesNumber = v[i] # vrem sa ramana un numar impar de linii cu o piatra
            }
            break
          }
          if(v[i] >= XorS){ # verificam daca putem aduce jocul intr-o stare de echilibru, daca da, il aducem
            PickedLine = i
            StonesNumber = v[i] - XorS # in acest moment xor-ul tuturor liniilor va fi zero => joc echilibrat
            break
          }
      }
      
      v[PickedLine] = v[PickedLine] - StonesNumber # updatam vectorul de linii si suma totala
      s = s - StonesNumber
    }
    
    if(s==0){
      cat("You have won, congratulations!\n")
      break
    }
    
    if(StonesNumber==1){
      cat("The computer has picked 1 stone from line", PickedLine, "\n\n")
    }
    else cat("The computer has picked ", StonesNumber, "stones from line ", PickedLine, "\n\n")
    
    Afisare(n,v)
  }
}

Afisare = function(n,v){ # afiseaza jocul in starea curenta
  Maxim = 0
  for(i in 1:n){
    if(v[i]>Maxim) Maxim = v[i] # numarul maxim de pietre de pe o linie
  }
  for(i in 1:Maxim) cat("- ") # design joc
  cat("\n")
  for(i in 1:n){
    cat("Line ", i, ": ")
    if(v[i]){
      for(j in 1:v[i]) cat("O ") # "O" reprezinta o piatra
    }
    cat("\n")
  }
  for(j in 1:Maxim) cat("- ")
  cat("\n\n")
}

newGame()