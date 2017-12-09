atrNum = 100                                                                 # ustalenie liczby atrybutow kazdego z przykladow

myCut = function(index, str) {                                                                  # funkcja wycina podciag atrNum znakow
    tmp = substr(x = str, start = index - (atrNum / 2), stop = index + (atrNum / 2) -1)         # z duzego ciagu znakow
    invisible(tmp)
}

cutSeq = function(str, index) {                                                                 # funkcja wycina wiele podciagow znakow
    tmp = sapply(index, myCut, str, simplify = TRUE, USE.NAMES = FALSE)                         # na podstawie listy indexow
    invisible(tmp)
}

removeDups = function(falIdVec, trueIdVec) {                                                    # funkcja usuwa z indeksow falszywych donorow/akceptorow
    lowBorder = trueIdVec - atrNum                                                              # te ktore pokrywaja sie z prawdziwymi
    hiBorder = trueIdVec + atrNum
    for (i in 1:length(trueIdVec)) {
        falIdVec = falIdVec[ falIdVec > hiBorder[i] | falIdVec < lowBorder[i]  ] 
    }
    invisible(falIdVec) 
}

prepareData = function(inputFile, donorOutFile, akceptorOutFile) {
    raw = readLines(inputFile)                                              # wczytaj caly plik z danymi DNA
    intronBordersLines = grep(raw, pattern="Introns") + 1                   # znajdz numery lini zawierajace poczatki i konce intronów
    intronBorders = raw[intronBordersLines]                                 # znajdz liczby okreslajace poczatki i konce intronów
    
    DNAseqLines = grep(raw, pattern="Data") + 1
    DNAseq = raw[DNAseqLines]                                               # to samo dla sekwencji DNA
    
    intronBorders = substring(intronBorders, 2)                             # usuniecie pierwszej spacji z ciagow 
    intronBorders = strsplit(intronBorders, split = " ")                    # stringi na listy stringow
    intronBorders = lapply(intronBorders, as.numeric)                       # listy stringow na listy liczb

    donorIndex = lapply(intronBorders, function(l) l[c(TRUE,FALSE)])        # pozycje donorow - nieparzyste w listach
    donorIndex = sapply(donorIndex, function(l) l + 1)                      # zwiekszenie indeksow donorow o 1, w celu pokrywania sie sekw. GT w praw. i falszywych
    akceptorIndex = lapply(intronBorders, function(l) l[c(FALSE,TRUE)])     # pozycje akceptorow - parzyste

    trueDonor = unlist(mapply(cutSeq, DNAseq, donorIndex, MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = FALSE))         # wyciecie z sekwencji prawdziwych donorow
    trueAkceptor = unlist(mapply(cutSeq, DNAseq, akceptorIndex, MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = FALSE))   # wyciecie z sekwencji prawdziwych akceptorow
    trueDonor = trueDonor[nchar(trueDonor) == atrNum]                       # usuniecie sekwencji donorow i akceptorow o dlugosci mniejszej od atrNum
    trueAkceptor = trueAkceptor[nchar(trueAkceptor) == atrNum]              # dla atrNum = 100, jest to tylko dwa przyklady

    #Przygotowanie falszywych przykladow
    gtIndex = gregexpr(pattern = "GT", DNAseq)                              # znalezienie miejsc GT i AG w sekwencjach
    agIndex = gregexpr(pattern = "AG", DNAseq)
    falseDonorIndex = mapply(removeDups, gtIndex, donorIndex)               # usuniecie tych miejsc, ktore pokrywaja sie z prawdziwymi koncami intronowi
    falseAkceptorIndex = mapply(removeDups, agIndex, akceptorIndex)

    falseDonor = unlist(mapply(cutSeq, DNAseq, falseDonorIndex, MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = FALSE))         # wyciecie z sekwencji falszywych donorow
    falseAkceptor = unlist(mapply(cutSeq, DNAseq, falseAkceptorIndex, MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = FALSE))   # wyciecie z sekwencji falszywych akceptorow
    falseDonor = falseDonor[nchar(falseDonor) == atrNum]                    # usuniecie sekwencji falszywych donorow i akceptorow o dlugosci mniejszej od atrNum
    falseAkceptor = falseAkceptor[nchar(falseAkceptor) == atrNum]              
   
    trueDonor = trueDonor[- grep("N", trueDonor)]                           # usuniecie sekwencji zawierajacych "N" - w DNA sa tylko GTAC
    tmp = strsplit(trueDonor,"")
    trueDonorDF = data.frame(do.call(rbind, tmp))       
    trueDonorDF$Y = 1                                                       # zrobienie dataframe z prawdziwych donorow i dodanie kolumny Y=1
    falseDonor = falseDonor[- grep("N", falseDonor)]                        # usuniecie sekwencji zawierajacych "N" - w DNA sa tylko GTAC
    tmp = strsplit(falseDonor,"")
    falseDonorDF = data.frame(do.call(rbind, tmp))
    falseDonorDF$Y = 0                                                      # zrobienie dataframe z falszywych donorow, Y=0

    allDonor = rbind(trueDonorDF, falseDonorDF)                             # wspolny dataframe z wszystkimi donorami
   
    trueAkceptor = trueAkceptor[- grep("N", trueAkceptor)]                  # usuniecie sekwencji zawierajacych "N" - w DNA sa tylko GTAC
    tmp = strsplit(trueAkceptor,"")
    trueAkceptorDF = data.frame(do.call(rbind, tmp))       
    trueAkceptorDF$Y = 1                                                    # zrobienie dataframe z prawdziwych akceptorow i dodanie kolumny Y=1
    falseAkceptor = falseAkceptor[- grep("N", falseAkceptor)]               # usuniecie sekwencji zawierajacych "N" - w DNA sa tylko GTAC 
    tmp = strsplit(falseAkceptor,"")
    falseAkceptorDF = data.frame(do.call(rbind, tmp))
    falseAkceptorDF$Y = 0                                                   # zrobienie dataframe z falszywych akceptorow, Y=0

    allAkceptor = rbind(trueAkceptorDF, falseAkceptorDF)                    # wspolny dataframe z wszystkimi donorami

    write.csv(file=donorOutFile, x=allDonor)                                # zapis dataframe do plikow
    write.csv(file=akceptorOutFile, x=allAkceptor)
}


prepareData("araclean.dat", "donory.csv", "akceptory.csv")

