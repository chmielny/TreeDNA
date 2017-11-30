atrNum = 10                                                                 # ustalenie liczby atrybutow kazdego z przykladow

myCut = function(index, str) {                                                                  # funkcja wycina podciag atrNum znakow
    tmp = substr(x = str, start = index - (atrNum / 2), stop = index + (atrNum / 2) -1)         # z duzego ciagu znakow
    invisible(tmp)
}

cutSeq = function(str, index) {                                                                 # funkcja wycina wiele podciagow znakow
    tmp = sapply(index, myCut, str, simplify = TRUE, USE.NAMES = FALSE)                         # na podstawie listy indexow
    invisible(tmp)
}

prepareData = function(inputFile) {
    raw = readLines(inputFile)                                              # wczytaj caly plik z danymi DNA
    intronBordersLines = grep(raw, pattern="Introns") + 1                   # znajdz numery lini zawierajace poczatki i konce intronów
    intronBorders = raw[intronBordersLines]                                 # znajdz liczby okreslajace poczatki i konce intronów
    
    DNAseqLines = grep(raw, pattern="Data") + 1
    DNAseq = raw[DNAseqLines]                                               # to samo dla sekwencji DNA

    intronBorders = substring(intronBorders, 2)                             # usuniecie pierwszej spacji z ciagow 
    intronBorders = strsplit(intronBorders, split = " ")                    # stringi na listy stringow
    intronBorders = lapply(intronBorders, as.numeric)                       # listy stringow na listy liczb

    donorIndex = lapply(intronBorders, function(l) l[c(TRUE,FALSE)])        # pozycje donorow - nieparzyste w listach
    akceptorIndex = lapply(intronBorders, function(l) l[c(FALSE,TRUE)])     # pozycje akceptorow - parzyste
 
    trueDonor = unlist(mapply(cutSeq, DNAseq, donorIndex, MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = FALSE))         # wyciecie z sekwencji prawdziwych donorow
    trueAkceptor = unlist(mapply(cutSeq, DNAseq, akceptorIndex, MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = FALSE))   # wyciecie z sekwencji prawdziwych akceptorow
#    out

}


print(prepareData("araclean.dat"))

