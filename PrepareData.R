atrNum = 10                                                    # ustalenie liczby atrybutow kazdego z przykladow


cutSeq = function(str, index) {
    tmp = substr(x = str, start = index[[1]][1] - (atrNum / 2), stop = index[[1]][1] + (atrNum / 2) -1)
    invisible(tmp)
}

prepareData = function(inputFile) {
    raw = readLines(inputFile)                                  # wczytaj caly plik z danymi DNA
    intronBordersLines = grep(raw, pattern="Introns") + 1       # znajdz numery lini zawierajace poczatki i konce intronów
    intronBorders = raw[intronBordersLines]                     # znajdz liczby okreslajace poczatki i konce intronów
    DNAseqLines = grep(raw, pattern="Data") + 1
    DNAseq = raw[DNAseqLines]                                   # to samo dla sekwencji DNA

    intronBorders = substring(intronBorders, 2)                 # usuniecie pierwszej spacji z ciagow 
    intronBorders = strsplit(intronBorders, split = " ")        # stringi na listy stringow
    intronBorders = lapply(intronBorders, as.numeric)           # listy stringow na listy numerow


    out = mapply(cutSeq, DNAseq, intronBorders, MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = FALSE)
    out

}


print(prepareData("araclean.dat"))

