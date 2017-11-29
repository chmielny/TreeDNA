prepareData = function(inputFile) {
    raw = readLines(inputFile)                              # wczytaj caly plik z danymi DNA
    intronBordersLines = grep(raw, pattern="Introns") + 1   # znajdz numery lini zawierajace poczatki i konce intronów
    intronBorders = raw[intronBordersLines]                 # znajdz liczby okreslajace poczatki i konce intronów
    DNAseqLines = grep(raw, pattern="Data") + 1
    DNAseq = raw[DNAseqLines]                               # to samo dla sekwencji DNA

    intronBorders = substring(intronBorders, 2)             # usuniecie pierwszej spacji z ciagow 
    intronBorders
#    substr(DNAseq[50], start=10, stop=50)


}


print(prepareData("araclean.dat"))

