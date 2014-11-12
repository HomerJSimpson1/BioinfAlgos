#import math

proteindict = {
    'F': ['TTT', 'TTC'], 
    'L': ['TTA', 'TTG', 'CTT', 'CTC', 'CTA', 'CTG'],
    'I': ['ATT', 'ATC', 'ATA'],
    'M': ['ATG'],
    'V': ['GTT', 'GTC', 'GTA', 'GTG'],
    'S': ['TCT', 'TCC', 'TCA', 'TCG', 'AGT', 'AGC'],
    'P': ['CCT', 'CCC', 'CCA', 'CCG'], 
    'T': ['ACT', 'ACC', 'ACA', 'ACG'],
    'A': ['GCT', 'GCC', 'GCA', 'GCG'],
    'Y': ['TAT', 'TAC'],
    'X': ['TAA', 'TAG', 'TGA'],
    'H': ['CAT', 'CAC'],
    'Q': ['CAA', 'CAG'],
    'N': ['AAT', 'AAC'],
    'K': ['AAA', 'AAG'],
    'D': ['GAT', 'GAC'],
    'E': ['GAA', 'GAG'],
    'C': ['TGT', 'TGC'],
    'W': ['TGG'],
    'R': ['CGT', 'CGC', 'CGA', 'CGG', 'AGA', 'AGG'],
    'G': ['GGT', 'GGC', 'GGA', 'GGG']
    }

#numcombos = 1

def aatont(proteinseq):
    proteinlist = list(proteinseq)
    intmedlist = []
    #mylist = []
    #mylist = [[] for i in range(len(proteinlist))]
    numcombos = 1

    for i in range(len(proteinlist)):
        #global numcombos
        aalist = proteindict[proteinlist[i]]
        intmedlist.append(aalist)
        numcombos *= len(aalist) 
        #for j in range
        #finallist.append(''.join(aalist))

    #print numcombos
    mylist = [[] for i in range(numcombos)]
    print len(mylist)
    print mylist[1]

    #for i in range(len(intmedlist) - 1, -1, -1):
    for i in range(len(intmedlist)):
        for j in range(len(intmedlist[i])):
            mylist[j].append(intmedlist[i][j])
        
    #print(intmedlist)
    #return intmedlist
    return mylist



