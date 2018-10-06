def DNA_strand(dna):
    return ''.join(map(lambda x: {'A':'T', 'T':'A', 'C':'G', 'G':'C'}[x], dna))
    