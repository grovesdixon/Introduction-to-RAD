#!/usr/bin/env python
from sys import argv
from Bio import Phylo
treeFile=argv[1]
tree = Phylo.read(treeFile, "newick")
Phylo.draw_ascii(tree)


