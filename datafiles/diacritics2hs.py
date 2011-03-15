#!/usr/bin/env python

infile = open("DiacriticFeatures.org", "r", encoding="utf-8")

infile.readline()
infile.readline()
pairs = [(a.strip(), b.strip()) for [a,b] in [line.split("|")[1:3] for line in infile]]
print("diacritics = [ {0}\n             ]".format("\n             , ".join(['("{0}", "{1}")'.format(k, v) for (k,v) in pairs])))
