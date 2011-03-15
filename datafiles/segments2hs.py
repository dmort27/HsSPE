#!/usr/bin/env python

infile = open("SegmentFeatures.org", "r", encoding="utf-8")

line = infile.readline()
features = [ft.strip() for ft in line.split("|")[2:-1]]
null = infile.readline()
pairs = [(r[0].strip(), ",".join([s.strip()+n for (n,s) in zip(features, r[1:])])) for r in [line.split("|")[1:-1] for line in infile]]
print("segments = [ {0}\n           ]".format("\n           , ".join(['("{0}","[{1}]")'.format(k, v) for (k,v) in pairs])))
