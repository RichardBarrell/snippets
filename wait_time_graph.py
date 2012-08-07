#!/usr/bin/env python
from xml import sax
import sys, os

# Turns jtl files into .plot files for gnuplot that show a chart whose
# area is proportional to total time spent waiting on HTTP requests.

# Usage: python wait_time_graph.py [foo.jtl]
# If run with no args, looks for *.jtl in current directory.
# Given foo.jtl input, spits out foo.plot

# Recommend plotting with gnuplot.

class TimeMuncher(sax.ContentHandler):
    def __init__(self):
        self.results = []
    def startElement(self, name, attrs):
        if name == 'httpSample':
            self.results.append(int(attrs['t']))

if __name__ == '__main__':
    files = sys.argv[1:]
    if files == []:
        files = [ x for x in os.listdir('.') if x.endswith('.jtl') ]
    for x in files:
        parser = sax.make_parser()
        handler = TimeMuncher()
        parser.setContentHandler( handler )
        parser.parse( x )
        results = handler.results
        mn = min(results)
        mx = max(results)
        width = 5
        dots = [0] * (mx-mn+(2*width))
        for point in results:
            dots[point] += point
            for w in xrange(width):
                dots[point+w] += point
        outfile = file(x+".plot","wb")
        for x in xrange(mn-width,mx+width):
            outfile.write("%f %f\n" % (x,dots[x]))
        outfile.close()
