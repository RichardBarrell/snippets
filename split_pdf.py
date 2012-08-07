import sys
import pyPdf

# Trivial use of pyPdf to cut foo.pdf into a bunch of PDF files, one per page.
# Usage: python split_pdf.py foo.pdf
# Spits out page_000.pdf, page_001.pdf, and so on.

input = pyPdf.PdfFileReader(open(sys.argv[1],"rb"))
for ix, page in enumerate(input.pages):
	output = pyPdf.PdfFileWriter()
	output.addPage(page)
	output.write(open("page_0.3d.pdf" % ix, "wb"))

