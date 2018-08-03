import PyPDF2
diccionarioVar = open("diccionario20de20codigos.pdf", "rb")
read_pdf = PyPDF2.PdfFileReader(diccionarioVar, strict=False)
read_pdf.getNumPages()
page = read_pdf.getPage(0)

page_content = page.extractText()
print(page_content)