# thesis
This repository will store all the writing, code, and references for my undergraduate thesis in Political & Social Thought. I'm writing longform with GitHub and Yihui Xie's [bookdown](https://github.com/rstudio/bookdown) for the first time for three aims. First, I need to back-up everything. Work built from source will stand up to the most egregious crash, hack, or loss. Second, I am both an open-source and publishing nerd. I am interested in seeing how working from source facilitates interoperability, quick drastic design changes, and the magic that happens when you finally get to preview work. Finally, I hope this repository will help someone shopping around for workflow examples, software tips, or plain-old ideas about foreclosure and American responses.

## workflow
I am following Chester Ismay's [thesisdown](https://github.com/ismayc/thesisdown) package to build my file structure, though altering his TeX styles to meet my own requirements. It is essentially a template for creating theses with bookdown. He also offers some helpful workarounds for some necessities of thesis writing, namely the title page's rigid style. Additionally, this R-bloggers article guided me through my references and bibliography. I outfitted the wonderful Zotero with Better Bib(La)Tex and ZotFile, which make bibliographies in RStudio and LaTeX almost as easy as in Word, whose speed and integration with Zotero I still miss. Under Ismay's template, I make the following revisions:

* `data` contains all my evidence that exists in electronic form. It is ignored by Git due to the fact it's mostly PDFs
  + `literature` holds the myriad papers that ZotFile downloads
  + `voting` holds election, geographic, and demographic data that I will download to analyze voting trends
* `bib` contains CMOS' 17th edition of Note-Bibliography style, rather than the APA Ismay used
* `figure` exists in my file structure but is hidden since it is composed of data-hungry images