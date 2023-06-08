# CancerCovidEndocrineTx

All analyses of incidence of breast and prostate cancer-related treatments, before and after the COVID-19 lockdown, in years and months from 2017 to July 2022: 
- use of endocrine treatments in breast and prostate cancer (Aromatase Inhibitors, Aromatase Inhibitors with GnRH agonists or antagonists, First-generation anti-androgens, GnRH Agonists with first-generation andorgen deprivation therapy, GnRH/LHRH antagonists, Second-generation anti-androgens, Tamoxifen, Tamoxifen with GnRH agonists or antagonists);
- associated treatment-related outcomes (osteoporosis, osteopenia, bone fracture, prescriptions of bisphosphonates and denosumab) 

# Running the analysis
Download this entire repository (you can download as a zip folder using Code -> Download ZIP, or you can use GitHub Desktop).
Open the project CancerCovid_EndocrineTx.Rproj in RStudio (when inside the project, you will see its name on the top-right of your RStudio session)

Open and work though the CodeToRun.R file which should be the only file that you need to interact with. Run the lines in the file, adding your database specific information and so on (see comments within the file for instructions). The last line of this file will run the study (source(here("RunStudy.R")).

After running you should then have 4 separate results files, specific to each of the outcomes above, with a zipped folder within them with the results to share. Here you will also find the PDFs of all the plots, and the .RData objects for each part of the analyses.


