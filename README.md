# BM424workshops
Workshop materials for BM424
Dr. Morgan Feeney, AY 2024/2025

## How to use this repository - instructor notes

### To launch this repository for a new academic year:

1. To generate the data files needed for workshop, run the scripts (in the "scripts" folder) - more details in the README.txt file in that folder.  [add details about how to load these for the webR instances where they're needed]

2. Edit each of the workshop 3 group pages (e.g. 03A.qmd, 03B.qmd, etc.) to update the workshop3_date variable with the correct date for the third workshop.

### Render the pages for each individual workshop

#### Workshop 3

#### Workshop 4 

1. 
2. add the pages for each group to the _quarto.yml navbar
3. Render the pages for the workshop

#### Workshop 5 

1. 
2. add the pages for each group to the _quarto.yml navbar
3. Render the pages for the workshop

### To add a new group:

1. Select an appropriate pathogen. Provide images of this pathogen (save in the "images" folder with file names in the format pathogenX.jpg (for an SEM/micrograph of the pathogen) and plateX.jpg (for an image of the pathogen grown on an agar plate), where X should be replaced by the group letter.)

2. Add information/resources for the new pathogen. 
a. You will need an "_includes/pathogenX.qmd" file with links to fact sheet, UK SMID, and other relevant resources for clinical micro, diagnosis, public comms, etc. [See "_includes/pathogenA.qmd" for an example.]
b. You will need an "_includes/epiX.qmd" file with epidemiology data (e.g. 16S, MLST, depending on the particular pathogen) [See "_includes/epiA.qmd" for an example.]
b. You will need a bibliography file named X.bib (where X is the correct group letter for your new group). This should contain ~15-20 relevant references (diagnostics, epidemiology, etc.) for the pathogen. [See "A.bib" for an example.]
c. Add an entry to the pathogen_info list in the "_includes/news1.qmd" header info - a brief description of the disease's etiological agent, mechanism of transmission, and symptoms.

3. Add images for the group's city and hospital (save in the "images" folder with file names in the format cityX.jpg (for an image of the city centre) and hospital.jpg (for an image of the city's hospital), where X should be replaced by the group letter.) [N.B. These images can be generated by Bing Image Creator, DALL-E, or similar.]

4. Add pages for the group's workshops: 
a. Duplicate and rename the workshop 3 page (03A.qmd), the workshop 4 page (04A.qmd), and the workshop 5 page (05A.qmd) files. [e.g. to add a group named Q, you would add 03Q.qmd, 04Q.qmd, 05Q.qmd]
b. Add links to these pages to the _quarto.yml page in the appropriate sections, i.e.: 
    - text: "Workshop 3"
      menu:
        - 03workshop.qmd
        - 03A.qmd
        ....
        - 03Q.qmd
c. Edit the workshop YAML headers/pages as needed. 

Header information for each group should be as follows: 

```{r setup, include=FALSE}
#define variables for each group
city_name <- "Inverkeld"
dis_name <- "Legionellosis"
inf_agent <- "Legionella pneumophila"
hosp_name <- "Queen Rose"
group <- "A"
```

The groups/pathogens list (current for 2024/25) is: 
A. Legionella pneumophila
B. Influenza
C. MRSA
D. Campylobacter jejuni
E. Listeria monocytogenes
F. Neisseria meningitidis
G. MDR M. tuberculosis
H. Group A Strep
I. Bacillus anthracis
J. Bordetella pertusis
K. Acinetobacter baumanii
L. Aspergillus
