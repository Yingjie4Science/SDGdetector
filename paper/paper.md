---
title: "SDGdetector: an R-based text mining tool for quantifying the efforts toward SDGs"
tags:
    - Sustainability
    - Sustainable Development Goals (SDGs)
    - Text mining
    - Text analysis
    - R
authors:
    - name: Yingjie Li
      orcid: 0000-0002-8401-0649
      affiliation: 1
    - name: Meng Cai
      orcid: 0000-0002-8318-572X
      affiliation: "2, 3"
    - name: Veronica Frans
      orcid: 0000-0002-5634-3956
      affiliation: 1
    - name: Yuqian Zhang
      orcid: 0000-0001-7576-2526
      affiliation: 1
    - name: Jianguo Liu
      orcid: 0000-0001-6344-0087
      affiliation: 1
affiliations:
    - name: Center for Systems Integration and Sustainability, Michigan State University, East Lansing, MI 48823, United States
      index: 1
    - name: School of Planning, Design and Construction, Michigan State University, East Lansing, MI, 48824, United States
      index: 2
    - name: Institute for Traffic Planning and Traffic Engineering, Technical University of Darmstadt, Darmstadt 64287, Germany
      index: 3  
date: 23 October 2022
bibliography: paper.bib

---

# Summary

The global interest in moving towards a sustainable future has grown exponentially at all levels. The United Nations’ Sustainable Development Goals (SDGs) adopted by world leaders in 2015 provided an integrated framework to track progress toward sustainability [@un_global_2019]. Textual data, such as public statements posted on websites, organization reports, and scientific publications, provide a rich source for evaluating the planned and ongoing efforts, as well as achievements towards sustainability. However, no computational tool exists to date that is able to accurately and efficiently identify SDG-related statements from these large amounts of text data. To fill this gap, we developed the **SDGdetector** package in R to map textual data to specific goals and targets under the UN SDG framework for quantitative analysis. This is the first open-source, high-resolution, and high-accuracy analytic package that can identify which and how many SDG goals and targets are declared in any type of text-based data. This package thus enabled a unique way to monitor individuals and organizations’ commitments and efforts towards advancing the 17 SDGs and 169 associated targets. 

# Statement of need

The Sustainable Development Goals (SDGs) agenda, adopted by all United Nations Member States in 2015, provides a shared blueprint for nations, cities, corporations, research institutions, and individuals to track and plan their contributions to social, economic, and environmental transformations [@un_global_2019]. Although considerable efforts and contributions have been made, half of the 231 indicators listed in the global indicator framework for SDGs lack either established methodology or available data on measuring and implementing the goals (https://unstats.un.org/sdgs/iaeg-sdgs/tier-classification/). As a complement to the widely used statistical data, the unstructured text provides a rich and important data source to narrow the data gap. For example, by identifying SDGs commitments and contributions in text from the legally binding corporate annual reports, one can evaluate which SDGs and to what extent the corporation is moving towards [@li_global_2022]. Manually reviewing and matching text to specific SDGs or targets can be extremely time-consuming and costly. In addition, though conventional manual coding may achieve high accuracy, it faces precision issues because of intercoder reliability challenges. This issue especially stands out when dealing with textual big data and with the objective to classify and map the massive data into tens and hundreds of topic categories (e.g., 169 SDG targets). To address these challenges, we assembled and refined six databases on SDG search queries [@un_global_2019; @duran-silva_controlled_2019; @jayabalasingham_identifying_2019; @vanderfeesten_search_2020; @schubert_scientific_2020; @bautista-puig_unveiling_2019; @wulff_text2sdg_2021] and developed the **SDGdetector** package to automate the text analysis process in a text mining approach (Figure 1). Our integrated database is unique because it is by far the only available one that can be used to detect SDG-relevant statements at the SDG target level. In combination with this database, the text mining approach, an artificial intelligence (AI) technology, enables us to use natural language processing to transform the unstructured text in documents into normalized and structured data suitable for analysis. After repeated validation and calibration, this package has achieved high accuracy in detecting SDG-related statements in textual data (> 75.5%, measured by the alignment between the R package results and experts’ manually coded results). This package has been used in large-scale research projects in the field of corporate sustainability and urban science [@li_global_2022; @cai_sustainable_2022; @kassens-noor_urban_2022]. 

![](figure1.png)

**Figure 1.** Flowchart for identifying SDG-related statements from textual data.

# Functionality

**SDGdetector** is an R [@r_core_team_r_2021] package that provides functions for three main tasks: 

(1) detecting whether a reported action aligns with any specific Goals (among the 17 SDGs) and Targets (among the 169 targets) under the Global indicator framework for Sustainable Development Goals [@un_global_2019]. The unit of text can be a clause, a sentence, or a paragraph. For the best accuracy, we suggest users split a large chunk of text into sentence or clause levels for analysis.

(2) estimating the priorities of sustainability contributions by counting how frequently a particular Goal or Target is mentioned in the text report. 

(3) detecting which countries or regions are mentioned along with the SDG statements. For global studies, this function provides a means to show where the SDG efforts could be possibly implemented or have been planned.

The package is based on the tidyverse [@wickham_welcome_2019] framework and is available on GitHub https://github.com/Yingjie4Science/SDGdetector. This lightweight package has great potential to be useful in many disciplines with objectives to identify which SDGs and to what extent an entity is putting effort into [@li_global_2022; @cai_sustainable_2022]. The associated lexicon database can be also used for developing similar applications in Python or other programming languages. 



# Acknowledgements

The authors acknowledge contributions from UN Global Sustainability Index Institute (UNGSII) Foundation during the genesis of this project. We thank Racheline Maltese for her input in developing the SDG search terms in the early stage. This work was funded by the National Science Foundation (grant numbers: DEB-1924111, OAC-2118329). 



# References
