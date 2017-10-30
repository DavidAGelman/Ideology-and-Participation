# Ideology-and-Participation
This repository contains all the code and data used in "Ideology and Participation at the Constitutional Convention of 1787."

For ease of use, I have broken down the replication materials into smaller parts, with each part corresponding to a section of the paper.  The .R files contain code relevant to that section of the paper regardless of the materials eventual location in either the actual paper of the supplementary materials.  `"CC_Verbosity_Codebook.xlsx"` provides a description of all the variables used across the several different data files in this repository.

## Analysis of Delegate Verbosity
This section corresponds to `"Aggregate_Delegate_Analysis_R_Code"` and uses `"Convention_Verbosity_Data_No_GWashington.csv"` and `"DelegateVerbosityandIdeology.csv"`.

## Analysis of Convention Speeches
This section corresponds to `"Individual_Delegate_Analysis_R_Code.R"` and uses `"CC_SpeechesVerbosity_DataSet"`.  

## Assessing Potential Bias in Madison's Notes
This section corresponds to `"Bias_Assessment_Madisons_Notes.R_Code.R"` and uses `"DiaryDelegateBiasData.csv"`.

## W-NOMINATE at the Convention
This section corresponds to `"Convention_NOMINATE.R"` and uses `"33865-001.rda"`.
As I explain in the paper, Delegate W-NOMINATE scores were originally computed  by Jac C. Heckelman and Keith Dougherty in "A Spatial Analysis of Delegate Voting at the Constitutional Convention" (2013) using the W-NOMINATE Program developed by Keith Poole and Howard Rosenthal (for more on W-NOMINATE, see The W-NOMINATE Program).   However, Heckelman and Dougherty do not provide replication code and NOMINATE can be finicky.  `"Convention_NOMINATE.R"` provides a complete walk through of how to implement W-NOMINATE for the Constitutional Convention delegate roll call data of data available in ICPSR study 33865.  This includes correctly transforming the roll call data, setting the NOMINATE anchor, and creating NOMINATE scores on various dimensions and with different delegates as anchors to verify the veracity of Convention NOMINATE scores.  It also provides how to create the figures used in the paper.
