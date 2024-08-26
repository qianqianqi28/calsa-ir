# Improving information retrieval through correspondence analysis instead of latent semantic analysis
This project is for Improving information retrieval through correspondence analysis instead of latent semantic analysis.

## Configuration instructions
The project structure distinguishes five folders:
- The folder "small data" is for the toy dataset of 6*6.
- The folder "BBCNews data fold in IR" is for BBCNews dataset.
- The folder "BBCSport data 5 fold in IR" is for BBCSport dataset.
- The folder "20Newsgroups fold in IR" is for 20 Newsgroups dataset.
- The folder "Wilhelmus data fold in IR" is for Wilhelmus dataset.
- The meanprelsacapvalue.R is the R function to calculating the mean average precision (MAP) for latent sementic analysis (LSA) and correspondence analysis (CA), which is used in code folders of the folder "BBCNews data fold in IR", folder "BBCSport data 5 fold in IR", and folder "20Newsgroups fold in IR".
- The meanprerawnrowtfidfpvalue.R is the R function to calculating the MAP for raw, norm L1, norm L2, and tfidf matrices, which is used in code folders of the folder "BBCNews data fold in IR", folder "BBCSport data 5 fold in IR", and folder "20Newsgroups fold in IR".
- The whmeanprelsacapvalue.R is the R function to calculating the MAP for LSA and CA, which is used in code folders of the folder "Wilhelmus data fold in IR".
- The whmeanprerawnrowtfidfpvalue.R is the R function to calculating the MAP for raw, norm L1, norm L2, and tfidf matrices, which is used in code folders of the folder "Wilhelmus data fold in IR".

## Paper
Qi, Q., Hessen, D.J. & van der Heijden, P.G.M. Improving information retrieval through correspondence analysis instead of latent semantic analysis. Journal of Intelligent Information Systems 62, 209–230 (2024). https://doi.org/10.1007/s10844-023-00815-y.

## License
This project is licensed under the terms of the LICENSE GNU General Public License v3.0.
