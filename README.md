# Speech-Rewards-Unless-You-Are-a-Women
This is the repository containing the code and data for the replication of the article.

Please download the data on legislative speeches using this link.
https://drive.google.com/file/d/1Q8wrRr4GqO-wehD11-M5ScmTzZcifhau/view?usp=sharing

This data is compiled using this code in the repository: tbmm_tutanaklar_web_scraping.ipynb

Use this code to calculate the number of words and sessions spoken for each MP in each year: milletvekilleri_word_sentence_count.jpynb

Using the libraries for gendered Turkish names in this (https://github.com/eoner/turkce_isimler) depository, find the gender of MPs using code:gender_temp.ipynb
The genders of some of the MPs with gender-neutral names are manually edited later when cleaning and merging data.

For ease of replication, I have merged the resulting data on genders and speech data for each individual and upload that csv in this depository. You can use speech_data_genders.csv to skip the processes describes above.

The rest of the cleaning, merging and analysis can be reproduced using this code: Cleaning_Merging_Analysis.R and election data available in this repository.
