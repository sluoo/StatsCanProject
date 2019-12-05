## Job Tenure of Canada
- Based on data from Statistics Canada, we wish to explore how the length of employment changed over the period of 1976 to 2018, by age and sex. 
- Job tenure is defined as the length of time a work has been continuously employed by the same employer and is not affected if an employee changes position within the same company
## An animated pyramid plot 
- The animated pyramid plot illustrates the evolution of tenure according to gender from 1976 to 2018. 
- Selected high contrast colors and added directed labels 
- Output as MP4 to have more control of each frame and slowed down the animation with fps
- For visual purposes, assumed bin width to be equal for each bracket 
## An animated pyramid plot
<video width="320" height="240" controls>
  <source src="output_pyramid.mp4" type="video/mp4">
    </video>
    ## An animated pyramid plot 
    - It appears that job tenure has increased for both genders and high proportion of men in each of the three categories compared to women. 
  - Impact of economic trends on job tenure
  Generally, when the economy is stable, the number of short-term jobs increase, and transition cycles follow accordingly
  When a financial crisis strikes such as the recession in 2008 - 2012, the number of short-term jobs decrease, but the transition cycles in both the intermediate and longer-term brackets remain relatively unchanged. 
  i.e. layoffs affect workers with lower seniority more compared to workers with higher seniority 
  ## Average Tenure by Sex
  -	Created interactive plot with plotly 
  -	Selected high contrast colors and to be consistent applied the same colors used in the previous plot 
  -	Interactive component (hover for more detail and able to click and hide legend)
  
  
  
  ## Average Tenure by Sex 
  -	Examined mean tenure by gender and a more detailed trend of job tenure is exhibited  
  -	Huge gap between men and women but over time this gap narrowed as more women entered the work force around the early 90s 
  -	Mean tenure for women has been increasing steadily.
  -	After 1996, mean tenure for men has decreased compared to previous years 
  -	See impact of the recession here as well 
  -	This graph did not take account the age groups, so this plot could be misleading 
  o	View age separately for each gender 
  ## Average Tenure by Sex and Age
  -	Selected colors with high contrast, aesthetic consistency with other plots, and to highlight the ordered age categories while trying to keep the image accessible to the visually impaired.
  -	Facets were used to allow a side-by-side comparison of trends for each gender and age group
  -	We used a linear axis as the discrepancies between groups were fairly substantial and a log scale would have been rather deceptive when it came to showing the differences between groups. 
  -	Interactive elements were used as they allow the data to be explored in greater detail without adding excessive clutter to the plot.
  -	For females, we see that for all age groups except for the 15 to 24 year age brackets, there is a slight increase in the average job tenure over time.
  -	For males aged 65 years or more, job tenure increases until 1996 and substantially decreases afterwards. We also see a similar trend in 55 to 64 year old males, though the decrease is very marginal compared to 65 years and older. 
  Sunburst Plot
  -	Cumulatively from 1976 to 2018, the workforce is 40% female and 60% male. 
  -	The largest tenure bracket for both genders appears to be 1 to 5 years, though the 5 to 10 year, 10 to 20 year, and 20 plus year brackets are all fairly large as well. 
  -	We can explore the workforce composition for each sex at different job tenure levels during different years. For example, among female workers who have been at their jobs for 1 to 5 years in 2009, the majority are between 25 and 54 years old. 
  -	We can also look at the same scenario for men. In 2009, male workers who have been at their job for 1 to 5 years are overwhelmingly between 25 and 54 years old, mirroring the trend that we saw in female workers.
  
  Overall Conclusion
  -	Job tenures are increasing for females, whereas job tenure for males is decreasing.  Several possible reasons for these trends are: 
    o	better workplace accommodations for women such as maternity leave, as well as possible social shifts on the role of men in parenting. 
  o	The unstable yet highly lucrative nature of work in the technology sector, which is heavily male-dominated, may partially explain the trend towards shorter job tenures for males, who may hop from one startup company to another for short periods of time before making enough money to exit the traditional full-time workforce. 
  o	Globalization may result in highly experienced workers having greater access to lucrative job positions in other countries, which would result in an outflux of workers in older age brackets.
  o	People from the silent generation may have come to retirement age in large numbers from 1997 onwards
  Choropleth
  -	Now that we have looked at how job tenures for both sexes have changed over time, we will now change our focus to pay, namely the state of the gender pay gap today. 
  