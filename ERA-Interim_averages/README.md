### 2/26/2020

I'm creating this folder so that you guys can see what I've been doing. So far I've created some code that calculates the average temperature across all pixels at each time step. I'm uploading a .csv of the output, as well as the .R file I used to calculate it.  
The "temp-crawler.R" file is the code that I used to calculate the average temperatures. I used a nested for loop (I'm so sorry, I learned to code with java, not R.) When I timed a loop iteration at home, and calculated how long it would take, my calculations put it at around 21 hours. It probably took a little less than that when I ran it here, since my PC at home is from 2013. There's got to be a more efficient way to do this, I'd love to hear your ideas. 

In the .csv file I left the data values with their default units, which means the temperatures are in kelvin, and the times are integers. I know Ericka had figured out how to translate the units, I may work on that next. 

This is all really preliminary, but I wanted to make what I've got done available to you all so if you wanted to work with it you could. If we want to work with this index, the next step on working with it could be to fit a trend to it, which would need to have a seasonal component to it. 

-Kate
