# Description of script:

The script run\_analysis.R produces a clean data set as laid out in the
instructions for the course’s peer-reviewed assignment.

1.  Getting the data. The data was downloaded and then exracted into a
    folder called UCI HAR Dataset.

2.  Merging the data After creating data frames for each file, the teset
    and training data was merged using the rbind() function.

3.  Extracting mean and std values After using the metadata to create
    column names, another data set was created by extracting only the
    columns with ‘mean’ and ‘std’ values.

4.  Changing variable names. The variables were given more descriptive
    names by changing words such as ‘Acc’ to ‘Accelerometer’, ‘Gyro’ to
    ‘Gyroscope’, ‘Mag’ to ‘Magnitude’, etc. All columns that started
    with the character ‘f’ were renamed with ‘Frequency’ and ‘t’ were
    renamed with ‘Time’.

5.  Create a second data set with the average of each variable. This
    FinalData set was created by first grouping by subject and activity
    and then using the summarise function to produce the means of each
    activity and each subject.

# Data report overview

The dataset examined has the following dimensions:

<table style="width:47%;">
<colgroup>
<col style="width: 34%" />
<col style="width: 12%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Number of observations</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of variables</td>
<td style="text-align: right;">88</td>
</tr>
</tbody>
</table>

# Codebook summary table

<table>
<colgroup>
<col style="width: 7%" />
<col style="width: 52%" />
<col style="width: 9%" />
<col style="width: 9%" />
<col style="width: 9%" />
<col style="width: 12%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Label</th>
<th style="text-align: left;">Variable</th>
<th style="text-align: left;">Class</th>
<th style="text-align: right;"># unique values</th>
<th style="text-align: center;">Missing</th>
<th style="text-align: left;">Description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#subject">Subject</a></strong></td>
<td style="text-align: left;">factor</td>
<td style="text-align: right;">30</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#activity">Activity</a></strong></td>
<td style="text-align: left;">factor</td>
<td style="text-align: right;">6</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#timebodyaccelerometermean-x">TimeBodyAccelerometerMean()-X</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#timebodyaccelerometermean-y">TimeBodyAccelerometerMean()-Y</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#timebodyaccelerometermean-z">TimeBodyAccelerometerMean()-Z</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#timebodyaccelerometerstd-x">TimeBodyAccelerometerSTD()-X</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#timebodyaccelerometerstd-y">TimeBodyAccelerometerSTD()-Y</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#timebodyaccelerometerstd-z">TimeBodyAccelerometerSTD()-Z</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#timegravityaccelerometermean-x">TimeGravityAccelerometerMean()-X</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#timegravityaccelerometermean-y">TimeGravityAccelerometerMean()-Y</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#timegravityaccelerometermean-z">TimeGravityAccelerometerMean()-Z</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#timegravityaccelerometerstd-x">TimeGravityAccelerometerSTD()-X</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#timegravityaccelerometerstd-y">TimeGravityAccelerometerSTD()-Y</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#timegravityaccelerometerstd-z">TimeGravityAccelerometerSTD()-Z</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#timebodyaccelerometerjerkmean-x">TimeBodyAccelerometerJerkMean()-X</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#timebodyaccelerometerjerkmean-y">TimeBodyAccelerometerJerkMean()-Y</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#timebodyaccelerometerjerkmean-z">TimeBodyAccelerometerJerkMean()-Z</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#timebodyaccelerometerjerkstd-x">TimeBodyAccelerometerJerkSTD()-X</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#timebodyaccelerometerjerkstd-y">TimeBodyAccelerometerJerkSTD()-Y</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#timebodyaccelerometerjerkstd-z">TimeBodyAccelerometerJerkSTD()-Z</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#timebodygyroscopemean-x">TimeBodyGyroscopeMean()-X</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#timebodygyroscopemean-y">TimeBodyGyroscopeMean()-Y</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#timebodygyroscopemean-z">TimeBodyGyroscopeMean()-Z</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#timebodygyroscopestd-x">TimeBodyGyroscopeSTD()-X</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#timebodygyroscopestd-y">TimeBodyGyroscopeSTD()-Y</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#timebodygyroscopestd-z">TimeBodyGyroscopeSTD()-Z</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#timebodygyroscopejerkmean-x">TimeBodyGyroscopeJerkMean()-X</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#timebodygyroscopejerkmean-y">TimeBodyGyroscopeJerkMean()-Y</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#timebodygyroscopejerkmean-z">TimeBodyGyroscopeJerkMean()-Z</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#timebodygyroscopejerkstd-x">TimeBodyGyroscopeJerkSTD()-X</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#timebodygyroscopejerkstd-y">TimeBodyGyroscopeJerkSTD()-Y</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#timebodygyroscopejerkstd-z">TimeBodyGyroscopeJerkSTD()-Z</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#timebodyaccelerometermagnitudemean">TimeBodyAccelerometerMagnitudeMean()</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#timebodyaccelerometermagnitudestd">TimeBodyAccelerometerMagnitudeSTD()</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#timegravityaccelerometermagnitudemean">TimeGravityAccelerometerMagnitudeMean()</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#timegravityaccelerometermagnitudestd">TimeGravityAccelerometerMagnitudeSTD()</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#timebodyaccelerometerjerkmagnitudemean">TimeBodyAccelerometerJerkMagnitudeMean()</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#timebodyaccelerometerjerkmagnitudestd">TimeBodyAccelerometerJerkMagnitudeSTD()</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#timebodygyroscopemagnitudemean">TimeBodyGyroscopeMagnitudeMean()</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#timebodygyroscopemagnitudestd">TimeBodyGyroscopeMagnitudeSTD()</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#timebodygyroscopejerkmagnitudemean">TimeBodyGyroscopeJerkMagnitudeMean()</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#timebodygyroscopejerkmagnitudestd">TimeBodyGyroscopeJerkMagnitudeSTD()</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#frequencybodyaccelerometermean-x">FrequencyBodyAccelerometerMean()-X</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#frequencybodyaccelerometermean-y">FrequencyBodyAccelerometerMean()-Y</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#frequencybodyaccelerometermean-z">FrequencyBodyAccelerometerMean()-Z</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#frequencybodyaccelerometerstd-x">FrequencyBodyAccelerometerSTD()-X</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#frequencybodyaccelerometerstd-y">FrequencyBodyAccelerometerSTD()-Y</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#frequencybodyaccelerometerstd-z">FrequencyBodyAccelerometerSTD()-Z</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#frequencybodyaccelerometermeanfreq-x">FrequencyBodyAccelerometerMeanFreq()-X</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#frequencybodyaccelerometermeanfreq-y">FrequencyBodyAccelerometerMeanFreq()-Y</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#frequencybodyaccelerometermeanfreq-z">FrequencyBodyAccelerometerMeanFreq()-Z</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#frequencybodyaccelerometerjerkmean-x">FrequencyBodyAccelerometerJerkMean()-X</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#frequencybodyaccelerometerjerkmean-y">FrequencyBodyAccelerometerJerkMean()-Y</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#frequencybodyaccelerometerjerkmean-z">FrequencyBodyAccelerometerJerkMean()-Z</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#frequencybodyaccelerometerjerkstd-x">FrequencyBodyAccelerometerJerkSTD()-X</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#frequencybodyaccelerometerjerkstd-y">FrequencyBodyAccelerometerJerkSTD()-Y</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#frequencybodyaccelerometerjerkstd-z">FrequencyBodyAccelerometerJerkSTD()-Z</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#frequencybodyaccelerometerjerkmeanfreq-x">FrequencyBodyAccelerometerJerkMeanFreq()-X</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#frequencybodyaccelerometerjerkmeanfreq-y">FrequencyBodyAccelerometerJerkMeanFreq()-Y</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#frequencybodyaccelerometerjerkmeanfreq-z">FrequencyBodyAccelerometerJerkMeanFreq()-Z</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#frequencybodygyroscopemean-x">FrequencyBodyGyroscopeMean()-X</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#frequencybodygyroscopemean-y">FrequencyBodyGyroscopeMean()-Y</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#frequencybodygyroscopemean-z">FrequencyBodyGyroscopeMean()-Z</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#frequencybodygyroscopestd-x">FrequencyBodyGyroscopeSTD()-X</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#frequencybodygyroscopestd-y">FrequencyBodyGyroscopeSTD()-Y</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#frequencybodygyroscopestd-z">FrequencyBodyGyroscopeSTD()-Z</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#frequencybodygyroscopemeanfreq-x">FrequencyBodyGyroscopeMeanFreq()-X</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#frequencybodygyroscopemeanfreq-y">FrequencyBodyGyroscopeMeanFreq()-Y</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#frequencybodygyroscopemeanfreq-z">FrequencyBodyGyroscopeMeanFreq()-Z</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#frequencybodyaccelerometermagnitudemean">FrequencyBodyAccelerometerMagnitudeMean()</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#frequencybodyaccelerometermagnitudestd">FrequencyBodyAccelerometerMagnitudeSTD()</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#frequencybodyaccelerometermagnitudemeanfreq">FrequencyBodyAccelerometerMagnitudeMeanFreq()</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#frequencybodyaccelerometerjerkmagnitudemean">FrequencyBodyAccelerometerJerkMagnitudeMean()</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#frequencybodyaccelerometerjerkmagnitudestd">FrequencyBodyAccelerometerJerkMagnitudeSTD()</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#frequencybodyaccelerometerjerkmagnitudemeanfreq">FrequencyBodyAccelerometerJerkMagnitudeMeanFreq()</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#frequencybodygyroscopemagnitudemean">FrequencyBodyGyroscopeMagnitudeMean()</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#frequencybodygyroscopemagnitudestd">FrequencyBodyGyroscopeMagnitudeSTD()</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#frequencybodygyroscopemagnitudemeanfreq">FrequencyBodyGyroscopeMagnitudeMeanFreq()</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#frequencybodygyroscopejerkmagnitudemean">FrequencyBodyGyroscopeJerkMagnitudeMean()</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#frequencybodygyroscopejerkmagnitudestd">FrequencyBodyGyroscopeJerkMagnitudeSTD()</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#frequencybodygyroscopejerkmagnitudemeanfreq">FrequencyBodyGyroscopeJerkMagnitudeMeanFreq()</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#angletimebodyaccelerometermeangravity">Angle(TimeBodyAccelerometerMean,Gravity)</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#angletimebodyaccelerometerjerkmeangravitymean">Angle(TimeBodyAccelerometerJerkMean),GravityMean)</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#angletimebodygyroscopemeangravitymean">Angle(TimeBodyGyroscopeMean,GravityMean)</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#angletimebodygyroscopejerkmeangravitymean">Angle(TimeBodyGyroscopeJerkMean,GravityMean)</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#anglexgravitymean">Angle(X,GravityMean)</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#angleygravitymean">Angle(Y,GravityMean)</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"><strong><a
href="#anglezgravitymean">Angle(Z,GravityMean)</a></strong></td>
<td style="text-align: left;">numeric</td>
<td style="text-align: right;">180</td>
<td style="text-align: center;">0.00 %</td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>

# Variable list

## Subject

<table style="width:50%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 13%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">factor</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">30</td>
</tr>
<tr class="even">
<td style="text-align: left;">Mode</td>
<td style="text-align: right;">“1”</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Reference category</td>
<td style="text-align: right;">1</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-1-Subject-1.png)

-   Observed factor levels: "1", "10", "11", "12", "13", "14", "15",
    "16", "17", "18", "19", "2", "20", "21", "22", "23", "24", "25",
    "26", "27", "28", "29", "3", "30", "4", "5", "6", "7", "8", "9".

------------------------------------------------------------------------

## Activity

<table style="width:51%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 15%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">factor</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">6</td>
</tr>
<tr class="even">
<td style="text-align: left;">Mode</td>
<td style="text-align: right;">“LAYING”</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Reference category</td>
<td style="text-align: right;">LAYING</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-2-Activity-1.png)

-   Observed factor levels: "LAYING", "SITTING", "STANDING", "WALKING",
    "WALKING\_DOWNSTAIRS", "WALKING\_UPSTAIRS".

------------------------------------------------------------------------

## TimeBodyAccelerometerMean()-X

<table style="width:54%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 18%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">0.28</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">0.27; 0.28</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">0.22; 0.3</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-3-TimeBodyAccelerometerMean()-X-1.png)

------------------------------------------------------------------------

## TimeBodyAccelerometerMean()-Y

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.02</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.02; -0.01</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.04; 0</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-4-TimeBodyAccelerometerMean()-Y-1.png)

------------------------------------------------------------------------

## TimeBodyAccelerometerMean()-Z

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.11</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.11; -0.1</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.15; -0.08</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-5-TimeBodyAccelerometerMean()-Z-1.png)

------------------------------------------------------------------------

## TimeBodyAccelerometerSTD()-X

<table style="width:56%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 19%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.75</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.98; -0.2</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-1; 0.63</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-6-TimeBodyAccelerometerSTD()-X-1.png)

------------------------------------------------------------------------

## TimeBodyAccelerometerSTD()-Y

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.51</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.94; -0.03</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.99; 0.62</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-7-TimeBodyAccelerometerSTD()-Y-1.png)

------------------------------------------------------------------------

## TimeBodyAccelerometerSTD()-Z

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.65</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.95; -0.23</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.99; 0.61</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-8-TimeBodyAccelerometerSTD()-Z-1.png)

------------------------------------------------------------------------

## TimeGravityAccelerometerMean()-X

<table style="width:56%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 19%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">0.92</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">0.84; 0.94</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.68; 0.97</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-9-TimeGravityAccelerometerMean()-X-1.png)

------------------------------------------------------------------------

## TimeGravityAccelerometerMean()-Y

<table style="width:56%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 19%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.13</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.23; 0.09</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.48; 0.96</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-10-TimeGravityAccelerometerMean()-Y-1.png)

------------------------------------------------------------------------

## TimeGravityAccelerometerMean()-Z

<table style="width:56%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 19%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">0.02</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.12; 0.15</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.5; 0.96</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-11-TimeGravityAccelerometerMean()-Z-1.png)

------------------------------------------------------------------------

## TimeGravityAccelerometerSTD()-X

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.97</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.98; -0.95</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-1; -0.83</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-12-TimeGravityAccelerometerSTD()-X-1.png)

------------------------------------------------------------------------

## TimeGravityAccelerometerSTD()-Y

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.96</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.97; -0.94</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.99; -0.64</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-13-TimeGravityAccelerometerSTD()-Y-1.png)

------------------------------------------------------------------------

## TimeGravityAccelerometerSTD()-Z

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.95</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.96; -0.92</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.99; -0.61</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-14-TimeGravityAccelerometerSTD()-Z-1.png)

------------------------------------------------------------------------

## TimeBodyAccelerometerJerkMean()-X

<table style="width:54%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 18%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">0.08</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">0.07; 0.08</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">0.04; 0.13</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-15-TimeBodyAccelerometerJerkMean()-X-1.png)

------------------------------------------------------------------------

## TimeBodyAccelerometerJerkMean()-Y

<table style="width:56%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 19%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">0.01</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">0; 0.01</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.04; 0.06</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-16-TimeBodyAccelerometerJerkMean()-Y-1.png)

------------------------------------------------------------------------

## TimeBodyAccelerometerJerkMean()-Z

<table style="width:56%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 19%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">0</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.01; 0</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.07; 0.04</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-17-TimeBodyAccelerometerJerkMean()-Z-1.png)

------------------------------------------------------------------------

## TimeBodyAccelerometerJerkSTD()-X

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.81</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.98; -0.22</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.99; 0.54</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-18-TimeBodyAccelerometerJerkSTD()-X-1.png)

------------------------------------------------------------------------

## TimeBodyAccelerometerJerkSTD()-Y

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.78</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.97; -0.15</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.99; 0.36</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-19-TimeBodyAccelerometerJerkSTD()-Y-1.png)

------------------------------------------------------------------------

## TimeBodyAccelerometerJerkSTD()-Z

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.88</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.98; -0.51</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.99; 0.03</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-20-TimeBodyAccelerometerJerkSTD()-Z-1.png)

------------------------------------------------------------------------

## TimeBodyGyroscopeMean()-X

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.03</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.05; -0.02</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.21; 0.19</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-21-TimeBodyGyroscopeMean()-X-1.png)

------------------------------------------------------------------------

## TimeBodyGyroscopeMean()-Y

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.07</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.09; -0.06</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.2; 0.03</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-22-TimeBodyGyroscopeMean()-Y-1.png)

------------------------------------------------------------------------

## TimeBodyGyroscopeMean()-Z

<table style="width:56%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 19%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">0.09</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">0.07; 0.1</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.07; 0.18</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-23-TimeBodyGyroscopeMean()-Z-1.png)

------------------------------------------------------------------------

## TimeBodyGyroscopeSTD()-X

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.79</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.97; -0.44</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.99; 0.27</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-24-TimeBodyGyroscopeSTD()-X-1.png)

------------------------------------------------------------------------

## TimeBodyGyroscopeSTD()-Y

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.8</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.96; -0.42</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.99; 0.48</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-25-TimeBodyGyroscopeSTD()-Y-1.png)

------------------------------------------------------------------------

## TimeBodyGyroscopeSTD()-Z

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.8</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.96; -0.31</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.99; 0.56</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-26-TimeBodyGyroscopeSTD()-Z-1.png)

------------------------------------------------------------------------

## TimeBodyGyroscopeJerkMean()-X

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.1</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.1; -0.09</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.16; -0.02</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-27-TimeBodyGyroscopeJerkMean()-X-1.png)

------------------------------------------------------------------------

## TimeBodyGyroscopeJerkMean()-Y

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.04</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.05; -0.04</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.08; -0.01</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-28-TimeBodyGyroscopeJerkMean()-Y-1.png)

------------------------------------------------------------------------

## TimeBodyGyroscopeJerkMean()-Z

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.05</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.06; -0.05</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.09; -0.01</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-29-TimeBodyGyroscopeJerkMean()-Z-1.png)

------------------------------------------------------------------------

## TimeBodyGyroscopeJerkSTD()-X

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.84</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.98; -0.46</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-1; 0.18</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-30-TimeBodyGyroscopeJerkSTD()-X-1.png)

------------------------------------------------------------------------

## TimeBodyGyroscopeJerkSTD()-Y

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.89</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.98; -0.59</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-1; 0.3</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-31-TimeBodyGyroscopeJerkSTD()-Y-1.png)

------------------------------------------------------------------------

## TimeBodyGyroscopeJerkSTD()-Z

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.86</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.98; -0.47</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-1; 0.19</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-32-TimeBodyGyroscopeJerkSTD()-Z-1.png)

------------------------------------------------------------------------

## TimeBodyAccelerometerMagnitudeMean()

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.48</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.96; -0.09</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.99; 0.64</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-33-TimeBodyAccelerometerMagnitudeMean()-1.png)

------------------------------------------------------------------------

## TimeBodyAccelerometerMagnitudeSTD()

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.61</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.94; -0.21</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.99; 0.43</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-34-TimeBodyAccelerometerMagnitudeSTD()-1.png)

------------------------------------------------------------------------

## TimeGravityAccelerometerMagnitudeMean()

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.48</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.96; -0.09</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.99; 0.64</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-35-TimeGravityAccelerometerMagnitudeMean()-1.png)

------------------------------------------------------------------------

## TimeGravityAccelerometerMagnitudeSTD()

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.61</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.94; -0.21</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.99; 0.43</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-36-TimeGravityAccelerometerMagnitudeSTD()-1.png)

------------------------------------------------------------------------

## TimeBodyAccelerometerJerkMagnitudeMean()

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.82</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.98; -0.25</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.99; 0.43</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-37-TimeBodyAccelerometerJerkMagnitudeMean()-1.png)

------------------------------------------------------------------------

## TimeBodyAccelerometerJerkMagnitudeSTD()

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.8</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.98; -0.22</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.99; 0.45</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-38-TimeBodyAccelerometerJerkMagnitudeSTD()-1.png)

------------------------------------------------------------------------

## TimeBodyGyroscopeMagnitudeMean()

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.66</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.95; -0.22</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.98; 0.42</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-39-TimeBodyGyroscopeMagnitudeMean()-1.png)

------------------------------------------------------------------------

## TimeBodyGyroscopeMagnitudeSTD()

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.74</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.95; -0.36</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.98; 0.3</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-40-TimeBodyGyroscopeMagnitudeSTD()-1.png)

------------------------------------------------------------------------

## TimeBodyGyroscopeJerkMagnitudeMean()

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.86</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.99; -0.51</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-1; 0.09</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-41-TimeBodyGyroscopeJerkMagnitudeMean()-1.png)

------------------------------------------------------------------------

## TimeBodyGyroscopeJerkMagnitudeSTD()

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.88</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.98; -0.58</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-1; 0.25</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-42-TimeBodyGyroscopeJerkMagnitudeSTD()-1.png)

------------------------------------------------------------------------

## FrequencyBodyAccelerometerMean()-X

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.77</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.98; -0.22</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-1; 0.54</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-43-FrequencyBodyAccelerometerMean()-X-1.png)

------------------------------------------------------------------------

## FrequencyBodyAccelerometerMean()-Y

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.59</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.95; -0.06</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.99; 0.52</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-44-FrequencyBodyAccelerometerMean()-Y-1.png)

------------------------------------------------------------------------

## FrequencyBodyAccelerometerMean()-Z

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.72</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.96; -0.32</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.99; 0.28</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-45-FrequencyBodyAccelerometerMean()-Z-1.png)

------------------------------------------------------------------------

## FrequencyBodyAccelerometerSTD()-X

<table style="width:56%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 19%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.75</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.98; -0.2</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-1; 0.66</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-46-FrequencyBodyAccelerometerSTD()-X-1.png)

------------------------------------------------------------------------

## FrequencyBodyAccelerometerSTD()-Y

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.51</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.94; -0.08</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.99; 0.56</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-47-FrequencyBodyAccelerometerSTD()-Y-1.png)

------------------------------------------------------------------------

## FrequencyBodyAccelerometerSTD()-Z

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.64</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.95; -0.27</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.99; 0.69</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-48-FrequencyBodyAccelerometerSTD()-Z-1.png)

------------------------------------------------------------------------

## FrequencyBodyAccelerometerMeanFreq()-X

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.26</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.39; -0.06</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.64; 0.16</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-49-FrequencyBodyAccelerometerMeanFreq()-X-1.png)

------------------------------------------------------------------------

## FrequencyBodyAccelerometerMeanFreq()-Y

<table style="width:56%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 19%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">0.01</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.08; 0.09</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.38; 0.47</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-50-FrequencyBodyAccelerometerMeanFreq()-Y-1.png)

------------------------------------------------------------------------

## FrequencyBodyAccelerometerMeanFreq()-Z

<table style="width:56%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 19%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">0.07</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.04; 0.18</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.52; 0.4</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-51-FrequencyBodyAccelerometerMeanFreq()-Z-1.png)

------------------------------------------------------------------------

## FrequencyBodyAccelerometerJerkMean()-X

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.81</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.98; -0.28</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.99; 0.47</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-52-FrequencyBodyAccelerometerJerkMean()-X-1.png)

------------------------------------------------------------------------

## FrequencyBodyAccelerometerJerkMean()-Y

<table style="width:56%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 19%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.78</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.97; -0.2</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.99; 0.28</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-53-FrequencyBodyAccelerometerJerkMean()-Y-1.png)

------------------------------------------------------------------------

## FrequencyBodyAccelerometerJerkMean()-Z

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.87</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.98; -0.47</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.99; 0.16</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-54-FrequencyBodyAccelerometerJerkMean()-Z-1.png)

------------------------------------------------------------------------

## FrequencyBodyAccelerometerJerkSTD()-X

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.83</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.98; -0.25</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-1; 0.48</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-55-FrequencyBodyAccelerometerJerkSTD()-X-1.png)

------------------------------------------------------------------------

## FrequencyBodyAccelerometerJerkSTD()-Y

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.79</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.97; -0.17</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.99; 0.35</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-56-FrequencyBodyAccelerometerJerkSTD()-Y-1.png)

------------------------------------------------------------------------

## FrequencyBodyAccelerometerJerkSTD()-Z

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.9</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.98; -0.54</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.99; -0.01</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-57-FrequencyBodyAccelerometerJerkSTD()-Z-1.png)

------------------------------------------------------------------------

## FrequencyBodyAccelerometerJerkMeanFreq()-X

<table style="width:56%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 19%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.06</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.29; 0.18</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.58; 0.33</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-58-FrequencyBodyAccelerometerJerkMeanFreq()-X-1.png)

------------------------------------------------------------------------

## FrequencyBodyAccelerometerJerkMeanFreq()-Y

<table style="width:56%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 19%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.23</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.4; -0.05</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.6; 0.2</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-59-FrequencyBodyAccelerometerJerkMeanFreq()-Y-1.png)

------------------------------------------------------------------------

## FrequencyBodyAccelerometerJerkMeanFreq()-Z

<table style="width:56%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 19%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.09</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.31; 0.04</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.63; 0.23</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-60-FrequencyBodyAccelerometerJerkMeanFreq()-Z-1.png)

------------------------------------------------------------------------

## FrequencyBodyGyroscopeMean()-X

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.73</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.97; -0.34</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.99; 0.47</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-61-FrequencyBodyGyroscopeMean()-X-1.png)

------------------------------------------------------------------------

## FrequencyBodyGyroscopeMean()-Y

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.81</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.97; -0.45</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.99; 0.33</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-62-FrequencyBodyGyroscopeMean()-Y-1.png)

------------------------------------------------------------------------

## FrequencyBodyGyroscopeMean()-Z

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.79</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.96; -0.26</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.99; 0.49</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-63-FrequencyBodyGyroscopeMean()-Z-1.png)

------------------------------------------------------------------------

## FrequencyBodyGyroscopeSTD()-X

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.81</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.98; -0.48</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.99; 0.2</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-64-FrequencyBodyGyroscopeSTD()-X-1.png)

------------------------------------------------------------------------

## FrequencyBodyGyroscopeSTD()-Y

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.8</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.96; -0.42</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.99; 0.65</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-65-FrequencyBodyGyroscopeSTD()-Y-1.png)

------------------------------------------------------------------------

## FrequencyBodyGyroscopeSTD()-Z

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.82</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.96; -0.39</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.99; 0.52</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-66-FrequencyBodyGyroscopeSTD()-Z-1.png)

------------------------------------------------------------------------

## FrequencyBodyGyroscopeMeanFreq()-X

<table style="width:54%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 18%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.12</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.21; 0</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.4; 0.25</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-67-FrequencyBodyGyroscopeMeanFreq()-X-1.png)

------------------------------------------------------------------------

## FrequencyBodyGyroscopeMeanFreq()-Y

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.16</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.29; -0.04</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.67; 0.27</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-68-FrequencyBodyGyroscopeMeanFreq()-Y-1.png)

------------------------------------------------------------------------

## FrequencyBodyGyroscopeMeanFreq()-Z

<table style="width:56%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 19%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.05</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.15; 0.04</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.51; 0.38</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-69-FrequencyBodyGyroscopeMeanFreq()-Z-1.png)

------------------------------------------------------------------------

## FrequencyBodyAccelerometerMagnitudeMean()

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.67</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.96; -0.16</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.99; 0.59</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-70-FrequencyBodyAccelerometerMagnitudeMean()-1.png)

------------------------------------------------------------------------

## FrequencyBodyAccelerometerMagnitudeSTD()

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.65</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.95; -0.37</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.99; 0.18</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-71-FrequencyBodyAccelerometerMagnitudeSTD()-1.png)

------------------------------------------------------------------------

## FrequencyBodyAccelerometerMagnitudeMeanFreq()

<table style="width:56%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 19%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">0.08</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.01; 0.17</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.31; 0.44</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-72-FrequencyBodyAccelerometerMagnitudeMeanFreq()-1.png)

------------------------------------------------------------------------

## FrequencyBodyAccelerometerJerkMagnitudeMean()

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.79</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.98; -0.19</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.99; 0.54</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-73-FrequencyBodyAccelerometerJerkMagnitudeMean()-1.png)

------------------------------------------------------------------------

## FrequencyBodyAccelerometerJerkMagnitudeSTD()

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.81</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.98; -0.27</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.99; 0.32</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-74-FrequencyBodyAccelerometerJerkMagnitudeSTD()-1.png)

------------------------------------------------------------------------

## FrequencyBodyAccelerometerJerkMagnitudeMeanFreq()

<table style="width:56%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 19%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">0.17</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">0.05; 0.28</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.13; 0.49</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-75-FrequencyBodyAccelerometerJerkMagnitudeMeanFreq()-1.png)

------------------------------------------------------------------------

## FrequencyBodyGyroscopeMagnitudeMean()

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.77</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.96; -0.41</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.99; 0.2</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-76-FrequencyBodyGyroscopeMagnitudeMean()-1.png)

------------------------------------------------------------------------

## FrequencyBodyGyroscopeMagnitudeSTD()

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.77</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.95; -0.43</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.98; 0.24</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-77-FrequencyBodyGyroscopeMagnitudeSTD()-1.png)

------------------------------------------------------------------------

## FrequencyBodyGyroscopeMagnitudeMeanFreq()

<table style="width:56%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 19%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.05</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.17; 0.08</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.46; 0.41</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-78-FrequencyBodyGyroscopeMagnitudeMeanFreq()-1.png)

------------------------------------------------------------------------

## FrequencyBodyGyroscopeJerkMagnitudeMean()

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.88</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.98; -0.58</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-1; 0.15</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-79-FrequencyBodyGyroscopeJerkMagnitudeMean()-1.png)

------------------------------------------------------------------------

## FrequencyBodyGyroscopeJerkMagnitudeSTD()

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.89</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.98; -0.61</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-1; 0.29</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-80-FrequencyBodyGyroscopeJerkMagnitudeSTD()-1.png)

------------------------------------------------------------------------

## FrequencyBodyGyroscopeJerkMagnitudeMeanFreq()

<table style="width:56%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 19%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">0.11</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">0.05; 0.21</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.18; 0.43</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-81-FrequencyBodyGyroscopeJerkMagnitudeMeanFreq()-1.png)

------------------------------------------------------------------------

## Angle(TimeBodyAccelerometerMean,Gravity)

<table style="width:56%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 19%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">0.01</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.01; 0.02</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.16; 0.13</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-82-Angle(TimeBodyAccelerometerMean,Gravity)-1.png)

------------------------------------------------------------------------

## Angle(TimeBodyAccelerometerJerkMean),GravityMean)

<table style="width:56%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 19%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">0</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.02; 0.02</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.12; 0.2</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-83-Angle(TimeBodyAccelerometerJerkMean),GravityMean)-1.png)

------------------------------------------------------------------------

## Angle(TimeBodyGyroscopeMean,GravityMean)

<table style="width:56%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 19%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">0.02</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.02; 0.06</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.39; 0.44</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-84-Angle(TimeBodyGyroscopeMean,GravityMean)-1.png)

------------------------------------------------------------------------

## Angle(TimeBodyGyroscopeJerkMean,GravityMean)

<table style="width:56%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 19%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.02</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.06; 0.03</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.22; 0.18</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-85-Angle(TimeBodyGyroscopeJerkMean,GravityMean)-1.png)

------------------------------------------------------------------------

## Angle(X,GravityMean)

<table style="width:57%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">-0.74</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.79; -0.58</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.95; 0.74</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-86-Angle(X,GravityMean)-1.png)

------------------------------------------------------------------------

## Angle(Y,GravityMean)

<table style="width:56%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 19%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">0.17</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">0.02; 0.24</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.87; 0.42</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-87-Angle(Y,GravityMean)-1.png)

------------------------------------------------------------------------

## Angle(Z,GravityMean)

<table style="width:56%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 19%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Feature</th>
<th style="text-align: right;">Result</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Variable type</td>
<td style="text-align: right;">numeric</td>
</tr>
<tr class="even">
<td style="text-align: left;">Number of missing obs.</td>
<td style="text-align: right;">0 (0 %)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Number of unique values</td>
<td style="text-align: right;">180</td>
</tr>
<tr class="even">
<td style="text-align: left;">Median</td>
<td style="text-align: right;">0.01</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1st and 3rd quartiles</td>
<td style="text-align: right;">-0.08; 0.11</td>
</tr>
<tr class="even">
<td style="text-align: left;">Min. and max.</td>
<td style="text-align: right;">-0.87; 0.39</td>
</tr>
</tbody>
</table>

![](codebook_FinalData_files/figure-markdown_strict/Var-88-Angle(Z,GravityMean)-1.png)

------------------------------------------------------------------------

Report generation information:

-   Created by: Mattias Karen (username: `mattiaskaren`).

-   Report creation time: Mon Jan 30 2023 16:11:28

-   Report was run from directory:
    `/Users/mattiaskaren/Documents/datasciencecoursera/Cleaning_data_assignment`

-   dataMaid v1.4.1 \[Pkg: 2021-10-08 from CRAN (R 4.2.0)\]

-   R version 4.2.1 (2022-06-23).

-   Platform: aarch64-apple-darwin20 (64-bit)(macOS Big Sur 11.6).

-   Function call:
    `dataMaid::makeDataReport(data = FinalData, mode = c("summarize",  "visualize", "check"), smartNum = FALSE, file = "codebook_FinalData.Rmd",      replace = TRUE, checks = list(character = "showAllFactorLevels",          factor = "showAllFactorLevels", labelled = "showAllFactorLevels",          haven_labelled = "showAllFactorLevels", numeric = NULL,          integer = NULL, logical = NULL, Date = NULL), listChecks = FALSE,      maxProbVals = Inf, codebook = TRUE, reportTitle = "Codebook for FinalData")`
