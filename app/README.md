# arl-votes-2020
A application that tracks publicly-available voting data for Arlington County, VA. This app is a work in progress, and I will continue to publish new features and updates to the data as they become available.

# About Arlington Votes 2020
This app is intended as a public good to
visualize open voting data to all residents of Arlington County,
Virginia and to the general public at large. As voting is
fundamental right and responsibility in all democratic republics
around the world, everyone -- regardless of voting eligability or status --
should have access to the data that is already made availabile by public entities and
that data should transparently presented, human-intelligable, and anonymous. As such,
this project is run independently and is not affiliated with Arlington County in any
capacity.

## Contact
You can write me at buscandoaverroes@icloud.com or contribute to the code at on [GitHub](https://github.com/buscandoaverroes/arl-votes-2020). Please do write me or contribute suggestions on how to improve the dashboard.

## Viewing
The app link is [here](). <br>

## Using and Citing
I've designed this as a tool for public use and reference.
My creative additions, which prepare the data for visualizations,
are licensed under the [Mozilla Public License](https://choosealicense.com/licenses/mpl-2.0/),
meaning that you can almost do whatever you want with my work
as long as you keep this license with whatever you do. <br><br>
However the voting data and voting precinct data are not mine and
you shouldn't claim them as yours either. They come with their own rules
and licences, which I have actually taken care to find and read. You may
not, under any circumstances, modify or alter the voting or GIS data
from Arlington County. As I do not intend to redistribute their data,
please refer to the links below to get the voting tallies or precinct
boundaries from their respective sources. Please take care to note the
licenses in the R packages that I use as well. <br>

# About the Data
All data are updated within ~48 hours from from Arlington Country Government and
Department of Elections. As of now, the voting data are published by
Arlington County in pdf form, which means that they can't easily be
read by a computer. This leaves two options: copy and paste the data
manually into a spreadsheet, or use packages that can read and process
the data. While both are prone to different types of errors, I have
opted for the latter since it is entirely reproducible if you have
the data and my R code. This way if I or the computer make a mistake
in 'translating' the data from pdf to R, you can see exactly what I
did and improve it. The voting data are published sort-of-daily by
Arlington, so I upload the most recent document and tell you the
date on the splash page. Voter registration data is also public
and was last updated on 1 October, 2020. The data that define the
precinct geographic boundaries and names come from Arlington County;
the files were updated last on 24 September, 2020. Refer to the **About**
page for additional techincal details.


## Is This Data Actually Accurate?

Yes -- all data are the latest from Arlington Country Government and
Department of Elections. As of now, the voting data are published by
Arlington County in pdf form, which means that they can't easily be
read by a computer. This leaves two options: copy and paste the data
manually into a spreadsheet, or use packages that can read and process
the data. While both are prone to different types of errors, I have
opted for the latter since it is entirely reproducible if you have
the data and my R code. This way if I or the computer make a mistake
in 'translating' the data from pdf to R, you can see exactly what I
did and improve it. The voting data are published sort-of-daily by
Arlington, so I upload the most recent document and tell you the
date on the splash page. Voter registration data is also public
and was last updated on 1 October, 2020. The data that define the
precinct geographic boundaries and names come from Arlington County;
the files were updated last on 24 September, 2020.


## Technical Details

### Terms and Definitions

Arlington County provides figures for how many registered and active
voters are in each precinct. The information I use in this project was
last updated on the first day of October, 2020, so we can assume this
data is quite accurate for the 2020 election cycle. <br>

Arlington County provides voting data by precinct, with name and
precinct code. The voting data come in the form of three columns
or variables: **Mail Ballots Outstanding**, **Mail Ballots Received**,
and **Early Voting**.

### About the Terms I Use
- Number of Ballots Requested: The data provided by Arlington County
only indicate the number of ballots received and outstanding;
it can be reasonably assumed that the total number of ballots
requested is the sum of these two numbers, which is how I compute it.
- Mail Ballot Return Rate: is simply **(No. Mail Ballots Receieved)
/ (No. Mail Ballots Requested)**. The data do not allow us to discen
if voters who requested a mail ballot have voted early in lieu of
voting by mail, which is allowed by voting rules
- Early-to-Mail Ratio:tells us if more votes have been cast by
**Early Voting** or by Mail. It is calculated by
**(No. Ballots by Early Voting) / (No. Ballots by Mail)**.
A ratio of 1 indicates that the number of votes cast by
Early and Mail is about even; higher numbers indicate that
more votes were cast by **Early Voting**, and lower numbers
indicate greater Mail-in Voting numbers.
- Total Votes by Mail: the sum of all **Mail Ballots Received**
for all precincts.
- Total Votes by Early Voting: the sum of all Early Votes
for all precincts.
- Total Votes: the sum of **Mail Ballots Received** and
**Early Voting** for all precincts.
- Voter Turnout and Active Voter Turnout: the difference between
the two is a matter of what goes in the denominator:
**Active Voter Turnout** takes only active registered voters
(**active**, in the data) as the denominator, while **Voter Turnout**
uses all registered voters (**all**, in the data) for this subterranean
figure. In both cases, the numerator is **Total Votes**
- Mail Voter Turnout: This is simply the number of
**(No. Mail Ballots Received) / (Active Registered Voters)**
- Early Voter Turnout: Similarly to the above, **(Early Voting) /
(Active Registered Voters)**.
- Early-to-Mail Turnout Ratio: This is a measure of the extent to which
Voter Turnout can be attributed to Early Voting Compared to Mail-in Voting,
or **(Early Voter Turnout) / (Mail Voter Turnout)**. A ratio of 1.00
means that Early Voting and Mail-in Voting contributed equal shares
to the overall Active Turnout. A higher ratio indicates that a Early
Voting contributed a larger share of the (active) voter turnout compared
to Mail-in Voting.
- Percent Mail Counted: uses **Mail Ballots Received** as the denominator and the County-provided
total of Mail ballots counted as the natural numerator.

### Other Notes

- Rounding: For readability, I round all non-integer values that I calculate
myself to relevant digits. This does occasionally result in components of a
whole that do not sum perfectly. For example, **Mail Voter Turnout** and
**Early Voter Turnout** should sum perfectly to **Voter Turnout**, but there
are couple of instances where the values show in the map are about a tenth
off. However, the code ensures that these errors are in fact due to rounding
and, in the worst case, are marginal: it checks that after variable
construction that each case where relevant quantities that should be equal
are within half a percent or less.
- Normalized Data: I normalize the voting data to create the polar chart on the
stats tab. Conceptually, this means that, for each indicator seperately, the raw
data is transformed such as the highest value in each indicator becomes 1 and the
lowest value becomes zero. The code will reproduce this table for you as object
**sp.norm**.
- Jefferson Precinct: The Arlington Country GIS data indicate that the
Jefferson Precinct, while considered a single precinct (No. 27),
is split among two different Virginia House districts (Districts 47 and
49). However, as the Arlington County Voting data does not disaggregate
voting information by the two 'subsets' of the Jefferson district, for
this application I have simply combined the two boundaries of the
Jefferson House districts to form a single district consistent with the Arlington Voting Data.

## Data Sources
This page was developed using publicly-available data, including:
- Voting data from the Arlington County [Daily Turnout Reports](https://vote.arlingtonva.us/daily-turnout/)
- [Voter Precinct Polygons](https://gisdata-arlgis.opendata.arcgis.com/datasets/voter-precinct-polygons)
- [Voter Registration Numbers](https://arlingtonva.s3.amazonaws.com/wp-content/uploads/sites/3/2020/10/Registrant_Counts_By_Locality.pdf)
