# Spatial Berlin Rent Price Analysis
> This is a project prepared during the Digital Economy and Decision Analytics - Blockchain and Cryptocurrency Seminar class at Humboldt University of Berlin.

## Motivation and Goals
The motivation of this project came to me after I moved to Berlin and started to look more closely into the rent prices of the local market. Naturally, it was hard to get a grasp what are the rules on a market in a new country, not to even mention in a new currency. Is this apartment too expensive for what it provides? Is that a good deal, am I being ripped off or perhaps missing out on a great price? Hard to say when you can't rely on some form of passive subcounsoius price-compas that has been built up over many years. But even if you would be comfortable with any form of new currency and new prices for apartments you still might lack the idea whether an area is a place that you would enjoy living in or now. Hence I had the idea to do a research on this topic and try to figure out some form of benchmark to look up to that would help me out. In my head the benchmark should account both for the characteristics of the apartments but also what kind of area is the apartment located in. 

While thinking about the idea how to perform this kind of research I figured that I would want to have an equation that would look something like the following:

**Market Price = Weight * Model Price + Residuals**

By deriving a model for the prices based on historical data based on various apartment characteristics I could then analyse the infrastructure of Berlin on order to grade the areas by saturation by objects of interest like parks, shops etc. and create a flexible weight index that would serve me as a discount factor for the modelled price in order to account for the surrounding areas. The better the area the higher the price one would be okay with paying for that apartment, and the other way around the worse the area gets the bigger the discount a person would want for an apartment of the same parameters. This got me thining that the remaining resuduals could be viewed as a landlord premium factor. If my weighted model price would be less than the market price that would mean that the landlord has included a premium into that listed price, and in the opposite direction perhaps heor she could have undervalued the property.

***Hence the goal of my research was to create a stable benchmark model to check for price fairness and then to use that info to analyze the proposed landlord premiums.***

## 1. Developing the Spatial framework 
...comming soon
### Import the Shape Files
...comming soon
### Clean the data and separate the interest groups
...comming soon
### Creating the cound table
...comming soon
### Brief lookaround
...comming soon

## 2. Real estate data
...comming soon
### Import the data and clean it
...comming soon
### Explore the data
...comming soon
### Create the model
...comming soon

## 3. Geocoding
...comming soon
### Connect to the OpenStreetMap API and use it
...comming soon

## 4. Distance measurment and weight factor
...comming soon
### Calculate the distance betweenthe apartments and the polygon objetcts
...comming soon
### Aggregation and normalization of the data
...comming soon
### Calculate the cross-section area values between the apartments walk radius and multipolygon objects
...comming soon
### Aggregation and normalization of the data
...comming soon
### Finalize the weight factors
...comming soon

## 5. Landlord premium research
...comming soon
