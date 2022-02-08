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
All polygon points of interest
![alldots](https://user-images.githubusercontent.com/92677707/153077305-781a0a02-9f68-488d-9d63-9cf31fa536f1.png)
All multipolygon points of interest
![poipolyplot](https://user-images.githubusercontent.com/92677707/153077333-51891ef5-dcd8-4dca-aa14-ddaa1c47db3b.png)
All forests and rivers+lakes
![waterforestplot](https://user-images.githubusercontent.com/92677707/153077497-bbf4098a-9108-4e22-ad96-8a8b4db63b13.png)

After counting the objects:
Saturation by destinations(tourism objects) by Berlin Bezirks
![bezirkmap_destinations](https://user-images.githubusercontent.com/92677707/153076435-437fdc9e-190f-40eb-bc3d-23aaf6b872a2.png)
Saturation by Entertainment(night clubs, cinemas) objects by Berlin Bezirks
![bezirkmap_entertainment](https://user-images.githubusercontent.com/92677707/153076449-566eb288-43d1-4de1-bc63-4f2fdd4866f7.png)
Saturation by Health objects(doctors, apothecaries) by Berlin Bezirks
![bezirkmap_health](https://user-images.githubusercontent.com/92677707/153076465-17f62d5f-e2b4-4d5b-ae91-2481a656ef7b.png)
Saturation by kids object(playgrounds, kitas etc.) by Berlin Bezirks
![bezirkmap_kids](https://user-images.githubusercontent.com/92677707/153076477-0723d3ab-7593-4f57-8c62-95472bf08a46.png)
Saturation by parks by Berlin Bezirks(by hectars)
![bezirkmap_parks](https://user-images.githubusercontent.com/92677707/153076489-ad1c8e8a-c48c-452a-8ac8-408c080d9316.png)
Saturation by transport objects(Ubahn, Sbahn etc.) by berlin Bezirks
![bezirkmap_transport](https://user-images.githubusercontent.com/92677707/153076501-8e47f13d-b06d-4704-b74f-5c47a525282c.png)
Saturation by water objects(rivers and lakes) by Berlin Bezirks
![bezirkmap_water](https://user-images.githubusercontent.com/92677707/153076515-abca3790-4a1a-44c0-807e-771a9deac0d6.png)
Saturation by activities(sport) by Berlin Bezirks
![bezirkmap_activities](https://user-images.githubusercontent.com/92677707/153076411-5f93db9a-dbd1-4229-be4f-f7a6b16d77ef.png)
Saturation by catering(bars, restaurants  etc.) by berlin Bezirks
![bezirkmap_catering](https://user-images.githubusercontent.com/92677707/153076422-af2bb963-fc07-495a-a991-99147e79f266.png)


## 2. Real estate data
...comming soon
### Import the data and clean it
...comming soon
### Explore the data
...comming soon
Does the heating matter on the price?
![heatingtypewithlines](https://user-images.githubusercontent.com/92677707/153077593-5a5e11fa-fc46-4bfc-9585-d9880c675d19.png)
![heatingtypewithoutlines](https://user-images.githubusercontent.com/92677707/153077619-61a0d01b-8ccc-4097-a788-2783cfd4392a.png)


### Create the model
...comming soon
Diagnostic plots
![nologprice](https://user-images.githubusercontent.com/92677707/153077153-3bf835fb-a617-429e-ad0a-c226c01e3606.png)
Logprice model
![logprice](https://user-images.githubusercontent.com/92677707/153077145-dee395bb-748a-4fe1-aefe-415191332968.png)
The model
![modelingtheprices](https://user-images.githubusercontent.com/92677707/153077629-b6d661ff-25b4-46c9-8539-5ed0218beb6f.png)


## 3. Geocoding
...comming soon
### Connect to the OpenStreetMap API and use it
...comming soon!
Hence we get the following apartments placed on the map:
All the apartments combined with forests and rivers, seems to be a good distribution of points across the Berlin landscape
![all_apartments](https://user-images.githubusercontent.com/92677707/153078006-17938c6d-f67d-4459-917a-387feaf40f9b.png)
Combined with all the previous data
![all_apartments3](https://user-images.githubusercontent.com/92677707/153077807-313dfe3b-5996-40de-8160-a430c96a5c10.png)


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
### Scoring:
![score_activities](https://user-images.githubusercontent.com/92677707/153078168-50e7cad0-f025-42a7-b900-dbec85ceb56b.png)
![score_catering](https://user-images.githubusercontent.com/92677707/153078184-e186ba3b-e7b0-412a-b108-0972d877fc11.png)
![score_destinations](https://user-images.githubusercontent.com/92677707/153078193-a2b39f59-cd9b-4d30-a5c3-bae8d24230f6.png)
![score_entertainment](https://user-images.githubusercontent.com/92677707/153078204-bd8a48d6-e33e-4b58-a322-fd7acf74e88e.png)
![score_health](https://user-images.githubusercontent.com/92677707/153078223-d34d9fa7-3056-4c71-8f5e-dea5983ff0f0.png)
![score_kids](https://user-images.githubusercontent.com/92677707/153078229-bfc4bb88-1332-4847-8e08-820e26d14454.png)
![score_shopping](https://user-images.githubusercontent.com/92677707/153078239-b09b0ab0-23b6-4862-902d-e7198477870d.png)
![score_total](https://user-images.githubusercontent.com/92677707/153078248-a212694f-5177-46c9-a576-7761ed5d201c.png)
![score_transport](https://user-images.githubusercontent.com/92677707/153078258-8fc4099c-dd79-459f-9157-071cb84f65bf.png)


## 5. Landlord premium research
...comming soon
