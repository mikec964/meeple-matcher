# meeple-matcher
Recommend board games to users based on their current collection and stated interests.

Stretch goals:

* NLP of game blurb to predict success
* Parse game comments to create a 3D network of connected games
* Voice access via Alexa skill

# Data Source and Collection
Data can be acquired from the BoardGameGeek (BGG) website.

* https://boardgamegeek.com/collection/user/mikec?own=1&subtype=boardgame&ff=1
* https://boardgamegeek.com/boardgame/95105/1st-goal/ratings?rated=1

## Collection
We can probably get the data through the (BGG XML API)[https://boardgamegeek.com/wiki/page/BGG_XML_API] or their new XML API (API2)[https://boardgamegeek.com/wiki/page/BGG_XML_API2]. Below, I've indicated which data is available through the API.

Their (terms of use)[https://boardgamegeek.com/wiki/page/XML_API_Terms_of_Use] provide this information for non-commercial purposes.

## Data We'll Consider
Player data from their current collection:

* List of games they own (Collection in API2)
* Wish list (Collection on API2)
* Ratings and reviews they're posted (Collection in API2)

Stated interests can be used to filter or prioritize results. (Vague!)

* Type: Abstract strategy, customizable, thematic, family, children, party, strategy, wargames
* Family: Crowdfunding, and closely related and compatible games.
* Number of Players
* Category: Space exploration, economic, medieval, real-time, and other tags
* Mechanisms: Auction, Card drafting, Dice rolling, and other tags

Through the API We can access particular (games along with comments and commenters)[https://boardgamegeek.com/wiki/page/BGG_XML_API#toc3].


# Recommendation Engine

## Collaborative filtering (CF)
Memory-based, model-based, or hybrid?

Context-aware?
* GPS in a store: Limit to store inventory, or games published recently?
* Ebay & BGG market: Any game


## Conjoint Analysis
https://en.wikipedia.org/wiki/Conjoint_analysis

Conjoint analysis techniques may also be referred to as multiattribute compositional modelling, discrete choice modelling, or stated preference research, and is part of a broader set of trade-off analysis tools used for systematic analysis of decisions. These tools include Brand-Price Trade-Off, Simalto, and mathematical approaches such as AHP,[2] evolutionary algorithms or rule-developing experimentation.

https://cran.r-project.org/web/packages/support.CEs/index.html

# Implementation Notes
# Existing Solutions
Discussion list here: https://boardgamegeek.com/thread/1210019/recommendation-engine

# Questions
* How to access XML API from R 
* 
