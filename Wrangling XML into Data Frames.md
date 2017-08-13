# Data wrangling XML
In my recommendation engine project, I wanted to get data about the customer's collection of games, and about other people who liked similar games. This involved loading three basic kinds of data from the Board Game Geek (BGG) website:

* The games (collection) that the customer has owned, previously owned, or otherwise rated
* The ratings and gamers who have rated those games
* Attributes (like theme, mechanics, and number of ratings) for each game
* Game collections of gamers with ratings in common

So far, I've loaded the first three. My code has not progressed enough to select which gamer's collections I want to load for step four, but loading their collections should be the same as the first step.

The BGG XML API has two calls that are relevant.

| Data needed       | BGG call                  |
| ----------------- | ------------------------- |
| Gamer collection  | collection?username=GAMER |
| Game ratings      | thing?id=GAME             |
| Game attributes   | thing?id=GAME             |

In general I had to solve problems related to querying the server, parsing XML, and wrangling the data into a tidy data frame format.

## Solution Overview
Querying the server— The server required at least two queries before it would return results (HTTP status code `202 Accepted` on first query), and downloading lists of ratings could require up to 700 queries per game. I wrote an XML query function that cached XML data files when it could, and queried the server with polite delays when it couldn't. This paid off when 11 hours of downloading 134,000 pages was interrupted for various kinds of troubleshooting.

Parsing XML— Data about games was stored in many formats, and included chr, int, date, and logical (0 or 1) types:

| Data              | Example                       |
| ----------------- | ----------------------------- |
| Unique elements   | `<name>Eclipse</name>`        |
| Unique attributes | `<rating value="8"></rating>` |
| Attribute lists   | `<status fortrade="0" lastmodified="2013-09-02 23:55:28" own="0" wishlist="1" wishlistpriority="5"> ` |

For the most part `xpathApply` could extract these. Note that you have to use `xmlParse` to enable xpathApply; `xmlTreeParse` doesn't work for this.

```r
success <- try(collection.doc <- xmlParse(r))
game <- xpathSApply(collection.root, '//*/name', xmlValue)
rating <- as.integer(xpathSApply(collection.root, '//*/stats/rating', xmlAttrs))

```

Wrangling into a Tidy data frame— In all cases I avoided referencing attributes or values by position, because position isn't guaranteed. I created vectors of attribute types to extract (like date.fields) and then got a list of 159 results, rotated the list into a column, then bound it to my table.

```r
  date.fields <- c("lastmodified")

  # Status is a list (per item) of lists of attributes (up to 10)
  status <- xpathApply(collection.root, '//*/status', xmlAttrs)

  status.tbl <- status %>%
    lapply(function(x) as_tibble(t(x))) %>%
    bind_rows()
  for(c in date.fields) { status.tbl[c] <- (as.Date(status.tbl[[c]]))}
  bigT.tbl <- bind_cols(collection.tbl, status.tbl)

```

The remainder of this document goes into more detail on these aspects.

# Challenge: Querying the BGG server
Though BGG has a published API and permissive terms of use, the server introduced delays and the API did not support requests that were efficient.

The first request to the server is usually answered with HTTP response status code `202, Accepted`. *The request has been accepted for processing, but the processing has not been completed. The request might or might not be eventually acted upon, and may be disallowed when processing occurs.*

My code had to respond by waiting a few seconds and asking again until the status code was `200, OK`.

```r
r <- GET(collection.path)
if(r$status_code == 200) {
  # Data received, go!
} else if (r$status_code == 202) {
  # Wait a couple of seconds, try again
} else {
  # Some other problem, stop!
}

```

Ultimately I also received these [HTTP status codes](https://en.wikipedia.org/wiki/List_of_HTTP_status_codes) and handled them by delaying and asking again.
* 429, Too Many Requests — The user has sent too many requests in a given amount of time. Intended for use with rate-limiting schemes.
* 502, Bad Gateway — The server was acting as a gateway or proxy and received an invalid response from the upstream server.

For the customer's collection, this delay wasn't a problem. But for ratings, the inefficiency of the API multipled this into a problem. The API provided 100 game ratings per query, per game. This required my code to make many queries per game and aggregate the data into a single table. The query for ratings also provides attributes for the game, and this is redundent when reading hundreds of pages per game.

Ultimately I had to download 13,490 pages of rating data and the runtime was over 11 hours.

Because I was running my program repeatedly in order to debug the XML parsing, I decided to cache the XML files as-is, and prefer using the cache over going to the server. Though it was a lot of code, there were several benefits to this:
* It may have prevented getting permanently blocked by the server for too many queries
* It sped up re-running the program
* When one of the XML ratings files couldn't be parsed by R due to an invalid character, I was able to manually edit the offending data in the cache and re-run the application
* The API uses the same query for ratings data and game attributes. The cache eliminated the redundent server query for game attributes

One unforseen challenge to the cache was that Mac OS finder is not geared to handle 13,000+ files in a single directory; reportedly it gets sluggish at 4,000. I solved this by creating a cache subdirectory for each game.

All of the XML loading code is in `get_bgg_xml.R`

# Challenge: Parsing the XML

* Parse the XML text into an XML structure in R
* Get values that can be individually addressed, like name and rating
* Get values that are part of a set of attributes for an element

## Parse text into XML structure

```r
success <- try(collection.doc <- xmlParse(r))
if(("XMLInternalDocument" %in% class(success))) {
  # successfully parsed
}

```

This transforms the text into XML. I use xmlParse() and not xmlTreeParse(), because I need xmlPathApply to get values:

>  xmlParse and htmlParse are equivalent to the xmlTreeParse and htmlTreeParse respectively, except they both use a default value for the useInternalNodes parameter of TRUE, i.e. they working with and return internal nodes/C-level nodes. These can then be searched using XPath expressions via xpathApply and getNodeSet.

Note there will be an item for each game in the collection; one item shown.

```xml
<?xml version="1.0" encoding="utf-8" standalone="yes"?>
<items pubdate="Tue, 04 Jul 2017 15:48:11 +0000" termsofuse="http://boardgamegeek.com/xmlapi/termsofuse" totalitems="159">
	<item collid="18402612" objectid="421" objecttype="thing" subtype="boardgame">
		<name sortindex="1">1830: The Game of Railroads and Robber Barons</name>
		<stats maxplayers="7" maxplaytime="360" minplayers="2" minplaytime="180" numowned="4981" playingtime="360">
			<rating value="8">
				<average value="7.81393"/>
				<bayesaverage value="7.29646"/>
			</rating>
		</stats>
		<status fortrade="0" lastmodified="2013-03-22 00:15:21" own="0" preordered="0" prevowned="1" want="0" wanttobuy="0" wanttoplay="0" wishlist="0"/>
	</item>
	<item collid="18313731" objectid="38343" objecttype="thing" subtype="boardgame">
		<name sortindex="1">Ad Astra</name>
		<stats maxplayers="5" maxplaytime="60" minplayers="3" minplaytime="60" numowned="2632" playingtime="60">
			<rating value="N/A">
				<average value="6.89538"/>
				<bayesaverage value="6.51655"/>
			</rating>
		</stats>
		<status fortrade="0" lastmodified="2013-09-02 23:55:28" own="0" preordered="0" prevowned="0" want="0" wanttobuy="0" wanttoplay="0" wishlist="1" wishlistpriority="2"/>
	</item>
	<!-- There are 159 <item>s total -->
</items>

```

## Get values from elements
Values like the game's name and rating were easy to read because they were easy to address individually, like rating, or stored as elements (enclosed in tags), like game name.

There are 159 games (item) in the file. Game will be a list of 159 strings. Rating will be a list of 159 integers or NAs.

```r
  game <- xpathSApply(collection.root, '//*/name', xmlValue)
  rating <- as.integer(xpathSApply(collection.root, '//*/stats/rating', xmlAttrs))

```

## Get values from attribute lists
Status returns a list of 159 lists, one per game. Each of those lists is a vector of 9 or 10 attributes like "fortrade", "own", or "prevowned". If wishlist=1, then wishlistpriority will be between 1 and 5, otherwise the wishlistpriority attribute will not be present.

I defined the attributes to extract because:
* The column order isn't guaranteed
* Some columns aren't always present (wishlistpriority)
* Different columns have different data types: logical, integer, date

```r
  bool.fields <- c("own", "prevowned", "fortrade",
                   "want", "wanttoplay", "wanttobuy",
                   "wishlist", "preordered")
  int.fields <- c("wishlistpriority")
  date.fields <- c("lastmodified")

  # Status is a list (per item) of lists of attributes (up to 10)
  status <- xpathApply(collection.root, '//*/status', xmlAttrs)

  status.tbl <- status %>%
    lapply(function(x) as_tibble(t(x))) %>%
    bind_rows()
  for(c in date.fields) { status.tbl[c] <- (as.Date(status.tbl[[c]]))}
  for(c in bool.fields) { status.tbl[c] <- (status.tbl[[c]] == "1") }
  for(c in int.fields)  { status.tbl[c] <- (as.integer(status.tbl[[c]]))}

  ## combine into collection.df
  bigT.tbl <- bind_cols(collection1.tbl, status.tbl)

```

```text
> status <- xpathApply(collection.root, '//*/status', xmlAttrs)
> length(status)
[1] 159

> str(status)
List of 159
 $ : Named chr [1:9] "0" "1" "0" "0" ...
  ..- attr(*, "names")= chr [1:9] "own" "prevowned" "fortrade" "want" ...
 $ : Named chr [1:9] "1" "0" "0" "0" ...
  ..- attr(*, "names")= chr [1:9] "own" "prevowned" "fortrade" "want" ...
 $ : Named chr [1:9] "1" "0" "0" "0" ...
  ..- attr(*, "names")= chr [1:9] "own" "prevowned" "fortrade" "want" ...
 $ : Named chr [1:9] "1" "0" "0" "0" ...
  ..- attr(*, "names")= chr [1:9] "own" "prevowned" "fortrade" "want" ...
 $ : Named chr [1:10] "0" "0" "0" "0" ...
  ..- attr(*, "names")= chr [1:10] "own" "prevowned" "fortrade" "want" ...

> str(status[1])
List of 1
 $ : Named chr [1:9] "0" "1" "0" "0" ...
  ..- attr(*, "names")= chr [1:9] "own" "prevowned" "fortrade" "want" ...
> str(status[[1]])
 Named chr [1:9] "0" "1" "0" "0" "0" "0" "0" "0" "2013-03-22 00:15:21"
 - attr(*, "names")= chr [1:9] "own" "prevowned" "fortrade" "want" ...


>status[1] # class is list, length 1
[[1]]
                  own             prevowned              fortrade                  want 
                  "0"                   "1"                   "0"                   "0" 
           wanttoplay             wanttobuy              wishlist            preordered 
                  "0"                   "0"                   "0"                   "0" 
         lastmodified 
"2013-03-22 00:15:21" 

>status[[1]] # class is character, length 9
                  own             prevowned              fortrade                  want 
                  "0"                   "1"                   "0"                   "0" 
           wanttoplay             wanttobuy              wishlist            preordered 
                  "0"                   "0"                   "0"                   "0" 
         lastmodified 
"2013-03-22 00:15:21" 

> status[5][1]
[[1]]
                  own             prevowned              fortrade                  want 
                  "0"                   "0"                   "0"                   "0" 
           wanttoplay             wanttobuy              wishlist      wishlistpriority 
                  "0"                   "0"                   "1"                   "2" 
           preordered          lastmodified 
                  "0" "2013-09-02 23:55:28" 

> status[[1]]["lastmodified"]
lastmodified
"2013-03-22 00:15:21" 


```

# Note: XML Parsing Error: not well formed
This is a little obscure, but here's a look at how I found and fixed an XML parsing error in a downloaded XML file.

### Error in R

```text
Browse[1]> n
debug at ~/dev/data science/meeple-matcher/scripts/get_bgg_xml.R#84: success <- try(collection.doc <- xmlParse(r))
Browse[2]> success <- try(collection.doc <- xmlParse(r))
invalid character in attribute value
attributes construct error
Couldn't find end of Start Tag comment line 397
PCDATA invalid Char value 16
Error : 1: invalid character in attribute value
2: attributes construct error
3: Couldn't find end of Start Tag comment line 397
4: PCDATA invalid Char value 16

Browse[2]> success
[1] "Error : 1: invalid character in attribute value\n2: attributes construct error\n3: Couldn't find end of Start Tag comment line 397\n4: PCDATA invalid Char value 16\n\n"
attr(,"class")
[1] "try-error"
attr(,"condition")
<XMLParserErrorList: 1: invalid character in attribute value
2: attributes construct error
3: Couldn't find end of Start Tag comment line 397
4: PCDATA invalid Char value 16
>
Browse[2]> collection.path
[1] "https://boardgamegeek.com/xmlapi2/thing?id=48726&page=55&ratingcomments=1"
Browse[2]> 
```

```

### Error as seen from web browser
I put the URL into Firefox directly to look at the results. Here's the error:

```text
  XML Parsing Error: not well-formed
  Location: https://boardgamegeek.com/xmlapi2/thing?id=48726&page=55&ratingcomments=1
  Line Number 399, Column 265: The two biggest criticisms leveled against it: too much downtime, and a lack of meaningful decisions. I believe both these criticisms are unfounded. Sure, downtime does become somewhat of a problem during the last few rounds, but by then the game is mostly over. In addition, I&#039;ve never felt like this game is anywhere near scripted. By the end of the game, the board does get crowded, but not to the point where I felt like only poor choices were left for me. That&#039;s part of my problem with Alea Iacta Est, players can get completely screwed with extremely poor choices. This does not happen a lot with Alien Frontiers.
  ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------^

```

### Source from web browser for error page
In Firefox I typed command-U to view the source of the error page:

```text
                                   <comment username="ryanst85" rating="8" value="Perhaps overhyped, but a solid and entertaining addition to the recently crowded dice allocation genre.

 The two biggest criticisms leveled against it: too much downtime, and a lack of meaningful decisions. I believe both these criticisms are unfounded. Sure, downtime does become somewhat of a problem during the last few rounds, but by then the game is mostly over. In addition, I&#039;ve never felt like this game is anywhere near scripted. By the end of the game, the board does get crowded, but not to the point where I felt like only poor choices were left for me. That&#039;s part of my problem with Alea Iacta Est, players can get completely screwed with extremely poor choices. This does not happen a lot with Alien Frontiers.

I also hear complaints leveled against the raiders outpost, claiming that it&#039;s overpowered and abusive. I have not witnessed unchecked abuse from the raiders outpost in any of the games I&#039;ve played. It&#039;s useful to keep the leader in check, but then people complain there is too much leader bashing.

I guess the game can&#039;t win over everybody, and eventually it had to come back through the stratosphere and into the realm of realistic expectations.

What you get in this game: a worker placement dice allocation game with some area control and a strong science fiction theme. If you&#039;re expecting a game to revolutionize the worker placement genre, look somewhere else. It&#039;s been a while since I&#039;ve had this much fun rolling dice.

" />

```

## Problem
The problem is the "DEL" character, the red background yellow dot in "mostly over. In addition".

## Solution
I downloaded the file to disk into the cache directory, then used a text editor (Sublime) to remove the offending character. When I ran my program again it went to cache and successfully processed the working file.