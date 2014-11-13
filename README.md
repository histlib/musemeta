musemeta
=======



[![Build Status](https://api.travis-ci.org/ropensci/musemeta.png)](https://travis-ci.org/ropensci/musemeta)
[![Build status](https://ci.appveyor.com/api/projects/status/y3tefs9xb6pmql36/branch/master?svg=true)](https://ci.appveyor.com/project/sckott/musemeta/branch/master)

**R client for museum metadata**

Currently `musemeta` can get data from:

* [The Metropolitan Museum of Art](http://www.metmuseum.org/) via 
    * scraping the MET website (see function `muse_get()`)
    * http://scrapi.org/ (see functions `scrapi_()`)
* The Canadian Science & Technology Museum Corporation (CSTMC) (see functions `cstmc_()`)

Other sources of museum metadata will be added...check back later.

## Install

Get `ckanr` first, not on CRAN yet (I'll get `ckanr` up to CRAN before this is on CRAN)


```r
devtools::install_github("sckott/ckanr")
```

Then install musemeta


```r
devtools::install_github("ropensci/musemeta")
```


```r
library("musemeta")
```

## MET data

### Scraping site directly

Data for a single object


```r
muse_get(559490)
#> <Museum metadata> Siphon nozzle
#>   Period: New Kingdom, Ramesside
#>   Dynasty: Dynasty 19–20
#>   Date: ca. 1295–1070 B.C.
#>   Geography: From Egypt, Memphite Region, Lisht North, Cemetery, MMA 1913–1914
#>   Medium: Bronze
#>   Dimensions: l. 5.4 cm (2 1/8 in)
#>   Credit Line: Rogers Fund and Edward S. Harkness Gift, 1922
#>   Accession Number: 22.1.962
```

Or index to name of object, or values in the description


```r
muse_get(559490)$name
#> [1] "Siphon nozzle"
```


```r
muse_get(559490)$values[1:2]
#> [[1]]
#> [[1]]$name
#> [1] "Period"
#> 
#> [[1]]$value
#> [1] "New Kingdom, Ramesside"
#> 
#> 
#> [[2]]
#> [[2]]$name
#> [1] "Dynasty"
#> 
#> [[2]]$value
#> [1] "Dynasty 19–20"
```

A different object


```r
muse_get(246562)
#> <Museum metadata> Terracotta guttus (flask with handle and spout)
#>   Period: Classical
#>   Date: 4th century B.C.
#>   Culture: Greek, South Italian, Campanian
#>   Medium: Terracotta; black-glaze
#>   Dimensions: 2 7/8in. (7.3cm)
#>   Classification: Vases
#>   Credit Line: Purchase by subscription, 1896
#>   Accession Number: 96.18.35
```

Get many objects


```r
lapply(c(479283, 228901, 436876), muse_get)
#> [[1]]
#> <Museum metadata> Papyri Fragments
#>   Date: 7th century
#>   Geography: Made in Thebes, Byzantine Egypt
#>   Culture: Coptic
#>   Medium: Papyrus and ink
#>   Dimensions: Framed: 11 1/4 x 15 in. (28.5 x 38.1 cm)
#>   Classification: Papyrus
#>   Accession Number: 14.1.616
#> 
#> [[2]]
#> <Museum metadata> Piece
#>   Date: 19th century
#>   Culture: German
#>   Dimensions: 1 1/4 x 6 1/4in. (3.2 x 15.9cm)
#>   Classification: Textiles-Laces
#>   Accession Number: 63.80.18
#> 
#> [[3]]
#> <Museum metadata> Marion Lenbach (1892–1947), the Artist's Daughter
#>   Date: 1900
#>   Medium: Oil on canvas
#>   Dimensions: 58 7/8 x 41 1/2 in. (149.5 x 105.4 cm)
#>   Classification: Paintings
#>   Credit Line: Bequest of Collis P. Huntington, 1900
#>   Accession Number: 25.110.46
```

### Using the scrapi API

This is again, for The Metropolitan Museum of Art only 

Get a random object, limit to a few fields for brevity


```r
scrapi_random(fields=c('medium','whoList'))
#> $medium
#> [1] "Wood; carved"
```

Get a specific object


```r
scrapi_info(123, fields=c('title','primaryArtistNameOnly','medium'))
#> $title
#> [1] "Andiron"
#> 
#> $primaryArtistNameOnly
#> [1] "Richard Wittingham"
#> 
#> $medium
#> [1] "Brass"
```

Search for objects


```r
scrapi_search(query='mirror')
#> $out
#>  [1] "http://scrapi.org/object/207785" "http://scrapi.org/object/156225"
#>  [3] "http://scrapi.org/object/425550" "http://scrapi.org/object/436839"
#>  [5] "http://scrapi.org/object/60142"  "http://scrapi.org/object/54118" 
#>  [7] "http://scrapi.org/object/421538" "http://scrapi.org/object/421541"
#>  [9] "http://scrapi.org/object/421542" "http://scrapi.org/object/421543"
#> [11] "http://scrapi.org/object/421877" "http://scrapi.org/object/421892"
#> [13] "http://scrapi.org/object/414082" "http://scrapi.org/object/414085"
#> [15] "http://scrapi.org/object/413260" "http://scrapi.org/object/425547"
#> [17] "http://scrapi.org/object/412623" "http://scrapi.org/object/49591" 
#> [19] "http://scrapi.org/object/36624"  "http://scrapi.org/object/54864" 
#> [21] "http://scrapi.org/object/50397"  "http://scrapi.org/object/284630"
#> [23] "http://scrapi.org/object/267055" "http://scrapi.org/object/468197"
#> [25] "http://scrapi.org/object/44848"  "http://scrapi.org/object/284629"
#> [27] "http://scrapi.org/object/255880" "http://scrapi.org/object/427562"
#> [29] "http://scrapi.org/object/452364" "http://scrapi.org/object/449949"
#> [31] "http://scrapi.org/object/487410" "http://scrapi.org/object/467733"
#> [33] "http://scrapi.org/object/544234" "http://scrapi.org/object/38968" 
#> [35] "http://scrapi.org/object/198323" "http://scrapi.org/object/248153"
#> [37] "http://scrapi.org/object/247562" "http://scrapi.org/object/345749"
#> [39] "http://scrapi.org/object/415290" "http://scrapi.org/object/415292"
#> [41] "http://scrapi.org/object/461607" "http://scrapi.org/object/45522" 
#> [43] "http://scrapi.org/object/52475"  "http://scrapi.org/object/64481" 
#> [45] "http://scrapi.org/object/38424"  "http://scrapi.org/object/244558"
#> [47] "http://scrapi.org/object/244297" "http://scrapi.org/object/255617"
#> [49] "http://scrapi.org/object/255391" "http://scrapi.org/object/256949"
#> [51] "http://scrapi.org/object/247479" "http://scrapi.org/object/247869"
#> [53] "http://scrapi.org/object/249227" "http://scrapi.org/object/253556"
#> [55] "http://scrapi.org/object/253640" "http://scrapi.org/object/346681"
#> [57] "http://scrapi.org/object/317748" "http://scrapi.org/object/307734"
#> [59] "http://scrapi.org/object/386624" "http://scrapi.org/object/431176"
#> [61] "http://scrapi.org/object/431179" "http://scrapi.org/object/431181"
#> [63] "http://scrapi.org/object/427560" "http://scrapi.org/object/452852"
#> [65] "http://scrapi.org/object/452809" "http://scrapi.org/object/452948"
#> [67] "http://scrapi.org/object/434964" "http://scrapi.org/object/434966"
#> [69] "http://scrapi.org/object/459206" "http://scrapi.org/object/464248"
#> [71] "http://scrapi.org/object/471283" "http://scrapi.org/object/550263"
#> [73] "http://scrapi.org/object/17566"  "http://scrapi.org/object/63333" 
#> [75] "http://scrapi.org/object/55068"  "http://scrapi.org/object/53937" 
#> [77] "http://scrapi.org/object/193593" "http://scrapi.org/object/207520"
#> [79] "http://scrapi.org/object/203757" "http://scrapi.org/object/255960"
#> [81] "http://scrapi.org/object/251169" "http://scrapi.org/object/412628"
#> [83] "http://scrapi.org/object/415261" "http://scrapi.org/object/421876"
#> [85] "http://scrapi.org/object/413175" "http://scrapi.org/object/417966"
#> [87] "http://scrapi.org/object/423650" "http://scrapi.org/object/271708"
#> [89] "http://scrapi.org/object/427581" "http://scrapi.org/object/427585"
#> 
#> $links
#> $links$first
#> [1] "http://scrapi.org/search/mirror?page=1"
#> 
#> $links$`next`
#> [1] "http://scrapi.org/search/mirror?page=2"
#> 
#> $links$last
#> [1] "http://scrapi.org/search/mirror?page=29"
```

## CSTMC data

List changes


```r
cstmc_changes(limit = 1)
#> [[1]]
#> [[1]]$user_id
#> [1] "b50449ea-1dcc-4d52-b620-fc95bf56034b"
#> 
#> [[1]]$timestamp
#> [1] "2014-11-06T18:58:08.001743"
#> 
#> [[1]]$object_id
#> [1] "cc6a523c-cecf-4a95-836b-295a11ce2bce"
#> 
#> [[1]]$revision_id
#> [1] "5d11079e-fc05-4121-9fd5-fe086f5e5f33"
#> 
#> [[1]]$data
#> [[1]]$data$package
#> [[1]]$data$package$maintainer
#> [1] ""
#> 
#> [[1]]$data$package$name
#> [1] "test"
#> 
#> [[1]]$data$package$metadata_modified
#> [1] "2014-11-06T18:55:54.772675"
#> 
#> [[1]]$data$package$author
#> [1] ""
#> 
#> [[1]]$data$package$url
#> [1] ""
#> 
#> [[1]]$data$package$notes
#> [1] ""
#> 
#> [[1]]$data$package$owner_org
#> [1] "fafa260d-e2bf-46cd-9c35-34c1dfa46c57"
#> 
#> [[1]]$data$package$private
#> [1] FALSE
#> 
#> [[1]]$data$package$maintainer_email
#> [1] ""
#> 
#> [[1]]$data$package$author_email
#> [1] ""
#> 
#> [[1]]$data$package$state
#> [1] "deleted"
#> 
#> [[1]]$data$package$version
#> [1] ""
#> 
#> [[1]]$data$package$creator_user_id
#> [1] "b50449ea-1dcc-4d52-b620-fc95bf56034b"
#> 
#> [[1]]$data$package$id
#> [1] "cc6a523c-cecf-4a95-836b-295a11ce2bce"
#> 
#> [[1]]$data$package$title
#> [1] "test"
#> 
#> [[1]]$data$package$revision_id
#> [1] "5d11079e-fc05-4121-9fd5-fe086f5e5f33"
#> 
#> [[1]]$data$package$type
#> [1] "dataset"
#> 
#> [[1]]$data$package$license_id
#> [1] "notspecified"
#> 
#> 
#> 
#> [[1]]$id
#> [1] "59c308c8-68b2-4b92-bc57-129378d31882"
#> 
#> [[1]]$activity_type
#> [1] "deleted package"
```

List datasets


```r
cstmc_datasets(as = "table")
#>  [1] "artifact-data-agriculture"                                  
#>  [2] "artifact-data-aviation"                                     
#>  [3] "artifact-data-bookbinding"                                  
#>  [4] "artifact-data-chemistry"                                    
#>  [5] "artifact-data-communications"                               
#>  [6] "artifact-data-computing-technology"                         
#>  [7] "artifact-data-domestic-technology"                          
#>  [8] "artifact-data-energy-electric"                              
#>  [9] "artifact-data-exploration-and-survey"                       
#> [10] "artifact-data-fisheries"                                    
#> [11] "artifact-data-forestry"                                     
#> [12] "artifact-data-horology"                                     
#> [13] "artifact-data-industrial-technology"                        
#> [14] "artifact-data-lighting-technology"                          
#> [15] "artifact-data-location-canada-agriculture-and-food-museum"  
#> [16] "artifact-data-location-canada-aviation-and-space-museum"    
#> [17] "artifact-data-location-canada-science-and-technology-museum"
#> [18] "artifact-data-marine-transportation"                        
#> [19] "artifact-data-mathematics"                                  
#> [20] "artifact-data-medical-technology"                           
#> [21] "artifact-data-meteorology"                                  
#> [22] "artifact-data-metrology"                                    
#> [23] "artifact-data-mining-and-metallurgy"                        
#> [24] "artifact-data-motorized-ground-transportation"              
#> [25] "artifact-data-non-motorized-ground-transportation"          
#> [26] "artifact-data-on-loan"                                      
#> [27] "artifact-data-photography"                                  
#> [28] "artifact-data-physics"                                      
#> [29] "artifact-data-printing"                                     
#> [30] "artifact-data-railway-transportation"                       
#> [31] "artifact-dataset-fire-fighting"
```

Search for packages


```r
out <- cstmc_package_search(q = '*:*', rows = 2, as='table')
lapply(out$results$resources, function(x) x[,1:3])
#> [[1]]
#>                      resource_group_id cache_last_updated
#> 1 cce39b19-e07c-4c51-941b-242afd3f1c4a                 NA
#> 2 cce39b19-e07c-4c51-941b-242afd3f1c4a                 NA
#> 3 cce39b19-e07c-4c51-941b-242afd3f1c4a                 NA
#> 4 cce39b19-e07c-4c51-941b-242afd3f1c4a                 NA
#>           revision_timestamp
#> 1 2014-10-28T20:14:43.878106
#> 2 2014-11-04T03:04:24.281137
#> 3 2014-11-05T21:46:30.031396
#> 4 2014-11-05T21:48:27.302007
#> 
#> [[2]]
#>                      resource_group_id cache_last_updated
#> 1 02de4923-4b61-4078-9bf7-71cada759cbc                 NA
#> 2 02de4923-4b61-4078-9bf7-71cada759cbc                 NA
#> 3 02de4923-4b61-4078-9bf7-71cada759cbc                 NA
#> 4 02de4923-4b61-4078-9bf7-71cada759cbc                 NA
#>           revision_timestamp
#> 1 2014-10-28T15:25:38.337754
#> 2 2014-11-03T18:08:49.560921
#> 3 2014-11-05T18:44:23.555560
#> 4 2014-11-05T18:45:18.003557
```

## Meta

* Please report any issues or bugs](https://github.com/ropensci/musemeta/issues).
* License: MIT
* Get citation information for `musemeta` in R doing `citation(package = 'musemeta')`

[![](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)
