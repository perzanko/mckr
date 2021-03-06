module Store
  ( emails
  , firstNames
  , lastNames
  , cities
  , countries
  , phoneNumbers
  ) where

emails :: [String]
emails =
  [ "vmalik@sbcglobal.net"
  , "tjensen@mac.com"
  , "mkearl@msn.com"
  , "mleary@comcast.net"
  , "gknauss@att.net"
  , "natepuri@sbcglobal.net"
  , "fraterk@icloud.com"
  , "ntegrity@comcast.net"
  , "slanglois@yahoo.ca"
  , "klaudon@icloud.com"
  , "cameron@gmail.com"
  , "brickbat@outlook.com"
  , "syncnine@aol.com"
  , "bruck@comcast.net"
  , "dmbkiwi@comcast.net"
  , "augusto@msn.com"
  , "forsberg@yahoo.ca"
  , "mobileip@yahoo.ca"
  , "jonathan@outlook.com"
  , "hermanab@gmail.com"
  , "fluffy@outlook.com"
  , "salesgeek@live.com"
  , "fmtbebuck@yahoo.ca"
  , "vlefevre@aol.com"
  , "boein@live.com"
  , "stefano@comcast.net"
  , "jpflip@gmail.com"
  , "thomasj@comcast.net"
  , "rjones@live.com"
  , "stomv@yahoo.com"
  , "xtang@icloud.com"
  , "joglo@optonline.net"
  , "rbarreira@comcast.net"
  , "bonmots@gmail.com"
  , "kingma@hotmail.com"
  , "netsfr@hotmail.com"
  , "dmouse@me.com"
  , "dbrobins@yahoo.ca"
  , "firstpr@sbcglobal.net"
  , "reziac@att.net"
  , "froodian@verizon.net"
  , "joehall@yahoo.ca"
  , "wkrebs@live.com"
  , "ylchang@msn.com"
  , "pierce@hotmail.com"
  , "microfab@live.com"
  , "sopwith@live.com"
  , "klaudon@att.net"
  , "zavadsky@live.com"
  , "kewley@att.net"
  , "storerm@aol.com"
  , "jeteve@msn.com"
  , "cliffski@comcast.net"
  , "treeves@hotmail.com"
  , "bbirth@mac.com"
  , "janusfury@outlook.com"
  , "jsmith@optonline.net"
  , "wikinerd@me.com"
  , "hauma@comcast.net"
  , "mwandel@sbcglobal.net"
  , "milton@hotmail.com"
  , "lahvak@sbcglobal.net"
  , "novanet@comcast.net"
  , "harpes@yahoo.ca"
  , "timtroyr@msn.com"
  , "cmdrgravy@aol.com"
  , "mbswan@yahoo.ca"
  , "pthomsen@sbcglobal.net"
  , "afifi@mac.com"
  , "heidrich@gmail.com"
  , "ylchang@gmail.com"
  , "arnold@optonline.net"
  , "philb@me.com"
  , "choset@verizon.net"
  , "penna@att.net"
  , "gmcgath@att.net"
  , "mavilar@outlook.com"
  , "rfisher@hotmail.com"
  , "skaufman@comcast.net"
  , "tmccarth@me.com"
  , "bancboy@mac.com"
  , "policies@msn.com"
  , "aracne@sbcglobal.net"
  , "anicolao@me.com"
  , "pgolle@msn.com"
  , "unreal@gmail.com"
  , "dkrishna@outlook.com"
  , "singer@hotmail.com"
  , "cliffordj@yahoo.com"
  , "mgemmons@att.net"
  , "bruck@hotmail.com"
  , "rjones@verizon.net"
  , "onestab@yahoo.com"
  , "bmorrow@comcast.net"
  , "jsmith@gmail.com"
  , "sagal@outlook.com"
  , "hager@msn.com"
  , "jkegl@yahoo.ca"
  , "grolschie@gmail.com"
  , "delpino@sbcglobal.net"
  ]

firstNames :: [String]
firstNames =
  [ "Sean"
  , "Layla"
  , "Toby"
  , "Shamar"
  , "Travis"
  , "Rex"
  , "Denisse"
  , "Deven"
  , "Ronin"
  , "Romeo"
  , "Tabitha"
  , "Alfred"
  , "Jamir"
  , "Jazlyn"
  , "Jabari"
  , "Ivy"
  , "Preston"
  , "Abdiel"
  , "Camille"
  , "Angelique"
  , "Gabriel"
  , "Fatima"
  , "Raven"
  , "Kaden"
  , "Chris"
  , "Maxim"
  , "Litzy"
  , "Paula"
  , "Madelyn"
  , "Krista"
  , "Cyrus"
  , "Alejandra"
  , "Lacey"
  , "Jovany"
  , "Phoebe"
  , "Noah"
  , "Sophia"
  , "Mason"
  , "Hudson"
  , "Anne"
  , "Raegan"
  , "Brendan"
  , "Salvador"
  , "Alana"
  , "Lee"
  , "Isabell"
  , "Reynaldo"
  , "Elliot"
  , "Rylee"
  , "Marley"
  , "Leyla"
  , "Ayden"
  , "Cash"
  , "Imani"
  , "Zachary"
  , "Carly"
  , "Katelynn"
  , "Messiah"
  , "Jesse"
  , "Emilee"
  , "Kayla"
  , "Eden"
  , "Roselyn"
  , "Alyvia"
  , "Gia"
  , "Frances"
  , "Valentina"
  , "Karly"
  , "Alexandria"
  , "Taryn"
  , "Geovanni"
  , "Caden"
  , "Jameson"
  , "Madelynn"
  , "Hazel"
  , "Annabel"
  , "Nathen"
  , "Taylor"
  , "Jordyn"
  , "Jayla"
  , "Abagail"
  , "Jaiden"
  , "Meadow"
  , "Esther"
  , "Oliver"
  , "Kiersten"
  , "Aldo"
  , "Abby"
  , "Mckenzie"
  , "Nataly"
  , "Makenna"
  , "Stanley"
  , "Nathaniel"
  , "Vicente"
  , "Jocelyn"
  , "Lorelei"
  , "Erik"
  , "Damari"
  , "Amy"
  , "Jillian"
  ]

lastNames :: [String]
lastNames =
  [ "Dodson"
  , "Charles"
  , "Page"
  , "Boyer"
  , "David"
  , "Ballard"
  , "Hood"
  , "Stone"
  , "Russell"
  , "Maxwell"
  , "Beard"
  , "Hatfield"
  , "Carson"
  , "Choi"
  , "Skinner"
  , "Li"
  , "Santos"
  , "Whitehead"
  , "Santiago"
  , "Klein"
  , "Garrett"
  , "Farrell"
  , "Hendricks"
  , "Chan"
  , "Miranda"
  , "Luna"
  , "Torres"
  , "Mayer"
  , "Miller"
  , "Adams"
  , "Buck"
  , "Palmer"
  , "Marquez"
  , "Mercado"
  , "Mckinney"
  , "Peterson"
  , "Brandt"
  , "Taylor"
  , "Fuller"
  , "Saunders"
  , "Delacruz"
  , "Montoya"
  , "Wolfe"
  , "Michael"
  , "Joseph"
  , "Atkinson"
  , "Nash"
  , "Perez"
  , "Brewer"
  , "Walton"
  , "Lynn"
  , "Escobar"
  , "Navarro"
  , "Potter"
  , "Murillo"
  , "Cannon"
  , "Ashley"
  , "Payne"
  , "Jacobson"
  , "Monroe"
  , "Hancock"
  , "Flowers"
  , "Salinas"
  , "Woodard"
  , "Mason"
  , "Solomon"
  , "Cooley"
  , "Tucker"
  , "Sloan"
  , "Coleman"
  , "Hodges"
  , "Ibarra"
  , "Mays"
  , "Richardson"
  , "Leonard"
  , "Wilson"
  , "Haynes"
  , "Barton"
  , "Mclean"
  , "Mcconnell"
  , "Herman"
  , "Benton"
  , "Roberts"
  , "Raymond"
  , "Clay"
  , "Gilmore"
  , "Buckley"
  , "Giles"
  , "Johnston"
  , "Bender"
  , "English"
  , "Vance"
  , "Ware"
  , "Cook"
  , "Sheppard"
  , "Doyle"
  , "Park"
  , "Cardenas"
  , "Barnes"
  , "Ray"
  ]

phoneNumbers :: [String]
phoneNumbers =
  [ "(562) 280-7831"
  , "(703) 522-1522"
  , "(846) 483-3249"
  , "(735) 549-7945"
  , "(594) 987-0688"
  , "(802) 394-5991"
  , "(767) 761-9201"
  , "(797) 644-3983"
  , "(907) 541-8686"
  , "(578) 591-6259"
  , "(583) 242-9734"
  , "(311) 840-7811"
  , "(321) 804-2787"
  , "(708) 814-6101"
  , "(979) 921-4437"
  , "(926) 833-2717"
  , "(500) 451-2398"
  , "(419) 534-7290"
  , "(781) 448-6719"
  , "(839) 238-1771"
  , "(565) 873-6753"
  , "(689) 969-8373"
  , "(408) 671-2194"
  , "(708) 460-7178"
  , "(548) 676-1509"
  , "(330) 336-6407"
  , "(471) 207-9293"
  , "(306) 281-9077"
  , "(251) 508-8233"
  , "(355) 760-5388"
  , "(360) 892-5366"
  , "(482) 692-8755"
  , "(672) 741-8385"
  , "(423) 510-7792"
  , "(339) 740-8087"
  , "(988) 778-3164"
  , "(488) 525-7957"
  , "(518) 571-7400"
  , "(958) 837-1650"
  , "(808) 497-0450"
  , "(504) 898-1431"
  , "(863) 612-7404"
  , "(723) 939-1600"
  , "(982) 750-1529"
  , "(865) 416-5780"
  , "(759) 606-8186"
  , "(980) 517-0845"
  , "(873) 760-7553"
  , "(968) 327-1394"
  , "(462) 307-6307"
  , "(812) 566-2913"
  , "(555) 504-0512"
  , "(526) 668-6020"
  , "(663) 624-6251"
  , "(396) 389-0695"
  , "(947) 779-2354"
  , "(496) 490-6645"
  , "(400) 447-1104"
  , "(923) 874-6641"
  , "(580) 627-5022"
  , "(323) 625-3268"
  , "(858) 775-5836"
  , "(699) 880-4403"
  , "(323) 470-3659"
  , "(443) 754-6847"
  , "(442) 899-6186"
  , "(718) 428-5664"
  , "(413) 671-3474"
  , "(772) 542-9924"
  , "(691) 411-0735"
  , "(599) 552-1070"
  , "(608) 462-9034"
  , "(724) 955-1816"
  , "(691) 885-1139"
  , "(834) 707-9665"
  , "(306) 696-6804"
  , "(202) 738-1685"
  , "(973) 273-4120"
  , "(810) 648-6016"
  , "(416) 776-2296"
  , "(239) 239-9530"
  , "(902) 844-6894"
  , "(516) 773-7837"
  , "(813) 639-0688"
  , "(812) 904-4397"
  , "(603) 496-7156"
  , "(460) 696-7182"
  , "(276) 871-1605"
  , "(230) 507-8902"
  , "(704) 893-9663"
  , "(785) 430-5862"
  , "(264) 647-4965"
  , "(516) 912-9451"
  , "(519) 415-8798"
  , "(455) 740-7546"
  , "(554) 947-1445"
  , "(333) 863-6627"
  , "(797) 432-6861"
  , "(724) 641-7395"
  , "(442) 482-3831"
  ]

streetAddresses :: [String]
streetAddresses =
  [ "8477 Arrowhead St. "
  , "Norfolk, VA 23503"
  , "826 Lees Creek Court "
  , "La Crosse, WI 54601"
  , "48 Roehampton Street "
  , "Columbus, GA 31904"
  , "825 Woodsman Ave. "
  , "Anoka, MN 55303"
  , "5 Grand Dr. "
  , "West Bloomfield, MI 48322"
  , "9979 W. Warren St. "
  , "Warner Robins, GA 31088"
  , "9 Nut Swamp St. "
  , "Biloxi, MS 39532"
  , "848 Johnson Dr. "
  , "Hollywood, FL 33020"
  , "7624 Bayberry Street "
  , "Jackson, NJ 08527"
  , "934 Crescent Rd. "
  , "Missoula, MT 59801"
  , "9 Adams Ave. "
  , "Tullahoma, TN 37388"
  , "7697 Railroad Avenue "
  , "Spartanburg, SC 29301"
  , "7133 Cemetery Street "
  , "Rome, NY 13440"
  , "42 Mayfair Rd. "
  , "Vineland, NJ 08360"
  , "242 Trenton Drive "
  , "Pottstown, PA 19464"
  , "8298 Hudson Drive "
  , "Sewell, NJ 08080"
  , "765 Sunnyslope Drive "
  , "Hinesville, GA 31313"
  , "486 West Olive St. "
  , "Longwood, FL 32779"
  , "139 Argyle St. "
  , "Midlothian, VA 23112"
  , "8681 Nut Swamp St. "
  , "Leland, NC 28451"
  , "9852 Kirkland Street "
  , "Fargo, ND 58102"
  , "8 County Road "
  , "Rockville, MD 20850"
  , "8240 Heritage Road "
  , "Durham, NC 27703"
  , "7739 Wood Street "
  , "Hammonton, NJ 08037"
  , "9149 N. Warren Street "
  , "Manitowoc, WI 54220"
  , "599 NW. Clinton Drive "
  , "West Springfield, MA 01089"
  , "364 Sunbeam St. "
  , "Alabaster, AL 35007"
  , "771 Pearl St. "
  , "Wilson, NC 27893"
  , "974 West Pheasant Dr. "
  , "Palatine, IL 60067"
  , "213 Overlook St. "
  , "Strongsville, OH 44136"
  , "9775 South Pendergast Dr. "
  , "Hummelstown, PA 17036"
  , "115 N. Ann Lane "
  , "Ankeny, IA 50023"
  , "8320 Greystone Road "
  , "Lockport, NY 14094"
  , "135 Washington Lane "
  , "Milwaukee, WI 53204"
  , "8030 Andover Lane "
  , "Englewood, NJ 07631"
  , "8765 East Mammoth Street "
  , "Westerville, OH 43081"
  , "6 Cobblestone Court "
  , "Bloomington, IN 47401"
  , "9061 E. Mechanic Ave. "
  , "Maspeth, NY 11378"
  , "53 Fieldstone Dr. "
  , "Baldwinsville, NY 13027"
  , "605 Whitemarsh Drive "
  , "South Richmond Hill, NY 11419"
  , "128 Warren Ave. "
  , "Gaithersburg, MD 20877"
  , "49 William Drive "
  , "Annapolis, MD 21401"
  , "12 Walnutwood Street "
  , "Bel Air, MD 21014"
  , "7918 Lookout St. "
  , "Ashtabula, OH 44004"
  , "7769 Parker St. "
  , "Selden, NY 11784"
  , "267 NE. Hickory St. "
  , "Meriden, CT 06450"
  , "698 W. East Dr. "
  , "Englishtown, NJ 07726"
  , "7445 East Bay St. "
  , "Lutherville Timonium, MD 21093"
  , "47 Bellevue Drive "
  , "Westford, MA 01886"
  , "99 North 8th St. "
  , "Minot, ND 58701"
  , "15 Meadowbrook Drive "
  , "Grayslake, IL 60030"
  , "653 S. Bear Hill Dr. "
  , "Rosedale, NY 11422"
  , "7807 Anderson Drive "
  , "Fairport, NY 14450"
  , "707 Canal St. "
  , "Louisville, KY 40207"
  , "979 Buckingham St. "
  , "Glen Cove, NY 11542"
  , "79 Sunset Drive "
  , "Wilkes Barre, PA 18702"
  , "45 High Point Street "
  , "Hastings, MN 55033"
  , "8524 Linden Avenue "
  , "Garland, TX 75043"
  , "4 East Princess Rd. "
  , "Loveland, OH 45140"
  , "7544 3rd Ave. "
  , "Absecon, NJ 08205"
  , "7642 South Rockwell Ave. "
  , "Norwich, CT 06360"
  , "7573 San Carlos Road "
  , "Manassas, VA 20109"
  , "462 Oxford St. "
  , "Round Lake, IL 60073"
  , "29 Circle Drive "
  , "Suffolk, VA 23434"
  , "6 Wellington Rd. "
  , "Halethorpe, MD 21227"
  , "72 Cherry Hill Lane "
  , "Linden, NJ 07036"
  , "7357 Orange Street "
  , "Troy, NY 12180"
  , "250 Hartford Street "
  , "Woodstock, GA 30188"
  , "9160 Pineknoll St. "
  , "East Haven, CT 06512"
  , "9700 Harvard Road "
  , "Noblesville, IN 46060"
  , "8 Baker Drive "
  , "Gloucester, MA 01930"
  , "694 Plymouth St. "
  , "Prior Lake, MN 55372"
  , "976 Lakeshore Rd. "
  , "Ypsilanti, MI 48197"
  , "11 E. Adams Court "
  , "South Windsor, CT 06074"
  , "95 E. Carson Court "
  , "Fort Worth, TX 76110"
  , "16 Cedarwood Ave. "
  , "Morganton, NC 28655"
  , "7355 North Smith Store St. "
  , "Macungie, PA 18062"
  , "7 Summer Ave. "
  , "Mc Lean, VA 22101"
  , "317 Maple St. "
  , "Miamisburg, OH 45342"
  , "9422 Briarwood Court "
  , "Ozone Park, NY 11417"
  , "600 Trusel St. "
  , "Conyers, GA 30012"
  , "9949 Young St. "
  , "Baton Rouge, LA 70806"
  , "714 Elizabeth Dr. "
  , "Fort Lauderdale, FL 33308"
  , "8717 Sunnyslope Street "
  , "West Lafayette, IN 47906"
  , "97 Wrangler St. "
  , "Brighton, MA 02135"
  , "607 North Carriage Avenue "
  , "Central Islip, NY 11722"
  , "9980 Colonial St. "
  , "Park Forest, IL 60466"
  , "7010 Longbranch Drive "
  , "East Lansing, MI 48823"
  , "195 East Carpenter Street "
  , "Tupelo, MS 38801"
  , "504 Richardson Avenue "
  , "Cartersville, GA 30120"
  , "58 Jefferson St. "
  , "Los Banos, CA 93635"
  , "79 Illinois Street "
  , "Melrose, MA 02176"
  , "915 Wood Ave. "
  , "Tacoma, WA 98444"
  , "214 W. Clinton Dr. "
  , "Capitol Heights, MD 20743"
  , "84 West Cedarwood St. "
  , "Blackwood, NJ 08012"
  , "8361 Marlborough Lane "
  , "Stillwater, MN 55082"
  , "9101 Summit Court "
  , "Savage, MN 55378"
  , "69 NW. Woodside St. "
  , "Helena, MT 59601"
  , "6 Parker Court "
  , "Cape Coral, FL 33904"
  , "8021 Edgefield Street "
  , "Champlin, MN 55316"
  ]

countries :: [String]
countries =
  [ "Brazil"
  , "Russia"
  , "Morocco"
  , "Egypt"
  , "Indonesia"
  , "Colombia"
  , "China"
  , "Pakistan"
  , "Kenya"
  , "China"
  , "South"
  , "China"
  , "Guatemala"
  , "China"
  , "India"
  , "Uruguay"
  , "China"
  , "Yemen"
  , "United States"
  , "Serbia"
  , "Ivory Coast"
  , "India"
  , "India"
  , "Saudi Arabia"
  , "UnitedStates"
  , "Italy"
  , "Australia"
  , "Armenia"
  , "Singapore"
  , "Nepal"
  , "United States"
  , "Taiwan"
  , "Spain"
  , "South"
  , "South Korea"
  , "Thailand"
  , "China"
  , "Bulgaria"
  , "South"
  , "Kazakhstan"
  , "Nigeria"
  , "Turkey"
  , "China"
  , "Nigeria"
  , "Iran"
  , "Cameroon"
  , "Congo Republic"
  , "Cuba"
  , "Mexico"
  , "Pakistan"
  , "Russia"
  , "Pakistan"
  , "Brazil"
  , "China"
  , "China"
  , "Romania"
  , "United Kingdom"
  , "Senegal"
  , "Taiwan"
  , "United States"
  , "Saudi Arabia"
  , "China"
  , "Argentina"
  , "Venezuela"
  , "Belarus"
  , "South Africa"
  , "Brazil"
  , "Japan"
  , "South Korea"
  , "India"
  , "Colombia"
  , "Japan"
  , "Indonesia"
  , "China"
  , "Pakistan"
  , "Ecuador"
  , "Russia"
  , "Australia"
  , "China"
  , "Iraq"
  , "Argentina"
  , "Algeria"
  , "China"
  , "France"
  , "Iraq"
  , "China"
  , "India"
  , "Taiwan"
  , "China"
  , "Pakistan"
  , "China"
  , "Japan"
  , "China"
  , "Russia"
  , "Brazil"
  , "United States"
  , "Ukraine"
  , "Pakistan"
  , "Philippines"
  , "Algeria"
  ]

cities :: [String]
cities =
  [ "Fortaleza"
  , "Moscow"
  , "Casablanca"
  , "Giza"
  , "Makassar"
  , "Bogotá"
  , "Qingdao"
  , "Faisalabad"
  , "Nairobi"
  , "Wenzhou"
  , "Daegu"
  , "Changsha"
  , "Guatemala City"
  , "Quanzhou"
  , "Bhopal"
  , "Montevideo"
  , "Shenzhen"
  , "Sana'a"
  , "New York City"
  , "Belgrade"
  , "Abidjan"
  , "Kanpur"
  , "Mumbai"
  , "Riyadh"
  , "Chicago"
  , "Milan"
  , "Melbourne"
  , "Yerevan"
  , "Singapore"
  , "Kathmandu"
  , "Philadelphia"
  , "Taipei"
  , "Barcelona"
  , "Incheon"
  , "Seoul"
  , "Bangkok"
  , "Hong Kong"
  , "Sofia"
  , "Daejeon"
  , "Almaty"
  , "Ibadan"
  , "İzmir"
  , "Xiamen"
  , "Abuja"
  , "Shiraz"
  , "Douala"
  , "Brazzaville"
  , "Havana"
  , "Mexico City"
  , "Peshawar"
  , "Rostov-on-Don"
  , "Lahore"
  , "Curitiba"
  , "Hefei"
  , "Zhengzhou"
  , "Bucharest"
  , "Birmingham"
  , "Dakar"
  , "Tainan"
  , "Phoenix"
  , "Jeddah"
  , "Beijing"
  , "Rosario"
  , "Caracas"
  , "Minsk"
  , "Cape Town"
  , "Porto Alegre"
  , "Fukuoka"
  , "Suwon"
  , "Surat"
  , "Barranquilla"
  , "Nagoya"
  , "Surabaya"
  , "Shenyang"
  , "Karachi"
  , "Guayaquil"
  , "Nizhny Novgorod"
  , "Sydney"
  , "Tangshan"
  , "Baghdad"
  , "Buenos Aires"
  , "Algiers"
  , "Changchun"
  , "Paris"
  , "Basra"
  , "Chaozhou"
  , "Chennai"
  , "New Taipei City"
  , "Tianjin"
  , "Rawalpindi"
  , "Harbin"
  , "Kobe"
  , "Fuzhou"
  , "Novosibirsk"
  , "Brasília"
  , "Dallas"
  , "Kharkiv"
  , "Islamabad"
  , "Quezon City"
  , "Oran"
  ]
