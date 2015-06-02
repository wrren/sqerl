# Introduction

Sqerl/Squirrel Market Trade Processor. Written as part of the application process for [CurrencyFair](hhttps://www.currencyfair.com/). Accepts incoming currency trade information encoded in JSON through POST to the trade controller and records it in the configured MySQL database after decoding and validation. Implements two-tier rate limiting; all requests can be rate-limited based on IP Address while trade submissions can be rate-limited based on the user ID of the trade. 

After successful storage an event is fired through a gen_event manager. Clients connected via web socket to the feed controller will have trade data sent to them as they are processed. For an example of a trade feed display front-end based on Google's WebGL Globe, see [Peacock](http://github.com/wrren/peacock).

# Installation

## Dependencies

### System Level

- Erlang HiPE R17+
- MySQL Server

### Erlang Dependencies

- [cowboy](https://github.com/ninenines/cowboy)
- [emysql](https://github.com/Eonblast/Emysql)
- [jsx](https://github.com/talentdeficit/jsx)
- [lager](https://github.com/basho/lager)

```
git clone git@github.com:wrren/sqerl.git
cd sqerl
make
./_rel/sqerl_release/bin/sqerl_release start
```

Run Unit Tests:

```
make ct
```

# Configuration

Edit ```src/sqerl.app.src``` to set configuration values. Listen port, database connection and rate limiting options can be set. Changes to this file will only take effect after rebuilding the app using ```make```.

# Interacting with sqerl

Trade data should be POST'ed to the ```/trade``` route with a content type of ```application/json```. To test trade recording, use the ```request.sh``` script included with the repo. The first argument to the script is the trade submission URL. When running on the same server with the default listen port, the address would be ```http://localhost:8080/trade```. Request frequency is set to 10/s.

To receive trade submission events over websocket, connect to the ```/feed``` route. If using  [peacock](https://github.com/wrren/peacock), edit ```public/app/js/config.js``` and set the websocket URL. Example configuration:

```
var config = { server: "ws://example.com:8080/feed", max_feed_items: 30 };
```
