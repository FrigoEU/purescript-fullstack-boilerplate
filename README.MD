Aan te passen
-------------
- Naam in bower.json & package.json
- Description in package.json
- DB name in Server/Main.purs
- DB name in package.json "sqlschema"

TODO
----
staticHost disabelen in productie modus, nginx ofzoiets voorzien?

makkelijk reloaden van client en server
Waarom dumpen we die gebuilden files gewoon in de root? Waarom niet in zoiets als /dist? Dan wordt staticHost toch al iets veiliger

moet ik die launchBus gebruiken? (pm2.js)

Initial render
Save Session in LS
Meteen ingelogd na register?
Register update and other actions
wrap router in LazyRef so stuff like nav in worker -> setHashChannel -> hashChanged -> hashChangedChannel -> dispatch doesn't cause a rerender, among others

- Forks goed zetten

Opgeloste vragen
================
- Route in Router.purs ? cycle?: Route is essentieel deel van de State. Zit daar dus correct
- Alle State naast mekaar of activePage :: S-Page {pageState}? + loadPage. Als je alle State naast mekaar hebt kan je nog kiezen wat je bijhoudt of wegsmijt. Meestal zal bijhouden ook wel intuïtief zijn want gebruiker ziet gewoon een projectie van de state, en aparte pagina's en het feit dat die hun state elke keer reset is niet echt iets voor de user maar eerder een technisch ding van server side rendering dat de standaard is geworden.
- Waar moet effects zitten? Effects is ook een "interpreter" voor Action. Dus ofwel zit die bij Action bij (waar ik hem nu heb gezet), of zit die in een aparte file als Action te groot wordt.
- Port/DB URI etc: Env Variables zijn het beste volgens 12 factor app
- compression zit in endpoints, clustering via pm2
- build process: post-install gebruiken want Heroku doet gewoon npm install

- Vroeger heb ik er eens over gedacht om ipv functies gewoon puur die Action objecten in de VTree te steken, dan zijn al die problemen met "functies serializen" van de baan. Is dat een illusie of echt een optie? DONE