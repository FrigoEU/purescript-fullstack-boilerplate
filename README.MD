Aanpassen
---------
Naam in bower.json

TODO
----
DB address en andere 12 factor dingen
Build proces
props
Initial render
Route in Router.purs ? cycle?
Save Session in LS
Meteen ingelogd na register?
Alle State naast mekaar of activePage :: S-Page {pageState}?
 + loadPage
Clean up Worker & UI
Waar moet effects zitten?
Register update and other actions
Vroeger heb ik er eens over gedacht om ipv functies gewoon puur die Action objecten in de VTree te steken, dan zijn al die problemen met "functies serializen" van de baan. Is dat een illusie of echt een optie?
wrap router in LazyRef so stuff like nav in worker -> setHashChannel -> hashChanged -> hashChangedChannel -> dispatch doesn't cause a rerender, among others